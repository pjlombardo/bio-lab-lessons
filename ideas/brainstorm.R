library(readxl)
library(tidyverse)

#####################
## Preparing data ###
#####################

df<-read_excel('data/KwanDataset.xlsx',
               range="A3:I24")
View(df)
names(df)
df1<-df %>%
    select(-c(3,6))
names(df1)

name_to_time<-function(x){
    if (grepl("2$",x)){
        return("24hr")
    } else if (grepl("5$",x)){
        return("48hr")
    } else {
        return("72hr")
    }
}
name_to_time("lesion...5")

df2<-df1 %>% pivot_longer(cols = contains("lesion"),
                     values_to="lesion.length.mm"
                     ) %>%
    mutate(Time=sapply(name,name_to_time))

#
df2$Sample...1==df2$Sample...4
df2$Sample...1==df2$Sample...7

df3<-df2 %>%
    mutate(Sample=Sample...1)%>%
    select(Sample, Time, lesion.length.mm,pH) %>%
    as.data.frame()

create_na_pH<-function(time,ph){
    if (time=="72hr"){
        return(ph)
    } else {
        return(NA)
    }
}

df4 <- df3 %>% mutate(pH = mapply(FUN=create_na_pH,Time,pH)) %>%
    arrange(Time)

# create a variable that just tells us wild-type, budB or control
type_fxn<-function(samp){
    if (grepl("^WT",samp)){
        return("Wild")
    } else if (grepl("^bud",samp)){
        return("budB")
    } else {
        return("control")
    }
}

final_data<-df4 %>% 
    mutate(type = sapply(Sample, type_fxn)) %>%
    select(Sample,type,Time, lesion.length.mm,pH)

View(final_data)

####################
# Generating plots #
####################

# Summarise, then plot

## Summary data frame
final_data_summ<-final_data %>%
    group_by(Time,type) %>%
    summarise(mean = mean(lesion.length.mm),
              sd = sd(lesion.length.mm),
              low = mean -sd,
              high = mean + sd,
              ci_low=mean-2*sd,
              ci_high = mean+2*sd
    )

## Just 72hr mark df
df_72<-final_data %>% filter(Time == "72hr")
df_72_summ<-df_72 %>%
    mutate(type = factor(type,levels=c("Wild","budB","control"))) %>%
    group_by(type) %>%
    summarise(mean = mean(pH),
              sd = sd(pH),
              ci_low = mean -2*sd,
              ci_high = mean +2*sd)
    
# plot 1
ggplot(data = final_data_summ,
       aes(x = Time,
           y= mean,
           # need two aesthetics to add the point and line
           color=type,
           group=type))+ 
    geom_point()+geom_line() + 
    #
    geom_errorbar(aes(ymin=low, ymax=high),width=.05,
                  alpha = .6
    )+
    labs(y="Average lesion length (mm)",
         x="Time (hours)",
         title ="Average lesion length by time intervals")+
    theme_bw()

# plot 1b
ggplot(data = final_data_summ,
       aes(x = Time,
           y= mean,
           # need two aesthetics to add the point and line
           color=type, fill=type,
           group=type))+ 
    geom_ribbon(aes(ymin=low, ymax=high),width=.05,
                alpha = .2
    )+
    geom_point()+geom_line() + 
    labs(y="Average lesion length (mm)",
         x="Time (hours)",
         title ="Average lesion length by time intervals")+
    theme_bw()
ggsave('plot_images/plot1b.png',width = 8, height =5)


# All together...
plot1<-final_data %>%
    group_by(Time,type) %>%
    summarise(mean = mean(lesion.length.mm),
              sd = sd(lesion.length.mm),
              low = mean -sd,
              high = mean + sd
    ) %>%
    ggplot(data = .,
           aes(x = Time,
               y= mean,
               # need two aesthetics to add the point and line
               color=type,
               group=type)) + 
    geom_point()+geom_line() + 
    #
    geom_errorbar(aes(ymin=low, ymax=high),width=.05,
                  alpha = .6
                  )+
    labs(y="Average lesion length (mm)",
         x="Time (hours)",
         title ="Average lesion length by time intervals")+
    theme_bw()

ggsave('plot_images/plot1.png',width = 6, height =4)
# ?ggsave


#Plot 2
ggplot(data = df_72_summ,
       aes(x = type,
           y = mean,
           fill = type))+
    geom_col() + 
    geom_errorbar(aes(ymin=ci_low, ymax=ci_high),
                  width=.1)+
    labs(x="Treatment",
         y="Average pH")+theme_bw()

plot2<-final_data %>%
    filter(Time=="72hr") %>%
    mutate(type = factor(type,levels=c("Wild","budB","control"))) %>%
    group_by(type) %>%
    summarise(mean = mean(pH),
             sd = sd(pH),
             low = mean -2*sd,
             high = mean +2*sd) %>%
    ggplot(data = .,
           aes(x = type,
               y = mean,
               fill = type))+
    geom_col() + 
    geom_errorbar(aes(ymin=low, ymax=high),
                  width=.1)+
    labs(x="Treatment",
         y="Average pH")+theme_bw()

ggsave('plot_images/plot2.png',width = 6, height =4)


# Plot 3,
set.seed(133)
ggplot(data = df_72_summ,
       aes(x = type,
           y = mean,
           fill = type))+
    geom_col() + 
    geom_errorbar(aes(ymin=ci_low, ymax=ci_high),
                  width=.1)+
    geom_jitter(data = df_72, aes(x=type,y=pH),
                width = .08,
                pch = 1,
                color='gray2')+
    labs(x="Treatment",
         y="Average pH")+theme_bw()

set.seed(133)
plot3<-df_72 %>%
    mutate(type = factor(type,levels=c("Wild","budB","control"))) %>%
    group_by(type) %>%
    summarise(mean = mean(pH),
              sd = sd(pH),
              ci_low = mean -2*sd,
              ci_high = mean +2*sd) %>%
    ggplot(data = .,
           aes(x = type,
               y = mean,
               fill = type))+
    geom_col() + 
    geom_errorbar(aes(ymin=ci_low, ymax=ci_high),
                  width=.1)+
    geom_jitter(data = df_72, aes(x=type,y=pH),
                width = .08,
                pch = 1,
                color='gray2')+
    labs(x="Treatment",
         y="Average pH")+theme_bw()

ggsave('plot_images/plot3.png',width = 6, height =4)


set.seed(5432)
ggplot(data = df_72_summ,
       aes(x = type,
           y = mean,
           fill = type))+
    geom_col(alpha = .7) + 
    geom_errorbar(aes(ymin=ci_low, ymax=ci_high),
                  width=.2)+
    geom_jitter(data = df_72, aes(x=type,y=pH,fill=type),
                width = .08,
                pch = 21,
                color='gray2')+
    labs(x="Treatment",
         y="Average pH")+theme_bw()


set.seed(5432)
plot3alt<-df_72 %>%
    mutate(type = factor(type,levels=c("Wild","budB","control"))) %>%
    group_by(type) %>%
    summarise(mean = mean(pH),
              sd = sd(pH),
              ci_low = mean -2*sd,
              ci_high = mean +2*sd) %>%
    ggplot(data = .,
           aes(x = type,
               y = mean,
               fill = type))+
    geom_col(alpha = .7) + 
    geom_errorbar(aes(ymin=ci_low, ymax=ci_high),
                  width=.2)+
    geom_jitter(data = df_72, aes(x=type,y=pH,fill=type),
                width = .08,
                pch = 21,
                color='gray2')+
    labs(x="Treatment",
         y="Average pH")+theme_bw()

ggsave('plot_images/plot3alt.png',width = 6, height =4)


###################
## ANOVA Testing ##
###################

df_72 %>%
    aov(lesion.length.mm~type, data = .) %>%
    TukeyHSD()

df_72 %>%
    aov(pH~type, data = .) %>%
    TukeyHSD() 

