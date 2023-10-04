
<!-- rnb-text-begin -->

---
title: "Additional Visualizations, Endpoint pH Experiment"
author: "P. Lombardo"
output:
  html_document:
    df_print: paged
  html_notebook: default
---


<!-- rnb-text-end -->



<!-- rnb-text-begin -->


## Understanding the experimental data
The data, and the description below, were generously provided by Dr. Grace Kwan.

#### Abstract

*Pectobacterium carotovorum is a commonly found bacterial pathogen that rots plant tissues. You have probably seen P. carotovorum in action if you have ever left some fresh produce in the refrigerator too long and came back to a slimy, watery mess. This bacterium uses enzymes to break down plant cell walls, causing the plant cells to burst from osmotic pressure and release all of their water and nutrients -- nutrients that support the continued growth of the bacterial pathogen.*

*Enzymes typically have optimal pH at which they work best. Above or below this optimal pH, the enzyme's activity diminishes. Pathogens may need to regulate the pH of their environment in order to ensure that their enzymes function optimally. *

*This experiment assessed the importance of environmental pH on the ability of P. carotovorum to break down plant tissues (i.e., rot them). P. carotovorum uses the BudB protein to limit acid production by providing an alternate fermentation pathway that results in a product of neutral pH.*

### Experiment details
The experiment monitored several lettuce leaves within each treatment class:

* Three control leaves without bacteria (`Control`)
* Nine leaves inoculated with wild-type P. carotovorum (`Wild`), and
* Nine leaves inoculated with $\Delta$budB mutant (`budB`).

At 24, 48, and 72 hours, disease progression was monitored as the size of the soft rot area. At the conclusion of the experiment, 72 hrs, the soft rotted tissue was mashed up to evaluate the pH.

## Preparing data frames
We will need our raw data, but filtered to only include the endpoint measurements (i.e., those at 72 hours). We save this as `experiment72`

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZXhwZXJpbWVudCAlPiUgXG4gICAgZmlsdGVyKFRpbWU9PVwiNzJoclwiKSAtPiBleHBlcmltZW50NzJcbmBgYCJ9 -->

```r
experiment %>% 
    filter(Time=="72hr") -> experiment72
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


We will also need a data frame with summary information. Specifically, for each `Treatment` group we want

* the average pH (`average_pH`)
* the standard deviation of pH (`std.dev_pH`)
* the lower limit of a confidence interval (`low`) computed as `mean - 2*std.dev*`.
* the upper limit of a confidence interval (`high`) computed as `mean + 2*std.dev*`.

Here is the code to make that data frame, which we call `summary_experiment`

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZXhwZXJpbWVudCAlPiUgXG4gICAgZmlsdGVyKFRpbWU9PVwiNzJoclwiKSAlPiVcbiAgICBncm91cF9ieShUcmVhdG1lbnQpICU+JVxuICAgIHN1bW1hcml6ZShhdmVyYWdlX3BIID0gbWVhbihwSCksXG4gICAgICAgICAgICBzdGQuZGV2X3BIID0gc2QocEgpKSAlPiVcbiAgICAgbXV0YXRlKGxvdyA9IGF2ZXJhZ2VfcEggLSAyKnN0ZC5kZXZfcEgsXG4gICAgICAgICAgIGhpZ2ggPSBhdmVyYWdlX3BIICsgMipzdGQuZGV2X3BIKSAtPiBzdW1tYXJ5X2V4cGVyaW1lbnRcbmBgYCJ9 -->

```r
experiment %>% 
    filter(Time=="72hr") %>%
    group_by(Treatment) %>%
    summarize(average_pH = mean(pH),
            std.dev_pH = sd(pH)) %>%
     mutate(low = average_pH - 2*std.dev_pH,
           high = average_pH + 2*std.dev_pH) -> summary_experiment
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Original plot


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZ2dwbG90KGRhdGEgPSBzdW1tYXJ5X2V4cGVyaW1lbnQsICAgICBcbiAgICAgICBhZXMoeCA9IFRyZWF0bWVudCwgeSA9IGF2ZXJhZ2VfcEgpKStcbiAgICBnZW9tX2NvbChhZXMoZmlsbCA9IFRyZWF0bWVudCkpK1xuICAgIGdlb21fZXJyb3JiYXIoYWVzKHltaW4gPSBsb3csIHltYXggPSBoaWdoKSwgd2lkdGggPSAwLjEpK1xuICAgIGxhYnMoeT1cIkF2ZXJhZ2UgcEhcIiwgdGl0bGUgPSBcIkxlYWYgcEggYXQgNzIgaG91cnMgYnkgVHJlYXRtZW50XCIpK1xuICAgIHNjYWxlX3lfY29udGludW91cyhicmVha3MgPSBzZXEoMCw3LGJ5ID0gMSkpXG5gYGAifQ== -->

```r
ggplot(data = summary_experiment,     
       aes(x = Treatment, y = average_pH))+
    geom_col(aes(fill = Treatment))+
    geom_errorbar(aes(ymin = low, ymax = high), width = 0.1)+
    labs(y="Average pH", title = "Leaf pH at 72 hours by Treatment")+
    scale_y_continuous(breaks = seq(0,7,by = 1))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Adjusting color schemes with R Color Brewer
Using the `scale_*_brewer()` command, where `*` is replaced by your aesthetic, you can use R Color Brewer to select some pre-made color palettes.

Here is a link to the options: [R Color Brewer Palettes.](https://r-graph-gallery.com/38-rcolorbrewers-palettes.html)

Below is an example plot where we choose the "Accent" palette from the options and specify it in the `scale_fill_brewer()` command.

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZ2dwbG90KGRhdGEgPSBzdW1tYXJ5X2V4cGVyaW1lbnQsICAgICBcbiAgICAgICBhZXMoeCA9IFRyZWF0bWVudCwgeSA9IGF2ZXJhZ2VfcEgpKStcbiAgICBnZW9tX2NvbChhZXMoZmlsbCA9IFRyZWF0bWVudCkpK1xuICAgIGdlb21fZXJyb3JiYXIoYWVzKHltaW4gPSBsb3csIHltYXggPSBoaWdoKSwgd2lkdGggPSAwLjEpK1xuICAgIGxhYnMoeT1cIkF2ZXJhZ2UgcEhcIiwgdGl0bGUgPSBcIkxlYWYgcEggYXQgNzIgaG91cnMgYnkgVHJlYXRtZW50XCIpK1xuICAgIHNjYWxlX3lfY29udGludW91cyhicmVha3MgPSBzZXEoMCw3LGJ5ID0gMSkpICtcbiAgICAjIE5ldyBjb2RlIHRvIGFkanVzdCBjb2xvcnMhXG4gICAgc2NhbGVfZmlsbF9icmV3ZXIocGFsZXR0ZT1cIkFjY2VudFwiKSt0aGVtZV9idygpK2xhYnModGl0bGU9XCJBZGp1c3QgY29sb3Igc2NoZW1lcyB3aXRoIGBicmV3ZXJgXCIpXG5gYGAifQ== -->

```r
ggplot(data = summary_experiment,     
       aes(x = Treatment, y = average_pH))+
    geom_col(aes(fill = Treatment))+
    geom_errorbar(aes(ymin = low, ymax = high), width = 0.1)+
    labs(y="Average pH", title = "Leaf pH at 72 hours by Treatment")+
    scale_y_continuous(breaks = seq(0,7,by = 1)) +
    # New code to adjust colors!
    scale_fill_brewer(palette="Accent")+theme_bw()+labs(title="Adjust color schemes with `brewer`")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Confidence intervals only

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZ2dwbG90KGRhdGEgPSBzdW1tYXJ5X2V4cGVyaW1lbnQsICAgICBcbiAgICAgICBhZXMoeCA9IFRyZWF0bWVudCwgeSA9IGF2ZXJhZ2VfcEgpKStcbiAgICBnZW9tX2Vycm9yYmFyKGFlcyh5bWluID0gbG93LCB5bWF4ID0gaGlnaCxjb2xvcj1UcmVhdG1lbnQpLCB3aWR0aCA9IDAuNSxsaW5ld2lkdGg9LjcpK1xuICAgIGdlb21fcG9pbnQoYWVzKGZpbGwgPSBUcmVhdG1lbnQpLHBjaCA9IDIxLHNpemU9MykrXG4gICAgbGFicyh5PVwiQXZlcmFnZSBwSFwiLCB0aXRsZSA9IFwiQ29uZmlkZW5jZSBpbnRlcnZhbHMgb25seVwiKStcbiAgICBzY2FsZV95X2NvbnRpbnVvdXMobGltaXRzPWMoMyw3KSxicmVha3MgPSBzZXEoMyw3LGJ5ID0gMSkpK3RoZW1lX2J3KClcblxuYGBgIn0= -->

```r
ggplot(data = summary_experiment,     
       aes(x = Treatment, y = average_pH))+
    geom_errorbar(aes(ymin = low, ymax = high,color=Treatment), width = 0.5,linewidth=.7)+
    geom_point(aes(fill = Treatment),pch = 21,size=3)+
    labs(y="Average pH", title = "Confidence intervals only")+
    scale_y_continuous(limits=c(3,7),breaks = seq(3,7,by = 1))+theme_bw()

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Overlaying data over the original plot

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZ2dwbG90KGRhdGEgPSBzdW1tYXJ5X2V4cGVyaW1lbnQsICAgICBcbiAgICAgICBhZXMoeCA9IFRyZWF0bWVudCwgeSA9IGF2ZXJhZ2VfcEgpKStcbiAgICAjIFdlIGFkZGVkIGFscGhhPTAuNiBvdXRzaWRlIG9mIHRoZSBhZXN0aGV0aWNzIHRvIG1ha2UgXG4gICAgIyB0aGUgYmFycyBsZXNzIHZpc2libGUgYW5kIGhlbHAgaGlnaGxpZ2h0IHRoZSBkYXRhIHBvaW50cyBcbiAgICBnZW9tX2NvbChhZXMoZmlsbCA9IFRyZWF0bWVudCksYWxwaGEgPSAwLjYpK1xuICAgIGdlb21fZXJyb3JiYXIoYWVzKHltaW4gPSBsb3csIHltYXggPSBoaWdoKSwgd2lkdGggPSAwLjEpK1xuICAgIGxhYnMoeT1cIkF2ZXJhZ2UgcEhcIiwgdGl0bGUgPSBcIkxlYWYgcEggYXQgNzIgaG91cnMgYnkgVHJlYXRtZW50XCIsXG4gICAgICAgICAjIFdlIGFsc28gYWRkIGEgc3VidGl0bGUgdG8gbWFrZSB0aGUgbWVhbmluZyBvZiB0aGUgXG4gICAgICAgICAjIGNvbHVtbnMgY2xlYXJcbiAgICAgICAgIHN1YnRpdGxlID0gXCIoQ29sdW1ucyBpbmRpY2F0ZSB0aGUgbWVhbiBwSCBmb3IgZWFjaCBncm91cC4pXCIpK1xuICAgIHNjYWxlX3lfY29udGludW91cyhicmVha3MgPSBzZXEoMCw3LGJ5ID0gMSkpICsgXG4gICAgIyBOZXcgY29kZSB0byBhZGQgdGhlIGRhdGEgcG9pbnRzIG92ZXIgdGhlIG90aGVyIHBsb3RcbiAgICBnZW9tX3BvaW50KGRhdGEgPSBleHBlcmltZW50NzIsXG4gICAgICAgICAgICAgICAgICAgIGFlcyh4PVRyZWF0bWVudCx5PXBILGZpbGw9VHJlYXRtZW50KSxcbiAgICAgICAgICAgICAgICAgICAgcG9zaXRpb249cG9zaXRpb25faml0dGVyKHdpZHRoPS4yKSxcbiAgICAgICAgICAgICAgICAgICAgcGNoID0gMjEpXG5gYGAifQ== -->

```r
ggplot(data = summary_experiment,     
       aes(x = Treatment, y = average_pH))+
    # We added alpha=0.6 outside of the aesthetics to make 
    # the bars less visible and help highlight the data points 
    geom_col(aes(fill = Treatment),alpha = 0.6)+
    geom_errorbar(aes(ymin = low, ymax = high), width = 0.1)+
    labs(y="Average pH", title = "Leaf pH at 72 hours by Treatment",
         # We also add a subtitle to make the meaning of the 
         # columns clear
         subtitle = "(Columns indicate the mean pH for each group.)")+
    scale_y_continuous(breaks = seq(0,7,by = 1)) + 
    # New code to add the data points over the other plot
    geom_point(data = experiment72,
                    aes(x=Treatment,y=pH,fill=Treatment),
                    position=position_jitter(width=.2),
                    pch = 21)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Mixing violin plots and boxplots

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZ2dwbG90KGRhdGEgPSBleHBlcmltZW50NzIsICAgICBcbiAgICAgICBhZXMoeCA9IFRyZWF0bWVudCwgeSA9IHBIKSkrXG4gICAgZ2VvbV9ib3hwbG90KCkrXG4gICAgZ2VvbV92aW9saW4oYWVzKGZpbGwgPSBUcmVhdG1lbnQpLGFscGhhID0gLjMsY29sb3I9TkEpK1xuICAgIGxhYnMoeT1cInBIXCIsIHRpdGxlID0gXCJNaXggYm94cGxvdHMgYW5kIHZpb2xpbiBwbG90cyB0byBzZWUgdGhlXFxuZGlzdHJpYnV0aW9ucyBieSB0cmVhdG1lbnQgZ3JvdXBcIikrXG4gICAgc2NhbGVfeV9jb250aW51b3VzKGJyZWFrcyA9IHNlcSgwLDcsYnkgPSAxKSkrdGhlbWVfYncoKVxuYGBgIn0= -->

```r
ggplot(data = experiment72,     
       aes(x = Treatment, y = pH))+
    geom_boxplot()+
    geom_violin(aes(fill = Treatment),alpha = .3,color=NA)+
    labs(y="pH", title = "Mix boxplots and violin plots to see the\ndistributions by treatment group")+
    scale_y_continuous(breaks = seq(0,7,by = 1))+theme_bw()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->

