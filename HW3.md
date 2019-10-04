HW3 - Visualization
================
Holly Finertie
10/14/2019

# Problem1

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
data("instacart")
```

## Description of Instacart Data Set:

This data set contains 1384617 observations and 15 variables describing
order information like time and day order was placed, products ordered,
aisle where products are located, and days since last order was placed
per user id. For example, the individual with user id 5 ordered 9 items
of which most were from the produce department. They placed this order 6
days after their last order.

In total, there are 134 aisles and the most items are ordered from the
fresh vegetables.

``` r
plot_aisles = instacart %>% 
  count(aisle, name = "n_aisle") %>% 
  filter(n_aisle > 10000) %>% 
  arrange(n_aisle) %>% 
  ggplot(aes(x = aisle, y = n_aisle), color = aisle) +
  geom_point()

plot_aisles 
```

![](HW3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
