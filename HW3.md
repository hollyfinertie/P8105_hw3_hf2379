HW3 - Visualization
================
Holly Finertie
10/14/2019

# Problem1

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────── tidyverse_conflicts() ──
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

## Plot of Items Ordered

``` r
plot_aisles = instacart %>% 
  count(aisle, name = "n_aisle") %>% 
  filter(n_aisle > 10000) %>% 
  arrange((aisle)) %>% 
  mutate(
    aisle = str_to_title(aisle)) %>% 
  ggplot(aes(x = aisle, y = n_aisle)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n_aisle), hjust = -0.05, size = 1.5) +
  labs(
    title = "Number of Items Ordered in Aisles",
    x = "Aisle Name",
    y = "Total Items Ordered",
    caption = "Data from instacart") +
  scale_y_continuous(limits = c(0, 160000)) +
  theme(text = element_text(size = 7))

plot_aisles + coord_flip()
```

![](HW3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Most Popular Items in Each Row

``` r
top3_products = instacart %>% 
  select(aisle, product_name) %>% 
  filter(
    aisle == "baking ingredients" | 
    aisle == "dog food care" | 
    aisle == "packaged vegetables fruits") %>% 
  group_by(aisle) %>% 
  count(product_name, name = "n_product") %>% 
  filter(min_rank(desc(n_product)) < 4) %>% 
  rename(
    "Aisle Name" = aisle,
    "Product Name" = product_name, 
    "Count" = n_product
  ) %>% 
  knitr::kable()

top3_products
```

| Aisle Name                 | Product Name                                  | Count |
| :------------------------- | :-------------------------------------------- | ----: |
| baking ingredients         | Cane Sugar                                    |   336 |
| baking ingredients         | Light Brown Sugar                             |   499 |
| baking ingredients         | Pure Baking Soda                              |   387 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |    28 |
| dog food care              | Small Dog Biscuits                            |    26 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |    30 |
| packaged vegetables fruits | Organic Baby Spinach                          |  9784 |
| packaged vegetables fruits | Organic Blueberries                           |  4966 |
| packaged vegetables fruits | Organic Raspberries                           |  5546 |

## Pink Ladies and Coffee Ice Cream

``` r
apples_and_cream = instacart %>% 
  filter(product_name == "Pink Lady Apples" | product_name == "Coffee Ice Cream") %>% 
  group_by(order_dow, product_name) %>% 
  summarize(mean_hour_ordered = mean(order_hour_of_day)) %>% 
  ungroup(order_dow) %>% 
  mutate(order_dow = recode(order_dow, 
         `0` = "Monday", 
         `1` = "Tuesday", 
         `2` = "Wednesday", 
         `3` = "Thursday",
         `4` = "Friday", 
         `5` = "Saturday",
         `6` = "Sunday")) %>% 
  pivot_wider(
    names_from = "product_name", 
    values_from = "mean_hour_ordered"
  ) %>% 
  rename("Day of Week" = order_dow) %>% 
  knitr::kable()

apples_and_cream
```

| Day of Week | Coffee Ice Cream | Pink Lady Apples |
| :---------- | ---------------: | ---------------: |
| Monday      |         13.77419 |         13.44118 |
| Tuesday     |         14.31579 |         11.36000 |
| Wednesday   |         15.38095 |         11.70213 |
| Thursday    |         15.31818 |         14.25000 |
| Friday      |         15.21739 |         11.55172 |
| Saturday    |         12.26316 |         12.78431 |
| Sunday      |         13.83333 |         11.93750 |

# Problem 2

``` r
library(p8105.datasets)
data("brfss_smart2010")

brfss_smart2010 = brfss_smart2010 %>% 
  janitor::clean_names() %>% 
  filter(topic == "Overall Health") %>% 
  mutate(response = forcats::fct_relevel(response, c("Poor", "Fair","Good", "Very good", "Excellent")))
```

## 2002 states

``` r
states_2002 = brfss_smart2010 %>% 
  filter(year == 2002) %>% 
  group_by(locationabbr) %>% 
  summarize(n_location = n_distinct(locationdesc)) %>% 
  filter(n_location >= 7) %>% 
  rename(
    "State" = locationabbr, 
    "Number of Locations" = n_location
  ) %>% 
  knitr::kable()

states_2002
```

| State | Number of Locations |
| :---- | ------------------: |
| CT    |                   7 |
| FL    |                   7 |
| MA    |                   8 |
| NC    |                   7 |
| NJ    |                   8 |
| PA    |                  10 |

## 2010 states

``` r
states_2010 = brfss_smart2010 %>% 
  filter(year == 2010) %>% 
  group_by(locationabbr) %>% 
  summarize(n_location = n_distinct(locationdesc)) %>% 
  filter(n_location >= 7) %>% 
  rename(
    "State" = locationabbr, 
    "Number of Locations" = n_location
  ) %>% 
  knitr::kable()

states_2010
```

| State | Number of Locations |
| :---- | ------------------: |
| CA    |                  12 |
| CO    |                   7 |
| FL    |                  41 |
| MA    |                   9 |
| MD    |                  12 |
| NC    |                  12 |
| NE    |                  10 |
| NJ    |                  19 |
| NY    |                   9 |
| OH    |                   8 |
| PA    |                   7 |
| SC    |                   7 |
| TX    |                  16 |
| WA    |                  10 |

## Excellent Response

``` r
brfss_excellent = brfss_smart2010  %>% 
  filter(topic == "Overall Health" & response == "Excellent") %>% 
  group_by(year, locationabbr) %>% 
  mutate(
    mean_value = mean(data_value, na.rm = TRUE)) %>% 
  select(year, locationabbr, mean_value) %>% 
  distinct() %>% 
  ggplot(aes(x = year, y = mean_value)) +
  geom_line(aes(group = locationabbr, color = locationabbr))

brfss_excellent
```

![](HW3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Distribution of Data Values in NY

``` r
brfss_ny_state = brfss_smart2010 %>% 
  filter((year == 2010 | year == 2006) & locationabbr == "NY") %>% 
  ggplot(aes(x = locationdesc, y = data_value, fill = response)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

brfss_ny_state
```

![](HW3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Problem 3

``` r
accel_data = read_csv("./data/accel_data.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    activity_1:activity_1440, 
    names_to = "activity_minute", 
    values_to = "activity_count") %>% 
  mutate(day_type = case_when(
          (day == "Saturday" | day == "Sunday") ~ "Weekend", 
          TRUE ~ "Weekday"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.
