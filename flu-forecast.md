STAT 626 Time Series - Group 1 Project - Forecasting the Flu
================
Group 1 / Alex Hughes

## Libraries and plot theme

``` r
library(tidyverse)
library(astsa)
theme_set(theme_bw())
```

## Flu data

``` r
flu <- read_csv("VIW_FNT.csv")
head(flu)
```

    # A tibble: 6 × 49
      WHOREGION FLUSEASON HEMISPH…¹ ITZ   COUNT…² COUNT…³ ISO_WEEK…⁴ ISO_Y…⁵ ISO_W…⁶
      <chr>     <chr>     <chr>     <chr> <chr>   <chr>   <date>       <dbl>   <dbl>
    1 EUR       NH        NH        FLU_… PRT     Portug… 2003-12-15    2003      51
    2 AMR       YR        NH        FLU_… HND     Hondur… 2009-10-12    2009      42
    3 AMR       YR        NH        FLU_… HND     Hondur… 2016-01-25    2016       4
    4 AFR       YR        NH        FLU_… MLI     Mali    2013-11-25    2013      48
    5 EUR       NH        NH        FLU_… IRL     Ireland 2000-11-06    2000      45
    6 EMR       YR        NH        FLU_… JOR     Jordan  2019-01-28    2019       5
    # … with 40 more variables: MMWR_WEEKSTARTDATE <date>, MMWR_YEAR <dbl>,
    #   MMWR_WEEK <dbl>, ORIGIN_SOURCE <chr>, SPEC_PROCESSED_NB <dbl>,
    #   SPEC_RECEIVED_NB <dbl>, AH1N12009 <dbl>, AH1 <dbl>, AH3 <dbl>, AH5 <dbl>,
    #   AH7N9 <dbl>, ANOTSUBTYPED <dbl>, ANOTSUBTYPABLE <dbl>,
    #   AOTHER_SUBTYPE <dbl>, AOTHER_SUBTYPE_DETAILS <dbl>, INF_A <dbl>,
    #   BVIC_2DEL <dbl>, BVIC_3DEL <dbl>, BVIC_NODEL <dbl>, BVIC_DELUNK <dbl>,
    #   BYAM <dbl>, BNOTDETERMINED <dbl>, INF_B <dbl>, INF_ALL <dbl>, …

## Aggregate across countries to create flu count by date time series datasets for Influenza A, B, and all strains

``` r
InfA <- flu %>% 
  group_by(ISO_WEEKSTARTDATE) %>%
  summarise(total_flu = sum(INF_A, na.rm = T))

InfB <- flu %>%
  group_by(ISO_WEEKSTARTDATE) %>%
  summarise(total_flu = sum(INF_B, na.rm = T))

InfA$subtype <- "Inf_A"
InfB$subtype <- "Inf_B"

flu_df <- bind_rows(InfA, InfB)

InfAll <- flu %>% # contains more than just A and B; many strains
  group_by(ISO_WEEKSTARTDATE) %>%
  summarise(total_flu = sum(INF_ALL, na.rm = T))
```

## Plot Influenza A over time

``` r
# ggplot(flu, aes(x = ISO_WEEKSTARTDATE, y = INF_A))+
#   geom_line()

ggplot(InfA, aes(x = ISO_WEEKSTARTDATE, y = total_flu))+
  geom_line()+
  labs(x = "Time (in weeks)", y = "Influenza A number of specimens")
```

![](flu-forecast_files/figure-gfm/infA%20over%20time-1.png)

``` r
ggsave('InfA-by-year.png')
```

    Saving 7 x 5 in image

## Plot Influenza A vs Influenza B over time

``` r
ggplot(flu_df, aes(x = ISO_WEEKSTARTDATE, y = total_flu, color = subtype))+
  geom_line()+
  labs(x = "Time (in weeks)", y = "Number of specimens")
```

![](flu-forecast_files/figure-gfm/infA%20and%20infB%20over%20time-1.png)

``` r
ggsave('InfA-vs-InfB-time-series.png')
```

    Saving 7 x 5 in image
