viz_part_01
================

import weather dataset

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2022-09-29 10:33:36 (8.401)

    ## file min/max dates: 1869-01-01 / 2022-09-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2022-09-29 10:33:40 (1.699)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2022-09-29 10:33:42 (0.95)

    ## file min/max dates: 1999-09-01 / 2022-09-30

## basic scatterplot

We’ll use the weather_df data throughout, so we’ll move straight into
defining aesthetic mappings. To create a basic scatterplot, we need to
map variables to the X and Y coordinate aesthetics:

``` r
ggplot(weather_df, aes(x = tmin, y = tmax))+
  geom_point()
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](viz_part_01_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

let’s make the same scatterplot, but different

``` r
weather_df %>% 
  drop_na() %>% 
  filter(name=="CentralPark_NY") %>% 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_point()
```

![](viz_part_01_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

let’s keep making the same plot but different

``` r
weather_scatterplot = 
  weather_df %>% 
  drop_na() %>% 
  ggplot(aes(x=tmin,y=tmax))

weather_scatterplot + 
  geom_point()
```

![](viz_part_01_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

\##let’s fancy this up

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax,color=name)) +
  geom_point()
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](viz_part_01_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax)) +
  geom_point(aes(color=name))+
  geom_smooth()
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).
    ## Removed 15 rows containing missing values (geom_point).

![](viz_part_01_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax,color=name)) +
  geom_point(alpha=.3)+ #alpha transparancy 0~1
  geom_smooth(se=FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).
    ## Removed 15 rows containing missing values (geom_point).

![](viz_part_01_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

maybe make seperate panels

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax,color=name)) +
  geom_point(alpha=.3)+ #alpha transparancy 0~1
  geom_smooth(se=FALSE)+
  facet_grid(. ~ name) #row ~ column
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](viz_part_01_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax,color=name)) +
  geom_point(alpha=.3)+ #alpha transparancy 0~1
  geom_smooth(se=FALSE)+
  facet_grid(name ~ .) #row ~ column, . is a placeholder
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).
    ## Removed 15 rows containing missing values (geom_point).

![](viz_part_01_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

`tmax` vs `tmin` is boring. spicing it up

``` r
weather_df %>% 
  ggplot(aes(x=date,y=tmax,color=name))+
  geom_point(aes(size=prcp),alpha=.3)+
  geom_smooth(se=FALSE)+
  facet_grid(.~name)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))#rotate x axis labels 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 3 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](viz_part_01_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## some quick stuff

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax))+
  geom_hex()
```

    ## Warning: Removed 15 rows containing non-finite values (stat_binhex).

![](viz_part_01_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
