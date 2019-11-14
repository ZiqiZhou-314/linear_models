bootstrapping
================
Ziqi Zhou
11/14/2019

``` r
set.seed(1)
n_samp = 250

sim_df_const = 
  tibble(
    x = rnorm(n_samp, 1, 1),
    error = rnorm(n_samp, 0, 1),
    y = 2 + 3 * x + error
  )

sim_df_nonconst = sim_df_const %>% 
  mutate(
  error = error * .75 * x,
  y = 2 + 3 * x + error
)
```

``` r
sim_df = 
  bind_rows(const = sim_df_const, nonconst = sim_df_nonconst, .id = "data_source") 

sim_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = .5) +
  stat_smooth(method = "lm") +
  facet_grid(~data_source) 
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

Fit two models

``` r
sim_df_const %>% 
  lm(y ~x, data = . ) %>% 
  broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.98    0.0981      20.2 3.65e- 54
    ## 2 x               3.04    0.0699      43.5 3.84e-118

``` r
sim_df_nonconst %>% 
  lm(y ~x, data = . ) %>% 
  broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.93    0.105       18.5 1.88e- 48
    ## 2 x               3.11    0.0747      41.7 5.76e-114

The R here the outcome is wrong. in the sim\_df\_nonconst std.error. se
is not correct here. though other things might be true. Because of the
assumption might be not true but we analysed use those assumption.

\#\#Check in echo360.

## how can I bootstrap

Write a function to draw a bootstrap sample based on a dataframe.

``` r
sim_df_nonconst %>% 
  sample_frac(size = 1, replace = TRUE) %>%  #This is a wrapper around sample.int() to make it easy to 
  arrange(x)
```

    ## # A tibble: 250 x 3
    ##         x  error       y
    ##     <dbl>  <dbl>   <dbl>
    ##  1 -1.89   1.62  -2.04  
    ##  2 -1.29   1.40  -0.454 
    ##  3 -0.989 -1.97  -2.93  
    ##  4 -0.989 -1.97  -2.93  
    ##  5 -0.914 -0.908 -1.65  
    ##  6 -0.914 -0.908 -1.65  
    ##  7 -0.914 -0.908 -1.65  
    ##  8 -0.733  0.447  0.248 
    ##  9 -0.733  0.447  0.248 
    ## 10 -0.606 -0.106  0.0774
    ## # … with 240 more rows

``` r
boot_sample = function(df){
  sample_frac(df,size = 1, replace = TRUE)
}
```

``` r
boot_sample(df = sim_df_nonconst) %>% 
  ggplot(aes(x = x,y = y)) + 
  geom_point()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

organize the dataframe.\] keeping track all the different boot\_strap
sample

``` r
boot_straps = 
  tibble(
    strap_num = 1:1000,
    strap_sample = rerun(1000, boot_sample(df = sim_df_nonconst))
  ) 
```

do some kind of analysis what the slope looks like? slope distribution?

``` r
bootstrap_results = 
  boot_straps %>% 
  mutate(
    models = map(strap_sample, ~lm(y~x, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(-strap_sample,-models) %>% 
  unnest(results)
```

summarize these results

``` r
bootstrap_results %>% 
  group_by(term) %>% 
  summarize(se = sd(estimate))
```

    ## # A tibble: 2 x 2
    ##   term            se
    ##   <chr>        <dbl>
    ## 1 (Intercept) 0.0748
    ## 2 x           0.101

\#\#Try the modelr package. The difference is in the begining how the
sample generated?…?

``` r
boot_straps = 
  sim_df_nonconst %>% 
  modelr::bootstrap(1000) %>% 
  mutate(
    models = map(strap, ~lm(y~x, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(-strap,-models) %>% 
  unnest(results) %>% 
  group_by(term) %>% 
  summarize(se = sd(estimate))
```

## Revisit airbnb data.

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    boro = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(boro != "Staten Island") %>% 
  select(price, stars, boro, neighborhood, room_type)
```

``` r
nyc_airbnb %>% 
  ggplot(aes(x = stars, y = price, color = room_type)) + 
  geom_point() 
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />
However this is not constant and so many outliners.

What is the distrib of the slope.

Re-use the stuff we just did

``` r
nyc_airbnb %>% 
  filter(boro == "Manhattan") %>% 
  modelr::bootstrap(n = 1000) %>% 
  mutate(
    models = map(strap, ~lm(price ~ stars+ room_type, data = .x)),
    results = map(models, broom::tidy)) %>% 
  select(results) %>% 
  unnest(results) %>% 
  filter(term == "stars") %>% 
  ggplot(aes(x = estimate)) + geom_density()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-14-1.png" width="90%" />

It is skew.
