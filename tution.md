Tution
================
Gaurav Sharma
15/06/2020

# Load Data

``` r
tuition_cost <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   name = col_character(),
    ##   state = col_character(),
    ##   state_code = col_character(),
    ##   type = col_character(),
    ##   degree_length = col_character(),
    ##   room_and_board = col_double(),
    ##   in_state_tuition = col_double(),
    ##   in_state_total = col_double(),
    ##   out_of_state_tuition = col_double(),
    ##   out_of_state_total = col_double()
    ## )

``` r
diversity_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv") 
```

    ## Parsed with column specification:
    ## cols(
    ##   name = col_character(),
    ##   total_enrollment = col_double(),
    ##   state = col_character(),
    ##   category = col_character(),
    ##   enrollment = col_double()
    ## )

``` r
diversity_school <- diversity_raw %>%
  filter(category == "Total Minority") %>%
  mutate(TotalMinority = enrollment / total_enrollment)
```

# Explore Data

``` r
tuition_cost %>% 
    tabyl(type)
```

    ##        type    n      percent
    ##  For Profit  107 0.0359905819
    ##       Other    1 0.0003363606
    ##     Private 1281 0.4308779011
    ##      Public 1584 0.5327951564

``` r
diversity_school %>% 
    ggplot(aes(TotalMinority)) +
    geom_histogram() +
    geom_vline(xintercept =  median(diversity_school$TotalMinority), color = 'pink', size = 3) +
    geom_vline(xintercept =  mean(diversity_school$TotalMinority), color = 'blue', size = 3) +
  annotate("text", x = 0.43, y = 300, label = 'Mean', size = 5, color = 'blue')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](tution_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Wrangle the data to prepare it for modelling

``` r
university_df <- diversity_school %>% 
  transmute(diversity = case_when(TotalMinority > 0.3 ~ "High",
                                  TRUE ~ "Low"),
            name, state,
            total_enrollment) %>% 
  inner_join(tuition_cost %>% 
               select(name, type, degree_length,
                      in_state_tuition:out_of_state_total)) %>% 
  left_join(tibble(state = state.name, region = state.region)) %>% 
  select(-name, -state) %>% 
  mutate_if(is_character, factor)
```

    ## Joining, by = "name"

    ## Joining, by = "state"

``` r
skimr::skim(university_df)
```

|                                                  |                |
| :----------------------------------------------- | :------------- |
| Name                                             | university\_df |
| Number of rows                                   | 2159           |
| Number of columns                                | 9              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| factor                                           | 4              |
| numeric                                          | 5              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                            |
| :------------- | ---------: | -------------: | :------ | --------: | :------------------------------------- |
| diversity      |          0 |              1 | FALSE   |         2 | Low: 1241, Hig: 918                    |
| type           |          0 |              1 | FALSE   |         3 | Pub: 1145, Pri: 955, For: 59           |
| degree\_length |          0 |              1 | FALSE   |         2 | 4 Y: 1296, 2 Y: 863                    |
| region         |          0 |              1 | FALSE   |         4 | Sou: 774, Nor: 543, Nor: 443, Wes: 399 |

**Variable type: numeric**

| skim\_variable          | n\_missing | complete\_rate |     mean |       sd |   p0 |   p25 |   p50 |     p75 |  p100 | hist  |
| :---------------------- | ---------: | -------------: | -------: | -------: | ---: | ----: | ----: | ------: | ----: | :---- |
| total\_enrollment       |          0 |              1 |  6183.76 |  8263.64 |   15 |  1352 |  3133 |  7644.5 | 81459 | ▇▁▁▁▁ |
| in\_state\_tuition      |          0 |              1 | 17044.02 | 15460.76 |  480 |  4695 | 10161 | 28780.0 | 59985 | ▇▂▂▁▁ |
| in\_state\_total        |          0 |              1 | 23544.64 | 19782.17 |  962 |  5552 | 17749 | 38519.0 | 75003 | ▇▅▂▂▁ |
| out\_of\_state\_tuition |          0 |              1 | 20797.98 | 13725.29 |  480 |  9298 | 17045 | 29865.0 | 59985 | ▇▆▅▂▁ |
| out\_of\_state\_total   |          0 |              1 | 27298.60 | 18220.62 | 1376 | 11018 | 23036 | 40154.0 | 75003 | ▇▅▅▂▁ |

# Lets do some visualization

``` r
university_df %>% 
  ggplot(aes(type, in_state_tuition, fill = diversity)) +
  facet_wrap(~region) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar_format())
```

![](tution_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
university_df %>% 
  ggplot(aes(type, total_enrollment, fill = diversity)) +
  geom_boxplot() +
  scale_y_log10()
```

![](tution_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

# lets split the data

``` r
set.seed(1234)

uni_split <- university_df %>% 
  initial_split(strata = diversity)

uni_train <- training(uni_split)
uni_test <- testing(uni_split)
```

# Create a recipe

``` r
uni_recipe <- recipe(diversity~., data = uni_train) %>%
  step_corr(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>% 
  prep()
  
uni_recipe 
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          8
    ## 
    ## Training data contained 1620 data points and no missing data.
    ## 
    ## Operations:
    ## 
    ## Correlation filter removed in_state_tuition, ... [trained]
    ## Dummy variables from type, degree_length, region [trained]
    ## Zero variance filter removed no terms [trained]
    ## Centering and scaling for total_enrollment, ... [trained]

# Lets create a few models for classification

``` r
# Logistic Regression
glm_spec <- logistic_reg() %>% 
  set_engine(engine = 'glm') %>% 
  set_mode(mode = 'classification')

glm_fit <- glm_spec %>% 
  fit(diversity~., data = juice(uni_recipe))

glm_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  10ms 
    ## 
    ## Call:  stats::glm(formula = formula, family = stats::binomial, data = data)
    ## 
    ## Coefficients:
    ##           (Intercept)       total_enrollment     out_of_state_total  
    ##                0.3704                -0.4581                 0.5074  
    ##          type_Private            type_Public  degree_length_X4.Year  
    ##               -0.1656                 0.2058                 0.2082  
    ##          region_South   region_North.Central            region_West  
    ##               -0.5175                 0.3004                -0.5363  
    ## 
    ## Degrees of Freedom: 1619 Total (i.e. Null);  1611 Residual
    ## Null Deviance:       2210 
    ## Residual Deviance: 1859  AIC: 1877

``` r
# nearest neighour
knn_spec <- nearest_neighbor() %>% 
  set_engine(engine = 'kknn') %>% 
  set_mode(mode = 'classification')

knn_fit <- knn_spec %>% 
  fit(diversity~., data = juice(uni_recipe))

knn_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  71ms 
    ## 
    ## Call:
    ## kknn::train.kknn(formula = formula, data = data, ks = 5)
    ## 
    ## Type of response variable: nominal
    ## Minimal misclassification: 0.3277778
    ## Best kernel: optimal
    ## Best k: 5

``` r
# Decision Tree
tree_spec <- decision_tree() %>% 
  set_engine(engine = 'rpart') %>% 
  set_mode(mode = 'classification')

tree_fit <- tree_spec %>% 
  fit(diversity~., data = juice(uni_recipe))

tree_fit
```

    ## parsnip model object
    ## 
    ## Fit time:  30ms 
    ## n= 1620 
    ## 
    ## node), split, n, loss, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ##  1) root 1620 689 Low (0.4253086 0.5746914)  
    ##    2) region_North.Central< 0.5346496 1192 586 High (0.5083893 0.4916107)  
    ##      4) out_of_state_total< -0.7087237 418 130 High (0.6889952 0.3110048) *
    ##      5) out_of_state_total>=-0.7087237 774 318 Low (0.4108527 0.5891473)  
    ##       10) out_of_state_total< 0.35164 362 180 Low (0.4972376 0.5027624)  
    ##         20) region_South>=0.3002561 212  86 High (0.5943396 0.4056604)  
    ##           40) degree_length_X4.Year>=-0.2001293 172  62 High (0.6395349 0.3604651) *
    ##           41) degree_length_X4.Year< -0.2001293 40  16 Low (0.4000000 0.6000000) *
    ##         21) region_South< 0.3002561 150  54 Low (0.3600000 0.6400000)  
    ##           42) region_West>=0.8128302 64  28 High (0.5625000 0.4375000) *
    ##           43) region_West< 0.8128302 86  18 Low (0.2093023 0.7906977) *
    ##       11) out_of_state_total>=0.35164 412 138 Low (0.3349515 0.6650485)  
    ##         22) region_West>=0.8128302 88  38 High (0.5681818 0.4318182)  
    ##           44) out_of_state_total>=1.547681 30   5 High (0.8333333 0.1666667) *
    ##           45) out_of_state_total< 1.547681 58  25 Low (0.4310345 0.5689655) *
    ##         23) region_West< 0.8128302 324  88 Low (0.2716049 0.7283951) *
    ##    3) region_North.Central>=0.5346496 428  83 Low (0.1939252 0.8060748)  
    ##      6) out_of_state_total< -1.19287 17   5 High (0.7058824 0.2941176) *
    ##      7) out_of_state_total>=-1.19287 411  71 Low (0.1727494 0.8272506) *

# Lets compare these models using resampling

``` r
set.seed(123)
folds <- juice(uni_recipe) %>% 
  vfold_cv(strata = diversity)

set.seed(234)
glm_rs <- fit_resamples(glm_spec,
              diversity~.,
              resamples = folds,
              metrics = metric_set(roc_auc, spec, sens, precision, recall, accuracy),
              control = control_resamples(save_pred = T))

glm_rs %>% 
  collect_metrics()
```

    ## # A tibble: 6 x 5
    ##   .metric   .estimator  mean     n std_err
    ##   <chr>     <chr>      <dbl> <int>   <dbl>
    ## 1 accuracy  binary     0.681    10 0.00950
    ## 2 precision binary     0.629    10 0.00989
    ## 3 recall    binary     0.607    10 0.0209 
    ## 4 roc_auc   binary     0.758    10 0.00916
    ## 5 sens      binary     0.607    10 0.0209 
    ## 6 spec      binary     0.737    10 0.00638

``` r
set.seed(234)
knn_rs <- fit_resamples(knn_spec,
              diversity~.,
              resamples = folds,
              metrics = metric_set(roc_auc, spec, sens, precision, recall, accuracy),
              control = control_resamples(save_pred = T))

knn_rs %>% 
  collect_metrics()
```

    ## # A tibble: 6 x 5
    ##   .metric   .estimator  mean     n std_err
    ##   <chr>     <chr>      <dbl> <int>   <dbl>
    ## 1 accuracy  binary     0.675    10 0.00814
    ## 2 precision binary     0.623    10 0.0106 
    ## 3 recall    binary     0.599    10 0.0115 
    ## 4 roc_auc   binary     0.732    10 0.00639
    ## 5 sens      binary     0.599    10 0.0115 
    ## 6 spec      binary     0.730    10 0.0100

``` r
set.seed(234)
tree_rs <- fit_resamples(tree_spec,
              diversity~.,
              resamples = folds,
              metrics = metric_set(roc_auc, spec, sens, precision, recall, accuracy),
              control = control_resamples(save_pred = T))

tree_rs %>% 
  collect_metrics()
```

    ## # A tibble: 6 x 5
    ##   .metric   .estimator  mean     n std_err
    ##   <chr>     <chr>      <dbl> <int>   <dbl>
    ## 1 accuracy  binary     0.690    10 0.00658
    ## 2 precision binary     0.636    10 0.00756
    ## 3 recall    binary     0.636    10 0.0175 
    ## 4 roc_auc   binary     0.722    10 0.00599
    ## 5 sens      binary     0.636    10 0.0175 
    ## 6 spec      binary     0.730    10 0.00901

# Lets visualize the roc auc curve

``` r
glm_rs %>%
  unnest(.predictions) %>%
  mutate(model = "glm") %>%
  bind_rows(knn_rs %>%
              unnest(.predictions) %>%
              mutate(model = "knn")) %>%
  bind_rows(tree_rs %>%
              unnest(.predictions) %>%
              mutate(model = "tree")) %>% 
  group_by(model) %>% 
  roc_curve(diversity, .pred_High) %>% 
  autoplot()
```

![](tution_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Test the model on test data on logistic model

``` r
glm_fit %>% 
  predict(new_data = bake(uni_recipe, new_data = uni_test), type = 'prob') %>% 
  mutate(truth = uni_test$diversity) %>% 
  roc_auc(truth, .pred_High)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.756
