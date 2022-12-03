HW6
================
Soungbin Yim
2022-11-29

Problem 2 The Washington Post has gathered data on homicides in 50 large
U.S. cities and made the data available through a GitHub repository
here. You can read their accompanying article here.

Create a city_state variable (e.g. “Baltimore, MD”), and a binary
variable indicating whether the homicide is solved. Omit cities Dallas,
TX; Phoenix, AZ; and Kansas City, MO – these don’t report victim race.
Also omit Tulsa, AL – this is a data entry mistake. For this problem,
limit your analysis those for whom victim_race is white or black. Be
sure that victim_age is numeric.

``` r
homicide_df =
  read_csv("./homicide-data.csv") %>%
  mutate(
    victim_age = recode(victim_age,"Unknown" = ""),
    victim_age = as.numeric(victim_age),
    victim_race = fct_relevel(victim_race, "White"),
    city_state = str_c(city, ", ", state), 
    resolved = as.numeric(disposition == "Closed by arrest"),
  ) %>%
  filter(!(city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL"))&
         victim_race %in% c("White", "Black")
  )
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

For the city of Baltimore, MD, use the glm function to fit a logistic
regression with resolved vs unresolved as the outcome and victim age,
sex and race as predictors. Save the output of glm as an R object; apply
the broom::tidy to this object; and obtain the estimate and confidence
interval of the adjusted odds ratio for solving homicides comparing male
victims to female victims keeping all other variables fixed.

``` r
baltimore_df = 
  homicide_df %>% 
  filter(city == "Baltimore") %>% 
  select(resolved, victim_age, victim_race, victim_sex)
```

``` r
fit_logistic = 
  baltimore_df %>% 
  glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial()) 

fit_logistic %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate),
         LCL = exp(estimate - 1.96*std.error),
         UCL = exp(estimate + 1.96*std.error)) %>%
  select(term, log_OR = estimate, OR, LCL, UCL) %>% 
  knitr::kable(digits = 3)
```

| term             | log_OR |    OR |   LCL |   UCL |
|:-----------------|-------:|------:|------:|------:|
| (Intercept)      |  1.152 | 3.164 | 1.989 | 5.031 |
| victim_age       | -0.007 | 0.993 | 0.987 | 1.000 |
| victim_raceBlack | -0.842 | 0.431 | 0.306 | 0.607 |
| victim_sexMale   | -0.854 | 0.426 | 0.325 | 0.558 |

Now run glm for each of the cities in your dataset, and extract the
adjusted odds ratio (and CI) for solving homicides comparing male
victims to female victims. Do this within a “tidy” pipeline, making use
of purrr::map, list columns, and unnest as necessary to create a
dataframe with estimated ORs and CIs for each city.

Create a plot that shows the estimated ORs and CIs for each city.
Organize cities according to estimated OR, and comment on the plot.

``` r
city_glm = 
  homicide_df %>%
  nest(data = -city_state) %>%
  mutate(
    models = map(data, ~glm(resolved ~ victim_age + victim_sex + victim_race, data = ., family = binomial())),
    results = map(models, broom::tidy)
    ) %>%
  select(city_state, models, results) %>%
  unnest(cols = results) %>%
  filter(term == "victim_sexMale") %>%
  mutate(
    OR = exp(estimate),
    LCL = exp(estimate - 1.96*std.error),
    UCL = exp(estimate + 1.96*std.error)
  ) %>%
   select(city_state, OR, LCL, UCL)
```

Problem 3 In this problem, you will analyze data gathered to understand
the effects of several variables on a child’s birthweight. This dataset,
available here, consists of roughly 4000 children and includes the
following variables:

babysex: baby’s sex (male = 1, female = 2) bhead: baby’s head
circumference at birth (centimeters) blength: baby’s length at birth
(centimeteres) bwt: baby’s birth weight (grams) delwt: mother’s weight
at delivery (pounds) fincome: family monthly income (in hundreds,
rounded) frace: father’s race (1 = White, 2 = Black, 3 = Asian, 4 =
Puerto Rican, 8 = Other, 9 = Unknown) gaweeks: gestational age in weeks
malform: presence of malformations that could affect weight (0 = absent,
1 = present) menarche: mother’s age at menarche (years) mheigth:
mother’s height (inches) momage: mother’s age at delivery (years) mrace:
mother’s race (1 = White, 2 = Black, 3 = Asian, 4 = Puerto Rican, 8 =
Other) parity: number of live births prior to this pregnancy pnumlbw:
previous number of low birth weight babies pnumgsa: number of prior
small for gestational age babies ppbmi: mother’s pre-pregnancy BMI ppwt:
mother’s pre-pregnancy weight (pounds) smoken: average number of
cigarettes smoked per day during pregnancy wtgain: mother’s weight gain
during pregnancy (pounds) Load and clean the data for regression
analysis (i.e. convert numeric to factor where appropriate, check for
missing data, etc.).

Propose a regression model for birthweight. This model may be based on a
hypothesized structure for the factors that underly birthweight, on a
data-driven model-building process, or a combination of the two.
Describe your modeling process and show a plot of model residuals
against fitted values – use add_predictions and add_residuals in making
this plot.

Compare your model to two others:

One using length at birth and gestational age as predictors (main
effects only) One using head circumference, length, sex, and all
interactions (including the three-way interaction) between these Make
this comparison in terms of the cross-validated prediction error; use
crossv_mc and functions in purrr as appropriate.

Note that although we expect your model to be reasonable, model building
itself is not a main idea of the course and we don’t necessarily expect
your model to be “optimal”.