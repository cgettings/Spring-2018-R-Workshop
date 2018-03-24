---
title: "R Analyses"
output:
  html_notebook:
    highlight: tango
    theme: yeti
  html_document:
    df_print: paged
  pdf_document: default
editor_options:
  chunk_output_type: inline
---


# Getting started

## Loading packages

```{r}
library(lsr)
library(readr)
library(Hmisc)
library(car)
library(dplyr)
library(DescTools)
library(psych)
library(haven)
library(broom)
```


## Loading data

### From OSF

```{r}
# affect.data <- read_csv("https://osf.io/ahda9/?action=download")
# affect.data <- read_sav("https://osf.io/n88bc/?action=download")


# From your working directory

# setwd("~/BASP/Fall 2016/R Workshop")

# affect.data <- readRDS("affect.data.RDS")
# affect.data <- read_csv("affect.data.RDS")
# affect.data <- read_sav("affect.data.RDS")
```

##
## These data come from an experiment Timothy Luke and I ran exploring the
##  viability of different dissonance reduction / threat compensation strategies.
##  
## In this data set are 5 variables:
##
##  ID:                     Participant ID
##  CondThreat:             Threat manipulation: Belief Threat Essay vs. Neutral Essay
##  CondDiscomfort:         Timing of Negative Affect: Before compensation vs. After
##  dissonance.1.scale:     Negative affect measurement (range 1-7)
##  Issue.imp_scale:        Scale measuring belief importance (range 0-100)
##


#===#===#===#===#===#===#===#===#===#===#
#  ---- Data structure and classes ----
#===#===#===#===#===#===#===#===#===#===#

##
## Check on the structure of the data
##

```{r}
str(affect.data)
head(affect.data)
```

##
## This will show you the classes of the data contained in the data frame.
##
## The condition indicators `CondThreat` and `CondDiscomfort` are "factors", 
##  which are analogous to "nominal" variables in SPSS.
##
## I set them as factors in the original dataset, and these properties of the data
##  are preserved in the RDS file.
##
## They are not preserved in the .csv file, and so if you loaded the data from the 
##  .csv file, you will need to change the condition indicators to be factors.
##
## You do this by "coercing" them into another class, using the coercion function
##  `as.factor()`.
##

```{r}
affect.data$CondThreat      <- as.factor(affect.data$CondThreat)
affect.data$CondDiscomfort  <- as.factor(affect.data$CondDiscomfort)
```

##
## With the $ operator, we're specifying that we want to change these columns in 
##  the data frame (left-hand side) using the same data (right-hand side). The 
##  sides don't have to be identical.
##


##
## Check the class of the factors to make sure
##


```{r}
class(affect.data$CondThreat)
class(affect.data$CondDiscomfort)
```
## Check their levels too

```{r}
levels(affect.data$CondThreat)
levels(affect.data$CondDiscomfort)
```

#=======================================#
# ---- Exploring the data ----
#=======================================#

#===#===#===#===#===#===#===#
#   ---- Descriptives ----
#===#===#===#===#===#===#===#

##
## Use the `describeBy()` function from the `psych` package to get lots of 
##  descriptive statistics for all of the variables, across all participants,
##  or split by condition using the `group = ` argument. 
##
## Whith no `group = ` argument specified, it will give a warning, but will return
##  descriptives for the whole sample. 
##


# Whole dataset

```{r}
describeBy(affect.data)
```


##
## You can use the `group = ` argument to split the data, and get stats within each 
##  level of grouping.
##


# Within both groups

## Separate output for each level

```{r}
describeBy(affect.data, group = list(affect.data$CondThreat))
describeBy(affect.data, group = list(affect.data$CondDiscomfort))
```
## Matrix output combining all levels

```{r}
describeBy(affect.data, group = list(affect.data$CondThreat), mat = TRUE, digits = 2)
describeBy(affect.data, group = list(affect.data$CondDiscomfort), mat = TRUE, digits = 2)
```

# At each level pair

## Separate output for each level

```{r}
describeBy(affect.data, group = list(affect.data$CondThreat, affect.data$CondDiscomfort))
```
## Matrix output combining all levels

```{r}
describeBy(affect.data, group = list(affect.data$CondThreat, affect.data$CondDiscomfort), mat = TRUE, digits = 2)
```


###----------------------------------------------------------###
#               ==== Confidence intervals ==== 
###----------------------------------------------------------###

##
## There are several ways to get confidence intervals for the mean. I like 
##  `MeanCI()` from the `DescTools` package, because it prints the mean along 
##  with the upper and lower bounds.
##

# Overall

```{r}
round( MeanCI(x = affect.data$dissonance.1.scale), digits = 2)
round( MeanCI(x = affect.data$Issue.imp_scale), digits = 2)
```

##
## There are a lot of ways to get the CI for each group, but all of them are
##  a bit confusing. The easiest way is probably to subset the data frame by
##  pulling out the conditions that you want with `[]`, and then assigning that 
##  subset to its own object. Then you can run `MeanCI()` on these new variables. 
##  `with()` is a way to avoid having to do `$` subsetting.
##


# Threat + Before

```{r}
diss.tb <- with(affect.data,
                
     dissonance.1.scale[CondThreat == "threat" & CondDiscomfort == "before"])
```

# Threat + After

```{r}
diss.ta <- with(affect.data,
                
     dissonance.1.scale[CondThreat == "threat" & CondDiscomfort == "after"])
```

# Neutral + Before

```{r}
diss.nb <- with(affect.data,
                
     dissonance.1.scale[CondThreat == "neutral" & CondDiscomfort == "before"])
```

# Neutral + After

```{r}
diss.na <- with(affect.data,
                
     dissonance.1.scale[CondThreat == "neutral" & CondDiscomfort == "after"])
```

# Combine together using `rbind()` which "BINDs" the "Rows" together, with row names

```{r}
diss.CI <-
    rbind(
        "threat:before" = MeanCI(diss.tb),
        "threat:after" = MeanCI(diss.ta),
        "neutral:before" = MeanCI(diss.nb),
        "neutral:after" = MeanCI(diss.na))
```
# Now round the output

```{r}
round(diss.CI, 2)
```

###----------------------------------------------------------###
#                   ==== Normality tests ==== 
###----------------------------------------------------------###

##
## To check if the data are distributed normally, you can run a Kolmogorov-Smirnov 
##  Test, or a Shapiro-Wilk Normality Test. The Shapiro-Wilk test is less 
##  complicated.
##


# Shapiro-Wilk Test

```{r}
shapiro.test(affect.data$dissonance.1.scale)
shapiro.test(affect.data$Issue.imp_scale)
```

# Kolmogorov-Smirnov Test

```{r}
ks.test(affect.data$dissonance.1.scale, "pnorm") # ***
ks.test(affect.data$Issue.imp_scale, "pnorm") # ***
```

# *** (`pnorm` is the probability distribution function for the normal distribtion.)

##
## Definitely not normal. To see why, further down we will use plots to see how 
##  the data are distributed
##


###----------------------------------------------------------###
#                   ==== Cell counts ====
###----------------------------------------------------------###

# Marginal cell counts

```{r}
table(affect.data$CondThreat)
table(affect.data$CondDiscomfort)
```

# Marginal cell proportions

```{r}
prop.table(table(affect.data$CondThreat))
prop.table(table(affect.data$CondDiscomfort))
```

# Contingency table of counts for all levels

```{r}
table(affect.data$CondThreat, affect.data$CondDiscomfort)
```

# Proportions for full contingency table

```{r}
prop.table(table(affect.data$CondThreat, affect.data$CondDiscomfort))
```

##
## You can also get marginal cell counts and proportions from the full contingency 
##  table by using`margin.table()` and `prop.table()`, specifying the margin you 
##  want - 1 for rows, 2 for columns.
##

# Row counts

```{r}
margin.table(table(affect.data$CondThreat, affect.data$CondDiscomfort), margin = 1)
```

# Within rows, column proportions

```{r}
prop.table(table(affect.data$CondThreat, affect.data$CondDiscomfort), margin = 1)
```

# Within columns, row proportions

```{r}
prop.table(table(affect.data$CondThreat, affect.data$CondDiscomfort), margin = 2)
```


##
## It doesn't make sense in this context, but if you had categorical data and wanted
##  to run a chi-squared test on the contingency table of cell counts, you would
##  wrap the `table()` function call inside of `chisq.test()`, like this:
##

```{r}
chisq.test(table(affect.data$CondThreat, affect.data$CondDiscomfort))
```


##
## This is because `chisq.test()` can take a matrix as input, and `table()` is
##  a convenient way of creating the cell count matrix we want in this case
##


#===#===#===#===#===#===#===#
#       ---- Plots ----
#===#===#===#===#===#===#===#

##
## The plotting functions in R are vastly superior to SPSS. There are a lot of 
##  reasons why this is, but here are 3:
##      1. You can produce many different kinds of plots with very simple code
##      2. You specify every detail of a plot from within the plot function call
##      3. Many classes of objects have "methods" for the generic `plot()` function,
##          which allows you to get specialized plots just by using `plot(object)`
##
## There are also packages with more complex plotting functions, like `ggplot2`, 
##  that only make this superiority to SPSS more stark. R is worth learning if only
##  for the plotting capabilities.
##
## For now, here are some basic descriptive plots:
##


###----------------------------------------------------------###
#                   ==== Barplots ====
###----------------------------------------------------------###

##
## You can use the same `table()` trick as above using the `barplot()` function
##


# Barplots for cell counts

```{r}
barplot(table(affect.data$CondThreat))
barplot(table(affect.data$CondDiscomfort))
```

# Barplot for the contingency table

```{r}
barplot(table(affect.data$CondThreat, affect.data$CondDiscomfort), beside = TRUE, legend.text = TRUE)
```
##
## The default is "stacked", which is ugly, so I chose the `beside = TRUE` option
##


###----------------------------------------------------------###
                    # ==== Histograms ==== 
###----------------------------------------------------------###

##
## You can also make nice-looking histograms of your continuous variables using `hist()`
##  The defaults work well.
##


# Negative affect

```{r}
hist(affect.data$dissonance.1.scale)
```

# Issue importance

```{r}
hist(affect.data$Issue.imp_scale)
```

##
## But you can make these prettier by customizing different options through arguments 
##  to `plot()`. I'll change the color, the title, and the x-axis label.
##


# Negative affect

```{r}
hist(affect.data$dissonance.1.scale, 
    col = "springgreen", 
    main = "Histogram of negative affect", 
    xlab = "Affect",
    las = 1)
```

# Issue importance

```{r}
hist(affect.data$Issue.imp_scale, 
    col = "#d3ce97", 
    main = "Histogram of issue importance", 
    xlab = "Importance",
    las = 1)
```

##
## You can also specify colors as hexadecimal values, e.g. #d3ce97, which exactly 
##  recreates the default histogram color in SPSS.
##

###----------------------------------------------------------###
#               ==== Density estimates ==== 
###----------------------------------------------------------###

##
## You can get a better sense of the distribution of the data by using kernel density
##  plots. Kernel density estimates take the data and "smooth" it by assuming that
##  the data points are drawn from a population with a specific probability distribution,
##  which implies that they have a certain statistical relationship. Based on the 
##  observed distribution of your data, it estimates the shape of the population 
##  distribution. The default is Gaussian (i.e., normal).
##
## As you can imagine, this dosn't work very well with small samples.
##
## Kernel desnity estimates are computed using `density()`. There is a plot method
##  for "density" class objects, which you can read about by typing ?plot.density.
##  So to get a density plot, you type `plot(density(x))`.
##  
## (Methods for classes are labeled as `function.class`, so if you want to know 
##  about a method for a given class, you type `?function.class`. To see what methods 
##  are available for a given function, type `methods(function)`, and for a specific 
##  class, type `methods(function, class)` )
##

```{r}
plot(density(affect.data$Issue.imp_scale))
plot(density(affect.data$dissonance.1.scale))
```

##
## You can specify options for both `density()` and `plot()`, so make sure you 
##  put them inside the correct pair of parentheses
##


#=======================================#
# ---- Analyses ----
#=======================================#


#===#===#===#===#===#===#===#
#   ---- Formulas ----
#===#===#===#===#===#===#===#

##
## Formulas were originally for fitting models, but since they're easy to 
##  understand and pretty flexible, they've been adpated for other purposes as well.
##
## I'll explain formulas by way of a description of the models they would be 
##  specifying if we were running a linear regression (although the code retains
##  the same basic meaning for other kinds of regression), what the code is saying, 
##  and also the corresponding SPSS syntax.
##
## I'll get into the real model fitting commands a little further down.
##


##################################################
#
# ***In these examples:***
#
#       "Y" is the dependent variable
#       "A" and "B" are factors 
#       "X1" and "X2" are continuous covariates
#
##################################################


###----------------------------------------------------------###
#           ==== Simple linear regression ==== 
###----------------------------------------------------------###

```
formula_1 <- Y ~ A
```

# `Y` regressed onto `A`
#
# SPSS: 
#   Y BY A

##
## When `A` has 2 groups, this is identical to a Student's t-test.
## When `A` has 3+ groups, this is basically a one-way ANOVA
##

```
formula_2 <- Y ~ X1
```

# `Y` regressed onto `X1`
#
# SPSS: 
#   Y WITH X1

##
## This is like running a Pearson correlation if you standardize the coefficients
##


#==== Multiple linear regression - No interaction ====#

```
formula_3 <- Y ~ X1 + X2
```

# `Y` regressed onto `X1` and `X2`
#
# SPSS: 
#   Y WITH X1 X2
#   /DESIGN = INTERCEPT X1 X2

```
formula_4 <- Y ~ A + B
```

# `Y` regressed onto `A` and `B`
#
# SPSS: 
#   Y BY A B
#   /DESIGN = INTERCEPT A B

##
## This is a 2-way ANOVA with just main effects
##

```
formula_5 <- Y ~ A + B + X1
```

# `Y` regressed onto `A`, `B` and X1
#
# SPSS: 
#   Y BY A B WITH X1
#   /DESIGN = INTERCEPT A B X1

##
## This is a 2-way *ANCOVA* with just main effects
##


#==== Multiple linear regression - With interactions ====#

```
formula_6 <- Y ~ X1 * X2
```

# `Y` regressed onto `X1` and `X2` and the interaction between `X1` and `X2`
#
# SPSS: 
#   Y WITH X1 X2
#   /DESIGN = INTERCEPT X1 X2 X1*X2

##
## The formula can also be specified like this:
##  formula_6 <- Y ~ X1 + X2 + X1:X2
##
## This lets you specify interaction models other than the full factorial, because you
##  can specify that a variable interacts with some variables but not others, 
##  e.g. every 2-way interaction, but no 3-way interactions. This will become
##  very useful in ANCOVA with interactions below (formula_8).
##

```
formula_7 <- Y ~ A * B
```

# `Y` regressed onto `A` and `B` and the interaction between `A` and `B`
#
# SPSS: 
#   Y BY A B
#   /DESIGN = INTERCEPT A B A*B

##
## The formula can also be specified like this:
##  formula_7 <- Y ~ A + B + A:B
##
## This is a 2-way ANOVA with main effects and interaction
##

```
formula_8 <- Y ~ A * B + X1
```

# `Y` regressed onto `A`, `B` AND X1, and the interaction between `A` and `B`
#
# SPSS: 
#   Y BY A B WITH X1
#   /DESIGN = INTERCEPT A B X1 A*B

##
## The formula can also be specified like this:
##  `formula_8 <- Y ~ A + B + X1 + A:B`
##
## This is a 2-way *ANCOVA* with main effects and interaction
##


#===#===#===#===#===#===#===#===#===#
#       ---- Correlations ----
#===#===#===#===#===#===#===#===#===#

##
## R has built-in correlation functions - one that produces a correlation table
##  that has no p-values (`cor()`), and another that tests a single correlation 
##  at a time (`cor.test()`). It's possible to write a loop that runs `cor.test()`
##  for each pair of variables in your dataset, but possibly the best aspect of R
##  is that most problems like this have been solved by people much better at stats
##  than we are, and they've collected their solutions into packages that we can
##  download.
##
## So anytime you have a problem like this, you should look to see if there's a
##  function in a package that solves it. There are a few ways to conduct such a 
##  search, in order of increasing specificity:
##      1. Within R, use "??" in front of a relevant term (e.g., `??correlation`),
##          or search in the RStudio "Help" pane.
##      2. Search on StackOverflow.com, a forum for programming questions that has
##          a huge repository of R questions.
##      3. Look on Rseek.org, a search engine that looks through blogs and user
##          forums dedicated to R
##      4. Ask Dr. Google. Apply the same rules as always: if you put enough words
##          or exact phrases, you'll eventually find what you're looking for.
##
## You can also ask me. Chances are that I've cycled through these 4 steps enough 
##  that I have an answer to your question, or I at least know what to look for.
##
## So, onto correlation...


###----------------------------------------------------------###
# ==== Are negative affect and importance correlated? ==== 
###----------------------------------------------------------###


# ==== Built-in `cor.test()` function ==== #


```{r}
cor.test(x = affect.data$Issue.imp_scale, y = affect.data$dissonance.1.scale)
```

# ==== `correlate()` from 'lsr` package ==== #

##
## `correlate()` takes a matrix or a data frame as input, it automatically 
##  filters out non-numeric variables, and it will give you a matrix of 
##  correlation coefficients with significance stars, along with a matrix of 
##  p-values, and a matrix of sample sizes. Just make sure you type `test = TRUE`.
##

```{r}
correlate(affect.data, test = TRUE)
```


##
## Obviously, there's only 1 pair of variables here, so a list of matrices is 
##  probably overkill.
##
## What would `correlate()` look like if there were multiple numeric variables?
##

```{r}
correlate(iris, test = TRUE)    # Using built-in `iris` dataset
```


# ==== Scatterplots ==== #

##
## You can also visualize these correlations by making scatterplots. The default
##  plot method when you supply 2 continuous variables is a scatterplot. You can specify
##  the variables with the "(x, y)" method, as in `cor.test()`, or the formula
##  method, as in `lm()`
##
## I often run a correlation test and make a scatterplot at the same time.
##


```{r}
cor.test(affect.data$Issue.imp_scale, affect.data$dissonance.1.scale)

plot(affect.data$dissonance.1.scale ~ affect.data$Issue.imp_scale)
```

##
## You can also add a regression fit line using the function `abline()`. If you
##  supply `abline()` with a formula inside of an `lm()` function call, it will
##  plot the best fit line.
##

```{r}
abline(lm(affect.data$dissonance.1.scale ~ affect.data$Issue.imp_scale), col = "red")
```

## Alternately, you can also take a model fit object and use that to plot the 
##  fit line. Note that this only works for simple linear regression, because
##  R is simply extracting the intercept and slope coefficients from the model 
##  and then plotting them.

```{r}
fit.1 <- lm(affect.data$dissonance.1.scale ~ affect.data$Issue.imp_scale)

abline(reg = fit.1, col = "green3")
```

#===#===#===#===#===#===#===#===#===#
#       ---- t-tests ----
#===#===#===#===#===#===#===#===#===#

###----------------------------------------------------------###
# ==== Are the conditions different? ==== 
###----------------------------------------------------------###

##
## Before diving into the ANOVA that is inevitably coming, you might like to see
##  if the conditions within each factor are different from each other. We'll 
##  run t-tests for this.
##
## R's t-test function calculates Welch's t-test by default. If you remember from
##  stats 1, this corrects for unequal variances by adjusting the degrees of freedom.
##  When the variances of the conditions are identical, it gives results that
##  are basically no different from Student's t-test. When variances are unequal,
##  it's more robust to Type-I error. Because of this, people recommend just using
##  Welch's t-test right at the start.
##
## There are two ways of specifying the t-test: the default method, which uses
##  `x = ` and `y = ` to specify two numeric vectors; and the "formula" method,
##  which takes the form `DV ~ IV`. If you use the formula method, you can only
##  do a 2-group Welch's t-test. If you specify `x` and `y` arguments, you can
##  enter `var.equal = TRUE` to do a Student's t-test, or `paired = TRUE` to do
##  a dependent t-test, or just supply 1 variable to `x` and `mu = 0` to do 
##  a 1-sample t-test.
##
## Because we're doing a 2-group t-test, we'll just use the formula method, 
##  because it's so much easier to specify.
##
## We'll also add in Cohen's d, using `cohensD()` from the `lsr` package. This
##  has almost identical specifications as `t.test()`
##


#==== Does Screening Issue importance differ between groups? ====#


# Between "Threat" conditions

```{r}
t.test(Issue.imp_scale ~ CondThreat, data = affect.data)

cohensD(Issue.imp_scale ~ CondThreat, data = affect.data)
```

# Between "Discomfort timing" conditions

```{r}
t.test(Issue.imp_scale ~ CondDiscomfort, data = affect.data)

cohensD(Issue.imp_scale ~ CondDiscomfort, data = affect.data)
```


##
## Issue importance is a measured covariate, so it's nice that it doesn't vary
##  between the groups. Thank you random assignment!
##


#==== Does negative affect differ between the groups? ====#


# Between "Threat" conditions

```{r}
t.test(dissonance.1.scale ~ CondThreat, data = affect.data)

cohensD(dissonance.1.scale ~ CondThreat, data = affect.data)
```

# Between "Discomfort timing" conditions

```{r}
t.test(dissonance.1.scale ~ CondDiscomfort, data = affect.data)

cohensD(dissonance.1.scale ~ CondDiscomfort, data = affect.data)
```

##
## If you need to get the 95% CI for Cohen's d, you can use the `ci.smd()` 
##  function in the `MBESS` package.
##
## These analyses are looking at marginal means for the conditions, which is to 
##  say, negative affect between threat conditions *averaging over* the timing 
##  of the measurement (before vs. after), and negative affect before vs. after
##  threat compensation *averaging over* threat and neutral essays. This is 
##  interesting (to Timothy and I, at least), but not the information that what 
##  we really want, which is how these two conditions interact. To do that, we'll
##  turn to ANOVA in the next section.
##
## First we'll do some plots.
##


###----------------------------------------------------------###
# ==== Plots ==== 
###----------------------------------------------------------###


#==== Barplots (for means) ====#

##
## You can get nicely formatted means plots with 95% CIs by default, using `bars()`
##  from the `lsr` package.
##


# Issue importance

```{r}
bars(formula = Issue.imp_scale ~ CondThreat, data = affect.data)
bars(formula = Issue.imp_scale ~ CondDiscomfort, data = affect.data)
```

# Negative affect

```{r}
bars(formula = dissonance.1.scale ~ CondThreat, data = affect.data)
bars(formula = dissonance.1.scale ~ CondDiscomfort, data = affect.data)
```


##
## Instead of barplots, you can also get box-and-whisker plots.
##

# Issue importance

```{r}
boxplot(Issue.imp_scale ~ CondThreat, data = affect.data, notch = TRUE)
boxplot(Issue.imp_scale ~ CondDiscomfort, data = affect.data, notch = TRUE)
```


# Negative affect

```{r}
boxplot(dissonance.1.scale ~ CondThreat, data = affect.data, notch = TRUE)
boxplot(dissonance.1.scale ~ CondDiscomfort, data = affect.data, notch = TRUE)
```

#===#===#===#===#===#===#===#===#===#
#   ---- Linear models ----
#===#===#===#===#===#===#===#===#===#


##
## Now, it's the big time.
##
## Model fitting is actually incredibly easy in R. This is where its object-
##  orientation is very useful. Model objects store all the model-related information 
##  in a hierarchical list, where they can be accessed by subsetting with the
##  usual methods -- [[]] or $ -- or extracted using "extractor" functions
##  like `resid()`, which returns raw residuals.
##


###----------------------------------------------------------###
# ==== Contrasts ==== 
###----------------------------------------------------------###


##
## Contrasts are very important to understand if you want to use R for model
##  fitting.
##
## Every factor has an "attribute" that defines its contrasts for model fitting.
##  The default contrast type in R is "treatment contrasts", which look like this:
##

# ==== Treatment contrasts ==== #

```{r}
contr.treatment(2)
contr.treatment(5)
```

##
## You can see that our 2 factors have these contrasts already set:
##

```{r}
contrasts(affect.data$CondThreat)
contrasts(affect.data$CondDiscomfort)
```


##
## In SPSS, these are called "simple" contrasts.
##

```
/CONTRAST(A) = SIMPLE(1)
```

# ==== Orthogonal contrasts ==== #

##
## The default contrasts used in SPSS are "deviation" contrasts. This takes each 
##  level of the factor (except the last, which is set as the reference group) 
##  and compares it to the Grand Mean (aka the mean of the group means).
##
## In SPSS syntax, this is:
##

```
/CONTRAST(A) = DEVIATION
```

##
## These are preferred for a number of reasons having to do with multicollinearity
##  and identifiability, which you can read more about 
##  here: http://goanna.cs.rmit.edu.au/~fscholer/anova.php
##
## In R, these are called "sum to zero" contrasts, or `contr.sum()`. 
##

```{r}
contr.sum(2)
contr.sum(5)
```

##
## To get them in R, you have to set them manually.
##

```{r}
contrasts(affect.data$CondThreat) <- contr.sum(2)
contrasts(affect.data$CondDiscomfort) <- contr.sum(2)
```


##
## Check what you've done by running this:
##

```{r}
contrasts(affect.data$CondThreat)
contrasts(affect.data$CondDiscomfort)
```


# ==== Other contrasts ==== #

##
## R also has Helmert contrasts, for more complicated comparisons, and polynomial
##  contrasts, for repeated measures
##

```{r}
contr.poly(2)
contr.poly(5)
```

```{r}
contr.helmert(2)
contr.helmert(5)
```
 

##
## You can also define your own contrasts using matrices. For example, if you
##  wanted to change the reference condition in "sum to zero contrasts" from fist 
##  to last, with a 5-group factor, you could do this:
##

```{r}
fac.5 <- 1:5
fac.5 <- as.factor(fac.5)

contrasts(fac.5) <-
    
    matrix(
        c(-1, -1, -1, -1,
           1,  0,  0,  0,
           0,  1,  0,  0,
           0,  0,  1,  0,
           0,  0,  0,  1),
        nrow = 5,
        byrow = TRUE
    )
```

###----------------------------------------------------------###
# ==== Running the ANOVA model ==== 
###----------------------------------------------------------###

##
## Let's run an ANOVA with both factors, plus their interaction. I often put
##  the arguments and variables on different lines to improve readability.
##


```{r}
diss.anova <- 
    
    aov(dissonance.1.scale ~ 
            
            CondThreat,
            #CondDiscomfort +
            #CondThreat:CondDiscomfort,
        
        data = affect.data)
```

# ==== Summaries ==== #

```{r}
summary(diss.anova)
```

##
## Using `summary(diss.aov)` would get you Type-1 SS, which you don't want.
##  Instead use `Anova()` from the `car` package to get Type-2 and Type-3 SS.
##

```{r}
Anova(diss.anova, type = 2)
Anova(diss.anova, type = 3)
```

##
## I dislike the standard output that R produces. Luckily, other people have 
##  written packages that standardize and clarify it. The major package is
##  `broom` and the main function is `tidy()`, which has methods for lots
##  of different kinds of model output. To see what these are, type `methods(tidy)`
##


```{r}
tidy(Anova(diss.anova, type = 2))
tidy(Anova(diss.anova, type = 3))
```

##
## If you find yourself *really* wanting to round the output from `tidy()`, you
##  can use the `mutate_if()` function from `dplyr`, along with the "pipe operator".
##
## The pipe operator is pretty simple. First you take an object, like a data frame,
##  and then add `%>%` after it (Ctrl + Shift + M), and then after that, add any
##  function that can take that kind of object as its *first* argument.
##
## For example, say we want to see the structure of `affect.data`. We could type:
##

```{r}
str(affect.data)
```

##
## because `str()` has `object = ` as its first argument:
##

```{r}
str(object = affect.data)
```

##
## So, you can type
##

```{r}
affect.data %>% str()
```

##
## And get the exact same thing as before, because `%>%` is passing `affect.data`
##  as the first argument to `str()`.
##
##
## So, if you want to do some rounding, here's what you do:
##
##  After
##
##      tidy(Anova(diss.anova, type = 2))
##
##  Add
##
##      %>% mutate_if(is.numeric, round, digits = 2)
##

```{r}
tidy(Anova(diss.anova, type = 2)) %>% mutate_if(is.numeric, round, digits = 2)
```

##
## Which says "modify the numeric columns in this data frame by rounding them
##  to 2 digits." 
##

##
## If you want more info on `mutate_if()` or `dplyr`, check out the "data manipulation
##  cheatsheet" on OSF (https://osf.io/5c7du/?action=download) or in the 'Help'
##  menu in RStudio
##


### ANOVA Effect sizes ###

```{r}
round(etaSquared(diss.anova, type = 2), digits = 3)
round(etaSquared(diss.anova, type = 3), digits = 3)
```

```{r}
pairwise.t.test(x = affect.data$dissonance.1.scale, g = affect.data$CondThreat, p.adj = "bonf")

posthocPairwiseT(diss.anova, p.adj = "bonf")
```

# ==== Post-hoc tests ==== #


### Tukey HSD ###

```{r}
print(TukeyHSD(diss.anova), digits = 2)
```

###----------------------------------------------------------###
# ==== Overall Model stats ==== 
###----------------------------------------------------------###

```{r}
glance(diss.anova)
```

###----------------------------------------------------------###
# ==== Plots ==== 
###----------------------------------------------------------###

```{r}
bars(formula = Issue.imp_scale ~ CondDiscomfort + CondThreat, data = affect.data)
bars(formula = dissonance.1.scale ~ CondDiscomfort + CondThreat, data = affect.data)
```


```{r}
boxplot(Issue.imp_scale ~ CondThreat + CondDiscomfort, data = affect.data, notch = TRUE)
boxplot(dissonance.1.scale ~ CondThreat + CondDiscomfort, data = affect.data, notch = TRUE)
```


###----------------------------------------------------------###
# ==== Diagnostic Plots ==== 
###----------------------------------------------------------###

```{r}
oldpar <- par(mfrow = c(2,2))

plot(diss.anova)

par(oldpar)
```


###----------------------------------------------------------###
# ==== Diagnostics ==== 
###----------------------------------------------------------###

```{r}
diss.anova.augment <- augment(diss.anova)
str(diss.anova.augment)
```



