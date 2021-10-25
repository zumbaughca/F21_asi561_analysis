library(readxl)
library(tidyverse)
library(gridExtra)
library(grid)
library(lsmeans)
source('plots.R')
source('transformations.R')

df <- read_xlsx("ASI561 digestibility.xlsx",
                sheet = "Digestibility")

summary_stats <- df %>%
  select(!Pen) %>%
  group_by(Treatment) %>%
  summarise(
    across(
      .cols = is.numeric,
      .fns = list(
        avg = mean,
        SD = sd,
        min = min,
        max = max
      ),
      .names = "{col}_{fn}"
    )
  ) %>%
  pivot_longer(cols = !Treatment)


###### Linear Model ######
cp_fit <- lm(CP_dig ~ Treatment, data = df)

##### ANOVA #######
cp_anova <- anova(cp_fit)

df$asin_cp <- arcsine_transform(df$CP_dig)

cp_asin_fit <- lm(asin_cp ~ Treatment, data = df)
cp_asin_anova <- anova(cp_asin_fit)

cp_means <- lsmeans(cp_fit, pairwise ~ Treatment)
cp_asin_means <- summary(lsmeans(cp_asin_fit, ~ Treatment))
cp_asin_means[, c(2:3, 5:6)] <- lapply(cp_asin_means[, c(2:3, 5:6)], inverse_asin)

diagnostic_plots(cp_fit, "asin_cp")
