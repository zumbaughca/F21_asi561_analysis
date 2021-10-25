library(readxl) # Reading Excel files
library(tidyverse) # Data wrangling and plotting
library(gridExtra) # Creating panel plots with ggplot
library(grid)
library(emmeans) # Easy functions to get predicted means
library(bannerCommenter) # This provides an easy way to create banner comments
source('plots.R') # Read in our plots file
source('transformations.R') # Read in our transformation functions

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                           FITTING FUNCTIONS                         ###
###                                                                     ###
###########################################################################
###########################################################################

fit_model <- function(var) {
  fit <- lm(var ~ factor(Treatment), data = df)
}

############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###            DATA INPUT, INITIALIZATION, AND TRANSFORMATION            ###
###                                                                      ###
############################################################################
############################################################################
df <- read_xlsx('ASI561 digestibility.xlsx', sheet = "Digestibility") 

summary_stats <- df %>%
  select(!Pen) %>%
  group_by(Treatment) %>%
  summarise(
    across(.cols = is.numeric,
           .fns = list(avg = mean,
                       SD = sd,
                       max = max,
                       min = min),
           .names = "{col}_{fn}")
  ) %>%
  pivot_longer(cols = !Treatment)

df %>%
  select(!Pen) %>%
  group_by(Treatment) %>%
  summarise(across(
    .cols = is.numeric,
    .fns = list(avg = mean,
                SD = sd,
                max = max,
                min = min)
  ))



# Transform the input data and rename the new columns
df[, 8:12] <- lapply(df[, 3:7], arcsine_transform) %>%
  setNames(c("asin_cp", "asin_fat", "asin_ndf", "asin_adf", "asin_om"))

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 3:                             ###
###                    MODEL FITTING AND DIAGNOSTICS                    ###
###                                                                     ###
###########################################################################
###########################################################################

# Create a list of models of each variable
models <- map(df[, 8:12], fit_model)
untrans_models <- map(df[, 3:7], fit_model)

# Loop through models in the models list and create a plot pane for it
create_diagnostic_plots <- function(model, model_name) {
    suppressWarnings(diagnostic_plots(model, model_name)) 
}
############################################################################
############################################################################
###                                                                      ###
###                              SECTION 4:                              ###
###                  ANALYSIS OF VARIANCE AND CONTRASTS                  ###
###                                                                      ###
############################################################################
############################################################################

# First create a contrast matrix. This should be a list.
# The milo vs corn contrast throws a wrench in things. If we 
# were only doing polynomial contrasts, we could use contr.poly()
# to create orthogonal polynomial contrasts. Note that these contrasts
# are not orthogonal to each other.
contrs <- list(linear = c(-3, -1, 1, 3),
             quadratic = c(1, -1, -1, 1),
             milo_vs_corn = c(-3, 1, 1, 1))

# Conduct analysis of variance
anovas <- map(models, anova)
untrans_anova <- map(untrans_models, anova)

# Generate Least Squares means
means <- map2(models, names(models), function(x, y) {
  if (y == "asin_ndf") {
    return(emmeans(x, pairwise ~ Treatment))
  }
  emmeans(x, ~ Treatment)
})

untrans_means <- map2(untrans_models, names(untrans_models), function(x, y) {
  if (y == "NDF_dig") {
    return(emmeans(x, pairwise ~ Treatment))
  }
  emmeans(x, ~ Treatment)
})

# Test contrasts
contr_effect <- map2(means, names(means), function(x, y){
  if (y == "asin_ndf") {
    return(contrast(x$emmeans, contrs))
  }
  contrast(x, contrs)  
})

# Convert the predicted means back to the response scale.
# This involves back-transforming the values in means.
# Standard errors are invalidated and removed from the df.
response_means <- map2(means, names(means), function(x, y) {
  if (y == "asin_ndf") {
    x <- as.data.frame(x$emmeans)
    x[, c(2:3, 5:6)] <- lapply(x[, c(2:3, 5:6)], inverse_asin)
    return(as.data.frame(x) %>%
      select(!SE))
  }
  x <- as.data.frame(x)
  x[, c(2:3, 5:6)] <- lapply(x[, c(2:3, 5:6)], inverse_asin)
  as.data.frame(x) %>%
    select(!SE)
})

