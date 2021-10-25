############################################################################
############################################################################
###                                                                      ###
###                              SECTION 1:                              ###
###                    FUNCTIONS FOR DIAGNOSTIC PLOTS                    ###
###                                                                      ###
############################################################################
############################################################################

resid_vs_fit <- function(data, x, y) {
  x <- enquo(x)
  y <- enquo(y)
  ggplot(data = data,
         aes(x = !!x,
             y = !!y)) +
    geom_point() +
    geom_hline(yintercept = 0,
               color = "red",
               linetype = "dashed") +
    xlab("Fitted values") +
    ylab("Residuals")
}

gg_qqplot <- function(model_data) {
  ggplot(data = model_data, aes(sample = var)) +
    stat_qq() +
    stat_qq_line() +
    ylab("Sample Quantiles") +
    xlab("Theoretical Quantiles")
}

gg_scale_loc <- function(model, x, y) {
  x <- enquo(x)
  y <- enquo(y)
  ggplot(data = model,
         aes(x = !!x,
             y = sqrt(abs(!!y)))) +
    geom_point() +
    stat_smooth(method = "loess") +
    xlab("Fitted") +
    ylab(expression(sqrt('|Standardized Residuals|')))
}

gg_cooksd <- function(model) {
  ggplot(data = model,
         aes(x = seq_along(.cooksd),
             y = .cooksd)) +
    geom_bar(stat = "identity") +
    xlab("Observation") +
    ylab("Cook's Distance") +
    geom_hline(yintercept = mean(cooks.distance(model)),
               color = 'firebrick',
               linetype = 'dotted')
}

# Function for generating diagnostic grid
diagnostic_plots <- function(model, name) {
  temp <- data.frame(fitted = fitted(model),
                     residuals = resid(model),
                     std_residuals = rstandard(model))
  
  plot_title = paste("Diagnostic plots for:", name, sep = " ")
  
  # Residuals vs. fitted plot
  resid_vs_fit <- resid_vs_fit(data = temp,
                               x = fitted,
                               y = residuals)
  
  # QQ plot. Assesses normality of residuals. We want a straight line.
  qq <- gg_qqplot(model$model)
  
  # Scale-location plot. Another way to look at variance distribution
  scale_loc <- gg_scale_loc(model,
                            .fitted,
                            .stdresid)
  
  # Cook's distance. High cook's distance is indicative of outliers
  cooksd <- gg_cooksd(model)
  
  grid.arrange(resid_vs_fit, qq, scale_loc, cooksd, 
               ncol = 2,
               top = textGrob(plot_title,
                              gp = gpar(fontsize = 20,
                                        fontweight = "bold")))
}

residual_hist <- function(model) {
  ggplot(mapping = aes(x = resid(model))) +
    geom_histogram(bins = 10,
                   fill = "cornflowerblue",
                   color = "black") +
    xlab("Residuals")
}

plot_lsmeans <- function(data, ylabel, xlabel, ymin = 0, ymax = 100,
                         x = Treatment, y = lsmean, bar_color = "#512888") {
  x <- enquo(x)
  y <- enquo(y)
  
  ggplot(data = data,
         aes(x = !!x,
             y = !!y)) +
    geom_col(fill = bar_color,
             color = "black",
             size = 1) +
    geom_errorbar(aes(ymin = lower.CL,
                      ymax = upper.CL),
                  width = 0.3) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    ylab(ylabel) +
    xlab(xlabel)
}
