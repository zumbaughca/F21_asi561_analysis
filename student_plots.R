source('analysis.R')

cp_df <- data.frame("trt" = c("A", "B", "C", "D"),
                    "mean" = c(73.8, 71.7, 70.9, 71.7),
                    "se" = rep(0.919, times = 4))

cp_df <- data.frame(untrans_means$CP_dig) %>%
  mutate(Treatment = c("0", "10", "20", "30"))

my_theme <- function() {
  th <- theme(
    panel.background = element_blank(),
    axis.line = element_line(size = 1),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20,
                              face = "bold",
                              hjust = 0.5)
  )
  return(list(th))
}


ggplot(data = cp_df,
       aes(x = Treatment,
           y = emmean)) +
  geom_col(color = "black",
           fill = rgb(134 / 255, 31 / 255, 65 / 255),
           size = 1) +
  geom_errorbar(aes(ymax = emmean + SE,
                    ymin = emmean - SE),
                width = 0.2) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     labels = seq(0, 100, by = 20)) +
  ylab("Digestibility, %") +
  xlab("Sorghum Inclusion, % DM") +
  labs(title = "CP digestibility") +
  my_theme()
  
