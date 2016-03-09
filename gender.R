g <- ggplot(data = avgs, aes(x = mean.gender, y = mean.duration))
g + geom_point(size = 6, color = "black") + geom_point(size = 5, color = "green") +
  geom_smooth(method = "lm", formula = y ~ x, se = T, color = "black") +
  labs(x = "Average Gender", y = "Average Duration", 
       title = "Average Duration by Average Gender per Month") +
  theme_linedraw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )