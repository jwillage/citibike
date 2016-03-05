### POPULARITY OVER TIME
sink("rows")
for (i in list.files("data", pattern = ".csv")){
  print (i)
  system(paste0("wc -l 'data/", i, "'"))
}
sink(NULL)
rows <- read.csv ("data/rows")
rows <- rows$X843417.data.2013.07
rows <- as.character(rows)
rows <-gsub(" data.*", "", rows)
rows <- as.numeric(rows)
ggplot(aes(x = x, y = y), data = data.frame(x = months[1:27], y = rows/1000)) +
  scale_x_discrete(breaks = c("2013-07-01", "2014-01-01", "2014-07-01", 
                              "2015-01-01", "2015-07-01")) +
  geom_line (aes(group = 1), color = "slateblue1", size = 2) +
  geom_point(size = 7, color = "black", shape = 1) +
  theme_linedraw() +
  labs(y = "Number of trips in thousands", x = "Month", title = "Total Number of Trips by Month") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

### GENDER 
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