install.packages("png")

library(png)
library(dplyr)

dataframe <- readRDS("data_after_multiclass_classification.rds")

r <- c()
g <- c()
b <- c()

for (image_name in dataframe$Image) {
  image <- readPNG(paste("images", image_name, sep = "/"))
  height <- dim(image)[1]
  width <- dim(image)[2]
  
  total_pixels <- height * width
  total_rgb <- c(0, 0, 0)
  
  for (i in 1:height) {
    for (j in 1:width) {
      rgb <- (image[i, j, ] * 256)[1:3]
      total_rgb <- total_rgb + rgb
    }
  }
  
  total_rgb <- total_rgb / total_pixels
  
  r <- c(r, total_rgb[1])
  g <- c(g, total_rgb[2])
  b <- c(b, total_rgb[3])
}

dataframe <- cbind(dataframe, R=r, G=g, B=b)

w <- (dataframe$R + dataframe$G + dataframe$B) / 3

dataframe <- cbind(dataframe, W=w)

library(ggplot2)

dataframe <- dataframe %>% filter(`Publication Year` %in% (2007:2017))

# Whiteness graphs ####
whiteness_by_region <- dataframe %>% 
  group_by(`Publication Year`, Region) %>%
  summarise(W = mean(W))

ggplot(whiteness_by_region, aes(`Publication Year`, W, color = Region, group = 1)) + 
  geom_line() + geom_point() + facet_wrap(~ Region) +
  theme(legend.position="none") + xlab("Publication Year") + 
  ylab("Whiteness (0-256)")

whiteness_average <- dataframe %>% 
  group_by(`Publication Year`) %>%
  summarise(W = mean(W))

ggplot(whiteness_average, aes(`Publication Year`, W, group = 1)) + 
  geom_point() + geom_line() +
  theme(legend.position="none") + xlab("Publication Year") + 
  ylab("Whiteness (0-256)")

red_average <- dataframe %>% 
  group_by(`Publication Year`) %>%
  summarise(R = mean(R))

green_average <- dataframe %>% 
  group_by(`Publication Year`) %>%
  summarise(G = mean(G))

blue_average <- dataframe %>% 
  group_by(`Publication Year`) %>%
  summarise(B = mean(B))

colors <- data.frame(Year = red_average$`Publication Year`, R = red_average$R, 
                     G = green_average$G, B = blue_average$B)

ggplot(colors, aes(Year, group = 1)) +
  geom_point(aes(y = R, color = "red")) + geom_line(aes(y = R, color = "red")) +
  geom_point(aes(y = G, color = "green")) + geom_line(aes(y = G, color = "green")) +
  geom_point(aes(y = B, color = "blue")) + geom_line(aes(y = B, color = "blue")) +
  theme(legend.position="none") + xlab("Publication Year") + 
  ylab("Color intensity (0-256)")
