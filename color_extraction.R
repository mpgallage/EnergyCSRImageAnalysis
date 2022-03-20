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

saveRDS(dataframe, "data_after_color_extraction.rds")

library(ggplot2)
library(dplyr)

dataframe <- readRDS("data_after_color_extraction.rds")


dataframe <- dataframe %>% filter(`Publication Year` %in% (2007:2017))

# Whiteness graphs ####
whiteness_average <- dataframe %>% 
  group_by(`Publication Year`) %>%
  summarise(W = mean(W)*100/256)

ggplot(whiteness_average, aes(`Publication Year`, W, group = 1)) + 
  geom_line(color = "green") + geom_point(color = "springgreen4") + 
  ggthemes::theme_clean() + theme(legend.position="none") + 
  theme(axis.title.x = element_text(size=12, face="bold", color = "black", 
                                    margin=margin(10,0,10,0)),
        axis.title.y = element_text(size=12, face="bold", color = "black",
                                    margin=margin(0,10,0,10))) +
  xlab("Publication Year") + ylab("Whiteness (%)") + ylim(0, 100)

ggsave("whitenes_by_year.png", dpi = 300)

whiteness_by_region <- dataframe %>% 
  group_by(`Publication Year`, Region) %>%
  summarise(W = mean(W)*100/256)

ggplot(whiteness_by_region, aes(`Publication Year`, W, color = Region, group = Region)) + 
  geom_line() + geom_point() + ggthemes::theme_clean() + 
  theme(axis.title.x = element_text(size=12, face="bold", color = "black", 
                                    margin=margin(10,0,10,0)),
        axis.title.y = element_text(size=12, face="bold", color = "black",
                                    margin=margin(0,10,0,10))) +
  scale_color_manual(name = "Region",
                     labels = c("Africa",
                                "Asia",
                                "Europe",
                                "Latin America",
                                "North America",
                                "Oceania"),
                     values = c("cyan4",
                                "green",
                                "seagreen2",
                                "royalblue4",
                                "olivedrab4",
                                "slateblue3")) +
  xlab("Publication Year") + ylab("Whiteness (%)") + ylim(0, 100)

ggsave("whitenes_by_year_group_region.png", dpi = 300)

red_average <- dataframe %>% 
  group_by(`Publication Year`) %>%
  summarise(R = mean(R)*100/256)

green_average <- dataframe %>% 
  group_by(`Publication Year`) %>%
  summarise(G = mean(G)*100/256)

blue_average <- dataframe %>% 
  group_by(`Publication Year`) %>%
  summarise(B = mean(B)*100/256)

colors <- data.frame(Year = red_average$`Publication Year`, R = red_average$R, 
                     G = green_average$G, B = blue_average$B)

ggplot(colors, aes(Year, group = 1)) +
  geom_point(aes(y = R, color = "red")) + geom_line(aes(y = R, color = "red")) +
  geom_point(aes(y = G, color = "green")) + geom_line(aes(y = G, color = "green")) +
  geom_point(aes(y = B, color = "blue")) + geom_line(aes(y = B, color = "blue")) +
  ggthemes::theme_clean() + 
  theme(axis.title.x = element_text(size=12, face="bold", color = "black", 
                                    margin=margin(10,0,10,0)),
        axis.title.y = element_text(size=12, face="bold", color = "black",
                                    margin=margin(0,10,0,10))) +
  scale_color_manual(name = "Color Channel",
                     labels = c("Red",
                                "Green",
                                "Blue"),
                     values = c("red",
                                "green",
                                "blue")) +
  xlab("Publication Year") + ylab("Color intensity (%)")

ggsave("rgb_by_year.png", dpi = 300)
