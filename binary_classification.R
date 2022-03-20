install.packages("reticulate")
install.packages("tidyverse")
install.packages("tensorflow")
install.packages("keras")
install.packages("magick")


library(reticulate)
library(tidyverse)

library(tensorflow)
library(keras)
library(magick)

use_condaenv("r-reticulate", required = TRUE)

# training data
face_train_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/person", full.names = TRUE)
no_face_train_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/mix", full.names = TRUE)

# test data
face_test_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/test/faces", full.names = TRUE)
no_face_test_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/test/no_faces", full.names = TRUE)

loadpics <- function(filenames) {
  grayscale_images <- lapply(filenames, image_load, color_mode = "grayscale") #grayscale the image
  image_arrays <- lapply(grayscale_images, image_to_array) #turns it into an array
  resized_arrays <- lapply(image_arrays,image_array_resize, height = 256, width = 256) #resize
  normalized_arrays <- normalize(resized_arrays, axis = 1) #normalize to make small numbers 
  return(normalized_arrays)}

training_data <- loadpics(c(face_train_pics, no_face_train_pics))

test_data <- loadpics(c(face_test_pics, no_face_test_pics))

train_lables <- to_categorical(c(rep(1, 1000), rep(0, 1000)))

test_lables <- to_categorical(c(rep(1, 20), rep(0, 20)))

face_model <- keras_model_sequential()

face_model %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 2, activation = "sigmoid")

face_model %>%
  compile(optimizer = "adam", loss = "binary_crossentropy",
          metrics = c("accuracy"))

face_fit <- face_model %>%
  fit(x = training_data, y = train_lables, epochs = 30, batch_size=32)

plot(face_fit)

face_model %>%
  evaluate(training_data, train_lables)

face_model %>%
  evaluate(test_data, test_lables)

real_images = list.files("images")
pre_dataframe <- readRDS("data_with_images.rds")
dataframe <- pre_dataframe[pre_dataframe$Image %in% real_images, ]
dataframe <- dataframe[!duplicated(dataframe$Image), ]

real_data = loadpics(lapply(real_images, function(filename){
  paste("images", filename, sep = "/")
}))

prediction <- face_model %>%
  predict(real_data) %>% `>` (0.5)

dataframe <- cbind(dataframe, Human = prediction[, 2])

saveRDS(dataframe, "data_after_binary_classification.rds")

dataframe <- readRDS("data_after_binary_classification.rds")

library(ggplot2)
library(dplyr)

dataframe <- dataframe %>% filter(`Publication Year` %in% (2007:2017))

# General human figure average ####
human_figures <- dataframe %>%
  filter(Human) %>%
  group_by(`Publication Year`) %>%
  count()

total_reports <- dataframe %>%
  group_by(`Publication Year`) %>%
  count()

avg_human_figures <- data.frame(year = total_reports$`Publication Year`, 
                                avg_human = (human_figures$n/total_reports$n))

ggplot(avg_human_figures, aes(year, avg_human, group = 1)) + 
  geom_line(color = "green") + geom_point(color = "springgreen4") + 
  ggthemes::theme_clean() + theme(legend.position="none") + 
  xlab("Publication Year") + ylab("Average human face appearance") +
  theme(axis.title.x = element_text(size=12, face="bold", color = "black", 
                                    margin=margin(10,0,10,0)),
        axis.title.y = element_text(size=12, face="bold", color = "black", 
                                    margin=margin(0,10,0,10)))
ggsave("avg_human_figures_by_year.png", dpi = 300)


# Human figure average by region ####
human_figures <- dataframe %>%
  filter(Human) %>%
  group_by(`Publication Year`, Region) %>%
  count()

total_reports <- dataframe %>%
  group_by(`Publication Year`, Region) %>%
  count()

avg_human_figures <- data.frame(year = total_reports$`Publication Year`, 
                                region = total_reports$Region,
                                avg_human = (human_figures$n/total_reports$n))

ggplot(avg_human_figures, aes(year, avg_human, color = region, group = region)) + 
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
                     values = c("red",
                                "green",
                                "springgreen4",
                                "royalblue4",
                                "olivedrab4",
                                "pink")) +
  xlab("Publication Year") + 
  ylab("Average human face appearance")

ggsave("avg_human_figures_by_year_group_region.png", dpi = 300)

# Human figure average by company size ####
human_figures <- dataframe %>%
  filter(Human) %>%
  group_by(`Publication Year`, Size) %>%
  count()

total_reports <- dataframe %>%
  group_by(`Publication Year`, Size) %>%
  count()

avg_human_figures <- data.frame(year = total_reports$`Publication Year`, 
                                size = total_reports$Size,
                                avg_human = (human_figures$n/total_reports$n))

ggplot(avg_human_figures, aes(year, avg_human, color = size, group = size)) + 
  geom_line() + geom_point() + ggthemes::theme_clean() +
  theme(axis.title.x = element_text(size=12, face="bold", color = "black", 
                                    margin=margin(10,0,10,0)),
        axis.title.y = element_text(size=12, face="bold", color = "black",
                                    margin=margin(0,10,0,10))) +
  scale_color_manual(name = "Company Size",
                     labels = c("Large", 
                                "MNE", 
                                "SME"),
                     values = c("green",
                                "springgreen4",
                                "olivedrab4")) +
  xlab("Publication Year") + ylab("Average human face appearance")

ggsave("avg_human_figures_by_year_group_size.png", dpi = 300)
