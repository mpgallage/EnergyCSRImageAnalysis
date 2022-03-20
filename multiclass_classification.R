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

class_names = c("Misc",
                "People",
                "Nature",
                "Tech")

use_condaenv("r-reticulate", required = TRUE)

# training data
misc_train_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/misc", full.names = TRUE)
people_train_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/people", full.names = TRUE)
nature_train_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/nature", full.names = TRUE)
tech_train_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/technology", full.names = TRUE)


# test data
misc_test_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/test/misc", full.names = TRUE)
people_test_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/test/people", full.names = TRUE)
nature_test_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/test/nature", full.names = TRUE)
tech_test_pics = list.files("/Users/malaka/Downloads/kaggle/natural_images/test/technology", full.names = TRUE)

loadpics <- function(filenames) {
  grayscale_images <- lapply(filenames, image_load, color_mode = "grayscale") #grayscale the image
  image_arrays <- lapply(grayscale_images, image_to_array) #turns it into an array
  resized_arrays <- lapply(image_arrays,image_array_resize, height = 512, width = 512) #resize
  normalized_arrays <- normalize(resized_arrays, axis = 1) #normalize to make small numbers 
  return(normalized_arrays)}

training_data <- loadpics(c(misc_train_pics, people_train_pics, 
                            nature_train_pics, tech_train_pics))

test_data <- loadpics(c(misc_test_pics, people_test_pics, nature_test_pics,
                        tech_test_pics))

train_lables <- to_categorical(c(rep(0, 30), rep(1, 30), rep(2, 30), 
                                 rep(3, 30)))

test_lables <- to_categorical(c(rep(0, 5), rep(1, 5), rep(2, 5), 
                                rep(3, 5)))

multiclass_model <- keras_model_sequential()

multiclass_model %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax")

multiclass_model %>%
  compile(optimizer = "adam", loss = "categorical_crossentropy",
          metrics = c("accuracy"))

multiclass_fit <- multiclass_model %>%
  fit(training_data, train_lables, epochs = 30, verbose = 2)

plot(multiclass_fit)

multiclass_model %>%
  evaluate(training_data, train_lables)

multiclass_model %>%
  evaluate(test_data, test_lables)

real_images = list.files("images")
dataframe <- readRDS("data_after_binary_classification.rds")

real_data = loadpics(lapply(real_images, function(filename){
  paste("images", filename, sep = "/")
}))

prediction <- as.array(multiclass_model %>%
  predict(real_data) %>% k_argmax())

prediction <- prediction + 1

dataframe <- cbind(dataframe, Class = class_names[prediction])

saveRDS(dataframe, "data_after_multiclass_classification.rds")


library(ggplot2)
library(dplyr)

dataframe <- readRDS("data_after_multiclass_classification.rds")

dataframe <- dataframe %>% filter(`Publication Year` %in% (2007:2017))

# Figure average by region ####
year_to_class <- dataframe %>%
  group_by(`Publication Year`, Class) %>%
  count()

total_reports <- dataframe %>%
  group_by(`Publication Year`) %>%
  count()

avg_year_to_class <- merge(year_to_class, total_reports, by = "Publication Year", all.x = TRUE)

ggplot(avg_year_to_class, aes(`Publication Year`, n.x/n.y, color = Class, group = Class)) + 
  geom_line() + geom_point() + ggthemes::theme_clean() +
  theme(axis.title.x = element_text(size=12, face="bold", color = "black", 
                                    margin=margin(10,0,10,0)),
        axis.title.y = element_text(size=12, face="bold", color = "black",
                                    margin=margin(0,10,0,10))) +
  scale_color_manual(name = "Image Class",
                     labels = c("Miscellaneous",
                                "Nature",
                                "People", 
                                "Technology"),
                     values = c("royalblue4",
                                "green3",
                                "olivedrab4",
                                "springgreen4")) +
  xlab("Publication Year") + ylab("Average probability of class")

ggsave("avg_prob_image_class.png", dpi = 300)
