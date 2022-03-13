library(ggplot2)
library(dplyr)

dataframe <- readRDS("data_after_multiclass_classification.rds")

count_by_year <- dataframe %>%
  group_by(`Publication Year`) %>%
  count()

ggplot(count_by_year, aes(`Publication Year`, group = 1, title = "Total reports by year")) + 
  geom_(aes(y = n)) + ylab("Total Reports")

count_by_year <- data.frame(Year = count_by_year$`Publication Year`, 
                            `Count` = count_by_year$n)
       