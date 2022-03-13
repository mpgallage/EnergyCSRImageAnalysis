install.packages("pdftools")
install.packages("png")

library(stringi)
library(pdftools)
library(png)

rds_data <- readRDS("GRIexcel.rds")
head(rds_data)

SECTOR <- "Energy"
ONE_DRIVE_PDF_PATH <- "/Users/malaka/Library/CloudStorage/OneDrive-Personal/pdfs"

sector_data <- rds_data[rds_data$Sector == SECTOR & !is.na(rds_data$`Report Pdf Address`),]

dir.create("files")
dir.create("images")
setwd("./files")

file_list <- list.files(ONE_DRIVE_PDF_PATH)

sector_data_filtered <- c()

for (row in 1:nrow(sector_data)) {
  company <- sector_data[row, "Name"]
  year <- sector_data[row, "Publication Year"]
  filename <- paste(stri_replace_all_fixed(company, " ", ""), "_", year, ".pdf", sep = "")
  
  if (filename %in% file_list) {
    print(paste("Processing...", filename))
    file.copy(paste(ONE_DRIVE_PDF_PATH, filename, sep = "/"), ".")
    
    tryCatch({
      image_name <- stri_replace_all_fixed(filename, ".pdf", ".png")
      bitmap <- pdf_render_page(filename, page = 1)
      png::writePNG(bitmap, paste("../images", image_name, sep = "/"))
      sector_data[row, "Image"] <- image_name
      sector_data_filtered <- rbind(sector_data_filtered, sector_data[row,])
    }, error = function(e) {
      print(paste("File", filename, "is corrupted"))
    })
  }
}

setwd("./..")
saveRDS(sector_data_filtered, "data_with_images.rds")
