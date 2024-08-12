###libraries
library(tidyverse)
library(magick)
library(gridExtra)
library(ggplot2)
library(ggtext)



# Here you can set the rows and cols. Format is <row>, <col>
dims <- c(4, 2)

map <- "figures"

make_pdf <- function(map,
                     output_dir = "./",
                     name_pattern = "P1070(.+).JPG",
                     pattern_remove = "(\\s\\(Klein\\))",
                     extension = ".JPG",
                     dims = c(4, 2)) {
  print("making pdfs")
  fotoVtr <- list.files(map,
                        pattern = extension)
  comment_table  <- retrieve_comment_table(map)
  pictures <- make_image_list(fotoVtr, 
                              name_pattern = name_pattern,
                              comment_table = comment_table,
                              extension = extension,
                              pattern_remove = "(\\s\\(Klein\\))")
  write_picture_list(pictures, output_dir, dims = dims)
}

retrieve_comment_table <- function(map) {
  comment_file <- list.files(map, pattern = ".csv")
  print(comment_file, map)
  if (comment_file == "") {
    stop("No comment file has been supplied! Please place a comment file with",
         " two columns, name (file name without the pattern that should be",
         " removed) and the description of the image.")
  }
    if (length(comment_file) > 1)  {
      stop(paste("Only one comment file allowed, the programme detected:", 
                 paste(comment_file)))
    }
  comment_table <- read.csv(paste0(map, "/", comment_file), sep = ',')
  if (!(all(colnames(comment_table) %in% c("name", "description")))) {
    stop("The description table has the wrong column names, the names",
         " `name` and `description` are expected.")
  }
  comment_table
}

write_picture_list <- function(picture_list,
                               output_dir = "./",
                               output.name = paste0("pictur_output_", 
                                                    Sys.Date()),
                               dims=c(4,2)) {
  #Writing to disk.
  ml <- marrangeGrob(grobs=picture_list,
                     nrow=dims[1],
                     ncol=dims[2])
  
  ggsave(paste0(output_dir, 
                output.name,".pdf",
                collapse = " "), # Filename
         # We save a pdf.
         width =  210,
         height =  297 , 
         units = "mm", 
         ml
  )
  
  graphics.off()
  
}

make_image_list <- function(fotoVtr,
                            name_pattern = "P1070(.+)",
                            comment_table = NA,
                            pattern_remove = "(\\s\\(Klein\\))",
                            extension = ".JPG") {
  p <- list()
  message(paste("There are ", length(fotoVtr), " images to process"))
  if (is.data.frame(comment_table)) {
    message("Also adding comments.")
  }
  message("\n=====")
  
  for(image_file_name in fotoVtr){
    image_name <- image_display_name(image_file_name, 
                                     name_pattern = name_pattern,
                                     pattern_remove = pattern_remove,
                                     extension = extension)
    if (is.data.frame(comment_table)) {
      # there is a comment to be read
      search_name <- str_remove(image_file_name,
                                pattern_remove) |> str_remove(extension)
      comment_row <- comment_table |>
        filter(name == search_name)
      if (length(comment_row$name) > 1) {
        stop("Invalid comment data. Please specify only one comment per file.\n",
             "found comment data ", comment_row)
      }
      comment_string <- comment_row$description |> 
        stringr::str_to_sentence()
    } else {
      # there is no comment
      comment_string <- ""
    }
    caption_string <- build_caption(image_name, comment_string)
    message(paste("Processing picture ", image_file_name, "\n\t new name: ",
                  image_name, "\n\t caption:", caption_string))
    p[[image_file_name]] <- build_image(image_file_name, caption_string)
  }
  p
}

build_image <- function(image_file_name, caption_string) {
  require(ggtext)
  fotofile <- paste0(map, "/", image_file_name)
  foto <- image_read(fotofile)
  image_ggplot(foto, interpolate = FALSE) + 
    labs(caption = caption_string) +
    theme(plot.caption = element_textbox_simple(
      size = 6,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    ))
}

build_caption <- function(image_name, image_caption) {
  if (!identical(image_caption, character(0))) {
    cap <-paste0("**","Foto: ", image_name,
                 "**", "</b><br>", image_caption, 
                 collapse = " ")  
  } else {
    cap <- paste0("**","Foto: ", image_name,
                  "**", collapse = " ")  
  }
  cap
}

image_display_name <- function(image_name, pattern_remove = NA,
                               name_pattern = "P1070(.+).JPG",
                               extension = ".JPG") {
  
  # Removing the additional junk from the names.
  if (!is.na(pattern_remove)) {
    image_name <- str_remove(image_name, pattern_remove)  
  }
  image_name <- str_extract(image_name, name_pattern, group=1)
  image_name <- str_remove(image_name, extension)
  image_name
}

