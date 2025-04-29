library(tidyverse)
library(magick)
library(gridExtra)
library(ggplot2)
library(ggtext)

make_pdf <- function(Folder,
                     output_dir = "./",
                     name_pattern = "P\\d{4}(.+).JPG",
                     pattern_remove = "(\\s\\(Klein\\))",
                     extension = "\\.JPG",
                     dims = c(4, 2),
                     output_name = "app") {
  fotoVtr <- list.files(Folder,
                        pattern = extension)
  print(fotoVtr)
  comment_table  <- retrieve_comment_table(Folder)
  pictures <- make_image_list(fotoVtr, 
                              name_pattern = name_pattern,
                              comment_table = comment_table,
                              extension = extension,
                              pattern_remove = pattern_remove,
                              Folder = Folder)
  output_file <- write_picture_list(pictures, 
                                    output_dir = output_dir,
                                    dims = dims, 
                                    output.name = output_name)
  return(output_file)
  
}

retrieve_comment_table <- function(Folder) {
  message(str_glue("entering {Folder}"))
  comment_file <- list.files(Folder, pattern = "*.tsv")
  if (comment_file == "" || length(comment_file) == 0) {
    stop("No comment file has been supplied! Please place a comment file with",
            " two columns, name (file name without the pattern that should be",
            " removed) and the description of the image.")
  }
  if (length(comment_file) > 1)  {
    warning(paste("Only one comment file allowed, the programme detected:", 
                  paste(comment_file)))
  }
  comment_table_file <- paste0(Folder, "/", comment_file)
  print("reading table")
  print(comment_table_file)
  comment_table <- read.delim(comment_table_file, sep = '\t')
  if (!(all(colnames(comment_table) %in% c("name", "description")))) {
    warning("The description table has the wrong column names, the names",
            " `name` and `description` are expected.")
  }
  print(comment_table)
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
  outfile <- paste0(output_dir, 
                    output.name,".pdf",
                    collapse = " ")
  ggsave(outfile, # Filename
         # We save a pdf.
         width =  210,
         height =  297 , 
         units = "mm", 
         ml
  )
  
  graphics.off()
  outfile
}

make_image_list <- function(fotoVtr,
                            name_pattern = "P1070(.+)",
                            comment_table = NA,
                            pattern_remove = "(\\s\\(Klein\\))",
                            extension = "\\.JPG",
                            Folder = Folder) {
  p <- list()
  message(paste("There are ", length(fotoVtr), " images to process"))
  if (is.data.frame(comment_table)) {
    message("Also adding comments.")
    print(tibble(comment_table))
  }
  message("\n=====")
  for(image_file_name in fotoVtr){
    image_name <- image_display_name(image_file_name, 
                                     name_pattern = name_pattern,
                                     pattern_remove = pattern_remove,
                                     extension = extension)
    if (is.data.frame(comment_table)) {
      # there is a comment to be read
      print(stringr::str_glue("image file name {image_file_name}\n  makes {image_name}"))
      search_name <- image_name
      comment_row <- comment_table |>
        mutate(name = as.character(name)) |> 
        filter(stringr::str_detect(name, search_name))
      if (length(comment_row$name) > 1) {
        warning("Invalid comment data. Please specify only one comment per file.\n",
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
    p[[image_file_name]] <- build_image(image_file_name, caption_string,
                                        Folder)
  }
  p
}

build_image <- function(image_file_name, caption_string,
                        Folder) {
  require(ggtext)
  fotofile <- paste0(Folder, "/", image_file_name)
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
    cap <- paste0("**","Foto: ", image_name,
                  "**", "</b><br>", image_caption, 
                  collapse = " ")  
  } else {
    cap <- paste0("**","Foto: ", image_name,
                  "**", collapse = " ")  
  }
  cap
}

image_display_name <- function(image_name, 
                               pattern_remove = NA,
                               name_pattern = "P\\d{4}(.+)\\s(\\(Klein\\))\\.JPG",
                               extension = "\\.JPG") {
  print("t")
  # Removing the additional junk from the names
  message(str_glue("starting {image_name} with {pattern_remove} as the pattern to remove. Name pattern {name_pattern} and {extension}"))
  if (!is.na(pattern_remove)) {
    image_name <- str_remove(image_name, pattern_remove)  
  }
  image_name <- str_extract(image_name, name_pattern, group=1)
  image_name <- str_remove(image_name, extension)
  image_name
}



