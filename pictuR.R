###libraries

#https://stackoverflow.com/questions/53279365/adding-a-logo-to-a-grid-table-pdf-output-in-r
# logo

library(tidyverse)
library(magick)
library(gridExtra)
library(ggplot2)
library(ggtext)


##IVOER VELD#####################################################################
# Path to the image-folder, can be choosen with `choose.files()`
map <- "figures"

# Name of the output file, _without_ file extention.
output.name <- "report"

# Here you can set the rows and cols. Format is <row>, <col>
dims <- c(4, 2)

# Set this option to `TRUE` if you want the numbering to start from 0. Else 'FALSE'
#start.from.one <- FALSE Broken

#Set this option to `TRUE` if you want comments, else 'FALSE'
comment.flag <- TRUE
numbers.flag <- FALSE

comment.file <- "figures/description.csv"

comment_table <- read.csv(comment.file)
pattern_remove <- "(\\s\\(Klein\\))"


####
fotoVtr <-  list.files(map,
                       pattern = ".jpg|.JPG"
) 

nummers <- as.numeric(str_extract_all(fotoVtr, "[0-9]+")) 


name_tranformation <- function(x) {
  str_remove(x, "P1070")
}

p <- list()
message(paste("comment ", comment.flag))
print(comment_table$name)
for(image.file.name in fotoVtr){
  
  # if(start.from.one == TRUE){
  #   image.name <- image.file.name
  
  image.name <- str_remove(image.file.name, pattern_remove)
  
  image.name.plot <- paste0(str_remove(image.name, pattern = ".JPG"),
                            #str_remove(., pattern = "P1070"), 
                            collapse = " ") |> name_tranformation()
  currentFotoNumber <- as.numeric(image.name.plot)
  if(comment.flag == TRUE){
    if(image.name.plot %in% name_tranformation(comment_table$name)) {
      bijschr <- comment_table %>% 
        filter(image.name.plot == name_tranformation(name)) %>% 
        mutate(part = if_else(is.na(part),
                                   "",
                                   part),
               description = if_else(is.na(description),
                                      "",
                                      description))
      onderschrift <- paste0(c(bijschr$part,  " ", bijschr$description),
                             collapse = " ")
     message("onderschift", onderschrift)
    currentFotoNumber <- currentFotoNumber + 1
    }else(onderschrift <- " ")
    #current photonumber for a number 
    cap <-labs(caption = paste0("**","Foto: ", image.name.plot,
                                 "**", "</b><br>", onderschrift, 
                                collapse = " "))
  }else(cap <- labs(caption = paste0("**", image.name,
                                     "**", collapse = " ")))
  
  
  fotofile <- paste0(map, "/", image.file.name)
  foto <- image_read(fotofile)
  message(paste("preparing plot ", cap))
  
  p[[image.file.name]] <- image_ggplot(foto, interpolate = FALSE) + 
    cap +
    theme(plot.caption = element_textbox_simple(
      size = 6,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    ))
}

#Writing to disk.
ml <- marrangeGrob(grobs=p,
                   nrow=dims[1],
                   ncol=dims[2])

ggsave(paste0(Sys.Date(), " ",output.name,".pdf", collapse = " "), #Filename
       width =  210,
       height =  297 , 
       units = "mm", 
       ml
)

graphics.off()
