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
map <- "231810A_Amstelnext"

# Name of the output file, _without_ file extention.
output.name <- "report"

# Here you can set the rows and cols. Format is <row>, <col>
dims <- c(4, 2)

# Set this option to `TRUE` if you want the numbering to start from 0. Else 'FALSE'
#start.from.one <- FALSE Broken

#Set this option to `TRUE` if you want comments, else 'FALSE'
comment.flag <- TRUE

comment.file <- "figures/description.csv"

comment_table <- read.csv(comment.file)


####
fotoVtr <-  list.files(map,
                       pattern = ".jpg|.JPG"
) 
nummers <- as.numeric(str_extract_all(fotoVtr, "[0-9]+")) 

pattern_remove <- "\\s\\(Klein\\)"

# voorbeeld voorbeeldig gestolen van https://stackoverflow.com/questions/43491685/use-grid-arrange-over-multiple-pages-or-marrangegrob-with-a-layout-matrix 
if(comment.flag == TRUE){
  library(readxl)
  bijschriften <- read_excel(comment.file) 
} %>% 
  select(fotonr, onderdeel, omschrijving)

#TODO fix the file
#TODO: rand, letters het zelfde, en logo, en Fig in bf.
p <- list()
message(paste("comment ", comment.flag))
print(bijschriften$fotonr)
for(image.file.name in fotoVtr){
  
  # if(start.from.one == TRUE){
  #   image.name <- image.file.name
  
  image.name <- str_remove(image.file.name, pattern_remove)
  
  image.name.plot <- paste0(str_remove(image.name, pattern = ".JPG"),
                            #str_remove(., pattern = "P1070"), 
                            collapse = " ")
  currentFotoNumber <- as.numeric(image.name.plot)
  if(comment.flag == TRUE){
    if(image.name.plot %in% bijschriften$fotonr) {
      bijschr <- bijschriften %>% 
        filter(currentFotoNumber == fotonr) %>% 
        mutate(onderdeel = if_else(is.na(onderdeel),
                                   "",
                                   onderdeel),
               omschrijving = if_else(is.na(omschrijving),
                                      "",
                                      omschrijving))
      onderschrift <- paste0(c(bijschr$onderdeel,  " ", bijschr$omschrijving),
                             collapse = " ")
     print("onderschift", onderschrift)
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
#BACKUP
# p <- list()
# for(image in nummers - nummers[1] + 1){
#   if(image %in% bijschriften$fotonr) {
#     bijschr <- bijschriften %>% 
#       filter(fotonr == image) %>% 
#       mutate(onderdeel = if_else(is.na(onderdeel),
#                                  "",
#                                  onderdeel),
#              omschrijving = if_else(is.na(omschrijving),
#                                     "",
#                                     omschrijving))
#     onderschrift <- paste0(bijschr$onderdeel, " ", bijschr$omschrijving, collapse = " ")
#   }else(onderschrift <- " ")
#   
#   if(start.from.zero == TRUE){
#     image.name <- image
#   }else(image.name <- fotoVtr[image])
#   fotofile <- paste0(map, "/", fotoVtr[image])
#   foto <- image_read(fotofile)
#   
#   image.name.plot <- paste0("Foto: ", image.name, collapse = " ")
#   
#   p[[image]] <- image_ggplot(foto, interpolate = FALSE) + 
#     labs(caption = paste0("**",image.name.plot, "**", "</b><br>", onderschrift, collapse = " "))  +
#     theme(plot.caption = element_textbox_simple(
#       size = 6,
#       lineheight = 1,
#       padding = margin(5.5, 5.5, 5.5, 5.5),
#       margin = margin(0, 0, 5.5, 0)
#     ))
#   
#   # onderschrift <- text_grob(onderschrift, 
#   #                           size = 10) %>% 
#   #   drawDetails() %>% 
#   #   as_ggplot()
#   # 
#   # p[[image]] <- ggarrange(foto, onderschrift,
#   #            ncol = 1, nrow = 2,
#   #            heights = c(1, 0.3))
# }


#Writing to disk.
ml <- marrangeGrob(grobs=p,
                   nrow=dims[1],
                   ncol=dims[2])

ggsave(paste0(Sys.Date(), " ",output.name,".pdf", collapse = " "), #Filename
       width =  210,
       height =  297 , 
       units = "mm", 
       ml #ml seems to be large arrange list, generated by `r::marrageGrob()`
)

graphics.off()
# pdf(paste0(Sys.Date(), " ",output.name,".pdf", collapse = " " ), #Filename
#     #width =  229,
#     #height =  143 , 
#     onefile = TRUE,
#     # units = "mm", 
#     paper = "a4r",
#     ml #ml seems to be large arrange list, generated by `r::marrageGrob()`
# )

