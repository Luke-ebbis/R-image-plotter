# Install and load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")

library(shiny)
library(shinyjs)
library(tidyverse)
library(magick)
library(gridExtra)
library(ggplot2)
library(ggtext)

# functions
make_pdf <- function(map,
                     output_dir = "./",
                     name_pattern = "P1070(.+).JPG",
                     pattern_remove = "(\\s\\(Klein\\))",
                     extension = ".JPG",
                     dims = c(4, 2),
                     output_name = "app") {
  fotoVtr <- list.files(map,
                        pattern = extension)
  comment_table  <- retrieve_comment_table(map)
  pictures <- make_image_list(fotoVtr, 
                              name_pattern = name_pattern,
                              comment_table = comment_table,
                              extension = extension,
                              pattern_remove = "(\\s\\(Klein\\))",
                              map = map)
  output_file <- write_picture_list(pictures, 
                                    output_dir = output_dir,
                                    dims = dims, 
                     output.name = output_name)
  return(output_file)
  
}

retrieve_comment_table <- function(map) {
  comment_file <- list.files(map, pattern = ".csv")
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
                            extension = ".JPG",
                            map = map) {
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
    p[[image_file_name]] <- build_image(image_file_name, caption_string,
                                        map)
  }
  p
}

build_image <- function(image_file_name, caption_string,
                        map) {
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
    cap <- paste0("**","Foto: ", image_name,
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



your_processing_function <- function(map) {
  
}

# Define UI
ui <- fluidPage(
  titlePanel("Image Processing App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("zipFile", "Choose a zip file with images", accept = c(".zip")),
      actionButton("processBtn", "Process Zip File"),
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.downloadlinkvisible == 'done'",
          downloadLink("downloadData", "Download Data")
      ),
      textOutput("result"),
      uiOutput("pdfviewer")
    )
  )
)

# Define server
server <- function(input, output) {
  rv <- reactiveValues(messages = NULL,
                       output_file = NULL)
  
  # Function to process the uploaded zip file
  processZipFile <- function(zipFilePath) {
    
    dir.create("www")
    # Unzip the contents of the zip file into the temporary directory
    files <- unzip(zipFilePath, exdir = "workingdir")
    
    # Print the list of extracted files for debugging
    print(files)
    outfile <- make_pdf("workingdir", output_dir = "./www/")

    
    # Clean up: remove the temporary directory and its contents
    unlink(zipFilePath, recursive = TRUE)
    rv$output_file <- outfile
    result <- outfile
    
    # Print messages directly to verbatimTextOutput
    message(paste("Process completed. Result:", rv$output_file, "\n"))
    
    return(result)
  }
  
  # Event handler for the "Process Zip File" button
  observeEvent(input$processBtn, {
    req(input$zipFile)
    
    # Process the zip file and store the result
    result <- processZipFile(input$zipFile$datapath)
    
    # Display the result (you can customize this based on your actual output)
    output$result <- renderText({
      paste("Processed result:", result)
    })
    output$downloadlinkVisible <- reactive({
      "done"
    })
  })
  
  output$message <- renderPrint({
    # Print messages from the reactiveValues
    isolate(rv$messages)
  })
  
  # Event handler for the "Download PDF" button
  observeEvent(input$downloadBtn, {
    # Generate PDF and save it (replace this with your actual PDF generation code)
    
    
    
    # Trigger download of the generated PDF
    downloadHandler(
      filename = function() {
        "output.pdf"
        },
      content = function(file) {
        message(paste("the output exist ", 
                      file.exists("www/app.pdf")))
        file.copy("www/app.pdf", file)
      }
    )
  })
  
  
  output$downloadData <- downloadHandler(
    filename = "app.pdf",
    content = function(file) {
      file.copy("www/app.pdf", file)
    }
  )
}
# Run the Shiny app
shinyApp(ui, server)

