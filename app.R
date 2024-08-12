
library(shiny)
library(shinyjs)
library(shinyWidgets)



# functions
source("lib.R")

# Define UI
ui <- fluidPage(
  titlePanel("Image Processing App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("remove_pattern", label = "What pattern should be removed?",
                value = "(\\s\\(Klein\\))"),
      textInput("name_pattern", 
                label = "What pattern is the same between all files?",
                value = "P\\d{4}(.+)."),
      textInput("file_extension", label = "What file type are you using?",
                value = ".JPG"),
      fileInput("zipFile", "Choose a zip file with images", accept = c(".zip")),
      actionButton("processBtn", "Process Zip File"),
      useShinyjs(),
      div(id="dwnbutton", 
          downloadButton("downloadData", "Download", 
                         onclick = "Shiny.setInputValue('dwnClicked',
                         true, {priority:'event'});")
      ),
    ),
    
    mainPanel(
      includeMarkdown("README.md"),
      
      textOutput("result"),
    )
  )
)

# Define server
server <- function(input, output, session) {
  file.remove("www/app.pdf")
  unlink("workingdir/*")
  unlink("workingdir/figures")
  rv <- reactiveValues(messages = NULL,
                       output_file = NULL)
  
  # Function to process the uploaded zip file
  processZipFile <- function(zipFilePath) {
    
    dir.create("www")
    # Unzip the contents of the zip file into the temporary directory
    files <- unzip(zipFilePath, exdir = "workingdir")
    
    # Print the list of extracted files for debugging
    print("files are")
    print(files)
    outfile <- make_pdf("workingdir", output_dir = "./www/",
                        extension = input$file_extension,
                        name_pattern = input$name_pattern,
                        pattern_remove = input$remove_pattern)

    
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
      paste("COMPLETED: Processed result:", result)
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
  
  observeEvent(input[["dwnClicked"]], {
    if(file.exists("www/app.pdf")){
      message("the file exist, allowing download!")
    }else{
      sendSweetAlert(
        session = session,
        title = "No data !",
        text = "No data available \n Upload data first,
        and press the process zip file button!",
        type = "error"
      )
    }
  })
}
# Run the Shiny app
shinyApp(ui, server)

