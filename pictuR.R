###libraries

load <- function() {
  source("lib.R")
  library(argparse)
}

suppressMessages(load())

main <- function() {
  parser <- ArgumentParser(description='Process a folder of ZIPs into a pdf of images.')
  parser$add_argument("folder", type="character",
                      help="Path to the zip")
  parser$add_argument("output", type="character",
                      help="Path the output PDF")

  parser$add_argument("-e", "--extension", type="character",
                      default="\\.JPG",
                      help="Type of the extension of the images in the zip.")
  parser$add_argument("-p", "--pictureName", type="character",
                      default="P\\d{4}(.+)\\.JPG",
                      help="First")
  parser$add_argument("-r", "--remove", type="character",
                      default="\\s(\\(Klein\\))",
                      help="Second recurrent pattern to remove")
  args <- parser$parse_args()

  make_pdf(args$folder,
           output_dir = args$output,
           extension = args$extension,
           name_pattern = args$pictureName,
           pattern_remove = args$remove)
}

main()
