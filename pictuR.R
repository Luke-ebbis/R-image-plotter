###libraries

load <- function() {
  source("lib.R")
  library(argparse)
}

suppressMessages(load())

main <- function() {
  parser <- ArgumentParser(description='Process a folder of ZIPs into a pdf of images.')
  parser$add_argument("-z", "--zip", type="character",
                      help="Path to the zip")
  parser$add_argument("-o", "--output", type="character",
                      help="Path the output PDF")
  args <- parser$parse_args()
}

main()