# Source: https://github.com/marimuraki/reproducible_research

rm(list=ls())

setwd("/Users/marimuraki/Dropbox/Mari/courses/Coursera/Reproducible Research/project1")

url    <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip    <- "./data/repdata-data-activity.zip" 
file   <- "./data/activity.csv"

if (!file.exists(zip)) {
  download.file(url, 
                destfile=zip)
}

if (!file.exists(file)) {
  unzip(zip, 
        exdir="./data")
  file.remove(zip)
}

data <- read.csv(file, 
                 sep=";",
                 na.string = "?",
                 header=TRUE)

