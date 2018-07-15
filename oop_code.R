# OOP using S3 system in R
# Loading needed packages
require(readr, quietly = T)
require(magrittr, quietly = T)
source("oop_code.R")

# read-in the dataframe
data <- read_csv("C:/Users/m316615/Documents/R_Datasets/MIE.csv", col_types = cols())

# Create function that converts df to LongitudinalData x
make_LD <- function(x)
{
  structure(x, class = "LongitudinalData")
}
############################################################
# Generic method for extracting subject information
subject <- function(x, i)
{
  UseMethod("subject")
}
# subject function definition for extracting user-specific info
subject.LongitudinalData <- function(x, i)
{
  ld <- which(x$id %in% i)
  x <- lapply(x, function(x) x[ld])
  
  structure(x, class = "LongitudinalData")
}
############################################################
# Generic function for extracting visit-specific information
visit <- function(i, j)
{
  UseMethod("visit")
}
# Generic function definition for extracting visit-specific info
visit.LongitudinalData <- function(x, j)
{
  ld <- which(x$visit %in% j)
  x <- lapply(x, function(x) x[ld])
    
  structure(x, class = "LongitudinalData")
}
###########################################################
# Generic function for extracting room-speficic information
room <- function(i, j)
{
  UseMethod("room")
}
# room function definition for extracting room-specific info
room.LongitudinalData <- function(x, k)
{
  ld <- which(x$room %in% k)
  x <- lapply(x, function(x) x[ld])
  
  structure(x, class = "LongitudinalData")
}

# print function for Longitudinal Data
print.LongitudinalData <- function(x, ...)
{
  if(length(unique(x$id)) == 1)
  {
     cat("Subject ID: ", unique(x$id), "\n")
  }
  else if(length(unique(x$id)) == 0)
  {
    cat("No data found for that id!")
  }
  else
  {
    cat("Longitudinal Data with", length(unique(x$id)), "subjects")
  }
  
  if(length(unique(x$visit)) == 1)
  {
    cat("Visit: ", unique(x$visit), "\n")
  }
  
  if(length(unique(x$room)) == 1)
  {
    cat("Room: ", unique(x$room), "\n")
  }
  invisible(x)
}

# function for summarizing data 
summary.LongitudinalData <- function(x, ...)
{
  x <- list(
    summary.id = unique(x$id),
    summary.data = data.frame(
      visit = x$visit,
      room = x$room,
      value = x$value
    )
  )
  
  if(length(unique(x$summary.data$visit)) == 1 & length(unique(x$summary.data$room)) == 1)
  {
    x$summary.data <- summary(x$summary.data$value)
  }
  
  else
  {
    x$summary.data <- x$summary.data %>%
      aggregate(value ~ visit + room, FUN = mean, data = .) %>%
      spread(room, value)
  }
  
  class(x) <- "summary_LongitudinalData"
  x
}

# print summary function for LongitudinalData x
print.summary_LongitudinalData <- function(x, ...)
{
  cat("Subject ID: ", x$summary$id, "\n")
  print(x$summary.data)
  invisible(x)
}
