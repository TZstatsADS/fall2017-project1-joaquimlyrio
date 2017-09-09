

## IDEAS:
# comparisons between republicans and democrats
# see how certain topics evolv over time, such as equality, human rights, war, etc
#      as well as comparison between parties
#
# - maybe do graph comparing frequency of words between republicans and democrats 
#       -> (same as website: http://tidytextmining.com/tidytext.html#word-frequencies)
# - compare average length of speeches between parties
#
# compare differences between speeches of two different terms (1/2)
#

## load packages
library(dplyr)
library(tidytext)
library(readtext)
library(ggplot2)
library(readxl)

# load stop words table
data( stop_words );

# read inauguration info
#inaugInfo <- read_excel(".data/InaugurationInfo.xlsx", sheet = 1)
inaugInfo <- read.csv("/Users/pedrohmariani/Documents/Columbia/Academic/Fall 2017/AppliedDataScience/Projects/proj1/data/InaugurationInfo copy.csv",
                      header = TRUE ) # temporario
inaugInfo <- lapply( inaugInfo[[1]], as.character );
inaugInfo <- lapply( inaugInfo, function(x) strsplit( x , ";" ) );

# read inauguration date
inaugDates <-  readtext( "./data/InauguationDates.txt"); # check if its possible to read txt line by line

## read text input

# data <- list( "president" = c(),
#               "term"      = c(),
#               "party"     = c(),
#               "date"      = c(),
#               "speech"    = list(),
#               "counts"    = list() )

speeches   <- list();
counts     <- list();
presidents <- c();
term       <- c();
party      <- c();
date       <- c();
i <- 1;
for( iFile in list.files("./data/InauguralSpeeches") ){
  
  #iFile <- list.files("./data/InauguralSpeeches")[14]
  
  # store president's name
  aux <- strsplit( x = iFile, split = "inaug" )
  aux <- strsplit( x = aux[[1]][2], split = "-" )
  presidents[ i ] <- aux[[1]][1]
  
  # store term information
  term[ i ] <- strsplit( x = aux[[1]][2], split = ".txt" )
  
  # store party information
  aux <- lapply( inaugInfo, unlist )
  aux <- unlist( lapply( aux, function(x) ifelse( presidents[i] == x[2], x[4], NA ) ) );
  
  if( length( which( !is.na( aux ) == TRUE ) ) == 0 ){
    party[ i ] <- NA;
  }
  else{
    party[ i ] <- aux[  which( !is.na( aux ) == TRUE ) ][1];
  }
  
  ###
  for( j in 1:58 ){
    inaugInfo[[j]][4]
  }
  ###
  
  # store date information
  
  # read speech
  speeches[[ i ]] <- readtext( paste( "./data/InauguralSpeeches", iFile, sep = "/") );
  
  # put speech in tidy format
  speeches[[ i ]] <- speeches[[ i ]] %>% 
    unnest_tokens(word, text);
  
  # remove stop words from speech
  speeches[[ i ]] <- speeches[[ i ]] %>%
    anti_join( stop_words[ stop_words$lexicon == "snowball", ] )
  
  # perform word count
  counts[[ i ]] <- count( speeches[[ i ]], word, sort = TRUE );
  
  i <- i + 1;
  
}


# try to perform SENTIMENT ANALYSIS and TOPIC MODELLING



