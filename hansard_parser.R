library(tidyverse)
library(multigrep)
library(glue)
library(here)

data_files <- #PATH TO FOLDER CONTAINING HANSARD DATA FILES

file_paths <- list.files(data_files, recursive = TRUE, pattern = "csv$", full.names = TRUE)

#TEST <- file_paths[1:500]

file_name <- #OUTPUT FILE NAME

keywords <- c( #KEY WORDS)

#create empty csv file with headers to write data to
create_data <- function(
  hid = NA,
  speechdate = NA,
  pid = NA,
  opid = NA,
  speakeroldname = NA,
  speakerposition = NA,
  maintopic = NA,
  subtopic = NA,
  subsubtopic = NA,
  speechtext = NA,
  speakerparty = NA,
  speakerriding = NA,
  speakername = NA,
  speakerurl = NA
) {
  tibble(
    hid = hid,
    speechdate = speechdate,
    pid = pid,
    opid = opid,
    speakeroldname = speakeroldname,
    speakerposition = speakerposition,
    maintopic = maintopic,
    subtopic = subtopic,
    subsubtopic = subsubtopic,
    speechtext = speechtext,
    speakerparty = speakerparty,
    speakerriding = speakerriding,
    speakername = speakername,
    speakerurl = speakerurl
    )
}

# write once to create headers
write_csv(create_data() %>% drop_na(), file_name, append = TRUE, col_names = TRUE)

#get debates by keyword dictionary
get_debates <- lapply(file_paths, function(i) {
  
  #read file as csv
  data <- read_csv(i)
  
  #correct col classes
  data_corrected <- data %>%
    mutate(basepk = as.numeric(basepk),
           hid = as.character(hid),
           speechdate = as.Date(speechdate, "%Y-%m-%d"),
           pid = as.character(pid),
           opid = as.character(opid),
           speakeroldname = as.character(speakeroldname),
           speakerposition = as.character(speakerposition),
           maintopic = as.character(maintopic),
           subtopic = as.character(subtopic),
           subsubtopic = as.logical(subsubtopic),
           speechtext = as.character(speechtext),
           speakerparty = as.character(speakerparty),
           speakerriding = as.character(speakerriding),
           speakername = as.character(speakername),
           speakerurl = as.character(speakerurl))
  
  #create lower case speechtext var for keyword searching
  data_lowered <- data_corrected %>%
    mutate(speechtextlower = tolower(speechtext))
  
  #search speechtextlower var for keywords in dictionary
  print(glue("Filtering {i}..."))
  
  if(nrow(data_lowered) > 0){
    data_filtered <- data_lowered %>%
      filter(multigrep(keywords, speechtextlower))
      
    currdata <- create_data(
      hid = data_filtered$hid,
      speechdate = data_filtered$speechdate,
      pid = data_filtered$pid,
      opid = data_filtered$opid,
      speakeroldname = data_filtered$speakeroldname,
      speakerposition = data_filtered$speakerposition,
      maintopic = data_filtered$maintopic,
      subtopic = data_filtered$subtopic,
      subsubtopic = data_filtered$subsubtopic,
      speechtext = data_filtered$speechtext,
      speakerparty = data_filtered$speakerparty,
      speakerriding = data_filtered$speakerriding,
      speakername = data_filtered$speakername,
      speakerurl = data_filtered$speakerurl
    )
    
    #save speeches to csv
    write_csv(currdata, file_name, append = TRUE)
  }
})
