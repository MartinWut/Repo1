setwd("C:/Users/gerle/Desktop/Statistische Programmierung mit R/?bung_Flexstat")
getwd()

#library(jsonlite)
library(httr)
#install.packages("rjson")
#library(rjson)
library(jsonlite)
library(dplyr)



#### get semesters ####

semester_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916076&type=SEMESTERNR&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")

semester_df <- jsonlite::fromJSON(txt = content(semester_get, as="text"))
semester_df$value <- as.numeric(semester_df$value)
head(semester_df)

## function to list all semesters ##

list_semesters <- function(){
  return(semester_df)
}

list_semesters()

## function to get the value for a specific semester ##

give_semester <- function(name){
  value <- semester_df$value[semester_df$label == name]
  return(value)
}

give_semester("WS 2017/2018")


#### get faculties ####

faculty_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916300&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")

faculty_df <- jsonlite::fromJSON(txt = content(faculty_get, as="text"))
faculty_df$value <- as.numeric(faculty_df$value)
head(faculty_df)

## function to list all faculties ##

list_faculties <- function(){
  return(faculty_df)
}

list_faculties()

## function to get the value for a specific faculty ##

give_faculty <- function(name){
  value <- faculty_df$value[faculty_df$label == name]
  return(value)
}

give_faculty("Wirtschaftswissenschaftliche Fakult?t")


#### get module url and divide url in characters before (url_part1) and after character number (url_part2) ####

module_url <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710933664&type=STUDIENMODUL&path=FAK%3D12&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25"
length(module_url) #1
nchar(module_url)   #190
# faculty number:character 129-130
substr(module_url, 129, 130)  #passt!

module_url_part1 <- substr(module_url, 1, 128)
module_url_part2 <- substr(module_url, 131, nchar(module_url))

## function to list all modules for a selected faculty ##

list_modules <- function(faculty){
  module_url2 <- paste(module_url_part1, as.character(faculty), module_url_part2, sep = "")
  module_get <- GET(module_url2)
  module_df <- jsonlite::fromJSON(txt = content(module_get, as="text"))
  return(module_df)
}

list_modules(12)


#### function to get the data for one module ####

module_data <- function(semester, faculty, module){
  results_file <- readChar("json/request.json", file.info("json/request.json")$size)
  for (i in semester) {
    for (j in faculty) {
      for (k in module) {
        semester_list <- paste('"lastValue":"', semester, '"')
        results_list <- sub('"lastValue":"60"',semester_list, results_file)
        
        faculty_list <- paste('"lastValue":"', faculty, '"')
        results_list <- sub('"lastValue":"12"',faculty_list, results_list)
        
        module_list <- paste('"lastValue":"', module, '"')
        results_list <- sub('"lastValue":"112"',module_list, results_list)
      }
    }
  }
  bodyList <- list(data = results_list)
  records <- data.frame(matrix(nrow = 0, ncol = 21))
  flex_url <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"
  response <- POST(flex_url, body = bodyList, encode = "form")
  stop_for_status(response)
  responseJSON <- content(response, encoding = "UTF-8", type = "text")
  responseDF <- fromJSON(responseJSON)$data$records
  return(responseDF)
}

UnternehmenssteuernI <- module_data(66, 12, 112)  #results for WS 2017/18, WIWI faculty, module "Unternehmenssteuern I"
UnternehmenssteuernI
UnternehmenssteuernI$`Notenschnitt (nur Bestanden)`
UnternehmenssteuernI$Notenschnitt

EcoI <- module_data(66, 12, 217)   #results for WS 2017/18, WIWI faculty, module "Econometrics I"
EcoI


#### Compute the mean for one module (of one faculty) for all semesters ###

semester_vec <- semester_df$value
semester_vec
class(semester_vec)

module_mean <- function(sem_vec, faculty, module){
  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
  grade_entries <- sapply(res_allSem, function(x){x[18]})                             #extract elements for "Notenschnitt" (entry 18 in each list element)
  grades <- list()
  for (i in grade_entries) {                        #throw out all list element without a grade entry
    for (j in i) {
      if(!is.null(j) && j != "-"){
        grades[i] <- j
      }
    }
  }
  mean_vec <- as.numeric(grades)
  mean_val <- mean(mean_vec, na.rm = T)
  return(mean_val)
}

module_mean(semester_vec, 12, 112)
module_mean(semester_vec, 12, 217)   #Econometrics I


#### Compute the mean for all modules of one faculty for one semesters ####

module_vec <- as.numeric(list_modules(12)$value)
module_vec

faculty_mean <- function(semester, faculty, module_vec){
  res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty)
  grade_entries <- sapply(res_allMod, function(x){x[18]})
  grades <- list()
  for (i in grade_entries) {                  #throw out all list element without a grade entry
    for (j in i) {
      if(!is.null(j) && j != "-"){
        grades[i] <- j
      }
    }
  }
  mean_vec <- as.numeric(grades)
  mean_val <- mean(mean_vec, na.rm = T)
  return(mean_val)
}

faculty_mean(66, 12, module_vec)   #mean for WIWI faculty in WS 2017/2018

