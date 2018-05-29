setwd("C:/Users/gerle/Desktop/Statistische Programmierung mit R/Übung_Flexstat")
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

give_faculty("Wirtschaftswissenschaftliche Fakultät")


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

#module_mean <- function(sem_vec, faculty, module){
#  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
#  grade_entries <- sapply(res_allSem, function(x){x[18]})                             #extract elements for "Notenschnitt" (entry 18 in each list element)
#  grades <- list()
#  for (i in grade_entries) {                        #throw out all list element without a grade entry
#    for (j in i) {
#      if(!is.null(j) && j != "-"){
#        grades[i] <- j
#      }
#    }
#  }
#  mean_vec <- as.numeric(grades)
#  mean_val <- mean(mean_vec, na.rm = T)
#  return(mean_val)
#}
# --> function gives wrong result, if there are more than one grade entries for one module!!!

#replace for loop by sapply function
module_mean <- function(sem_vec, faculty, module){
  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
  grade_entries <- sapply(res_allSem, function(x){x[18]})                             #extract elements for "Notenschnitt" (entry 18 in each list element)
  grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))   #throw out all NULL list elements
  grades <- grades[grades != "-" & grades != ""]                   #throw out all list elements without a grade entry ("-", "")
  mean_vec <- as.numeric(grades)
  mean_val <- mean(mean_vec, na.rm = T)
  return(mean_val)
}
# --> function gives right result, also if there are more than one grade entries for one module!!!

module_mean(semester_vec, 12, 112)
module_mean(semester_vec, 12, 217)   #Econometrics I


#### Compute the mean for all modules of one faculty for one semesters ####

module_vec <- as.numeric(list_modules(12)$value)
module_vec

#faculty_mean <- function(semester, faculty, module_vec){
#  res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty)
#  grade_entries <- sapply(res_allMod, function(x){x[18]})
#  grades <- list()
#  for (i in grade_entries) {                  #throw out all list element without a grade entry
#    for (j in i) {
#      if(!is.null(j) && j != "-"){
#        grades[i] <- j
#      }
#    }
#  }
#  mean_vec <- as.numeric(grades)
#  mean_val <- mean(mean_vec, na.rm = T)
#  return(mean_val)
#}
# --> function gives wrong result, if there are more than one grade entries for one module!!!


#replace for loop by sapply function
faculty_mean <- function(semester, faculty, module_vec){
  res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty)
  grade_entries <- sapply(res_allMod, function(x){x[18]})
  grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))   #throw out all NULL list elements
  grades <- grades[grades != "-" & grades != ""]                   #throw out all list elements without a grade entry ("-", "")
  mean_vec <- as.numeric(grades)
  mean_val <- mean(mean_vec, na.rm = T)
  return(mean_val)
}
# --> function gives right result, also if there are more than one grade entries for one module!!!

WiWi <- faculty_mean(66, 12, module_vec)   #mean for WIWI faculty in WS 2017/2018
WiWi
class(WiWi)

## Compute the mean for all modules of one faculty for one semesters without module_vec as argument ##

#faculty_mean2 <- function(semester, faculty){
#  module_list <- list_modules(faculty)
#  module_vec <- as.numeric(module_list$value)
#  res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty)
#  grade_entries <- sapply(res_allMod, function(x){x[18]})
#  grades <- list()
#  for (i in grade_entries) {                  #throw out all list element without a grade entry
#    for (j in i) {
#      if(!is.null(j) && j != "-"){
#        grades[i] <- j
#      }
#    }
#  }
#  mean_vec <- as.numeric(grades)
#  mean_val <- mean(mean_vec, na.rm = T)
#  return(mean_val)
#}

#replace for loop by sapply function
faculty_mean2 <- function(semester, faculty){
  module_list <- list_modules(faculty)
  module_vec <- as.numeric(module_list$value)
  res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty)
  grade_entries <- sapply(res_allMod, function(x){x[18]})
  grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))   #throw out all NULL list elements
  grades <- grades[grades != "-" & grades != ""]                   #throw out all list elements without a grade entry ("-", "")
  mean_vec <- as.numeric(grades)
  mean_val <- mean(mean_vec, na.rm = T)
  return(mean_val)
}

WiWi <- faculty_mean2(66, 12)
WiWi

Agrar <- faculty_mean2(66, 11)
Agrar

BioPsych <- faculty_mean2(66, 9)
BioPsych

Chemie <- faculty_mean2(66, 7)
Chemie

ForstWald <- faculty_mean2(66, 10)
ForstWald

Geo <- faculty_mean2(66, 8)
Geo

MatheInf <- faculty_mean2(66, 5)
MatheInf

Physik <- faculty_mean2(66, 6)
Physik

ZentrEinricht <- faculty_mean2(66, 17)
ZentrEinricht

Jura <- faculty_mean2(66, 2)
Jura

Medizin <- faculty_mean2(66, 3)
Medizin

Philo <- faculty_mean2(66, 4)
Philo

SoWi <- faculty_mean2(66, 13)
SoWi

Theo <- faculty_mean2(66, 1)
Theo

facMeansWS17 <- c(Agrar, BioPsych, Chemie, ForstWald, Geo, MatheInf, Physik, ZentrEinricht, Jura, Medizin, Philo, SoWi, Theo, WiWi)
facMeansWS17 

WS1718_means <- faculty_df
WS1718_means$mean <- facMeansWS17
WS1718_means <- WS1718_means[order(WS1718_means$mean),]
WS1718_means

write.csv(WS1718_means, "WS1718_faculty_means.csv", row.names = F)


#### Compute the mean for all modules of one faculty for all semesters ####

#faculty_meanSem <- function(semester_vec, faculty){
#  semester_mean <- c()
#  module_list <- list_modules(faculty)
#  module_vec <- as.numeric(module_list$value)
#  for (i in semester_vec) {
#    res_allMod <- lapply(module_vec, module_data, semester = semester_vec[i], faculty = faculty)
#    grade_entries <- sapply(res_allMod, function(x){x[18]})
#    grades <- list()
#    for (j in grade_entries) {
#      for (k in j) {
#        if(!is.null(k) && k != "-"){
#          grades[j] <- k
#        }
#      }
#    }
#    mean_vec <- as.numeric(grades)
#    mean_val <- mean(mean_vec, na.rm = T)
#    semester_mean[i] <- mean_val
#  }
#  overall_mean <- mean(semester_mean, na.rm = T)
#  return(overall_mean)
#}

#replace for loop by sapply function
faculty_meanSem <- function(semester_vec, faculty){
  semester_mean <- c()
  module_list <- list_modules(faculty)
  module_vec <- as.numeric(module_list$value)
  for (i in semester_vec) {
    res_allMod <- lapply(module_vec, module_data, semester = semester_vec[i], faculty = faculty)
    grade_entries <- sapply(res_allMod, function(x){x[18]})
    grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))
    grades <- grades[grades != "-" & grades != ""]                   
    mean_vec <- as.numeric(grades)
    mean_val <- mean(mean_vec, na.rm = T)
    semester_mean[i] <- mean_val
  }
  overall_mean <- mean(semester_mean, na.rm = T)
  return(overall_mean)
}

semester_vec 
semester_vec2 <- semester_vec[4:length(semester_vec)]  #WS 2017/18 (4. Element) bis WS 2003/4
semester_vec2


WiWi_allSem <- faculty_meanSem(semester_vec2, 12)


#### Compare the faculty means for one semesters ####

faculty_vec <- faculty_df$value
faculty_vec
class(faculty_vec)

#means_df <- faculty_df
#means_df$means <- NA
#means_df
#means_df$value[7]

faculty_semCompare <- function(faculty_vec, semester){
  means_df <- faculty_df
  means_df$means <- NA
  for (i in faculty_vec) {
    module_list <- list_modules(faculty_vec[i])
    module_vec <- as.numeric(module_list$value)
    fac_mean <- lapply(module_vec, faculty_mean, semester = semester, faculty = faculty_vec[i])
    means_df$means[i] <- fac_mean
  }
  means_df <- means_df[order(means_df$means),]
  return(means_df[,c(1, 3)])
}

#faculty_semCompare <- function(faculty_vec, semester){
#  mean_vector <- c()
#  for (i in faculty_vec) {
#    module_list <- list_modules(faculty_vec[i])
#    module_vec <- as.numeric(module_list$value)
#    fac_mean <- lapply(module_vec, faculty_mean, semester = semester, faculty = faculty_vec[i])
#    mean_vector[i] <- fac_mean
#  }
#  return(mean_vector)
#}

WS1718 <- faculty_semCompare(faculty_vec, 66)


#faculty_semCompare <- function(faculty_vec, semester){
#  means_df <- faculty_df
#  means_df$means <- NA
#  for (i in faculty_vec) {
#    module_list <- list_modules(faculty_vec[i])
#    module_vec <- as.numeric(module_list$value)
#    res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty_vec[i])
#    grade_entries <- sapply(res_allMod, function(x){x[18]})
#    grades <- list()
#    for (j in grade_entries) {
#      for (k in j) {
#        if(!is.null(k) && k != "-"){
#          grades[j] <- k
#        }
#      }
#    }
#    mean_vec <- as.numeric(grades)
#    mean_val <- mean(mean_vec, na.rm = T)
#    means_df$means[i] <- mean_val
#  }
#  return(means_df[,c(1,3)])
#}

#replace for loop by sapply function
faculty_semCompare <- function(faculty_vec, semester){
  means_df <- faculty_df
  means_df$means <- NA
  for (i in faculty_vec) {
    module_list <- list_modules(faculty_vec[i])
    module_vec <- as.numeric(module_list$value)
    res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty_vec[i])
    grade_entries <- sapply(res_allMod, function(x){x[18]})
    grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))
    grades <- grades[grades != "-" & grades != ""]  
    mean_vec <- as.numeric(grades)
    mean_val <- mean(mean_vec, na.rm = T)
    means_df$means[i] <- mean_val
  }
  return(means_df[,c(1,3)])
}

faculty_semCompare(faculty_vec, 66)



#### Compare the faculty means for all semesters ####

#faculty_vec <- faculty_df$value
#faculty_vec
#class(faculty_vec)

#faculty_compare <- function(faculty_vec, semester_vec){
#  means <- c()
#  for (i in faculty_vec) {
#    for (j in semester_vec) {
#      module_vec <- as.numeric(list_modules(faculty_vec[i])$value)
#    }
#  }
#}

#faculty_compare(faculty_vec, semester_vec)



#### Compute the mean for all modules of one faculty for all semesters ####




#### Which faculty has the best mean grade -- for one specific semester ####

#faculty_vec <- faculty_df$value
#faculty_vec
#class(faculty_vec)

