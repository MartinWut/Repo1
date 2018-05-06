
setwd("/Users/martinwutke/Desktop/Git/MyFolder/Repo1/Repo1")

# install.packages("jsonlite")
# install.packages("httr")
# install.packages("rjson")
# install.packages("dplyr")

library(jsonlite)
library(httr)
#library(rjson)
library(dplyr)

##########################################
##### URL"s of the dropdown-elements #####
##########################################

# erste dropdown-url für Semester
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524293694665&type=SEMESTERNR&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25

# zweite dropdown-url für Fakultäten
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524293694760&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25

# dritte dropdown-url für Module

## Wiwi                                                Dieser Wert ändert sich bei jeder Abfrage _______
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524293982269&type=STUDIENMODUL&path=FAK%253D12&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294274439&type=STUDIENMODUL&path=FAK%253D12&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Theo
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294032418&type=STUDIENMODUL&path=FAK%253D1&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Sowi                                                                                          Unterschied ist in diesem Bereich _____
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294090571&type=STUDIENMODUL&path=FAK%253D13&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Philosophische Fakultät
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294167537&type=STUDIENMODUL&path=FAK%253D4&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Medizin
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294201512&type=STUDIENMODUL&path=FAK%253D3&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Jura
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294309354&type=STUDIENMODUL&path=FAK%253D2&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Gemeinsame und zentrale Einrichtungen
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294343349&type=STUDIENMODUL&path=FAK%253D17&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Physik
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294381966&type=STUDIENMODUL&path=FAK%253D6&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Mathematik und Informatik
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294397137&type=STUDIENMODUL&path=FAK%253D5&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Geowissenschaften und Geographie
# 
## Forstwissenschaften und Waldökologie
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294461661&type=STUDIENMODUL&path=FAK%253D10&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Chemie
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294491976&type=STUDIENMODUL&path=FAK%253D7&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Biologie und Psychologie
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294899996&type=STUDIENMODUL&path=FAK%253D9&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
## Agrarwissenschaften
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294537670&type=STUDIENMODUL&path=FAK%253D11&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25


##############################################
##### Creating semester and faculty data #####
##############################################


### Creating a Dataframe of the semesternumber

semester_vec <- fromJSON(readLines("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524293694665&type=SEMESTERNR&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25"))

### Creating a Dataframe of the faculties

faculty_vec <- fromJSON(readLines("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524293694760&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25"))

### Result

semester_vec
faculty_vec
sort(faculty_vec[,2])



##############################################################
##### Creating the data for the faculty specific modules #####
##############################################################


## create a vector with the module-URL for each faculty

url <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294424781&type=STUDIENMODUL&path=FAK%253D8&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25"

url_part1 <- substr(url, start=1, stop=130)
url_part2 <- substr(url, start=132, stop=nchar(url))

module_vec <- rep(NA, length(faculty_vec[,2]))
  
for (i in 1:length(faculty_vec[,2])) {
  module_vec[i] <- paste(url_part1, sort(faculty_vec[,2])[i] , url_part2, sep = "")
}

module_vec

## create a list with dataframes for each faculty which contain the information for the faculty specific modules

all_modules <- list(NA)

for (i in 1:length(faculty_vec[,2])) {
  all_modules[[i]] <- fromJSON(readLines(module_vec[i]))
}

names(all_modules) <- c(arrange(faculty_vec, value)[,1])

### Result: all_modules contains the information for every module of the faculties

#####################################################
##### Defining a function for a single request  #####
#####################################################

single_request <- function(Semester, Fakultät, Modul){
  resultsURL <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"
  requestJSON <- readChar("request.json", file.info("request.json")$size)
  #modulesDataFrame <- all_modules[[Fakultät]] #### hier die gewünschte Fakultät angegeben. Bsp.: all_modules$`Theologische Fakultät`

  records <- data.frame(matrix(nrow = 0, ncol = 21))
  
  facultyString <- paste0('"lastValue":"',subset(faculty_vec[,2], faculty_vec[,1] == Fakultät | faculty_vec[,2] == Fakultät) , '"') ### Hier den Wert der Fakultät angeben
  thisRequestJSON <- sub('"lastValue":"12"', facultyString, requestJSON)
  
  moduleString <- paste0('"lastValue":"', Modul, '"') ### Hier den Wert des Modules angeben
  thisRequestJSON <- sub('"lastValue":"112"', moduleString, requestJSON)
  
  semesterString <- paste0('"lastValue":"', subset(semester_vec[,2], semester_vec[,1] == Semester), '"') ### Hier den Wert des Semesters angeben 
  thisRequestJSON <- sub('"lastValue":"60"', semesterString, thisRequestJSON)
  
  bodyList <- list(data = thisRequestJSON)
  request <- POST(resultsURL, body = bodyList, encode = "form")
  stop_for_status(request)
  
  responseJSON <- content(request, encoding = "UTF-8", type = "text")
  responseDataFrame <- fromJSON(responseJSON)$data$records
  results <<- data.frame(matrix(nrow = max(length(Semester),length(Fakultät), length(Modul)), ncol = 21))
  results <- responseDataFrame
  #results <- results[,c(17,12,2,15,20,14,3,18,5,4,6,21,7,16,1,8,13,19,9,11,10)]
  return(results)
}

test <- single_request("WS 2016/2017", 14, 104)
test

## Example: all semesters: "Produktion und Logistik (Wiwi-Fakultät (14)) (Value 115)

res <- data.frame(matrix(nrow = length(semester_vec[,1]), ncol = 21))
for (i in 1:length(semester_vec[,1])) {
  if (class(single_request(semester_vec[i,1], 14, 115)) == "data.frame") {
    res[i,] <- single_request(semester_vec[i,1], 14, 115)
  }
}

# compute the mean
res$Notenschnitt
str(res$Notenschnitt)
tmp <- subset(as.numeric(res$Notenschnitt), as.numeric(res$Notenschnitt) != "NA")
mean(tmp)



## Example II: Mean of all grades of the Wiwi-Faculty between SS 2010 (value 19) and WS 2017 (value 4)

res2 <- list(NA) # store every semester in a seperate list

for (i in 4:19) {
  for (j in 1:679) {
    if (class(single_request(semester_vec[i,1],14,all_modules[[4]][j,2])) == "data.frame") {
      res[j,] <- single_request(semester_vec[i,1],14,all_modules[[4]][j,2])
    }
  }
  res2[[i]] <- res
  res2[[i]] <- res2[[i]][,c(17,12,2,15,20,14,3,18,5,4,6,21,7,16,1,8,13,19,9,11,10)]
  colnames(res2[[i]]) <- colnames(test)
}

for (i in 1:length) {
  if (class(res2[[i]]) == "data.frame") {
    res2[[i]] <- res2[[i]][,c(17,12,2,15,20,14,3,18,5,4,6,21,7,16,1,8,13,19,9,11,10)]
    colnames(res2[[i]]) <- colnames(test)
  }
}

# calculate the overall mean for every semester

mean_df <- data.frame(matrix(nrow = length(res2[[4]][,1]) ,ncol = length(4:19)))
tmp <- rep(NA, length(4:19))
for (i in 1:16) {
  mean_df[,i] <- as.numeric(unlist(res2[[3+i]]$Notenschnitt))
  tmp[i] <- mean(subset(mean_df[,i], mean_df[i] != "NA"))
  overall_mean <- mean(tmp)
}

overall_mean
