setwd("C:/Users/gerle/Desktop/Statistische Programmierung mit R/Übung_Flexstat")
getwd()

#library(jsonlite)
library(httr)
#install.packages("rjson")
#library(rjson)
library(jsonlite)
library(dplyr)


#?fromJSON()

#### get semesters ####

semester_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916076&type=SEMESTERNR&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")
head(semester_get)

#semester <- jsonlite::fromJSON(txt = content(semester_get, as="text"), simplifyDataFrame = T)
semester <- jsonlite::fromJSON(txt = content(semester_get, as="text"))
head(semester)


#### get faculties ####

faculty_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916300&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")
#head(faculty_get)

#faculty <- jsonlite::fromJSON(txt = content(faculty_get, as="text"), simplifyDataFrame = T)
faculty <- jsonlite::fromJSON(txt = content(faculty_get, as="text"))
head(faculty)


#### get modules ####

module_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710933664&type=STUDIENMODUL&path=FAK%3D12&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")
#head(module_get)

#module <- jsonlite::fromJSON(txt = content(module_get, as="text"), simplifyDataFrame = T)
module <- jsonlite::fromJSON(txt = content(module_get, as="text"))
head(module)


#### get results ####

results_post <- POST("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results", encode = "form")
head(results_post)

results <- jsonlite::fromJSON(txt = content(results_post, as="text"), simplifyDataFrame = T)
head(results)
# doesn't work --> results$data: "Object reference not set to an instance of an object."
# --> use file from Hannes for the time being!

results_file <- readChar("request.json", file.info("request.json")$size)
#results_file <- readChar("request.json", nchars = nchar("request.json"))
head(results_file)


#### replace semester value in results_json by values in semester ####
#### replace: \"lastValue\":\"60\"                                ####

for (i in semester$value) {
  semester_list <- paste('"lastValue":"', semester$value[i], '"')
  results_list <- sub('"lastValue":"60"',semester_list, results_file)
}


#### replace faculty value in results_json by values in faculty ####
#### replace: \"lastValue\":\"12\"                              ####

for (i in faculty$value) {
  faculty_list <- paste('"lastValue":"', faculty$value[i], '"')
  results_list <- sub('"lastValue":"12"',faculty_list, results_list)
}


#### replace module value in results_json by values in module ####
#### replace: \"lastValue\":\"112\"                           ####

for (i in module$value) {
  module_list <- paste('"lastValue":"', module$value[i], '"')
  results_list <- sub('"lastValue":"112"',module_list, results_list)
}


#### convert results_list to list object ####

bodyList <- list(data = results_list)

is.list(bodyList)   #TRUE


#### connect to FlexStat-URL ####

records <- data.frame(matrix(nrow = 0, ncol = 21))

flex_url <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"

flexstat <- POST(flex_url, body = bodyList, encode = "form")
stop_for_status(flexstat)

flexstatJSON <- content(flexstat, encoding = "UTF-8", type = "text")
responseDF <- fromJSON(flexstatJSON)$data$records   #funktioniert nicht
