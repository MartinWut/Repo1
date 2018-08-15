### Version 0.2 R-Package Flexstat-Crawler
 
setwd("/Users/martinwutke/Desktop/Git/Repo1/FlexStatCrawler") # wichtig für den Programmablauf
setwd("/Users/martinwutke/Desktop/Git/Repo1/FlexStatCrawler/faculty_data") # zum laden der einzelnen Daten
setwd("/Users/martinwutke/Desktop/Git/Repo1/FlexStatCrawler") # wichtig für den Programmablauf

# load the data stored in "all_data"
all_data <- readRDS("all_data")


#library(jsonlite)
library(httr)
#install.packages("rjson")
#library(rjson)
library(jsonlite)
library(dplyr)
#install.packages("zoo")
library("zoo")
library("ggplot2")
#install.packages("stringr")
library(stringr)
#install.packages("reshape2")
#library(reshape2)
#install.packages("tidyr")
library(tidyr)




#############################################################
## 1. Functions for accessing semesters, faculties, modules ##
#############################################################

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

give_semester("WS 2016/2017")



############################################
###### all-in-one Semester - Function ######
############################################

semester_data <- function(x){ # input either "all" or a certain semester in form of "Semesterterm year" (e.g. "WS 2016/2017")
  
  semester_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916076&type=SEMESTERNR&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")
  semester_df <- jsonlite::fromJSON(txt = content(semester_get, as="text"))
  semester_df$value <- as.numeric(semester_df$value)
  head(semester_df)
  if (x == "all") {
    return(semester_df)
  }else{
    if (any(x == semester_df$label)) {
      return(semester_df$value[semester_df$label==x])
    }else{
      stop("Input not in the correct form. Put in \"all\" or \n \"semesterterm year\" ")
    }
    }
}

semester_data("WS 2016/2017")
semester_data("all")

#### get faculties ####

faculty_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916300&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")

faculty_df <- jsonlite::fromJSON(txt = content(faculty_get, as="text"))
faculty_df$value <- as.numeric(faculty_df$value)
head(faculty_df)

#remove white space at the end of some faculty names
faculty_df$label <- str_trim(faculty_df$label, "right")
head(faculty_df)

faculty_df$label[faculty_df$value == 7]

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

############################################
###### all-in-one faculty - Function ######
############################################

faculty_data <- function(x){# input either "all" or a certain faculty (e.g. "Wirtschaftswissenschaftliche Fakultät ")
  faculty_get <- GET("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1525710916300&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25")
  faculty_df <- jsonlite::fromJSON(txt = content(faculty_get, as="text"))
  faculty_df$label <- str_trim(faculty_df$label, "right")   #remove whitespace at the end of some faculty names
  faculty_df$value <- as.numeric(faculty_df$value)
  if (x == "all"){
    return(faculty_df)
  } else{
    if (any(x == faculty_df$label)){
      return(faculty_df$value[faculty_df$label == x])
    } else{
      stop("Input not in the correct form.")
    }
  }
}

faculty_data("all")

faculty_data("Wirtschaftswissenschaftliche Fakultät")

#faculty_data("Medizinische Fakultät") ### noch beheben: Fehler bei bestimmten Fakultäten. Bsp. "Medizinische Fakultät"

#problem solved with str_trim function!!!
faculty_data("Medizinische Fakultät") #works!



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



########################################
## 2. 1 faculty, 1 module, 1 semester ##
########################################

module_data <- function(semester, faculty, module){
  results_file <- readChar("json/request.json", file.info("json/request.json")$size)
  #for (i in semester) {  ## diese drei loops sind überflüssig
  #  for (j in faculty) {
  #    for (k in module) {
        semester_list <- paste('"lastValue":"', semester, '"')
        results_list <- sub('"lastValue":"60"',semester_list, results_file)
        
        faculty_list <- paste('"lastValue":"', faculty, '"')
        results_list <- sub('"lastValue":"12"',faculty_list, results_list)
        
        module_list <- paste('"lastValue":"', module, '"')
        results_list <- sub('"lastValue":"112"',module_list, results_list)
  bodyList <- list(data = results_list)
  records <- data.frame(matrix(nrow = 0, ncol = 21))
  flex_url <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"
  response <- POST(flex_url, body = bodyList, encode = "form")
  stop_for_status(response)
  responseJSON <- content(response, encoding = "UTF-8", type = "text")
  responseDF <- fromJSON(responseJSON)$data$records
  
  # ordnen der Spalten (nur, wenn der resultierende Data.Frame nicht leer ist)
  if(length(responseDF) != 0 ){
  responseDF <- responseDF[,c(17,12,2,15,20,14,3,18,5,4,6,21,7,16,1,8,13,19,9,11,10)]
  }
  
  return(responseDF)
}

ÖkoI <- module_data(64, 12, 217)
ÖkoI





#################################################################
## 2.1 Downloading the data                                    ##
#################################################################

# Laden der Fakultätsdaten durch Verwendung der Funktionen:
# single-request, semester_data, faculty_data, list_modules

semester_all <- semester_data("all")
faculty_all <- faculty_data("all")


single_request <- function(Semester, Fakultät, Modul){
  
  semester_all <- semester_data("all")
  faculty_all <- faculty_data("all")
  
  resultsURL <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"
  requestJSON <- readChar("json/request.json", file.info("json/request.json")$size)
  
  records <- data.frame(matrix(nrow = 0, ncol = 21))
  
  facultyString <- paste0('"lastValue":"',subset(faculty_all[,2], faculty_all[,1] == Fakultät | faculty_all[,2] == Fakultät) , '"') ### Hier den Wert der Fakultät angeben
  thisRequestJSON <- sub('"lastValue":"12"', facultyString, requestJSON)
  
  moduleString <- paste0('"lastValue":"', Modul, '"') ### Hier den Wert des Modules angeben
  thisRequestJSON <- sub('"lastValue":"112"', moduleString, requestJSON)
  
  semesterString <- paste0('"lastValue":"', subset(semester_all[,2], semester_all[,1] == Semester), '"') ### Hier den Wert des Semesters angeben 
  thisRequestJSON <- sub('"lastValue":"60"', semesterString, thisRequestJSON)
  
  bodyList <- list(data = thisRequestJSON)
  request <- POST(resultsURL, body = bodyList, encode = "form")
  stop_for_status(request)
  
  responseJSON <- content(request, encoding = "UTF-8", type = "text")
  responseDataFrame <- fromJSON(responseJSON)$data$records
  results <<- data.frame(matrix(nrow = max(length(Semester),length(Fakultät), length(Modul)), ncol = 21))
  results <- responseDataFrame
  return(results)
}

faculty_down <- function(facultyNr){
  
  # download the necessary data
  
  module_all <- as.numeric(list_modules(facultyNr)$value)
  
  # define the output
  
  fac_mod_list <- list()
  
  # download the data for one module and (looped)
  for (moduleNr in 1:length(module_all)) {
    res <- data.frame(matrix( ncol = 21))
   
      if (class(single_request("all", facultyNr, module_all[moduleNr])) == "data.frame") {
        res <- single_request("all", facultyNr, module_all[moduleNr])
  
    }  
    Spaltennamen <- c("2_3" ,"Studienmodul" ,"Nicht bestanden", "Ohne Note", "Notenschnitt (nur Bestanden)", "1_0" ,"1_7", "2_7", "3_7" ,"5_0" ,"4_0", "Klausurtermin", "3_0", "Bestanden", "Prüfer" ,"2_0" ,"Semester","Notenschnitt" ,"3_3", "Anzahl" ,"1_3" )
    colnames(res) <- Spaltennamen
    res <- res[,c(17,12,2,15,20,14,3,18,5,4,6,21,7,16,1,8,13,19,9,11,10)]
    res <- na.omit(res) # Falls NA's entfernt werden sollen -> erzeugt einen Leeren Data.Frame (Falls lediglich NA's für ein Modul vorhanden sind) 
    fac_mod_list[[moduleNr]] <- res
  }
  
  # Einträge mit leerer Data.Frame entfernen
  index_df <- which(sapply(fac_mod_list, nrow) == 0)
  tmp <- fac_mod_list[-index_df]
  result_list <- tmp
  
  # return the result
  return(result_list)
}

## Download und Speichern der Fakultätsdaten als RDS-Datei (einlesen über readRDS-command)

# Medizinische Fakultät (Nr:3) (Start: 08:37 Ende:08:46)
Med_data <- faculty_down(3)
# lapply(Med_data, function(x) write.table( x, 'Med_data.csv'  , append= T, sep=',' ))
saveRDS(Med_data, "Med_data")

# Wiwi Fakultät (Nr:12) (Start: 09:00 Ende: 09:42)

Wiwi_data <- faculty_down(12)
saveRDS(Wiwi_data, "Wiwi_data")

# Juristische Falkultät (Nr: 2)
Jur_data <- faculty_down(2)
saveRDS(Jur_data, "Jur_data")

# Agrar-Fakultät (Nr:11)
Agr_data <- faculty_down(11)
saveRDS(Agr_data, "Agrar_data")

# Geo-Fakulät (Nr:8)
Geo_data <- faculty_down(8)
saveRDS(Geo_data, "Geo_data")

# Bio_Psy-Fakultät (Nr:9)
bio_psy_data <- faculty_down(9)
saveRDS(bio_psy_data, "bio_psy_data")

# Chemi-Fakultät (Nr:7)
che_data <- faculty_down(7)
saveRDS(che_data, "che_data")

# Forst-fakultät (Nr:10)
forst_data <- faculty_down(10)
saveRDS(forst_data,"forst_data")

# Mathe-Fakultät (Nr:5)
math_data <- faculty_down(5)
saveRDS(math_data,"math_data")

# Physik-Fakultät (Nr:6)
phys_data <- faculty_down(6)
saveRDS(phys_data,"phys_data")

# Gemeinsame und Zentrale Einrichtungen (Nr:17)
zess_data <- faculty_down(17)
saveRDS(zess_data,"zess_data")

# Philosophische Fakultät (Nr:4)
phil_data <- faculty_down(4)
saveRDS(phil_data,"phil_data")

# Sozialwissenschaftliche Fakultät (Nr:13)
sowi_data <- faculty_down(13)
saveRDS(sowi_data,"sowi_data")

# Theologische Fakultät (Nr:1)
theo_data <- faculty_down(1)
saveRDS(theo_data,"theo_data")

# downladoading the data for all faculties using lapply 

facultyNr_vec <- faculty_data("all")[,2] # can also be sorted but doesn't have to be
all_data <- lapply(facultyNr_vec, faculty_down)
saveRDS(all_data, "all_data")

##########################################
## 3. 1 faculty, 1 module, > 1 semester ##
##########################################

module_mean <- function(sem_vec, faculty, module){
  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
  grade_entries <- sapply(res_allSem, function(x){x[8]})                             #extract elements for "Notenschnitt" (entry 8 in each list element)
  grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))   #throw out all NULL list elements
  grades <- grades[grades != "-" & grades != ""]                   #throw out all list elements without a grade entry ("-", "")
  mean_vec <- as.numeric(grades)
  mean_val <- mean(mean_vec, na.rm = T)
  return(mean_val)
}


semester_vec <- semester_df$value
semester_vec2 <- semester_vec[4:length(semester_vec)]  #WS 2017/18 (4. Element) bis WS 2003/04

module_mean(semester_vec, 12, 217)


############ Alternative module_mean-function

module_mean2 <- function(semester_vector = "all", faculty_nr= NA, module_nr=NA){
  
  # create additional data about semester-numbers and faculty-numbers
  semester_all <- semester_data("all")[,2]
  faculty_all <- faculty_data("all")
  modules_faculty <- list_modules(faculty_nr)
  
  # First Case: wrong Input
  if (length(faculty_nr) != 1 || is.na(faculty_nr) == TRUE || semester_vector == "wrong" || is.na(module_nr) == TRUE) {
    stop("Wrong or missing input")
  }else{
  
  ## Second Case: all Semesters, one Module
  # load the data for every semester
  if (semester_vector == "all" && length(module_nr) == 1 && length(faculty_nr) == 1 ) {
    
    tmp1 <- lapply(semester_all, module_data, faculty = faculty_nr, module = module_nr)
  
  # clean the data -> remove empty semester entries
    index_df <- which(sapply(tmp1, length) == 0)
    tmp1 <- tmp1[-index_df]  
  # remove entries with NA's for mean-value
    for (j in 1:length(tmp1)) {
      tmp1[[j]] <- subset(tmp1[[j]], tmp1[[j]][8] != "-" & tmp1[[j]][,8] != "" )
    } 
  
  # in some cases this will create a new list element with NA's because there is sometime just one observation for a certain module
    index_df <- which(sapply(tmp1, nrow) == 0)
    tmp1 <- tmp1[-index_df]
  
  # compute the mean for the cleaned data  
    mean_values <- sapply(tmp1,function(x){x[8]}) 
    mean_values <- unlist(mean_values)
    numeric_values <- as.numeric(mean_values)
    overall_mean <- mean(numeric_values)
  }else{
    
   # Third Case: Mean value for certain semesters ( more/equal than/to 1 but not all semesters)  
    if (length(semester_vector) >= 1 && semester_vector != "all") {
      
      sem_tmp <- semester_vector
      
      tmp1 <- lapply(sem_tmp, module_data, faculty = faculty_nr, module = module_nr)
      
      # clean the data -> remove empty semester entries
      index_df <- which(sapply(tmp1, length) == 0)
      tmp1 <- tmp1[-index_df]  
      # remove entries with NA's for mean-value
      for (j in 1:length(tmp1)) {
        tmp1[[j]] <- subset(tmp1[[j]], tmp1[[j]][8] != "-" & tmp1[[j]][,8] != "" )
      } 
      
      # in some cases this will create a new list element with NA's because there is sometime just one observation for a certain module
      index_df <- which(sapply(tmp1, nrow) == 0)
      tmp1 <- tmp1[-index_df]
      
      # compute the mean for the cleaned data  
      mean_values <- sapply(tmp1,function(x){x[8]}) 
      mean_values <- unlist(mean_values)
      numeric_values <- as.numeric(mean_values)
      overall_mean <- mean(numeric_values)  
      }
    }
  }

  # define the result as the mean and the name of the module
  module_name <- subset(modules_faculty[,1], modules_faculty[,2] == module_nr)
  result <- list(Mean = overall_mean,Module = module_name )
  
  # define a class object (S3)
  #class(result) <- append(class(result), "faculty_mean")
  attr(result, "class") <- "module_mean"
  
  # return the result
  result
  
}

# Define the depiction of the module_mean2-function
print.module_mean <- function(obj){
  cat("Mean = ", obj$Mean,"\n")
  cat("Module = ", obj$Module, "\n")
}



# test for wrong input format -> error message expected
module_mean2(semester_vector = "wrong")
module_mean2()
module_mean2(semester_vector = "all", module_vector = 217)
module_mean2(semester_vector = "all", faculty_nr = c(1,12), module_vector = 217)

# test for correctnes of module_mean2  (all Semesters, Wiwi-Faculty(12), Econometrics (217)) -> expected Value: 3.036818
test_1 <- module_mean2(semester_vector = "all", faculty_nr = 12, module_nr = c(217))
test_1

# test for a specific semester range (valua 50 - 55)
test_2 <- module_mean2(semester_vector = c(50:55), faculty_nr = 12, module_nr = 217)
test_2

# test for more than one module (here module 217, 104, 109 = Econometrics, Mathematics, Statistics)

module_test_vec <- c(217,104,109)

test_3 <- lapply(module_test_vec, module_mean2, semester_vector = "all", faculty_nr = 12 )
  









##########################################
## 4. 1 faculty, > 1 module, 1 semester ##
##########################################

#faculty_mean <- function(semester, faculty, module_vec){
#  res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty)
#  grade_entries <- sapply(res_allMod, function(x){x[18]})
#  grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))   #throw out all NULL list elements
#  grades <- grades[grades != "-" & grades != ""]                   #throw out all list elements without a grade entry ("-", "")
#  mean_vec <- as.numeric(grades)
#  mean_val <- mean(mean_vec, na.rm = T)
#  return(mean_val)
#}

#faculty_mean <- function(semester, faculty, module_vec = as.numeric(list_modules(faculty)$value)){
#  res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty)
#  grade_entries <- sapply(res_allMod, function(x){x[18]})
#  grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))   #throw out all NULL list elements
#  grades <- grades[grades != "-" & grades != ""]                   #throw out all list elements without a grade entry ("-", "")
#  mean_vec <- as.numeric(grades)
#  mean_val <- mean(mean_vec, na.rm = T)
#  return(mean_val)
#}

faculty_mean <- function(semester, faculty, module_vec = as.numeric(list_modules(faculty)$value)){
  res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty)
  grade_entries <- unlist(sapply(res_allMod, function(x){x[18]}))
  grade_entries <- as.numeric(gsub("-", NA,  grade_entries))  
  mean_val <- mean(grade_entries, na.rm = T)
  return(mean_val)
}

module_vec <- as.numeric(list_modules(12)$value)

faculty_mean(66, 12, module_vec)
faculty_mean(66, 12)

# Compute the mean for all modules of one faculty for one semesters without module_vec as argument #

##faculty_mean2 <- function(semester, faculty){
#  module_list <- list_modules(faculty)
#  module_vec <- as.numeric(module_list$value)
#  res_allMod <- lapply(module_vec, module_data, semester = semester, faculty = faculty)
#  grade_entries <- sapply(res_allMod, function(x){x[18]})
#  grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))   #throw out all NULL list elements
#  grades <- grades[grades != "-" & grades != ""]                   #throw out all list elements without a grade entry ("-", "")
#  mean_vec <- as.numeric(grades)
#  mean_val <- mean(mean_vec, na.rm = T)
#  return(mean_val)
#}


### Compare different faculty means for one semester by using faculty_mean - function


result_df <- data.frame(matrix(nrow =1, ncol = 3 ))
colnames(result_df) <- faculty_df[c(12:14),1]
tmp <- sapply(faculty_df[c(12:14),2], faculty_mean, semester=66)
for (i in 1:length(tmp)) {
  result_df[,i] <- tmp[i]
}

result_df

faculty_names <- c("Sozialwiss. Fakultät", "Theol. Fakultät", "Wirtschaftswiss. Fakultät")
barplot(names.arg=faculty_names, tmp, main = "Compare faculty means")



df <- data.frame(Mean_Grades=tmp, Faculty_Name=c("Sozialwiss. Fakultät", "Theol. Fakultät", 
                                                 "Wirtschaftswiss. Fakultät"))

ggplot(df, aes( x=(df$Faculty_Name), y=df$Mean_Grades, fill=df$Faculty_Name) ) + 
  geom_bar(stat = "identity") + 
  xlab("Faculty") + ylab("Mean grades") +
  coord_cartesian(ylim=c(min(df$Mean_Grades-0.5),max(df$Mean_Grades)+0.5)) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle("Comparison of faculty means")

### compare the same faculties over 3 Semesters (values: 64-66 -> WS 2016/2017; SS 2017; WS 2017/2018 )
# time needed: round about 10 minutes
semester_vec_test <- semester_df$value[c(5:7)]
faculty_vec_test <- faculty_df[c(12:14),2]
result_df <- data.frame(matrix(nrow =3, ncol = 3 )) # change nrows to 3 for 3 semesters
colnames(result_df) <- faculty_df[c(12:14),1]




for (i in 1:3) { 
  for (j in 1:3) {
    result_df[i,j] <- sapply(faculty_vec_test[j], faculty_mean, semester=semester_vec_test[i])
  }
}
result_df

mean_results <- matrix(nrow = 1,ncol = 3)
colnames(mean_results) <- colnames(result_df)

for (i in 1:3) {
  mean_results[,i] <- mean(result_df[,i])
}

mean_results



############################################
## 4. 1 faculty, > 1 module, > 1 semester ##
############################################

#only a try; too slow to process the amount of data
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

#debugging (still needs to be tested!)
faculty_meanSem <- function(semester_vec, faculty){
  semester_mean <- c()
  module_list <- list_modules(faculty)
  module_vec <- as.numeric(module_list$value)
  for (i in semester_vec) {
    res_allMod <- lapply(module_vec, module_data, semester = semester_vec[i], faculty = faculty)
    grade_entries <- sapply(res_allMod, function(x){print(x[18])})
    grades <- unlist(sapply(grade_entries, function(x) print(x[!is.null(x)])))
    grades <- grades[grades != "-" & grades != ""] 
    print(grades)
    mean_vec <- as.numeric(grades)
    print(mean_vec)
    mean_val <- mean(mean_vec, na.rm = T)
    print(mean_val)
    semester_mean[i] <- mean_val
    print(semester_mean[i])
  }
  overall_mean <- mean(semester_mean, na.rm = T)
  print(overall_mean)
  return(overall_mean)
}

semester_vec <- semester_df$value
semester_vec2 <- semester_vec[4:length(semester_vec)] #WS 2017/18 (4. Element) bis WS 2003/4
semester_vec2


WiWi_allSem <- faculty_meanSem(semester_vec2, 12)

debug(faculty_meanSem)
faculty_meanSem(semester_vec2, 12)
undebug(faculty_meanSem)

?browser()
faculty_meanSem <- function(semester_vec, faculty){
  browser()
  semester_mean <- c()
  browser()
  module_list <- list_modules(faculty)
  browser()
  module_vec <- as.numeric(module_list$value)
  for (i in semester_vec) {
    browser()
    res_allMod <- lapply(module_vec, module_data, semester = semester_vec[i], faculty = faculty)
    #Funktion bleibt hier hängen (aber keine Fehlermeldung!)
    browser()
    grade_entries <- sapply(res_allMod, function(x){x[18]})
    browser()
    grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))
    browser()
    grades <- grades[grades != "-" & grades != ""] 
    browser()
    mean_vec <- as.numeric(grades)
    browser()
    mean_val <- mean(mean_vec, na.rm = T)
    browser()
    semester_mean[i] <- mean_val
    browser()
  }
  browser()
  overall_mean <- mean(semester_mean, na.rm = T)
  browser()
  return(overall_mean)
  browser()
}

faculty_meanSem(semester_vec2, 12)















####### Alternative: mit bereits heruntergeladenen Daten
## mit Einführung einer neuen S3-Klasse: "fac_mean"
## mit faculty_nr = "all" and download = TRUE werden alle Faculty-means berechnet

faculty_mean2 <- function(faculty_nr, download=FALSE, FacData=NA){ # download= FALSE bedeutet, dass die Daten vorab geladen wurden. Die Liste mit den Faculty-Daten muss dann unter FacData angegeben werden
  
  # load the needed data for result-format
  faculty_vec <- faculty_data("all")
  faculty_vec <- faculty_vec[order(faculty_vec$value),]
  
  # create the data depending on the parameters 
  if (download==FALSE && is.na(FacData)) {
    stop("Wrong data-type. A list with the faculty data is required. Either set download to FALSE or provide the faculty data if download is set to TRUE")
  }else{
    if (download==TRUE) {
      tmp1 <- faculty_down(faculty_nr)
    }else{ # Means download = FALSE and data provided
       tmp1 <- FacData
    }
  }
  
  # exclude the observations without a mean grade for an exam (this is seen by "-" or "")
  tmp2 <- tmp1
  for (j in 1:length(tmp1)) {
    tmp2[[j]] <- subset(tmp2[[j]], tmp2[[j]][8] != "-" & tmp2[[j]][,8] != "" )
  } 
  
  # in some cases this will create a new list element with NA's because there is sometime just one observation for a certain module
  index_df <- which(sapply(tmp2, nrow) == 0)
  tmp2 <- tmp2[-index_df]
  
  # now the data is "clean". Compute the mean for every module the whole mean
  tmp3 <- NA
  for (j in 1:length(tmp2)) {
    tmp3[j] <- mean(as.numeric(tmp2[[j]][,8]))
  }
  mean_tmp <- mean(tmp3)
  faculty_tmp <- faculty_vec[faculty_nr,1]
  result <- list(Mean = mean_tmp,Faculty = faculty_tmp )
  
  # define a class object (S3)
  #class(result) <- append(class(result), "faculty_mean")
  attr(result, "class") <- "fac_mean"
  
  # return the result
  result
  }

# Definiere die Darstellung der faculty_mean2-Funktion
print.fac_mean <- function(obj){
  cat("Mean = ", obj$Mean,"\n")
  cat("Faculty = ", obj$Faculty, "\n")
}


### Testen der faculty-mean2-Funtion


# test with separat download -> mean-value expected
test <- faculty_mean2(12, download = TRUE)
# test with unsufficient inputs -> error message expected
test2 <- faculty_mean2(12)
test3 <- faculty_mean2(12, download = FALSE)
# test with already downloaded data -> mean-value expected
test4 <- faculty_mean2(12, FacData = Wiwi_data)
test4
test4$Mean
test5 <- faculty_mean2(3, FacData=Med_data)
test5
# test with for-loop for several faculties
test6 <- list()
faculty_vec <- c(1,2)  # define a vector with faculty-numbers: faculty_vec (here: number 1 and 2)
for (i in 1:length(faculty_vec)) {
  test6[[i]] <- faculty_mean2(faculty_nr = i, download = TRUE)
}
View(test6)
class(test6)

# using lapply (takes some time!!!!) 

system.time(
test7 <- lapply(as.list(fac_vec2), faculty_mean2, download = TRUE)
)
test7
## see new Funtion (section 9) for plotting the functions

# test with perloaded data (all_data stored as RDS-file)
test8 <- lapply(all_data, faculty_mean2, faculty_nr = faculty_data("all")[,2], download = FALSE)

  
fac_vec2 <- faculty_data("all")[,2] # load the faculty-numbers as input for the faculty_mean-function
test8 <- list(NA)

  ### Wichtig: für den loop ist es erfoderlich, dass die Reihenfolge der Fakultätsnummer
  # (hier ungeordent) mit der Reihenfolge der Fakultätsdaten der liste all_data übereinstimmt
for (j in 1:length(fac_vec2)) {
  test8[[j]] <- faculty_mean2(fac_vec2[j], download = FALSE, FacData = all_data[[j]])
}
View(test8)













##########################################
## 5. Compare faculty means: 1 semester ##
##########################################

#only a try; too slow to process the amount of data
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


#############################################################
## 6. Compare examiners: 1 faculty, 1 module, > 1 semester ##
#############################################################

#examiner_compare <- function(sem_vec, faculty, module, plot=FALSE){
#  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
#  examiner_entries <- unlist(sapply(res_allSem, function(x){x[15]}))            #extract elements for "Prüfer" (entry 15 in each list element)
#  grade_entries <- unlist(sapply(res_allSem, function(x){x[18]}))
#  grade_entries <- gsub("-", NA,  grade_entries)
#  grade_entries <- as.numeric(gsub("-", NA,  grade_entries))
#  res_df <- na.omit(data.frame(examiner_entries, grade_entries))
#  ex_comp <- sort(tapply(res_df$grade_entries, list(res_df$examiner_entries), mean))
#  if (plot==FALSE) {
#    return(ex_comp)
#  }else{
#    name_exam <- rownames(ex_comp)
#    plot(a$name_exam, examiner_math_ba, main="Examiners by means")  #Was ist "a"? #examiner_math_ba nicht in Funktion definiert!
#    return(ex_comp)
#  }
#}

examiner_compare <- function(sem_vec, faculty, module, plot=FALSE){
  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
  examiner_entries <- unlist(sapply(res_allSem, function(x){x[15]}))            
  grade_entries <- unlist(sapply(res_allSem, function(x){x[18]}))
  grade_entries <- as.numeric(gsub("-", NA,  grade_entries))
  res_df <- na.omit(data.frame(examiner_entries, grade_entries))
  ex_comp <- sort(tapply(res_df$grade_entries, list(res_df$examiner_entries), mean))
  if (plot==FALSE) {
    return(ex_comp)
  }else{
    df <- data.frame(Mean_Grades=ex_comp, Examiner_Name=rownames(ex_comp))
    ### order the factor levels to get a sorted plot
    
    df$Examiner_Name <-  factor(df$Examiner_Name, levels = c(names(ex_comp)))
    
    ### plot the result
    ggplot(df, aes( x=(df$Examiner_Name), y=df$Mean_Grades, fill=df$Examiner_Name) ) + 
      geom_bar(stat = "identity") + 
      xlab("Examiner") + ylab("Mean grades") +
      coord_cartesian(ylim=c(min(df$Mean_Grades-0.5),max(df$Mean_Grades)+0.5)) +
      guides(fill=guide_legend(title=NULL)) +
      ggtitle("Comparison of Examiners")
  }
}

 
semester_vec <- semester_df$value

examiner_compare(semester_vec, faculty = 12, module = 217)
examiner_compare(semester_vec, faculty = 12, module = 217, plot = TRUE)


### e.g. Compare examiners
examiner_math_ba <- examiner_compare(semester_data("all")$value, 12, 104, plot = TRUE)
examiner_math_ba

### Compare examiners for "Interne Unternehmensrechnung" (113) just wintersemesters

# select just the wintersemesters
semester_winter <- semester_df$value[seq(1,length(semester_df$value),2)] 

exam_intern_wi <- examiner_compare(semester_winter, 12, 113, plot = TRUE)

exam_intern_wi

### Comparison for Econometrics I (217) in Winter terms

#without plot
econometricsI <- examiner_compare(semester_winter, 12,217)
econometricsI

# with plot
econometricsI <- examiner_compare(semester_winter, 12,217, plot = TRUE)
econometricsI


###################################################################################
## 7. Compare examiners by number of students: 1 faculty, 1 module, > 1 semester ##
###################################################################################

examiner_stud <- function(sem_vec, faculty, module,mean=TRUE, plot=FALSE){
  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
  examiner_entries <- unlist(sapply(res_allSem, function(x){x[15]}))            #extract elements for "Prüfer" (entry 15 in each list element)
  stud_entries <- unlist(sapply(res_allSem, function(x){x[20]}))
  stud_entries <- gsub("-", NA,  stud_entries)
  stud_entries <- as.numeric(gsub("-", NA,  stud_entries))
  sem_info <- unlist(sapply(res_allSem, function(x){x[17]}))
  res_df <- na.omit(data.frame(sem_info,examiner_entries, stud_entries))
  ex_comp <- sort(tapply(res_df$stud_entries, list(res_df$examiner_entries), mean))
  if (mean==TRUE) {
    return(ex_comp)
  }else{
    return(res_df)
  }
}

examiner_stud(semester_winter, 12, 113,mean=TRUE, plot = TRUE)


############################################################################
## 8. Compare exams (1. and 2. date): 1 faculty, > 1 module, > 1 semester ##
############################################################################

date_compare <- function(sem_vec, faculty, module, plot = FALSE){
  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
  # save the infomrations in date.frame
  sem_info <- unlist(sapply(res_allSem, function(x){x[17]}))
  date_info <- unlist(sapply(res_allSem, function(x){x[12]}))
  mean_info  <- unlist(sapply(res_allSem, function(x){x[18]}))
  date_names <- names(mean_info)  
  #for group variable with groups "Notenschnitt1", "Notenschnitt2", etc.
  #problem: for one entry the date name is "Notenschnitt
  date_names <- gsub("Notenschnitt1", "Notenschnitt", x = date_names)
  mean_info <- as.numeric(gsub("-", NA,  mean_info))
  info_df <- na.omit(data.frame(sem_info,date_info, mean_info, date_names)) 
  info_df$date_info <- as.Date(info_df[,2], "%d.%m.%Y") 
  info_df <- spread(info_df, date_names, mean_info) #transform to wide format
  result <- apply(info_df[,c(3:ncol(info_df))], MARGIN = 2, FUN = mean, na.rm = T)
  if (plot == FALSE){
    return(result)
  } else{
    df <- data.frame(mean_grades=as.numeric(result), x_names=names(result))
    ggplot(df, aes(x=x_names, y=mean_grades, fill=x_names)) +
      geom_bar(stat = "identity") + 
      xlab("Examination date") + ylab("Mean grades") +
      coord_cartesian(ylim=c(min(df$mean_grades-0.5),max(df$mean_grades)+0.5)) +
      guides(fill=guide_legend(title=NULL)) +
      ggtitle("Comparison of examination dates")
  }
}

date_compare(semester_vec, faculty = 12, module = 217)
date_compare(semester_vec, faculty = 12, module = 217, plot = TRUE)

## Test with Module Mathematics from Wiwi-Faculty
date_compare(semester_df$value, 12, 104, plot = TRUE)
date_compare(semester_df$value, 12, 104)

module_data(63, 12, 104) #3 Klausurtermine (ein Termin doppelt eingetragen)
module_data(61, 12, 104) #3 Klausurtermine
module_data(49, 12, 104) #3 Einträge für Klausurtermine
module_data(48, 12, 104) #3 Einträge für Klausurtermine
module_data(47, 12, 104) #3 Termine (2 doppelt)/ 4 Einträge
module_data(45, 12, 104) #2 Termine/ 3 Einträge
module_data(43, 12, 104) #3 Einträge für Klausurtermine

date_compare(semester_df$value, 12, 217, plot = TRUE)
date_compare(semester_df$value, 12, 217)

date_compare_old <- function(sem_vec, faculty, module, plot = FALSE){
  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
  # save the infomrations in date.frame
  sem_info <- unlist(sapply(res_allSem, function(x){x[17]}))
  date_info <- unlist(sapply(res_allSem, function(x){x[12]}))
  mean_info  <- unlist(sapply(res_allSem, function(x){x[18]}))
  mean_info <- as.numeric(gsub("-", NA,  mean_info))
  info_df <- na.omit(data.frame(sem_info,date_info, mean_info)) 
  info_df$date_info <- as.Date(info_df[,2], "%d.%m.%Y") 
  # extract the information for the second date
  second_mean_all <- data.frame(matrix(ncol = 1))
  for (i in 1:(length(info_df[,1])-1 )) {
    if (info_df[i,1] == info_df[i+1,1]) {
      second_mean_all[i,] <- info_df[i,3]  
    }
  }
  # compute mean for second date
  second_mean_all <- na.omit(second_mean_all)
  second_mean_all <- as.numeric(second_mean_all[,1])
  second_mean <- mean(second_mean_all)
  # extract the information for the first date
  first_mean_all <- data.frame(matrix(ncol = 1))
  for (i in 1:(length(info_df[,1])-1 )) {
    if (info_df[i,1] == info_df[i+1,1]) {
      first_mean_all[i,] <- info_df[i+1,3]  
    }
  }
  # compute mean for first date
  first_mean_all <- na.omit(first_mean_all)
  first_mean_all <- as.numeric(first_mean_all[,1])
  first_mean <- mean(first_mean_all)
  result <- data.frame(first_mean, second_mean)
  if (plot == FALSE){
    return(result)
  } else{
    df <- data.frame(Mean_Grades=as.numeric(result[1,]), date_names=c("First date", "Second date"))
    ggplot(df, aes(x=df$date_names, y=df$Mean_Grades, fill=df$date_names)) +
      geom_bar(stat = "identity") + 
      xlab("Examination date") + ylab("Mean grades") +
      coord_cartesian(ylim=c(min(df$Mean_Grades-0.5),max(df$Mean_Grades)+0.5)) +
      guides(fill=guide_legend(title=NULL)) +
      ggtitle("First vs. second examination date")
  }
}


### compare first vs. second exam date per semester ##

date_compare_sem <- function(sem_vec, faculty, module, plot = FALSE){
  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
  # save the informations in date.frame
  sem_info <- unlist(sapply(res_allSem, function(x){x[17]}))
  date_info <- unlist(sapply(res_allSem, function(x){x[12]}))
  mean_info  <- unlist(sapply(res_allSem, function(x){x[18]}))
  mean_info <- as.numeric(gsub("-", NA,  mean_info))
  info_df <- na.omit(data.frame(sem_info,date_info, mean_info))
  info_df$sem_info <- as.character(info_df$sem_info)
  info_df$date_info <- as.Date(info_df[,2], "%d.%m.%Y") 
  info_df$Date <- rep(NA, nrow(info_df))  #grouping varialble for plot
  # extract the information for the second date
  grade_table <- data.frame(matrix(ncol = 3))
  for (i in 1:(length(info_df[,1])-1 )) {
    if (info_df[i,1] == info_df[i+1,1]) {
      grade_table[i,1] <- info_df[i,1]
      grade_table[i,2] <- info_df[i,3]
      info_df[i,4] <- "Second date"  #grouping varialble for plot
    }
  }
  # extract the information for the first date
  for (i in 1:(length(info_df[,1])-1 )) {
    if (info_df[i,1] == info_df[i+1,1]) {
      grade_table[i,3] <- info_df[i+1,3]
      info_df[i+1,4] <- "First date"    #grouping varialble for plot
    }
  }
  info_df$Date[is.na(info_df$Date)] <- "First date" #set all semesters without a second exam date to "first date"
  grade_table <- na.omit(grade_table)
  info_df <- na.omit(info_df)
  grade_table$second_date_compare <- rep(NA, nrow(grade_table))
  for(i in 1:nrow(grade_table)){
    if (grade_table[i,2] > grade_table[i,3]){
      grade_table[i,4] <- "+"
    }else if (grade_table[i,2] == grade_table[i,3]){
      grade_table[i,4] <- "="
    }else{
      grade_table[i,4] <- "-"
    }
  }
  grade_table <- grade_table[c(nrow(grade_table):1),]
  if (plot == FALSE){
    return(grade_table)
  } else{
    ggplot(info_df, aes(x=sem_info, y=mean_info, group=Date)) +
      geom_line(aes(color=Date)) +
      geom_point(aes(color=Date)) +
      xlab("Semester")  + ylab("Mean grades") +
      ggtitle("First vs. second examination date per semester")
  }
}

date_compare_sem(semester_vec, faculty = 12, module = 217)
date_compare_sem(semester_vec, faculty = 12, module = 217, plot = TRUE)

## Test with Module Mathematics from Wiwi-Faculty
date_compare_sem(semester_df$value, 12, 104, plot = TRUE)










############################################################################
## 9. Plot function depending on class #####################################
############################################################################

plotFS <- function(x){
  UseMethod("plotFS", x)
}

plotFS.fac_mean <- function(x){
  print("Noch offen. Sinnvollen Plot-Output für einen Faculty-Mean-Wert überlegen")
}

plotFS.module_mean <- function(x){
  print("Noch offen. Sinnvollen Plot-Output für einen Module-Mean-Wert überlegen")
}

plotFS.list <- function(x){
  
  if (class(x[[1]]) == "fac_mean") {
    
    faculty_means <- NA
    faculty_names <- NA
    for (i in 1:length(x)) {
      faculty_means[i] <- x[[i]]$Mean
      faculty_names[i] <- x[[i]]$Faculty
    }
    df <- data.frame(faculty_means, faculty_names)
    ggplot(df, aes(x = faculty_names, y = faculty_means, fill=faculty_names))+
      geom_bar(stat = "identity")+
      xlab("Faculty") + ylab("Mean grades") +
      coord_cartesian(ylim=c(min(df$faculty_means-0.5),max(df$faculty_means)+0.5)) +
      guides(fill=guide_legend(title=NULL)) +
      ggtitle("Comparison of faculty means")
    
  }else{
    if (class(x[[1]]) == "module_mean") {
      
    module_means <- NA
    module_names <- NA
    
    for (i in 1:length(x)) {
      module_means[i] <- x[[i]]$Mean
      module_names[i] <- x[[i]]$Module
    }  
    
    df <- data.frame(module_means, module_names)
    ggplot(df, aes(x = module_names, y = module_means, fill=module_names))+
      geom_bar(stat = "identity")+
      xlab("Module") + ylab("Mean grades") +
      coord_cartesian(ylim=c(min(df$module_means-0.5),max(df$module_means)+0.5)) +
      guides(fill=guide_legend(title=NULL)) +
      ggtitle("Comparison of module means")
    }
  }
}

# Testing the plotFS-Function

# testing for errors
plotFS(4)

plotFS(test5)

plotFS(test4)
plotFS(test7)

# test for comparison of faculty means
test8 <- list(NA)
for (j in 1:length(fac_vec2)) {
  test8[[j]] <- faculty_mean2(fac_vec2[j], download = FALSE, FacData = all_data[[j]])
}
plotFS(test8)

# test for comparison of module means
module_test_vec <- c(217,104,109)
test_3 <- lapply(module_test_vec, module_mean2, semester_vector = "all", faculty_nr = 12 )

plotFS(test_3)

#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
#### Alternativ: eine Funktion -> gelooped ######
#################################################
#################################################
#################################################
#################################################
#################################################
#################################################


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


### Creating a Dataframe of the semesternumber

semester_vec <- fromJSON(readLines("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524293694665&type=SEMESTERNR&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25"))

### Creating a Dataframe of the faculties

faculty_vec <- fromJSON(readLines("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524293694760&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25"))

### Result

semester_vec
faculty_vec
sort(faculty_vec[,2])

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



View(res) # Result


# compute the mean
res$X18
str(res$X18)
tmp <- subset(as.numeric(res$X18), as.numeric(res$X18) != "NA")
mean(tmp)

a <- c(4,3,7,5)
var(a)
