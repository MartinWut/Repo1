### Version 0.1 R-Package Flexstat-Crawler
 
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
  return(responseDF)
}

ÖkoI <- module_data(64, 12, 217)
ÖkoI

module_data(60, 12, module = c(104,217))

##########################################
## 3. 1 faculty, 1 module, > 1 semester ##
##########################################

module_mean <- function(sem_vec, faculty, module){
  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
  grade_entries <- sapply(res_allSem, function(x){x[18]})                             #extract elements for "Notenschnitt" (entry 18 in each list element)
  grades <- unlist(sapply(grade_entries, function(x) x[!is.null(x)]))   #throw out all NULL list elements
  grades <- grades[grades != "-" & grades != ""]                   #throw out all list elements without a grade entry ("-", "")
  mean_vec <- as.numeric(grades)
  mean_val <- mean(mean_vec, na.rm = T)
  return(mean_val)
}


semester_vec <- semester_df$value
semester_vec2 <- semester_vec[4:length(semester_vec)]  #WS 2017/18 (4. Element) bis WS 2003/04

module_mean(semester_vec2, 12, 217)


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

examiner_stud(semester_winter, 12, 113,mean=FALSE)


############################################################################
## 8. Compare exams (1. and 2. date): 1 faculty, > 1 module, > 1 semester ##
############################################################################

#date_compare <- function(sem_vec, faculty, module){
#  res_allSem <- lapply(sem_vec, module_data, faculty = faculty, module = module)
#  # save the infomrations in date.frame
#  sem_info <- unlist(sapply(res_allSem, function(x){x[17]}))
#  date_info <- unlist(sapply(res_allSem, function(x){x[12]}))
#  mean_info  <- as.numeric(unlist(sapply(res_allSem, function(x){x[18]})))
#  info_df <- data.frame(sem_info,date_info, mean_info) 
#  info_df <- na.omit(info_df)
#  info_df$date_info <- as.Date(info_df[,2], "%d.%m.%Y") 
#  # extract the information for the second date
#  second_mean_all <- data.frame(matrix(ncol = 1))
#  for (i in 1:(length(info_df[,1])-1 )) {
#    if (info_df[i,1] == info_df[i+1,1]) {
#      second_mean_all[i,] <- info_df[i,3]  
#    }
#  }
#  # compute mean for second date
#  second_mean_all <- na.omit(second_mean_all)
#  second_mean_all <- as.numeric(second_mean_all[,1])
#  second_mean <- mean(second_mean_all)
#  # extract the information for the first date
#  first_mean_all <- data.frame(matrix(ncol = 1))
#  for (i in 1:(length(info_df[,1])-1 )) {
#    if (info_df[i,1] == info_df[i+1,1]) {
#      first_mean_all[i,] <- info_df[i+1,3]  
#    }
#  }
#  # compute mean for second date
#  first_mean_all <- na.omit(first_mean_all)
#  first_mean_all <- as.numeric(first_mean_all[,1])
#  first_mean <- mean(first_mean_all)
#  result <- data.frame(first_mean, second_mean)
#  return(result)
#}

date_compare <- function(sem_vec, faculty, module, plot = FALSE){
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

date_compare(semester_vec, faculty = 12, module = 217)
date_compare(semester_vec, faculty = 12, module = 217, plot = TRUE)

## Test with Module Mathematics from Wiwi-Faculty
date_compare(semester_df$value, 12, 104, plot = TRUE)

date_compare(semester_df$value, 12, 217, plot = TRUE)
date_compare(semester_df$value, 12, 217)


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





#################################################
#################################################
#################################################
#################################################
#### Alternativ: eine Funktion -> gelooped ######
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
