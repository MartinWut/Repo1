
setwd("/Users/martinwutke/Desktop/Git/MyFolder/Repo1/Repo1")

# install.packages("jsonlite")
# install.packages("httr")
# install.packages("rjson")
# install.packages("dplyr")

#library(jsonlite)
#library(httr)
library(rjson)
library(dplyr)

##########################################
##### URL's of the dropdown-elements #####
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
# https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524294424781&type=STUDIENMODUL&path=FAK%253D8&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25
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

## reading the raw data
semester_raw <- fromJSON(readLines("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524293694665&type=SEMESTERNR&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25"))

semester_label <- rep(0, length(semester_raw))
semester_value <- rep(0, length(semester_raw))

## creating the dataframe 
semester_fun <- function(x) { # x is the list of semester data (= semester_raw)
  for (i in 1:length(x)) {
    semester_label[i] <- x[[i]]$label
    semester_value[i] <- x[[i]]$value
  }
  semester_df <- data.frame(semester_label, semester_value)
  semester_df <- rename(semester_df, Semester = semester_label, Value = semester_value )
  semester_df[1] <- semester_label
  semester_df[2] <- semester_value
  return(semester_df)
}

semester <- semester_fun(semester_raw)

### Creating a Dataframe of the faculties

fac_raw <- fromJSON(readLines("https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/dropdownvalues?_dc=1524293694760&type=FAK&path=&selectAllDummy=false&forQueryId=215&page=1&start=0&limit=25"))

fac_label <- rep(0, length(fac_raw))
fac_value <- rep(0, length(fac_raw))

## creating the dataframe 
fac_fun <- function(x) { # x is the list of semester data (= semester_raw)
  for (i in 1:length(x)) {
    fac_label[i] <- x[[i]]$label
    fac_value[i] <- as.numeric(x[[i]]$value)
  }
  fac_df <- data.frame(fac_label, fac_value)
  fac_df <- rename(fac_df, Faculty = fac_label, Value = fac_value )
  fac_df[1] <- fac_label
  fac_df[2] <- fac_value
  return(fac_df)
}

faculty <- fac_fun(fac_raw)

### Remark: The package jsonlite dgives the same result as using the package
### rjson and writing the functions



###################

semester
faculty
