#-----------------------------------#
# Server                            #
# GAFL Project - Shweta Chopra      #
#                                   #
#-----------------------------------#


# In this script, we are combining the UI and Server into one script called 
# app.R
# This changes our syntax slightly. instead
# of using the shinyui and server functions, we will create
# two objects, server and ui, and then use the shinyApp() fuction
# at the end of the script to run our app. Pretty cool!

#--------------------------------------
# Library
#--------------------------------------
# We only need to include our library at the top of this script
library(shinythemes)
library(shiny)
library(rsconnect)
library(mfx)
library(survey)
library(foreign)
library(ggplot2)
library(stargazer)
library(rgdal)
library(plyr)
library(leaflet)
library(e1071)
library(randomForest)
library(caret)
library(tidyverse)
library(data.table)
library(DT)
library(reshape2)
library(RColorBrewer)

#------------------------------------
# Data Set up
#------------------------------------

#Read data
schools <- read.csv("all_data_middle.csv")

schools$Testers <- ifelse(is.na(schools$Testers), 0, schools$Testers)
schools$Offers <- ifelse(is.na(schools$Offers), 0, schools$Offers)
schools$Perc_Testers <- ifelse(is.na(schools$Perc_Testers), 0, schools$Perc_Testers)
schools$Perc_Offers <- ifelse(is.na(schools$Perc_Offers), 0, schools$Perc_Offers)


schools_copy <- data.frame(schools)
schools_corr <- data.frame(schools)

#Read feature labels
feature_labels <- read.csv("FeatureLabels.csv")

#Get Demographic Data to extract Borough Details for each school
demographics <- read.csv("School_District_Demo_Breakdowns.csv")

Districts <- demographics[,c("JURISDICTION.NAME")]
Districts <- as.data.frame(Districts)
Districts$Districts <- as.character(Districts$Districts)

extract_number <- function(x){
  return(strsplit(x, " ")[[1]][2])
}

extract_borough <- function(x){
  return(strsplit(x, " ")[[1]][3])
}

clean_staten <- function(x){
  if(x == 'Staten'){
    return("Staten Island")
  }
  else{
    return(x)
  }
}

Districts$Number <- sapply(Districts$Districts, extract_number)
Districts$Number <- as.numeric(Districts$Number)

Districts$Borough <- sapply(Districts$Districts, extract_borough)
Districts$Borough <- sapply(Districts$Borough, clean_staten)

# Add borough details for each school
schools <- left_join(schools, Districts, by = c("District" = "Number"))

schools_copy2 <- data.frame(schools)

#For filtering Tab - Data Preparation
filter_schools <- schools_copy2[,c("School.Name", "District","Address..Full.","Borough",
                                   "Economic.Need.Index", "School.Income.Estimate", 
                                   "Percent.ELL","Percent.Black","Percent.Hispanic",
                                   "Student.Attendance.Rate", 
                                   "Percent.of.Students.Chronically.Absent",
                                   "Average.ELA.Proficiency",
                                   "Average.Math.Proficiency", "Offer", "Perc_Testers",
                                   "Perc_Offers")]

clean_district <- function(x){
  return(paste("School District", as.character(x), sep = " "))
}

clean_offer <- function(x){
  if(x == 0){
    return("Non-Offer School")
  }
  
  else{
    return("Offer School")
  }
}


filter_schools$District <- sapply(filter_schools$District, clean_district)
filter_schools$Offer <- sapply(filter_schools$Offer, clean_offer)
filter_schools$Percent.Black.Hispanic <- filter_schools$Percent.Black + filter_schools$Percent.Hispanic
filter_schools <- filter_schools[,c("School.Name", "District","Address..Full.","Borough",
                                    "Economic.Need.Index", "School.Income.Estimate", 
                                    "Percent.ELL","Percent.Black.Hispanic",
                                    "Student.Attendance.Rate", 
                                    "Percent.of.Students.Chronically.Absent",
                                    "Average.ELA.Proficiency",
                                    "Average.Math.Proficiency", "Offer", "Perc_Testers",
                                    "Perc_Offers")]
filter_schools$Average.Total.Proficiency <- (filter_schools$Average.ELA.Proficiency +
                                               filter_schools$Average.Math.Proficiency) /2

filter_schools$Percent.ELL <- filter_schools$Percent.ELL*100
filter_schools$Percent.Black.Hispanic <- filter_schools$Percent.Black.Hispanic * 100
filter_schools$Percent.of.Students.Chronically.Absent <- filter_schools$Percent.of.Students.Chronically.Absent * 100

filter_schools$Percent.Black.Hispanic <- round(filter_schools$Percent.Black.Hispanic, 2)
filter_schools$Perc_Testers <- round(filter_schools$Perc_Testers, 2)
filter_schools$Perc_Offers <- round(filter_schools$Perc_Offers, 2)

district_input <- append(list(unique(filter_schools$District))[[1]],"All")
borough_input <- append(list(unique(filter_schools$Borough))[[1]],"All")

#Create District Level Data for Map
district_wise <- schools %>% group_by(District)
district_data <- district_wise %>% summarise(
  eco_need = mean(Economic.Need.Index, na.rm = TRUE),
  avg_income = mean(School.Income.Estimate, na.rm = TRUE),
  avg_attendance = mean(Student.Attendance.Rate, na.rm = TRUE),
  avg_black = mean(Percent.Black, na.rm = TRUE),
  avg_asian = mean(Percent.Asian, na.rm = TRUE),
  avg_ell = mean(Percent.ELL, na.rm = TRUE),
  avg_hispanic = mean(Percent.Hispanic, na.rm = TRUE),
  avg_white = mean(Percent.White, na.rm = TRUE),
  avg_chronic_absentee = mean(Percent.of.Students.Chronically.Absent, na.rm = TRUE),
  avg_ELA_prof = round(mean(Average.ELA.Proficiency, na.rm = TRUE), 2),
  avg_math_prof = round(mean(Average.Math.Proficiency, na.rm = TRUE), 2),
  avg_offer_rate = round(mean(Perc_Offers, na.rm = TRUE), 2),
  avg_tester_rate = round(mean(Perc_Testers, na.rm = TRUE), 2),
  offer_school_count = sum(Offer),
  total_schools = n(),
  percent_offer_schools = (offer_school_count / total_schools) * 100
  
)

district_data <- left_join(district_data, Districts, by = c("District" = "Number"))



#reading  in the shapefile
schooldistricts <- readOGR("nycschooldistrict", layer = "nycschooldistrict",encoding = "UTF-8")

#Import Specialized High School Locations
sshlocations <- read.csv("SpecializedHighSchools.csv")


#Compare Plot Data Prep
schools_copy$Offer <- as.numeric(schools_copy$Offer)

district_wise_comp <- schools_copy %>% group_by(District)
compareplot_data <- district_wise_comp %>% summarise(
  eco_need = mean(Economic.Need.Index, na.rm = TRUE),
  avg_income = mean(School.Income.Estimate, na.rm = TRUE),
  avg_attendance = mean(Student.Attendance.Rate, na.rm = TRUE),
  avg_black = mean(Percent.Black, na.rm = TRUE),
  avg_asian = mean(Percent.Asian, na.rm = TRUE),
  avg_ell = mean(Percent.ELL, na.rm = TRUE),
  avg_hispanic = mean(Percent.Hispanic, na.rm = TRUE),
  avg_white = mean(Percent.White, na.rm = TRUE),
  avg_chronic_absentee = mean(Percent.of.Students.Chronically.Absent, na.rm = TRUE),
  avg_ELA_prof = mean(Average.ELA.Proficiency, na.rm = TRUE),
  avg_math_prof = mean(Average.Math.Proficiency, na.rm = TRUE),
  avg_offer_rate = round(mean(Perc_Offers, na.rm = TRUE), 2),
  avg_tester_rate = round(mean(Perc_Testers, na.rm = TRUE), 2)
  
)

compareplot_data <- compareplot_data[,1:13]
compareplot_data <- data.frame(compareplot_data)

compareplot_data$District <- sapply(compareplot_data$District, clean_district)

#Create input list for dropdown
compare_input <- append(list(unique(compareplot_data$District))[[1]],"All")

city_avg <-data.frame("All", 
                      mean(compareplot_data$eco_need),
                      mean(compareplot_data$avg_income, na.rm = TRUE),
                      mean(compareplot_data$avg_attendance),
                      mean(compareplot_data$avg_black),
                      mean(compareplot_data$avg_asian),
                      mean(compareplot_data$avg_ell),
                      mean(compareplot_data$avg_hispanic),
                      mean(compareplot_data$avg_white),
                      mean(compareplot_data$avg_chronic_absentee),
                      mean(compareplot_data$avg_ELA_prof),
                      mean(compareplot_data$avg_math_prof),
                      mean(compareplot_data$avg_offer_rate)
)

names(city_avg)<-c("District","eco_need","avg_income", "avg_attendance", 
                   "avg_black", "avg_asian", "avg_ell", "avg_hispanic",
                   "avg_white", "avg_chronic_absentee", "avg_ELA_prof",
                   "avg_math_prof", "avg_offer_rate")


plotdata <- rbind(compareplot_data, city_avg)
plotdata$avg_performance <- ((plotdata$avg_ELA_prof + plotdata$avg_math_prof) / 2)
plotdata$avg_blackhispanic <- ((plotdata$avg_black + plotdata$avg_hispanic))*100
plotdata$avg_chronic_absentee <- plotdata$avg_chronic_absentee * 100
plotdata$avg_ell <- plotdata$avg_ell * 100

plotdata <- plotdata[,c("District","eco_need","avg_ell", 
                        "avg_blackhispanic",
                        "avg_chronic_absentee", "avg_performance",
                        "avg_offer_rate" )]

colnames(plotdata) <- c("District", "Average Economic Need",
                        "Average % of English Language Learners", 
                        "Average % of Black or Hispanic Students",
                        "Average Rate of Chronic Absenteeism (%)",
                        "Average Performance",
                        "Average Offer Rate (%)")

#Gathering the data for plotting
comparedata <- gather(plotdata, key = category, value = value, `Average Economic Need`:`Average Offer Rate (%)`, factor_key=TRUE)


#------------------------------------
# Beginning Shiny Code
#------------------------------------

# This line creates the object server
server<-function(input, output) {
  
  #------------------------------------
  # Data Set up
  #------------------------------------
  
  #Read data
  schools <- read.csv("all_data_middle.csv")
  
  schools$Testers <- ifelse(is.na(schools$Testers), 0, schools$Testers)
  schools$Offers <- ifelse(is.na(schools$Offers), 0, schools$Offers)
  schools$Perc_Testers <- ifelse(is.na(schools$Perc_Testers), 0, schools$Perc_Testers)
  schools$Perc_Offers <- ifelse(is.na(schools$Perc_Offers), 0, schools$Perc_Offers)
  
  
  schools_copy <- data.frame(schools)
  schools_corr <- data.frame(schools)
  
  #Read feature labels
  feature_labels <- read.csv("FeatureLabels.csv")
  
  #Get Demographic Data to extract Borough Details for each school
  demographics <- read.csv("School_District_Demo_Breakdowns.csv")
  
  Districts <- demographics[,c("JURISDICTION.NAME")]
  Districts <- as.data.frame(Districts)
  Districts$Districts <- as.character(Districts$Districts)
  
  extract_number <- function(x){
    return(strsplit(x, " ")[[1]][2])
  }
  
  extract_borough <- function(x){
    return(strsplit(x, " ")[[1]][3])
  }
  
  clean_staten <- function(x){
    if(x == 'Staten'){
      return("Staten Island")
    }
    else{
      return(x)
    }
  }
  
  Districts$Number <- sapply(Districts$Districts, extract_number)
  Districts$Number <- as.numeric(Districts$Number)
  
  Districts$Borough <- sapply(Districts$Districts, extract_borough)
  Districts$Borough <- sapply(Districts$Borough, clean_staten)
  
  # Add borough details for each school
  schools <- left_join(schools, Districts, by = c("District" = "Number"))
  
  schools_copy2 <- data.frame(schools)
  
  #For filtering Tab - Data Preparation
  filter_schools <- schools_copy2[,c("School.Name", "District","Address..Full.","Borough",
                                     "Economic.Need.Index", "School.Income.Estimate", 
                                     "Percent.ELL","Percent.Black","Percent.Hispanic",
                                     "Student.Attendance.Rate", 
                                     "Percent.of.Students.Chronically.Absent",
                                     "Average.ELA.Proficiency",
                                     "Average.Math.Proficiency", "Offer", "Perc_Testers",
                                     "Perc_Offers")]
  
  clean_district <- function(x){
    return(paste("School District", as.character(x), sep = " "))
  }
  
  clean_offer <- function(x){
    if(x == 0){
      return("Non-Offer School")
    }
    
    else{
      return("Offer School")
    }
  }
  
  
  filter_schools$District <- sapply(filter_schools$District, clean_district)
  filter_schools$Offer <- sapply(filter_schools$Offer, clean_offer)
  filter_schools$Percent.Black.Hispanic <- filter_schools$Percent.Black + filter_schools$Percent.Hispanic
  filter_schools <- filter_schools[,c("School.Name", "District","Address..Full.","Borough",
                                      "Economic.Need.Index", "School.Income.Estimate", 
                                      "Percent.ELL","Percent.Black.Hispanic",
                                      "Student.Attendance.Rate", 
                                      "Percent.of.Students.Chronically.Absent",
                                      "Average.ELA.Proficiency",
                                      "Average.Math.Proficiency", "Offer", "Perc_Testers",
                                      "Perc_Offers")]
  filter_schools$Average.Total.Proficiency <- (filter_schools$Average.ELA.Proficiency +
                                                 filter_schools$Average.Math.Proficiency) /2
  
  filter_schools$Percent.ELL <- filter_schools$Percent.ELL*100
  filter_schools$Percent.Black.Hispanic <- filter_schools$Percent.Black.Hispanic * 100
  filter_schools$Percent.of.Students.Chronically.Absent <- filter_schools$Percent.of.Students.Chronically.Absent * 100
  
  filter_schools$Percent.Black.Hispanic <- round(filter_schools$Percent.Black.Hispanic, 2)
  filter_schools$Perc_Testers <- round(filter_schools$Perc_Testers, 2)
  filter_schools$Perc_Offers <- round(filter_schools$Perc_Offers, 2)
  
  district_input <- append(list(unique(filter_schools$District))[[1]],"All")
  borough_input <- append(list(unique(filter_schools$Borough))[[1]],"All")
  
  #Create District Level Data for Map
  district_wise <- schools %>% group_by(District)
  district_data <- district_wise %>% summarise(
    eco_need = mean(Economic.Need.Index, na.rm = TRUE),
    avg_income = mean(School.Income.Estimate, na.rm = TRUE),
    avg_attendance = mean(Student.Attendance.Rate, na.rm = TRUE),
    avg_black = mean(Percent.Black, na.rm = TRUE),
    avg_asian = mean(Percent.Asian, na.rm = TRUE),
    avg_ell = mean(Percent.ELL, na.rm = TRUE),
    avg_hispanic = mean(Percent.Hispanic, na.rm = TRUE),
    avg_white = mean(Percent.White, na.rm = TRUE),
    avg_chronic_absentee = mean(Percent.of.Students.Chronically.Absent, na.rm = TRUE),
    avg_ELA_prof = round(mean(Average.ELA.Proficiency, na.rm = TRUE), 2),
    avg_math_prof = round(mean(Average.Math.Proficiency, na.rm = TRUE), 2),
    avg_offer_rate = round(mean(Perc_Offers, na.rm = TRUE), 2),
    avg_tester_rate = round(mean(Perc_Testers, na.rm = TRUE), 2),
    offer_school_count = sum(Offer),
    total_schools = n(),
    percent_offer_schools = (offer_school_count / total_schools) * 100
    
  )
  
  district_data <- left_join(district_data, Districts, by = c("District" = "Number"))
  
  
  
  #reading  in the shapefile
  schooldistricts <- readOGR("nycschooldistrict", layer = "nycschooldistrict",encoding = "UTF-8")
  
  #Import Specialized High School Locations
  sshlocations <- read.csv("SpecializedHighSchools.csv")
  
  
  #Compare Plot Data Prep
  schools_copy$Offer <- as.numeric(schools_copy$Offer)
  
  district_wise_comp <- schools_copy %>% group_by(District)
  compareplot_data <- district_wise_comp %>% summarise(
    eco_need = mean(Economic.Need.Index, na.rm = TRUE),
    avg_income = mean(School.Income.Estimate, na.rm = TRUE),
    avg_attendance = mean(Student.Attendance.Rate, na.rm = TRUE),
    avg_black = mean(Percent.Black, na.rm = TRUE),
    avg_asian = mean(Percent.Asian, na.rm = TRUE),
    avg_ell = mean(Percent.ELL, na.rm = TRUE),
    avg_hispanic = mean(Percent.Hispanic, na.rm = TRUE),
    avg_white = mean(Percent.White, na.rm = TRUE),
    avg_chronic_absentee = mean(Percent.of.Students.Chronically.Absent, na.rm = TRUE),
    avg_ELA_prof = mean(Average.ELA.Proficiency, na.rm = TRUE),
    avg_math_prof = mean(Average.Math.Proficiency, na.rm = TRUE),
    avg_offer_rate = round(mean(Perc_Offers, na.rm = TRUE), 2),
    avg_tester_rate = round(mean(Perc_Testers, na.rm = TRUE), 2)
    
  )
  
  compareplot_data <- compareplot_data[,1:13]
  compareplot_data <- data.frame(compareplot_data)
  
  compareplot_data$District <- sapply(compareplot_data$District, clean_district)
  
  #Create input list for dropdown
  compare_input <- append(list(unique(compareplot_data$District))[[1]],"All")
  
  city_avg <-data.frame("All", 
                        mean(compareplot_data$eco_need),
                        mean(compareplot_data$avg_income, na.rm = TRUE),
                        mean(compareplot_data$avg_attendance),
                        mean(compareplot_data$avg_black),
                        mean(compareplot_data$avg_asian),
                        mean(compareplot_data$avg_ell),
                        mean(compareplot_data$avg_hispanic),
                        mean(compareplot_data$avg_white),
                        mean(compareplot_data$avg_chronic_absentee),
                        mean(compareplot_data$avg_ELA_prof),
                        mean(compareplot_data$avg_math_prof),
                        mean(compareplot_data$avg_offer_rate)
  )
  
  names(city_avg)<-c("District","eco_need","avg_income", "avg_attendance", 
                     "avg_black", "avg_asian", "avg_ell", "avg_hispanic",
                     "avg_white", "avg_chronic_absentee", "avg_ELA_prof",
                     "avg_math_prof", "avg_offer_rate")
  
  
  plotdata <- rbind(compareplot_data, city_avg)
  plotdata$avg_performance <- ((plotdata$avg_ELA_prof + plotdata$avg_math_prof) / 2)
  plotdata$avg_blackhispanic <- ((plotdata$avg_black + plotdata$avg_hispanic))*100
  plotdata$avg_chronic_absentee <- plotdata$avg_chronic_absentee * 100
  plotdata$avg_ell <- plotdata$avg_ell * 100
  
  plotdata <- plotdata[,c("District","eco_need","avg_ell", 
                          "avg_blackhispanic",
                          "avg_chronic_absentee", "avg_performance",
                          "avg_offer_rate" )]
  
  colnames(plotdata) <- c("District", "Average Economic Need",
                          "Average % of English Language Learners", 
                          "Average % of Black or Hispanic Students",
                          "Average Rate of Chronic Absenteeism (%)",
                          "Average Performance",
                          "Average Offer Rate (%)")
  
  #Gathering the data for plotting
  comparedata <- gather(plotdata, key = category, value = value, `Average Economic Need`:`Average Offer Rate (%)`, factor_key=TRUE)
  
  
  
  
  
  #-------------------------#
  # Welcome
  #-------------------------#
  
  
  #-------------------------#
  # Map
  #-------------------------#
  
  # using a left_join because the datafile and the shapefile line up EXACTLY - if they dont, you cant use left join
  schooldistricts@data <- left_join(schooldistricts@data, district_data, by = c("school_dis" = "District"))
  
  # writing the popup values
  district_popup <- paste0("<strong>School District: </strong>", 
                           schooldistricts$school_dis, 
                           "<br>Borough: ",
                           schooldistricts$Borough,
                           "<br>Avg Economic Need: ",
                           round(schooldistricts$eco_need,2),
                           "<br>Avg School Income: ",
                           round(schooldistricts$avg_income,2),
                           "<br>Avg Student Attendance: ",
                           round(schooldistricts$avg_attendance,2),
                           "<br>Avg ELA Proficiency: ",
                           round(schooldistricts$avg_ELA_prof, 2),
                           "<br>Avg Math Proficiency: ",
                           round(schooldistricts$avg_math_prof, 2),
                           "<br>Avg Offer Rate: ",
                           round(schooldistricts$avg_offer_rate, 2),
                           "<br>Avg Testing Rate: ",
                           round(schooldistricts$avg_tester_rate, 2)
  )
  
  eco_need_map <- function(){
    
    #Create bins for Economic Need
    schooldistricts$eco_need_buckets <- ifelse(schooldistricts$eco_need <= 0.3, "0.00 - 0.30", ifelse(schooldistricts$eco_need <= 0.5, "0.30 - 0.50", ifelse(schooldistricts$eco_need <= 0.7, "0.50 - 0.70", "0.70 - 1.00")) )
    
    # make  variable a factor
    #schooldistricts$school_dis <- factor(schooldistricts$school_dis)
    
    # coloring by factor level
    factpal <- colorFactor(c("#CDC604","#fcf76e","#f6c4c9","#e45664"), schooldistricts$eco_need_buckets)
    
    # Notice I am putting this in "map" object
    map<-leaflet(schooldistricts, options = leafletOptions(zoomControl = FALSE)) %>%fitBounds(lng1 = -73.90402, 
                                                                                              lat1 = 40.5178, 
                                                                                              lng2 = -73.75892, 
                                                                                              lat2 = 40.89346)%>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .6,
                  color = "#444444",
                  fillColor = ~factpal(eco_need_buckets),
                  weight = 1, 
                  popup = district_popup) %>%
      addLegend("topright", 
                colors =c("#CDC604","#fcf76e","#f6c4c9","#e45664"),
                labels= c("0.00 - 0.30", "0.30 - 0.50","0.50 - 0.70","0.70 - 1.00"),  
                title= "Economic Need",
                opacity = 1) 
    
    return(map)
    
  }
  
  race_dist_map <- function(){
    
    #Create bins for Black/Hispanic Percentage
    schooldistricts$black_or_hispanic_perc <- ifelse(schooldistricts$avg_black + schooldistricts$avg_hispanic <= 0.2, "0.00 - 0.20", 
                                                     ifelse(schooldistricts$avg_black + schooldistricts$avg_hispanic <= 0.4, "0.20 - 0.40", 
                                                            ifelse(schooldistricts$avg_black + schooldistricts$avg_hispanic <= 0.6, "0.40 - 0.60", 
                                                                   ifelse(schooldistricts$avg_black + schooldistricts$avg_hispanic <= 0.8, "0.60 - 0.80", "0.80 - 1.00"))) )
    
    
    factpal_race <- colorFactor(c("#3cceca", "#2baba8","#1c6e6c","#175a58","#124644"), schooldistricts$black_or_hispanic_perc)
    
    race_map <- leaflet(schooldistricts, options = leafletOptions(zoomControl = FALSE)) %>%fitBounds(lng1 = -73.90402, 
                                                                                                     lat1 = 40.5178, 
                                                                                                     lng2 = -73.75892, 
                                                                                                     lat2 = 40.89346)%>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .6,
                  color = "#444444",
                  fillColor = ~factpal_race(black_or_hispanic_perc),
                  weight = 1, 
                  popup = district_popup) %>%
      addLegend("topright", 
                colors =c("#3cceca", "#2baba8","#1c6e6c","#175a58","#124644"),
                labels= c("0% - 20%", "20% - 40%","40% - 60%","60 - 80%", "80% - 100%"),  
                title= "% Black or Hispanic Students",
                opacity = 1) 
    
    return(race_map)
    
  }
  
  performance_map <- function(){
    
    #Create bins for Avg Performance Percentage
    schooldistricts$avg_performance <- (schooldistricts$avg_ELA_prof + schooldistricts$avg_math_prof)/2
    schooldistricts$performance_buckets <- ifelse(schooldistricts$avg_performance <= 2.00, "0.00 - 2.00", 
                                                  ifelse(schooldistricts$avg_performance <= 2.33, "2.00 - 2.33", 
                                                         ifelse(schooldistricts$avg_performance <= 2.67, "2.33 - 2.67", 
                                                                ifelse(schooldistricts$avg_performance <= 3.00, "2.67 - 3.00", "3.00 - 4.00")))) 
    
    # make buckets variable a factor
    schooldistricts$performance_buckets <- factor(schooldistricts$performance_buckets)
    
    
    factpal_performance <- colorFactor(c("#e45664","#f6c4c9","#fcf76e","#CDC604"), schooldistricts$performance_buckets)
    
    performance_map1 <- leaflet(schooldistricts, options = leafletOptions(zoomControl = FALSE)) %>%fitBounds(lng1 = -73.90402, 
                                                                                                             lat1 = 40.5178, 
                                                                                                             lng2 = -73.75892, 
                                                                                                             lat2 = 40.89346)%>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .6,
                  color = "#444444",
                  fillColor = ~factpal_performance(performance_buckets),
                  weight = 1, 
                  popup = district_popup) %>%
      addLegend("topright", 
                colors =c("#e45664","#f6c4c9","#fcf76e","#CDC604"),
                labels= c("2.00 - 2.33", "2.33 - 2.67", "2.67 - 3.00", "3.00 - 4.00"),  
                title= "Average Performance (ELA and Math)",
                opacity = 1) 
    
    return(performance_map1)
    
  }
  
  #Offer Rate % Map
  offer_map <- function(){
    
    schooldistricts$avg_offer_rate <- as.numeric(schooldistricts$avg_offer_rate)
    
    #Create bins for Offer Percentage
    schooldistricts$offer_buckets <- ifelse(schooldistricts$avg_offer_rate <= 4.00, "0% - 4%", 
                                            ifelse(schooldistricts$avg_offer_rate <= 8.00, "4% - 8%", 
                                                   ifelse(schooldistricts$avg_offer_rate <= 12.00, "8% - 12%", 
                                                          "12%+"))) 
    
    # make buckets variable a factor
    schooldistricts$offer_buckets <- factor(schooldistricts$offer_buckets, levels = c("0% - 4%", "4% - 8%", "8% - 12%", "12%+"))
    
    
    factpal_offers <- colorFactor(c("#e7f7f7","#ace3e3","#71d0d0","#45c1c1"), schooldistricts$offer_buckets)
    
    offer_map1 <- leaflet(schooldistricts, options = leafletOptions(zoomControl = FALSE)) %>%fitBounds(lng1 = -73.90402, 
                                                                                                       lat1 = 40.5178, 
                                                                                                       lng2 = -73.75892, 
                                                                                                       lat2 = 40.89346)%>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .7,
                  color = "#444444",
                  fillColor = ~factpal_offers(offer_buckets),
                  weight = 1, 
                  popup = district_popup) %>%
      addLegend("topright", 
                colors =c("#e7f7f7","#ace3e3","#71d0d0","#45c1c1"),
                labels= c("0% - 4%", "4% - 8%", "8% - 12%", "12%+"),  
                title= "Average Offer Rate",
                opacity = 1) 
    
    return(offer_map1)
    
  }
  
  
  #Testing Rate % Map
  tester_map <- function(){
    
    schooldistricts$avg_tester_rate <- as.numeric(schooldistricts$avg_tester_rate)
    
    #Create bins for Tester Percentage
    schooldistricts$tester_buckets <- ifelse(schooldistricts$avg_tester_rate <= 15.00, "0% - 15%", 
                                             ifelse(schooldistricts$avg_tester_rate <= 25.00, "15% - 25%", 
                                                    ifelse(schooldistricts$avg_tester_rate <= 35.00, "25% - 35%", 
                                                           "35%+"))) 
    
    # make buckets variable a factor
    schooldistricts$tester_buckets <- factor(schooldistricts$tester_buckets, levels = c("0% - 15%", "15% - 25%", "25% - 35%", "35%+"))
    
    
    factpal_testers <- colorFactor(c("#fcf76e","#fbf323","#cec604","#827e03"), schooldistricts$tester_buckets)
    
    tester_map <- leaflet(schooldistricts, options = leafletOptions(zoomControl = FALSE)) %>%fitBounds(lng1 = -73.90402, 
                                                                                                       lat1 = 40.5178, 
                                                                                                       lng2 = -73.75892, 
                                                                                                       lat2 = 40.89346)%>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .6,
                  color = "#444444",
                  fillColor = ~factpal_testers(tester_buckets),
                  weight = 1, 
                  popup = district_popup) %>%
      addLegend("topright", 
                colors =c("#fcf76e","#fbf323","#cec604","#827e03"),
                labels= c("0% - 15%", "15% - 25%", "25% - 35%", "35%+"),  
                title= "Average Testing Rate",
                opacity = 1) 
    
    return(tester_map)
    
  }
  
  
  
  #Create markers
  schoolIcons <- icons(
    iconUrl = "schoolicon.png",
    iconWidth = 20, iconHeight = 20)
  
  #Create Map output
  output$map1 <- renderLeaflet({
    
    if(input$mapinput == "Economic Need"){
      x <- eco_need_map()
    } else if(input$mapinput == "Race"){
      x <- race_dist_map()
    } else if (input$mapinput == "Performance"){
      x <- performance_map()
    } else if (input$mapinput == "Testers"){
      x <- tester_map()
    }
    else {
      x <- offer_map()
    }
    return(x %>% addMarkers(data = sshlocations, lng = ~Longitude, lat = ~Latitude, icon = schoolIcons, popup = ~Name))
    
  })
  
  output$map2 <- renderLeaflet({
    
    if(input$mapinput2 == "Economic Need"){
      x <- eco_need_map() 
    } else if(input$mapinput2 == "Race"){
      x <- race_dist_map()
    } else if (input$mapinput2 == "Performance"){
      x <- performance_map()
    } else if (input$mapinput2 == "Testers"){
      x <- tester_map()
    }
    else {
      x <- offer_map()
    }
    
    return(x %>% addMarkers(data = sshlocations, lng = ~Longitude, lat = ~Latitude, icon = schoolIcons, popup = ~Name))
  })
  
  #Create School Map - by Offer Rate
  school_popup <- paste0("<strong>School Name: </strong>", 
                         schools$School.Name, 
                         "<br>Offer Rate: ",
                         round(schools$Perc_Offers,2),
                         "%")
  
  school_map <- function(){
    
    pal <- colorNumeric(palette = c("#ef98a0","#e86c78", "#b71d2d"), domain = schools$Perc_Offers)
    
    school_map1 <- leaflet(schooldistricts) %>%fitBounds(lng1 = -73.89402, 
                                                         lat1 = 40.5278, 
                                                         lng2 = -73.76892, 
                                                         lat2 = 40.88346)%>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lat = 40.699622, lng = -73.872494,zoom = 11) %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .3,
                  color = "#444444",
                  fillColor = "white",
                  weight = 1, 
                  popup = district_popup) %>%
      addLegend("topright", 
                pal = pal,
                values = schools$Perc_Offers,
                labFormat = labelFormat(suffix = "%"),
                title= "Offer Rate (%)",
                opacity = 1) %>% 
      addCircleMarkers(data = schools, lat = ~Latitude, lng = ~Longitude,
                       fillColor = ~pal(Perc_Offers), color = ~pal(Perc_Offers),
                       fillOpacity =  1,
                       radius = ~Perc_Offers/5, popup = school_popup) %>% 
      addMarkers(data = sshlocations, lng = ~Longitude, lat = ~Latitude, icon = schoolIcons, popup = ~Name)
    
    return(school_map1)
    
  }
  
  output$schoolMap <- renderLeaflet({school_map()})
  
  
  
  #------------------
  # Random Forest
  #------------------
  #Cleaning up and formatting data for the RF Model
  schools$AverageProficiency <- (schools$Average.ELA.Proficiency + schools$Average.Math.Proficiency) / 2
  schools$Perc_Black_Hispanic <- schools$Percent.Black + schools$Percent.Hispanic
  schools$Perc_Black_Hispanic <- schools$Perc_Black_Hispanic * 100
  schools$Offer <- factor(schools$Offer, levels = c(0,1))
  schools$Borough <- factor(schools$Borough, levels = c('Manhattan', 'Staten', 'Bronx', 'Brooklyn', 'Queens'))
  schools$District <- factor(schools$District, levels = c('2','1','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32'))
  schools$Percent.of.Students.Chronically.Absent <- schools$Percent.of.Students.Chronically.Absent * 100
  schools$Percent.ELL <- schools$Percent.ELL * 100
  
  #creating a reactive regression forumula that uses inputs from the check list
  #as independent variables to predict the variable ACE_BI
  rfFormula<- eventReactive(input$go, {
    as.formula(paste("Perc_Offers"," ~ ",paste(input$rfinputs,collapse="+")))
  },
  ignoreNULL = FALSE)
  
  
  # then, put that formula into the randomForest()
  output$rfVarPlot <- renderPlot({
    
    schools <- na.omit(schools[,c("Perc_Offers", "Borough",
                                  "Economic.Need.Index", "Student.Attendance.Rate",
                                  "Percent.of.Students.Chronically.Absent",
                                  "Total_Students", "AverageProficiency",
                                  "Perc_Black_Hispanic", "Percent.ELL",
                                  "District", "Perc_Testers", 
                                  "Average.ELA.Proficiency", 
                                  "Average.Math.Proficiency")])
    
    index2 = createDataPartition(schools$Perc_Offers, p= 0.9,list = FALSE)
    train2 = schools[index2,]
    test2 = schools[-index2,]
    
    rfmodel <- randomForest(rfFormula(), 
                            data = train2,
                            ntree=500, 
                            mtry = 3,
                            importance = TRUE)
    
    varimp_results <- as.data.frame(importance(rfmodel, type = 1, scale = F))
    varimp_results$Feature <- row.names(varimp_results)
    colnames(varimp_results) <- c("Score", "Feature")
    
    varimp_results <- left_join(varimp_results, feature_labels, by = c("Feature" = "Variable"))
    #Ordering by importance
    varimp_results <- varimp_results %>% arrange(Score)
    varimp_results$Label <- factor(varimp_results$Label, levels = varimp_results$Label)
    
    importancePlot <- ggplot(varimp_results, aes(x = Score, y = Label, color = Score)) +
      geom_point(stat = 'identity', shape = 16, size = 20, show.legend = FALSE, alpha = .8) + 
      xlab("Importance") +
      ylab("Feature") +
      title("Comparing Feature Importance") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme_minimal(base_size = 18) +
      scale_color_gradient(low = "#CDC604", high = "#DF5867")
    
    return(importancePlot)
    
  })
  
  ###------------------------------
  # CORRELATION MATRIX
  ###------------------------------
  
  #Create correlation matrix
  schools_corr$AverageProficiency <- (schools_corr$Average.ELA.Proficiency + schools_corr$Average.Math.Proficiency) / 2
  schools_corr$Perc_Black_Hispanic <- schools_corr$Percent.Black + schools_corr$Percent.Hispanic
  schools_corr$Perc_Black_Hispanic <- schools_corr$Perc_Black_Hispanic * 100
  schools_corr$Offer <- factor(schools_corr$Offer, levels = c(0,1))
  schools_corr$Percent.of.Students.Chronically.Absent <- schools_corr$Percent.of.Students.Chronically.Absent * 100
  schools_corr$Percent.ELL <- schools_corr$Percent.ELL * 100
  
  schools_corr <- na.omit(schools_corr[,c("Perc_Offers",
                                          "Economic.Need.Index", "Student.Attendance.Rate",
                                          "Percent.of.Students.Chronically.Absent",
                                          "Total_Students",
                                          "Perc_Black_Hispanic", "Percent.ELL",
                                          "Perc_Testers", 
                                          "Average.ELA.Proficiency", 
                                          "Average.Math.Proficiency")])
  
  colnames(schools_corr) <- c("Offer Rate",
                              "Economic Need", "Attendance Rate",
                              "Rate of Chronic Absenteeism",
                              "Total Student Count",
                              "% of Black or Hispanic Students", 
                              "% of English Language Learners",
                              "Testing Rate", 
                              "Average ELA Performance", 
                              "Average Math Performance")
  
  cormat <- round(cor(schools_corr),2)
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  upper_tri <- get_upper_tri(cormat)
  
  melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
  
  #Create plot
  correlation_plot <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "#E45664", high = "#CDC604", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 14, hjust = 1),
          axis.text.y = element_text(size = 14),
          legend.text=element_text(size=12))+
    coord_fixed()+
    xlab("")+
    ylab("")
  
  output$correlationPlot <- renderPlot(correlation_plot)
  
  
  ###------------------------------
  # COMPARE PLOTS
  ###------------------------------
  
  output$comparePlot1 <- renderPlot({
    
    
    row1 <- subset(comparedata, District == input$comparisoninput1)
    final <- row1
    final$District <- factor(final$District, levels = unique(final$District))
    
    charts<- ggplot(data = final) + 
      geom_bar(aes(x = category, y = value, fill = category), stat="identity") + 
      #facet_wrap(~category, scales = "free") +
      scale_fill_manual(values = c("#E45664", "#CDC604", "#1C5D6F",
                                   "#54c6c6", "#C7CBCA", "#E45664")) +
      coord_flip() +
      scale_y_continuous(limit = c(0, 100)) +
      ylab("") +
      xlab("") +
      geom_text(aes(label = round(value, 2), x = category, y = value), stat= "identity",
                vjust = 0.5, hjust = 0, size = 5) +
      theme(legend.position = "none", text = element_text(size=14), panel.background = element_rect(fill="white"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.y = element_blank(), axis.ticks = element_blank(),
            axis.text.x = element_blank())
    
    return(charts)
  })
  
  output$comparePlot2 <- renderPlot({
    
    row2 <- subset(comparedata, District == input$comparisoninput2)
    final <- row2
    final$District <- factor(final$District, levels = unique(final$District))
    
    charts<- ggplot(data = final) + 
      geom_bar(aes(x = category, y = value, fill = category), stat="identity") + 
      #facet_wrap(~category, scales = "free") +
      scale_fill_manual(values = c("#E45664", "#CDC604", "#1C5D6F",
                                   "#54c6c6", "#C7CBCA", "#E45664")) +
      coord_flip() +
      scale_y_continuous(limit = c(0, 100)) +
      ylab("") +
      xlab("") +
      geom_text(aes(label = round(value, 2), x = category, y = value), stat= "identity", 
                vjust = 0.5, hjust = 0, size = 5) +
      theme(legend.position = "none", text = element_text(size=14), panel.background = element_rect(fill="white"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.y = element_blank(), axis.ticks = element_blank(),
            axis.text.x = element_blank())
    
    return(charts)
  })
  
  
  #--------------------
  # DATA TABLE
  #--------------------
  
  output$datatable <- DT::renderDataTable(DT::datatable({
    
    #Create data variable
    data1 <- data.frame(filter_schools)
    
    data1 <- data1[,c("School.Name", "District","Address..Full.","Borough",
                      "Economic.Need.Index", "School.Income.Estimate", 
                      "Percent.ELL","Percent.Black.Hispanic",
                      "Student.Attendance.Rate", 
                      "Percent.of.Students.Chronically.Absent",
                      "Average.ELA.Proficiency",
                      "Average.Math.Proficiency", 
                      "Average.Total.Proficiency",
                      "Offer", "Perc_Testers",
                      "Perc_Offers")]
    
    # Allowing the FILTERS to work
    
    data1 <- data1[which((data1$Perc_Offers >=  
                            input$filterofferrate[1])
                         & (data1$Perc_Offers <=
                              input$filterofferrate[2])),]
    
    data1 <- data1[which((data1$Perc_Testers >=  
                            input$filtertestingrate[1])
                         & (data1$Perc_Testers <=
                              input$filtertestingrate[2])),]
    
    if (input$filterdistrict != "All"){
      data1 <- data1[data1$District == input$filterdistrict,]
    }
    
    if (input$filterborough != "All"){
      data1 <- data1[data1$Borough == input$filterborough,]
    }
    
    data1 <- data1[which((data1$Average.Math.Proficiency >=  
                            input$filtermathperformance[1])
                         & (data1$Average.Math.Proficiency <=
                              input$filtermathperformance[2])),]
    
    data1 <- data1[which((data1$Average.ELA.Proficiency >=  
                            input$filterelaperformance[1])
                         & (data1$Average.ELA.Proficiency <=
                              input$filterelaperformance[2])),]
    
    data1 <- data1[which((data1$Economic.Need.Index >=  
                            input$filterneed[1])
                         & (data1$Economic.Need.Index <=
                              input$filterneed[2])),]
    
    data1 <- data1[which((data1$Percent.Black.Hispanic >=  
                            input$filterrace[1])
                         & (data1$Percent.Black.Hispanic <=
                              input$filterrace[2])),]
    
    data1 <- data1[which((data1$Percent.ELL >=  
                            input$filterell[1])
                         & (data1$Percent.ELL <=
                              input$filterell[2])),]
    
    data1 <- data1[which((data1$Percent.of.Students.Chronically.Absent >=  
                            input$filterabsenteeism[1])
                         & (data1$Percent.of.Students.Chronically.Absent <=
                              input$filterabsenteeism[2])),]
    
    data1 <- data1[,c("School.Name", "District","Address..Full.","Borough",
                      "Percent.Black.Hispanic","Average.Total.Proficiency",
                      "Perc_Testers","Perc_Offers")]
    
    data1
  },
  colnames = c("School Name", "District","Address","Borough","% Black/Hispanic","Average Proficiency","Testing Rate","Offer Rate")
  
  ))
  
}




#-----------------------------------#
# UI                                #
# Navigation Bar II                 #
#                                   #
#-----------------------------------#

# Here's our new UI code

# Notice there is no function here, just the fluidPage()
ui <- fluidPage(theme = shinytheme("journal"),
                tags$style(HTML(".tabs-above > .nav > li[class=active] > a {background-color: #CACBCB;color: #FFF;}")),
                # navbarPage() is making multiple tabs in our app
                tags$style(type = 'text/css',
                           
                           '.navbar-default .navbar-brand {
                           color: #E45664;
                           }'
                           
                ),
                navbarPage(
                  "PASSNYC App",
                  theme = shinytheme("journal"),
                  position = "fixed-top",
                  #this is the title for our navigation bar
                  tabPanel(
                    "Welcome", 
                    theme = shinytheme("journal"),
                    br(),
                    br(),
                    img(src = "WelcomeGraphic.png", width = "100%"),
                    br(),
                    h4("Data Sources: PassNYC School Explorer 2016 (Combines data from NYC Education Department, New York State Education Department and NYC Open Data),
                       SHSAT Admissions Test Offers by Sending School 2016-17 (NYC Open Data)")
                    
                    ),
                  tabPanel(
                    "Spatial Patterns",
                    theme = shinytheme("journal"),
                    br(),
                    br(),
                    br(),
                    headerPanel("Comparing Spatial Patterns"),
                    br(),
                    h2(""),
                    column(width = 3,
                           selectInput(
                             "mapinput",
                             label = "Pattern 1: ",
                             choices = list("Economic Need" = "Economic Need", 
                                            "Race" = "Race", 
                                            "Performance" = "Performance",
                                            "Offer Rate" = "Offers",
                                            "Testing Rate" = "Testers"),
                             selected = "Economic Need"
                             #Note Economic Need is selected when the app starts
                           )),
                    column(width = 3,offset = 3,
                           selectInput(
                             "mapinput2",
                             label = "Pattern 2: ",
                             choices = list("Economic Need" = "Economic Need", 
                                            "Race" = "Race", 
                                            "Performance" = "Performance",
                                            "Offer Rate" = "Offers",
                                            "Testing Rate" = "Testers"),
                             selected = "Race"
                             #Note Economic Need is selected when the app starts
                           )),
                    h2(""),
                    h2(""),
                    h2(""),
                    fluidRow(column(6, leafletOutput("map1", width = "95%", height = "500px")),
                             column(6, leafletOutput("map2", width = "95%", height = "500px"))),
                    h2(""),
                    h2(""),
                    h2("Mapping Middle Schools by Offer Rate"),
                    fluidRow(
                      column(12, leafletOutput("schoolMap", width = "100%", height = "600px"))
                    )
                    
                    
                    
                  ),
                  
                  tabPanel(
                    "Feature Importance",
                    tags$head(
                      tags$style("h2 {color: #04B4AE; }
                                 h1 {color: #04B4AE}; }
                                 
                                 ")),
                    br(),
                    br(),
                    br(),
                    headerPanel("Understanding Feature Relationships"),
                    fluidRow(column(width = 5, 
                                    h3("Target Feature/Variable"),
                                    h4("The target variable for our investigation is the Percentage of Offers to Specialized High Schools that a School recieves, what we are calling the 'Offer Rate'."),
                                    br(),
                                    h3("Correlation Matrix"),
                                    h4("A correlation matrix is a table showing correlation coefficients between features These coefficients depict whether any two features have a linear relationship with one another. These values range between -1 and 1, and express the strength and direction (+ve/-ve) of relationships between features in our dataset. Each cell in the matrix alongside shows the correlation between two features"),
                                    br(),
                                    h3("Insight"),
                                    h4("In its bottom-most row, the correlation matrix tells us which features are positively related to a school's Offer Rate, and those that are negatively related.")
                    ),
                    column(width = 5, align = "right",
                           plotOutput("correlationPlot", width = "800px", height = "600px")
                    )),
                    headerPanel( "Using Random Forests to Determine Feature Importance"),
                    #The sidebar allows me to select
                    #multiple independent variables
                    br(),
                    h4("A Random Forest Regressor allows us to run a linear regression to predict our target variable - Offer Rate. 
                       It outputs a measure of Feature Importance, which tells us the relative importance of our prediction features 
                       in determining a school's Offer Rate. We can then use these features to specifically identify schools and districts
                       of need."),
                    sidebarLayout(position = "right",
                                  sidebarPanel(
                                    h2("Build your model"),
                                    br(),
                                    checkboxGroupInput("rfinputs", 
                                                       label = "Select any of the features below to include in your model and press 'Run Model' to see results", 
                                                       list("Average ELA Performance"="Average.ELA.Proficiency", 
                                                            "Average Math Performance"="Average.Math.Proficiency", 
                                                            "NYC Borough"="Borough",
                                                            "School District"="District",
                                                            "Economic Need"="Economic.Need.Index", 
                                                            "Rate of Chronic Absenteeism" = "Percent.of.Students.Chronically.Absent",
                                                            "Total Student Count"="Total_Students",
                                                            "% of Black or Hispanic Students"="Perc_Black_Hispanic",
                                                            "% of English Language Learners"="Percent.ELL"
                                                       ), 
                                                       selected= list("Average.ELA.Proficiency", "Economic.Need.Index",
                                                                      "Average.Math.Proficiency", "Borough",
                                                                      "District")
                                                       #Note Avg ELA Performance is selected when the app starts
                                    ),
                                    tags$head(
                                      tags$style(HTML('#go{background-color:#E45664}'))
                                    ),
                                    div(style="display:inline-block",actionButton("go", "Run Model"), style="float:right")
                                    
                                  ),
                                  
                                  
                                  
                                  mainPanel(
                                    br(),
                                    fluidRow(
                                      plotOutput("rfVarPlot")
                                    ),
                                    h1("")
                                    
                                  )
                    )
                    
                    ), 
                  
                  tabPanel(
                    "District Comparisons",
                    tags$head(
                      tags$style("h2 {color: #04B4AE; }
                                 h1 {color: #04B4AE}; }
                                 
                                 ")),
                    br(),
                    br(),
                    br(),
                    headerPanel( "Comparing School Districts"),
                    h2("             "),
                    h4("Pick any two School Districts to compare: "),
                    br(),
                    br(),
                    fluidRow(height = 1),
                    fluidRow(column(width = 3,
                                    offset = 3,
                                    selectInput(
                                      "comparisoninput1",
                                      label = "District 1: ",
                                      choices = compare_input,
                                      selected = "1"
                                      #Note Economic Need is selected when the app starts
                                    )),
                             column(width = 3, offset = 1,
                                    selectInput(
                                      "comparisoninput2",
                                      label = "District 2: ",
                                      choices = compare_input,
                                      selected = "All"
                                      #Note All is selected when the app starts
                                    ))),
                    h2(""),
                    fluidRow(
                      column(width = 3,
                             br(),
                             h4("Average Offer Rate (%)"),
                             br(),
                             h4("Average Performance (0-4)"),
                             br(),
                             h4("Average Rate of Chronic Absenteeism (%)"),
                             br(),
                             h4("Average % Black/Hispanic Students"),
                             br(),
                             h4("Average % English Language Learners"),
                             br(),
                             h4("Average Economic Need (0-1)")),
                      column(width = 3, height = "500px", offest = 1, plotOutput("comparePlot1"))
                      ,
                      column(width = 3, height = "500px", offset = 1, plotOutput("comparePlot2"))
                      
                    )
                      ),
                  #Add data filtering tab
                  tabPanel(
                    "School Identification",
                    br(),
                    br(),
                    br(),
                    headerPanel("Identifying Schools for Intervention"),
                    br(),
                    h4("Outreach & Awareness: Schools with low Testing Rates but High Performance"),
                    h4("Test Prep & Mentoring: Schools with high Performance but low Offer Rate"),
                    h4("Identifying Potential: Schools with high proportion of underrepresented students, with high Average Performance"),
                    br(),
                    sidebarLayout(position = 'left', 
                                  sidebarPanel(
                                    width = 3,
                                    sliderInput("filterofferrate", 
                                                "Offer Rate: ", 
                                                0, 
                                                100, 
                                                c(0,100),
                                                step = 10),
                                    sliderInput("filtertestingrate", 
                                                "Testing Rate: ", 
                                                0, 
                                                100, 
                                                c(0,100),
                                                step = 10),
                                    selectInput(
                                      "filterborough",
                                      label = "Borough: ",
                                      choices = borough_input,
                                      selected = "All"
                                      #Note All is selected when the app starts
                                    ),
                                    selectInput(
                                      "filterdistrict",
                                      label = "School District: ",
                                      choices = district_input,
                                      selected = "All"
                                      #Note All is selected when the app starts
                                    ),
                                    sliderInput("filtermathperformance", 
                                                "Average Math Performance", 
                                                0, 
                                                4, 
                                                c(0,4),
                                                step = 0.5),
                                    sliderInput("filterelaperformance", 
                                                "Average ELA Performance", 
                                                0, 
                                                4, 
                                                c(0,4),
                                                step = 0.5),
                                    sliderInput("filterneed", 
                                                "Economic Need", 
                                                0, 
                                                1, 
                                                c(0,1)),
                                    sliderInput("filterrace", 
                                                "Black/Hispanic Student Proportion", 
                                                0, 
                                                100, 
                                                c(0,100)),
                                    sliderInput("filterell", 
                                                "ELL Student Proportion", 
                                                0, 
                                                100, 
                                                c(0,100)),
                                    sliderInput("filterabsenteeism", 
                                                "Rate of Chronic Absenteeism", 
                                                0, 
                                                100, 
                                                c(0,100)),
                                    
                                    tags$head(
                                      tags$style(HTML('#go{background-color:#E45664}'))
                                    )
                                  ),
                                  mainPanel(
                                    fluidRow(DT::dataTableOutput("datatable"))
                                  ))
                  )
                  
                    ))
# This is our new code to merge the server and ui objects into an app!
shinyApp(ui = ui, server = server)

