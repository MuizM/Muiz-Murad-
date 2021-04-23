#Load Libraries 
library(dplyr)
library(stringr)
library(lubridate)
library(eeptools)
library(purrr)
library(readr)
library(expss)
library(tidyr)
library(ggplot2)

#Load Data
players <- read.csv("players.csv") #read data in a csv format into R
seasons <- read.csv("seasons.csv")
match_stats <- read.csv("match_stats.csv")

#Check Data STR 
str(players) #make sure data format is correct for investigation 

#Make New Players Data frame Structure
newplayers = data.frame(matrix(vector(), nrow(players), ncol(players),
                               dimnames=list(c(), c("player ID", 'first name', "last name","age","BMI"))))

#Change Names To Character
##Format of the names need to be changed to make sure we can use functions on the name data
players$full_name <- as.character(players$full_name)

#First Name 
##Strip the full name to extract the first part only, for middle names, this is factored in below
firstname = sapply(strsplit(players$full_name,' '), function(x) x[1])
newplayers$first.name <- firstname

#Last Name 
##Strip the full name to extract the last part only 
lastname = sapply(strsplit(players$full_name, ' '), function(x) x[length(x)])
newplayers$last.name <- lastname

#Player ID
##Rename player id to make it an easy to use variable name 
playerid <- players$pid
newplayers$player.ID <- playerid

#Age (As At 2015/12/31)

##Change dates to date class objects to enable them to be used within the function 
players$dob <- as.Date(players$dob)
to_date <- as.Date("2015/12/31")

##Create for loop calculation for age in years
for (i in 1:length(newplayers)){ 
  newplayers$age <- age_calc(players$dob,enddate =to_date,unit="years",precise = FALSE)}

#BMI
##Divide height by 100 to get it into meters as requested, weight is already in KG
for (i in 1:length(newplayers)){ 
  newplayers$BMI <- (players$weight / (players$height/100)^2)}

#Clean Up Column Headers 
##This will ensure clarity when looking at dataset 
names(newplayers[1:3,5]) <- gsub("\\.", "", names(newplayers[1:3,5]))

#Write NewPlayers Data To CSV 
write.csv(newplayers,"D:\\AssignmentOne\\Data\\NewPlayers.csv")

######################################################################################

#Seasons DataFrame 
##Create new dataframe with all of the existing seasons data frame columns 
newseasons <- subset(seasons,select=c(1:12))

#Margin Column 
##Create loop to calculate difference between the win and lose score defined as the margin 
for (i in 1:length(newseasons)){ 
  newseasons$margin <- (newseasons$win_score - newseasons$lose_score)}

#Score For Team ID 1
##Use an ifelse statement to produce the win and lose score if team id 1 was equal to the winning team id
newseasons$tid1_score <- ifelse(newseasons$tid1 == newseasons$win_tid,newseasons$win_score,
                                newseasons$lose_score)

#Score For Team ID 2
##Use an ifelse statement to produce the win and lose score if team id 2 was equal to the winning team id
newseasons$tid2_score <- ifelse(newseasons$tid2 == newseasons$win_tid,newseasons$win_score,
                                newseasons$lose_score)

#Home Team ID 
####Use an ifelse statement to produce a dataframe with all rows where the team id one or two value was equal to the home team id 
newseasons$home_team_id <- ifelse(newseasons$tid1_loc == "h",newseasons$tid1,
                                  newseasons$tid2)

#Home Team Outcome
##Using a case statement, write the conditions to define a win lose or draw and assign these values to team id one and two teams to show when they were the home team and the outcome when they were the home team 
newseasons <- newseasons %>% 
  mutate(home_team_outcome = case_when(newseasons$tid1_loc =="h" & newseasons$win_tid == newseasons$tid1 ~ 1,
                                       newseasons$tid2_loc =="h" & newseasons$win_tid == newseasons$tid2 ~ 1,
                                       newseasons$tid1_loc =="h" & newseasons$tid1_score == newseasons$tid2_score~ 0,
                                       
                                       newseasons$tid2_loc =="h" & newseasons$tid2_score == newseasons$tid1_score~ 0,
                                       newseasons$tid1_loc =="h" & newseasons$tid1_score < newseasons$tid2_score~ -1,
                                       newseasons$tid2_loc =="h" & newseasons$tid2_score < newseasons$tid1_score~ -1))


#Crowd Size 
##Use a case when statement to assign a new column to the dataframe where it shows the crowd size defined by small, medium, large and enormous based on the number of people that attended 
newseasons <- newseasons %>% 
  mutate(crowd_size = case_when(newseasons$att < 24000 ~ "Small",
                                24000<=newseasons$att & newseasons$att<33000 ~ "Medium",                        33000<=newseasons$att & newseasons$att<42000 ~ "Large",
                                42000 <= newseasons$att ~ "Enormous"
  ))

#Write NewSeasons Data To CSV 
write.csv(newseasons,"D:\\AssignmentOne\\Data\\NewSeasons.csv")

########################################################################################

#New Match Statistics Data Frame 
##Create a the new match statistics data frame where the match id is equal to the match id to ensure all unique results are shown, the total number of kicks is the summation of all the kicks over all the kicks per player from the data and as is for total kicks, handballs and inside 50s. Then once the columns have been defined, use a for loop to generate results for all possible matches and bind this by row into a new match summary stats data set
match_stats <- data.frame(read.csv("match_stats.csv"))

match_summary_stats <- data.frame()

for(mid in unique(match_stats$mid)) {
  match_id <- match_stats[match_stats$mid == mid,]
  summary_stats <- data.frame(mid = mid, 
                              total_Kicks = sum(match_id$Kicks), 
                              total_Marks = sum(match_id$Marks),
                              total_Handballs = sum(match_id$Handballs),
                              total_Inside50s = sum(match_id$Inside50s))
  match_summary_stats <- rbind(match_summary_stats, summary_stats)
}

match_summary_stats

#Write Match Summary Stats Data To CSV 
write.csv(match_summary_stats,"D:\\AssignmentOne\\Data\\Match_Summary_Stats.csv")
