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

#########################################################################################

#Player Reporting Function 
player_report <-function(player_id, match_data, player_data) #define function name and inputs 
{
  
  if(!any(player_id == player_data$pid)) #if an incorrect player id is inputted into the function, return an error message as reads below 
  {
    stop(player_id," ","is not in the players data") #this ensures the code is stopped and not run any further and shows the user the reason why it didn't work 
  }
  player_id_unique <- match_data[match_data$player_id == player_id] #retrieve all unique player id's 
  match_data$mid<-as.character(match_data$mid) #change match id structure to character to allow it to be used below 
  player_stats <- data.frame(
    Player = player_data[which(player_data$pid == player_id),5],
    Games = sum(match_data$pid == player_id),
    FirstGame = min(parse_number(match_data$mid)[which(match_data$pid == player_id)]),
    LastGame =  max(parse_number(match_data$mid)[which(match_data$pid == player_id)]),
    average_kicks = round(mean(match_data$Kicks[which(match_data$pid == player_id)]),2),
    average_marks = round(mean(match_data$Marks[which(match_data$pid == player_id)]),2),
    average_handballs = round(mean(match_data$Handballs[which(match_data$pid == player_id)]),2),
    average_inside50s = round(mean(match_data$Inside50s[which(match_data$pid == player_id)]),2)  #define the dataframe, make column for all player id's, make column for all match id's where the player id was present, use the parse number (extracts the year from the match id string and the min/max function is used to find the first and last game the palyer was present for based on the matches they played) function to retrieve the first and last game year from the player's match history, the average (marks, average 50s, handballs and kicks) are calculated from using the mean function 
  )
  return(player_stats) #return the data frame as defined above 
}

player_report(player_id = 10001,match_data = match_stats,player_data = players) #input data and player id into formula to generate results 

#####################################################################################

#Plotting Function 
plot_prep <- function(match_data,seasons_data,team_id){
  combined_data <- inner_join(seasons_data,match_data,by="mid")
  
  plot_summary<-data.frame(
    mid=seasons_data$mid[which(seasons_data$home_team_id == team_id)],
    season = seasons_data$season[which(seasons_data$home_team_id == team_id)],
    h_id = seasons_data$home_team_id[which(seasons_data$home_team_id == team_id)],
    total_Kicks = combined_data$total_Kicks[which(combined_data$home_team_id == team_id)],
    total_Marks = combined_data$total_Marks[which(combined_data$home_team_id == team_id)],
    total_Handballs = combined_data$total_Handballs[which(combined_data$home_team_id == team_id)],
    total_Inside50s = combined_data$total_Inside50s[which(combined_data$home_team_id == team_id)]) #define the dataframe for the summary stats based on the inputs as defined above in the function. which statements make it easier to extract where the home team id was equal to the team id but then also return the total number of kicks, marks, handballs and inside 50s based on the combined data set which was in the function produced through an inner join on match id 
  
  return(plot_summary) #return the dataframe 
}


plotting_data<-plot_prep(match_data=match_summary_stats,seasons_data=newseasons,103) #use function to generate the plotting data required for the plotting function below 

#Plotting Code 

##Plot Total Handballs
plotting_data<-plot_prep(match_data=match_summary_stats,seasons_data=newseasons,team_id=103)
ggplot(data = plotting_data, mapping = aes(x = season, y = total_Handballs)) +
  geom_jitter() +
  geom_smooth(method = "gam", span = 0.3, formula = y ~ s(x, bs = "cs")) +
  xlab("Season (Year)") +
  ylab("Total Handballs") +
  ggtitle("Handballs in Each Match by Season (team ID = 103)") #code used as requested from assignment sheet 

##Plot Total Kicks
plotting_data<-plot_prep(match_data=match_summary_stats,seasons_data=newseasons,team_id=103)
ggplot(data = plotting_data, mapping = aes(x = season, y = total_Kicks)) +
  geom_jitter() +
  geom_smooth(method = "gam", span = 0.3, formula = y ~ s(x, bs = "cs")) +
  xlab("Season (Year)") +
  ylab("Total Kicks") +
  ggtitle("Kicks in Each Match by Season (team ID = 103)") #code used as requested from assignment sheet 

##Plot Total Inside 50s
plotting_data<-plot_prep(match_data=match_summary_stats,seasons_data=newseasons,team_id=103)
ggplot(data = plotting_data, mapping = aes(x = season, y = total_Inside50s)) +
  geom_jitter() +
  geom_smooth(method = "gam", span = 0.3, formula = y ~ s(x, bs = "cs")) +
  xlab("Season (Year)") +
  ylab("Total Inside50s") +
  ggtitle("Inside 50s in Each Match by Season (team ID = 103)") #code used as requested from assignment sheet 