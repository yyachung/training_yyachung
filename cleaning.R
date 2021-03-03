library(rvest)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)

#Getting the species code table set up-----------------------------
webpage <- read_html("https://www.pwrc.usgs.gov/bbl/manual/speclist.cfm")

tbls <- html_nodes(webpage, "table") %>% 
  html_table(fill = TRUE)

species <- tbls[[1]] %>% 
  clean_names() %>% 
  select(alpha_code, common_name) %>% 
  mutate(alpha_code = tolower(alpha_code))

#Read in data--------------------------
pred <- read_csv("training_yyachung/Utqiagvik_predator_surveys.csv")
nest <- read_csv("training_yyachung/Utqiagvik_nest_data.csv")

#Function for transforming species codes-------------------
name_join <- function(data.df, species.df) {
  named.df<-left_join(data.df, species.df, by=c("species"="alpha_code"))
  return(named.df)
}

#Add common names using function--------------------
new.pred<-name_join(pred,species)
new.nest<-name_join(nest,species)

#Calculate total number of predators by year and species, and plot the result----
sum.pred<-
  new.pred %>%
  group_by(year,common_name) %>%
  summarise(total_pred=sum(count))

ggplot(data=sum.pred,aes(x=year,y=total_pred,col=common_name))+
  geom_jitter()

#Calculate total number of eggs predated by year and species----------
sum.nest<-
new.nest %>%
  group_by(year,common_name) %>%
  summarise(eggs_pred=sum(number_eggs_predated))

#Join the two data frames together again-----------------
#All predators by year
total.pred<-new.pred %>%
  group_by(year) %>%
  summarise(all_pred=sum(count,na.rm = TRUE))

combo.dat<-left_join(sum.nest,total.pred, by=c("year"))

ggplot(data=combo.dat,aes(x=all_pred,y=eggs_pred,col=common_name))+
  geom_point()
