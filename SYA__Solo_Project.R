# --------------------------------------------------------------------------------#
#                                    Solo Project                                 #
#                                                                                 #
# Title: Transmission of Covid-19 in Different Counties of Ohio by Demographics   #
#                                                                                 #
# Student Name: Sheikh Yasir Arafat                                               #
#                                                                                 #
# STA 504: Advanced Data Visualization                                            #
#---------------------------------------------------------------------------------#


#-----------------------------------Set Working Directory-------------------------#

setwd("E:/Miami/Spring_22/STA 504/Solo Project")

#---------------------------------------------------------------------------------#


#-----------------------------------Read the data set-----------------------------#
dat<-read.csv("COVID.csv")

#---------------------------------------------------------------------------------#

#-----------------------------Load the required Library---------------------------#
library(tidyverse, warn.conflicts = FALSE)
library(scales)
library(lubridate)
library(ggthemes)
library(gridExtra)
library(ggrepel)
library(forcats)
library(scales)
library(dplyr)
library(timetk)
options(dplyr.summarise.inform = FALSE)

#---------------------------------------------------------------------------------#


#=================================================================================#
#                                     Graph1                                      #
#=================================================================================#

######### -------------------- Data Manipulation------------------------- #########

# Creating a new date formatted variable 
# Then sum the cases according to that date
# Remove the missing cases

dat_u<-dat %>%
  mutate(Case.Date = as.Date(Onset.Date,"%m/%d/%Y")) %>% 
  group_by(Case.Date) %>% 
  summarise(case=sum(Case.Count)) %>% 
  drop_na(Case.Date)

######### --------------------------------------------------------------- #########

######### ----------------------- Graph Creation------------------------- #########

# Creating bar graph using geom_bar

ggplot(aes(x=Case.Date,y=case),data =dat_u) +
  geom_bar(stat="identity", width=1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1),
        plot.caption= element_text(size = 11))+
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%d,%Y",)+
  scale_y_continuous(limits = c(0, 35000), 
                     breaks = seq(0,35000,5000),labels = scales::comma)+
  labs(x=" ",y="Number of New Cases\n",
       caption = "*Source: Department of Health, Ohio")+
  ggtitle("Number of New COVID-19 Cases in Ohio Since January 2, 2020 (as of March 5, 2022)",
          subtitle ="By Starting Date of Illness")+
  theme_hc()

######### --------------------------------------------------------------- #########
######### --------------------------------------------------------------- #########

 
#=================================================================================#
#                                     Graph 2                                     #
#=================================================================================#

######### -------------------- Data Manipulation------------------------- #########

# Group the data by County
# Sum the Cases by County
# Org anise the data in ascending order
# Select the top 10 counties

dat_u3<-dat %>%
  group_by(County) %>% 
  summarise(Case=sum(Case.Count)) %>% 
  arrange(desc(Case)) %>%
  slice(1:10)
######### --------------------------------------------------------------- #########


######### ----------------------- Graph Creation------------------------- #########

# Creating bar graph using geom_bar

ggplot(aes(x=County,y=as.numeric(Case)),data =dat_u3) +
  geom_bar(stat="identity", width=0.5) +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_blank() ,
    panel.grid.major.x = element_line( size=.1, color="gray" ))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1),
        plot.caption= element_text(size = 11))+
  labs(x="\nCounty Name",y="Total Number of Cases\n",
       caption = "*Source: Department of Health, Ohio")+
  ggtitle("Total Number of COVID-19 Cases in Top 10 Affected Counties of Ohio ",
          subtitle = "Since January 2, 2020 (as of March 5, 2022).\n")+
  scale_y_continuous(labels = scales::label_number_si(),
                     breaks = seq(0,300000,50000))+
  coord_flip()

######### --------------------------------------------------------------- #########
######### --------------------------------------------------------------- #########


#=================================================================================#
#                                     Graph 3                                     #
#=================================================================================#

######### -------------------- Data Manipulation------------------------- #########

# Select the top three counties only
# Creating a new age variable by reorganizing the age category
# Group the data set by selected county and created Age category
#  Sum the number of cases accordingly

dat_u55<-dat %>%
  filter(County==c("Franklin","Hamilton","Cuyahoga")) %>% 
  rename(N.Death='Death.Due.To.Illness.Count...County.Of.Residence') %>%
  mutate(Age.cat=ordered(
    ifelse((Age.Range=="0-19" | Age.Range=="20-29"),"0-29 Years",
                                ifelse((Age.Range=="30-39" | Age.Range=="40-49" |Age.Range=="50-59"),"30-59 Years",
                                       ifelse((Age.Range=="60-69" | Age.Range=="70-79"|Age.Range=="80+"),"60+ Years",
                                              as.character(Age.Range)))),
                         levels=c("60+ Years","30-59 Years","0-29 Years"))) %>% 
  group_by(County,Age.cat) %>% 
  summarise(Case=sum(Case.Count))
######### --------------------------------------------------------------- #########


######### ----------------------- Graph Creation------------------------- #########

# Creating bar graph using geom_bar

ggplot()+       
  geom_bar(aes(x=County,y=Case,fill=Age.cat), 
           stat="identity",position="fill",
           data=dat_u55)+
  labs(title= "Spreadness of Covid-19 within Top 3 Affected Counties of Ohio by Age Group",
       x = "\nName of County",y="Relative Frequency\n",
       caption = "*Source: Department of Health, Ohio",
       fill="Age Groups")+
  theme(plot.title = element_text(hjust = 0.5,size = 13,vjust=2),
        panel.background = element_blank())+
  scale_fill_manual(values=c('green','red','blue'))

######### --------------------------------------------------------------- #########
######### --------------------------------------------------------------- #########


#=================================================================================#
#                                     Graph 4                                     #
#=================================================================================#

######### -------------------- Data Manipulation------------------------- #########

# Select the top three counties only
# Select the Sex
# Group the data by selected County and Sex
# Calculate the percentage
# Based on that percentage make a label variable

dat_u33<-dat %>%
  filter(County==c("Franklin","Hamilton","Cuyahoga"),Sex==c("Male","Female")) %>% 
  group_by(County,Sex) %>% 
  summarise(Case=sum(Case.Count)) %>% 
  mutate(per1=Case/ sum(Case)) %>% 
  arrange(per1) %>% 
  mutate(labels1 = scales::percent(per1))
######### --------------------------------------------------------------- #########


######### ----------------------- Graph Creation------------------------- #########

# Creating pie chart

ggplot(dat_u33, aes(x = "", y = per1, fill = Sex)) +
  geom_col(color = "black") +
  geom_text(aes(label = labels1),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y",start=0)+
  scale_fill_manual(values=c("green","red"))+
  facet_grid(. ~ County)+
  theme_void()+
  ggtitle("\nSpreadness of Covid-19 within Top 3 Affected Counties of Ohio by Sex")+
  theme(plot.title = element_text(hjust = 0.5,size = 13,vjust=12),legend.position = "bottom")+
  labs(caption = "*Source: Department of Health, Ohio")

######### --------------------------------------------------------------- #########
######### --------------------------------------------------------------- #########


#=================================================================================#
#                                     Graph 5                                     #
#=================================================================================#


######### -------------------- Data Manipulation------------------------- #########

# Create the date variable
# Select the desired county
# Group the data set by county
# Sum the hospitalized cases by date

dat_u3007<-dat %>%
  mutate(Hos.Date = as.Date(Admission.Date,"%m/%d/%Y")) %>%
  filter(County==c("Franklin","Hamilton","Cuyahoga")) %>% 
  group_by(County) %>% 
  summarise_by_time(.date_var  = Hos.Date,.by= "month",
                    Hos.sum= sum(Hospitalized.Count)) %>% 
  drop_na()
######### --------------------------------------------------------------- #########

######### ----------------------- Graph Creation------------------------- #########

# Creating line graphs

ggplot(dat_u3007, aes(x = Hos.Date, y = Hos.sum)) + 
  geom_line(aes(color = County, linetype = County),lwd=1.1)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(),axis.text.x = element_text(angle = 45,hjust=1),
        plot.caption= element_text(size = 11))+
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%d,%Y",)+
  scale_y_continuous(limits = c(0, 550), breaks = seq(0,550,100))+
  labs(x="Hospital Admission Date ",y="Number of Hospital Admission\n",caption = "*Source: Department of Health, Ohio")+
  ggtitle("Weekly Trend of COVID-19 Patients Hospital Admission in Most 3 Afftected Couties",
          subtitle ="Since March 1, 2020 (as of March 1, 2022)")+
  scale_color_manual(values=c('green','red','blue'))+
  theme_hc()

######### --------------------------------------------------------------- #########
######### --------------------------------------------------------------- #########




#==================================================================================#
#==================================================================================#
#                                       END                                        #
#==================================================================================#
#==================================================================================#