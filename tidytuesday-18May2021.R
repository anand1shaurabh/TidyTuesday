#Loading libraries
library(tidytuesdayR)
library(tidyverse)
library(forcats)

#Getting Data
tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey <- tuesdata$survey

#Data preparation
survey_subset<- survey[, c("annual_salary",
                           "highest_level_of_education_completed",
                           "gender",
                           "currency")]

names(survey_subset)[names(survey_subset) == "annual_salary"] <- "Salary"
names(survey_subset)[names(survey_subset) == "highest_level_of_education_completed"] <- "Education"
names(survey_subset)[names(survey_subset) == "gender"] <- "Gender"
names(survey_subset)[names(survey_subset) == "currency"] <- "currency"

survey_subset<-filter(survey_subset, currency!="Other")


survey_subset<-survey_subset %>% 
                mutate(Salary_USD = 
                       case_when(
                      currency == "AUD/NZD" ~ Salary*0.78,
                      currency == "CAD" ~ Salary*0.83,
                      currency == "CHF" ~ Salary*1.11,
                      currency == "GBP" ~ Salary*1.42,
                      currency == "HKD" ~  Salary*0.13,
                      currency == "CAD" ~ Salary*0.0092,
                      currency == "SEK" ~ Salary*0.12,
                      currency == "USD" ~ Salary*1,
                      currency == "CAD" ~ Salary*0.072))

survey_subset<-filter(survey_subset, Salary_USD>0)
survey_subset<-filter(survey_subset, Salary_USD<102000000)

survey_subset<-survey_subset %>% 
                filter(Gender!= "Other or prefer not to answer") %>% 
                filter(Gender!= "Prefer not to answer")
survey_subset<-droplevels(survey_subset)
survey_subset$Gender<-factor(survey_subset$Gender, levels = c("Man", "Woman", "Non-binary"))

survey_subset<-survey_subset %>% 
              mutate(
                Education = fct_collapse(
                Education,
                "College degree"=c("College degree","Some college"),
                "High School" = "High School",
                "Master's degree" = "Master's degree",
                "PhD" = "PhD",
                "Professional degree" = "Professional degree (MD, JD, etc.)"))

survey_subset<- survey_subset[complete.cases(survey_subset), ]

#Plotting
plot1<-ggplot(data = survey_subset, aes(x = Gender,
                                     y = Salary_USD/10^6,
                                     fill = Gender))+
  geom_boxplot()+
  facet_wrap(~ Education, scales = "free_y")+
  scale_fill_brewer (type = "qual", palette = "Set2")+
  labs(x="Gender",
       y = "Annual Salary (in USD Millions)",
       title = "Annual Salary Across Different Levels of Education",
       caption = "Submission for TidyTuesday - 18 May 2021. Data source: Ask A Manager Salary Survey")+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white"
                                    ),
    panel.grid.major = element_line(size = 0.3, linetype = 'solid',
                                    colour = "gray90"),
    axis.text.y = element_text(color="black"),
    axis.text.x = element_text(color="black"))
    
#Display plot  
plot1

              
                                   
                






