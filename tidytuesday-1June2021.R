
cast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')

library(tidyverse)
survivor<-cast %>% 
  filter(result =="Sole Survivor")

cols<-c("state","personality_type")
survivor[cols]<- lapply(survivor[cols], factor)

survivor_summ<- survivor %>% 
  mutate(agegroup = case_when(age >= 21  & age <= 30 ~ 'Between 21 and 30 years',
                              age >= 31  & age <= 40 ~ 'Between 31 and 40 years',
                              age >= 41  & age <= 50 ~ 'Between 41 and 50 years',
                              age>=51 ~ '51 years or more')) %>%
  group_by(agegroup, personality_type) %>% 
  summarise(n=n()) %>% 
  ungroup()

library(showtext)
font_add_google(name = "Roboto", family = "roboto")
showtext_auto()
clist<-c("#C6DDC1","#E2C547", "#04BFBF","#812F33")

survivor_summ$agegroup<-factor(survivor_summ$agegroup,
                                       levels = c("Between 21 and 30 years",
                                                  "Between 31 and 40 years",
                                                  "Between 41 and 50 years",
                                                  "51 years or more"))

plot1june<- ggplot(data = survivor_summ, mapping = aes(x = personality_type,
                                                       y = agegroup,
                                                       fill = factor(n),
                                                       width=.7, height=.4))+
  geom_tile()

plot1june<-plot1june+ scale_fill_manual(
                      name = "Number of winner", 
                      values = clist)

plot1june<- plot1june+labs(x="Personality type", y="Age group", 
                   title="Profile of the winner", 
                   subtitle="Combination of personality type and age agroup",
                   caption = "Submission for TidyTuesday-1 June 2021. Visualization: @ShaurabhAnand")

plot1june <- plot1june + theme(plot.background=element_rect(fill = "black", colour = "black"))
plot1june <- plot1june + theme(panel.background=element_rect(fill = "black", colour = "black"))
plot1june <- plot1june + theme(plot.title=element_text(size = 44, family = "roboto",colour = "white", face = "bold", hjust = 0))
plot1june <- plot1june + theme(plot.subtitle=element_text(size = 36, family = "mono", colour = "white", hjust = 0))
plot1june <- plot1june + theme(plot.caption=element_text(size = 24, family = "mono", colour = "white", hjust = 0))
plot1june <- plot1june + theme(axis.text.x=element_text(size = 28, colour = "white"))
plot1june <- plot1june + theme(axis.text.y=element_text(size = 28, colour = "white"))
plot1june <- plot1june + theme(axis.title.x=element_text(size = 32, colour = "white")) 
plot1june <- plot1june + theme(axis.title.y=element_text(size=32, colour = "white"))
plot1june <- plot1june + theme(axis.ticks=element_blank())
plot1june <- plot1june + theme(panel.grid.major.y=element_line(colour = "gray30", linetype = "dashed"))
plot1june <- plot1june + theme(panel.grid.major.x=element_line(colour = "gray30", linetype = "dashed"))
plot1june <- plot1june + theme(strip.background = element_rect(fill = "grey93"))
plot1june <- plot1june+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot1june <- plot1june+theme (
            legend.background = element_rect(fill = "black", color = NA),
            legend.position="top",
            legend.title = element_text(color = "white", size = 32),
            legend.text = element_text(color = "white", size = 28))

ggsave("plot1june.png", plot = plot1june, width = 8, height = 6, units = "in")
