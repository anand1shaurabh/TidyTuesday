# loading libraries
library("tidyverse")
library(ggalt)
# importing data
tuesdata <- tidytuesdayR::tt_load('2021-05-25')
records <- tuesdata$records

# data cleaning
records_sub<-records[,c("track", "type", "shortcut","date", "time")]
cols<-c("track", "type", "shortcut")
records_sub[cols]<- lapply(records_sub[cols], factor)

records_sub<-records_sub %>% 
  mutate(year = format(date, format = "%Y")) %>% 
  filter(shortcut=="No") %>% 
  filter(type=="Single Lap") %>% 
  group_by(track, year) %>% 
  summarise(avg_time = mean(time))

records_start<-records_sub %>% 
  slice_min(year) %>% 
  mutate(time_start = avg_time)

records_end<-records_sub %>% 
  slice_max(year) %>% 
  mutate(time_end = avg_time)

records_com<-cbind(records_start,records_end)

records_com<-records_com[, c("track...1", "time_start", "time_end")]
names(records_com)[names(records_com) == "track...1"] <- "track"

# plotting
plot1<-ggplot()

plot1<- plot1+ geom_dumbbell(
                data = records_com,
                aes(x = time_start, xend = time_end, y = track),
                colour_x = "darkred", colour_xend = "darkBlue",
                size_x = 2.5, size_xend  = 2.5)

plot1<-plot1+geom_text(data = data.frame(),
                       aes(x=88, y= "Wario Stadium", label="Latest record"),
                       color="darkBlue", hjust=1, size=3, nudge_x=-3)

plot1<-plot1+geom_text(data = data.frame(),
                       aes(x=108, y= "Wario Stadium", label="Earliest record"),
                       color="darkred", hjust=1, size=3, nudge_x=-5)

plot1<- plot1+labs(x="Average time (seconds)", y="Track", 
       title="Mario Kart World Records", 
       subtitle="Single Lap Without Shortcut - Change in average time",
       caption = "Submission for TidyTuesday-25 March 2021. Visualization: Shaurabh Anand")

plot1 <- plot1 + theme(plot.background=element_rect(fill = "grey93", colour = "grey93"))
plot1 <- plot1 + theme(plot.title=element_text(size = 12, family = "mono", face = "bold", hjust = 0))
plot1 <- plot1 + theme(axis.text.x=element_text(size = 8))
plot1 <- plot1 + theme(axis.text.y=element_text(size = 8))
plot1 <- plot1 + theme(axis.title.x=element_text(size = 9)) 
plot1 <- plot1 + theme(axis.title.y=element_text(size=9))
plot1 <- plot1 + theme(axis.ticks=element_blank())
plot1 <- plot1 + theme(panel.grid.major.y=element_blank())

ggsave("plot1.png", plot = plot1, width = 9.5, height = 5, units = "in")