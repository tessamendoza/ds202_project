library(tidyverse)
library(ggplot2)
library(ipumsr)
library(ggcorrplot)
# sample script for final project

time <-read_csv("time.csv")

# 
# #### - What factors affect individual's work and travel time? (Jay)
# 1) How does an individual's work time correspond to their income? travel time?
# 2) Do parents with children at home travel more or less than individuals with no children at home? 

#DATa  XPLORATION
##  
##   (has_child$ACT_)
##   (has_child$ACT_RELIG)
##    (has_child$ACT_TRAVEL)
##   (has_child$ACT_WORK)


has_child <- time %>% filter(HH_CHILD == 1)
no_child <- time %>% filter(HH_CHILD ==0)

# a little bit of data exploration
summary(has_child$ACT_WORK)
range(has_child$ACT_WORK)
quantile(has_child$ACT_WORK)
IQR(has_child$ACT_WORK)
sd(has_child$ACT_WORK)
mean(has_child$ACT_WORK)


time %>% select(HH_CHILD, ACT_WORK) %>% 
  ggplot(aes(x=HH_CHILD, y=ACT_WORK)) + geom_boxplot()

str(time$HH_CHILD)
range(time$HH_CHILD)


time2 <- time %>% mutate(HH_CHILD_factor = as.factor(HH_CHILD))
time2 %>% group_by(HH_CHILD_factor) %>% summarise(n=n(),
                                                  mean_act_work = mean(ACT_WORK),
                                                  mean_act_social = mean(ACT_SOCIAL),
                                                  mean_act_relig = mean(ACT_RELIG),
                                                  mean_act_travel = mean(ACT_TRAVEL))


# change the 0/1 option for kids / no kids into a useful factor
time2 <- time %>% mutate(kids = factor(ifelse(HH_CHILD==1,'children','no children')))

#some quick means for the groups, just to see
time2 %>% group_by(kids) %>% summarise(n=n(),
                                                  mean_act_work = mean(ACT_WORK),
                                                  mean_act_social = mean(ACT_SOCIAL),
                                                  mean_act_relig = mean(ACT_RELIG),
                                                  mean_act_travel = mean(ACT_TRAVEL))

time2 %>%  
  filter(ACT_SOCIAL>0) %>% 
  ggplot(aes(kids,ACT_SOCIAL)) + geom_boxplot() +
  ggtitle(label="boxplot of act_social", subtitle="where social>0") + 
  ylab("social time. minutes per ___?") + 
  xlab("Family status")


time2 %>% 
  filter(ACT_RELIG > 0) %>% 
  ggplot(aes(kids,ACT_RELIG)) + geom_boxplot() + 
  ggtitle(label="boxplot of act_relig", subtitle="where relig>0") + 
  ylab("relig time. minutes per ___?") + 
  xlab("Family status")

time2 %>%  filter(ACT_TRAVEL>0) %>%
  ggplot(aes(kids,ACT_TRAVEL)) + geom_boxplot() +
  ggtitle(label="boxplot of act_travel", subtitle="where travel>0") + 
  ylab("travel time. minutes per ___?") + 
  xlab("Family status")

# this is a very ugly plot! too many data points!!!
time3 %>%  ggplot(aes(x=ACT_WORK, y=ACT_TRAVEL, color = kids)) + geom_point()



time2 %>%  ggplot(aes(x=ACT_WORK, y=ACT_TRAVEL, color = kids)) + 
  geom_smooth(method=lm) + 
  ggtitle(label="Time work by time travel", subtitle="work includes 0") +
  xlab("time spent working. minutes per ___?")

time2 %>%  ggplot(aes(x=ACT_WORK, y=ACT_SOCIAL, color = kids)) + 
  geom_smooth(method=lm) + 
  ggtitle(label="time work by time social", subtitle="work includes 0") +
  xlab("Time spent working.  minutes per  ___?")


# the variable time three only includes the data where the respondent
# indicated that they work
time3 <- time2 %>% filter(ACT_WORK>0)
time3 %>%  
  ggplot(aes(x=ACT_WORK, y=ACT_TRAVEL, color = kids)) + 
  geom_smooth(method=lm) +
  ggtitle(label="work by travel", subtitle="where work >0")


time3 %>% 
  ggplot(aes(x=ACT_WORK, y=ACT_SOCIAL, color=kids)) + 
  geom_smooth(method=lm) + 
  ggtitle(label="work by social", subtitle="where work > 0")

range(time3$ACT_SOCIAL)           


time2 %>% 
  filter(kids=="no children") %>% 
  select(ACT_WORK, ACT_SOCIAL, ACT_TRAVEL, ACT_RELIG) %>% 
  cor() %>% 
  ggcorrplot(method="square", lab=TRUE) + 
  ggtitle("correlation matrix - No Children")


# a correlation matrix of our time data, with kids, for work =0
time2 %>% 
  filter(kids=="children") %>% 
  select(ACT_WORK, ACT_SOCIAL, ACT_TRAVEL, ACT_RELIG) %>% 
  cor() %>% 
  ggcorrplot(method="square", lab=TRUE) + 
  ggtitle("correlation matrix - No Children")





#################################


# more data exploration:

# how many people have 0 minutes of work?
# how many people have more than 8 hours  (480 minutes)

# here we distribute the countinuus data of the visibility into different bins 
library(dplyr) 
library(ggplot2)
library(ggpubr)
theme_set(theme_pubclean())



#library(ggthemes)

breaks <- c(0,240,480,Inf)
labels <-c("0-3.99 hours", "4-8 hours", "8+ hours")
time2$work_time_bins <- cut(time2$ACT_WORK, breaks = breaks, labels = labels, right=FALSE)

time2 %>% 
  group_by(kids, work_time_bins) %>% 
  summarize(n = n(), mean_work_minutes = mean(ACT_WORK))

time2 %>% 
  group_by(kids, work_time_bins) %>% 
  summarize(n = n(), mean_work_minutes = mean(ACT_WORK)) %>%
  ggplot(aes(x=work_time_bins, y=mean_work_minutes)) + 
  geom_linerange(aes(x=work_time_bins, ymin=0, ymax=mean_work_minutes, group=kids), 
                 color="lightgray", 
                 size=1.5, 
                 position=position_dodge(0.3), 
                 size=1.5) + 
  geom_point(aes(color=kids), position=position_dodge(0.3), size=3) + 
  theme_pubclean() + 
  ggtitle("Average minutes worked by daily hours and child status") + 
  ylab("Mean minutes worked per day") + xlab("Reported daily hours worked")






## try a boxplot

time2 %>%
  ggplot(aes(x=work_time_bins, y=ACT_WORK)) + geom_boxplot(aes(color=kids)) + ggtitle("Boxplot of minutes worked daily by child status") + xlab('Reported Daily Hours') + ylab('Minutes worked')





## traveling stuff
# a little bit of data exploration
summary(time2$ACT_TRAVEL)
range(time2$ACT_TRAVEL)
quantile(time2$ACT_TRAVEL)
IQR(time2$ACT_TRAVEL)
sd(time2$ACT_TRAVEL)
mean(time2$ACT_TRAVEL)

breaks <- c(0,120,240,360,480,Inf)
labels <-c("0-1.99 hours", "2-3.99 hours", "4-5.99 hours", "6-7.99 hours","8+")
time2$travel_time_bins <- cut(time2$ACT_TRAVEL, breaks = breaks, labels = labels, right=FALSE)

time2  %>%
  ggplot(aes(x=travel_time_bins, y=ACT_TRAVEL)) + geom_boxplot(aes(color=kids)) + ggtitle(label="Boxplot of minutes traveled daily by child status", subtitle="subtitle") + xlab('Binned times') + ylab('Minutes travelling')

## because there are so many data points greater than full-time traveling, it's hard to compre the lows and the highs. 

time2 %>% 
  group_by(kids, travel_time_bins) %>% 
  summarize(n = n(), mean_travel_minutes = mean(ACT_TRAVEL))


time2 %>% 
  group_by(kids, travel_time_bins) %>% 
  summarize(n = n(), mean_travel_minutes = mean(ACT_TRAVEL)) %>%
  ggplot(aes(x=travel_time_bins, y=mean_travel_minutes)) + 
  geom_linerange(aes(x=travel_time_bins, ymin=0, ymax=mean_travel_minutes, group=kids), 
                 color="lightgray", 
                 position=position_dodge(0.3), 
                 size=1.5) + 
  geom_point(aes(color=kids), position=position_dodge(0.3), size=3) + 
  theme_pubclean() + 
  ggtitle("Average minutes traveld by daily hours and child status") + 
  ylab("Mean minutes traveling per day") + xlab("Reported daily hours traveling")




####################################################
# RELIGION



summary(time2$ACT_RELIG)
range(time2$ACT_RELIG)
quantile(time2$ACT_RELIG)
IQR(time2$ACT_RELIG)
sd(time2$ACT_RELIG)
mean(time2$ACT_RELIG)


breaks <- c(0,30,60,90,Inf)
labels <-c("0-29 minutes", "30-59 minutes", "60-89 minutes", "90+ minutes")
time2$relig_time_bins <- cut(time2$ACT_RELIG, breaks = breaks, labels = labels, right=FALSE)

time2  %>%
  ggplot(aes(x=relig_time_bins, y=ACT_RELIG)) + geom_boxplot(aes(color=kids)) + ggtitle(label="Boxplot of minutes religious daily by child status", subtitle="subtitle") + xlab('Binned times') + ylab('Minutes religious')


time2 %>% 
  group_by(kids, relig_time_bins) %>% 
  summarize(n = n(), mean_relig_minutes = mean(ACT_RELIG))


time2 %>% 
  group_by(kids, relig_time_bins) %>% 
  summarize(n = n(), mean_relig_minutes = mean(ACT_RELIG)) %>%
  ggplot(aes(x=relig_time_bins, y=mean_relig_minutes)) + 
  geom_linerange(aes(x=relig_time_bins, ymin=0, ymax=mean_relig_minutes, group=kids), 
                 color="lightgray", 
                 position=position_dodge(0.3), 
                 size=1.5) + 
  geom_point(aes(color=kids), position=position_dodge(0.3), size=3) + 
  theme_pubclean() + 
  ggtitle("Average minutes religious by daily hours and child status") + 
  ylab("Mean minutes religious per day") + xlab("Reported daily hours religious")



## and finally, ACT_SOCIAL with KIDS



summary(time2$ACT_SOCIAL)
range(time2$ACT_SOCIAL)
quantile(time2$ACT_SOCIAL)
IQR(time2$ACT_SOCIAL)
sd(time2$ACT_SOCIAL)
mean(time2$ACT_SOCIAL)


breaks <- c(0,60,120,180,240,Inf)
labels <-c("0-59 minutes", "60-119 minutes", "120-179 minutes", "180-239 minutes"," 240+")
time2$social_time_bins <- cut(time2$ACT_SOCIAL, breaks = breaks, labels = labels, right=FALSE)

time2  %>%
  ggplot(aes(x=social_time_bins, y=ACT_SOCIAL)) + geom_boxplot(aes(color=kids))

time2 %>% 
  group_by(kids, social_time_bins) %>% 
  summarize(n = n(), mean_social_minutes = mean(ACT_SOCIAL))

time2 %>% 
  group_by(kids, social_time_bins) %>% 
  summarize(n = n(), mean_social_minutes = mean(ACT_SOCIAL)) %>%
  ggplot(aes(x=social_time_bins, y=mean_social_minutes)) + 
  geom_linerange(aes(x=social_time_bins, ymin=0, ymax=mean_social_minutes, group=kids), 
                 color="lightgray", 
                 position=position_dodge(0.3), 
                 size=1.5) + 
  geom_point(aes(color=kids), position=position_dodge(0.3), size=3) + 
  theme_pubclean()

range(time2$YEAR)
