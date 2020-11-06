library(tidyverse)
library(ggplot2)
library(ipumsr)
library(ggcorrplot)
# sample script for final project

time <-read_csv("ds202_project/time.csv")

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
  ggtitle(label="work by travel", subtitle="work >0")


time3 %>% 
  ggplot(aes(x=ACT_WORK, y=ACT_SOCIAL, color=kids)) + 
  geom_smooth(method=lm) + 
  ggtitle(label="work by social", subtitle="work > 0")

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

