# Pumpkinseed Diet

#Author(s): Timothy Fernandes
#Version: 2023-10-12

#Pkgs
library(tidyverse)
library(RColorBrewer)

#load data
df_temp <- read.csv("../Pumpkinseeds/Data/SEB_Temps.csv", header = T)

##### Code #####
View(df_temp)

str(df_temp)

df_temp$Period <- as.factor(df_temp$Period)
df_temp$Pond <- as.factor(df_temp$Pond)
df_temp$Julian <- as.numeric(df_temp$Julian)

temp.gsp.w <- df_temp %>% filter(Julian < 112)

View(temp.gsp.w)

#GSP Winter Temps ----


temp.trial <- temp.gsp.w[temp.gsp.w$Pond == "PP" | temp.gsp.w$Pond == "KSS" | temp.gsp.w$Pond == "GBP",] %>% group_by(Pond, Julian) %>% mutate(Record = 1:n())

View(temp.trial)

temp.trial$Record <- as.factor(temp.trial$Record)

str(temp.trial)

temp.gsp.win <- temp.trial %>% group_by(Julian, Record) %>% summarize(Temp = mean(Temp))

View(temp.gsp.win)


temp.gsp.win$Pond <- "GSP"


View(temp.gsp.win)


temp.t <- df_temp %>% select(-c(Serial, MeasuredDate, Period))

View(temp.t)


temp.gsp.win.t <- temp.gsp.win %>% select(-Record)


temp.all <- rbind(temp.t, temp.gsp.win.t)


View(temp.all)


#Summarize for daily temps

temp.sum <- temp.all %>% group_by(Pond, Julian) %>% summarize(DailyTemp = mean(Temp))

View(temp.sum)

temp.sum$Julian <- as.numeric(temp.sum$Julian)



ggplot(temp.sum, aes(y = DailyTemp, x = Julian, colour = Pond, shape = Pond)) + geom_point() + geom_smooth(method = "loess", se = T)


#Trim for only contemporary (Stouffville)


temp.sum <- temp.sum %>% filter(Pond == "KSS" | Pond == "GSP" | Pond == "PP")


ggplot(temp.sum, aes(y = DailyTemp, x = Julian)) + geom_point(aes(colour = Pond), alpha = 0.35) + geom_smooth(aes(colour = Pond), method = loess, linewidth = 1.5)



#Trim out later dates

temp.sum.t <- temp.sum %>% filter(Julian < 274)

View(temp.sum.t)

ggplot(temp.sum.t, aes(y = DailyTemp, x=as.Date(Julian, origin = as.Date("2021-01-01")), shape = Pond, colour = Pond)) + geom_point(alpha = 0.5) +  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) + geom_smooth(method = 'gam', linewidth = 1.5) + xlab("Month") + ylab("Water Temperature (°C)") +  scale_y_continuous(breaks = seq(0,25,5)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black")) + guides(color=guide_legend(override.aes=list(fill=NA)))












