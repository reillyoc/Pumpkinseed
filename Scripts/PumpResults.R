#Pumpkinseed Diet

#Author(s): Timothy Fernandes
#Version: 2023-10-12

#Pkgs
library(RColorBrewer)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(FSA)
library(rcompanion)
library(car)
library(stats)
library(scales)
library(gridExtra)
library(cowplot)
library(caret)
library(purrr)
library(tidyr)
library(emmeans)
library(rstatix)
library(nlme)
library(piecewiseSEM)

#load data
df_temp <- read.csv("../Pumpkinseeds/Data/SEB_Temps.csv", header = T)

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



#Now pump data

pump <- read.csv("../Pumpkinseeds/Data/PumpData_GSI2.csv", header = T)

str(pump)

pump$Fullness <- as.factor(pump$Fullness)
pump$Pond <- as.factor(pump$Pond)
pump$Sex <- as.factor(pump$Sex)



pump.t <- pump %>% filter(Pond == "GSP" | Pond == "KSS" | Pond == "PP")
pump.t <- pump.t %>% filter(TLEN >= 60)
pump.t <- pump.t %>% filter(JulianDate <= 270)


head(pump.t)


#Logistic Regressions (Phenology) ----


pump2 <- read.csv("../Pumpkinseeds/Data/PumpData_GSI2.csv", header = T)

str(pump)

pump2$Fullness <- as.factor(pump2$Fullness)
pump2$Pond <- as.factor(pump2$Pond)
pump2$Sex <- as.factor(pump2$Sex)


pump.c2 <- pump2 %>% filter(Pond == "GSP" | Pond == "KSS" | Pond == "PP")
pump.c2 <- pump.c2 %>% filter(TLEN >= 60)


gsp.sam7 <- pump.c2 %>% filter(Pond == "GSP")
pp.sam7 <- pump.c2 %>% filter(Pond == "PP")
kss.sam7 <- pump.c2 %>% filter(Pond == "KSS")


#Now join


View(temp.all)

colnames(temp.all)[2] <- "JulianDate"
View(temp.all)


GSP.ta <- temp.all %>% filter(Pond == "GSP")
PP.ta <- temp.all %>% filter(Pond == "PP")
KSS.ta <- temp.all %>% filter(Pond == "KSS")



gsp.join7 <- gsp.sam7 %>% left_join(GSP.ta, by = "JulianDate")
pp.join7 <- pp.sam7 %>% left_join(PP.ta, by = "JulianDate")
kss.join7 <- kss.sam7 %>% left_join(KSS.ta, by = "JulianDate")


gsp.join7$ID = substr(gsp.join7$FishCode,8,11)
pp.join7$ID = substr(pp.join7$FishCode,7,10)
kss.join7$ID = substr(kss.join7$FishCode,8,11)


str(gsp.join7)
head(gsp.join7)


set.seed(200)

gsp.join77 <- gsp.join7 %>% group_by(Pond.x, ID) %>% slice_sample(n = 1)

View(gsp.join77)

pp.join77 <- pp.join7 %>% group_by(Pond.x, ID) %>% slice_sample(n = 1)
kss.join77 <- kss.join7 %>% group_by(Pond.x, ID) %>% slice_sample(n = 1)


#Now join back together

gut.join77 <- rbind(gsp.join77, pp.join77, kss.join77)

head(gut.join77)

gut.join77 <- as.data.frame(gut.join77)

gut.join.t7 <- gut.join77[,-c(18:73)]

head(gut.join.t7)


#Now let's try logistic regression again


gut.join.t7 <- gut.join.t7 %>% filter(JulianDate < 270)



gut.join.t7 <- gut.join.t7 %>% mutate(gonstatus = case_when(Stage >= 23 ~ "1",
                                                            Stage < 23 ~ "0",
))

gut.join.t7 <- gut.join.t7 %>% mutate(gonripe = case_when(Stage > 23 ~ "1",
                                                          Stage <= 23 ~ "0",
))

gut.join.t7 <- gut.join.t7 %>% mutate(gonspent = case_when(Stage > 30 ~ "1",
                                                          Stage <= 30 ~ "0",
))


View(gut.join.t7)


gut.join.t7$gonstatus <- as.numeric(gut.join.t7$gonstatus)
gut.join.t7$gonripe <- as.numeric(gut.join.t7$gonripe)
gut.join.t7$gonspent <- as.numeric(gut.join.t7$gonspent)



gut.join.gsp7 <- gut.join.t7 %>% filter(Pond.x == "GSP")
gut.join.pp7 <- gut.join.t7 %>% filter(Pond.x == "PP")
gut.join.kss7 <- gut.join.t7 %>% filter(Pond.x == "KSS")


#GSP

gut.join.gsp7 %>% ggplot(aes(x = Temp, y = gonstatus)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Mature Gonads"
  ) 


gut.join.gsp7 %>% ggplot(aes(x = Temp, y = gonripe)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Mature Gonads"
  ) 


gut.join.gsp7 %>% ggplot(aes(x = Temp, y = gonspent)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Spent Gonads"
  ) 


# PP

gut.join.pp7 %>% ggplot(aes(x = Temp, y = gonstatus)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Mature Gonads"
  ) 

gut.join.pp7 %>% ggplot(aes(x = Temp, y = gonripe)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Ripe Gonads"
  ) 


gut.join.pp7 %>% ggplot(aes(x = Temp, y = gonspent)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Spent Gonads"
  ) 


#KSS

gut.join.kss7 %>% ggplot(aes(x = Temp, y = gonstatus)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Mature Gonads"
  ) 

gut.join.kss7 %>% ggplot(aes(x = Temp, y = gonripe)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Mature Gonads"
  ) 

gut.join.kss7 %>% ggplot(aes(x = Temp, y = gonspent)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Spent Gonads"
  ) 


#KSS spawning at the warmest temperatures; ~50% mature at 10oC, 75% ripe at 12.5oC, 25% spent at 20OC
#PP spawning at coldest temps; 100% mature at 10oC, 100% ripe at 12.5oC, 25% spent at 20oC
#GSP spawning at similar temps to PP; 75% mature at 10oC, 98% ripe at 12.5oC, 37.5% spent at 20oC

#All ponds

gut.join.t7 %>% ggplot(aes(x = Temp, y = gonstatus)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Mature Gonads"
  ) 

gut.join.t7 %>% ggplot(aes(x = Temp, y = gonripe)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Temperature",
    y = "Probability of Ripe Gonads"
  ) 












#GFI Plots ----

pump.gfi.df <- with(pump.t[pump.t$Sex == "F",], aggregate(GFI, list(Pond=Pond, Sample=Sample), mean, na.rm = T))
pump.gfi.df$sd <- with(pump.t[pump.t$Sex == "F",], aggregate(GFI, list(Pond=Pond, Sample=Sample),
                                                             function(x) sd(x, na.rm = T)))[,3]

pump.gfi.df$Julian <- with(pump.t[pump.t$Sex == "F",], aggregate(JulianDate, list(Pond=Pond, Sample=Sample), mean))[,3]




pump.f.gfi <- ggplot(pump.gfi.df[pump.gfi.df$Julian < 180,], aes(x=as.Date(Julian, origin = as.Date("2021-01-01")), y=x, colour=Pond, group=Pond)) + 
  geom_line(aes(linetype=Pond), size=.6) +
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) + 
  geom_point(size=2.8) + 
  geom_errorbar(aes(ymax=x+sd, ymin=x-sd), width=.1) + xlab("Month") + ylab("Gut Fullness Index") +
  labs(fill = "Pond") + scale_linetype_manual(values=c("solid", "solid","solid")) +
  scale_color_manual(values=c('black','navy','cyan4')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

pump.f.gfi



pump.f.gfi <- ggplot(pump.gfi.df[pump.gfi.df$Julian < 170,], aes(x=as.Date(Julian, origin = as.Date("2021-01-01")), y=x, colour=Pond, group=Pond)) + 
  geom_line(aes(linetype=Pond), size=.6) +
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) + 
  geom_point(size=2.8) + 
  geom_errorbar(aes(ymax=x+sd, ymin=x-sd), width=.1) + xlab("Month") + ylab("Gut Fullness Index") +
  labs(fill = "Pond") + scale_linetype_manual(values=c("solid", "solid","solid")) +
  scale_color_manual(values=c('black','navy','cyan4')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

pump.f.gfi


#Let's plot male, female, and all body sizes

#MesInd Plots ----


pump.mes.df <- with(pump.t[pump.t$Sex == "F",], aggregate(MesInd, list(Pond=Pond, Sample=Sample), mean, na.rm = T))
pump.mes.df$sd <- with(pump.t[pump.t$Sex == "F",], aggregate(MesInd, list(Pond=Pond, Sample=Sample),
                                                      function(x) sd(x, na.rm = T)))[,3]

pump.mes.df$Julian <- with(pump.t[pump.t$Sex == "F",], aggregate(JulianDate, list(Pond=Pond, Sample=Sample), mean))[,3]




pump.f.mes <- ggplot(pump.mes.df[pump.mes.df$Julian < 166,], aes(x=as.Date(Julian, origin = as.Date("2021-01-01")), y=x, colour=Pond, group=Pond)) + 
  geom_line(aes(linetype=Pond), size=.6) +
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) + 
  geom_point(size=2.8) + 
  geom_errorbar(aes(ymax=x+sd, ymin=x-sd), width=.1) + xlab("Month") + ylab("Mesenteric Lipid") +
  labs(fill = "Pond") + scale_linetype_manual(values=c("solid", "solid","solid")) +
  scale_color_manual(values=c('black','navy','cyan4')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

pump.f.mes


#MesInd Analysis ----

pump.f <- pump.t %>% filter(Sex == "F")
pump.m <- pump.t %>% filter(Sex =="M")

hist(log(pump.f$MesInd))

#Not going to produce relevant hypothesis test -- simply compare peak MesInd



#GFI Plots ----


pump.gfi.df <- with(pump.t[pump.t$Sex == "F",], aggregate(GFI, list(Pond=Pond, Sample=Sample), mean, na.rm = T))
pump.gfi.df$sd <- with(pump.t[pump.t$Sex == "F",], aggregate(GFI, list(Pond=Pond, Sample=Sample),
                                                             function(x) sd(x, na.rm = T)))[,3]

pump.gfi.df$Julian <- with(pump.t[pump.t$Sex == "F",], aggregate(JulianDate, list(Pond=Pond, Sample=Sample), mean))[,3]




pump.f.gfi <- ggplot(pump.gfi.df[pump.gfi.df$Julian < 180,], aes(x=as.Date(Julian, origin = as.Date("2021-01-01")), y=x, colour=Pond, group=Pond)) + 
  geom_line(aes(linetype=Pond), size=.6) +
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) + 
  geom_point(size=2.8) + 
  geom_errorbar(aes(ymax=x+sd, ymin=x-sd), width=.1) + xlab("Month") + ylab("Gut Fullness Index") +
  labs(fill = "Pond") + scale_linetype_manual(values=c("solid", "solid","solid")) +
  scale_color_manual(values=c('black','navy','cyan4')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

pump.f.gfi




#Males

pump.gfi.m.df <- with(pump.t[pump.t$Sex == "M",], aggregate(GFI, list(Pond=Pond, Sample=Sample), mean, na.rm = T))
pump.gfi.m.df$sd <- with(pump.t[pump.t$Sex == "M",], aggregate(GFI, list(Pond=Pond, Sample=Sample),
                                                             function(x) sd(x, na.rm = T)))[,3]

pump.gfi.m.df$Julian <- with(pump.t[pump.t$Sex == "M",], aggregate(JulianDate, list(Pond=Pond, Sample=Sample), mean))[,3]




pump.m.gfi <- ggplot(pump.gfi.m.df[pump.gfi.df$Julian < 180,], aes(x=as.Date(Julian, origin = as.Date("2021-01-01")), y=x, colour=Pond, group=Pond)) + 
  geom_line(aes(linetype=Pond), size=.6) +
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) + 
  geom_point(size=2.8) + 
  geom_errorbar(aes(ymax=x+sd, ymin=x-sd), width=.1) + xlab("Month") + ylab("Gut Fullness Index") +
  labs(fill = "Pond") + scale_linetype_manual(values=c("solid", "solid","solid")) +
  scale_color_manual(values=c('black','navy','cyan4')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

pump.m.gfi



#Diet Plotting and Analysis ----


#Import


diet.all <- read.csv("../Pumpkinseeds/Data/GypsyMoth_DietData.csv", header = T)



#Now remove rows with NA's anywhere else


View(diet.all)

diet.complete <- diet.all[complete.cases(diet.all),]


View(diet.complete)



#Convert date into usable format, then extract month

diet.complete$newdate <- strptime(as.character(diet.complete$Date), "%d/%m/%Y")
diet.complete <- diet.complete %>% mutate(Month = format(newdate, "%m"))



View(diet.complete)
str(diet.complete)

diet.complete$newdate
diet.complete$Month


diet.complete$Month <- as.factor(diet.complete$Month)
diet.complete$Season <- as.factor(diet.complete$Season)
diet.complete$Pond <- as.factor(diet.complete$Pond)


str(diet.complete)


#Remove MT stomachs


diet.complete <- diet.complete %>% filter(!Fullness == "MT")


str(diet.complete)

View(diet.complete)


#Now trim mature


diet.mat <- diet.complete %>% filter(TLEN >=60) 



#Trim unnecesarry columns


diet.mat.tr <- diet.mat[,-c(1,3,4,6,7,8,9,10,41,42,45,46,47,48)]

diet.mat.tr <- diet.mat.tr[,-c(35:40)]
diet.mat.tr <- diet.mat.tr[,c(1,35,3:34)]



#Now calculate row totals for Numeric and Mass 



str(diet.mat.tr)


diet.mat.tr <- diet.mat.tr %>% rowwise() %>% mutate(NumTot = sum(c(Orthop_num,Dermap_num,TerrColeo_num,
                                                                 Hymen_num,Lepid_num,BivalvTot_num,GastroTot_num,
                                                                 PelZoopTot_num,ClitellaTot_num,EphemTot_num,
                                                                 TrichopteraTot_num,DipteraTot_num,OdonataTot_num,
                                                                 HemipteraTot_num,Other_num,FishTot_num)))


diet.mat.tr <- diet.mat.tr %>% rowwise() %>% mutate(MassTot = sum(c(Orthop_mass,Dermap_mass,TerrColeo_mass,
                                                                  Hymen_mass,Lepid_mass,BivalvTot_mass,GastroTot_mass,
                                                                  PelZoopTot_mass,ClitellaTot_mass,EphemTot_mass,
                                                                  Trichoptera_mass,DipteraTot_mass,OdonataTot_mass,
                                                                  HemipteraTot_mass,Other_mass,FishTot_mass)))


View(diet.mat.tr)



diet.mat.tr <- diet.mat.tr %>% rowwise() %>% mutate(FreqTot = sum(Orthop_num>0,Dermap_num>0,TerrColeo_num>0,
                                                                Hymen_num>0, Lepid_num>0, BivalvTot_num>0, GastroTot_num>0,
                                                                PelZoopTot_num>0, ClitellaTot_num>0, EphemTot_num>0,
                                                                TrichopteraTot_num>0, DipteraTot_num>0, OdonataTot_num>0,
                                                                HemipteraTot_num>0, Other_num>0, FishTot_num>0))



View(diet.sm.tr)



#Create new column of Freq per taxa

diet.mat.tr <- diet.mat.tr %>% rowwise() %>% mutate(Orthop_freq = sum(Orthop_num>0)) %>% mutate(Dermap_freq = sum(Dermap_num>0)) %>%
  mutate(TerrColeo_freq = sum(TerrColeo_num>0)) %>% mutate(Hymen_freq = sum(Hymen_num>0)) %>%
  mutate(Lepid_freq = sum(Lepid_num>0)) %>% mutate(Bivalv_freq = sum(BivalvTot_num>0)) %>%
  mutate(Gastro_freq = sum(GastroTot_num>0)) %>% mutate(Zoop_freq = sum(PelZoopTot_num>0)) %>%
  mutate(Clitella_freq = sum(ClitellaTot_num>0)) %>% mutate(Ephem_freq = sum(EphemTot_num>0)) %>%
  mutate(Trichop_freq = sum(TrichopteraTot_num>0)) %>% mutate(Diptera_freq = sum(DipteraTot_num>0)) %>%
  mutate(Odonata_freq = sum(OdonataTot_num>0)) %>% mutate(Hemiptera_freq = sum(HemipteraTot_num>0)) %>%
  mutate(Other_freq = sum(Other_num>0)) %>% mutate(Fish_freq = sum(FishTot_num>0))



#Check freq columns to make sure correct

diet.mat.freqcheck <- diet.mat.tr[,-c(3:36)]

View(diet.mat.freqcheck)



#Good to go
#Check to make sure FreqTot same as sum of all freq columns


diet.mat.check <- diet.mat.tr %>% rowwise() %>% mutate(TotFreqCheck = sum(Orthop_freq, Dermap_freq, TerrColeo_freq, Hymen_freq, Lepid_freq, Bivalv_freq,
                                                                        Gastro_freq, Zoop_freq, Clitella_freq, Ephem_freq,Trichop_freq,Diptera_freq,Odonata_freq,
                                                                        Hemiptera_freq, Other_freq, Fish_freq))

diet.mat.check <- diet.mat.check[,-c(3:36,38:50)]

View(diet.mat.check)


#FreqTot and _freq columns agree


#Now that we have three columns summarizing total numeric, mass, and frequency,
#summarize by pond (across months) and calculate IRI per pond, per taxa



all.mat.summ <- diet.mat.tr %>% group_by(Pond) %>% 
  summarise_at(c("Orthop_num","Orthop_mass","Dermap_num","Dermap_mass","TerrColeo_num","TerrColeo_mass","Hymen_num","Hymen_mass","Lepid_num",
                 "Lepid_mass","BivalvTot_num","BivalvTot_mass","GastroTot_num","GastroTot_mass","PelZoopTot_num","PelZoopTot_mass","ClitellaTot_num",
                 "ClitellaTot_mass","EphemTot_num","EphemTot_mass","TrichopteraTot_num","Trichoptera_mass","DipteraTot_num","DipteraTot_mass","OdonataTot_num",
                 "OdonataTot_mass","HemipteraTot_num","HemipteraTot_mass","Other_num","Other_mass","FishTot_num","FishTot_mass","NumTot","MassTot","FreqTot",
                 "Orthop_freq", "Dermap_freq", "TerrColeo_freq", "Hymen_freq", "Lepid_freq", "Bivalv_freq",
                 "Gastro_freq", "Zoop_freq", "Clitella_freq", "Ephem_freq","Trichop_freq","Diptera_freq","Odonata_freq",
                 "Hemiptera_freq", "Other_freq", "Fish_freq"), sum, na.rm = TRUE)

View(all.mat.summ)




#Calculate percent numeric, mass, and freq per taxa
#ungroup first to remove group_by() limit and allow rowwise() calculation 


all.mat.summ <- all.mat.summ %>% ungroup()
str(all.mat.summ)



all.mat.summ <- all.mat.summ %>% rowwise() %>% mutate(Orthop_numP = Orthop_num/NumTot)
View(all.mat.summ)

all.mat.summ <- all.mat.summ %>% rowwise() %>% mutate(Orthop_numP = Orthop_num/NumTot) %>% mutate(Dermap_numP = Dermap_num/NumTot) %>%
  mutate(TerrColeo_numP = TerrColeo_num/NumTot) %>% mutate(Hymen_numP = Hymen_num/NumTot) %>%
  mutate(Lepid_numP = Lepid_num/NumTot) %>% mutate(Bivalv_numP = BivalvTot_num/NumTot) %>%
  mutate(Gastro_numP = GastroTot_num/NumTot) %>% mutate(Zoop_numP = PelZoopTot_num/NumTot) %>%
  mutate(Clitella_numP = ClitellaTot_num/NumTot) %>% mutate(Ephem_numP = EphemTot_num/NumTot) %>%
  mutate(Trichop_numP = TrichopteraTot_num/NumTot) %>% mutate(Diptera_numP = DipteraTot_num/NumTot) %>%
  mutate(Odonata_numP = OdonataTot_num/NumTot) %>% mutate(Hemiptera_numP = HemipteraTot_num/NumTot) %>%
  mutate(Other_numP = Other_num/NumTot) %>% mutate(Fish_numP = FishTot_num/NumTot)




all.mat.summ <- all.mat.summ %>% rowwise() %>% mutate(Orthop_massP = Orthop_mass/MassTot) %>%
  mutate(Dermap_massP = Dermap_mass/MassTot) %>% mutate(TerrColeo_massP = TerrColeo_mass/MassTot) %>%
  mutate(Hymen_massP = Hymen_mass/MassTot) %>% mutate(Lepid_massP = Lepid_mass/MassTot) %>%
  mutate(Bivalv_massP = BivalvTot_mass/MassTot) %>% mutate(Gastro_massP = GastroTot_mass/MassTot) %>%
  mutate(Zoop_massP = PelZoopTot_mass/MassTot) %>% mutate(Clitella_massP = ClitellaTot_mass/MassTot) %>%
  mutate(Ephem_massP = EphemTot_mass/MassTot) %>% mutate(Trichop_massP = Trichoptera_mass/MassTot) %>% 
  mutate(Diptera_massP = DipteraTot_mass/MassTot) %>% mutate(Odonata_massP = OdonataTot_mass/MassTot) %>%
  mutate(Hemiptera_massP = HemipteraTot_mass/MassTot) %>% mutate(Other_massP = Other_mass/MassTot) %>% 
  mutate(Fish_massP = FishTot_mass/MassTot)





all.mat.summ <- all.mat.summ %>% rowwise() %>% mutate(Orthop_freqP = Orthop_freq/FreqTot) %>%
  mutate(Dermap_freqP = Dermap_freq/FreqTot) %>% mutate(TerrColeo_freqP = TerrColeo_freq/FreqTot) %>%
  mutate(Hymen_freqP = Hymen_freq/FreqTot) %>% mutate(Lepid_freqP = Lepid_freq/FreqTot) %>%
  mutate(Bivalv_freqP = Bivalv_freq/FreqTot) %>% mutate(Gastro_freqP = Gastro_freq/FreqTot) %>%
  mutate(Zoop_freqP = Zoop_freq/FreqTot) %>% mutate(Clitella_freqP = Clitella_freq/FreqTot) %>%
  mutate(Ephem_freqP = Ephem_freq/FreqTot) %>% mutate(Trichop_freqP = Trichop_freq/FreqTot) %>%
  mutate(Diptera_freqP = Diptera_freq/FreqTot) %>% mutate(Odonata_freqP = Odonata_freq/FreqTot) %>% 
  mutate(Hemiptera_freqP = Hemiptera_freq/FreqTot) %>% mutate(Other_freqP = Other_freq/FreqTot) %>%
  mutate(Fish_freqP = Fish_freq/FreqTot)




View(all.mat.summ)



#Trim table to include only IRI components (numP,massP,freqP)


mat.IRI <- all.mat.summ[,-c(2:33)]
View(mat.IRI)


mat.IRI <- mat.IRI[,-c(5:20)]

str(mat.IRI)


mat.IRI.tr <- mat.IRI %>% rowwise() %>% mutate(Orthop_IRI = ((Orthop_numP*100 + Orthop_massP*100)*(Orthop_freqP*100))) %>%
  mutate(Dermap_IRI = ((Dermap_numP*100 + Dermap_massP*100)*(Dermap_freqP*100))) %>%
  mutate(TerrColeo_IRI = ((TerrColeo_numP*100 + TerrColeo_massP*100)*(TerrColeo_freqP*100))) %>%
  mutate(Hymen_IRI = ((Hymen_numP*100 + Hymen_massP*100)*(Hymen_freqP*100))) %>%
  mutate(Lepid_IRI = ((Lepid_numP*100 + Lepid_massP*100)*(Lepid_freqP*100))) %>%
  mutate(Bivalv_IRI = ((Bivalv_numP*100 + Bivalv_massP*100)*(Bivalv_freqP*100))) %>%
  mutate(Gastro_IRI = ((Gastro_numP*100 + Gastro_massP*100)*(Gastro_freqP*100))) %>%
  mutate(Zoop_IRI = ((Zoop_numP*100 + Zoop_massP*100)*(Zoop_freqP*100))) %>%
  mutate(Clitella_IRI = ((Clitella_numP*100 + Clitella_massP*100)*(Clitella_freqP*100))) %>%
  mutate(Ephem_IRI = ((Ephem_numP*100 + Ephem_massP*100)*(Ephem_freqP*100))) %>%
  mutate(Trichop_IRI = ((Trichop_numP*100 + Trichop_massP*100)*(Trichop_freqP*100))) %>%
  mutate(Diptera_IRI = ((Diptera_numP*100 + Diptera_massP*100)*(Diptera_freqP*100))) %>%
  mutate(Odonata_IRI = ((Odonata_numP*100 + Odonata_massP*100)*(Odonata_freqP*100))) %>%
  mutate(Hemiptera_IRI = ((Hemiptera_numP*100 + Hemiptera_massP*100)*(Hemiptera_freqP*100))) %>%
  mutate(Other_IRI = ((Other_numP*100 + Other_massP*100)*(Other_freqP*100))) %>%
  mutate(Fish_IRI = ((Fish_numP*100 + Fish_massP*100)*(Fish_freqP*100)))




View(mat.IRI.tr)



mat.IRI.tr.1 <- mat.IRI.tr[,-c(2:49)]
View(mat.IRI.tr.1)


mat.IRI.tr.1 <- mat.IRI.tr.1[,-c(2:4)]
View(mat.IRI.tr.1)



#Changing DF from Wide to Long ----


names(mat.IRI.tr.1)

IRI.new <- mat.IRI.tr.1 %>% pivot_longer(cols=c( "Orthop_IRI", "Dermap_IRI", "TerrColeo_IRI", "Hymen_IRI", "Lepid_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI", "Fish_IRI"),
                    names_to='Taxon',
                    values_to='IRI')

View(IRI.new)


#Spearman Rank Correlation for Annual Diet ----

cor.test(IRI.new[IRI.new$Pond == "GSP",]$IRI, IRI.new[IRI.new$Pond == "PP",]$IRI, method = "spearman")
cor.test(IRI.new[IRI.new$Pond == "GSP",]$IRI, IRI.new[IRI.new$Pond == "KSS",]$IRI, method = "spearman")
cor.test(IRI.new[IRI.new$Pond == "PP",]$IRI, IRI.new[IRI.new$Pond == "KSS",]$IRI, method = "spearman")



#None of the annual diets are dissimilar between populations (all IRI rank distribution's are correlated)


#Plotting ----
#Make IRI's percentages


IRI.new.n <- IRI.new %>%                                  
  group_by(Pond) %>%
  mutate(IRI_P = IRI / sum(IRI)*100) %>% as.data.frame()

View(IRI.new.n)
  


#Now plot


p <- ggplot(IRI.new.n, aes(x = Taxon, y = IRI_P, fill = Pond))+
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

p + coord_flip()


#Try ordering by IRI

p.ord <- ggplot(IRI.new.n, aes(x = reorder(Taxon, IRI_P), y = IRI_P, fill = Pond))+
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  xlab("Prey Category") + ylab("Index of Relative Importance (%)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

p.ord + coord_flip()  + scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_fill_brewer(palette = "Blues")


#test different palettes for plot


p.ord + coord_flip()  + scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_fill_brewer(palette = "Greys")




#Seasonal Diet IRI ----
#Repeat above steps but split into seasonal groupings


diet.all <- read.csv("GypsyMoth_DietData.csv", header = T)



#Now remove rows with NA's anywhere else


View(diet.all)

diet.complete <- diet.all[complete.cases(diet.all),]


View(diet.complete)



#Convert date into usable format, then extract month

diet.complete$newdate <- strptime(as.character(diet.complete$Date), "%d/%m/%Y")
diet.complete <- diet.complete %>% mutate(Month = format(newdate, "%m"))



View(diet.complete)
str(diet.complete)

diet.complete$newdate
diet.complete$Month


diet.complete$Season <- as.factor(diet.complete$Season)
diet.complete$Pond <- as.factor(diet.complete$Pond)


str(diet.complete)


## Remove MT stomachs


diet.complete <- diet.complete %>% filter(!Fullness == "MT")


str(diet.complete)

View(diet.complete)


#Now trim mature


diet.mat <- diet.complete %>% filter(TLEN >=60) 



#Trim unnecessary columns


diet.mat.tr <- diet.mat[,-c(1,3,4,6,7,8,9,10,41,42,45,46,47,48)]





diet.mat.tr <- diet.mat.tr[,-c(35:40)]
diet.mat.tr <- diet.mat.tr[,c(1,35,3:34)]

str(diet.mat.tr)

diet.mat.tr$Month <- as.numeric(diet.mat.tr$Month)



diet.mat.pr <- diet.mat.tr %>% filter(Month < 6)



#Now calculate row totals for Numeric and Mass 



diet.mat.pr <- diet.mat.pr %>% rowwise() %>% mutate(NumTot = sum(c(Orthop_num,Dermap_num,TerrColeo_num,
                                                                   Hymen_num,Lepid_num,BivalvTot_num,GastroTot_num,
                                                                   PelZoopTot_num,ClitellaTot_num,EphemTot_num,
                                                                   TrichopteraTot_num,DipteraTot_num,OdonataTot_num,
                                                                   HemipteraTot_num,Other_num,FishTot_num)))


diet.mat.pr <- diet.mat.pr %>% rowwise() %>% mutate(MassTot = sum(c(Orthop_mass,Dermap_mass,TerrColeo_mass,
                                                                    Hymen_mass,Lepid_mass,BivalvTot_mass,GastroTot_mass,
                                                                    PelZoopTot_mass,ClitellaTot_mass,EphemTot_mass,
                                                                    Trichoptera_mass,DipteraTot_mass,OdonataTot_mass,
                                                                    HemipteraTot_mass,Other_mass,FishTot_mass)))


View(diet.mat.pr)



diet.mat.pr <- diet.mat.pr %>% rowwise() %>% mutate(FreqTot = sum(Orthop_num>0,Dermap_num>0,TerrColeo_num>0,
                                                                  Hymen_num>0, Lepid_num>0, BivalvTot_num>0, GastroTot_num>0,
                                                                  PelZoopTot_num>0, ClitellaTot_num>0, EphemTot_num>0,
                                                                  TrichopteraTot_num>0, DipteraTot_num>0, OdonataTot_num>0,
                                                                  HemipteraTot_num>0, Other_num>0, FishTot_num>0))



View(diet.mat.pr)



#Create new column of Freq per taxa

diet.mat.pr <- diet.mat.pr %>% rowwise() %>% mutate(Orthop_freq = sum(Orthop_num>0)) %>% mutate(Dermap_freq = sum(Dermap_num>0)) %>%
  mutate(TerrColeo_freq = sum(TerrColeo_num>0)) %>% mutate(Hymen_freq = sum(Hymen_num>0)) %>%
  mutate(Lepid_freq = sum(Lepid_num>0)) %>% mutate(Bivalv_freq = sum(BivalvTot_num>0)) %>%
  mutate(Gastro_freq = sum(GastroTot_num>0)) %>% mutate(Zoop_freq = sum(PelZoopTot_num>0)) %>%
  mutate(Clitella_freq = sum(ClitellaTot_num>0)) %>% mutate(Ephem_freq = sum(EphemTot_num>0)) %>%
  mutate(Trichop_freq = sum(TrichopteraTot_num>0)) %>% mutate(Diptera_freq = sum(DipteraTot_num>0)) %>%
  mutate(Odonata_freq = sum(OdonataTot_num>0)) %>% mutate(Hemiptera_freq = sum(HemipteraTot_num>0)) %>%
  mutate(Other_freq = sum(Other_num>0)) %>% mutate(Fish_freq = sum(FishTot_num>0))



#Check freq columns to make sure correct

diet.mat.freqcheck <- diet.mat.pr[,-c(3:36)]

View(diet.mat.freqcheck)



#Good to go
#Check to make sure FreqTot same as sum of all freq columns


diet.mat.check <- diet.mat.pr %>% rowwise() %>% mutate(TotFreqCheck = sum(Orthop_freq, Dermap_freq, TerrColeo_freq, Hymen_freq, Lepid_freq, Bivalv_freq,
                                                                          Gastro_freq, Zoop_freq, Clitella_freq, Ephem_freq,Trichop_freq,Diptera_freq,Odonata_freq,
                                                                          Hemiptera_freq, Other_freq, Fish_freq))

diet.mat.check <- diet.mat.check[,-c(3:36,38:50)]

View(diet.mat.check)


#FreqTot and _freq columns agree


#Now that we have three columns summarizing total numeric, mass, and frequency,
#summarize by pond (across months) and calculate IRI per pond, per taxa


View(diet.mat.pr)

all.mat.summ <- diet.mat.pr %>% group_by(Pond) %>% 
  summarise_at(c("Orthop_num","Orthop_mass","Dermap_num","Dermap_mass","TerrColeo_num","TerrColeo_mass","Hymen_num","Hymen_mass","Lepid_num",
                 "Lepid_mass","BivalvTot_num","BivalvTot_mass","GastroTot_num","GastroTot_mass","PelZoopTot_num","PelZoopTot_mass","ClitellaTot_num",
                 "ClitellaTot_mass","EphemTot_num","EphemTot_mass","TrichopteraTot_num","Trichoptera_mass","DipteraTot_num","DipteraTot_mass","OdonataTot_num",
                 "OdonataTot_mass","HemipteraTot_num","HemipteraTot_mass","Other_num","Other_mass","FishTot_num","FishTot_mass","NumTot","MassTot","FreqTot",
                 "Orthop_freq", "Dermap_freq", "TerrColeo_freq", "Hymen_freq", "Lepid_freq", "Bivalv_freq",
                 "Gastro_freq", "Zoop_freq", "Clitella_freq", "Ephem_freq","Trichop_freq","Diptera_freq","Odonata_freq",
                 "Hemiptera_freq", "Other_freq", "Fish_freq"), sum, na.rm = TRUE)

View(all.mat.summ)




#Calculate percent numeric, mass, and freq per taxa
#ungroup first to remove group_by() limit and allow rowwise() calculation 


all.mat.summ <- all.mat.summ %>% ungroup()



all.mat.summ <- all.mat.summ %>% rowwise() %>% mutate(Orthop_numP = Orthop_num/NumTot)


all.mat.summ <- all.mat.summ %>% rowwise() %>% mutate(Orthop_numP = Orthop_num/NumTot) %>% mutate(Dermap_numP = Dermap_num/NumTot) %>%
  mutate(TerrColeo_numP = TerrColeo_num/NumTot) %>% mutate(Hymen_numP = Hymen_num/NumTot) %>%
  mutate(Lepid_numP = Lepid_num/NumTot) %>% mutate(Bivalv_numP = BivalvTot_num/NumTot) %>%
  mutate(Gastro_numP = GastroTot_num/NumTot) %>% mutate(Zoop_numP = PelZoopTot_num/NumTot) %>%
  mutate(Clitella_numP = ClitellaTot_num/NumTot) %>% mutate(Ephem_numP = EphemTot_num/NumTot) %>%
  mutate(Trichop_numP = TrichopteraTot_num/NumTot) %>% mutate(Diptera_numP = DipteraTot_num/NumTot) %>%
  mutate(Odonata_numP = OdonataTot_num/NumTot) %>% mutate(Hemiptera_numP = HemipteraTot_num/NumTot) %>%
  mutate(Other_numP = Other_num/NumTot) %>% mutate(Fish_numP = FishTot_num/NumTot)




all.mat.summ <- all.mat.summ %>% rowwise() %>% mutate(Orthop_massP = Orthop_mass/MassTot) %>%
  mutate(Dermap_massP = Dermap_mass/MassTot) %>% mutate(TerrColeo_massP = TerrColeo_mass/MassTot) %>%
  mutate(Hymen_massP = Hymen_mass/MassTot) %>% mutate(Lepid_massP = Lepid_mass/MassTot) %>%
  mutate(Bivalv_massP = BivalvTot_mass/MassTot) %>% mutate(Gastro_massP = GastroTot_mass/MassTot) %>%
  mutate(Zoop_massP = PelZoopTot_mass/MassTot) %>% mutate(Clitella_massP = ClitellaTot_mass/MassTot) %>%
  mutate(Ephem_massP = EphemTot_mass/MassTot) %>% mutate(Trichop_massP = Trichoptera_mass/MassTot) %>% 
  mutate(Diptera_massP = DipteraTot_mass/MassTot) %>% mutate(Odonata_massP = OdonataTot_mass/MassTot) %>%
  mutate(Hemiptera_massP = HemipteraTot_mass/MassTot) %>% mutate(Other_massP = Other_mass/MassTot) %>% 
  mutate(Fish_massP = FishTot_mass/MassTot)





all.mat.summ <- all.mat.summ %>% rowwise() %>% mutate(Orthop_freqP = Orthop_freq/FreqTot) %>%
  mutate(Dermap_freqP = Dermap_freq/FreqTot) %>% mutate(TerrColeo_freqP = TerrColeo_freq/FreqTot) %>%
  mutate(Hymen_freqP = Hymen_freq/FreqTot) %>% mutate(Lepid_freqP = Lepid_freq/FreqTot) %>%
  mutate(Bivalv_freqP = Bivalv_freq/FreqTot) %>% mutate(Gastro_freqP = Gastro_freq/FreqTot) %>%
  mutate(Zoop_freqP = Zoop_freq/FreqTot) %>% mutate(Clitella_freqP = Clitella_freq/FreqTot) %>%
  mutate(Ephem_freqP = Ephem_freq/FreqTot) %>% mutate(Trichop_freqP = Trichop_freq/FreqTot) %>%
  mutate(Diptera_freqP = Diptera_freq/FreqTot) %>% mutate(Odonata_freqP = Odonata_freq/FreqTot) %>% 
  mutate(Hemiptera_freqP = Hemiptera_freq/FreqTot) %>% mutate(Other_freqP = Other_freq/FreqTot) %>%
  mutate(Fish_freqP = Fish_freq/FreqTot)




View(all.mat.summ)



#Trim table to include only IRI components (numP,massP,freqP)


mat.IRI <- all.mat.summ[,-c(2:33)]
View(mat.IRI)


mat.IRI <- mat.IRI[,-c(5:20)]

str(mat.IRI)


mat.IRI.tr <- mat.IRI %>% rowwise() %>% mutate(Orthop_IRI = ((Orthop_numP*100 + Orthop_massP*100)*(Orthop_freqP*100))) %>%
  mutate(Dermap_IRI = ((Dermap_numP*100 + Dermap_massP*100)*(Dermap_freqP*100))) %>%
  mutate(TerrColeo_IRI = ((TerrColeo_numP*100 + TerrColeo_massP*100)*(TerrColeo_freqP*100))) %>%
  mutate(Hymen_IRI = ((Hymen_numP*100 + Hymen_massP*100)*(Hymen_freqP*100))) %>%
  mutate(Lepid_IRI = ((Lepid_numP*100 + Lepid_massP*100)*(Lepid_freqP*100))) %>%
  mutate(Bivalv_IRI = ((Bivalv_numP*100 + Bivalv_massP*100)*(Bivalv_freqP*100))) %>%
  mutate(Gastro_IRI = ((Gastro_numP*100 + Gastro_massP*100)*(Gastro_freqP*100))) %>%
  mutate(Zoop_IRI = ((Zoop_numP*100 + Zoop_massP*100)*(Zoop_freqP*100))) %>%
  mutate(Clitella_IRI = ((Clitella_numP*100 + Clitella_massP*100)*(Clitella_freqP*100))) %>%
  mutate(Ephem_IRI = ((Ephem_numP*100 + Ephem_massP*100)*(Ephem_freqP*100))) %>%
  mutate(Trichop_IRI = ((Trichop_numP*100 + Trichop_massP*100)*(Trichop_freqP*100))) %>%
  mutate(Diptera_IRI = ((Diptera_numP*100 + Diptera_massP*100)*(Diptera_freqP*100))) %>%
  mutate(Odonata_IRI = ((Odonata_numP*100 + Odonata_massP*100)*(Odonata_freqP*100))) %>%
  mutate(Hemiptera_IRI = ((Hemiptera_numP*100 + Hemiptera_massP*100)*(Hemiptera_freqP*100))) %>%
  mutate(Other_IRI = ((Other_numP*100 + Other_massP*100)*(Other_freqP*100))) %>%
  mutate(Fish_IRI = ((Fish_numP*100 + Fish_massP*100)*(Fish_freqP*100)))




View(mat.IRI.tr)



mat.IRI.tr.1 <- mat.IRI.tr[,-c(2:49)]
View(mat.IRI.tr.1)


mat.IRI.tr.1 <- mat.IRI.tr.1[,-c(2:4)]
View(mat.IRI.tr.1)



#Changing DF from Wide to Long ----


names(mat.IRI.tr.1)

IRI.new <- mat.IRI.tr.1 %>% pivot_longer(cols=c( "Orthop_IRI", "Dermap_IRI", "TerrColeo_IRI", "Hymen_IRI", "Lepid_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI", "Fish_IRI"),
                                         names_to='Taxon',
                                         values_to='IRI')

View(IRI.new)


#Spearman Rank Correlation for Annual Diet ----

cor.test(IRI.new[IRI.new$Pond == "GSP",]$IRI, IRI.new[IRI.new$Pond == "PP",]$IRI, method = "spearman")
cor.test(IRI.new[IRI.new$Pond == "GSP",]$IRI, IRI.new[IRI.new$Pond == "KSS",]$IRI, method = "spearman")
cor.test(IRI.new[IRI.new$Pond == "PP",]$IRI, IRI.new[IRI.new$Pond == "KSS",]$IRI, method = "spearman")



#None of the annual diets are dissimilar between populations (all IRI rank distribution's are correlated)
#However, differences in the size distribution of prey items: 

#GSP Diptera

3.894/788 # 3.894 total grams of diptera over 788 total individuals = 5 mg average wet mass in GSP

0.694/178 # 4 mg average wet mass in KSS

1.74/440 # 4 mg average wet mass in PP



#Plotting ----
#Make IRI's percentages


IRI.new.n <- IRI.new %>%                                  
  group_by(Pond) %>%
  mutate(IRI_P = IRI / sum(IRI)*100) %>% as.data.frame()

View(IRI.new.n)


IRI.new.n <- IRI.new.n %>% filter(Pond == "GSP" | Pond == "PP" | Pond == "KSS")
  

#Now plot


p <- ggplot(IRI.new.n, aes(x = Taxon, y = IRI_P, fill = Pond))+
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

p + coord_flip()


#Try ordering by IRI

p.ord <- ggplot(IRI.new.n, aes(x = reorder(Taxon, IRI_P), y = IRI_P, fill = Pond))+
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  xlab("Prey Category") + ylab("Index of Relative Importance (%)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

p.ord + coord_flip()  + scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_fill_brewer(palette = "Blues")


#test different palettes for plot


p.ord + coord_flip()  + scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_fill_brewer(palette = "Greys")



#Relationship between Preparation and Allocation ----

#If we trim the timeline so only include the preparatory and allocatory windows, linear relationships between HSI and GSI should be reasonable
#Need to specify windows for each population;



pump.gsi.df <- with(pump.t[pump.t$Sex == "F",], aggregate(GSI, list(Pond=Pond, Sample=Sample), mean, na.rm = T))
pump.gsi.df$sd <- with(pump.t[pump.t$Sex == "F",], aggregate(GSI, list(Pond=Pond, Sample=Sample),
                                                             function(x) sd(x, na.rm = T)))[,3]

pump.gsi.df$Julian <- with(pump.t[pump.t$Sex == "F",], aggregate(JulianDate, list(Pond=Pond, Sample=Sample), mean))[,3]




pump.f.gsi <- ggplot(pump.gsi.df[pump.gsi.df$Julian < 180,], aes(x=as.Date(Julian, origin = as.Date("2021-01-01")), y=x, colour=Pond, group=Pond)) + 
  geom_line(aes(linetype=Pond), size=.6) +
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) + 
  geom_point(size=2.8) + 
  geom_errorbar(aes(ymax=x+sd, ymin=x-sd), width=.1) + xlab("Month") + ylab("GSI") +
  labs(fill = "Pond") + scale_linetype_manual(values=c("solid", "solid","solid")) +
  scale_color_manual(values=c('black','navy','cyan4')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

pump.f.gsi





pump.hsi.df <- with(pump.t[pump.t$Sex == "F",], aggregate(HSI, list(Pond=Pond, Sample=Sample), mean, na.rm = T))
pump.hsi.df$sd <- with(pump.t[pump.t$Sex == "F",], aggregate(HSI, list(Pond=Pond, Sample=Sample),
                                                             function(x) sd(x, na.rm = T)))[,3]

pump.hsi.df$Julian <- with(pump.t[pump.t$Sex == "F",], aggregate(JulianDate, list(Pond=Pond, Sample=Sample), mean))[,3]




pump.f.hsi <- ggplot(pump.hsi.df[pump.hsi.df$Julian < 180,], aes(x=as.Date(Julian, origin = as.Date("2021-01-01")), y=x, colour=Pond, group=Pond)) + 
  geom_line(aes(linetype=Pond), size=.6) +
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) + 
  geom_point(size=2.8) + 
  geom_errorbar(aes(ymax=x+sd, ymin=x-sd), width=.1) + xlab("Month") + ylab("HSI") +
  labs(fill = "Pond") + scale_linetype_manual(values=c("solid", "solid","solid")) +
  scale_color_manual(values=c('black','navy','cyan4')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

pump.f.hsi



#Now trim to include only allocation and preparation windows - Prep starts in April, allocation ends in June (so months >3 and <7)

pump.t$Month <- as.numeric(pump.t$Month)

pump.t.pa <- pump.t %>% filter(Month > 3 & Month < 7)


plot(log(pump.t.pa[pump.t.pa$Sex == "F" & pump.t.pa$Pond == "KSS",]$GSI) ~ pump.t.pa[pump.t.pa$Sex == "F" & pump.t.pa$Pond == "KSS",]$HSI)
plot(log(pump.t.pa[pump.t.pa$Sex == "F" & pump.t.pa$Pond == "GSP",]$GSI) ~ pump.t.pa[pump.t.pa$Sex == "F" & pump.t.pa$Pond == "GSP",]$HSI)
plot(log(pump.t.pa[pump.t.pa$Sex == "F" & pump.t.pa$Pond == "PP",]$GSI) ~ pump.t.pa[pump.t.pa$Sex == "F" & pump.t.pa$Pond == "PP",]$HSI)



#No relationship -- Flat line driven by the delay (asynchrony)




#Analysis of Maximum Prep and Alloc ----
#ANOVA ----


View(pump.t)
str(pump.t)


gsp.max <- pump.t %>% filter(Sex == "F" & Sample == "10" & Pond == "GSP")


View(gsp.max)


#n = 13


kss.max <- pump.t %>% filter(Sex == "F" & Sample == "10" & Pond == "KSS")


View(kss.max)


#n = 8


pp.max <- pump.t %>% filter(Sex == "F" & Sample == "11" & Pond == "PP")


View(pp.max)


#n = 13


#Now rbind the subsetted dataframes

all.max <- rbind(gsp.max, kss.max, pp.max)

View(all.max)

hist(log(all.max.t$GSI))

all.max$GSI


# Remove spent outlier

all.max.t <- all.max %>% filter(Stage < 40)

hist(log(all.max.t$GSI))
shapiro.test(log(all.max.t$GSI))
shapiro.test(all.max.t$GSI)


#W = 0.97, p = 0.508


all.max.t$Pond <- as.factor(all.max.t$Pond)

#Now we have a normal and relevant GSI comparison; run aov


aov.max.g.l <- aov(log(GSI) ~ Pond, data = all.max.t)
aov.max.g <- aov(GSI ~ Pond, data = all.max.t)


summary(aov.max.g)
summary(aov.max.g.l)


TukeyHSD(aov.max.g)
TukeyHSD(aov.max.g.l)


#KSS > GSP (p = 0.0094; log-p = 0.021), KSS >> PP (p = 0.0002; log-p = 0.001)



#Find magnitudes of difference


gsi.s <- all.max.t %>% group_by(Pond) %>% summarize(gonad = mean(GSI))
gsi.s$sd <- all.max.t %>% group_by(Pond) %>% summarize(sd = sd(GSI))


View(gsi.s)



#GSP = 12.21 +- 6.33; KSS = 19.64 +- 6.01; PP = 8.70 +- 2.69


#Now HSI, combine two highest periods of HSI

pump.t$Sample <- as.numeric(pump.t$Sample)

gsp.max.h <- pump.t %>% filter(Sex == "F" & Sample > 7 & Sample < 10 & Pond == "GSP")


View(gsp.max.h)


#n = 17


kss.max.h <- pump.t %>% filter(Sex == "F" & Sample > 8 & Sample < 11 & Pond == "KSS")


View(kss.max.h)


#n = 12


pp.max.h <- pump.t %>% filter(Sex == "F" & Sample > 7 & Sample < 10 & Pond == "PP")


View(pp.max.h)


#n = 16


#Now combine as above


all.max.h <- rbind(gsp.max.h, kss.max.h, pp.max.h)

View(all.max.h)

hist(all.max.h$HSI)


shapiro.test(all.max.h$HSI)



#W = 0.981, p = 0.666


all.max.h$Pond <- as.factor(all.max.h$Pond)



#Now we run aov


aov.max.h <- aov(HSI ~ Pond, data = all.max.h)


summary(aov.max.h)


TukeyHSD(aov.max.h)


#GSP > PP (p = 0.0269), all else equal

#Find magnitudes of difference


hsi.s <- all.max.h %>% group_by(Pond) %>% summarize(liver = mean(HSI))
hsi.s$sd <- all.max.h %>% group_by(Pond) %>% summarize(sd = sd(HSI))


View(hsi.s)


#GSP = 3.74 +- 0.64; KSS = 3.48 +- 0.75; PP = 3.04 +- 0.84



#ANCOVA



aoc.h <- aov(Liver ~ RWT*Pond, data = all.max.h)

summary(aoc.h)

plot(all.max.t$Liver ~ all.max.t$RWT)

ggscatter(
  all.max.t, x = "RWT", y = "Liver",
  color = "Pond", add = "reg.line"
)


str(all.max.h)




liv.pht <- all.max.h %>% 
  emmeans_test(
    Liver ~ Pond, covariate = RWT,
    p.adjust.method = "bonferroni"
  )
liv.pht


all.max.h$RWT <- as.integer(all.max.h$RWT)
all.max.h$Liver <- as.integer(all.max.h$Liver)


#Still not working


emm.s <- emmeans(aoc.h, "Pond")

pairs(emm.s)


#Same result as with HSI aov <- GSP > PP (df = 39, p = 0.013)



#Let's Try SEM ----


#Filter for only females and for prespawning
View(gut.join.t7)

gut.join.t7.f <- gut.join.t7 %>% filter(Sex == "F")


boxplot(gut.join.t7.f$Temp ~ gut.join.t7.f$Sample)


hist(log(sem.df$GFI))
hist(sem.df$Temp)
hist(log(sem.df$MesInd))


head(gut.join.t7.f)
str(gut.join.t7.f)

View(gut.join.t7.f)

sem.df <- gut.join.t7.f %>% select(c(FishCode, Pond.x, RWT, Liver, HSI, GFI, MesInd, Temp))

sem.df.t <- na.omit(sem.df)

View(sem.df.t)



pump_sem <- psem(
  lme(log(GFI) ~ Temp, random = ~1|Pond.x, data = sem.df.t, method = "ML"),
  lme(log(MesInd) ~ Temp, random = ~1|Pond.x, data = sem.df.t, method = "ML"),
  lme(HSI ~ log(MesInd) + log(GFI) + Temp, random = ~1|Pond.x, data = sem.df.t, method = "ML"),
  data = sem.df.t
)


summary(pump_sem, .progressBar = FALSE)


#Try trimming out the post-spawning windows

plot(sem.df$HSI ~ sem.df$Temp)
plot(sem.df$GFI ~ sem.df$Temp)
plot(sem.df$MesInd ~ sem.df$Temp)


#15oC seems to be relevant cutoff point between preparation and spawning -> all samples before 10 are below 15 oC


gut.join.t7.f$Sample <- as.numeric(gut.join.t7.f$Sample)

sem.df.n <- gut.join.t7.f %>% filter(Sample < 10)

sem.df.n.t <- sem.df.n %>% select(c(FishCode, Pond.x, RWT, Liver, HSI, GFI, MesInd, Temp))

sem.df.n.t <- na.omit(sem.df.n.t)



pump_sem2 <- psem(
  lme(log(GFI) ~ Temp, random = ~1|Pond.x, data = sem.df.n.t, method = "ML"),
  lme(log(MesInd) ~ Temp, random = ~1|Pond.x, data = sem.df.n.t, method = "ML"),
  lme(HSI ~ log(MesInd) + log(GFI) + Temp, random = ~1|Pond.x, data = sem.df.n.t, method = "ML"),
  data = sem.df.n.t
)


summary(pump_sem2, .progressBar = FALSE)


plot(sem.df.n.t$HSI ~ log(sem.df.n.t$GFI))
plot(sem.df.n.t$HSI ~ log(sem.df.n.t$MesInd))


#Converges and give interesting output

#What if we remove the temp ~ HSI?

pump_sem.r <- psem(
  lme(log(GFI) ~ Temp, random = ~1|Pond.x, data = sem.df.n.t, method = "ML"),
  lme(log(MesInd) ~ Temp, random = ~1|Pond.x, data = sem.df.n.t, method = "ML"),
  lme(HSI ~ log(MesInd) + log(GFI), random = ~1|Pond.x, data = sem.df.n.t, method = "ML"),
  data = sem.df.n.t
)


summary(pump_sem.r, .progressBar = FALSE)


#Need the temp HSI, otherwise model breaks down
#Let's try a different configuration



pump_sem3 <- psem(
  lme(log(GFI) ~ Temp, random = ~1|Pond.x, data = sem.df.n.t, method = "ML"),
  lme(log(MesInd) ~ log(GFI), random = ~1|Pond.x, data = sem.df.n.t, method = "ML"),
  lme(HSI ~ log(MesInd) + log(GFI), random = ~1|Pond.x, data = sem.df.n.t, method = "ML"),
  data = sem.df.n.t
)


summary(pump_sem3, .progressBar = FALSE)


#Functionally does not converge
