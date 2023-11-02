

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
library(GGally)
library(AICcmodavg)
library(usdm)
library(piecewiseSEM)
library(multcomp)
library(reshape2)
library(viridis)
library(RColorBrewer)


# Pumpkinseed Diet Summary ----


setwd("E:/Surface_2023_Working/UofT_Surface/Projects/Ongoing")

getwd()


diet <- read.csv("PumpkinseedDiet/DietMatrix.csv", header = T)

str(diet)


diet.t <- diet %>% filter(Sample > 2) %>% filter(TLEN >= 60)


diet.t$Sample <- as.factor(diet.t$Sample)
diet.t$Pond <- as.factor(diet.t$Pond)

str(diet.t)
head(diet.t)

diet.df <- with(diet.t, aggregate(Stickle_mass, list(Pond=Pond, Sample=Sample), sum))

diet.df$FatH <- with(diet.t, aggregate(FatH_mass, list(Pond=Pond, Sample=Sample), sum))[,3]

diet.df$RBD <- with(diet.t, aggregate(RBD_mass, list(Pond=Pond, Sample=Sample), sum))[,3]

diet.df$PU <- with(diet.t, aggregate(PU_mass, list(Pond=Pond, Sample=Sample), 
                                         sum))[,3]

diet.df$DFish <- with(diet.t, aggregate(Fish_mass, list(Pond=Pond, Sample=Sample), sum))[,3]

diet.df$Veg <- with(diet.t, aggregate(Vegetation_mass, list(Pond=Pond, Sample=Sample), sum))[,3]

diet.df$Bivalv <- with(diet.t, aggregate(BivalvTot_mass, list(Pond=Pond, Sample=Sample), sum))[,3]

diet.df$Gastro <- with(diet.t, aggregate(GastroTot_mass, list(Pond=Pond,
                                                              Sample=Sample),
                                         sum))[,3]

diet.df$Zoop <- with(diet.t, aggregate(PelZoopTot_mass, list(Pond=Pond, Sample=Sample), 
                                      sum))[,3]

diet.df$Clit <- with(diet.t, aggregate(ClitellaTot_mass, list(Pond=Pond, Sample=Sample), 
                                      sum))[,3]

diet.df$Ephem <- with(diet.t, aggregate(EphemTot_mass, list(Pond=Pond, Sample=Sample), 
                                       sum))[,3]

diet.df$Trich <- with(diet.t, aggregate(Trichoptera_mass, list(Pond=Pond, Sample=Sample), 
                                        sum))[,3]

diet.df$Dip <- with(diet.t, aggregate(DipteraTot_mass, list(Pond=Pond, Sample=Sample), 
                                       sum))[,3]

diet.df$Odo <- with(diet.t, aggregate(OdonataTot_mass, list(Pond=Pond, Sample=Sample), 
                                      sum))[,3]

diet.df$Hemi <- with(diet.t, aggregate(HemipteraTot_mass, list(Pond=Pond, Sample=Sample), 
                                      sum))[,3]

diet.df$Other <- with(diet.t, aggregate(Other_mass, list(Pond=Pond, Sample=Sample), 
                                      sum))[,3]



View(diet.df)


colnames(diet.df)[3] <- c("Stickle")


str(diet.df)


# Trim out GBP

diet.df.t <- diet.df %>% filter(Pond == "PP" | Pond == "GSP" | Pond == "KSS"
                                | Pond == "KSB")


diet.df.t <- diet.df.t %>% group_by(Pond, Sample) %>% mutate(Total = Stickle + FatH + RBD + PU + DFish + Veg + Bivalv + Gastro + Zoop + Clit + Ephem + Trich + Dip + Odo + Hemi + Other)


View(diet.df.t)



# Now calculate percent mass


diet.df.all <- diet.df.t %>% mutate(VegP = Veg/Total*100) %>% mutate(StickleP = Stickle/Total*100) %>% 
  mutate(FatHP = FatH/Total*100) %>%
  mutate(RBDP = RBD/Total*100) %>%
  mutate(PUP = PU/Total*100) %>%
  mutate(DFishP = DFish/Total*100) %>%
  mutate(VegP = Veg/Total*100) %>%
  mutate(BivalvP = Bivalv/Total*100) %>%
  mutate(GastroP = Gastro/Total*100) %>%
  mutate(ZoopP = Zoop/Total*100) %>%
  mutate(ClitP = Clit/Total*100) %>%
  mutate(EphemP = Ephem/Total*100) %>%
  mutate(TrichP = Trich/Total*100) %>%
  mutate(DipP = Dip/Total*100) %>%
  mutate(OdoP = Odo/Total*100) %>%
  mutate(HemiP = Hemi/Total*100) %>%
  mutate(OtherP = Other/Total*100)

View(diet.df.all)

str(diet.df.all)

# Make long instead of wide for plotting 


diet.df.tt <- diet.df.all[,-c(3:19)]

str(diet.df.tt)


diet.df.l <- melt(diet.df.tt, id.vars = c("Pond", "Sample"))


str(diet.df.l)
View(diet.df.l)



ggplot(data=diet.df.l[diet.df.l$Pond == "GSP",], aes(x=Sample, y=value, 
                                                         fill=variable)) + 
  geom_bar(stat="identity")

ggplot(data=diet.df.l[diet.df.l$Pond == "KSS",], aes(x=Sample, y=value, 
                                                         fill=variable)) + 
  geom_bar(stat="identity")

ggplot(data=diet.df.l[diet.df.l$Pond == "PP",], aes(x=Sample, y=value, 
                                                        fill=variable)) + 
  geom_bar(stat="identity")


# Convert Sample into logical scale (Sample - 2)


# Final Diet Plots ----


nb.col <- 18
myPal <- colorRampPalette(brewer.pal(11, "Spectral"))(nb.col)

myPal

gsp.p <- ggplot(data=diet.df.l[diet.df.l$Pond == "GSP",], aes(x=Sample, y=value, 
                                                     fill=variable)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) + 
  scale_x_discrete(limits=c( "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14", "15")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  xlab("Sample Period") + 
  ylab("Proportion of Stomach Content Mass (%)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), axis.text.y = element_text(color="black", size = 15), axis.title=element_text(size=15)) 


pp.p <- ggplot(data=diet.df.l[diet.df.l$Pond == "PP",], aes(x=Sample, y=value, 
                                                    fill=variable)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) +  
  scale_x_discrete(limits=c( "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14", "15")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  xlab("Sample Period") + 
  ylab("Proportion of Stomach Content Mass (%)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), axis.text.y = element_text(color="black", size = 15), axis.title=element_text(size=15)) 


kss.p <- ggplot(data=diet.df.l[diet.df.l$Pond == "KSS",], aes(x=Sample, y=value, 
                                                     fill=variable)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) + 
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  scale_x_discrete(limits=c( "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14", "15")) +
  xlab("Sample Period") + 
  ylab("Proportion of Stomach Content Mass (%)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), axis.text.y = element_text(color="black", size = 15), axis.title=element_text(size=15)) 


ksb.p <- ggplot(data=diet.df.l[diet.df.l$Pond == "KSB",], aes(x=Sample, y=value, 
                                                     fill=variable)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) + 
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  scale_x_discrete(limits=c( "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14", "15")) +
  xlab("Sample Period") + 
  ylab("Proportion of Stomach Content Mass (%)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), axis.text.y = element_text(color="black", size = 15), axis.title=element_text(size=15)) 



# check body size

boxplot(TLEN ~ Sample, data = diet.t[diet.t$Pond == "GSP",])
boxplot(TLEN ~ Sample, data = diet.t[diet.t$Pond == "PP",])
boxplot(TLEN ~ Sample, data = diet.t[diet.t$Pond == "KSS",])




# IRI ----



# Seasonal Diet IRI ----
# Repeat above steps but split into seasonal groupings


diet.all <- read.csv("PumpkinseedDiet/GypsyMoth_DietData.csv", header = T)



# Now remove rows with NA's anywhere else


View(diet.all)

diet.complete <- diet.all[complete.cases(diet.all),]


View(diet.complete)



## Convert date into usable format, then extract month

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


# Now trim mature


diet.mat <- diet.complete %>% filter(TLEN >=60) 



# Trim unnecessary columns


diet.mat.tr <- diet.mat[,-c(1,3,4,6,7,8,9,10,41,42,45,46,47,48)]





diet.mat.tr <- diet.mat.tr[,-c(35:40)]
diet.mat.tr <- diet.mat.tr[,c(1,35,3:34)]

str(diet.mat.tr)

diet.mat.tr$Month <- as.numeric(diet.mat.tr$Month)




#### Pre Spawn ----

diet.mat.pr <- diet.mat.tr %>% filter(Month < 6)



# Now calculate row totals for Numeric and Mass 



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



# Create new column of Freq per taxa

diet.mat.pr <- diet.mat.pr %>% rowwise() %>% mutate(Orthop_freq = sum(Orthop_num>0)) %>% mutate(Dermap_freq = sum(Dermap_num>0)) %>%
  mutate(TerrColeo_freq = sum(TerrColeo_num>0)) %>% mutate(Hymen_freq = sum(Hymen_num>0)) %>%
  mutate(Lepid_freq = sum(Lepid_num>0)) %>% mutate(Bivalv_freq = sum(BivalvTot_num>0)) %>%
  mutate(Gastro_freq = sum(GastroTot_num>0)) %>% mutate(Zoop_freq = sum(PelZoopTot_num>0)) %>%
  mutate(Clitella_freq = sum(ClitellaTot_num>0)) %>% mutate(Ephem_freq = sum(EphemTot_num>0)) %>%
  mutate(Trichop_freq = sum(TrichopteraTot_num>0)) %>% mutate(Diptera_freq = sum(DipteraTot_num>0)) %>%
  mutate(Odonata_freq = sum(OdonataTot_num>0)) %>% mutate(Hemiptera_freq = sum(HemipteraTot_num>0)) %>%
  mutate(Other_freq = sum(Other_num>0)) %>% mutate(Fish_freq = sum(FishTot_num>0))



# Check freq columns to make sure correct

diet.mat.freqcheck <- diet.mat.pr[,-c(3:36)]

View(diet.mat.freqcheck)



# Good to go
# Check to make sure FreqTot same as sum of all freq columns


diet.mat.check <- diet.mat.pr %>% rowwise() %>% mutate(TotFreqCheck = sum(Orthop_freq, Dermap_freq, TerrColeo_freq, Hymen_freq, Lepid_freq, Bivalv_freq,
                                                                          Gastro_freq, Zoop_freq, Clitella_freq, Ephem_freq,Trichop_freq,Diptera_freq,Odonata_freq,
                                                                          Hemiptera_freq, Other_freq, Fish_freq))

diet.mat.check <- diet.mat.check[,-c(3:36,38:50)]

View(diet.mat.check)


# FreqTot and _freq columns agree


# Now that we have three columns summarizing total numeric, mass, and frequency,
# summarize by pond (across months) and calculate IRI per pond, per taxa


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




# Calculate percent numeric, mass, and freq per taxa
# ungroup first to remove group_by() limit and allow rowwise() calculation 


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



# Trim table to include only IRI components (numP,massP,freqP)


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



# Changing DF from Wide to Long ----


names(mat.IRI.tr.1)

IRI.new <- mat.IRI.tr.1 %>% pivot_longer(cols=c( "Orthop_IRI", "Dermap_IRI", "TerrColeo_IRI", "Hymen_IRI", "Lepid_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI", "Fish_IRI"),
                                         names_to='Taxon',
                                         values_to='IRI')

View(IRI.new)


# Spearman Rank Correlation for Annual Diet ----

cor.test(IRI.new[IRI.new$Pond == "GSP",]$IRI, IRI.new[IRI.new$Pond == "PP",]$IRI, method = "spearman")
cor.test(IRI.new[IRI.new$Pond == "GSP",]$IRI, IRI.new[IRI.new$Pond == "KSS",]$IRI, method = "spearman")
cor.test(IRI.new[IRI.new$Pond == "PP",]$IRI, IRI.new[IRI.new$Pond == "KSS",]$IRI, method = "spearman")



# None of the annual diets are dissimilar between populations (all IRI rank distribution's are correlated)
# However, differences in the size distribution of prey items: 

# GSP Diptera

3.894/788 # 3.894 total grams of diptera over 788 total individuals = 5 mg average wet mass in GSP

0.694/178 # 4 mg average wet mass in KSS

1.74/440 # 4 mg average wet mass in PP



# Plotting ----
# Make IRI's percentages


IRI.new.n <- IRI.new %>%                                  
  group_by(Pond) %>%
  mutate(IRI_P = IRI / sum(IRI)*100) %>% as.data.frame()

View(IRI.new.n)


IRI.new.n <- IRI.new.n %>% filter(Pond == "GSP" | Pond == "PP" | Pond == "KSS" | 
                                    Pond == "KSB")


# Now plot


p <- ggplot(IRI.new.n, aes(x = Taxon, y = IRI_P, fill = Pond))+
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

p + coord_flip()


# Try ordering by IRI

p.ord <- ggplot(IRI.new.n, aes(x = reorder(Taxon, IRI_P), y = IRI_P, fill = Pond))+
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  xlab("Prey Category") + ylab("Index of Relative Importance (%)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

p.ord + coord_flip()  + scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_fill_brewer(palette = "Blues")


# test different palettes for plot


p.ord + coord_flip()  + scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_fill_brewer(palette = "Greys")






#### Post-spawn ----



diet.mat.pr <- diet.mat.tr %>% filter(Month >= 6)



# Now calculate row totals for Numeric and Mass 



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



# Create new column of Freq per taxa

diet.mat.pr <- diet.mat.pr %>% rowwise() %>% mutate(Orthop_freq = sum(Orthop_num>0)) %>% mutate(Dermap_freq = sum(Dermap_num>0)) %>%
  mutate(TerrColeo_freq = sum(TerrColeo_num>0)) %>% mutate(Hymen_freq = sum(Hymen_num>0)) %>%
  mutate(Lepid_freq = sum(Lepid_num>0)) %>% mutate(Bivalv_freq = sum(BivalvTot_num>0)) %>%
  mutate(Gastro_freq = sum(GastroTot_num>0)) %>% mutate(Zoop_freq = sum(PelZoopTot_num>0)) %>%
  mutate(Clitella_freq = sum(ClitellaTot_num>0)) %>% mutate(Ephem_freq = sum(EphemTot_num>0)) %>%
  mutate(Trichop_freq = sum(TrichopteraTot_num>0)) %>% mutate(Diptera_freq = sum(DipteraTot_num>0)) %>%
  mutate(Odonata_freq = sum(OdonataTot_num>0)) %>% mutate(Hemiptera_freq = sum(HemipteraTot_num>0)) %>%
  mutate(Other_freq = sum(Other_num>0)) %>% mutate(Fish_freq = sum(FishTot_num>0))



# Check freq columns to make sure correct

diet.mat.freqcheck <- diet.mat.pr[,-c(3:36)]

View(diet.mat.freqcheck)



# Good to go
# Check to make sure FreqTot same as sum of all freq columns


diet.mat.check <- diet.mat.pr %>% rowwise() %>% mutate(TotFreqCheck = sum(Orthop_freq, Dermap_freq, TerrColeo_freq, Hymen_freq, Lepid_freq, Bivalv_freq,
                                                                          Gastro_freq, Zoop_freq, Clitella_freq, Ephem_freq,Trichop_freq,Diptera_freq,Odonata_freq,
                                                                          Hemiptera_freq, Other_freq, Fish_freq))

diet.mat.check <- diet.mat.check[,-c(3:36,38:50)]

View(diet.mat.check)


# FreqTot and _freq columns agree


# Now that we have three columns summarizing total numeric, mass, and frequency,
# summarize by pond (across months) and calculate IRI per pond, per taxa


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




# Calculate percent numeric, mass, and freq per taxa
# ungroup first to remove group_by() limit and allow rowwise() calculation 


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



# Trim table to include only IRI components (numP,massP,freqP)


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



# Changing DF from Wide to Long ----


names(mat.IRI.tr.1)

IRI.new <- mat.IRI.tr.1 %>% pivot_longer(cols=c( "Orthop_IRI", "Dermap_IRI", "TerrColeo_IRI", "Hymen_IRI", "Lepid_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI", "Fish_IRI"),
                                         names_to='Taxon',
                                         values_to='IRI')

View(IRI.new)


# Spearman Rank Correlation for Annual Diet ----

cor.test(IRI.new[IRI.new$Pond == "GSP",]$IRI, IRI.new[IRI.new$Pond == "PP",]$IRI, method = "spearman")
cor.test(IRI.new[IRI.new$Pond == "GSP",]$IRI, IRI.new[IRI.new$Pond == "KSS",]$IRI, method = "spearman")
cor.test(IRI.new[IRI.new$Pond == "PP",]$IRI, IRI.new[IRI.new$Pond == "KSS",]$IRI, method = "spearman")



# None of the annual diets are dissimilar between populations (all IRI rank distribution's are correlated)
# However, differences in the size distribution of prey items: 

# GSP Diptera

3.894/788 # 3.894 total grams of diptera over 788 total individuals = 5 mg average wet mass in GSP

0.694/178 # 4 mg average wet mass in KSS

1.74/440 # 4 mg average wet mass in PP



# Plotting ----
# Make IRI's percentages


IRI.new.n <- IRI.new %>%                                  
  group_by(Pond) %>%
  mutate(IRI_P = IRI / sum(IRI)*100) %>% as.data.frame()

View(IRI.new.n)


IRI.new.n <- IRI.new.n %>% filter(Pond == "GSP" | Pond == "PP" | Pond == "KSS" | 
                                    Pond == "KSB")


# Now plot


p <- ggplot(IRI.new.n, aes(x = Taxon, y = IRI_P, fill = Pond))+
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

p + coord_flip()


# Try ordering by IRI

p.ord <- ggplot(IRI.new.n, aes(x = reorder(Taxon, IRI_P), y = IRI_P, fill = Pond))+
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  xlab("Prey Category") + ylab("Index of Relative Importance (%)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))

p.ord + coord_flip()  + scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_fill_brewer(palette = "Blues")


# test different palettes for plot


p.ord + coord_flip()  + scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_fill_brewer(palette = "Greys")

