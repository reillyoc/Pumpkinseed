# Pumpkin Seed Diet Analysis & IRI

#Author(s): Timothy Fernandes & Reilly O'Connor
#Version: 2023-11-02

#Pkgs
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(lubridate)
library(reshape2)

#load data
diet <- read.csv("../Pumpkinseeds/Data/DietMatrix.csv", header = T)

##### Code #####
#Filter out sample periods greater than 2 and pumpkinseed with a size greater or equal to 60 mm
diet.t <- diet %>% filter(Sample > 2) %>% filter(TLEN >= 60) %>% filter(! Fullness == "MT")

#Filter out Macrophytes as they are already included in other
diet.t <- diet.t %>% select(-c(Vegetation_num, Vegetation_mass))

#Ensure sample and pond are set as factors
diet.t$Sample <- as.factor(diet.t$Sample)
diet.t$Pond <- as.factor(diet.t$Pond)

#Calculate sample sizes!
sample_size <- diet.t %>% group_by(Pond, Sample, Sex) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(sum_count = sum(count))

write.csv(sample_size, "sample_size.csv")

#Calculate total wet biomass for each diet item among individuals
diet.df <- with(diet.t, aggregate(Stickle_mass, list(Pond=Pond, Sample=Sample), sum))
diet.df$FatH <- with(diet.t, aggregate(FatH_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$RBD <- with(diet.t, aggregate(RBD_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$PU <- with(diet.t, aggregate(PU_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$DFish <- with(diet.t, aggregate(Fish_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Bivalv <- with(diet.t, aggregate(BivalvTot_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Gastro <- with(diet.t, aggregate(GastroTot_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Zoop <- with(diet.t, aggregate(PelZoopTot_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Clit <- with(diet.t, aggregate(ClitellaTot_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Ephem <- with(diet.t, aggregate(EphemTot_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Trich <- with(diet.t, aggregate(Trichoptera_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Dip <- with(diet.t, aggregate(DipteraTot_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Odo <- with(diet.t, aggregate(OdonataTot_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Hemi <- with(diet.t, aggregate(HemipteraTot_mass, list(Pond=Pond, Sample=Sample), sum))[,3]
diet.df$Other <- with(diet.t, aggregate(Other_mass, list(Pond=Pond, Sample=Sample), sum))[,3]

#Rename stickleback column
colnames(diet.df)[3] <- c("Stickle")

#Remove pond with only two sampling periods
diet.df.t <- diet.df %>% filter(! Pond == "GBP")

#Calculate total mass of of diet for each sampling period
diet.df.t <- diet.df.t %>% group_by(Pond, Sample) %>% 
  mutate(Total = Stickle + FatH + RBD + PU + DFish + Bivalv + Gastro + Zoop + Clit + Ephem + Trich + Dip + Odo + Hemi + Other)


#Calculate Percent Mass
diet.df.all <- diet.df.t %>% group_by(Pond, Sample) %>%
  summarize(StickleP = Stickle/Total*100,
            FatHP = FatH/Total*100,
            RBDP = RBD/Total*100,
            PUP = PU/Total*100,
            DFishP = DFish/Total*100,
            BivalvP = Bivalv/Total*100,
            GastroP = Gastro/Total*100,
            ZoopP = Zoop/Total*100,
            ClitP = Clit/Total*100,
            EphemP = Ephem/Total*100,
            TrichP = Trich/Total*100,
            DipP = Dip/Total*100,
            OdoP = Odo/Total*100,
            HemiP = Hemi/Total*100,
            OtherP = Other/Total*100)

#Transpose diet matrix into dataframe
diet.df.l <- melt(diet.df.all, id.vars = c("Pond", "Sample"))
diet.df.l <- diet.df.l %>% rename(diet_item = variable, mass_p = value)

###### Final Diet Plots ######
#Set colors for diet items
myPal <- c("#9E0142", "#BE2449", "#DA464C", "#EC6145", "#F7834D", "#FCAA5F","#FDC877", "#FEE391", "#CAE99D","#A6DBA4", "#7ECBA4", "#59B4AA","#3B92B8", "#4470B1", "#5E4FA2")


gsp.p <- ggplot(data=diet.df.l[diet.df.l$Pond == "GSP",], aes(x=Sample, y=mass_p, fill=diet_item)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) + 
  scale_x_discrete(limits=c( "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14", "15")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), 
        axis.text.y = element_text(color="black", size = 15), 
        axis.title = element_blank()) 
gsp.p

pp.p <- ggplot(data=diet.df.l[diet.df.l$Pond == "PP",], aes(x=Sample, y=mass_p, fill=diet_item)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) +  
  scale_x_discrete(limits=c( "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14", "15")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  xlab("Sample Period") + 
  ylab("Proportion of Stomach Content Mass (%)") + 
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), 
        axis.text.y = element_text(color="black", size = 15), 
        axis.title = element_blank()) 
pp.p

kss.p <- ggplot(data=diet.df.l[diet.df.l$Pond == "KSS",], aes(x=Sample, y=mass_p, fill=diet_item)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) + 
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  scale_x_discrete(limits=c( "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14", "15")) +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), 
        axis.text.y = element_text(color="black", size = 15), 
        axis.title = element_blank())
kss.p


ksb.p <- ggplot(data=diet.df.l[diet.df.l$Pond == "KSB",], aes(x=Sample, y=mass_p, fill=diet_item)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) + 
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  scale_x_discrete(limits=c( "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14", "15")) +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), 
        axis.text.y = element_text(color="black", size = 15), 
        axis.title = element_blank()) 
ksb.p


gg_pond_diet_mass <- ggarrange(gsp.p, pp.p,
                               kss.p, ksb.p,
                               ncol = 2, nrow = 2)#,
                               #labels = c("A", "B", "C", "D"))

gg_pond_diet_mass


