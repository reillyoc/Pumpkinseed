# Index of Relative Importance - Pumpkinseeds

#Author(s): Timothy Fernandes & Reilly O'Connor
#Version: 2023-11-02

#Pkgs
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(pspearman)
library(ggpubr)

#load data
df_iri <- read.csv("../Pumpkinseeds/Data/GypsyMoth_DietData.csv", header = T)

##### Code #####
###### Calculating IRI #####
#Now remove rows with NA's anywhere else
diet.complete <- df_iri[complete.cases(df_iri),]

##Convert date into usable format, then extract month
diet.complete$newdate <- strptime(as.character(diet.complete$Date), "%d/%m/%Y")

diet.complete <- diet.complete %>% mutate(Month = format(newdate, "%m"))

diet.complete$Season <- as.factor(diet.complete$Season)
diet.complete$Pond <- as.factor(diet.complete$Pond)

##Remove Empty stomachs (aka - MT) and GBP
diet.complete <- diet.complete %>% filter(!Fullness == "MT")
diet.complete <- diet.complete %>% filter(!Pond == "GBP")

#Remove fish less than 60
diet.mat <- diet.complete %>% filter(TLEN >=60) 

diet.mat.tr <- diet.mat %>% select(Pond, Month, Sex,
                                   Orthop_num, Orthop_mass,
                                   Dermap_num, Dermap_mass,
                                   TerrColeo_num, TerrColeo_mass,
                                   Hymen_num, Hymen_mass,
                                   Lepid_num, Lepid_mass,
                                   BivalvTot_num, BivalvTot_mass,
                                   GastroTot_num, GastroTot_mass,
                                   PelZoopTot_num, PelZoopTot_mass,
                                   ClitellaTot_num, ClitellaTot_mass,
                                   EphemTot_num, EphemTot_mass,
                                   TrichopteraTot_num, Trichoptera_mass,
                                   DipteraTot_num, DipteraTot_mass,
                                   OdonataTot_num, OdonataTot_mass,
                                   HemipteraTot_num, HemipteraTot_mass,
                                   Other_num, Other_mass,
                                   FishTot_num, FishTot_mass)

#Ensure month is numeric
diet.mat.tr$Month <- as.numeric(diet.mat.tr$Month)

#Add 5 spp into the other column
diet.mat.tr <- diet.mat.tr %>% 
  mutate(Other_num = Orthop_num + Dermap_num + TerrColeo_num + Hymen_num + Lepid_num,
         Other_mass = Orthop_mass + Dermap_mass + TerrColeo_mass + Hymen_mass + Lepid_mass) %>%
  select(-c(Orthop_num, Orthop_mass, Dermap_num, Dermap_mass, TerrColeo_num, TerrColeo_mass, Hymen_num, Hymen_mass, Lepid_num, Lepid_mass))

##### Pre Spawn #####
#Filter data pre spawn time
diet.mat.pr <- diet.mat.tr %>% filter(Month < 6)

#Now calculate row totals for Numeric and Mass 
#Calculate Total Number of Individual Occurences
diet.mat.pr <- diet.mat.pr %>% 
  group_by(Pond, Sex) %>%
  rowwise() %>% 
  mutate(NumTot = sum(c(BivalvTot_num, GastroTot_num, PelZoopTot_num, ClitellaTot_num, EphemTot_num, TrichopteraTot_num, DipteraTot_num, OdonataTot_num, HemipteraTot_num, Other_num,FishTot_num)))

#Calculate Mass Totals
diet.mat.pr <- diet.mat.pr %>% rowwise() %>% mutate(MassTot = sum(c(BivalvTot_mass,GastroTot_mass, PelZoopTot_mass, ClitellaTot_mass, EphemTot_mass, Trichoptera_mass, DipteraTot_mass, OdonataTot_mass, HemipteraTot_mass, Other_mass, FishTot_mass)))

#Calculate frequency total
diet.mat.pr <- diet.mat.pr %>% rowwise() %>% 
  mutate(FreqTot = sum(BivalvTot_num>0, GastroTot_num>0, PelZoopTot_num>0, ClitellaTot_num>0, EphemTot_num>0, TrichopteraTot_num>0, DipteraTot_num>0, OdonataTot_num>0, HemipteraTot_num>0, Other_num>0, FishTot_num>0))


#Create new column of Freq per taxa
diet.mat.pr <- diet.mat.pr %>% rowwise() %>% 
  mutate(Bivalv_freq = sum(BivalvTot_num>0),
         Gastro_freq = sum(GastroTot_num>0),
         Zoop_freq = sum(PelZoopTot_num>0),
         Clitella_freq = sum(ClitellaTot_num>0),
         Ephem_freq = sum(EphemTot_num>0),
         Trichop_freq = sum(TrichopteraTot_num>0),
         Diptera_freq = sum(DipteraTot_num>0),
         Odonata_freq = sum(OdonataTot_num>0),
         Hemiptera_freq = sum(HemipteraTot_num>0),
         Other_freq = sum(Other_num>0),
         Fish_freq = sum(FishTot_num>0))


#Summarize by pond (across months) and calculate IRI per pond, per taxa
all.mat.summ.pr <- diet.mat.pr %>% group_by(Pond, Sex) %>% 
  summarise_at(c("BivalvTot_num","BivalvTot_mass","GastroTot_num","GastroTot_mass","PelZoopTot_num","PelZoopTot_mass","ClitellaTot_num","ClitellaTot_mass","EphemTot_num","EphemTot_mass","TrichopteraTot_num","Trichoptera_mass","DipteraTot_num","DipteraTot_mass","OdonataTot_num","OdonataTot_mass","HemipteraTot_num","HemipteraTot_mass","Other_num","Other_mass","FishTot_num","FishTot_mass","NumTot","MassTot","FreqTot","Bivalv_freq", "Gastro_freq", "Zoop_freq", "Clitella_freq", "Ephem_freq","Trichop_freq","Diptera_freq","Odonata_freq","Hemiptera_freq", "Other_freq", "Fish_freq"), sum, na.rm = TRUE)

#Ungroup first to remove group_by() limit and allow rowwise() calculation 
all.mat.summ.pr <- all.mat.summ.pr %>% ungroup()

#Calculate percent numeric, mass, and freq per taxa
#Numeric
all.mat.summ.pr <- all.mat.summ.pr %>% rowwise() %>% 
  mutate(Bivalv_numP = BivalvTot_num/NumTot,
         Gastro_numP = GastroTot_num/NumTot,
         Zoop_numP = PelZoopTot_num/NumTot,
         Clitella_numP = ClitellaTot_num/NumTot,
         Ephem_numP = EphemTot_num/NumTot,
         Trichop_numP = TrichopteraTot_num/NumTot, 
         Diptera_numP = DipteraTot_num/NumTot,
         Odonata_numP = OdonataTot_num/NumTot,
         Hemiptera_numP = HemipteraTot_num/NumTot,
         Other_numP = Other_num/NumTot, 
         Fish_numP = FishTot_num/NumTot)

#Mass
all.mat.summ.pr <- all.mat.summ.pr %>% rowwise() %>% 
  mutate(Bivalv_massP = BivalvTot_mass/MassTot,
         Gastro_massP = GastroTot_mass/MassTot,
         Zoop_massP = PelZoopTot_mass/MassTot, 
         Clitella_massP = ClitellaTot_mass/MassTot,
         Ephem_massP = EphemTot_mass/MassTot,
         Trichop_massP = Trichoptera_mass/MassTot,
         Diptera_massP = DipteraTot_mass/MassTot, 
         Odonata_massP = OdonataTot_mass/MassTot,
         Hemiptera_massP = HemipteraTot_mass/MassTot, 
         Other_massP = Other_mass/MassTot,
         Fish_massP = FishTot_mass/MassTot)

#Frequency
all.mat.summ.pr <- all.mat.summ.pr %>% rowwise() %>% 
  mutate(Bivalv_freqP = Bivalv_freq/FreqTot,
         Gastro_freqP = Gastro_freq/FreqTot,
         Zoop_freqP = Zoop_freq/FreqTot, 
         Clitella_freqP = Clitella_freq/FreqTot,
         Ephem_freqP = Ephem_freq/FreqTot, 
         Trichop_freqP = Trichop_freq/FreqTot,
         Diptera_freqP = Diptera_freq/FreqTot, 
         Odonata_freqP = Odonata_freq/FreqTot, 
         Hemiptera_freqP = Hemiptera_freq/FreqTot,
         Other_freqP = Other_freq/FreqTot,
         Fish_freqP = Fish_freq/FreqTot)

#Calculate IRI for each diet item
mat.IRI.pr.tr <- all.mat.summ.pr %>% group_by(Pond, Sex) %>%
  rowwise() %>% 
  summarize(Bivalv_IRI = ((Bivalv_numP + Bivalv_massP)*(Bivalv_freqP)),
            Gastro_IRI = ((Gastro_numP + Gastro_massP)*(Gastro_freqP)),
            Zoop_IRI = ((Zoop_numP + Zoop_massP)*(Zoop_freqP)),
            Clitella_IRI = ((Clitella_numP + Clitella_massP)*(Clitella_freqP)),
            Ephem_IRI = ((Ephem_numP + Ephem_massP)*(Ephem_freqP)),
            Trichop_IRI = ((Trichop_numP + Trichop_massP)*(Trichop_freqP)),
            Diptera_IRI = ((Diptera_numP + Diptera_massP)*(Diptera_freqP)),
            Odonata_IRI = ((Odonata_numP + Odonata_massP)*(Odonata_freqP)),
            Hemiptera_IRI = ((Hemiptera_numP + Hemiptera_massP)*(Hemiptera_freqP)),
            Other_IRI = ((Other_numP + Other_massP)*(Other_freqP)),
            Fish_IRI = ((Fish_numP + Fish_massP)*(Fish_freqP)))


#Transpose dataframe to long format
IRI.pr <- mat.IRI.pr.tr %>% pivot_longer(cols=c("Fish_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI"), names_to='Taxon', values_to='IRI')

#Calculate IRI's percentages
IRI.pr.percent <- IRI.pr %>%                                  
  group_by(Pond, Sex) %>%
  mutate(sum_iri = sum(IRI)) %>%
  group_by(Pond, Sex) %>%
  mutate(IRI_P = (IRI/sum_iri)*100) %>%
  as.data.frame()

IRI.pr.percent$Spawning <- "Pre-Spawn"

##### Post Spawn #####
#Filter data pre spawn time
diet.mat.po <- diet.mat.tr %>% filter(Month > 6)

#Now calculate row totals for Numeric and Mass 
#Calculate Total Number of Individual Occurences
diet.mat.po <- diet.mat.po %>% 
  group_by(Pond, Sex) %>%
  rowwise() %>% 
  mutate(NumTot = sum(c(BivalvTot_num, GastroTot_num, PelZoopTot_num, ClitellaTot_num, EphemTot_num, TrichopteraTot_num, DipteraTot_num, OdonataTot_num, HemipteraTot_num, Other_num,FishTot_num)))

#Calculate Mass Totals
diet.mat.po <- diet.mat.po %>% rowwise() %>% mutate(MassTot = sum(c(BivalvTot_mass,GastroTot_mass, PelZoopTot_mass, ClitellaTot_mass, EphemTot_mass, Trichoptera_mass, DipteraTot_mass, OdonataTot_mass, HemipteraTot_mass, Other_mass, FishTot_mass)))

#Calculate frequency total
diet.mat.po <- diet.mat.po %>% rowwise() %>% 
  mutate(FreqTot = sum(BivalvTot_num>0, GastroTot_num>0, PelZoopTot_num>0, ClitellaTot_num>0, EphemTot_num>0, TrichopteraTot_num>0, DipteraTot_num>0, OdonataTot_num>0, HemipteraTot_num>0, Other_num>0, FishTot_num>0))


#Create new column of Freq per taxa
diet.mat.po <- diet.mat.po %>% rowwise() %>% 
  mutate(Bivalv_freq = sum(BivalvTot_num>0),
         Gastro_freq = sum(GastroTot_num>0),
         Zoop_freq = sum(PelZoopTot_num>0),
         Clitella_freq = sum(ClitellaTot_num>0),
         Ephem_freq = sum(EphemTot_num>0),
         Trichop_freq = sum(TrichopteraTot_num>0),
         Diptera_freq = sum(DipteraTot_num>0),
         Odonata_freq = sum(OdonataTot_num>0),
         Hemiptera_freq = sum(HemipteraTot_num>0),
         Other_freq = sum(Other_num>0),
         Fish_freq = sum(FishTot_num>0))


#Summarize by pond (across months) and calculate IRI per pond, per taxa
all.mat.summ.po <- diet.mat.po %>% group_by(Pond, Sex) %>% 
  summarise_at(c("BivalvTot_num","BivalvTot_mass","GastroTot_num","GastroTot_mass","PelZoopTot_num","PelZoopTot_mass","ClitellaTot_num","ClitellaTot_mass","EphemTot_num","EphemTot_mass","TrichopteraTot_num","Trichoptera_mass","DipteraTot_num","DipteraTot_mass","OdonataTot_num","OdonataTot_mass","HemipteraTot_num","HemipteraTot_mass","Other_num","Other_mass","FishTot_num","FishTot_mass","NumTot","MassTot","FreqTot","Bivalv_freq", "Gastro_freq", "Zoop_freq", "Clitella_freq", "Ephem_freq","Trichop_freq","Diptera_freq","Odonata_freq","Hemiptera_freq", "Other_freq", "Fish_freq"), sum, na.rm = TRUE)

#Ungroup first to remove group_by() limit and allow rowwise() calculation 
all.mat.summ.po <- all.mat.summ.po %>% ungroup()

#Calculate percent numeric, mass, and freq per taxa
#Numeric
all.mat.summ.po <- all.mat.summ.po %>% rowwise() %>% 
  mutate(Bivalv_numP = BivalvTot_num/NumTot,
         Gastro_numP = GastroTot_num/NumTot,
         Zoop_numP = PelZoopTot_num/NumTot,
         Clitella_numP = ClitellaTot_num/NumTot,
         Ephem_numP = EphemTot_num/NumTot,
         Trichop_numP = TrichopteraTot_num/NumTot, 
         Diptera_numP = DipteraTot_num/NumTot,
         Odonata_numP = OdonataTot_num/NumTot,
         Hemiptera_numP = HemipteraTot_num/NumTot,
         Other_numP = Other_num/NumTot, 
         Fish_numP = FishTot_num/NumTot)

#Mass
all.mat.summ.po <- all.mat.summ.po %>% rowwise() %>% 
  mutate(Bivalv_massP = BivalvTot_mass/MassTot,
         Gastro_massP = GastroTot_mass/MassTot,
         Zoop_massP = PelZoopTot_mass/MassTot, 
         Clitella_massP = ClitellaTot_mass/MassTot,
         Ephem_massP = EphemTot_mass/MassTot,
         Trichop_massP = Trichoptera_mass/MassTot,
         Diptera_massP = DipteraTot_mass/MassTot, 
         Odonata_massP = OdonataTot_mass/MassTot,
         Hemiptera_massP = HemipteraTot_mass/MassTot, 
         Other_massP = Other_mass/MassTot,
         Fish_massP = FishTot_mass/MassTot)

#Frequency
all.mat.summ.po <- all.mat.summ.po %>% rowwise() %>% 
  mutate(Bivalv_freqP = Bivalv_freq/FreqTot,
         Gastro_freqP = Gastro_freq/FreqTot,
         Zoop_freqP = Zoop_freq/FreqTot, 
         Clitella_freqP = Clitella_freq/FreqTot,
         Ephem_freqP = Ephem_freq/FreqTot, 
         Trichop_freqP = Trichop_freq/FreqTot,
         Diptera_freqP = Diptera_freq/FreqTot, 
         Odonata_freqP = Odonata_freq/FreqTot, 
         Hemiptera_freqP = Hemiptera_freq/FreqTot,
         Other_freqP = Other_freq/FreqTot,
         Fish_freqP = Fish_freq/FreqTot)

#Calculate IRI for each diet item
mat.IRI.po.tr <- all.mat.summ.po %>% group_by(Pond, Sex) %>%
  rowwise() %>% 
  summarize(Bivalv_IRI = ((Bivalv_numP + Bivalv_massP)*(Bivalv_freqP)),
            Gastro_IRI = ((Gastro_numP + Gastro_massP)*(Gastro_freqP)),
            Zoop_IRI = ((Zoop_numP + Zoop_massP)*(Zoop_freqP)),
            Clitella_IRI = ((Clitella_numP + Clitella_massP)*(Clitella_freqP)),
            Ephem_IRI = ((Ephem_numP + Ephem_massP)*(Ephem_freqP)),
            Trichop_IRI = ((Trichop_numP + Trichop_massP)*(Trichop_freqP)),
            Diptera_IRI = ((Diptera_numP + Diptera_massP)*(Diptera_freqP)),
            Odonata_IRI = ((Odonata_numP + Odonata_massP)*(Odonata_freqP)),
            Hemiptera_IRI = ((Hemiptera_numP + Hemiptera_massP)*(Hemiptera_freqP)),
            Other_IRI = ((Other_numP + Other_massP)*(Other_freqP)),
            Fish_IRI = ((Fish_numP + Fish_massP)*(Fish_freqP)))


#Transpose dataframe to long format
IRI.po <- mat.IRI.po.tr %>% pivot_longer(cols=c("Fish_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI"), names_to='Taxon', values_to='IRI')

#Calculate IRI's percentages
IRI.po.percent <- IRI.po %>%                                  
  group_by(Pond, Sex) %>%
  mutate(sum_iri = sum(IRI)) %>%
  group_by(Pond, Sex) %>%
  mutate(IRI_P = (IRI/sum_iri)*100) %>%
  as.data.frame()

IRI.po.percent$Spawning <- "Post-Spawn"

### Important to note that KSB males are 0 because all males caught had empty stomachs

##### Combine IPI Dataframes into one #####
IRI.pr.po <- rbind(IRI.pr.percent, IRI.po.percent)

#subset by pond
IRI.pr.po.gsp.m <- IRI.pr.po %>% filter(Pond == "GSP", Sex == "M")
IRI.pr.po.gsp.f <- IRI.pr.po %>% filter(Pond == "GSP", Sex == "F")
IRI.pr.po.gsp.prs <- IRI.pr.po %>% filter(Pond == "GSP", Spawning == "Pre-Spawn")
IRI.pr.po.gsp.pos <- IRI.pr.po %>% filter(Pond == "GSP", Spawning == "Post-Spawn")
IRI.pr.po.pp.m <- IRI.pr.po %>% filter(Pond == "PP", Sex == "M")
IRI.pr.po.pp.f <- IRI.pr.po %>% filter(Pond == "PP", Sex == "F")
IRI.pr.po.pp.prs <- IRI.pr.po %>% filter(Pond == "PP", Spawning == "Pre-Spawn")
IRI.pr.po.pp.pos <- IRI.pr.po %>% filter(Pond == "PP", Spawning == "Post-Spawn")
IRI.pr.po.kss.m <- IRI.pr.po %>% filter(Pond == "KSS", Sex == "M")
IRI.pr.po.kss.f <- IRI.pr.po %>% filter(Pond == "KSS", Sex == "F")
IRI.pr.po.kss.prs <- IRI.pr.po %>% filter(Pond == "KSS", Spawning == "Pre-Spawn")
IRI.pr.po.kss.pos <- IRI.pr.po %>% filter(Pond == "KSS", Spawning == "Post-Spawn")

#Spearman Rank Correlation For Pre vs Post Spawn by Pond
#GSP - Males Pre-Post Spawning
cor.test(IRI.pr.po.gsp.m[IRI.pr.po.gsp.m$Spawning == "Pre-Spawn",]$IRI_P, IRI.pr.po.gsp.m[IRI.pr.po.gsp.m$Spawning == "Post-Spawn",]$IRI_P, method = "spearman")

#GSP - Females Pre-Post Spawning
cor.test(IRI.pr.po.gsp.f[IRI.pr.po.gsp.f$Spawning == "Pre-Spawn",]$IRI_P, IRI.pr.po.gsp.f[IRI.pr.po.gsp.f$Spawning == "Post-Spawn",]$IRI_P, method = "spearman")

#GSP - Males-Females Pre-Spawning
cor.test(IRI.pr.po.gsp.prs[IRI.pr.po.gsp.prs$Sex == "M",]$IRI_P, IRI.pr.po.gsp.prs[IRI.pr.po.gsp.prs$Sex == "F",]$IRI_P, method = "spearman")

#GSP - Males-Females Post-Spawning
cor.test(IRI.pr.po.gsp.pos[IRI.pr.po.gsp.pos$Sex == "M",]$IRI_P, IRI.pr.po.gsp.pos[IRI.pr.po.gsp.pos$Sex == "F",]$IRI_P, method = "spearman")

#PP - Males Pre-Post Spawning
cor.test(IRI.pr.po.pp.m[IRI.pr.po.pp.m$Spawning == "Pre-Spawn",]$IRI_P, IRI.pr.po.pp.m[IRI.pr.po.pp.m$Spawning == "Post-Spawn",]$IRI_P, method = "spearman")

#PP - Females Pre-Post Spawning
cor.test(IRI.pr.po.pp.f[IRI.pr.po.pp.f$Spawning == "Pre-Spawn",]$IRI_P, IRI.pr.po.pp.f[IRI.pr.po.pp.f$Spawning == "Post-Spawn",]$IRI_P, method = "spearman")

#PP - Males-Females Pre-Spawning
cor.test(IRI.pr.po.pp.prs[IRI.pr.po.pp.prs$Sex == "M",]$IRI_P, IRI.pr.po.pp.prs[IRI.pr.po.pp.prs$Sex == "F",]$IRI_P, method = "spearman")

#PP - Males-Females Post-Spawning
cor.test(IRI.pr.po.pp.pos[IRI.pr.po.pp.pos$Sex == "M",]$IRI_P, IRI.pr.po.pp.pos[IRI.pr.po.pp.pos$Sex == "F",]$IRI_P, method = "spearman")

#KSS - Males Pre-Post Spawning
cor.test(IRI.pr.po.kss.m[IRI.pr.po.kss.m$Spawning == "Pre-Spawn",]$IRI_P, IRI.pr.po.kss.m[IRI.pr.po.kss.m$Spawning == "Post-Spawn",]$IRI_P, method = "spearman")

#KSS - Females Pre-Post Spawning
cor.test(IRI.pr.po.kss.f[IRI.pr.po.kss.f$Spawning == "Pre-Spawn",]$IRI_P, IRI.pr.po.kss.f[IRI.pr.po.kss.f$Spawning == "Post-Spawn",]$IRI_P, method = "spearman")

#KSS - Males-Females Pre-Spawning
cor.test(IRI.pr.po.kss.prs[IRI.pr.po.kss.prs$Sex == "M",]$IRI_P, IRI.pr.po.kss.prs[IRI.pr.po.kss.prs$Sex == "F",]$IRI_P, method = "spearman")

#KSS - Males-Females Post-Spawning
cor.test(IRI.pr.po.kss.pos[IRI.pr.po.kss.pos$Sex == "M",]$IRI_P, IRI.pr.po.kss.pos[IRI.pr.po.kss.pos$Sex == "F",]$IRI_P, method = "spearman")


#Plots of IRI Pre-Post
#Four plots - one each for males and females pre/post spawn
IRI.pr.percent.mpr <- IRI.pr.percent %>% filter(Sex == "M")
IRI.pr.percent.fpr <- IRI.pr.percent %>% filter(Sex == "F")
IRI.po.percent.mpo <- IRI.po.percent %>% filter(Sex == "M")
IRI.po.percent.fpo <- IRI.po.percent %>% filter(Sex == "F")

#Set Color Palette
myPal <- c("#9E0142", "#FCAA5F","#FDC877", "#FEE391", "#CAE99D","#A6DBA4", "#7ECBA4", "#59B4AA","#3B92B8", "#4470B1", "#5E4FA2")

IRI.pr.percent.mpr$Taxon <- factor(IRI.pr.percent.mpr$Taxon, levels = c("Fish_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI"))

IRI.pr.percent.fpr$Taxon <- factor(IRI.pr.percent.fpr$Taxon, levels = c("Fish_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI"))

IRI.po.percent.mpo$Taxon <- factor(IRI.pr.percent.mpo$Taxon, levels = c("Fish_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI"))

IRI.po.percent.fpo$Taxon <- factor(IRI.pr.percent.fpo$Taxon, levels = c("Fish_IRI", "Bivalv_IRI", "Gastro_IRI", "Zoop_IRI", "Clitella_IRI", "Ephem_IRI", "Trichop_IRI", "Diptera_IRI", "Odonata_IRI", "Hemiptera_IRI", "Other_IRI"))


gg_pre_M_IRI <- ggplot(IRI.pr.percent.mpr, aes(x = Pond, y = IRI_P, fill = Taxon)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = myPal) + 
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), 
        axis.text.y = element_text(color="black", size = 15), 
        axis.title = element_blank()) 
gg_pre_M_IRI

gg_pre_F_IRI <- ggplot(IRI.pr.percent.fpr, aes(x = Pond, y = IRI_P, fill = Taxon)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = myPal) + 
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), 
        axis.text.y = element_text(color="black", size = 15), 
        axis.title = element_blank()) 
gg_pre_F_IRI

gg_post_M_IRI <- ggplot(IRI.po.percent.mpo, aes(x = Pond, y = IRI_P, fill = Taxon)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) + 
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), 
        axis.text.y = element_text(color="black", size = 15), 
        axis.title = element_blank()) 
gg_post_M_IRI

gg_post_F_IRI <- ggplot(IRI.po.percent.fpo, aes(x = Pond, y = IRI_P, fill = Taxon)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = myPal) + 
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(color="black", size = 15), 
        axis.text.y = element_text(color="black", size = 15), 
        axis.title = element_blank()) 
gg_post_F_IRI


gg_pre_post_MF <- ggarrange(gg_pre_M_IRI, gg_post_M_IRI,
                            gg_pre_F_IRI, gg_post_F_IRI,
                         nrow = 2, ncol = 2)
gg_pre_post_MF

