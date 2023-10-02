# Pumpkinseed diet from UoT research station Ponds

#Author(s): Timothy Fernandes & Reilly O'Connor
#Version: 2023-10-02

#Pkgs
library(tidyverse)
library(RColorBrewer)

#load data
df_diet <- read.csv("../Pumpkinseeds/Data/Stouffville_DietMatrix_Master.xlsx", header = T)
df_all <- read.csv("../Pumpkinseeds/Data/Stouffville_Sampling_Master.xlsx", header = T)

##### Code #####