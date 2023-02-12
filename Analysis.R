library(stringr)
library(raster)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plyr)
library(ggpubr)
library(data.table)

Batchfile <- read.csv("C:/FionaPlenderleith/CH4_models/Analysis/Batchfile.csv") #file with batch info
Batchfile_5000 <- read.csv("C:/FionaPlenderleith/CH4_models/Analysis/Batchfile_5000.csv")#file with batch info for other area
Tree <- read.csv("C:/FionaPlenderleith/Northern_forest_treeplant/data_subset.csv") #treee cover data
Tree_BS <- read.csv("C:/FionaPlenderleith/CH4_models/Analysis/Tree_data.csv") # broadleaf cover
All_tree <- left_join(Tree, Tree_BS, by = c("Grid"), all = TRUE) # all tree data 
# Pop ####

Batch1001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1_pop <- rbind(Batch1001_pop, Batch1002_pop, Batch1003_pop, Batch1004_pop, Batch1005_pop, Batch1006_pop)

Batch2001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2_pop <- rbind(Batch2001_pop, Batch2002_pop, Batch2003_pop, Batch2004_pop, Batch2005_pop, Batch2006_pop)

Batch3001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3_pop <- rbind(Batch3001_pop, Batch3002_pop, Batch3003_pop, Batch3004_pop, Batch3005_pop, Batch3006_pop)

Batch4001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4_pop <- rbind(Batch4001_pop, Batch4002_pop, Batch4003_pop, Batch4004_pop, Batch4005_pop, Batch4006_pop)

Batch5001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5_pop <- rbind(Batch5001_pop, Batch5002_pop, Batch5003_pop, Batch5004_pop, Batch5005_pop, Batch5006_pop)

Batch6001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6_pop <- rbind(Batch6001_pop, Batch6002_pop, Batch6003_pop, Batch6004_pop, Batch6005_pop, Batch6006_pop)

Batch7001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7_pop <- rbind(Batch7001_pop, Batch7002_pop, Batch7003_pop, Batch7004_pop, Batch7005_pop, Batch7006_pop)

Batch8001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8_pop <- rbind(Batch8001_pop, Batch8002_pop, Batch8003_pop, Batch8004_pop, Batch8005_pop, Batch8006_pop)

Batch9001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9_pop <- rbind(Batch9001_pop, Batch9002_pop, Batch9003_pop, Batch9004_pop, Batch9005_pop, Batch9006_pop)

Batch10001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10_pop <- rbind(Batch10001_pop, Batch10002_pop, Batch10003_pop, Batch10004_pop, Batch10005_pop, Batch10006_pop)

Batch11001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11_pop <- rbind(Batch11001_pop, Batch11002_pop, Batch11003_pop, Batch11004_pop, Batch11005_pop, Batch11006_pop)

Batch12001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12_pop <- rbind(Batch12001_pop, Batch12002_pop, Batch12003_pop, Batch12004_pop, Batch12005_pop, Batch12006_pop)

Batch13001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13_pop <- rbind(Batch13001_pop, Batch13002_pop, Batch13003_pop, Batch13004_pop, Batch13005_pop, Batch13006_pop)

Batch14001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14_pop <- rbind(Batch14001_pop, Batch14002_pop, Batch14003_pop, Batch14004_pop, Batch14005_pop, Batch14006_pop)

Batch15001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15_pop <- rbind(Batch15001_pop, Batch15002_pop, Batch15003_pop, Batch15004_pop, Batch15005_pop, Batch15006_pop)

Batch16001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16_pop <- rbind(Batch16001_pop, Batch16002_pop, Batch16003_pop, Batch16004_pop, Batch16005_pop, Batch16006_pop)

Batch17001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17_pop <- rbind(Batch17001_pop, Batch17002_pop, Batch17003_pop, Batch17004_pop, Batch17005_pop, Batch17006_pop)

Batch18001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18_pop <- rbind(Batch18001_pop, Batch18002_pop, Batch18003_pop, Batch18004_pop, Batch18005_pop, Batch18006_pop)

Batch19001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19_pop <- rbind(Batch19001_pop, Batch19002_pop, Batch19003_pop, Batch19004_pop, Batch19005_pop, Batch19006_pop)

Batch20001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20_pop <- rbind(Batch20001_pop, Batch20002_pop, Batch20003_pop, Batch20004_pop, Batch20005_pop, Batch20006_pop)

Batch21001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21_pop <- rbind(Batch21001_pop, Batch21002_pop, Batch21003_pop, Batch21004_pop, Batch21005_pop, Batch21006_pop)

Batch22001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22_pop <- rbind(Batch22001_pop, Batch22002_pop, Batch22003_pop, Batch22004_pop, Batch22005_pop, Batch22006_pop)

Batch23001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23_pop <- rbind(Batch23001_pop, Batch23002_pop, Batch23003_pop, Batch23004_pop, Batch23005_pop, Batch23006_pop)

Batch24001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24_pop <- rbind(Batch24001_pop, Batch24002_pop, Batch24003_pop, Batch24004_pop, Batch24005_pop, Batch24006_pop)

Batch25001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25_pop <- rbind(Batch25001_pop, Batch25002_pop, Batch25003_pop, Batch25004_pop, Batch25005_pop, Batch25006_pop)

Batch26001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26_pop <- rbind(Batch26001_pop, Batch26002_pop, Batch26003_pop, Batch26004_pop, Batch26005_pop, Batch26006_pop)

Batch27001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27_pop <- rbind(Batch27001_pop, Batch27002_pop, Batch27003_pop, Batch27004_pop, Batch27005_pop, Batch27006_pop)

Batch28001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28_pop <- rbind(Batch28001_pop, Batch28002_pop, Batch28003_pop, Batch28004_pop, Batch28005_pop, Batch28006_pop)

Batch29001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29_pop <- rbind(Batch29001_pop, Batch29002_pop, Batch29003_pop, Batch29004_pop, Batch29005_pop, Batch29006_pop)

Batch30001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30_pop <- rbind(Batch30001_pop, Batch30002_pop, Batch30003_pop, Batch30004_pop, Batch30005_pop, Batch30006_pop)

Batch31001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31_pop <- rbind(Batch31001_pop, Batch31002_pop, Batch31003_pop, Batch31004_pop, Batch31005_pop, Batch31006_pop)

Batch32001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32_pop <- rbind(Batch32001_pop, Batch32002_pop, Batch32003_pop, Batch32004_pop, Batch32005_pop, Batch32006_pop)

Batch33001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33_pop <- rbind(Batch33001_pop, Batch33002_pop, Batch33003_pop, Batch33004_pop, Batch33005_pop, Batch33006_pop)

Batch34001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34_pop <- rbind(Batch34001_pop, Batch34002_pop, Batch34003_pop, Batch34004_pop, Batch34005_pop, Batch34006_pop)

Batch35001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35_pop <- rbind(Batch35001_pop, Batch35002_pop, Batch35003_pop, Batch35004_pop, Batch35005_pop, Batch35006_pop)

Batch36001_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36002_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36003_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36004_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36005_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36006_pop <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36_pop <- rbind(Batch36001_pop, Batch36002_pop, Batch36003_pop, Batch36004_pop, Batch36005_pop, Batch36006_pop)

pop_data <- rbind(Batch1_pop, Batch2_pop, Batch3_pop, Batch4_pop, Batch5_pop, Batch6_pop, Batch7_pop, Batch8_pop, Batch9_pop, Batch10_pop, Batch11_pop, Batch12_pop, Batch13_pop, Batch14_pop, Batch15_pop, Batch16_pop, Batch17_pop, Batch18_pop, Batch19_pop, Batch20_pop, Batch21_pop, Batch22_pop, Batch23_pop, Batch24_pop, Batch25_pop, Batch26_pop, Batch27_pop, Batch28_pop, Batch29_pop, Batch30_pop, Batch31_pop, Batch32_pop, Batch33_pop, Batch34_pop, Batch35_pop, Batch36_pop)

# disp ####

Batch1001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1_disp <- rbind(Batch1001_disp, Batch1002_disp, Batch1003_disp, Batch1004_disp, Batch1005_disp, Batch1006_disp)

Batch2001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2_disp <- rbind(Batch2001_disp, Batch2002_disp, Batch2003_disp, Batch2004_disp, Batch2005_disp, Batch2006_disp)

Batch3001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3_disp <- rbind(Batch3001_disp, Batch3002_disp, Batch3003_disp, Batch3004_disp, Batch3005_disp, Batch3006_disp)

Batch4001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4_disp <- rbind(Batch4001_disp, Batch4002_disp, Batch4003_disp, Batch4004_disp, Batch4005_disp, Batch4006_disp)

Batch5001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5_disp <- rbind(Batch5001_disp, Batch5002_disp, Batch5003_disp, Batch5004_disp, Batch5005_disp, Batch5006_disp)

Batch6001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6_disp <- rbind(Batch6001_disp, Batch6002_disp, Batch6003_disp, Batch6004_disp, Batch6005_disp, Batch6006_disp)

Batch7001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7_disp <- rbind(Batch7001_disp, Batch7002_disp, Batch7003_disp, Batch7004_disp, Batch7005_disp, Batch7006_disp)

Batch8001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8_disp <- rbind(Batch8001_disp, Batch8002_disp, Batch8003_disp, Batch8004_disp, Batch8005_disp, Batch8006_disp)

Batch9001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9_disp <- rbind(Batch9001_disp, Batch9002_disp, Batch9003_disp, Batch9004_disp, Batch9005_disp, Batch9006_disp)

Batch10001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10_disp <- rbind(Batch10001_disp, Batch10002_disp, Batch10003_disp, Batch10004_disp, Batch10005_disp, Batch10006_disp)

Batch11001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11_disp <- rbind(Batch11001_disp, Batch11002_disp, Batch11003_disp, Batch11004_disp, Batch11005_disp, Batch11006_disp)

Batch12001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12_disp <- rbind(Batch12001_disp, Batch12002_disp, Batch12003_disp, Batch12004_disp, Batch12005_disp, Batch12006_disp)

Batch13001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13_disp <- rbind(Batch13001_disp, Batch13002_disp, Batch13003_disp, Batch13004_disp, Batch13005_disp, Batch13006_disp)

Batch14001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14_disp <- rbind(Batch14001_disp, Batch14002_disp, Batch14003_disp, Batch14004_disp, Batch14005_disp, Batch14006_disp)

Batch15001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15_disp <- rbind(Batch15001_disp, Batch15002_disp, Batch15003_disp, Batch15004_disp, Batch15005_disp, Batch15006_disp)

Batch16001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16_disp <- rbind(Batch16001_disp, Batch16002_disp, Batch16003_disp, Batch16004_disp, Batch16005_disp, Batch16006_disp)

Batch17001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17_disp <- rbind(Batch17001_disp, Batch17002_disp, Batch17003_disp, Batch17004_disp, Batch17005_disp, Batch17006_disp)

Batch18001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18_disp <- rbind(Batch18001_disp, Batch18002_disp, Batch18003_disp, Batch18004_disp, Batch18005_disp, Batch18006_disp)

Batch19001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19_disp <- rbind(Batch19001_disp, Batch19002_disp, Batch19003_disp, Batch19004_disp, Batch19005_disp, Batch19006_disp)

Batch20001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20_disp <- rbind(Batch20001_disp, Batch20002_disp, Batch20003_disp, Batch20004_disp, Batch20005_disp, Batch20006_disp)

Batch21001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21_disp <- rbind(Batch21001_disp, Batch21002_disp, Batch21003_disp, Batch21004_disp, Batch21005_disp, Batch21006_disp)

Batch22001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22_disp <- rbind(Batch22001_disp, Batch22002_disp, Batch22003_disp, Batch22004_disp, Batch22005_disp, Batch22006_disp)

Batch23001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23_disp <- rbind(Batch23001_disp, Batch23002_disp, Batch23003_disp, Batch23004_disp, Batch23005_disp, Batch23006_disp)

Batch24001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24_disp <- rbind(Batch24001_disp, Batch24002_disp, Batch24003_disp, Batch24004_disp, Batch24005_disp, Batch24006_disp)

Batch25001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25_disp <- rbind(Batch25001_disp, Batch25002_disp, Batch25003_disp, Batch25004_disp, Batch25005_disp, Batch25006_disp)

Batch26001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26_disp <- rbind(Batch26001_disp, Batch26002_disp, Batch26003_disp, Batch26004_disp, Batch26005_disp, Batch26006_disp)

Batch27001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27_disp <- rbind(Batch27001_disp, Batch27002_disp, Batch27003_disp, Batch27004_disp, Batch27005_disp, Batch27006_disp)

Batch28001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28_disp <- rbind(Batch28001_disp, Batch28002_disp, Batch28003_disp, Batch28004_disp, Batch28005_disp, Batch28006_disp)

Batch29001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29_disp <- rbind(Batch29001_disp, Batch29002_disp, Batch29003_disp, Batch29004_disp, Batch29005_disp, Batch29006_disp)

Batch30001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30_disp <- rbind(Batch30001_disp, Batch30002_disp, Batch30003_disp, Batch30004_disp, Batch30005_disp, Batch30006_disp)

Batch31001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31_disp <- rbind(Batch31001_disp, Batch31002_disp, Batch31003_disp, Batch31004_disp, Batch31005_disp, Batch31006_disp)

Batch32001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32_disp <- rbind(Batch32001_disp, Batch32002_disp, Batch32003_disp, Batch32004_disp, Batch32005_disp, Batch32006_disp)

Batch33001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33_disp <- rbind(Batch33001_disp, Batch33002_disp, Batch33003_disp, Batch33004_disp, Batch33005_disp, Batch33006_disp)

Batch34001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34_disp <- rbind(Batch34001_disp, Batch34002_disp, Batch34003_disp, Batch34004_disp, Batch34005_disp, Batch34006_disp)

Batch35001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35_disp <- rbind(Batch35001_disp, Batch35002_disp, Batch35003_disp, Batch35004_disp, Batch35005_disp, Batch35006_disp)

Batch36001_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36002_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36003_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36004_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36005_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36006_disp <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36_disp <- rbind(Batch36001_disp, Batch36002_disp, Batch36003_disp, Batch36004_disp, Batch36005_disp, Batch36006_disp)

disp_data <- rbind(Batch1_disp, Batch2_disp, Batch3_disp, Batch4_disp, Batch5_disp, Batch6_disp, Batch7_disp, Batch8_disp, Batch9_disp, Batch10_disp, Batch11_disp, Batch12_disp, Batch13_disp, Batch14_disp, Batch15_disp, Batch16_disp, Batch17_disp, Batch18_disp, Batch19_disp, Batch20_disp, Batch21_disp, Batch22_disp, Batch23_disp, Batch24_disp, Batch25_disp, Batch26_disp, Batch27_disp, Batch28_disp, Batch29_disp, Batch30_disp, Batch31_disp, Batch32_disp, Batch33_disp, Batch34_disp, Batch35_disp, Batch36_disp)



# iso ####

Batch1001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch1002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch1003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch1004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch1005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch1006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch1_iso <- rbind(Batch1001_iso, Batch1002_iso, Batch1003_iso, Batch1004_iso, Batch1005_iso, Batch1006_iso)

Batch2001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch2002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch2003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch2004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch2005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch2006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch2_iso <- rbind(Batch2001_iso, Batch2002_iso, Batch2003_iso, Batch2004_iso, Batch2005_iso, Batch2006_iso)

Batch3001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch3002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch3003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch3004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch3005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch3006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch3_iso <- rbind(Batch3001_iso, Batch3002_iso, Batch3003_iso, Batch3004_iso, Batch3005_iso, Batch3006_iso)

Batch4001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch4002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch4003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch4004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch4005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch4006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch4_iso <- rbind(Batch4001_iso, Batch4002_iso, Batch4003_iso, Batch4004_iso, Batch4005_iso, Batch4006_iso)

Batch5001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch5002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch5003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch5004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch5005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch5006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch5_iso <- rbind(Batch5001_iso, Batch5002_iso, Batch5003_iso, Batch5004_iso, Batch5005_iso, Batch5006_iso)

Batch6001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch6002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch6003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch6004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch6005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch6006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch6_iso <- rbind(Batch6001_iso, Batch6002_iso, Batch6003_iso, Batch6004_iso, Batch6005_iso, Batch6006_iso)

Batch7001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch7002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch7003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch7004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch7005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch7006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch7_iso <- rbind(Batch7001_iso, Batch7002_iso, Batch7003_iso, Batch7004_iso, Batch7005_iso, Batch7006_iso)

Batch8001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch8002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch8003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch8004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch8005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch8006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch8_iso <- rbind(Batch8001_iso, Batch8002_iso, Batch8003_iso, Batch8004_iso, Batch8005_iso, Batch8006_iso)

Batch9001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch9002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch9003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch9004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch9005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch9006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch9_iso <- rbind(Batch9001_iso, Batch9002_iso, Batch9003_iso, Batch9004_iso, Batch9005_iso, Batch9006_iso)

Batch10001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch10002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch10003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch10004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch10005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch10006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch10_iso <- rbind(Batch10001_iso, Batch10002_iso, Batch10003_iso, Batch10004_iso, Batch10005_iso, Batch10006_iso)

Batch11001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch11002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch11003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch11004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch11005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch11006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch11_iso <- rbind(Batch11001_iso, Batch11002_iso, Batch11003_iso, Batch11004_iso, Batch11005_iso, Batch11006_iso)

Batch12001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch12002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch12003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch12004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch12005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch12006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch12_iso <- rbind(Batch12001_iso, Batch12002_iso, Batch12003_iso, Batch12004_iso, Batch12005_iso, Batch12006_iso)

Batch13001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch13002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch13003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch13004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch13005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch13006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch13_iso <- rbind(Batch13001_iso, Batch13002_iso, Batch13003_iso, Batch13004_iso, Batch13005_iso, Batch13006_iso)

Batch14001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch14002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch14003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch14004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch14005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch14006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch14_iso <- rbind(Batch14001_iso, Batch14002_iso, Batch14003_iso, Batch14004_iso, Batch14005_iso, Batch14006_iso)

Batch15001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch15002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch15003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch15004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch15005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch15006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch15_iso <- rbind(Batch15001_iso, Batch15002_iso, Batch15003_iso, Batch15004_iso, Batch15005_iso, Batch15006_iso)

Batch16001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch16002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch16003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch16004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch16005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch16006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch16_iso <- rbind(Batch16001_iso, Batch16002_iso, Batch16003_iso, Batch16004_iso, Batch16005_iso, Batch16006_iso)

Batch17001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch17002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch17003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch17004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch17005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch17006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch17_iso <- rbind(Batch17001_iso, Batch17002_iso, Batch17003_iso, Batch17004_iso, Batch17005_iso, Batch17006_iso)

Batch18001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch18002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch18003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch18004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch18005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch18006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch18_iso <- rbind(Batch18001_iso, Batch18002_iso, Batch18003_iso, Batch18004_iso, Batch18005_iso, Batch18006_iso)

Batch19001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch19002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch19003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch19004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch19005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch19006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch19_iso <- rbind(Batch19001_iso, Batch19002_iso, Batch19003_iso, Batch19004_iso, Batch19005_iso, Batch19006_iso)

Batch20001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch20002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch20003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch20004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch20005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch20006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch20_iso <- rbind(Batch20001_iso, Batch20002_iso, Batch20003_iso, Batch20004_iso, Batch20005_iso, Batch20006_iso)

Batch21001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch21002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch21003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch21004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch21005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch21006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch21_iso <- rbind(Batch21001_iso, Batch21002_iso, Batch21003_iso, Batch21004_iso, Batch21005_iso, Batch21006_iso)

Batch22001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch22002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch22003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch22004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch22005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch22006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch22_iso <- rbind(Batch22001_iso, Batch22002_iso, Batch22003_iso, Batch22004_iso, Batch22005_iso, Batch22006_iso)

Batch23001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch23002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch23003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch23004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch23005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch23006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch23_iso <- rbind(Batch23001_iso, Batch23002_iso, Batch23003_iso, Batch23004_iso, Batch23005_iso, Batch23006_iso)

Batch24001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch24002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch24003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch24004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch24005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch24006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch24_iso <- rbind(Batch24001_iso, Batch24002_iso, Batch24003_iso, Batch24004_iso, Batch24005_iso, Batch24006_iso)

Batch25001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch25002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch25003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch25004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch25005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch25006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch25_iso <- rbind(Batch25001_iso, Batch25002_iso, Batch25003_iso, Batch25004_iso, Batch25005_iso, Batch25006_iso)

Batch26001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch26002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch26003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch26004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch26005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch26006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch26_iso <- rbind(Batch26001_iso, Batch26002_iso, Batch26003_iso, Batch26004_iso, Batch26005_iso, Batch26006_iso)

Batch27001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch27002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch27003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch27004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch27005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch27006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch27_iso <- rbind(Batch27001_iso, Batch27002_iso, Batch27003_iso, Batch27004_iso, Batch27005_iso, Batch27006_iso)

Batch28001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch28002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch28003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch28004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch28005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch28006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch28_iso <- rbind(Batch28001_iso, Batch28002_iso, Batch28003_iso, Batch28004_iso, Batch28005_iso, Batch28006_iso)

Batch29001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch29002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch29003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch29004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch29005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch29006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch29_iso <- rbind(Batch29001_iso, Batch29002_iso, Batch29003_iso, Batch29004_iso, Batch29005_iso, Batch29006_iso)

Batch30001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch30002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch30003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch30004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch30005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch30006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch30_iso <- rbind(Batch30001_iso, Batch30002_iso, Batch30003_iso, Batch30004_iso, Batch30005_iso, Batch30006_iso)

Batch31001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch31002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch31003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch31004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch31005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch31006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch31_iso <- rbind(Batch31001_iso, Batch31002_iso, Batch31003_iso, Batch31004_iso, Batch31005_iso, Batch31006_iso)

Batch32001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch32002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch32003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch32004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch32005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch32006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch32_iso <- rbind(Batch32001_iso, Batch32002_iso, Batch32003_iso, Batch32004_iso, Batch32005_iso, Batch32006_iso)

Batch33001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch33002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch33003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch33004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch33005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch33006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch33_iso <- rbind(Batch33001_iso, Batch33002_iso, Batch33003_iso, Batch33004_iso, Batch33005_iso, Batch33006_iso)

Batch34001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch34002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch34003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch34004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch34005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch34006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch34_iso <- rbind(Batch34001_iso, Batch34002_iso, Batch34003_iso, Batch34004_iso, Batch34005_iso, Batch34006_iso)

Batch35001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch35002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch35003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch35004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch35005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch35006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch35_iso <- rbind(Batch35001_iso, Batch35002_iso, Batch35003_iso, Batch35004_iso, Batch35005_iso, Batch35006_iso)

Batch36001_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch36002_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch36003_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch36004_iso <-Isolated_patch(patch = "R1_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch36005_iso <-Isolated_patch(patch = "R2_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch36006_iso <-Isolated_patch(patch = "R3_10000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch36_iso <- rbind(Batch36001_iso, Batch36002_iso, Batch36003_iso, Batch36004_iso, Batch36005_iso, Batch36006_iso)

iso_data <- rbind(Batch1_iso, Batch2_iso, Batch3_iso, Batch4_iso, Batch5_iso, Batch6_iso, Batch7_iso, Batch8_iso, Batch9_iso, Batch10_iso, Batch11_iso, Batch12_iso, Batch13_iso, Batch14_iso, Batch15_iso, Batch16_iso, Batch17_iso, Batch18_iso, Batch19_iso, Batch20_iso, Batch21_iso, Batch22_iso, Batch23_iso, Batch24_iso, Batch25_iso, Batch26_iso, Batch27_iso, Batch28_iso, Batch29_iso, Batch30_iso, Batch31_iso, Batch32_iso, Batch33_iso, Batch34_iso, Batch35_iso, Batch36_iso)









# gen ####

Batch1001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch1002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch1003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch1004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch1005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch1006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ10NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch1_gen <- rbind(Batch1001_gen_5000, Batch1002_gen_5000, Batch1003_gen_5000, Batch1004_gen_5000, Batch1005_gen_5000, Batch1006_gen_5000)

Batch2001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch2002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch2003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch2004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch2005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch2006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ40NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch2_gen <- rbind(Batch2001_gen_5000, Batch2002_gen_5000, Batch2003_gen_5000, Batch2004_gen_5000, Batch2005_gen_5000, Batch2006_gen_5000)

Batch3001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch3002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch3003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch3004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch3005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch3006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/NZ70NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch3_gen <- rbind(Batch3001_gen_5000, Batch3002_gen_5000, Batch3003_gen_5000, Batch3004_gen_5000, Batch3005_gen_5000, Batch3006_gen_5000)

Batch4001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch4002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch4003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch4004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch4005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch4006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD31SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch4_gen <- rbind(Batch4001_gen_5000, Batch4002_gen_5000, Batch4003_gen_5000, Batch4004_gen_5000, Batch4005_gen_5000, Batch4006_gen_5000)

Batch5001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch5002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch5003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch5004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch5005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch5006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD44NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch5_gen <- rbind(Batch5001_gen_5000, Batch5002_gen_5000, Batch5003_gen_5000, Batch5004_gen_5000, Batch5005_gen_5000, Batch5006_gen_5000)

Batch6001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch6002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch6003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch6004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch6005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch6006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD55NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch6_gen <- rbind(Batch6001_gen_5000, Batch6002_gen_5000, Batch6003_gen_5000, Batch6004_gen_5000, Batch6005_gen_5000, Batch6006_gen_5000)

Batch7001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch7002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch7003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch7004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch7005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch7006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD75SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch7_gen <- rbind(Batch7001_gen_5000, Batch7002_gen_5000, Batch7003_gen_5000, Batch7004_gen_5000, Batch7005_gen_5000, Batch7006_gen_5000)

Batch8001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch8002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch8003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch8004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch8005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch8006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD78NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch8_gen <- rbind(Batch8001_gen_5000, Batch8002_gen_5000, Batch8003_gen_5000, Batch8004_gen_5000, Batch8005_gen_5000, Batch8006_gen_5000)

Batch9001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch9002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch9003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch9004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch9005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch9006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD88NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch9_gen <- rbind(Batch9001_gen_5000, Batch9002_gen_5000, Batch9003_gen_5000, Batch9004_gen_5000, Batch9005_gen_5000, Batch9006_gen_5000)

Batch10001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch10002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch10003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch10004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch10005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch10006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SD95NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch10_gen <- rbind(Batch10001_gen_5000, Batch10002_gen_5000, Batch10003_gen_5000, Batch10004_gen_5000, Batch10005_gen_5000, Batch10006_gen_5000)

Batch11001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch11002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch11003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch11004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch11005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch11006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE01SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch11_gen <- rbind(Batch11001_gen_5000, Batch11002_gen_5000, Batch11003_gen_5000, Batch11004_gen_5000, Batch11005_gen_5000, Batch11006_gen_5000)

Batch12001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch12002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch12003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch12004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch12005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch12006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE05SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch12_gen <- rbind(Batch12001_gen_5000, Batch12002_gen_5000, Batch12003_gen_5000, Batch12004_gen_5000, Batch12005_gen_5000, Batch12006_gen_5000)

Batch13001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch13002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch13003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch13004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch13005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch13006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE07NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch13_gen <- rbind(Batch13001_gen_5000, Batch13002_gen_5000, Batch13003_gen_5000, Batch13004_gen_5000, Batch13005_gen_5000, Batch13006_gen_5000)

Batch14001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch14002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch14003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch14004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch14005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch14006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE15SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch14_gen <- rbind(Batch14001_gen_5000, Batch14002_gen_5000, Batch14003_gen_5000, Batch14004_gen_5000, Batch14005_gen_5000, Batch14006_gen_5000)

Batch15001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch15002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch15003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch15004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch15005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch15006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE28NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch15_gen <- rbind(Batch15001_gen_5000, Batch15002_gen_5000, Batch15003_gen_5000, Batch15004_gen_5000, Batch15005_gen_5000, Batch15006_gen_5000)

Batch16001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch16002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch16003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch16004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch16005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch16006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE29NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch16_gen <- rbind(Batch16001_gen_5000, Batch16002_gen_5000, Batch16003_gen_5000, Batch16004_gen_5000, Batch16005_gen_5000, Batch16006_gen_5000)

Batch17001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch17002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch17003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch17004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch17005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch17006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE40SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch17_gen <- rbind(Batch17001_gen_5000, Batch17002_gen_5000, Batch17003_gen_5000, Batch17004_gen_5000, Batch17005_gen_5000, Batch17006_gen_5000)

Batch18001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch18002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch18003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch18004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch18005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch18006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE51SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch18_gen <- rbind(Batch18001_gen_5000, Batch18002_gen_5000, Batch18003_gen_5000, Batch18004_gen_5000, Batch18005_gen_5000, Batch18006_gen_5000)

Batch19001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch19002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch19003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch19004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch19005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch19006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE53SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch19_gen <- rbind(Batch19001_gen_5000, Batch19002_gen_5000, Batch19003_gen_5000, Batch19004_gen_5000, Batch19005_gen_5000, Batch19006_gen_5000)

Batch20001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch20002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch20003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch20004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch20005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch20006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch20_gen <- rbind(Batch20001_gen_5000, Batch20002_gen_5000, Batch20003_gen_5000, Batch20004_gen_5000, Batch20005_gen_5000, Batch20006_gen_5000)

Batch21001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch21002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch21003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch21004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch21005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch21006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE56NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch21_gen <- rbind(Batch21001_gen_5000, Batch21002_gen_5000, Batch21003_gen_5000, Batch21004_gen_5000, Batch21005_gen_5000, Batch21006_gen_5000)

Batch22001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch22002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch22003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch22004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch22005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch22006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch22_gen <- rbind(Batch22001_gen_5000, Batch22002_gen_5000, Batch22003_gen_5000, Batch22004_gen_5000, Batch22005_gen_5000, Batch22006_gen_5000)

Batch23001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch23002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch23003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch23004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch23005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch23006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE65SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch23_gen <- rbind(Batch23001_gen_5000, Batch23002_gen_5000, Batch23003_gen_5000, Batch23004_gen_5000, Batch23005_gen_5000, Batch23006_gen_5000)

Batch24001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch24002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch24003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch24004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch24005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch24006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch24_gen <- rbind(Batch24001_gen_5000, Batch24002_gen_5000, Batch24003_gen_5000, Batch24004_gen_5000, Batch24005_gen_5000, Batch24006_gen_5000)

Batch25001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch25002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch25003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch25004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch25005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch25006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE66SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch25_gen <- rbind(Batch25001_gen_5000, Batch25002_gen_5000, Batch25003_gen_5000, Batch25004_gen_5000, Batch25005_gen_5000, Batch25006_gen_5000)

Batch26001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch26002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch26003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch26004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch26005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch26006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE73SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch26_gen <- rbind(Batch26001_gen_5000, Batch26002_gen_5000, Batch26003_gen_5000, Batch26004_gen_5000, Batch26005_gen_5000, Batch26006_gen_5000)

Batch27001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch27002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch27003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch27004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch27005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch27006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SE88SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch27_gen <- rbind(Batch27001_gen_5000, Batch27002_gen_5000, Batch27003_gen_5000, Batch27004_gen_5000, Batch27005_gen_5000, Batch27006_gen_5000)

Batch28001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch28002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch28003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch28004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch28005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch28006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SJ56SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch28_gen <- rbind(Batch28001_gen_5000, Batch28002_gen_5000, Batch28003_gen_5000, Batch28004_gen_5000, Batch28005_gen_5000, Batch28006_gen_5000)

Batch29001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch29002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch29003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch29004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch29005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch29006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch29_gen <- rbind(Batch29001_gen_5000, Batch29002_gen_5000, Batch29003_gen_5000, Batch29004_gen_5000, Batch29005_gen_5000, Batch29006_gen_5000)

Batch30001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch30002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch30003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch30004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch30005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch30006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK05SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch30_gen <- rbind(Batch30001_gen_5000, Batch30002_gen_5000, Batch30003_gen_5000, Batch30004_gen_5000, Batch30005_gen_5000, Batch30006_gen_5000)

Batch31001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch31002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch31003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch31004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch31005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch31006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK18SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch31_gen <- rbind(Batch31001_gen_5000, Batch31002_gen_5000, Batch31003_gen_5000, Batch31004_gen_5000, Batch31005_gen_5000, Batch31006_gen_5000)

Batch32001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch32002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch32003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch32004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch32005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch32006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK79NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch32_gen <- rbind(Batch32001_gen_5000, Batch32002_gen_5000, Batch32003_gen_5000, Batch32004_gen_5000, Batch32005_gen_5000, Batch32006_gen_5000)

Batch33001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch33002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch33003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch33004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch33005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch33006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/SK98SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch33_gen <- rbind(Batch33001_gen_5000, Batch33002_gen_5000, Batch33003_gen_5000, Batch33004_gen_5000, Batch33005_gen_5000, Batch33006_gen_5000)

Batch34001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch34002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch34003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch34004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch34005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch34006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TA20SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch34_gen <- rbind(Batch34001_gen_5000, Batch34002_gen_5000, Batch34003_gen_5000, Batch34004_gen_5000, Batch34005_gen_5000, Batch34006_gen_5000)

Batch35001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch35002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch35003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch35004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch35005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch35006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF18NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch35_gen <- rbind(Batch35001_gen_5000, Batch35002_gen_5000, Batch35003_gen_5000, Batch35004_gen_5000, Batch35005_gen_5000, Batch35006_gen_5000)

Batch36001_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch36002_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch36003_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch36004_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch36005_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch36006_gen <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian/TF39SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch36_gen <- rbind(Batch36001_gen_5000, Batch36002_gen_5000, Batch36003_gen_5000, Batch36004_gen_5000, Batch36005_gen_5000, Batch36006_gen_5000)

gen_data <- rbind(Batch1_gen_5000, Batch2_gen_5000, Batch3_gen_5000, Batch4_gen_5000, Batch5_gen_5000, Batch6_gen_5000, Batch7_gen_5000, Batch8_gen_5000, Batch9_gen_5000, Batch10_gen_5000, Batch11_gen_5000, Batch12_gen_5000, Batch13_gen_5000, Batch14_gen_5000, Batch15_gen_5000, Batch16_gen_5000, Batch17_gen_5000, Batch20_gen_5000, Batch21_gen_5000, Batch22_gen_5000, Batch23_gen_5000, Batch24_gen_5000, Batch25_gen_5000, Batch26_gen_5000, Batch27_gen_5000, Batch28_gen_5000, Batch29_gen_5000, Batch30_gen_5000, Batch31_gen_5000, Batch32_gen_5000, Batch33_gen_5000, Batch34_gen_5000, Batch35_gen_5000, Batch36_gen_5000)


## Master data ####

mergeCols <- c("Simulation", "Batch", "Land", "Rep", "Year")
Master_data <-
  merge(iso_data, disp_data, by = mergeCols, all = TRUE)
Master_data <-
  merge(Master_data, pop_data, by = mergeCols, all = TRUE)
Master_data <-
  merge(Master_data, gen_data, by = mergeCols, all = TRUE)
Master_data <-
  left_join(Master_data, Batchfile, by = c("Simulation", "Batch", "Land"), all = TRUE)



Master_data <-
  left_join(Master_data, All_tree, by = c("Grid"), all = TRUE)

Master_data$Disp_mort <- 1 - Master_data$proportion_success_con
Master_data$Target <- 10000



 #combining metrics for all replicates into data frame
# Pop_5000 ####

Batch1001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch1_pop_5000 <- rbind(Batch1001_pop_5000, Batch1002_pop_5000, Batch1003_pop_5000, Batch1004_pop_5000, Batch1005_pop_5000, Batch1006_pop_5000)

Batch2001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch2_pop_5000 <- rbind(Batch2001_pop_5000, Batch2002_pop_5000, Batch2003_pop_5000, Batch2004_pop_5000, Batch2005_pop_5000, Batch2006_pop_5000)

Batch3001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch3_pop_5000 <- rbind(Batch3001_pop_5000, Batch3002_pop_5000, Batch3003_pop_5000, Batch3004_pop_5000, Batch3005_pop_5000, Batch3006_pop_5000)

Batch4001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch4_pop_5000 <- rbind(Batch4001_pop_5000, Batch4002_pop_5000, Batch4003_pop_5000, Batch4004_pop_5000, Batch4005_pop_5000, Batch4006_pop_5000)

Batch5001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch5_pop_5000 <- rbind(Batch5001_pop_5000, Batch5002_pop_5000, Batch5003_pop_5000, Batch5004_pop_5000, Batch5005_pop_5000, Batch5006_pop_5000)

Batch6001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch6_pop_5000 <- rbind(Batch6001_pop_5000, Batch6002_pop_5000, Batch6003_pop_5000, Batch6004_pop_5000, Batch6005_pop_5000, Batch6006_pop_5000)

Batch7001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch7_pop_5000 <- rbind(Batch7001_pop_5000, Batch7002_pop_5000, Batch7003_pop_5000, Batch7004_pop_5000, Batch7005_pop_5000, Batch7006_pop_5000)

Batch8001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch8_pop_5000 <- rbind(Batch8001_pop_5000, Batch8002_pop_5000, Batch8003_pop_5000, Batch8004_pop_5000, Batch8005_pop_5000, Batch8006_pop_5000)

Batch9001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch9_pop_5000 <- rbind(Batch9001_pop_5000, Batch9002_pop_5000, Batch9003_pop_5000, Batch9004_pop_5000, Batch9005_pop_5000, Batch9006_pop_5000)

Batch10001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch10_pop_5000 <- rbind(Batch10001_pop_5000, Batch10002_pop_5000, Batch10003_pop_5000, Batch10004_pop_5000, Batch10005_pop_5000, Batch10006_pop_5000)

Batch11001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch11_pop_5000 <- rbind(Batch11001_pop_5000, Batch11002_pop_5000, Batch11003_pop_5000, Batch11004_pop_5000, Batch11005_pop_5000, Batch11006_pop_5000)

Batch12001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch12_pop_5000 <- rbind(Batch12001_pop_5000, Batch12002_pop_5000, Batch12003_pop_5000, Batch12004_pop_5000, Batch12005_pop_5000, Batch12006_pop_5000)

Batch13001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch13_pop_5000 <- rbind(Batch13001_pop_5000, Batch13002_pop_5000, Batch13003_pop_5000, Batch13004_pop_5000, Batch13005_pop_5000, Batch13006_pop_5000)

Batch14001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch14_pop_5000 <- rbind(Batch14001_pop_5000, Batch14002_pop_5000, Batch14003_pop_5000, Batch14004_pop_5000, Batch14005_pop_5000, Batch14006_pop_5000)

Batch15001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch15_pop_5000 <- rbind(Batch15001_pop_5000, Batch15002_pop_5000, Batch15003_pop_5000, Batch15004_pop_5000, Batch15005_pop_5000, Batch15006_pop_5000)

Batch16001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch16_pop_5000 <- rbind(Batch16001_pop_5000, Batch16002_pop_5000, Batch16003_pop_5000, Batch16004_pop_5000, Batch16005_pop_5000, Batch16006_pop_5000)

Batch17001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch17_pop_5000 <- rbind(Batch17001_pop_5000, Batch17002_pop_5000, Batch17003_pop_5000, Batch17004_pop_5000, Batch17005_pop_5000, Batch17006_pop_5000)

Batch18001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch18_pop_5000 <- rbind(Batch18001_pop_5000, Batch18002_pop_5000, Batch18003_pop_5000, Batch18004_pop_5000, Batch18005_pop_5000, Batch18006_pop_5000)

Batch19001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch19_pop_5000 <- rbind(Batch19001_pop_5000, Batch19002_pop_5000, Batch19003_pop_5000, Batch19004_pop_5000, Batch19005_pop_5000, Batch19006_pop_5000)

Batch20001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch20_pop_5000 <- rbind(Batch20001_pop_5000, Batch20002_pop_5000, Batch20003_pop_5000, Batch20004_pop_5000, Batch20005_pop_5000, Batch20006_pop_5000)

Batch21001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch21_pop_5000 <- rbind(Batch21001_pop_5000, Batch21002_pop_5000, Batch21003_pop_5000, Batch21004_pop_5000, Batch21005_pop_5000, Batch21006_pop_5000)

Batch22001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch22_pop_5000 <- rbind(Batch22001_pop_5000, Batch22002_pop_5000, Batch22003_pop_5000, Batch22004_pop_5000, Batch22005_pop_5000, Batch22006_pop_5000)

Batch23001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch23_pop_5000 <- rbind(Batch23001_pop_5000, Batch23002_pop_5000, Batch23003_pop_5000, Batch23004_pop_5000, Batch23005_pop_5000, Batch23006_pop_5000)

Batch24001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch24_pop_5000 <- rbind(Batch24001_pop_5000, Batch24002_pop_5000, Batch24003_pop_5000, Batch24004_pop_5000, Batch24005_pop_5000, Batch24006_pop_5000)

Batch25001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch25_pop_5000 <- rbind(Batch25001_pop_5000, Batch25002_pop_5000, Batch25003_pop_5000, Batch25004_pop_5000, Batch25005_pop_5000, Batch25006_pop_5000)

Batch26001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch26_pop_5000 <- rbind(Batch26001_pop_5000, Batch26002_pop_5000, Batch26003_pop_5000, Batch26004_pop_5000, Batch26005_pop_5000, Batch26006_pop_5000)

Batch27001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch27_pop_5000 <- rbind(Batch27001_pop_5000, Batch27002_pop_5000, Batch27003_pop_5000, Batch27004_pop_5000, Batch27005_pop_5000, Batch27006_pop_5000)

Batch28001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch28_pop_5000 <- rbind(Batch28001_pop_5000, Batch28002_pop_5000, Batch28003_pop_5000, Batch28004_pop_5000, Batch28005_pop_5000, Batch28006_pop_5000)

Batch29001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch29_pop_5000 <- rbind(Batch29001_pop_5000, Batch29002_pop_5000, Batch29003_pop_5000, Batch29004_pop_5000, Batch29005_pop_5000, Batch29006_pop_5000)

Batch30001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch30_pop_5000 <- rbind(Batch30001_pop_5000, Batch30002_pop_5000, Batch30003_pop_5000, Batch30004_pop_5000, Batch30005_pop_5000, Batch30006_pop_5000)

Batch31001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch31_pop_5000 <- rbind(Batch31001_pop_5000, Batch31002_pop_5000, Batch31003_pop_5000, Batch31004_pop_5000, Batch31005_pop_5000, Batch31006_pop_5000)

Batch32001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch32_pop_5000 <- rbind(Batch32001_pop_5000, Batch32002_pop_5000, Batch32003_pop_5000, Batch32004_pop_5000, Batch32005_pop_5000, Batch32006_pop_5000)

Batch33001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch33_pop_5000 <- rbind(Batch33001_pop_5000, Batch33002_pop_5000, Batch33003_pop_5000, Batch33004_pop_5000, Batch33005_pop_5000, Batch33006_pop_5000)

Batch34001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch34_pop_5000 <- rbind(Batch34001_pop_5000, Batch34002_pop_5000, Batch34003_pop_5000, Batch34004_pop_5000, Batch34005_pop_5000, Batch34006_pop_5000)

Batch35001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch35_pop_5000 <- rbind(Batch35001_pop_5000, Batch35002_pop_5000, Batch35003_pop_5000, Batch35004_pop_5000, Batch35005_pop_5000, Batch35006_pop_5000)

Batch36001_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch1/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36002_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch2/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36003_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch3/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36004_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch4/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36005_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch5/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36006_pop_5000 <-Pop_size(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch6/Outputs",mypattern = "Pop.txt", sep="\t")
Batch36_pop_5000 <- rbind(Batch36001_pop_5000, Batch36002_pop_5000, Batch36003_pop_5000, Batch36004_pop_5000, Batch36005_pop_5000, Batch36006_pop_5000)

pop_data_5000 <- rbind(Batch1_pop_5000, Batch2_pop_5000, Batch3_pop_5000, Batch4_pop_5000, Batch5_pop_5000, Batch6_pop_5000, Batch7_pop_5000, Batch8_pop_5000, Batch9_pop_5000, Batch10_pop_5000, Batch11_pop_5000, Batch12_pop_5000, Batch13_pop_5000, Batch14_pop_5000, Batch15_pop_5000, Batch16_pop_5000, Batch17_pop_5000, Batch18_pop_5000, Batch19_pop_5000, Batch20_pop_5000, Batch21_pop_5000, Batch22_pop_5000, Batch23_pop_5000, Batch24_pop_5000, Batch25_pop_5000, Batch26_pop_5000, Batch27_pop_5000, Batch28_pop_5000, Batch29_pop_5000, Batch30_pop_5000, Batch31_pop_5000, Batch32_pop_5000, Batch33_pop_5000, Batch34_pop_5000, Batch35_pop_5000, Batch36_pop_5000)

# dis_5000 ####

Batch1001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch1_disp_5000 <- rbind(Batch1001_disp_5000, Batch1002_disp_5000, Batch1003_disp_5000, Batch1004_disp_5000, Batch1005_disp_5000, Batch1006_disp_5000)

Batch2001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch2_disp_5000 <- rbind(Batch2001_disp_5000, Batch2002_disp_5000, Batch2003_disp_5000, Batch2004_disp_5000, Batch2005_disp_5000, Batch2006_disp_5000)

Batch3001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch3_disp_5000 <- rbind(Batch3001_disp_5000, Batch3002_disp_5000, Batch3003_disp_5000, Batch3004_disp_5000, Batch3005_disp_5000, Batch3006_disp_5000)

Batch4001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch4_disp_5000 <- rbind(Batch4001_disp_5000, Batch4002_disp_5000, Batch4003_disp_5000, Batch4004_disp_5000, Batch4005_disp_5000, Batch4006_disp_5000)

Batch5001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch5_disp_5000 <- rbind(Batch5001_disp_5000, Batch5002_disp_5000, Batch5003_disp_5000, Batch5004_disp_5000, Batch5005_disp_5000, Batch5006_disp_5000)

Batch6001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch6_disp_5000 <- rbind(Batch6001_disp_5000, Batch6002_disp_5000, Batch6003_disp_5000, Batch6004_disp_5000, Batch6005_disp_5000, Batch6006_disp_5000)

Batch7001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch7_disp_5000 <- rbind(Batch7001_disp_5000, Batch7002_disp_5000, Batch7003_disp_5000, Batch7004_disp_5000, Batch7005_disp_5000, Batch7006_disp_5000)

Batch8001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch8_disp_5000 <- rbind(Batch8001_disp_5000, Batch8002_disp_5000, Batch8003_disp_5000, Batch8004_disp_5000, Batch8005_disp_5000, Batch8006_disp_5000)

Batch9001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch9_disp_5000 <- rbind(Batch9001_disp_5000, Batch9002_disp_5000, Batch9003_disp_5000, Batch9004_disp_5000, Batch9005_disp_5000, Batch9006_disp_5000)

Batch10001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch10_disp_5000 <- rbind(Batch10001_disp_5000, Batch10002_disp_5000, Batch10003_disp_5000, Batch10004_disp_5000, Batch10005_disp_5000, Batch10006_disp_5000)

Batch11001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch11_disp_5000 <- rbind(Batch11001_disp_5000, Batch11002_disp_5000, Batch11003_disp_5000, Batch11004_disp_5000, Batch11005_disp_5000, Batch11006_disp_5000)

Batch12001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch12_disp_5000 <- rbind(Batch12001_disp_5000, Batch12002_disp_5000, Batch12003_disp_5000, Batch12004_disp_5000, Batch12005_disp_5000, Batch12006_disp_5000)

Batch13001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch13_disp_5000 <- rbind(Batch13001_disp_5000, Batch13002_disp_5000, Batch13003_disp_5000, Batch13004_disp_5000, Batch13005_disp_5000, Batch13006_disp_5000)

Batch14001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch14_disp_5000 <- rbind(Batch14001_disp_5000, Batch14002_disp_5000, Batch14003_disp_5000, Batch14004_disp_5000, Batch14005_disp_5000, Batch14006_disp_5000)

Batch15001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch15_disp_5000 <- rbind(Batch15001_disp_5000, Batch15002_disp_5000, Batch15003_disp_5000, Batch15004_disp_5000, Batch15005_disp_5000, Batch15006_disp_5000)

Batch16001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch16_disp_5000 <- rbind(Batch16001_disp_5000, Batch16002_disp_5000, Batch16003_disp_5000, Batch16004_disp_5000, Batch16005_disp_5000, Batch16006_disp_5000)

Batch17001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch17_disp_5000 <- rbind(Batch17001_disp_5000, Batch17002_disp_5000, Batch17003_disp_5000, Batch17004_disp_5000, Batch17005_disp_5000, Batch17006_disp_5000)

Batch18001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch18_disp_5000 <- rbind(Batch18001_disp_5000, Batch18002_disp_5000, Batch18003_disp_5000, Batch18004_disp_5000, Batch18005_disp_5000, Batch18006_disp_5000)

Batch19001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch19_disp_5000 <- rbind(Batch19001_disp_5000, Batch19002_disp_5000, Batch19003_disp_5000, Batch19004_disp_5000, Batch19005_disp_5000, Batch19006_disp_5000)

Batch20001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch20_disp_5000 <- rbind(Batch20001_disp_5000, Batch20002_disp_5000, Batch20003_disp_5000, Batch20004_disp_5000, Batch20005_disp_5000, Batch20006_disp_5000)

Batch21001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch21_disp_5000 <- rbind(Batch21001_disp_5000, Batch21002_disp_5000, Batch21003_disp_5000, Batch21004_disp_5000, Batch21005_disp_5000, Batch21006_disp_5000)

Batch22001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch22_disp_5000 <- rbind(Batch22001_disp_5000, Batch22002_disp_5000, Batch22003_disp_5000, Batch22004_disp_5000, Batch22005_disp_5000, Batch22006_disp_5000)

Batch23001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch23_disp_5000 <- rbind(Batch23001_disp_5000, Batch23002_disp_5000, Batch23003_disp_5000, Batch23004_disp_5000, Batch23005_disp_5000, Batch23006_disp_5000)

Batch24001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch24_disp_5000 <- rbind(Batch24001_disp_5000, Batch24002_disp_5000, Batch24003_disp_5000, Batch24004_disp_5000, Batch24005_disp_5000, Batch24006_disp_5000)

Batch25001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch25_disp_5000 <- rbind(Batch25001_disp_5000, Batch25002_disp_5000, Batch25003_disp_5000, Batch25004_disp_5000, Batch25005_disp_5000, Batch25006_disp_5000)

Batch26001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch26_disp_5000 <- rbind(Batch26001_disp_5000, Batch26002_disp_5000, Batch26003_disp_5000, Batch26004_disp_5000, Batch26005_disp_5000, Batch26006_disp_5000)

Batch27001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch27_disp_5000 <- rbind(Batch27001_disp_5000, Batch27002_disp_5000, Batch27003_disp_5000, Batch27004_disp_5000, Batch27005_disp_5000, Batch27006_disp_5000)

Batch28001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch28_disp_5000 <- rbind(Batch28001_disp_5000, Batch28002_disp_5000, Batch28003_disp_5000, Batch28004_disp_5000, Batch28005_disp_5000, Batch28006_disp_5000)

Batch29001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch29_disp_5000 <- rbind(Batch29001_disp_5000, Batch29002_disp_5000, Batch29003_disp_5000, Batch29004_disp_5000, Batch29005_disp_5000, Batch29006_disp_5000)

Batch30001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch30_disp_5000 <- rbind(Batch30001_disp_5000, Batch30002_disp_5000, Batch30003_disp_5000, Batch30004_disp_5000, Batch30005_disp_5000, Batch30006_disp_5000)

Batch31001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch31_disp_5000 <- rbind(Batch31001_disp_5000, Batch31002_disp_5000, Batch31003_disp_5000, Batch31004_disp_5000, Batch31005_disp_5000, Batch31006_disp_5000)

Batch32001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch32_disp_5000 <- rbind(Batch32001_disp_5000, Batch32002_disp_5000, Batch32003_disp_5000, Batch32004_disp_5000, Batch32005_disp_5000, Batch32006_disp_5000)

Batch33001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch33_disp_5000 <- rbind(Batch33001_disp_5000, Batch33002_disp_5000, Batch33003_disp_5000, Batch33004_disp_5000, Batch33005_disp_5000, Batch33006_disp_5000)

Batch34001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch34_disp_5000 <- rbind(Batch34001_disp_5000, Batch34002_disp_5000, Batch34003_disp_5000, Batch34004_disp_5000, Batch34005_disp_5000, Batch34006_disp_5000)

Batch35001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch35_disp_5000 <- rbind(Batch35001_disp_5000, Batch35002_disp_5000, Batch35003_disp_5000, Batch35004_disp_5000, Batch35005_disp_5000, Batch35006_disp_5000)

Batch36001_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch1/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36002_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch2/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36003_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch3/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36004_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch4/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36005_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch5/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36006_disp_5000 <-Dispersal_summary_con(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch6/Outputs",mypattern = "Connect.txt", sep="\t")
Batch36_disp_5000 <- rbind(Batch36001_disp_5000, Batch36002_disp_5000, Batch36003_disp_5000, Batch36004_disp_5000, Batch36005_disp_5000, Batch36006_disp_5000)

disp_data_5000 <- rbind(Batch1_disp_5000, Batch2_disp_5000, Batch3_disp_5000, Batch4_disp_5000, Batch5_disp_5000, Batch6_disp_5000, Batch7_disp_5000, Batch8_disp_5000, Batch9_disp_5000, Batch10_disp_5000, Batch11_disp_5000, Batch12_disp_5000, Batch13_disp_5000, Batch14_disp_5000, Batch15_disp_5000, Batch16_disp_5000, Batch17_disp_5000, Batch18_disp_5000, Batch19_disp_5000, Batch20_disp_5000, Batch21_disp_5000, Batch22_disp_5000, Batch23_disp_5000, Batch24_disp_5000, Batch25_disp_5000, Batch26_disp_5000, Batch27_disp_5000, Batch28_disp_5000, Batch29_disp_5000, Batch30_disp_5000, Batch31_disp_5000, Batch32_disp_5000, Batch33_disp_5000, Batch34_disp_5000, Batch35_disp_5000, Batch36_disp_5000)



# iso_5000 ####

Batch1001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch1002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch1003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch1004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch1005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch1006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch1_iso_5000 <- rbind(Batch1001_iso_5000, Batch1002_iso_5000, Batch1003_iso_5000, Batch1004_iso_5000, Batch1005_iso_5000, Batch1006_iso_5000)

Batch2001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch2002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch2003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch2004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch2005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch2006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch2_iso_5000 <- rbind(Batch2001_iso_5000, Batch2002_iso_5000, Batch2003_iso_5000, Batch2004_iso_5000, Batch2005_iso_5000, Batch2006_iso_5000)

Batch3001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch3002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch3003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch3004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch3005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch3006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch3_iso_5000 <- rbind(Batch3001_iso_5000, Batch3002_iso_5000, Batch3003_iso_5000, Batch3004_iso_5000, Batch3005_iso_5000, Batch3006_iso_5000)

Batch4001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch4002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch4003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch4004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch4005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch4006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch4_iso_5000 <- rbind(Batch4001_iso_5000, Batch4002_iso_5000, Batch4003_iso_5000, Batch4004_iso_5000, Batch4005_iso_5000, Batch4006_iso_5000)

Batch5001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch5002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch5003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch5004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch5005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch5006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch5_iso_5000 <- rbind(Batch5001_iso_5000, Batch5002_iso_5000, Batch5003_iso_5000, Batch5004_iso_5000, Batch5005_iso_5000, Batch5006_iso_5000)

Batch6001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch6002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch6003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch6004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch6005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch6006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch6_iso_5000 <- rbind(Batch6001_iso_5000, Batch6002_iso_5000, Batch6003_iso_5000, Batch6004_iso_5000, Batch6005_iso_5000, Batch6006_iso_5000)

Batch7001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch7002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch7003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch7004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch7005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch7006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch7_iso_5000 <- rbind(Batch7001_iso_5000, Batch7002_iso_5000, Batch7003_iso_5000, Batch7004_iso_5000, Batch7005_iso_5000, Batch7006_iso_5000)

Batch8001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch8002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch8003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch8004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch8005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch8006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch8_iso_5000 <- rbind(Batch8001_iso_5000, Batch8002_iso_5000, Batch8003_iso_5000, Batch8004_iso_5000, Batch8005_iso_5000, Batch8006_iso_5000)

Batch9001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch9002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch9003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch9004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch9005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch9006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch9_iso_5000 <- rbind(Batch9001_iso_5000, Batch9002_iso_5000, Batch9003_iso_5000, Batch9004_iso_5000, Batch9005_iso_5000, Batch9006_iso_5000)

Batch10001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch10002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch10003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch10004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch10005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch10006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch10_iso_5000 <- rbind(Batch10001_iso_5000, Batch10002_iso_5000, Batch10003_iso_5000, Batch10004_iso_5000, Batch10005_iso_5000, Batch10006_iso_5000)

Batch11001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch11002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch11003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch11004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch11005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch11006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch11_iso_5000 <- rbind(Batch11001_iso_5000, Batch11002_iso_5000, Batch11003_iso_5000, Batch11004_iso_5000, Batch11005_iso_5000, Batch11006_iso_5000)

Batch12001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch12002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch12003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch12004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch12005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch12006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch12_iso_5000 <- rbind(Batch12001_iso_5000, Batch12002_iso_5000, Batch12003_iso_5000, Batch12004_iso_5000, Batch12005_iso_5000, Batch12006_iso_5000)

Batch13001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch13002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch13003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch13004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch13005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch13006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch13_iso_5000 <- rbind(Batch13001_iso_5000, Batch13002_iso_5000, Batch13003_iso_5000, Batch13004_iso_5000, Batch13005_iso_5000, Batch13006_iso_5000)

Batch14001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch14002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch14003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch14004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch14005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch14006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch14_iso_5000 <- rbind(Batch14001_iso_5000, Batch14002_iso_5000, Batch14003_iso_5000, Batch14004_iso_5000, Batch14005_iso_5000, Batch14006_iso_5000)

Batch15001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch15002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch15003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch15004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch15005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch15006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch15_iso_5000 <- rbind(Batch15001_iso_5000, Batch15002_iso_5000, Batch15003_iso_5000, Batch15004_iso_5000, Batch15005_iso_5000, Batch15006_iso_5000)

Batch16001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch16002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch16003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch16004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch16005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch16006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch16_iso_5000 <- rbind(Batch16001_iso_5000, Batch16002_iso_5000, Batch16003_iso_5000, Batch16004_iso_5000, Batch16005_iso_5000, Batch16006_iso_5000)

Batch17001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch17002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch17003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch17004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch17005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch17006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch17_iso_5000 <- rbind(Batch17001_iso_5000, Batch17002_iso_5000, Batch17003_iso_5000, Batch17004_iso_5000, Batch17005_iso_5000, Batch17006_iso_5000)

Batch18001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch18002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch18003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch18004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch18005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch18006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch18_iso_5000 <- rbind(Batch18001_iso_5000, Batch18002_iso_5000, Batch18003_iso_5000, Batch18004_iso_5000, Batch18005_iso_5000, Batch18006_iso_5000)

Batch19001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch19002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch19003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch19004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch19005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch19006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch19_iso_5000 <- rbind(Batch19001_iso_5000, Batch19002_iso_5000, Batch19003_iso_5000, Batch19004_iso_5000, Batch19005_iso_5000, Batch19006_iso_5000)

Batch20001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch20002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch20003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch20004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch20005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch20006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch20_iso_5000 <- rbind(Batch20001_iso_5000, Batch20002_iso_5000, Batch20003_iso_5000, Batch20004_iso_5000, Batch20005_iso_5000, Batch20006_iso_5000)

Batch21001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch21002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch21003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch21004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch21005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch21006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch21_iso_5000 <- rbind(Batch21001_iso_5000, Batch21002_iso_5000, Batch21003_iso_5000, Batch21004_iso_5000, Batch21005_iso_5000, Batch21006_iso_5000)

Batch22001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch22002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch22003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch22004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch22005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch22006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch22_iso_5000 <- rbind(Batch22001_iso_5000, Batch22002_iso_5000, Batch22003_iso_5000, Batch22004_iso_5000, Batch22005_iso_5000, Batch22006_iso_5000)

Batch23001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch23002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch23003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch23004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch23005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch23006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch23_iso_5000 <- rbind(Batch23001_iso_5000, Batch23002_iso_5000, Batch23003_iso_5000, Batch23004_iso_5000, Batch23005_iso_5000, Batch23006_iso_5000)

Batch24001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch24002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch24003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch24004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch24005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch24006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch24_iso_5000 <- rbind(Batch24001_iso_5000, Batch24002_iso_5000, Batch24003_iso_5000, Batch24004_iso_5000, Batch24005_iso_5000, Batch24006_iso_5000)

Batch25001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch25002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch25003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch25004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch25005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch25006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch25_iso_5000 <- rbind(Batch25001_iso_5000, Batch25002_iso_5000, Batch25003_iso_5000, Batch25004_iso_5000, Batch25005_iso_5000, Batch25006_iso_5000)

Batch26001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch26002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch26003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch26004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch26005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch26006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch26_iso_5000 <- rbind(Batch26001_iso_5000, Batch26002_iso_5000, Batch26003_iso_5000, Batch26004_iso_5000, Batch26005_iso_5000, Batch26006_iso_5000)

Batch27001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch27002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch27003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch27004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch27005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch27006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch27_iso_5000 <- rbind(Batch27001_iso_5000, Batch27002_iso_5000, Batch27003_iso_5000, Batch27004_iso_5000, Batch27005_iso_5000, Batch27006_iso_5000)

Batch28001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch28002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch28003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch28004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch28005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch28006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch28_iso_5000 <- rbind(Batch28001_iso_5000, Batch28002_iso_5000, Batch28003_iso_5000, Batch28004_iso_5000, Batch28005_iso_5000, Batch28006_iso_5000)

Batch29001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch29002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch29003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch29004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch29005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch29006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch29_iso_5000 <- rbind(Batch29001_iso_5000, Batch29002_iso_5000, Batch29003_iso_5000, Batch29004_iso_5000, Batch29005_iso_5000, Batch29006_iso_5000)

Batch30001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch30002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch30003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch30004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch30005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch30006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch30_iso_5000 <- rbind(Batch30001_iso_5000, Batch30002_iso_5000, Batch30003_iso_5000, Batch30004_iso_5000, Batch30005_iso_5000, Batch30006_iso_5000)

Batch31001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch31002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch31003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch31004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch31005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch31006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch31_iso_5000 <- rbind(Batch31001_iso_5000, Batch31002_iso_5000, Batch31003_iso_5000, Batch31004_iso_5000, Batch31005_iso_5000, Batch31006_iso_5000)

Batch32001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch32002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch32003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch32004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch32005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch32006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch32_iso_5000 <- rbind(Batch32001_iso_5000, Batch32002_iso_5000, Batch32003_iso_5000, Batch32004_iso_5000, Batch32005_iso_5000, Batch32006_iso_5000)

Batch33001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch33002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch33003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch33004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch33005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch33006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch33_iso_5000 <- rbind(Batch33001_iso_5000, Batch33002_iso_5000, Batch33003_iso_5000, Batch33004_iso_5000, Batch33005_iso_5000, Batch33006_iso_5000)

Batch34001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch34002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch34003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch34004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch34005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch34006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch34_iso_5000 <- rbind(Batch34001_iso_5000, Batch34002_iso_5000, Batch34003_iso_5000, Batch34004_iso_5000, Batch34005_iso_5000, Batch34006_iso_5000)

Batch35001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch35002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch35003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch35004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch35005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch35006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch35_iso_5000 <- rbind(Batch35001_iso_5000, Batch35002_iso_5000, Batch35003_iso_5000, Batch35004_iso_5000, Batch35005_iso_5000, Batch35006_iso_5000)

Batch36001_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch1/",mypattern = "Connect.txt", sep="\t")
Batch36002_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch2/",mypattern = "Connect.txt", sep="\t")
Batch36003_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_generalist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch3/",mypattern = "Connect.txt", sep="\t")
Batch36004_iso_5000 <-Isolated_patch_5000(patch = "R1_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch4/",mypattern = "Connect.txt", sep="\t")
Batch36005_iso_5000 <-Isolated_patch_5000(patch = "R2_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch5/",mypattern = "Connect.txt", sep="\t")
Batch36006_iso_5000 <-Isolated_patch_5000(patch = "R3_5000_Scenario0_patch_specialist.asc",  mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch6/",mypattern = "Connect.txt", sep="\t")
Batch36_iso_5000 <- rbind(Batch36001_iso_5000, Batch36002_iso_5000, Batch36003_iso_5000, Batch36004_iso_5000, Batch36005_iso_5000, Batch36006_iso_5000)

iso_data_5000 <- rbind(Batch1_iso_5000, Batch2_iso_5000, Batch3_iso_5000, Batch4_iso_5000, Batch5_iso_5000, Batch6_iso_5000, Batch7_iso_5000, Batch8_iso_5000, Batch9_iso_5000, Batch10_iso_5000, Batch11_iso_5000, Batch12_iso_5000, Batch13_iso_5000, Batch14_iso_5000, Batch15_iso_5000, Batch16_iso_5000, Batch17_iso_5000, Batch18_iso_5000, Batch19_iso_5000, Batch20_iso_5000, Batch21_iso_5000, Batch22_iso_5000, Batch23_iso_5000, Batch24_iso_5000, Batch25_iso_5000, Batch26_iso_5000, Batch27_iso_5000, Batch28_iso_5000, Batch29_iso_5000, Batch30_iso_5000, Batch31_iso_5000, Batch32_iso_5000, Batch33_iso_5000, Batch34_iso_5000, Batch35_iso_5000, Batch36_iso_5000)









# gen_5000 ####

Batch1001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch1002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch1003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch1004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch1005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch1006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ10NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch1_gen_5000 <- rbind(Batch1001_gen_5000, Batch1002_gen_5000, Batch1003_gen_5000, Batch1004_gen_5000, Batch1005_gen_5000, Batch1006_gen_5000)

Batch2001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch2002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch2003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch2004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch2005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch2006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ40NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch2_gen_5000 <- rbind(Batch2001_gen_5000, Batch2002_gen_5000, Batch2003_gen_5000, Batch2004_gen_5000, Batch2005_gen_5000, Batch2006_gen_5000)

Batch3001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch3002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch3003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch3004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch3005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch3006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/NZ70NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch3_gen_5000 <- rbind(Batch3001_gen_5000, Batch3002_gen_5000, Batch3003_gen_5000, Batch3004_gen_5000, Batch3005_gen_5000, Batch3006_gen_5000)

Batch4001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch4002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch4003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch4004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch4005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch4006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD31SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch4_gen_5000 <- rbind(Batch4001_gen_5000, Batch4002_gen_5000, Batch4003_gen_5000, Batch4004_gen_5000, Batch4005_gen_5000, Batch4006_gen_5000)

Batch5001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch5002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch5003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch5004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch5005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch5006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD44NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch5_gen_5000 <- rbind(Batch5001_gen_5000, Batch5002_gen_5000, Batch5003_gen_5000, Batch5004_gen_5000, Batch5005_gen_5000, Batch5006_gen_5000)

Batch6001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch6002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch6003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch6004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch6005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch6006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD55NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch6_gen_5000 <- rbind(Batch6001_gen_5000, Batch6002_gen_5000, Batch6003_gen_5000, Batch6004_gen_5000, Batch6005_gen_5000, Batch6006_gen_5000)

Batch7001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch7002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch7003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch7004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch7005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch7006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD75SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch7_gen_5000 <- rbind(Batch7001_gen_5000, Batch7002_gen_5000, Batch7003_gen_5000, Batch7004_gen_5000, Batch7005_gen_5000, Batch7006_gen_5000)

Batch8001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch8002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch8003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch8004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch8005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch8006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD78NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch8_gen_5000 <- rbind(Batch8001_gen_5000, Batch8002_gen_5000, Batch8003_gen_5000, Batch8004_gen_5000, Batch8005_gen_5000, Batch8006_gen_5000)

Batch9001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch9002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch9003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch9004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch9005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch9006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD88NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch9_gen_5000 <- rbind(Batch9001_gen_5000, Batch9002_gen_5000, Batch9003_gen_5000, Batch9004_gen_5000, Batch9005_gen_5000, Batch9006_gen_5000)

Batch10001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch10002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch10003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch10004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch10005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch10006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SD95NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch10_gen_5000 <- rbind(Batch10001_gen_5000, Batch10002_gen_5000, Batch10003_gen_5000, Batch10004_gen_5000, Batch10005_gen_5000, Batch10006_gen_5000)

Batch11001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch11002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch11003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch11004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch11005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch11006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE01SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch11_gen_5000 <- rbind(Batch11001_gen_5000, Batch11002_gen_5000, Batch11003_gen_5000, Batch11004_gen_5000, Batch11005_gen_5000, Batch11006_gen_5000)

Batch12001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch12002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch12003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch12004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch12005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch12006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE05SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch12_gen_5000 <- rbind(Batch12001_gen_5000, Batch12002_gen_5000, Batch12003_gen_5000, Batch12004_gen_5000, Batch12005_gen_5000, Batch12006_gen_5000)

Batch13001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch13002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch13003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch13004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch13005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch13006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE07NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch13_gen_5000 <- rbind(Batch13001_gen_5000, Batch13002_gen_5000, Batch13003_gen_5000, Batch13004_gen_5000, Batch13005_gen_5000, Batch13006_gen_5000)

Batch14001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch14002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch14003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch14004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch14005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch14006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE15SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch14_gen_5000 <- rbind(Batch14001_gen_5000, Batch14002_gen_5000, Batch14003_gen_5000, Batch14004_gen_5000, Batch14005_gen_5000, Batch14006_gen_5000)

Batch15001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch15002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch15003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch15004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch15005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch15006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE28NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch15_gen_5000 <- rbind(Batch15001_gen_5000, Batch15002_gen_5000, Batch15003_gen_5000, Batch15004_gen_5000, Batch15005_gen_5000, Batch15006_gen_5000)

Batch16001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch16002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch16003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch16004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch16005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch16006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE29NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch16_gen_5000 <- rbind(Batch16001_gen_5000, Batch16002_gen_5000, Batch16003_gen_5000, Batch16004_gen_5000, Batch16005_gen_5000, Batch16006_gen_5000)

Batch17001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch17002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch17003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch17004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch17005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch17006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE40SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch17_gen_5000 <- rbind(Batch17001_gen_5000, Batch17002_gen_5000, Batch17003_gen_5000, Batch17004_gen_5000, Batch17005_gen_5000, Batch17006_gen_5000)

Batch18001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch18002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch18003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch18004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch18005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch18006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE51SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch18_gen_5000 <- rbind(Batch18001_gen_5000, Batch18002_gen_5000, Batch18003_gen_5000, Batch18004_gen_5000, Batch18005_gen_5000, Batch18006_gen_5000)

Batch19001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch19002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch19003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch19004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch19005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch19006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE53SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch19_gen_5000 <- rbind(Batch19001_gen_5000, Batch19002_gen_5000, Batch19003_gen_5000, Batch19004_gen_5000, Batch19005_gen_5000, Batch19006_gen_5000)

Batch20001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch20002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch20003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch20004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch20005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch20006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch20_gen_5000 <- rbind(Batch20001_gen_5000, Batch20002_gen_5000, Batch20003_gen_5000, Batch20004_gen_5000, Batch20005_gen_5000, Batch20006_gen_5000)

Batch21001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch21002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch21003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch21004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch21005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch21006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE56NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch21_gen_5000 <- rbind(Batch21001_gen_5000, Batch21002_gen_5000, Batch21003_gen_5000, Batch21004_gen_5000, Batch21005_gen_5000, Batch21006_gen_5000)

Batch22001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch22002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch22003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch22004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch22005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch22006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch22_gen_5000 <- rbind(Batch22001_gen_5000, Batch22002_gen_5000, Batch22003_gen_5000, Batch22004_gen_5000, Batch22005_gen_5000, Batch22006_gen_5000)

Batch23001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch23002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch23003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch23004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch23005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch23006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE65SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch23_gen_5000 <- rbind(Batch23001_gen_5000, Batch23002_gen_5000, Batch23003_gen_5000, Batch23004_gen_5000, Batch23005_gen_5000, Batch23006_gen_5000)

Batch24001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch24002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch24003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch24004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch24005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch24006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch24_gen_5000 <- rbind(Batch24001_gen_5000, Batch24002_gen_5000, Batch24003_gen_5000, Batch24004_gen_5000, Batch24005_gen_5000, Batch24006_gen_5000)

Batch25001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch25002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch25003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch25004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch25005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch25006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE66SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch25_gen_5000 <- rbind(Batch25001_gen_5000, Batch25002_gen_5000, Batch25003_gen_5000, Batch25004_gen_5000, Batch25005_gen_5000, Batch25006_gen_5000)

Batch26001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch26002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch26003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch26004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch26005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch26006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE73SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch26_gen_5000 <- rbind(Batch26001_gen_5000, Batch26002_gen_5000, Batch26003_gen_5000, Batch26004_gen_5000, Batch26005_gen_5000, Batch26006_gen_5000)

Batch27001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch27002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch27003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch27004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch27005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch27006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SE88SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch27_gen_5000 <- rbind(Batch27001_gen_5000, Batch27002_gen_5000, Batch27003_gen_5000, Batch27004_gen_5000, Batch27005_gen_5000, Batch27006_gen_5000)

Batch28001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch28002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch28003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch28004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch28005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch28006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SJ56SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch28_gen_5000 <- rbind(Batch28001_gen_5000, Batch28002_gen_5000, Batch28003_gen_5000, Batch28004_gen_5000, Batch28005_gen_5000, Batch28006_gen_5000)

Batch29001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch29002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch29003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch29004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch29005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch29006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05NE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch29_gen_5000 <- rbind(Batch29001_gen_5000, Batch29002_gen_5000, Batch29003_gen_5000, Batch29004_gen_5000, Batch29005_gen_5000, Batch29006_gen_5000)

Batch30001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch30002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch30003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch30004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch30005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch30006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK05SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch30_gen_5000 <- rbind(Batch30001_gen_5000, Batch30002_gen_5000, Batch30003_gen_5000, Batch30004_gen_5000, Batch30005_gen_5000, Batch30006_gen_5000)

Batch31001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch31002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch31003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch31004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch31005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch31006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK18SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch31_gen_5000 <- rbind(Batch31001_gen_5000, Batch31002_gen_5000, Batch31003_gen_5000, Batch31004_gen_5000, Batch31005_gen_5000, Batch31006_gen_5000)

Batch32001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch32002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch32003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch32004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch32005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch32006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK79NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch32_gen_5000 <- rbind(Batch32001_gen_5000, Batch32002_gen_5000, Batch32003_gen_5000, Batch32004_gen_5000, Batch32005_gen_5000, Batch32006_gen_5000)

Batch33001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch33002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch33003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch33004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch33005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch33006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/SK98SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch33_gen_5000 <- rbind(Batch33001_gen_5000, Batch33002_gen_5000, Batch33003_gen_5000, Batch33004_gen_5000, Batch33005_gen_5000, Batch33006_gen_5000)

Batch34001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch1/Outputs",min_pop_size = 5)
Batch34002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch2/Outputs",min_pop_size = 5)
Batch34003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch3/Outputs",min_pop_size = 5)
Batch34004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch4/Outputs",min_pop_size = 5)
Batch34005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch5/Outputs",min_pop_size = 5)
Batch34006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TA20SE/Sub_Batch6/Outputs",min_pop_size = 5)
Batch34_gen_5000 <- rbind(Batch34001_gen_5000, Batch34002_gen_5000, Batch34003_gen_5000, Batch34004_gen_5000, Batch34005_gen_5000, Batch34006_gen_5000)

Batch35001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch35002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch35003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch35004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch35005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch35006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF18NW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch35_gen_5000 <- rbind(Batch35001_gen_5000, Batch35002_gen_5000, Batch35003_gen_5000, Batch35004_gen_5000, Batch35005_gen_5000, Batch35006_gen_5000)

Batch36001_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch1/Outputs",min_pop_size = 5)
Batch36002_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch2/Outputs",min_pop_size = 5)
Batch36003_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch3/Outputs",min_pop_size = 5)
Batch36004_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch4/Outputs",min_pop_size = 5)
Batch36005_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch5/Outputs",min_pop_size = 5)
Batch36006_gen_5000 <-Filter_genetics(mypath = "C:/FionaPlenderleith/CH4_models/Riparian_5000/TF39SW/Sub_Batch6/Outputs",min_pop_size = 5)
Batch36_gen_5000 <- rbind(Batch36001_gen_5000, Batch36002_gen_5000, Batch36003_gen_5000, Batch36004_gen_5000, Batch36005_gen_5000, Batch36006_gen_5000)

gen_data_5000 <- rbind(Batch1_gen_5000, Batch2_gen_5000, Batch3_gen_5000, Batch4_gen_5000, Batch5_gen_5000, Batch6_gen_5000, Batch7_gen_5000, Batch8_gen_5000, Batch9_gen_5000, Batch10_gen_5000, Batch11_gen_5000, Batch12_gen_5000, Batch13_gen_5000, Batch14_gen_5000, Batch15_gen_5000, Batch16_gen_5000, Batch17_gen_5000, Batch18_gen_5000, Batch19_gen_5000, Batch20_gen_5000, Batch21_gen_5000, Batch22_gen_5000, Batch23_gen_5000, Batch24_gen_5000, Batch25_gen_5000, Batch26_gen_5000, Batch27_gen_5000, Batch28_gen_5000, Batch29_gen_5000, Batch30_gen_5000, Batch31_gen_5000, Batch32_gen_5000, Batch33_gen_5000, Batch34_gen_5000, Batch35_gen_5000, Batch36_gen_5000)


## Master_5000  ####

mergeCols <- c("Simulation", "Batch", "Land", "Rep", "Year")
Master_data2 <-
  merge(iso_data_5000, disp_data_5000, by = mergeCols, all = TRUE)
Master_data2 <-
  merge(Master_data2, pop_data_5000, by = mergeCols, all = TRUE)
Master_data2 <-
  merge(Master_data2, gen_data_5000, by = mergeCols, all = TRUE)
Master_data2 <-
  left_join(Master_data2, Batchfile_5000, by = c("Simulation", "Batch", "Land"), all = TRUE)



Master_data2 <-
  left_join(Master_data2, All_tree, by = c("Grid"), all = TRUE)

Master_data2$Disp_mort <- 1 - Master_data2$proportion_success_con
Master_data2$Target <- 5000
Master_data <-
  rbind(Master_data, Master_data2)
write.csv(Master_data, "C:/FionaPlenderleith/CH4_models/Analysis/Master_data.csv")












 # repreated for the smaller tree cover area adn combine
# start here if data already sorted ####
Master_data <- read.csv("C:/FionaPlenderleith/CH4_models/Analysis/Master_data")

Master_data$Grid <- factor(Master_data$Grid)
Master_data$generalist <- factor(Master_data$generalist )
Master_data$K <- factor(Master_data$K )
Master_data$HM <- factor(Master_data$HM )
Master_data$Target <- factor(Master_data$Target )





# calculate change in from baseline data set ####
   
   change_in <- Master_data[, c(
                "Grid",
                 "generalist",
                 "landrep",
                 "K",
                 "HM",
                 "Year",
                 "Rep.x",
                 "scenario",
                 "Mean_Distance",
                 "proportion_isolated", 
                 "proportion_isolated_NFI",
                 "pop_size",
                 "Disp_mort",
                 "FST", 
                 "MeanD", 
                "Target")]
   change_in <-  reshape(
     change_in,
     direction = "wide",
     idvar = c(
       "Grid", 
       "generalist", 
       "landrep", 
       "K", 
       "HM", 
       "Year", 
       "Rep.x", 
       "Target"
     ),
     timevar = "scenario"
   )
   
   change_in %>%
     group_by(Grid,
              generalist,
              landrep,
              K,
              HM,
              Year,
              Rep.x, 
              Target)  %>%
     dplyr::summarise(
       proportion_isolated = (proportion_isolated.Scenario1  - proportion_isolated.Scenario0),
       proportion_isolated_NFI = (proportion_isolated_NFI.Scenario1  - proportion_isolated_NFI.Scenario0),
       pop_size = (pop_size.Scenario1  - pop_size.Scenario0),
       Mean_Distance = (Mean_Distance.Scenario1 - Mean_Distance.Scenario0),
       Disp_mort = (Disp_mort.Scenario1  - Disp_mort.Scenario0),
       FST = (FST.Scenario1  - FST.Scenario0),
       MeanD = (MeanD.Scenario1  - MeanD.Scenario0)) -> Scenario1
   Scenario1$Scenario <- "Scenario1"
   change_in %>%
     group_by(Grid,
              generalist,
              landrep,
              K,
              HM,
              Year,
              Rep.x, 
              Target)  %>%
     dplyr::summarise(
       proportion_isolated = (proportion_isolated.Scenario2  - proportion_isolated.Scenario0),
       proportion_isolated_NFI = (proportion_isolated_NFI.Scenario2  - proportion_isolated_NFI.Scenario0),
       pop_size = (pop_size.Scenario2  - pop_size.Scenario0),
       Mean_Distance = (Mean_Distance.Scenario2 - Mean_Distance.Scenario0),
       Disp_mort = (Disp_mort.Scenario2  - Disp_mort.Scenario0),
       FST = (FST.Scenario2  - FST.Scenario0),
       MeanD = (MeanD.Scenario2  - MeanD.Scenario0)) -> Scenario2
   Scenario2$Scenario <- "Scenario2"
   change_in %>%
     group_by(Grid,
              generalist,
              landrep,
              K,
              HM,
              Year,
              Rep.x, 
              Target)  %>%
     dplyr::summarise(
       proportion_isolated = (proportion_isolated.Scenario3  - proportion_isolated.Scenario0),
       proportion_isolated_NFI = (proportion_isolated_NFI.Scenario3  - proportion_isolated_NFI.Scenario0),
       pop_size = (pop_size.Scenario3  - pop_size.Scenario0),
       Mean_Distance = (Mean_Distance.Scenario3 - Mean_Distance.Scenario0),
       Disp_mort = (Disp_mort.Scenario3  - Disp_mort.Scenario0),
       FST = (FST.Scenario3  - FST.Scenario0),
       MeanD = (MeanD.Scenario3  - MeanD.Scenario0)) -> Scenario3
   Scenario3$Scenario <- "Scenario3"
   change_in %>%
     group_by(Grid,
              generalist,
              landrep,
              K,
              HM,
              Year,
              Rep.x, 
              Target)  %>%
     dplyr::summarise(
       proportion_isolated = (proportion_isolated.Scenario4  - proportion_isolated.Scenario0),
       proportion_isolated_NFI = (proportion_isolated_NFI.Scenario4  - proportion_isolated_NFI.Scenario0),
       pop_size = (pop_size.Scenario4  - pop_size.Scenario0),
       Mean_Distance = (Mean_Distance.Scenario4 - Mean_Distance.Scenario0),
       Disp_mort = (Disp_mort.Scenario4  - Disp_mort.Scenario0),
       FST = (FST.Scenario4  - FST.Scenario0),
       MeanD = (MeanD.Scenario4  - MeanD.Scenario0)) -> Scenario4
   Scenario4$Scenario <- "Scenario4"
   
   change_in_final <- rbind(Scenario1, Scenario2, Scenario3, Scenario4)
   
   change_in_final <-
     left_join(change_in_final, All_tree, by = c("Grid"), all = TRUE)
   
  # write.csv(change_in_final, "C:/FionaPlenderleith/CH4_models/Analysis/change_in_final.csv")
  # now include PCA data created in 'landscapePCA' file 
   #change_in_final <- read.csv("E:/Northern_forest/CH4_models/Analysis/change_in_final.csv") # change in data above
   PCA <- read.csv("E:/Northern_forest/CH4_models/Analysis/Updated_land_PCA.csv") #get data from pca 
   change_in_final <- change_in_final[ , -c(1, 18:77)]  
   change_in_final <- merge(change_in_final, PCA, by = "Grid")
   
  # below is code for variance partitioning 
   anova(lm(proportion_isolated ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final))
   
   anova(lm(FST ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final))
   anova(lm(MeanD ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final))
   
   
   anova(lm(Disp_mort ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final))
   anova(lm(FST ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final))
   anova(lm(MeanD ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final))
   
   
   
# plotting outputs for manuscript ####
   
   
# extracting predicted values from anive models ####
   change_in_final_NA <- na.omit(change_in_final)
   change_in_final_NA$Pred_disp <- predict(lm(Disp_mort ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final_NA))
   change_in_final_NA$Pred_FST <- predict(lm(FST ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final_NA))
   change_in_final_NA$Pred_MeanD<- predict(lm(MeanD ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final_NA))
   
   change_in_final_NA$proportion_isolated_pred<- predict(lm(proportion_isolated ~ PC1 + PC2 + PC3 + PC4 + PC5 + Grid + Scenario + Target + landrep + Scenario*Target + generalist + K + HM + generalist*K + generalist*HM + K*HM + Scenario*generalist + Scenario*K + Scenario*HM + Scenario*generalist*Target + Scenario*K*Target + Scenario*HM*Target +  PC1*Scenario + PC2*Scenario + PC3*Scenario + PC4*Scenario + PC5*Scenario+ PC1*landrep + PC2*landrep + PC3*landrep + PC4*landrep + PC5*landrep + PC1*Scenario*Target + PC2*Scenario*Target + PC3*Scenario*Target + PC4*Scenario*Target + PC5*Scenario*Target, data = change_in_final_NA))
   
   
   
   
   
   
   
# Fig 1 ####
   
   
   All <-
     ddply(
       change_in_final_NA ,
       c("Scenario", "Target"),
       summarise,
       disp.N    = length(Pred_disp),
       disp.mean = mean(Pred_disp, na.rm =T),
       disp.sd   = sd(Pred_disp, na.rm = T),
       disp.se   = disp.sd / sqrt(disp.N),
       iso.N    = length(proportion_isolated_pred),
       iso.mean = mean(proportion_isolated_pred, na.rm = T),
       iso.sd   = sd(proportion_isolated_pred, na.rm = T),
       iso.se   = iso.sd / sqrt(iso.N),
       Pred_FST.N    = length(Pred_FST),
       Pred_FST.mean = mean(Pred_FST, na.rm = T),
       Pred_FST.sd   = sd(Pred_FST, na.rm = T),
       Pred_FST.se   = iso.sd / sqrt(Pred_FST.N),
       Pred_MeanD.N    = length(MeanD),
       Pred_MeanD.mean = mean(MeanD, na.rm = T),
       Pred_MeanD.sd   = sd(MeanD, na.rm = T),
       Pred_MeanD.se   = iso.sd / sqrt(Pred_MeanD.N)
     )
   
   All$Scenario <- factor(All$Scenario, levels = c("Expand existing", "Stepping stones", "Corridors", "Riparian corridors"))
   
   #All <- subset(All, All$Scenario != "Riparian corridors")
   
   All$Target[All$Target[] == 5000] <- "12.5 Ha"
   All$Target[All$Target[] == 10000] <- "25 Ha"
   All$Target <- factor(All$Target)
   
   
   
   Iso <- ggplot(All,
                 aes(y =  iso.mean, x = Scenario, colour = Target)) +
     geom_point(size =2) +
     geom_errorbar(aes(ymin = iso.mean - iso.se, ymax = iso.mean + iso.se), width=0.2, size=0.5)+
     guides(
       col = guide_legend("Area of new trees"),
     ) +
     xlab("Scenario") +
     ylab(expression(Delta ~ "Iso patches")) +
     plot_theme + 
     scale_x_discrete(position = "top")+
     ylim(-0.07,- 0.05)
   
   
   disp <- ggplot(All,
                  aes(y =  disp.mean, x = Scenario, colour = Target)) +
     geom_point(size =2) +
     geom_errorbar(aes(ymin = disp.mean - disp.se, ymax = disp.mean + disp.se), width=0.2, size=0.5)+
     guides(
       col = guide_legend("Area of new trees"),
     ) +
     xlab("Scenario") +
     ylab(expression(Delta ~ "Disp mortality")) + 
     scale_x_discrete(position = "top")+
     plot_theme   + 
     ylim(-0.13,-0.09)
   
   
   Pred_FST_g <- ggplot(All,
                        aes(y =  Pred_FST.mean, x = Scenario, colour = Target)) +
     geom_point(size =2) +  
     geom_errorbar(aes(ymin = Pred_FST.mean - Pred_FST.se, ymax = Pred_FST.mean + Pred_FST.se), width=0.2, size=0.5)+
     guides(
       col = guide_legend("Area of new trees"),
     ) +
     xlab("Scenario") +
     ylab(expression(Delta ~ "FST")) +
     plot_theme + 
     scale_x_discrete(position = "top")+
     ylim(-0.04,-0.025)
   
   ggarrange(
     Iso,
     disp + rremove("x.text") + rremove("xlab") + rremove("x.ticks"),
     Pred_FST_g + rremove("x.text") + rremove("xlab") + rremove("x.ticks"),
     common.legend = T,
     legend = "right",
     ncol = 1,
     heights = c(1.3, 1, 1),
     widths = c(1.2,.7,.9)

   )
   
   
# Fig 2 ####
   
   All_HM <-
     ddply(
       change_in_final_NA ,
       c("Scenario", "HM"),
       summarise,
       disp.N    = length(Pred_disp),
       disp.mean = mean(Pred_disp, na.rm =T),
       disp.sd   = sd(Pred_disp, na.rm = T),
       disp.se   = disp.sd / sqrt(disp.N),
       iso.N    = length(proportion_isolated_pred),
       iso.mean = mean(proportion_isolated_pred, na.rm = T),
       iso.sd   = sd(proportion_isolated_pred, na.rm = T),
       iso.se   = iso.sd / sqrt(iso.N),
       FST.N    = length(Pred_FST),
       FST.mean = mean(Pred_FST, na.rm = T),
       FST.sd   = sd(Pred_FST, na.rm = T),
       FST.se   = iso.sd / sqrt(FST.N),
       MeanD.N    = length(Pred_MeanD),
       MeanD.mean = mean(Pred_MeanD, na.rm = T),
       MeanD.sd   = sd(Pred_MeanD, na.rm = T),
       MeanD.se   = iso.sd / sqrt(MeanD.N)
     )
   All_HM$Scenario <- factor(All_HM$Scenario, levels = c("Expand existing", "Stepping stones", "Corridors", "Riparian corridors"))

   All_HM$HM <- factor(All_HM$HM)
   
   All_K <-
     ddply(
       change_in_final_NA ,
       c("Scenario", "K"),
       summarise,
       disp.N    = length(Pred_disp),
       disp.mean = mean(Pred_disp, na.rm =T),
       disp.sd   = sd(Pred_disp, na.rm = T),
       disp.se   = disp.sd / sqrt(disp.N),
       iso.N    = length(proportion_isolated_pred),
       iso.mean = mean(proportion_isolated_pred, na.rm = T),
       iso.sd   = sd(proportion_isolated_pred, na.rm = T),
       iso.se   = iso.sd / sqrt(iso.N),
       FST.N    = length(Pred_FST),
       FST.mean = mean(Pred_FST, na.rm = T),
       FST.sd   = sd(Pred_FST, na.rm = T),
       FST.se   = iso.sd / sqrt(FST.N),
       MeanD.N    = length(Pred_MeanD),
       MeanD.mean = mean(Pred_MeanD, na.rm = T),
       MeanD.sd   = sd(Pred_MeanD, na.rm = T),
       MeanD.se   = iso.sd / sqrt(MeanD.N)
     )
   All_K$Scenario <- factor(All_K$Scenario, levels = c("Expand existing", "Stepping stones", "Corridors", "Riparian corridors"))

   All_K$K <- factor(All_K$K)
   
   
   All_generalist <-
     ddply(
       change_in_final_NA ,
       c("Scenario", "generalist"),
       summarise,
       disp.N    = length(Pred_disp),
       disp.mean = mean(Pred_disp, na.rm =T),
       disp.sd   = sd(Pred_disp, na.rm = T),
       disp.se   = disp.sd / sqrt(disp.N),
       iso.N    = length(proportion_isolated_pred),
       iso.mean = mean(proportion_isolated_pred, na.rm = T),
       iso.sd   = sd(proportion_isolated_pred, na.rm = T),
       iso.se   = iso.sd / sqrt(iso.N),
       FST.N    = length(Pred_FST),
       FST.mean = mean(Pred_FST, na.rm = T),
       FST.sd   = sd(Pred_FST, na.rm = T),
       FST.se   = iso.sd / sqrt(FST.N),
       MeanD.N    = length(Pred_MeanD),
       MeanD.mean = mean(Pred_MeanD, na.rm = T),
       MeanD.sd   = sd(Pred_MeanD, na.rm = T),
       MeanD.se   = iso.sd / sqrt(MeanD.N)
     )
   All_generalist$Scenario <- factor(All_generalist$Scenario, levels = c("Expand existing", "Stepping stones", "Corridors", "Riparian corridors"))
  
   All_generalist$generalist <- factor(All_generalist$generalist)
   + 
     ylim(0.0, .1)
   
   
   disp_HM <- ggplot(All_HM,
                     aes(y =  disp.mean, x = HM, colour = Scenario)) +
     geom_point(size =1) +
     geom_errorbar(aes(ymin = disp.mean - disp.se, ymax = disp.mean + disp.se), width=0.5, size=0.5)+
     guides(
       col = guide_legend("Scenario"),
     ) +
     xlab("Habitat mortality (HM)") +
     ylab(expression(Delta ~ "Disp mort")) +
     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
     plot_theme  +
     ylim(-0.08, -0.13)
   
   iso_HM <- ggplot(All_HM,
                    aes(y =  iso.mean, x = HM, colour = Scenario)) +
     geom_point(size =1) +
     geom_errorbar(aes(ymin = iso.mean - iso.se, ymax = iso.mean + iso.se), width=0.5, size=0.5)+
     guides(
       col = guide_legend("Scenario"),
     ) +
     xlab("Habitat mortality (HM)") +
     ylab(expression(Delta~ "Iso patches")) +
     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
     plot_theme   +
     ylim(-0.04, -0.08)
   
   FST_HM <- ggplot(All_HM,
                    aes(y =  FST.mean, x = HM, colour = Scenario)) +
     geom_point(size =1)  +
     geom_errorbar(aes(ymin = FST.mean - FST.se, ymax = FST.mean + FST.se), width=0.5, size=0.5)+
     guides(
       col = guide_legend("Scenario"),
     ) +
     xlab("Habitat mortality (HM)") +
     ylab(expression(Delta ~ "gen connectivity")) +
     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
     plot_theme+
     ylim(-0.01, -0.06)
   
   
   disp_K <- ggplot(All_K,
                    aes(y =  disp.mean, x = K, colour = Scenario)) +
     geom_point(size =1)  +
     geom_errorbar(aes(ymin = disp.mean - disp.se, ymax = disp.mean + disp.se), width=0.5, size=0.5)+
     guides(
       col = guide_legend("Scenario"),
     ) +
     xlab("Carrying capacity (K)") +
     ylab(expression(Delta ~ "Disp mort")) +
     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
     plot_theme  +
     ylim(-0.08, -0.13)
   
   iso_K <- ggplot(All_K,
                   aes(y =  iso.mean, x = K, colour = Scenario)) +
     geom_point(size =1) +
     geom_errorbar(aes(ymin = iso.mean - iso.se, ymax = iso.mean + iso.se), width=0.5, size=0.5)+
     guides(
       col = guide_legend("Scenario"),
     ) +
     xlab("Carrying capacity (K)") +
     ylab(expression(Delta~ "Iso patches")) +
     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
     plot_theme   +
     ylim(-0.04, -0.08)
   
   FST_K <- ggplot(All_K,
                   aes(y =  FST.mean, x = K, colour = Scenario)) +
     geom_point(size =1)  +
     geom_errorbar(aes(ymin = FST.mean - FST.se, ymax = FST.mean + FST.se), width=0.5, size=0.5)+
     guides(
       col = guide_legend("Scenario"),
     ) +
     xlab("Carrying capacity (K)") +
     ylab(expression(Delta ~ "FST")) +
     plot_theme + 
     scale_x_discrete(position = "top")+
     ylim(-0.06, -0.01)
   
   
   
   
   disp_generalist <- ggplot(All_generalist,
                             aes(y =  disp.mean, x = generalist, colour = Scenario)) +
     geom_point(size =1)  +
     geom_errorbar(aes(ymin = disp.mean - disp.se, ymax = disp.mean + disp.se), width=0.5, size=0.5)+
     guides(
       col = guide_legend("Scenario"),
     ) +
     xlab("Tree preference") +
     ylab(expression(Delta ~ "Disp mort")) +
     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
     plot_theme   +
     ylim(-0.08, -0.13)
   
   iso_generalist <- ggplot(All_generalist,
                            aes(y =  iso.mean, x = generalist, colour = Scenario)) +
     geom_point(size =1)  +
     geom_errorbar(aes(ymin = iso.mean - iso.se, ymax = iso.mean + iso.se), width=0.5, size=0.5)+
     guides(
       col = guide_legend("Scenario"),
     ) +
     xlab("Tree preference") +
     ylab(expression(Delta~ "Iso patches")) +
     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
     plot_theme  +
     ylim(-0.04, -0.08) 
   
   FST_generalist <- ggplot(All_generalist,
                            aes(y =  FST.mean, x = generalist, colour = Scenario)) +
     geom_point(size =1)  +
     geom_errorbar(aes(ymin = FST.mean - FST.se, ymax = FST.mean + FST.se), width=0.5, size=0.5)+
     guides(
       col = guide_legend("Scenario"),
     ) +
     xlab("Generalism") +
     ylab(expression(Delta ~ "FST")) +
     plot_theme + 
     scale_x_discrete(position = "top")+
     ylim(-0.06, -0.01)
   
   ggarrange(
     disp_generalist,
     iso_generalist,
     FST_generalist,
     disp_K,
     iso_K,
     FST_K,
     disp_HM,
     iso_HM,
     FST_HM,
     common.legend = T,
     legend = "right",
     ncol = 3,
     nrow = 3,
     font.label = list(size = 10, color = "black", face = "plain")
   )
   
   
   
   ggarrange(
     FST_generalist,
     FST_K,
     common.legend = T,
     legend = "right",
     ncol = 2,
     font.label = list(size = 10, color = "black", face = "plain")
   )
   
# Fig 3 ####
   
   All <-
     ddply(
       change_in_final_NA,
       c("PC1"),
       summarise,
       disp.N    = length(Pred_disp),
       disp.mean = mean(Pred_disp, na.rm =T),
       disp.sd   = sd(Pred_disp, na.rm = T),
       disp.se   = disp.sd / sqrt(disp.N),
       iso.N    = length(proportion_isolated),
       iso.mean = mean(proportion_isolated, na.rm = T),
       iso.sd   = sd(proportion_isolated, na.rm = T),
       iso.se   = iso.sd / sqrt(iso.N),
       FST.N    = length(Pred_FST),
       FST.mean = mean(FST, na.rm = T),
       FST.sd   = sd(Pred_FST, na.rm = T),
       FST.se   = iso.sd / sqrt(FST.N),
       MeanD.N    = length(Pred_MeanD),
       MeanD.mean = mean(Pred_MeanD, na.rm = T),
       MeanD.sd   = sd(Pred_MeanD, na.rm = T),
       MeanD.se   = iso.sd / sqrt(MeanD.N)
     )
   
   Pred_FST <- ggplot(All,
                      aes(x = PC1, y =  FST.mean)) +
     geom_point(size = 1) +
     # facet_grid(generalist~K)+
     #geom_smooth(aes(linetype = Target, fill = Scenario), se = T)+
     plot_theme +
     geom_vline(xintercept = 0, linetype = "dotted")+
     labs(y = expression(Delta ~ "FST"))+
     ylim(-0.1, 0 )  +
     plot_theme + 
     scale_x_continuous(position = "top")
   
   All_PC2 <-
     ddply(
       change_in_final_NA,
       c("PC2"),
       summarise,
       disp.N    = length(Pred_disp),
       disp.mean = mean(Pred_disp, na.rm =T),
       disp.sd   = sd(Pred_disp, na.rm = T),
       disp.se   = disp.sd / sqrt(disp.N),
       iso.N    = length(proportion_isolated),
       iso.mean = mean(proportion_isolated, na.rm = T),
       iso.sd   = sd(proportion_isolated, na.rm = T),
       iso.se   = iso.sd / sqrt(iso.N),
       FST.N    = length(Pred_FST),
       FST.mean = mean(FST, na.rm = T),
       FST.sd   = sd(Pred_FST, na.rm = T),
       FST.se   = iso.sd / sqrt(FST.N),
       MeanD.N    = length(Pred_MeanD),
       MeanD.mean = mean(Pred_MeanD, na.rm = T),
       MeanD.sd   = sd(Pred_MeanD, na.rm = T),
       MeanD.se   = iso.sd / sqrt(MeanD.N)
     )
   
   ISO <- ggplot(All_PC2,
                 aes(x = PC2, y =  iso.mean)) +
     geom_point(size = 1) +
     # facet_grid(generalist~K)+
     #geom_smooth(aes(linetype = Target, fill = Scenario), se = T)+
     plot_theme +
     # geom_hline(yintercept = 0, linetype = "dotted")+
     geom_vline(xintercept = 0, linetype = "dotted")+
     labs(y = expression(Delta ~ "iso prop"))+
     ylim(-0.17, 0) +
     plot_theme + 
     scale_x_continuous(position = "top")
   
   ggarrange(
     ISO,
     Pred_FST,
     common.legend = TRUE,
     legend = "right",
     ncol = 1,
     font.label = list(size = 10, color = "black", face = "plain")
   )
   