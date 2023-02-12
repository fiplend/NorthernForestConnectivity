library(stringr)
library(raster)
library(graph4lg)
library(data.table)


# these functions run through the output folders of raw data and extract data on isolated patches, dispersal motality, poulation size, genertics and distance moved. 
# patch = lcoaiton of baseline patch map
# my path = path to output folers
# mypattern = which RS output e.g. con.txt
# min_pop_size = which pops to remove for genetics
# Isolated_patch_5000 is need as patch map is differenc 

#without ripiaran landscapes ####
Isolated_patch <- function(patch, mypath, mypattern, ...) {
  
  Outputpatch <- paste(mypath, "Outputs", sep = "")
  setwd(Outputpatch)
  library(dplyr)
  import.multiple.files<-function(Outputpatch,mypattern,...)
  {
    setwd(Outputpatch)
    tmp.list.1<-list.files(Outputpatch, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(Outputpatch = Outputpatch,
                          mypattern = mypattern,
                          sep = "\t")
  
  filenames <-
    list.files(Outputpatch, pattern = mypattern)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  Batch_update <- Map(cbind, files, Simulation = Sim)
  Batch_update <- Map(cbind, Batch_update, Batch = Batch)
  Batch_update <- Map(cbind, Batch_update, Land = Land)
  combined <- bind_rows(Batch_update, .id = "column_label")
 
  
    # calculate distance between all patches
  batch <- unique(combined$Batch) # which batch
  Patchfile <- read.csv("C:/FionaPlenderleith/CH4_models/Analysis/Patchfile.csv") #file with details 
  Patchfile <- subset(Patchfile, Patchfile$Batch == batch) #subset for teh abtch
  patchmaps <- unique(Patchfile$Patch_map) # now whch patch maps are using
  
 
  p_distance <-  function(patchmap){
      
    inputpath <- paste(mypath, "Inputs", sep = "")
    setwd(inputpath)
    patch_raster <- raster(patchmap) #import patch map
    p <- data.frame(rasterToPoints(patch_raster))
    name <- substr(patchmap, 1, nchar(patchmap)-4) 
    p <- p %>% dplyr::rename(PatchID = name)
    # filter out background
    p <- p[p$PatchID != 0,]
    p %>%
      group_by(PatchID)%>%
      dplyr::summarise(
        x = mean(x, na.rm=T),
        y = mean(y, na.rm=T)
      ) -> p
    p <- as.data.frame(p)
    real_raw <- mat_geo_dist(data = p, ID = "PatchID", x = "x", y = "y")
    real_raw <- as.data.frame(real_raw)
    setDT(real_raw, keep.rownames = TRUE)[]
    colnames(real_raw)[1] <- "StartPatch"
    real_raw$StartPatch <- as.factor(real_raw$StartPatch)
    # index for lower triangular matrix
    # real_ind <- lower.tri(real_raw, diag = TRUE)
    # # select values of interest and replace rest with NAs
    # real_tri <- real_raw
    # real_tri[real_ind == FALSE] <- NA
    
    real_long <-
      melt(
        data = real_raw,
        id.vars = "StartPatch",
        variable.name = "EndPatch",
        value.name = "Distance",
        na.rm = TRUE
      )
    real_long <- as.data.frame(real_long)
    real_long$Patch_map <- patchmap
    real_long <- subset(real_long, real_long$StartPatch != real_long$EndPatch)
    return(real_long)
  } #funciton to calc distance
  
  patch1 <- p_distance(patchmap = patchmaps[1])
  patch2 <- p_distance(patchmap = patchmaps[2])
  patch3 <- p_distance(patchmap = patchmaps[3])
  patch4 <- p_distance(patchmap = patchmaps[4])
  #patch5 <- p_distance(patchmap = patchmaps[5])
  patch_stats <- rbind(patch1, patch2, patch3, patch4) #distances between patchess
  patch_stats$StartPatch <- factor(patch_stats$StartPatch)
  patch_stats$EndPatch <- factor(patch_stats$EndPatch)

  combined_distance <- left_join(combined, Patchfile, by = c("Batch", "Simulation", "Land"))
  combined_distance$StartPatch <- factor(combined_distance$StartPatch)
  combined_distance$EndPatch <- factor(combined_distance$EndPatch)
  combined_distance <- left_join(combined_distance, patch_stats, by = c("StartPatch", "EndPatch", "Patch_map"))
  combined_distance <- subset(combined_distance, combined_distance$StartPatch != -999) #sum rows
  combined_distance <- subset(combined_distance, combined_distance$StartPatch != -888)
  combined_distance <- subset(combined_distance, combined_distance$EndPatch != -999) #sum rows
  combined_distance <- subset(combined_distance, combined_distance$EndPatch != -888)
  
  combined_distance <- combined_distance %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      Mean_Distance = sum(Distance)/sum(Ninds)
      )
  
    # now we want to merge distances 
  
  combined <- subset(combined, combined$StartPatch == -999) #sum rows
  combined <- subset(combined, combined$StartPatch != -888)#sum rows
  combined <- subset(combined, combined$Ninds == 0) # only patches receiving 0 
  number_isolated_patches <- combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      isolated_patch_count = n_distinct(EndPatch))
  
  
  # filter for only large patches ####
  library(dplyr)
  
  inputpath <- paste(mypath, "Inputs", sep = "")
  setwd(inputpath)
  patch_raster <- raster(patch) 
  patch_frequency <- as.data.frame(freq(patch_raster)) #calculate freq
  patch_frequency_NFI <- subset(patch_frequency, patch_frequency$count>200) #subset to Ha
  patch_frequency_NFI <- subset(patch_frequency_NFI, patch_frequency_NFI$value != 0) #remove matric cells
  patch_frequency_NFI <- subset(patch_frequency_NFI, patch_frequency_NFI$value < 9000) #remove planted
  NFIpatches <- combined %>%
    filter(EndPatch %in% patch_frequency_NFI$value) #filter isolated patches by NFI only
  
  number_isolated_NFIpatches<- NFIpatches %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(isolated_NFIpatch_count = n_distinct(EndPatch))
  

  
  mergeCols <- c("Simulation", "Batch", "Land", "Rep", "Year")
  isolated_patch <- merge(number_isolated_NFIpatches, number_isolated_patches, by = mergeCols, all = TRUE)
  isolated_patch <- merge(isolated_patch, combined_distance, by = mergeCols, all = TRUE)
  
  isolated_patch$isolated_NFIpatch_count[is.na(isolated_patch$isolated_NFIpatch_count)] <- 0 
  
  isolated_patch$totalNFI <- length(patch_frequency_NFI$value)
  isolated_patch$proportion_isolated_NFI  <- isolated_patch$isolated_NFIpatch_count / isolated_patch$totalNFI
  isolated_patch$total_patch <- length(patch_frequency$value)
  isolated_patch$proportion_isolated  <- isolated_patch$isolated_patch_count / isolated_patch$total_patch
  
  isolated_patch <- subset(isolated_patch, select = - c(total_patch, totalNFI))
  

  
  return(isolated_patch)
}
Individual_summary <- function(landscape, mypath_input, mypath_output, mypattern, ...) {
  library(raster)
  library(dplyr)
  library(stringr)
  
  setwd(mypath_input)
  # first step - define patches as stepping stones and woodlands
  Patch <- raster(landscape) #import patch map
  Patch_frequency <- as.data.frame(freq(Patch)) #calculate freq
  Natal_patch_frequency <- Patch_frequency
  Natal_patch_frequency <- subset(Natal_patch_frequency, Natal_patch_frequency$value != 0)
  Natal_patch_frequency$Natal_patch <- Natal_patch_frequency$value
  Natal_patch_frequency$Natal_patch_type <- NA
  Natal_patch_frequency$Natal_patch_type[Natal_patch_frequency$count >=
                                           200] <-  "Woodland"
  Natal_patch_frequency$Natal_patch_type[Natal_patch_frequency$count < 200] <-
    "Steppingstone"
  Natal_patch_frequency <- Natal_patch_frequency[c("Natal_patch", "Natal_patch_type")]
  
  
  
  Focal_patch_frequency <- Patch_frequency
  Focal_patch_frequency$PatchID <- Focal_patch_frequency$value
  Focal_patch_frequency$Focal_patch_type <- NA
  Focal_patch_frequency$Focal_patch_type[Focal_patch_frequency$count >=
                                           200] <-  "Woodland"
  Focal_patch_frequency$Focal_patch_type[Focal_patch_frequency$count < 200] <-
    "Steppingstone"
  Focal_patch_frequency$Focal_patch_type[Focal_patch_frequency$value == 0] <-
    "matrix"  
  Focal_patch_frequency <- Focal_patch_frequency[c("PatchID", "Focal_patch_type")]
  setwd(mypath_output)
  library(dplyr)
  import.multiple.files<-function(mypath_output,mypattern,...)
  {
    setwd(mypath_output)
    tmp.list.1<-list.files(mypath_output, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(mypath = mypath_output,
                          mypattern = mypattern,
                          sep = "\t")
  
  filenames <-
    list.files(mypath_output, pattern = mypattern)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  combined <- Map(cbind, files, Simulation = Sim)
  combined <- Map(cbind, combined, Batch = Batch)
  combined <- Map(cbind, combined, Land = Land)
  combined <- bind_rows(combined, .id = "column_label")
  combined <-  merge(combined, Focal_patch_frequency, by = "PatchID", all = TRUE)
  combined <-  merge(combined, Natal_patch_frequency, by = "Natal_patch", all = TRUE)
  # # 
  # 
  
  
  # stepping stone - stepping stone patches analysis
  non_dispersers <- subset(combined, combined$Nsteps == 0)
  dispersers <- subset(combined, combined$Nsteps > 0)
  dispersers_stepping <- subset(dispersers, dispersers$Natal_patch_type == "Steppingstone")
  
  Success_stepping_Woodland <-subset(dispersers_stepping, dispersers_stepping$PatchID != 0)
  Success_stepping_Woodland <-subset(Success_stepping_Woodland, Success_stepping_Woodland$Focal_patch_type == "Woodland")
  Success_stepping_Stepping <- subset(dispersers_stepping, dispersers_stepping$PatchID != 0)
  Success_stepping_Stepping <-subset(Success_stepping_Stepping, Success_stepping_Stepping$Focal_patch_type == "Steppingstone")
  Fails_stepping <- subset(dispersers_stepping, dispersers_stepping$PatchID == 0)
  
  Summary.success_stepping_woodland <- Success_stepping_Woodland %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.success_stepping_woodland$Success <- "Yes"
  Summary.success_stepping_Stepping <- Success_stepping_Stepping %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.success_stepping_Stepping$Success <- "Yes"
  Summary.fails_stepping <- Fails_stepping %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.fails_stepping$Success <- "No"
  
  # woodland patches analysis
  
  dispersers_Woodland <- subset(dispersers, dispersers$Natal_patch_type == "Woodland")
  
  Success_Woodland_Woodland <-subset(dispersers_Woodland, dispersers_Woodland$PatchID != 0)
  Success_Woodland_Woodland <-subset(Success_Woodland_Woodland, Success_Woodland_Woodland$Focal_patch_type == "Woodland")
  Success_Woodland_Stepping <- subset(dispersers_Woodland, dispersers_stepping$PatchID != 0)
  Success_Woodland_Stepping <-subset(Success_Woodland_Stepping, Success_Woodland_Stepping$Focal_patch_type == "Steppingstone")
  Fails_Woodland <- subset(dispersers_Woodland, dispersers_Woodland$PatchID == 0)
  
  Summary.success_Woodland_woodland <- Success_Woodland_Woodland %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.success_Woodland_woodland$Success <- "Yes"
  Summary.success_Woodland_Stepping <- Success_Woodland_Stepping %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.success_Woodland_Stepping$Success <- "Yes"
  Summary.fails_Woodland <- Fails_Woodland %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.fails_Woodland$Success <- "No"
  
  
  Summary.non_dispersers <-
    non_dispersers %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise(
      # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  
  Summary.success_stepping_woodland$Natal_patch_type <-
    "Steppingstone"           
  Summary.success_stepping_Stepping$Natal_patch_type <-
    "Steppingstone"           
  Summary.fails_stepping$Natal_patch_type <-
    "Steppingstone"           
  Summary.success_Woodland_woodland$Natal_patch_type <- "Woodland"
  Summary.success_Woodland_Stepping$Natal_patch_type <- "Woodland"
  Summary.fails_Woodland$Natal_patch_type <- "Woodland"
  Summary.success_stepping_woodland$Focal_patch_type <-
    "Woodland"       
  Summary.success_stepping_Stepping$Focal_patch_type <-
    "Steppingstone"           
  Summary.fails_stepping$Focal_patch_type <- "Matrix" 
  
  Summary.success_Woodland_woodland$Focal_patch_type <- "Woodland"
  Summary.success_Woodland_Stepping$Focal_patch_type <- "Steppingstone" 
  Summary.fails_Woodland$Focal_patch_type <- "Matrix" 
  
  #sort this
  Connect_summary <-
    rbind(
      Summary.non_dispersers,
      Summary.success_stepping_woodland,
      Summary.success_stepping_Stepping,
      Summary.fails_stepping,
      Summary.success_Woodland_woodland,
      Summary.success_Woodland_Stepping,
      Summary.fails_Woodland
    )
  
  return(Connect_summary)
  
}
Pop_size <- function(mypath, mypattern, ...) {
  setwd(mypath)
  library(dplyr)
  library(stringr)
  import.multiple.files<-function(mypath,mypattern,...)
  {
    setwd(mypath)
    tmp.list.1<-list.files(mypath, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(mypath = mypath,
                          mypattern = mypattern,
                          sep = "\t")
  
  filenames <-
    list.files(mypath, pattern = mypattern)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  Batch_update <- Map(cbind, files, Simulation = Sim)
  Batch_update <- Map(cbind, Batch_update, Batch = Batch)
  Batch_update <- Map(cbind, Batch_update, Land = Land)
  combined <- bind_rows(Batch_update, .id = "column_label")
  
  
  population_size <- combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      pop_size = sum(NInd))
  
  return(population_size)
}
Filter_genetics <- function(min_pop_size, mypath, ...) {
  setwd(mypath)
  library(dplyr)
  import.multiple.files<-function(mypath,pattern,...)
  {
    setwd(mypath)
    tmp.list.1<-list.files(mypath, pattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  pop_files <-
    import.multiple.files(mypath = mypath,
                          pattern = "Pop.txt",
                          sep = "\t")
  pop_files <- pop_files[sapply(pop_files, function(x) dim(x)[1]) > 0]
  pop_filenames <-
    list.files(mypath, pattern = "Pop.txt")
  pop_filenames <- as.data.frame(pop_filenames)
  pop_filenames <-
    as.data.frame(str_split_fixed(pop_filenames$pop_filenames, "_", 4))
  pop_Batch <- unname(unlist(pop_filenames["V1"]))
  pop_Sim <- unname(unlist(pop_filenames["V2"]))
  pop_Land <- unname(unlist(pop_filenames["V3"]))
  Pop <- Map(cbind, pop_files, Simulation = pop_Sim)
  Pop <- Map(cbind, Pop, Batch = pop_Batch)
  Pop <- Map(cbind, Pop, Land = pop_Land)
  Population <- bind_rows(Pop, .id = "column_label")
 
 
  
  gen_files <-
    import.multiple.files(mypath = mypath,
                          pattern = "LandGen.txt",
                          sep = "\t")
  gen_filenames <-
    list.files(mypath, pattern = "LandGen.txt")
  gen_filenames <- as.data.frame(gen_filenames)
  gen_filenames <-
    as.data.frame(str_split_fixed(gen_filenames$gen_filenames, "_", 4))
  gen_Batch <- unname(unlist(gen_filenames["V1"]))
  gen_Sim <- unname(unlist(gen_filenames["V2"]))
  gen_Land <- unname(unlist(gen_filenames["V3"]))
  gen <- Map(cbind, gen_files, Simulation = gen_Sim)
  gen <- Map(cbind, gen, Batch = gen_Batch)
  gen <- Map(cbind, gen, Land = gen_Land)
  Genetics <- bind_rows(gen, .id = "column_label")
  
  Genetics$PatchID0_size <- Population[match(paste(Genetics$Rep, Genetics$Year, Genetics$Simulation, Genetics$Batch, Genetics$Land, Genetics$PatchID0),paste(Population$Rep, Population$Year, Population$Simulation, Population$Batch, Population$Land, Population$PatchID)),"NInd"]
  Genetics$PatchID1_size <- Population[match(paste(Genetics$Rep, Genetics$Year, Genetics$Simulation, Genetics$Batch, Genetics$Land, Genetics$PatchID1),paste(Population$Rep, Population$Year, Population$Simulation, Population$Batch, Population$Land, Population$PatchID)),"NInd"]
  
  
  Genetics <- subset(Genetics, Genetics$PatchID0_size > min_pop_size)
  Genetics <- subset(Genetics, Genetics$PatchID1_size > min_pop_size)
  
  Genetics$FST[Genetics$FST[] == -999] <- "NA"
  Genetics$FST[Genetics$FST < 0] <- 0
  Genetics$FST <- as.numeric(Genetics$FST)
  
  Genetics_summary <- Genetics %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      Distance = mean(Distance, na.rm =T),
      MeanNAlleles = mean(MeanNAlleles, na.rm =T),
      FIS = mean(FIS, na.rm =T),
      FST = mean(FST, na.rm =T),
      MeanD = mean(MeanD, na.rm =T),
      HarmMeanD = mean(HarmMeanD, na.rm =T)
    )
  
  
  return(Genetics_summary)
  }
Dispersal_summary_con <- function(mypath_output, mypattern, ...) {
  
  
  setwd(mypath_output)
  #read in all ind files 
  import.multiple.files<-function(mypath_output,mypattern,...)
  {
    setwd(mypath_output)
    tmp.list.1<-list.files(mypath_output, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(mypath = mypath_output,
                          mypattern = mypattern,
                          sep = "\t")
  files <- files[sapply(files, function(x) dim(x)[1]) > 0]
  filenames <-
    names(files)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  
  combined <- Map(cbind, files, Simulation = Sim)
  combined <- Map(cbind, combined, Batch = Batch)
  combined <- Map(cbind, combined, Land = Land)
  combined <- bind_rows(combined, .id = "column_label")

  
  combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    subset(EndPatch == -888) %>%
    dplyr::summarise( 
      Nind_em_all = sum(Ninds)
    ) -> Emi
  combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    subset(EndPatch == -999) %>%
    dplyr::summarise( 
      Nind_em = sum(Ninds)
    ) -> Imi
  
  Final_data <-
    merge(
      Emi,
      Imi,
      by = c("Simulation", "Batch", "Land", "Rep", "Year"),
      all = TRUE
    )
  
  Final_data$proportion_success_con <- Final_data$Nind_em  / Final_data$Nind_em_all
  
  
  
  return(Final_data)
}
Isolated_patch_5000 <- function(patch, mypath, mypattern, ...) {
  
  Outputpatch <- paste(mypath, "Outputs", sep = "")
  setwd(Outputpatch)
  library(dplyr)
  import.multiple.files<-function(Outputpatch,mypattern,...)
  {
    setwd(Outputpatch)
    tmp.list.1<-list.files(Outputpatch, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(Outputpatch = Outputpatch,
                          mypattern = mypattern,
                          sep = "\t")
  
  filenames <-
    list.files(Outputpatch, pattern = mypattern)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  Batch_update <- Map(cbind, files, Simulation = Sim)
  Batch_update <- Map(cbind, Batch_update, Batch = Batch)
  Batch_update <- Map(cbind, Batch_update, Land = Land)
  combined <- bind_rows(Batch_update, .id = "column_label")
  
  
  # calculate distance between all patches
  batch <- unique(combined$Batch) # which batch
  Patchfile <- read.csv("C:/FionaPlenderleith/CH4_models/Analysis/Patchfile_5000.csv") #file with details 
  Patchfile <- subset(Patchfile, Patchfile$Batch == batch) #subset for teh abtch
  patchmaps <- unique(Patchfile$Patch_map) # now whch patch maps are using
  
  
  p_distance <-  function(patchmap){
    
    inputpath <- paste(mypath, "Inputs", sep = "")
    setwd(inputpath)
    patch_raster <- raster(patchmap) #import patch map
    p <- data.frame(rasterToPoints(patch_raster))
    name <- substr(patchmap, 1, nchar(patchmap)-4) 
    p <- p %>% dplyr::rename(PatchID = name)
    # filter out background
    p <- p[p$PatchID != 0,]
    p %>%
      group_by(PatchID)%>%
      dplyr::summarise(
        x = mean(x, na.rm=T),
        y = mean(y, na.rm=T)
      ) -> p
    p <- as.data.frame(p)
    real_raw <- mat_geo_dist(data = p, ID = "PatchID", x = "x", y = "y")
    real_raw <- as.data.frame(real_raw)
    setDT(real_raw, keep.rownames = TRUE)[]
    colnames(real_raw)[1] <- "StartPatch"
    real_raw$StartPatch <- as.factor(real_raw$StartPatch)
    # index for lower triangular matrix
    # real_ind <- lower.tri(real_raw, diag = TRUE)
    # # select values of interest and replace rest with NAs
    # real_tri <- real_raw
    # real_tri[real_ind == FALSE] <- NA
    
    real_long <-
      melt(
        data = real_raw,
        id.vars = "StartPatch",
        variable.name = "EndPatch",
        value.name = "Distance",
        na.rm = TRUE
      )
    real_long <- as.data.frame(real_long)
    real_long$Patch_map <- patchmap
    real_long <- subset(real_long, real_long$StartPatch != real_long$EndPatch)
    return(real_long)
  } #funciton to calc distance
  
  patch1 <- p_distance(patchmap = patchmaps[1])
  patch2 <- p_distance(patchmap = patchmaps[2])
  patch3 <- p_distance(patchmap = patchmaps[3])
  patch4 <- p_distance(patchmap = patchmaps[4])
  #patch5 <- p_distance(patchmap = patchmaps[5])
  patch_stats <- rbind(patch1, patch2, patch3, patch4) #distances between patchess
  patch_stats$StartPatch <- factor(patch_stats$StartPatch)
  patch_stats$EndPatch <- factor(patch_stats$EndPatch)
  
  combined_distance <- left_join(combined, Patchfile, by = c("Batch", "Simulation", "Land"))
  combined_distance$StartPatch <- factor(combined_distance$StartPatch)
  combined_distance$EndPatch <- factor(combined_distance$EndPatch)
  combined_distance <- left_join(combined_distance, patch_stats, by = c("StartPatch", "EndPatch", "Patch_map"))
  combined_distance <- subset(combined_distance, combined_distance$StartPatch != -999) #sum rows
  combined_distance <- subset(combined_distance, combined_distance$StartPatch != -888)
  combined_distance <- subset(combined_distance, combined_distance$EndPatch != -999) #sum rows
  combined_distance <- subset(combined_distance, combined_distance$EndPatch != -888)
  
  combined_distance <- combined_distance %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      Mean_Distance = sum(Distance)/sum(Ninds)
    )
  
  # now we want to merge distances 
  
  combined <- subset(combined, combined$StartPatch == -999) #sum rows
  combined <- subset(combined, combined$StartPatch != -888)#sum rows
  combined <- subset(combined, combined$Ninds == 0) # only patches receiving 0 
  number_isolated_patches <- combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      isolated_patch_count = n_distinct(EndPatch))
  
  
  # filter for only large patches ####
  library(dplyr)
  
  inputpath <- paste(mypath, "Inputs/", sep = "")
  setwd(inputpath)
  patch_raster <- raster(patch) 
  patch_frequency <- as.data.frame(freq(patch_raster)) #calculate freq
  patch_frequency_NFI <- subset(patch_frequency, patch_frequency$count>200) #subset to Ha
  patch_frequency_NFI <- subset(patch_frequency_NFI, patch_frequency_NFI$value != 0) #remove matric cells
  patch_frequency_NFI <- subset(patch_frequency_NFI, patch_frequency_NFI$value < 9000) #remove planted
  NFIpatches <- combined %>%
    filter(EndPatch %in% patch_frequency_NFI$value) #filter isolated patches by NFI only
  
  number_isolated_NFIpatches<- NFIpatches %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(isolated_NFIpatch_count = n_distinct(EndPatch))
  
  
  
  mergeCols <- c("Simulation", "Batch", "Land", "Rep", "Year")
  isolated_patch <- merge(number_isolated_NFIpatches, number_isolated_patches, by = mergeCols, all = TRUE)
  isolated_patch <- merge(isolated_patch, combined_distance, by = mergeCols, all = TRUE)
  
  isolated_patch$isolated_NFIpatch_count[is.na(isolated_patch$isolated_NFIpatch_count)] <- 0 
  
  isolated_patch$totalNFI <- length(patch_frequency_NFI$value)
  isolated_patch$proportion_isolated_NFI  <- isolated_patch$isolated_NFIpatch_count / isolated_patch$totalNFI
  isolated_patch$total_patch <- length(patch_frequency$value)
  isolated_patch$proportion_isolated  <- isolated_patch$isolated_patch_count / isolated_patch$total_patch
  
  isolated_patch <- subset(isolated_patch, select = - c(total_patch, totalNFI))
  
  
  
  return(isolated_patch)
}

#same functions but with ripiaran landscapes - different number of scenarions ####


Isolated_patch <- function(patch, mypath, mypattern, ...) {
  
  Outputpatch <- paste(mypath, "Outputs", sep = "")
  setwd(Outputpatch)
  library(dplyr)
  import.multiple.files<-function(Outputpatch,mypattern,...)
  {
    setwd(Outputpatch)
    tmp.list.1<-list.files(Outputpatch, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(Outputpatch = Outputpatch,
                          mypattern = mypattern,
                          sep = "\t")
  
  filenames <-
    list.files(Outputpatch, pattern = mypattern)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  Batch_update <- Map(cbind, files, Simulation = Sim)
  Batch_update <- Map(cbind, Batch_update, Batch = Batch)
  Batch_update <- Map(cbind, Batch_update, Land = Land)
  combined <- bind_rows(Batch_update, .id = "column_label")
  
  
  # calculate distance between all patches
  batch <- unique(combined$Batch) # which batch
  Patchfile <- read.csv("C:/FionaPlenderleith/CH4_models/Analysis/Patchfile.csv") #file with details 
  Patchfile <- subset(Patchfile, Patchfile$Batch == batch) #subset for teh abtch
  patchmaps <- unique(Patchfile$Patch_map) # now whch patch maps are using
  
  
  p_distance <-  function(patchmap){
    
    inputpath <- paste(mypath, "Inputs", sep = "")
    setwd(inputpath)
    patch_raster <- raster(patchmap) #import patch map
    p <- data.frame(rasterToPoints(patch_raster))
    name <- substr(patchmap, 1, nchar(patchmap)-4) 
    p <- p %>% dplyr::rename(PatchID = name)
    # filter out background
    p <- p[p$PatchID != 0,]
    p %>%
      group_by(PatchID)%>%
      dplyr::summarise(
        x = mean(x, na.rm=T),
        y = mean(y, na.rm=T)
      ) -> p
    p <- as.data.frame(p)
    real_raw <- mat_geo_dist(data = p, ID = "PatchID", x = "x", y = "y")
    real_raw <- as.data.frame(real_raw)
    setDT(real_raw, keep.rownames = TRUE)[]
    colnames(real_raw)[1] <- "StartPatch"
    real_raw$StartPatch <- as.factor(real_raw$StartPatch)
    # index for lower triangular matrix
    # real_ind <- lower.tri(real_raw, diag = TRUE)
    # # select values of interest and replace rest with NAs
    # real_tri <- real_raw
    # real_tri[real_ind == FALSE] <- NA
    
    real_long <-
      melt(
        data = real_raw,
        id.vars = "StartPatch",
        variable.name = "EndPatch",
        value.name = "Distance",
        na.rm = TRUE
      )
    real_long <- as.data.frame(real_long)
    real_long$Patch_map <- patchmap
    real_long <- subset(real_long, real_long$StartPatch != real_long$EndPatch)
    return(real_long)
  } #funciton to calc distance
  
  patch1 <- p_distance(patchmap = patchmaps[1])
  patch2 <- p_distance(patchmap = patchmaps[2])
  patch3 <- p_distance(patchmap = patchmaps[3])
  patch4 <- p_distance(patchmap = patchmaps[4])
  patch5 <- p_distance(patchmap = patchmaps[5])
  patch_stats <- rbind(patch1, patch2, patch3, patch4, patch5) #distances between patchess
  patch_stats$StartPatch <- factor(patch_stats$StartPatch)
  patch_stats$EndPatch <- factor(patch_stats$EndPatch)
  
  combined_distance <- left_join(combined, Patchfile, by = c("Batch", "Simulation", "Land"))
  combined_distance$StartPatch <- factor(combined_distance$StartPatch)
  combined_distance$EndPatch <- factor(combined_distance$EndPatch)
  combined_distance <- left_join(combined_distance, patch_stats, by = c("StartPatch", "EndPatch", "Patch_map"))
  combined_distance <- subset(combined_distance, combined_distance$StartPatch != -999) #sum rows
  combined_distance <- subset(combined_distance, combined_distance$StartPatch != -888)
  combined_distance <- subset(combined_distance, combined_distance$EndPatch != -999) #sum rows
  combined_distance <- subset(combined_distance, combined_distance$EndPatch != -888)
  
  combined_distance <- combined_distance %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      Mean_Distance = sum(Distance)/sum(Ninds)
    )
  
  # now we want to merge distances 
  
  combined <- subset(combined, combined$StartPatch == -999) #sum rows
  combined <- subset(combined, combined$StartPatch != -888)#sum rows
  combined <- subset(combined, combined$Ninds == 0) # only patches receiving 0 
  number_isolated_patches <- combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      isolated_patch_count = n_distinct(EndPatch))
  
  
  # filter for only large patches ####
  library(dplyr)
  
  inputpath <- paste(mypath, "Inputs", sep = "")
  setwd(inputpath)
  patch_raster <- raster(patch) 
  patch_frequency <- as.data.frame(freq(patch_raster)) #calculate freq
  patch_frequency_NFI <- subset(patch_frequency, patch_frequency$count>200) #subset to Ha
  patch_frequency_NFI <- subset(patch_frequency_NFI, patch_frequency_NFI$value != 0) #remove matric cells
  patch_frequency_NFI <- subset(patch_frequency_NFI, patch_frequency_NFI$value < 9000) #remove planted
  NFIpatches <- combined %>%
    filter(EndPatch %in% patch_frequency_NFI$value) #filter isolated patches by NFI only
  
  number_isolated_NFIpatches<- NFIpatches %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(isolated_NFIpatch_count = n_distinct(EndPatch))
  
  
  
  mergeCols <- c("Simulation", "Batch", "Land", "Rep", "Year")
  isolated_patch <- merge(number_isolated_NFIpatches, number_isolated_patches, by = mergeCols, all = TRUE)
  isolated_patch <- merge(isolated_patch, combined_distance, by = mergeCols, all = TRUE)
  
  isolated_patch$isolated_NFIpatch_count[is.na(isolated_patch$isolated_NFIpatch_count)] <- 0 
  
  isolated_patch$totalNFI <- length(patch_frequency_NFI$value)
  isolated_patch$proportion_isolated_NFI  <- isolated_patch$isolated_NFIpatch_count / isolated_patch$totalNFI
  isolated_patch$total_patch <- length(patch_frequency$value)
  isolated_patch$proportion_isolated  <- isolated_patch$isolated_patch_count / isolated_patch$total_patch
  
  isolated_patch <- subset(isolated_patch, select = - c(total_patch, totalNFI))
  
  
  
  return(isolated_patch)
}
Individual_summary <- function(landscape, mypath_input, mypath_output, mypattern, ...) {
  library(raster)
  library(dplyr)
  library(stringr)
  
  setwd(mypath_input)
  # first step - define patches as stepping stones and woodlands
  Patch <- raster(landscape) #import patch map
  Patch_frequency <- as.data.frame(freq(Patch)) #calculate freq
  Natal_patch_frequency <- Patch_frequency
  Natal_patch_frequency <- subset(Natal_patch_frequency, Natal_patch_frequency$value != 0)
  Natal_patch_frequency$Natal_patch <- Natal_patch_frequency$value
  Natal_patch_frequency$Natal_patch_type <- NA
  Natal_patch_frequency$Natal_patch_type[Natal_patch_frequency$count >=
                                           200] <-  "Woodland"
  Natal_patch_frequency$Natal_patch_type[Natal_patch_frequency$count < 200] <-
    "Steppingstone"
  Natal_patch_frequency <- Natal_patch_frequency[c("Natal_patch", "Natal_patch_type")]
  
  
  
  Focal_patch_frequency <- Patch_frequency
  Focal_patch_frequency$PatchID <- Focal_patch_frequency$value
  Focal_patch_frequency$Focal_patch_type <- NA
  Focal_patch_frequency$Focal_patch_type[Focal_patch_frequency$count >=
                                           200] <-  "Woodland"
  Focal_patch_frequency$Focal_patch_type[Focal_patch_frequency$count < 200] <-
    "Steppingstone"
  Focal_patch_frequency$Focal_patch_type[Focal_patch_frequency$value == 0] <-
    "matrix"  
  Focal_patch_frequency <- Focal_patch_frequency[c("PatchID", "Focal_patch_type")]
  setwd(mypath_output)
  library(dplyr)
  import.multiple.files<-function(mypath_output,mypattern,...)
  {
    setwd(mypath_output)
    tmp.list.1<-list.files(mypath_output, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(mypath = mypath_output,
                          mypattern = mypattern,
                          sep = "\t")
  
  filenames <-
    list.files(mypath_output, pattern = mypattern)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  combined <- Map(cbind, files, Simulation = Sim)
  combined <- Map(cbind, combined, Batch = Batch)
  combined <- Map(cbind, combined, Land = Land)
  combined <- bind_rows(combined, .id = "column_label")
  combined <-  merge(combined, Focal_patch_frequency, by = "PatchID", all = TRUE)
  combined <-  merge(combined, Natal_patch_frequency, by = "Natal_patch", all = TRUE)
  # # 
  # 
  
  
  # stepping stone - stepping stone patches analysis
  non_dispersers <- subset(combined, combined$Nsteps == 0)
  dispersers <- subset(combined, combined$Nsteps > 0)
  dispersers_stepping <- subset(dispersers, dispersers$Natal_patch_type == "Steppingstone")
  
  Success_stepping_Woodland <-subset(dispersers_stepping, dispersers_stepping$PatchID != 0)
  Success_stepping_Woodland <-subset(Success_stepping_Woodland, Success_stepping_Woodland$Focal_patch_type == "Woodland")
  Success_stepping_Stepping <- subset(dispersers_stepping, dispersers_stepping$PatchID != 0)
  Success_stepping_Stepping <-subset(Success_stepping_Stepping, Success_stepping_Stepping$Focal_patch_type == "Steppingstone")
  Fails_stepping <- subset(dispersers_stepping, dispersers_stepping$PatchID == 0)
  
  Summary.success_stepping_woodland <- Success_stepping_Woodland %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.success_stepping_woodland$Success <- "Yes"
  Summary.success_stepping_Stepping <- Success_stepping_Stepping %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.success_stepping_Stepping$Success <- "Yes"
  Summary.fails_stepping <- Fails_stepping %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.fails_stepping$Success <- "No"
  
  # woodland patches analysis
  
  dispersers_Woodland <- subset(dispersers, dispersers$Natal_patch_type == "Woodland")
  
  Success_Woodland_Woodland <-subset(dispersers_Woodland, dispersers_Woodland$PatchID != 0)
  Success_Woodland_Woodland <-subset(Success_Woodland_Woodland, Success_Woodland_Woodland$Focal_patch_type == "Woodland")
  Success_Woodland_Stepping <- subset(dispersers_Woodland, dispersers_stepping$PatchID != 0)
  Success_Woodland_Stepping <-subset(Success_Woodland_Stepping, Success_Woodland_Stepping$Focal_patch_type == "Steppingstone")
  Fails_Woodland <- subset(dispersers_Woodland, dispersers_Woodland$PatchID == 0)
  
  Summary.success_Woodland_woodland <- Success_Woodland_Woodland %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.success_Woodland_woodland$Success <- "Yes"
  Summary.success_Woodland_Stepping <- Success_Woodland_Stepping %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.success_Woodland_Stepping$Success <- "Yes"
  Summary.fails_Woodland <- Fails_Woodland %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise( # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  Summary.fails_Woodland$Success <- "No"
  
  
  Summary.non_dispersers <-
    non_dispersers %>% # Start by defining the original dataframe, AND THEN...
    group_by(Simulation, Batch, Land, Rep, Year) %>% # Define the grouping variable, AND THEN...
    summarise(
      # Now you define your summary variables with a name and a function...
      DistMoved = mean(DistMoved),
      Nsteps = mean(Nsteps),
      Nind = n(),
      ## The function n() in dlpyr gives you the number of observations
    )
  
  Summary.success_stepping_woodland$Natal_patch_type <-
    "Steppingstone"           
  Summary.success_stepping_Stepping$Natal_patch_type <-
    "Steppingstone"           
  Summary.fails_stepping$Natal_patch_type <-
    "Steppingstone"           
  Summary.success_Woodland_woodland$Natal_patch_type <- "Woodland"
  Summary.success_Woodland_Stepping$Natal_patch_type <- "Woodland"
  Summary.fails_Woodland$Natal_patch_type <- "Woodland"
  Summary.success_stepping_woodland$Focal_patch_type <-
    "Woodland"       
  Summary.success_stepping_Stepping$Focal_patch_type <-
    "Steppingstone"           
  Summary.fails_stepping$Focal_patch_type <- "Matrix" 
  
  Summary.success_Woodland_woodland$Focal_patch_type <- "Woodland"
  Summary.success_Woodland_Stepping$Focal_patch_type <- "Steppingstone" 
  Summary.fails_Woodland$Focal_patch_type <- "Matrix" 
  
  #sort this
  Connect_summary <-
    rbind(
      Summary.non_dispersers,
      Summary.success_stepping_woodland,
      Summary.success_stepping_Stepping,
      Summary.fails_stepping,
      Summary.success_Woodland_woodland,
      Summary.success_Woodland_Stepping,
      Summary.fails_Woodland
    )
  
  return(Connect_summary)
  
}
Pop_size <- function(mypath, mypattern, ...) {
  setwd(mypath)
  library(dplyr)
  library(stringr)
  import.multiple.files<-function(mypath,mypattern,...)
  {
    setwd(mypath)
    tmp.list.1<-list.files(mypath, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(mypath = mypath,
                          mypattern = mypattern,
                          sep = "\t")
  
  filenames <-
    list.files(mypath, pattern = mypattern)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  Batch_update <- Map(cbind, files, Simulation = Sim)
  Batch_update <- Map(cbind, Batch_update, Batch = Batch)
  Batch_update <- Map(cbind, Batch_update, Land = Land)
  combined <- bind_rows(Batch_update, .id = "column_label")
  
  
  population_size <- combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      pop_size = sum(NInd))
  
  return(population_size)
}
Filter_genetics <- function(min_pop_size, mypath, ...) {
  setwd(mypath)
  library(dplyr)
  import.multiple.files<-function(mypath,pattern,...)
  {
    setwd(mypath)
    tmp.list.1<-list.files(mypath, pattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  pop_files <-
    import.multiple.files(mypath = mypath,
                          pattern = "Pop.txt",
                          sep = "\t")
  pop_files <- pop_files[sapply(pop_files, function(x) dim(x)[1]) > 0]
  pop_filenames <-
    list.files(mypath, pattern = "Pop.txt")
  pop_filenames <- as.data.frame(pop_filenames)
  pop_filenames <-
    as.data.frame(str_split_fixed(pop_filenames$pop_filenames, "_", 4))
  pop_Batch <- unname(unlist(pop_filenames["V1"]))
  pop_Sim <- unname(unlist(pop_filenames["V2"]))
  pop_Land <- unname(unlist(pop_filenames["V3"]))
  Pop <- Map(cbind, pop_files, Simulation = pop_Sim)
  Pop <- Map(cbind, Pop, Batch = pop_Batch)
  Pop <- Map(cbind, Pop, Land = pop_Land)
  Population <- bind_rows(Pop, .id = "column_label")
  
  
  
  gen_files <-
    import.multiple.files(mypath = mypath,
                          pattern = "LandGen.txt",
                          sep = "\t")
  gen_filenames <-
    list.files(mypath, pattern = "LandGen.txt")
  gen_filenames <- as.data.frame(gen_filenames)
  gen_filenames <-
    as.data.frame(str_split_fixed(gen_filenames$gen_filenames, "_", 4))
  gen_Batch <- unname(unlist(gen_filenames["V1"]))
  gen_Sim <- unname(unlist(gen_filenames["V2"]))
  gen_Land <- unname(unlist(gen_filenames["V3"]))
  gen <- Map(cbind, gen_files, Simulation = gen_Sim)
  gen <- Map(cbind, gen, Batch = gen_Batch)
  gen <- Map(cbind, gen, Land = gen_Land)
  Genetics <- bind_rows(gen, .id = "column_label")
  
  Genetics$PatchID0_size <- Population[match(paste(Genetics$Rep, Genetics$Year, Genetics$Simulation, Genetics$Batch, Genetics$Land, Genetics$PatchID0),paste(Population$Rep, Population$Year, Population$Simulation, Population$Batch, Population$Land, Population$PatchID)),"NInd"]
  Genetics$PatchID1_size <- Population[match(paste(Genetics$Rep, Genetics$Year, Genetics$Simulation, Genetics$Batch, Genetics$Land, Genetics$PatchID1),paste(Population$Rep, Population$Year, Population$Simulation, Population$Batch, Population$Land, Population$PatchID)),"NInd"]
  
  
  Genetics <- subset(Genetics, Genetics$PatchID0_size > min_pop_size)
  Genetics <- subset(Genetics, Genetics$PatchID1_size > min_pop_size)
  
  Genetics$FST[Genetics$FST[] == -999] <- "NA"
  Genetics$FST[Genetics$FST < 0] <- 0
  Genetics$FST <- as.numeric(Genetics$FST)
  
  Genetics_summary <- Genetics %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      Distance = mean(Distance, na.rm =T),
      MeanNAlleles = mean(MeanNAlleles, na.rm =T),
      FIS = mean(FIS, na.rm =T),
      FST = mean(FST, na.rm =T),
      MeanD = mean(MeanD, na.rm =T),
      HarmMeanD = mean(HarmMeanD, na.rm =T)
    )
  
  
  return(Genetics_summary)
}
Dispersal_summary_con <- function(mypath_output, mypattern, ...) {
  
  
  setwd(mypath_output)
  #read in all ind files 
  import.multiple.files<-function(mypath_output,mypattern,...)
  {
    setwd(mypath_output)
    tmp.list.1<-list.files(mypath_output, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(mypath = mypath_output,
                          mypattern = mypattern,
                          sep = "\t")
  files <- files[sapply(files, function(x) dim(x)[1]) > 0]
  filenames <-
    names(files)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  
  combined <- Map(cbind, files, Simulation = Sim)
  combined <- Map(cbind, combined, Batch = Batch)
  combined <- Map(cbind, combined, Land = Land)
  combined <- bind_rows(combined, .id = "column_label")
  
  
  combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    subset(EndPatch == -888) %>%
    dplyr::summarise( 
      Nind_em_all = sum(Ninds)
    ) -> Emi
  combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    subset(EndPatch == -999) %>%
    dplyr::summarise( 
      Nind_em = sum(Ninds)
    ) -> Imi
  
  Final_data <-
    merge(
      Emi,
      Imi,
      by = c("Simulation", "Batch", "Land", "Rep", "Year"),
      all = TRUE
    )
  
  Final_data$proportion_success_con <- Final_data$Nind_em  / Final_data$Nind_em_all
  
  
  
  return(Final_data)
}
Isolated_patch_5000 <- function(patch, mypath, mypattern, ...) {
  
  Outputpatch <- paste(mypath, "Outputs", sep = "")
  setwd(Outputpatch)
  library(dplyr)
  import.multiple.files<-function(Outputpatch,mypattern,...)
  {
    setwd(Outputpatch)
    tmp.list.1<-list.files(Outputpatch, pattern=mypattern)
    tmp.list.2<-list(length=length(tmp.list.1))
    for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
    names(tmp.list.2)<-tmp.list.1
    tmp.list.2
  }
  files <-
    import.multiple.files(Outputpatch = Outputpatch,
                          mypattern = mypattern,
                          sep = "\t")
  
  filenames <-
    list.files(Outputpatch, pattern = mypattern)
  filenames <- as.data.frame(filenames)
  filenames <-
    as.data.frame(str_split_fixed(filenames$filenames, "_", 4))
  Batch <- unname(unlist(filenames["V1"]))
  Sim <- unname(unlist(filenames["V2"]))
  Land <- unname(unlist(filenames["V3"]))
  Batch_update <- Map(cbind, files, Simulation = Sim)
  Batch_update <- Map(cbind, Batch_update, Batch = Batch)
  Batch_update <- Map(cbind, Batch_update, Land = Land)
  combined <- bind_rows(Batch_update, .id = "column_label")
  
  
  # calculate distance between all patches
  batch <- unique(combined$Batch) # which batch
  Patchfile <- read.csv("C:/FionaPlenderleith/CH4_models/Analysis/Patchfile_5000.csv") #file with details 
  Patchfile <- subset(Patchfile, Patchfile$Batch == batch) #subset for teh abtch
  patchmaps <- unique(Patchfile$Patch_map) # now whch patch maps are using
  
  
  p_distance <-  function(patchmap){
    
    inputpath <- paste(mypath, "Inputs", sep = "")
    setwd(inputpath)
    patch_raster <- raster(patchmap) #import patch map
    p <- data.frame(rasterToPoints(patch_raster))
    name <- substr(patchmap, 1, nchar(patchmap)-4) 
    p <- p %>% dplyr::rename(PatchID = name)
    # filter out background
    p <- p[p$PatchID != 0,]
    p %>%
      group_by(PatchID)%>%
      dplyr::summarise(
        x = mean(x, na.rm=T),
        y = mean(y, na.rm=T)
      ) -> p
    p <- as.data.frame(p)
    real_raw <- mat_geo_dist(data = p, ID = "PatchID", x = "x", y = "y")
    real_raw <- as.data.frame(real_raw)
    setDT(real_raw, keep.rownames = TRUE)[]
    colnames(real_raw)[1] <- "StartPatch"
    real_raw$StartPatch <- as.factor(real_raw$StartPatch)
    # index for lower triangular matrix
    # real_ind <- lower.tri(real_raw, diag = TRUE)
    # # select values of interest and replace rest with NAs
    # real_tri <- real_raw
    # real_tri[real_ind == FALSE] <- NA
    
    real_long <-
      melt(
        data = real_raw,
        id.vars = "StartPatch",
        variable.name = "EndPatch",
        value.name = "Distance",
        na.rm = TRUE
      )
    real_long <- as.data.frame(real_long)
    real_long$Patch_map <- patchmap
    real_long <- subset(real_long, real_long$StartPatch != real_long$EndPatch)
    return(real_long)
  } #funciton to calc distance
  
  patch1 <- p_distance(patchmap = patchmaps[1])
  patch2 <- p_distance(patchmap = patchmaps[2])
  patch3 <- p_distance(patchmap = patchmaps[3])
  patch4 <- p_distance(patchmap = patchmaps[4])
  patch5 <- p_distance(patchmap = patchmaps[5])
  patch_stats <- rbind(patch1, patch2, patch3, patch4, patch5) #distances between patchess
  patch_stats$StartPatch <- factor(patch_stats$StartPatch)
  patch_stats$EndPatch <- factor(patch_stats$EndPatch)
  
  combined_distance <- left_join(combined, Patchfile, by = c("Batch", "Simulation", "Land"))
  combined_distance$StartPatch <- factor(combined_distance$StartPatch)
  combined_distance$EndPatch <- factor(combined_distance$EndPatch)
  combined_distance <- left_join(combined_distance, patch_stats, by = c("StartPatch", "EndPatch", "Patch_map"))
  combined_distance <- subset(combined_distance, combined_distance$StartPatch != -999) #sum rows
  combined_distance <- subset(combined_distance, combined_distance$StartPatch != -888)
  combined_distance <- subset(combined_distance, combined_distance$EndPatch != -999) #sum rows
  combined_distance <- subset(combined_distance, combined_distance$EndPatch != -888)
  
  combined_distance <- combined_distance %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      Mean_Distance = sum(Distance)/sum(Ninds)
    )
  
  # now we want to merge distances 
  
  combined <- subset(combined, combined$StartPatch == -999) #sum rows
  combined <- subset(combined, combined$StartPatch != -888)#sum rows
  combined <- subset(combined, combined$Ninds == 0) # only patches receiving 0 
  number_isolated_patches <- combined %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(
      isolated_patch_count = n_distinct(EndPatch))
  
  
  # filter for only large patches ####
  library(dplyr)
  
  inputpath <- paste(mypath, "Inputs", sep = "")
  setwd(inputpath)
  patch_raster <- raster(patch) 
  patch_frequency <- as.data.frame(freq(patch_raster)) #calculate freq
  patch_frequency_NFI <- subset(patch_frequency, patch_frequency$count>200) #subset to Ha
  patch_frequency_NFI <- subset(patch_frequency_NFI, patch_frequency_NFI$value != 0) #remove matric cells
  patch_frequency_NFI <- subset(patch_frequency_NFI, patch_frequency_NFI$value < 9000) #remove planted
  NFIpatches <- combined %>%
    filter(EndPatch %in% patch_frequency_NFI$value) #filter isolated patches by NFI only
  
  number_isolated_NFIpatches<- NFIpatches %>%
    group_by(Simulation, Batch, Land, Rep, Year) %>%
    dplyr::summarise(isolated_NFIpatch_count = n_distinct(EndPatch))
  
  
  
  mergeCols <- c("Simulation", "Batch", "Land", "Rep", "Year")
  isolated_patch <- merge(number_isolated_NFIpatches, number_isolated_patches, by = mergeCols, all = TRUE)
  isolated_patch <- merge(isolated_patch, combined_distance, by = mergeCols, all = TRUE)
  
  isolated_patch$isolated_NFIpatch_count[is.na(isolated_patch$isolated_NFIpatch_count)] <- 0 
  
  isolated_patch$totalNFI <- length(patch_frequency_NFI$value)
  isolated_patch$proportion_isolated_NFI  <- isolated_patch$isolated_NFIpatch_count / isolated_patch$totalNFI
  isolated_patch$total_patch <- length(patch_frequency$value)
  isolated_patch$proportion_isolated  <- isolated_patch$isolated_patch_count / isolated_patch$total_patch
  
  isolated_patch <- subset(isolated_patch, select = - c(total_patch, totalNFI))
  
  
  
  return(isolated_patch)
}

