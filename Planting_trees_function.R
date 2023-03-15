library(dplyr)
library(raster)
library(igraph)
library(rgdal)
library(mosaic)
library(landscapeR)
library(landscapeR)
library(raster)
library(stringr)
library(plyr)
library(rgdal)


# Read in maps

Landuse <-
  raster("C:/Users/r01fp19/OneDrive - University of Aberdeen/Northern_forest/NorthernForestSamples/NF_landuse1.tif" )# CEH through digimap landuse data
River_2020 <-
  readOGR(dsn = "C:/Users/r01fp19/OneDrive - University of Aberdeen/Northern_forest/NorthernForestSamples/Northern_data.gdb/drn-2017_4532725", layer = "14march17_riv_0_0_0_000_000_675_1250_line")  #River map - DIGIMAP for riparian
River_2020$ID <- 1 #create ID field for rivers

NFI <- readOGR(dsn = "C:/Users/r01fp19/OneDrive - University of Aberdeen/Northern_forest/National_Forest_Inventory_Woodland_England_2019", layer = "National_Forest_Inventory_Woodland_England_2019") #ger NFI data on woodland cover
NFI_conifer <-
  NFI[NFI$IFT_IOA == "Conifer" |
        NFI$IFT_IOA == " Mixed Predominantly conifer" ,] # subset the conifer data

TreeCanopy ="NTM_Canopy_Polygons_SE88SE"
TargetArea = 2500
patch_range = 400:2000
Rep ="R3"
Output_path ="C:/FionaPlenderleith/CH4_models/Riparian_2500/SE88SE"


PlantingTree <-
 function(TreeCanopy, TargetArea, patch_range, Rep, Output_path) {
    
 #  TreeCanopy = "NTM_Canopy_Polygons_SE88SE" Name of square 
 #  TargetArea = 2500 #target area to plant in cells
 #  patch_range = 400:2000 # target number of cells per patch
 #  Rep ="R1" #rep number
 # Output_path ="C:/FionaPlenderleith/CH4_models/Riparian_2500/SE88SE" #lication to save
   
    # step 2 - patch buffers
    # set seed for reproducibility
    set.seed(0)
    
    Treecanopy_map <-
      readOGR(dsn = "C:/Users/r01fp19/OneDrive - University of Aberdeen/Canopy_maps", layer = TreeCanopy) # upload tree canopy map
    # Step 1: create a 5 m2 raster of trees - all none tree cells to be classified as suitable and unsuitable (urban and bog).
    r <-     raster(
      ncol = 1000,
      nrow = 1000,
      res = 5,
      ext = extent(Treecanopy_map)
    ) # create empty raster with correct extent
    Treecanopy_map$ID <- 2 #create ID field for trees
    TreeCanopyRast <-
      rasterize(Treecanopy_map, r, field = "ID", background = 0) #raster of trees =1 and matrix = 0
    
    TreeCanopyRast_buffer <- clump(TreeCanopyRast)
    clumpFreq <- as.data.frame(freq(TreeCanopyRast_buffer))
    ## Put these into a vector of clump ID's to be removed
    excludeID <-
      clumpFreq$value[which(clumpFreq$count < 200)] # exclude patches with less than 400 cells (1 ha or 100 m2)
    ## Assign NA to all clumps whose IDs are found in excludeID
    TreeCanopyRast_buffer[TreeCanopyRast_buffer %in% excludeID] <-
      NA
    TreeCanopyRast_buffer[TreeCanopyRast_buffer[] < 0] <- 1
    TreeCanopyRast_buffer1 <-
      raster::buffer(TreeCanopyRast_buffer, width = 1)
    TreeCanopyRast_buffer40 <-
      raster::buffer(TreeCanopyRast_buffer1, width = 39)
    TreeCanopyRast_buffer300 <-
      raster::buffer(TreeCanopyRast_buffer40, width = 360)
    River <- rasterize(River_2020, r, field = "ID", background = NA)
    
    ### now create suitability maps for scenarios
    
    # step 3: Convert matrix to suitable (2) and unsuitable (0), and tree (1)
    Landuse_crop <-
      disaggregate(crop(Landuse, extent(Treecanopy_map)), 5)
    TreeCanopyRast_suit <- TreeCanopyRast
    TreeCanopyRast_suit[TreeCanopyRast_suit[] == 0 &
                          Landuse_crop[] == 3] <- 1 # Arable
    TreeCanopyRast_suit[TreeCanopyRast_suit[] == 0 &
                          Landuse_crop[] == 4] <-
      1 # Improve grassland
    TreeCanopyRast_suit[TreeCanopyRast_suit[] == 0 &
                          Landuse_crop[] == 5] <-
      1 # Neutral grassland
    TreeCanopyRast_suit[TreeCanopyRast_suit[] == 0 &
                          Landuse_crop[] == 6] <-
      1 # Calcareous grassland
    TreeCanopyRast_suit[TreeCanopyRast_suit[] == 0 &
                          Landuse_crop[] == 7] <- 1 # Acid grassland
    TreeCanopyRast_suit[TreeCanopyRast_suit[] == 0 &
                          Landuse_crop[] == 9] <- 1 # Heather
    TreeCanopyRast_suit[TreeCanopyRast_suit[] == 0 &
                          Landuse_crop[] == 10] <-
      1 # Heather grassland
    
    
    # now remove conifer plantations
    NFI_conifer$ID <- 1
    NFI_conifer_rast <-
      rasterize(NFI_conifer, r, field = "ID", background = 0) #raster of trees =1 and matrix = 0
    TreeCanopyRast_suit[TreeCanopyRast_suit[] == 2 &
                          NFI_conifer_rast[] == 1] <-
      0  #conifer woodlands are unsuitable
    
    # Step 5: scenario suiatbiltiy maps
    Scenario0 <- TreeCanopyRast_suit
    Scenario0[Scenario0[] == 1] <- 0
    #scenario 1: outwith woodlands
    
    Scenario1 <- TreeCanopyRast_suit #copy suitability map
    Scenario1[Scenario1[] == 1 &
                TreeCanopyRast_buffer40[] == 1] <- 0
    
    val_S1 <- 9000 # starting patch value
    
    while (length(Scenario1[Scenario1[] > 9000]) <= TargetArea * 0.99) {
      val_S1 <- val_S1 + 1
      size.clusters <- as.numeric(sample(patch_range, 1))
      SeedpointMap_S1 <- Scenario1
      SeedpointMap_S1[SeedpointMap_S1[] == 1 &
                        is.na(TreeCanopyRast_buffer300[])]  <- 0
      SeedpointMap_S1[SeedpointMap_S1[] == 5]  <- 0
      seedpoint_S1 <-
        as.data.frame(sampleStratified(SeedpointMap_S1, size = 1))
      seedpoint_S1 <- subset(seedpoint_S1, layer == 1)
      
      Scenario1 <-
        makePatch(
          Scenario1,
          spt = seedpoint_S1$cell,
          size = size.clusters,
          bgr = 1,
          edge = FALSE,
          rast = TRUE,
          val = val_S1
        )
    }
    Scenario1[Scenario1[] == 1] <- 0
    
    #scenario 2: expand woodlands
    Scenario2 <- TreeCanopyRast_suit #copy suitability map
    Scenario2[Scenario2[] == 1 &
                is.na(TreeCanopyRast_buffer40[])] <- 0
    
    Scenario2_seedpoints <- c()
    val_S2 <- 9000
    while (length(Scenario2[Scenario2[] > 9000]) <= TargetArea * 0.99) {
      size.clusters <- as.numeric(sample(patch_range, 1))
      val_S2 <- val_S2 + 1
      SeedpointMap_S2 <- Scenario2
      SeedpointMap_S2[SeedpointMap_S2[] == 1 &
                        is.na(TreeCanopyRast_buffer300[]) &
                        is.na(TreeCanopyRast_buffer1[])]  <- 0
      SeedpointMap_S2[SeedpointMap_S2[] == 5]  <- 0
      seedpoint_S2 <-
        as.data.frame(sampleStratified(SeedpointMap_S2, size = 1))
      seedpoint_S2 <- subset(seedpoint_S2, layer == 1)
      
      Scenario2 <-
        makePatch(
          Scenario2,
          spt = seedpoint_S2$cell,
          size = size.clusters,
          bgr = 1,
          edge = FALSE,
          rast = TRUE,
          val = val_S2
        )
      Scenario2_seedpoints <-
        c(Scenario2_seedpoints, seedpoint_S2$cell)
    }
    
    
    Scenario2[Scenario2[] == 1] <- 0
    
    # scenario 3: Corridors
    
    
    Scenario3 <- TreeCanopyRast_suit #copy suitabiltiy map
    Scenario3[Scenario3[] == 1 &
                TreeCanopyRast_buffer40[] == 1] <- 0
    
    val_S3 <- 9000
    while (length(Scenario3[Scenario3[] > 9000]) <= TargetArea * 0.99) {
      val_S3 <- val_S3 + 1
      size.clusters <- as.numeric(sample(patch_range, 1))
      dire <- as.numeric(sample(0:360, 1))
      SeedpointMap_S3 <- Scenario3
      SeedpointMap_S3[SeedpointMap_S3[] == 1 &
                        is.na(TreeCanopyRast_buffer300[])]  <- 0
      SeedpointMap_S3[SeedpointMap_S3[] == 5]  <- 0
      seedpoint_S3 <-
        as.data.frame(sampleStratified(SeedpointMap_S3, size = 1))
      seedpoint_S3 <- subset(seedpoint_S3, layer == 1)
      
      Scenario3 <-
        makeLine(
          Scenario3,
          spt = seedpoint_S3$cell,
          size = size.clusters,
          bgr = 1,
          edge = FALSE,
          rast = TRUE,
          val = val_S3,
          convol = 0.1,
          direction = dire
        )
    }
    Scenario3[Scenario3[] == 1] <- 0
    
    Scenario1_new_patch <- Scenario1
    Scenario2_new_patch <- Scenario2
    Scenario3_new_patch <- Scenario3
    
    Scenario1_new_patch[Scenario1_new_patch[] == 2] <- 0
    Scenario2_new_patch[Scenario2_new_patch[] == 2] <- 0
    Scenario3_new_patch[Scenario3_new_patch[] == 2] <- 0
    
    Scenario1[Scenario1[] > 9000] <- 2
    Scenario2[Scenario2[] > 9000] <- 2
    Scenario3[Scenario3[] > 9000] <- 2
    
    Scenario4 <- TreeCanopyRast_suit
    
    if(length(River[River[] == 1]) > 1){ 
      
      River_buffer <- raster::buffer(River, width = 10)
      Scenario4[Scenario4[] == 1 & is.na(River_buffer[])] <- 0
      Scenario4[Scenario4[] == 1 & TreeCanopyRast_buffer40[] == 1] <- 0
      
    } else{
      
      Scenario4[Scenario4[] == 1] <- 0
    }
    
    
    if(length(Scenario4[Scenario4[] == 1]) >= TargetArea){ # if we have enough riparian land create scenario4 
      
    # Scenario 4: riparian supplemented by steppingstones
      val_S4 <- 9000
      while (length(Scenario4[Scenario4[] > 5]) <= TargetArea) {
        val_S4 <- val_S4 + 1
        size.clusters <- as.numeric(sample(patch_range, 1))
        SeedpointMap_S4 <- Scenario4
        SeedpointMap_S4[SeedpointMap_S4[] == 1 & is.na(TreeCanopyRast_buffer300[])]  <- 0
        SeedpointMap_S4[SeedpointMap_S4[] == 5]  <- 0
        seedpoint_S4 <- as.data.frame(sampleStratified(SeedpointMap_S4, size = 1))
        seedpoint_S4 <- subset(seedpoint_S4, layer == 1)
        
        Scenario4 <-
          makePatch(
            Scenario4,
            spt = seedpoint_S4$cell,
            size = size.clusters,
            bgr = 1,
            edge = FALSE,
            rast = TRUE,
            val = val_S4
          )
        
      }
      
      Scenario4[Scenario4[] == 1] <- 0
      #step 6: plant the trees in suitable places
      Scenario4_new_patch <- Scenario4
      Scenario4_new_patch[Scenario4_new_patch[] == 2] <- 0
      Scenario4[Scenario4[] > 9000] <- 2
      
      # Now we need to create patch maps
      
      Patch_maps <- function(Scenario) {
        Scenario_patch <- Scenario # copy habitat map
        Scenario_patch[Scenario_patch[] == 1] <-
          0 ### converting all matrix cells to
        Scenario_patch <-
          clump(Scenario_patch) # clump patches (cells surrounded by matrix)
        ## Make an NA-value raster based on the LC raster attributes
        formask <- setValues(raster(Scenario_patch), NA)
        ## Assign 1 to formask to all cells corresponding to the forest class
        formask[Scenario0 != 0] <- 1
        clumpFreq <- freq(Scenario_patch)
        ## Coerce freq table to data.frame
        clumpFreq <- as.data.frame(clumpFreq)
        ## which rows of the data.frame are only represented by xx cell?
        ## Put these into a vector of clump ID's to be removed
        excludeID <- clumpFreq$value[which(clumpFreq$count < 5)]
        ## Assign NA to all clumps whose IDs are found in excludeID
        Scenario_patch[Scenario_patch %in% excludeID] <- NA
        
        return(Scenario_patch)
      }
      
      Scenario0_patch <-
        Patch_maps(Scenario = Scenario0)
      Scenario0_patch <-
        clump(Scenario0_patch) 
      #   sort numbering
      # renumber scenario 0 from 1
      # copy numbers to Scenario0 1, 2 and 3
      # new planted patches must be number 900xx (must be able to distinguish)
      # then save data set with size of each patch
      
      # restart patch count from 1
      
      
      Renumber_patch <- function(Scenario, Scenario0_patch) {
        Patch_map <- mosaic(Scenario, Scenario0_patch, fun = max) #mosaic together
        return(Patch_map) #return this
      }
      
      Scenario1_patch <-
        Renumber_patch(Scenario = Scenario1_new_patch, Scenario0_patch = Scenario0_patch)
      Scenario2_patch <-
        Renumber_patch(Scenario = Scenario2_new_patch, Scenario0_patch = Scenario0_patch)
      Scenario3_patch <-
        Renumber_patch(Scenario = Scenario3_new_patch, Scenario0_patch = Scenario0_patch)
      Scenario4_patch <-
        Renumber_patch(Scenario = Scenario4_new_patch, Scenario0_patch = Scenario0_patch)
      
      # scenario 2 expands patches so there are no 'new patches' therefore the new patches are given number of patch that was expanded ####
      Scenario2_patch[Scenario2_patch[]==0] <- NA
      xy <- as.data.frame(xyFromCell(Scenario2_patch, Scenario2_seedpoints))
      extracted = extract(x = Scenario2_patch, y = xy)
      Scenario2_patch_noNA <- Scenario2_patch # copy patch map
      Scenario2_patch_noNA[Scenario2_patch_noNA[] > 9000] <- NA #NA for all new patches
      Scenario2_patch_noNA[Scenario2_patch_noNA[] == "-Inf"] <- NA
      Scenario2_patch_noNA_filter <- Scenario2_patch_noNA
      formask_2 <- setValues(raster(Scenario2_patch_noNA_filter), NA)
      ## Assign 1 to formask to all cells corresponding to the forest class
      formask_2[Scenario0 != 0] <- 1
      clumpFreq_2 <- freq(Scenario2_patch_noNA_filter)
      ## Coerce freq table to data.frame
      clumpFreq_2 <- as.data.frame(clumpFreq_2)
      ## which rows of the data.frame are only represented by xx cell?
      ## Put these into a vector of clump ID's to be removed
      excludeID_2 <- clumpFreq_2$value[which(clumpFreq_2$count < 200)]
      ## Assign NA to all clumps whose IDs are found in excludeID
      Scenario2_patch_noNA_filter[Scenario2_patch_noNA_filter %in% excludeID_2] <- NA
      # then take the raster value with lowest distance to point AND non-NA value in the raster
      sampled <- apply(X = xy, MARGIN = 1, FUN = function(xy) Scenario2_patch_noNA_filter@data@values[which.min(replace(distanceFromPoints(Scenario2_patch_noNA_filter, xy), is.na(Scenario2_patch_noNA_filter), NA))]) #this is gives the cell ID of nearest patch
      sample.vec <- function(x, ...) x[sample(length(x), ...)]
      sampled <- unlist(lapply(sampled, sample.vec, 1)) #select 1 neighbor from each = NEW ID
      New_ID <- data.frame(extracted, sampled)
      Scenario2_patch_sub <- raster::subs(Scenario2_patch, y = New_ID, by = "extracted", which = "sampled") #replace extracted new patch ID with value of nearest raster
      Scenario2_patch <- mosaic(Scenario2_patch_sub, Scenario2_patch, fun = min) #mosaic together
      
      
      
      
      
      ### assign tree types 
      
      Scenario0[Scenario0[] == 2] <-
        sample(
          c(2, 3),
          size = length(Scenario0[Scenario0[] ==
                                    2]),
          replace  = T,
          prob = c(0.6, 0.4)
        )
      Scenario0[Scenario0[] == 2] <-
        sample(
          c(2, 4),
          size = length(Scenario0[Scenario0[] ==
                                    2]),
          replace  = T,
          prob = c(0.5, 0.5)
        )
      Scenario1[Scenario1[] == 2] <-
        sample(
          c(2, 3),
          size = length(Scenario1[Scenario1[] ==
                                    2]),
          replace  = T,
          prob = c(0.6, 0.4)
        )
      Scenario1[Scenario1[] == 2] <-
        sample(
          c(2, 4),
          size = length(Scenario1[Scenario1[] ==
                                    2]),
          replace  = T,
          prob = c(0.5, 0.5)
        )
      Scenario2[Scenario2[] == 2] <-
        sample(
          c(2, 3),
          size = length(Scenario2[Scenario2[] ==
                                    2]),
          replace  = T,
          prob = c(0.6, 0.4)
        )
      Scenario2[Scenario2[] == 2] <-
        sample(
          c(2, 4),
          size = length(Scenario2[Scenario2[] ==
                                    2]),
          replace  = T,
          prob = c(0.5, 0.5)
        )
      Scenario3[Scenario3[] == 2] <-
        sample(
          c(2, 3),
          size = length(Scenario3[Scenario3[] ==
                                    2]),
          replace  = T,
          prob = c(0.6, 0.4)
        )
      Scenario3[Scenario3[] == 2] <-
        sample(
          c(2, 4),
          size = length(Scenario3[Scenario3[] ==
                                    2]),
          replace  = T,
          prob = c(0.5, 0.5)
        )
      Scenario4[Scenario4[] == 2] <-
        sample(
          c(2, 3),
          size = length(Scenario4[Scenario4[] ==
                                    2]),
          replace  = T,
          prob = c(0.6, 0.4)
        )
      Scenario4[Scenario4[] == 2] <-
        sample(
          c(2, 4),
          size = length(Scenario4[Scenario4[] ==
                                    2]),
          replace  = T,
          prob = c(0.5, 0.5)
        )
      
      
      
      Scenario0[NFI_conifer_rast[] == 1 & TreeCanopyRast[] == 2] <- 1 #replace conifer plantations (is a tree and in conifer)
      Scenario1[NFI_conifer_rast[] == 1 & TreeCanopyRast[] == 2] <- 1
      Scenario2[NFI_conifer_rast[] == 1 & TreeCanopyRast[] == 2] <- 1
      Scenario3[NFI_conifer_rast[] == 1 & TreeCanopyRast[] == 2] <- 1
      Scenario4[NFI_conifer_rast[] == 1 & TreeCanopyRast[] == 2] <- 1
      
      
      
      # split corridors into sub-patches 
      
      split_corridors <- function(Patch_map){
        
        freq <- as.data.frame(freq(Patch_map))
        freq <- subset(freq, freq$value > 9000)
        p <- data.frame(rasterToPoints(Patch_map))
        p <- subset(p, p$layer > 9000)
        p <- ddply(
          p,
          .var = "layer",
          .fun = function(p) {
            return(subset(p, x %in% min(x)))
          }
        )
        p <- ddply(
          p,
          .var = "layer",
          .fun = function(p) {
            return(subset(p, y %in% min(y)))
          }
        )
        p$cell_num <- cellFromXY(Patch_map, p)
        
        
        for(i in freq$value) {
          
          patch <- i
          patch_new <- patch + 10000
          size.clusters <- length(Patch_map[Patch_map[]== patch])/3
          seedpoint <- subset(p, p$layer == patch)
          Patch_map <-
            makePatch(
              Patch_map,
              spt = seedpoint$cell_num,
              size = size.clusters,
              bgr = patch,
              edge = FALSE,
              rast = TRUE,
              val = patch_new
            )
        }
        
        freq <- as.data.frame(freq(Patch_map))
        freq <- subset(freq, freq$value > 9000 & freq$value < 10000)
        p <- data.frame(rasterToPoints(Patch_map))
        p <- subset(p, p$layer > 9000 & p$layer < 10000)
        p <- ddply(
          p,
          .var = "layer",
          .fun = function(p) {
            return(subset(p, x %in% min(x)))
          }
        )
        p <- ddply(
          p,
          .var = "layer",
          .fun = function(p) {
            return(subset(p, y %in% min(y)))
          }
        )
        p$cell_num <- cellFromXY(Patch_map, p)
        
        for(j in freq$value) {
          patch <- j
          patch_new <- patch + 20000
          size.clusters <- length(Patch_map[Patch_map[]== patch])/2
          seedpoint <- subset(p, p$layer == patch)
          Patch_map <-
            makePatch(
              Patch_map,
              spt = seedpoint$cell_num,
              size = size.clusters,
              bgr = patch,
              edge = FALSE,
              rast = TRUE,
              val = patch_new)
        }
        return(Patch_map)
      }
      
      Scenario3_patch <- split_corridors(Patch_map = Scenario3_patch)
      Scenario4_patch <- split_corridors(Patch_map = Scenario4_patch)
      
      # finally filter out species to create species and generalist maps
      
      Remove_patches <- function(Scenario_patch, scenario_hab, suitable_tree) {
        
        SPcountstats <-
          data.frame(zonal(
            match(scenario_hab, suitable_tree),
            #which patches contain tree type 
            Scenario_patch,
            #each forest patch is a zone
            fun = 'count',
            digits = 0,
            na.rm = TRUE
          ))
        #create replicate raster to work with
        excludeID_sp <-
          SPcountstats$zone[which(SPcountstats$count < 1)]  ## count of trees and update ID list
        
        ## Assign NA to all clumps whose IDs are found in excludeID
        Scenario_patch[Scenario_patch %in% excludeID_sp] <- NA
        return(Scenario_patch)
      }
      
      Scenario0_patch_generalist <- Remove_patches(Scenario_patch = Scenario0_patch, scenario_hab = Scenario0, suitable_tree = c(2, 4))
      Scenario1_patch_generalist <- Remove_patches(Scenario_patch = Scenario1_patch, scenario_hab = Scenario1, suitable_tree = c(2, 4))
      Scenario2_patch_generalist <- Remove_patches(Scenario_patch = Scenario2_patch, scenario_hab = Scenario2, suitable_tree = c(2, 4))
      Scenario3_patch_generalist <- Remove_patches(Scenario_patch = Scenario3_patch, scenario_hab = Scenario3, suitable_tree = c(2, 4))
      Scenario4_patch_generalist <- Remove_patches(Scenario_patch = Scenario4_patch, scenario_hab = Scenario4, suitable_tree = c(2, 4))
      
      Scenario0_patch_specialist <- Remove_patches(Scenario_patch = Scenario0_patch, scenario_hab = Scenario0, suitable_tree = 2)
      Scenario1_patch_specialist <- Remove_patches(Scenario_patch = Scenario1_patch, scenario_hab = Scenario1, suitable_tree = 2)
      Scenario2_patch_specialist <- Remove_patches(Scenario_patch = Scenario2_patch, scenario_hab = Scenario2, suitable_tree = 2)
      Scenario3_patch_specialist <- Remove_patches(Scenario_patch = Scenario3_patch, scenario_hab = Scenario3, suitable_tree = 2)
      Scenario4_patch_specialist <- Remove_patches(Scenario_patch = Scenario4_patch, scenario_hab = Scenario4, suitable_tree = 2)
      
      # paste all scenarios together #### 
      
      Scenario0[is.na(Scenario0)] <- 0
      Scenario1[is.na(Scenario1)] <- 0
      Scenario2[is.na(Scenario2)] <- 0
      Scenario3[is.na(Scenario3)] <- 0
      Scenario4[is.na(Scenario4)] <- 0
      
      Scenario0_patch_generalist[Scenario0_patch_generalist[] == "-Inf"] <- NA
      Scenario1_patch_generalist[Scenario1_patch_generalist[] == "-Inf"] <- NA
      Scenario2_patch_generalist[Scenario2_patch_generalist[] == "-Inf"] <- NA
      Scenario3_patch_generalist[Scenario3_patch_generalist[] == "-Inf"] <- NA
      Scenario4_patch_generalist[Scenario4_patch_generalist[] == "-Inf"] <- NA
      
      Scenario0_patch_specialist[Scenario0_patch_specialist[] == "-Inf"] <- NA
      Scenario1_patch_specialist[Scenario1_patch_specialist[] == "-Inf"] <- NA
      Scenario2_patch_specialist[Scenario2_patch_specialist[] == "-Inf"] <- NA
      Scenario3_patch_specialist[Scenario3_patch_specialist[] == "-Inf"] <- NA
      Scenario4_patch_specialist[Scenario4_patch_specialist[] == "-Inf"] <- NA
      
      Grid_ref <- str_sub(TreeCanopy, start = -6)
      filename <- paste(Grid_ref, Rep, TargetArea, sep = "_")
      setwd(Output_path)
      
      Tree_patch <-
        brick(
          Scenario0_patch_generalist,
          Scenario0_patch_specialist,
          Scenario1_patch_generalist,
          Scenario1_patch_specialist,
          Scenario2_patch_generalist,
          Scenario2_patch_specialist,
          Scenario3_patch_generalist,
          Scenario3_patch_specialist, 
          Scenario4_patch_generalist,
          Scenario4_patch_specialist
        )
      list <- c(
        "Scenario0_patch_generalist",
        "Scenario0_patch_specialist",
        "Scenario1_patch_generalist",
        "Scenario1_patch_specialist",
        "Scenario2_patch_generalist",
        "Scenario2_patch_specialist",
        "Scenario3_patch_generalist",
        "Scenario3_patch_specialist",
        "Scenario4_patch_generalist",
        "Scenario4_patch_specialist"
      )
      filename_patch <- paste(Grid_ref, Rep, TargetArea, "patch", ".csv", sep = "_")
      names(Tree_patch) <- paste(filename_patch, list, sep = '_')
      land_data_patch <- as.data.frame(freq(Tree_patch, merge = T))
      write.csv(land_data_patch, filename_patch, row.names = F)
      
      
      Tree_hab <-
        brick(
          Scenario0,
          Scenario1,
          Scenario2,
          Scenario3,
          Scenario4
        )
      list <- c("Scenario0",
                "Scenario1",
                "Scenario2",
                "Scenario3",
                "Scenario4")
      filename_hab <- paste(Grid_ref, Rep, TargetArea, "hab", ".csv", sep = "_")
      names(Tree_hab) <- paste(filename_hab, list, sep = '_')
      land_data_hab <- as.data.frame(freq(Tree_hab, merge = T))
      write.csv(land_data_hab, filename_hab, row.names = F)
      
      S0_spec <- as.data.frame(freq(Scenario0_patch_specialist))
      S0_spec <- subset(S0_spec, S0_spec$count>400)
      S0_spec <- subset(S0_spec, S0_spec$value != 0)
      S0_spec <- subset(S0_spec, S0_spec$value < 9000)
      if(length(S0_spec$value)>15){
      S0_spec <- sample_n(S0_spec, 15)
      S0_spec
      }else{
        S0_spec
      }
      S0_spec <- S0_spec[order(S0_spec$value),]
      S0_spec <- unname(unlist(S0_spec["value"]))
      filename_patch <- paste("patches", Rep, ".txt", sep = "")
      write.table(S0_spec, sep = " ", row.names = F, col.names = F, filename_patch)
      
      
      Scenario0[Scenario0[]==0] <- 5
      Scenario1[Scenario1[]==0] <- 5
      Scenario2[Scenario2[]==0] <- 5
      Scenario3[Scenario3[]==0] <- 5
      Scenario4[Scenario4[]==0] <- 5
      
      Scenario0_patch_generalist[is.na(Scenario0_patch_generalist)] <- 0
      Scenario1_patch_generalist[is.na(Scenario1_patch_generalist)] <- 0
      Scenario2_patch_generalist[is.na(Scenario2_patch_generalist)] <- 0
      Scenario3_patch_generalist[is.na(Scenario3_patch_generalist)] <- 0
      Scenario4_patch_generalist[is.na(Scenario4_patch_generalist)] <- 0
      
      Scenario0_patch_specialist[is.na(Scenario0_patch_specialist)] <- 0
      Scenario1_patch_specialist[is.na(Scenario1_patch_specialist)] <- 0
      Scenario2_patch_specialist[is.na(Scenario2_patch_specialist)] <- 0
      Scenario3_patch_specialist[is.na(Scenario3_patch_specialist)] <- 0
      Scenario4_patch_specialist[is.na(Scenario4_patch_specialist)] <- 0
      
      Tree <-
        brick(
          Scenario0,
          Scenario0_patch_generalist,
          Scenario0_patch_specialist,
          Scenario1,
          Scenario1_patch_generalist,
          Scenario1_patch_specialist,
          Scenario2,
          Scenario2_patch_generalist,
          Scenario2_patch_specialist,
          Scenario3,
          Scenario3_patch_generalist,
          Scenario3_patch_specialist,
          Scenario4,
          Scenario4_patch_generalist,
          Scenario4_patch_specialist
        )
      
      list <- c("Scenario0",
                "Scenario0_patch_generalist",
                "Scenario0_patch_specialist",
                "Scenario1",
                "Scenario1_patch_generalist",
                "Scenario1_patch_specialist",
                "Scenario2",
                "Scenario2_patch_generalist",
                "Scenario2_patch_specialist",
                "Scenario3",
                "Scenario3_patch_generalist",
                "Scenario3_patch_specialist", 
                "Scenario4",
                "Scenario4_patch_generalist",
                "Scenario4_patch_specialist"
      )
      filename <- paste(Rep, TargetArea, sep = "_")
      names(Tree) <- paste(filename, list, sep = '_')
      writeRaster(
        stack(Tree),
        names(Tree),
        bylayer = TRUE,
        NAflag = -9999,
        format = "ascii",
        overwrite = TRUE
      )
      
      
      
      
    } else{ #starts here if no riparian scenario
      
      # Now we need to create patch maps
      Patch_maps <- function(Scenario) {
        Scenario_patch <- Scenario # copy habitat map
        Scenario_patch[Scenario_patch[] == 1] <-
          0 ### converting all matrix cells to
        Scenario_patch <-
          clump(Scenario_patch) # clump patches (cells surrounded by matrix)
        ## Make an NA-value raster based on the LC raster attributes
        formask <- setValues(raster(Scenario_patch), NA)
        ## Assign 1 to formask to all cells corresponding to the forest class
        formask[Scenario0 != 0] <- 1
        clumpFreq <- freq(Scenario_patch)
        ## Coerce freq table to data.frame
        clumpFreq <- as.data.frame(clumpFreq)
        ## which rows of the data.frame are only represented by xx cell?
        ## Put these into a vector of clump ID's to be removed
        excludeID <- clumpFreq$value[which(clumpFreq$count < 5)]
        ## Assign NA to all clumps whose IDs are found in excludeID
        Scenario_patch[Scenario_patch %in% excludeID] <- NA
        
        return(Scenario_patch)
      }
      
      Scenario0_patch <-
        Patch_maps(Scenario = Scenario0)
      Scenario0_patch <-
        clump(Scenario0_patch) 
      #   sort numbering
      # renumber scenario 0 from 1
      # copy numbers to Scenario0 1, 2 and 3
      # new planted patches must be number 900xx (must be able to distinguish)
      # then save data set with size of each patch
      
      # restart patch count from 1
      
      
      Renumber_patch <- function(Scenario, Scenario0_patch) {
        Patch_map <- mosaic(Scenario, Scenario0_patch, fun = max) #mosaic together
        return(Patch_map) #return this
      }
      
      Scenario1_patch <-
        Renumber_patch(Scenario = Scenario1_new_patch, Scenario0_patch = Scenario0_patch)
      Scenario2_patch <-
        Renumber_patch(Scenario = Scenario2_new_patch, Scenario0_patch = Scenario0_patch)
      Scenario3_patch <-
        Renumber_patch(Scenario = Scenario3_new_patch, Scenario0_patch = Scenario0_patch)
      
      
      # scenario 2 expands patches so there are no 'new patches' therefore the new patches are given number of patch that was expanded ####
      Scenario2_patch[Scenario2_patch[]==0] <- NA
      xy <- as.data.frame(xyFromCell(Scenario2_patch, Scenario2_seedpoints))
      extracted = extract(x = Scenario2_patch, y = xy)
      Scenario2_patch_noNA <- Scenario2_patch # copy patch map
      Scenario2_patch_noNA[Scenario2_patch_noNA[] > 9000] <- NA #NA for all new patches
      Scenario2_patch_noNA[Scenario2_patch_noNA[] == "-Inf"] <- NA
      Scenario2_patch_noNA_filter <- Scenario2_patch_noNA
      formask_2 <- setValues(raster(Scenario2_patch_noNA_filter), NA)
      ## Assign 1 to formask to all cells corresponding to the forest class
      formask_2[Scenario0 != 0] <- 1
      clumpFreq_2 <- freq(Scenario2_patch_noNA_filter)
      ## Coerce freq table to data.frame
      clumpFreq_2 <- as.data.frame(clumpFreq_2)
      ## which rows of the data.frame are only represented by xx cell?
      ## Put these into a vector of clump ID's to be removed
      excludeID_2 <- clumpFreq_2$value[which(clumpFreq_2$count < 200)]
      ## Assign NA to all clumps whose IDs are found in excludeID
      Scenario2_patch_noNA_filter[Scenario2_patch_noNA_filter %in% excludeID_2] <- NA
      # then take the raster value with lowest distance to point AND non-NA value in the raster
      sampled <- apply(X = xy, MARGIN = 1, FUN = function(xy) Scenario2_patch_noNA_filter@data@values[which.min(replace(distanceFromPoints(Scenario2_patch_noNA_filter, xy), is.na(Scenario2_patch_noNA_filter), NA))]) #this is gives the cell ID of nearest patch
      sample.vec <- function(x, ...) x[sample(length(x), ...)]
      sampled <- unlist(lapply(sampled, sample.vec, 1)) #select 1 neighbor from each = NEW ID
      New_ID <- data.frame(extracted, sampled)
      Scenario2_patch_sub <- raster::subs(Scenario2_patch, y = New_ID, by = "extracted", which = "sampled") #replace extracted new patch ID with value of nearest raster
      Scenario2_patch <- mosaic(Scenario2_patch_sub, Scenario2_patch, fun = min) #mosaic together
      
      
      
      
      
      ### assign tree types 
      
      Scenario0[Scenario0[] == 2] <-
        sample(
          c(2, 3),
          size = length(Scenario0[Scenario0[] ==
                                    2]),
          replace  = T,
          prob = c(0.6, 0.4)
        )
      Scenario0[Scenario0[] == 2] <-
        sample(
          c(2, 4),
          size = length(Scenario0[Scenario0[] ==
                                    2]),
          replace  = T,
          prob = c(0.5, 0.5)
        )
      Scenario1[Scenario1[] == 2] <-
        sample(
          c(2, 3),
          size = length(Scenario1[Scenario1[] ==
                                    2]),
          replace  = T,
          prob = c(0.6, 0.4)
        )
      Scenario1[Scenario1[] == 2] <-
        sample(
          c(2, 4),
          size = length(Scenario1[Scenario1[] ==
                                    2]),
          replace  = T,
          prob = c(0.5, 0.5)
        )
      Scenario2[Scenario2[] == 2] <-
        sample(
          c(2, 3),
          size = length(Scenario2[Scenario2[] ==
                                    2]),
          replace  = T,
          prob = c(0.6, 0.4)
        )
      Scenario2[Scenario2[] == 2] <-
        sample(
          c(2, 4),
          size = length(Scenario2[Scenario2[] ==
                                    2]),
          replace  = T,
          prob = c(0.5, 0.5)
        )
      Scenario3[Scenario3[] == 2] <-
        sample(
          c(2, 3),
          size = length(Scenario3[Scenario3[] ==
                                    2]),
          replace  = T,
          prob = c(0.6, 0.4)
        )
      Scenario3[Scenario3[] == 2] <-
        sample(
          c(2, 4),
          size = length(Scenario3[Scenario3[] ==
                                    2]),
          replace  = T,
          prob = c(0.5, 0.5)
        )
      
      
      
      Scenario0[NFI_conifer_rast[] == 1 &
                  TreeCanopyRast[] == 2] <-
        1 #replace conifer plantations (is a tree and in conifer)
      Scenario1[NFI_conifer_rast[] == 1 & TreeCanopyRast[] == 2] <- 1
      Scenario2[NFI_conifer_rast[] == 1 & TreeCanopyRast[] == 2] <- 1
      Scenario3[NFI_conifer_rast[] == 1 & TreeCanopyRast[] == 2] <- 1
      
      
      # split corridors into sub-patches
      
      split_corridors <- function(Patch_map){
        
        freq <- as.data.frame(freq(Patch_map))
        freq <- subset(freq, freq$value > 9000)
        p <- data.frame(rasterToPoints(Patch_map))
        p <- subset(p, p$layer > 9000)
        p <- ddply(
          p,
          .var = "layer",
          .fun = function(p) {
            return(subset(p, x %in% min(x)))
          }
        )
        p <- ddply(
          p,
          .var = "layer",
          .fun = function(p) {
            return(subset(p, y %in% min(y)))
          }
        )
        p$cell_num <- cellFromXY(Patch_map, p)
        
        
        for(i in freq$value) {
          
          patch <- i
          patch_new <- patch + 10000
          size.clusters <- length(Patch_map[Patch_map[]== patch])/3
          seedpoint <- subset(p, p$layer == patch)
          Patch_map <-
            makePatch(
              Patch_map,
              spt = seedpoint$cell_num,
              size = size.clusters,
              bgr = patch,
              edge = FALSE,
              rast = TRUE,
              val = patch_new
            )
        }
        
        freq <- as.data.frame(freq(Patch_map))
        freq <- subset(freq, freq$value > 9000 & freq$value < 10000)
        p <- data.frame(rasterToPoints(Patch_map))
        p <- subset(p, p$layer > 9000 & p$layer < 10000)
        p <- ddply(
          p,
          .var = "layer",
          .fun = function(p) {
            return(subset(p, x %in% min(x)))
          }
        )
        p <- ddply(
          p,
          .var = "layer",
          .fun = function(p) {
            return(subset(p, y %in% min(y)))
          }
        )
        p$cell_num <- cellFromXY(Patch_map, p)
        
        for(j in freq$value) {
          patch <- j
          patch_new <- patch + 20000
          size.clusters <- length(Patch_map[Patch_map[]== patch])/2
          seedpoint <- subset(p, p$layer == patch)
          Patch_map <-
            makePatch(
              Patch_map,
              spt = seedpoint$cell_num,
              size = size.clusters,
              bgr = patch,
              edge = FALSE,
              rast = TRUE,
              val = patch_new)
        }
        
      }
      
      Scenario3_patch <- split_corridors(Patch_map = Scenario3_patch)
      
      
      
      
      
      # finally filter out species to create species and generalist maps
      
      
      
      
      Remove_patches <- function(Scenario_patch, scenario_hab, suitable_tree) {
        
        SPcountstats <-
          data.frame(zonal(
            match(scenario_hab, suitable_tree),
            #which patches contain tree type 
            Scenario_patch,
            #each forest patch is a zone
            fun = 'count',
            digits = 0,
            na.rm = TRUE
          ))
        #create replicate raster to work with
        excludeID_sp <-
          SPcountstats$zone[which(SPcountstats$count < 1)]  ## count of trees and update ID list
        
        ## Assign NA to all clumps whose IDs are found in excludeID
        Scenario_patch[Scenario_patch %in% excludeID_sp] <- NA
        return(Scenario_patch)
      }
      
      Scenario0_patch_generalist <- Remove_patches(Scenario_patch = Scenario0_patch, scenario_hab = Scenario0, suitable_tree = c(2, 4))
      Scenario1_patch_generalist <- Remove_patches(Scenario_patch = Scenario1_patch, scenario_hab = Scenario1, suitable_tree = c(2, 4))
      Scenario2_patch_generalist <- Remove_patches(Scenario_patch = Scenario2_patch, scenario_hab = Scenario2, suitable_tree = c(2, 4))
      Scenario3_patch_generalist <- Remove_patches(Scenario_patch = Scenario3_patch, scenario_hab = Scenario3, suitable_tree = c(2, 4))
      
      
      Scenario0_patch_specialist <- Remove_patches(Scenario_patch = Scenario0_patch, scenario_hab = Scenario0, suitable_tree = 2)
      Scenario1_patch_specialist <- Remove_patches(Scenario_patch = Scenario1_patch, scenario_hab = Scenario1, suitable_tree = 2)
      Scenario2_patch_specialist <- Remove_patches(Scenario_patch = Scenario2_patch, scenario_hab = Scenario2, suitable_tree = 2)
      Scenario3_patch_specialist <- Remove_patches(Scenario_patch = Scenario3_patch, scenario_hab = Scenario3, suitable_tree = 2)
      
      # paste all scenarios together #### 
      
      Scenario0[is.na(Scenario0)] <- 0
      Scenario1[is.na(Scenario1)] <- 0
      Scenario2[is.na(Scenario2)] <- 0
      Scenario3[is.na(Scenario3)] <- 0
      
      Scenario0_patch_generalist[Scenario0_patch_generalist[] == "-Inf"] <- NA
      Scenario1_patch_generalist[Scenario1_patch_generalist[] == "-Inf"] <- NA
      Scenario2_patch_generalist[Scenario2_patch_generalist[] == "-Inf"] <- NA
      Scenario3_patch_generalist[Scenario3_patch_generalist[] == "-Inf"] <- NA
      
      Scenario0_patch_specialist[Scenario0_patch_specialist[] == "-Inf"] <- NA
      Scenario1_patch_specialist[Scenario1_patch_specialist[] == "-Inf"] <- NA
      Scenario2_patch_specialist[Scenario2_patch_specialist[] == "-Inf"] <- NA
      Scenario3_patch_specialist[Scenario3_patch_specialist[] == "-Inf"] <- NA
      
      Grid_ref <- str_sub(TreeCanopy, start = -6)
      filename <- paste(Grid_ref, Rep, TargetArea, sep = "_")
      
      setwd(Output_path)
      
      
      Tree_patch <-
        brick(
          Scenario0_patch_generalist,
          Scenario0_patch_specialist,
          Scenario1_patch_generalist,
          Scenario1_patch_specialist,
          Scenario2_patch_generalist,
          Scenario2_patch_specialist,
          Scenario3_patch_generalist,
          Scenario3_patch_specialist
        )
      list <- c(
        "Scenario0_patch_generalist",
        "Scenario0_patch_specialist",
        "Scenario1_patch_generalist",
        "Scenario1_patch_specialist",
        "Scenario2_patch_generalist",
        "Scenario2_patch_specialist",
        "Scenario3_patch_generalist",
        "Scenario3_patch_specialist"
      )
      filename_patch <- paste(Grid_ref, Rep, TargetArea, "patch", ".csv", sep = "_")
      names(Tree_patch) <- paste(filename_patch, list, sep = '_')
      land_data_patch <- as.data.frame(freq(Tree_patch, merge = T))
      write.csv(land_data_patch, filename_patch, row.names = F)
      
      
      Tree_hab <-
        brick(
          Scenario0,
          Scenario1,
          Scenario2,
          Scenario3
        )
      list <- c("Scenario0",
                "Scenario1",
                "Scenario2",
                "Scenario3")
      filename_hab <- paste(Grid_ref, Rep, TargetArea, "hab", ".csv", sep = "_")
      names(Tree_hab) <- paste(filename_hab, list, sep = '_')
      land_data_hab <- as.data.frame(freq(Tree_hab, merge = T))
      write.csv(land_data_hab, filename_hab, row.names = F)
      
      S0_spec <- as.data.frame(freq(Scenario0_patch_specialist))
      S0_spec <- subset(S0_spec, S0_spec$count>400)
      S0_spec <- subset(S0_spec, S0_spec$value != 0)
      S0_spec <- subset(S0_spec, S0_spec$value < 9000)
      if(length(S0_spec$value)>15){
        S0_spec <- sample_n(S0_spec, 15)
        S0_spec
      }else{
        S0_spec
      }
      S0_spec <- S0_spec[order(S0_spec$value),]
      S0_spec <- unname(unlist(S0_spec["value"]))
      filename <- paste("patches", Rep, ".txt", sep = "")
      write.table(S0_spec, sep = " ", row.names = F, col.names = F, "filename")
      
      
      Scenario0[Scenario0[]==0] <- 5
      Scenario1[Scenario1[]==0] <- 5
      Scenario2[Scenario2[]==0] <- 5
      Scenario3[Scenario3[]==0] <- 5
      
      Scenario0_patch_generalist[is.na(Scenario0_patch_generalist)] <- 0
      Scenario1_patch_generalist[is.na(Scenario1_patch_generalist)] <- 0
      Scenario2_patch_generalist[is.na(Scenario2_patch_generalist)] <- 0
      Scenario3_patch_generalist[is.na(Scenario3_patch_generalist)] <- 0
      
      Scenario0_patch_specialist[is.na(Scenario0_patch_specialist)] <- 0
      Scenario1_patch_specialist[is.na(Scenario1_patch_specialist)] <- 0
      Scenario2_patch_specialist[is.na(Scenario2_patch_specialist)] <- 0
      Scenario3_patch_specialist[is.na(Scenario3_patch_specialist)] <- 0
      
      Tree <-
        brick(
          Scenario0,
          Scenario0_patch_generalist,
          Scenario0_patch_specialist,
          Scenario1,
          Scenario1_patch_generalist,
          Scenario1_patch_specialist,
          Scenario2,
          Scenario2_patch_generalist,
          Scenario2_patch_specialist,
          Scenario3,
          Scenario3_patch_generalist,
          Scenario3_patch_specialist
        )
      
      list <- c("Scenario0",
                "Scenario0_patch_generalist",
                "Scenario0_patch_specialist",
                "Scenario1",
                "Scenario1_patch_generalist",
                "Scenario1_patch_specialist",
                "Scenario2",
                "Scenario2_patch_generalist",
                "Scenario2_patch_specialist",
                "Scenario3",
                "Scenario3_patch_generalist",
                "Scenario3_patch_specialist"
      )
      names(Tree) <- paste(filename, list, sep = '_')
      writeRaster(
        stack(Tree),
        names(Tree),
        bylayer = TRUE,
        NAflag = -9999,
        format = "ascii",
        overwrite = TRUE
      )
    }

  }

