####Before using this script:##################

#1 the file base has to be changed to the personal directory
#2 the loaded .csv files in the "org" folder are extracted from the current version of the "Strukturplots_Tabelle"
#  every single excel sheet was extracted (just a copy) in a new excel file and was saved as .csv using the respective name
#3 the name of the current version has to be filled in  (with a leading 0 for one-digit numbers -> 05 instead of 5)

###############################################

<<<<<<< HEAD
file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeo_Dataprocessing/"
#file_base <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
=======
#file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeo_Dataprocessing/"
file_base <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
>>>>>>> 3d5887960317d2ece29e2a22b82afb66624ae670
currentVersion <- "07"
#--------------------------------------------------------------------
# 1 READ THE DATA

#general
general <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_general.csv")), sep = ";", stringsAsFactors = FALSE)

#trees
#load the processed table at 2.1
# trees <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_trees.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)
# treePlot <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_treesPlot.csv")), sep = ";", dec = ".", stringsAsFactors = FALSE)
# trees$plot <- NA
# for(i in 1:nrow(trees)){
#   trees$plot[i] <- treePlot$plotID[treePlot$treeID == trees$treeID[i]]
# }
# rm(treePlot)
# trees <- cbind(trees[10], trees[1:9])
# names(trees)[5] <- "species"
# names(trees)[8] <- "totalHeight"

#young trees
youngTrees <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_youngTrees.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)
youngTrees <- youngTrees[1:14,]

#herbals
herbals <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_herbals.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)

<<<<<<< HEAD
#deathwood
=======
>>>>>>> 3d5887960317d2ece29e2a22b82afb66624ae670
#load the processed table at 2.2
# deathwood <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_deathwood.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)

#--------------------------------------------------------------------

<<<<<<< HEAD
# 2 ASSIGN THE HIGHT LEVEL

=======
>>>>>>> 3d5887960317d2ece29e2a22b82afb66624ae670
# 2.1 assign the height level to each living tree (5m levels)

levels <- seq(from = 5, to = 45, by = 5)

#derivation
# trees$totalHeight[1] > levels
# levels[trees$totalHeight[1] > levels]
# length(levels[trees$totalHeight[1] > levels])
# length(levels[trees$totalHeight[1] > levels]) + 1

for(i in 1:nrow(trees)){
  trees$level[i] <- length(levels[trees$height[i] > levels]) + 1
}

# write.csv(trees, paste0(file_base, paste0("processed/treesWithLevels_vers", currentVersion, ".csv")), row.names = FALSE)
trees <- read.csv(paste0(file_base, paste0("processed/treesWithLevels_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)

#--------------------------------------------------------------------

# 2.2 assign the height level to each dead tree (5m levels)

#only the category 1 (standing tree) and 3 (lying tree) should be assigned to a height level
#-> other categories = NA
#standing trees could have a level > 1 -> biased by transversal lying trees which are defined as level 1

#smaller deathwood parts are not included in the form

levels <- seq(from = 5, to = 45, by = 5)

for(i in 1:nrow(deathwood)){
  if(deathwood$class[i] == 2 | deathwood$class[i] > 3){
    deathwood$level[i] <- NA
  }else if(deathwood$class[i] == 3){
    deathwood$level[i] <- 1
  }else{
    deathwood$level[i] <- length(levels[deathwood$length[i] > levels]) + 1
  }
}

# write.csv(deathwood, paste0(file_base, paste0("processed/deathwoodWithLevels_vers", currentVersion, ".csv")), row.names = FALSE)
<<<<<<< HEAD
deathwood <- read.csv(paste0(file_base, paste0("processed/deathwoodWithLevels_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)
=======
trees <- read.csv(paste0(file_base, paste0("processed/deathwoodWithLevels_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)
>>>>>>> 3d5887960317d2ece29e2a22b82afb66624ae670

#--------------------------------------------------------------------

# 3 STATISTICAL ANALYSIS

#needed parameters:
# tree species diversity    [v]
# tree level diversity      [v]
# tree condition diversity  [v]
# overall diversity         [ ]

# tree species evenness     [v]
# tree level evenness       [ ]
# tree condition evenness   [ ]
# overall evenness          [ ]

#shannon entropy

#as H = - E(from i= 1 to s)p(i)*log2(p(i)
#H ... Entropy
#i ... tree of the species x
#E ... sum from i = 1 to s
#s ... end value -> number of occuring tree species in the plot
#p(i) ... propability of i -> calculated by the percentage of the 
#         tree species in proportion to number of all trees
#
#exmpl. 10 trees total, 7 beech trees, 3 oak trees
#p(i = beech) = 7/10
#p(i = oak) = 3/10

#evenness

#as E = H/Hmax
#H...entropy as calculated above
#hmax... calculated by log(n)
# -> log(n) = equal distribution of the species number to the number of species
#
#exampl. 10 trees total, 7 beech trees, 3 oak trees
#Hmax = 0,5 for p(beech) and 0,5 for p(oak)
#--------------------------------------------------------------------

# 3.1 tree species entropy

#load tree species entropy function
source(paste0(file_base, "src/treespeciesEntropy_fun.R"))

#run the function and load the output
treespecies_entropy(treeTable = trees, outputFolder = paste0(file_base, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"))
totalTreespeciesEntropy <- read.csv(paste0(file_base, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.2 tree level entropy
#using living and dead trees

source(paste0(file_base, "src/treelevelEntropy_fun.R"))

treelevel_entropy(treeTable = trees, deathwoodTable = deathwood, outputFolder = paste0(file_base, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")))
totalTreeLevelEntropy <- read.csv(paste0(file_base, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.3 tree condition entropy
#only deathwood of the class 1 and 3 is assigned as condition 2 (dead)

source(paste0(file_base, "src/treeconditionEntropy_fun.R"))


treecondition_entropy(treeTable = trees, deathwoodTable = deathwood, outputFolder = paste0(file_base, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")))
totalTreeConditionEntropy <- read.csv(paste0(file_base, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.4 tree species evenness

#load tree species evenness function
source(paste0(file_base, "src/treespeciesEvenness_fun.R"))

#run the function and load the output
treespecies_evenness(treeTable = trees, 
                     entropy = read.csv(paste0(file_base, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE), 
                     outputFolder = paste0(file_base, "evenness/treeSpeciesEvenness_vers", currentVersion, ".csv"))

totalTreespeciesEntropy <- read.csv(paste0(file_base, "evenness/treeSpeciesEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)
