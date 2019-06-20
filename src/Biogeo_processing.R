####Before using this script:##################

#1 the file base has to be changed to the personal directory
#2 the loaded .csv files in the "org" folder are extracted from the current version of the "Strukturplots_Tabelle"
#  every single excel sheet was extracted (just a copy) in a new excel file and was saved as .csv using the respective name
#3 the name of the current version has to be filled in  (with a leading 0 for one-digit numbers -> 05 instead of 5)

###############################################

file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeo_Dataprocessing/"
currentVersion <- "06"
#--------------------------------------------------------------------
#read the table

general <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_general.csv")), sep = ";", stringsAsFactors = FALSE)

trees <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_trees.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)
trees <- trees[1:68,]

youngTrees <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_youngTrees.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)
youngTrees <- youngTrees[1:14,]

herbals <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_herbals.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)

deathwood <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_deathwood.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)

#--------------------------------------------------------------------
#assign the height level to each tree (5m levels)
levels <- seq(from = 5, to = 45, by = 5)

#derivation
# trees$totalHeight[1] > levels
# levels[trees$totalHeight[1] > levels]
# length(levels[trees$totalHeight[1] > levels])
# length(levels[trees$totalHeight[1] > levels]) + 1

for(i in 1:nrow(trees)){
  trees$level[i] <- length(levels[trees$totalHeight[i] > levels]) + 1
}

# write.csv(trees, paste0(file_base, paste0("processed/treesWithLevels_vers", currentVersion, ".csv")), row.names = FALSE)
# trees <- read.csv(paste0(file_base, paste0("processed/treesWithLevels_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)

#--------------------------------------------------------------------
#assign the height level to each dead tree (5m levels)
#the values are not useful because the lenght was summed up for treetrunks
#only tree trunks and standing trees have a level > 1
# -> the length is the indicator for the height
#smaller deathwood parts are not included in the form
levels <- seq(from = 5, to = 45, by = 5)


for(i in 1:nrow(deathwood)){
  if(deathwood$class[i] == 1 | deathwood$class[i] == 3){#####IS THIS VALUES CORRECT?
    deathwood$level[i] <- length(levels[deathwood$length[i] > levels]) + 1
  }else{
    deathwood$level[i] <- 1
  }
}
#--------------------------------------------------------------------
#STATISTICAL ANALYSIS

#needed parameters:
# tree species diversity    [v]
# tree level diversity      [?] does the deathwood also count? (in this case all instead of one tree in level 1) -> class 1+3
# tree condition diversity  [v] deathwood = of class 1(standing tree) or 3(lying tree)
# overall diversity         [ ]

# tree species evenness     [ ]
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

#--------------------------------------------------------------------

# 1 tree species entropy

#load tree species entropy function
source(paste0(file_base, "src/treespeciesEntropy_fun.R"))
#test
treespecies_entropy(plotNumber = 1, treeTable = trees)

totalTreespeciesEntropy <- data.frame(plotNumber = NA, entropy = NA)
for(i in 1:length(unique(trees$plot))){
  if(i == 1){
    totalTreespeciesEntropy$plotNumber <- unique(trees$plot)[i]
    totalTreespeciesEntropy$entropy <- treespecies_entropy(plotNumber = i, treeTable = trees)
  }else{
    temp_plotNumber <- unique(trees$plot)[i]
    temp_entropy <- treespecies_entropy(plotNumber = i, treeTable = trees)
    totalTreespeciesEntropy[i,] <- c(temp_plotNumber, temp_entropy)
  }
}
# write.csv(totalTreespeciesEntropy, paste0(file_base, paste0("entropy/treeSpeciesEntropy_vers", currentVersion, ".csv")), row.names = FALSE)
# totalTreespeciesEntropy <- read.csv(paste0(file_base, paste0("entropy/treeSpeciesEntropy_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 2 tree level entropy

source(paste0(file_base, "src/treelevelEntropy_fun.R"))
#test
treelevel_entropy(plotNumber = 1, treeTable = trees)

totalTreeLevelEntropy <- data.frame(plotNumber = NA, entropy = NA)
for(i in 1:length(unique(trees$plot))){
  if(i == 1){
    totalTreeLevelEntropy$plotNumber <- unique(trees$plot)[i]
    totalTreeLevelEntropy$entropy <- treelevel_entropy(plotNumber = i, treeTable = trees)
  }else{
    temp_plotNumber <- unique(trees$plot)[i]
    temp_entropy <- treelevel_entropy(plotNumber = i, treeTable = trees)
    totalTreeLevelEntropy[i,] <- c(temp_plotNumber, temp_entropy)
  }
}

# write.csv(totalTreeLevelEntropy, paste0(file_base, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")), row.names = FALSE)
# totalTreeLevelEntropy <- read.csv(paste0(file_base, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3 tree condition entropy

#trees of the class 1 and 3 are used in the calculation as dead trees
source(paste0(file_base, "src/treeconditionEntropy_fun.R"))
#test
treecondition_entropy(plotNumber = 2, treeTable = trees, deathwoodTable = deathwood)

totalTreeConditionEntropy <- data.frame(plotNumber = NA, entropy = NA)
for(i in 1:length(unique(trees$plot))){
  if(i == 1){
    totalTreeConditionEntropy$plotNumber <- unique(trees$plot)[i]
    totalTreeConditionEntropy$entropy <- treecondition_entropy(plotNumber = i, treeTable = trees, deathwoodTable = deathwood)
  }else{
    temp_plotNumber <- unique(trees$plot)[i]
    temp_entropy <- treecondition_entropy(plotNumber = i, treeTable = trees, deathwoodTable = deathwood)
    totalTreeConditionEntropy[i,] <- c(temp_plotNumber, temp_entropy)
  }
}

# write.csv(totalTreeConditionEntropy, paste0(file_base, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")), row.names = FALSE)
# totalTreeConditionEntropy <- read.csv(paste0(file_base, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------
