#1 the file base has to be changed to the personal directory
#2 the loaded .csv files at the beginning are extracted from the current version of the "Strukturplots_Tabelle"
#  every excel sheet has to be extracted (just a copy) in a new excel file and than have to be saved as .csv using the respective name
file_base <- "F:/MODULE/07_Biogeographie/R/"
#--------------------------------------------------------------------
#read the table

general <- read.csv(paste0(file_base, "Vers05_general.csv"), sep = ";", stringsAsFactors = FALSE)

trees <- read.csv(paste0(file_base, "Vers05_Trees.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
trees <- trees[1:68,]

youngTrees <- read.csv(paste0(file_base, "Vers05_youngTrees.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
youngTrees <- youngTrees[1:14,]

herbals <- read.csv(paste0(file_base, "Vers05_herbals.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)

deathwood <- read.csv(paste0(file_base, "Vers05_deathwood.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)

#--------------------------------------------------------------------
#assign the height level to each tree (5m levels)
levels <- seq(from = 5, to = 45, by = 5)

# trees$totalHeight[1] > levels
# levels[trees$totalHeight[1] > levels]
# length(levels[trees$totalHeight[1] > levels])
# length(levels[trees$totalHeight[1] > levels]) + 1

for(i in 1:nrow(trees)){
  trees$level[i] <- length(levels[trees$totalHeight[i] > levels]) + 1
}

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
#statistical alalysis

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

######################################################
# #example for one plot

# trees_plot1 <- trees[trees$Plot == 1,]
# species <- unique(trees_plot1$species)
# 
# count <- c()
# 
# for(i in 1:length(species)){
#   count[i] <- nrow(trees_plot1[trees_plot1$species == species[i],])
# }
# 
# entropy <- data.frame(species = species, count = count, probability = NA, entropy = NA)
# 
# for(i in 1:nrow(entropy)){
#   entropy$probability[i] <- entropy$count[i] / sum(entropy$count)
#   entropy$entropy[i] <- entropy$probability[i] * log(entropy$probability[i], base = 2)
# }
# 
# #the entropy of the tree species for plot 1
# H1_treespecies_plot1 <- -sum(entropy$entropy)
######################################################

treespecies_entropy <- function(plotNumber, treeTable){
  #' @description Calculating the tree species entropy using a dataframe column of the species in one plot  
  #' 
  #' @param plotNumber The number of the plot as numeric argument
  #' @param treeTable A dataframe containing at least the plotnumber (header = Plot) and the tree species (header = species)
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  #load the tree species of one plot
  treesInPlot <- treeTable[treeTable$Plot == plotNumber,]
  #extract the unique species
  species <- unique(treesInPlot$species)
  #extract the number of the unique individuals 
  count <- c()
  for(i in 1:length(species)){
    count[i] <- nrow(treesInPlot[treesInPlot$species == species[i],])
  }
  #generating a dataframe containing the results for each individual species
  entropy <- data.frame(species = species, count = count, probability = NA, entropy = NA)
  #calculating the probability and the entropy
  for(i in 1:nrow(entropy)){
    entropy$probability[i] <- entropy$count[i] / sum(entropy$count)
    entropy$entropy[i] <- entropy$probability[i] * log(entropy$probability[i], base = 2)
  }
  #calculating the overall tree species entropy for the plot
  overallTreespEntropy <- -sum(entropy$entropy)
  return(overallTreespEntropy)
}

treespecies_entropy(plotNumber = 3, treeTable = trees)


generalTreespeciesEntropy <- data.frame(plotNumber = NA, entropy = NA)
for(i in 1:length(unique(trees$Plot))){
  if(i == 1){
    generalTreespeciesEntropy$plotNumber <- unique(trees$Plot)[i]
    generalTreespeciesEntropy$entropy <- treespecies_entropy(plotNumber = i, treeTable = trees)
  }else{
    temp_plotNumber <- unique(trees$Plot)[i]
    temp_entropy <- treespecies_entropy(plotNumber = i, treeTable = trees)
    generalTreespeciesEntropy[i,] <- c(temp_plotNumber, temp_entropy)
  }
}
# write.csv(generalTreespeciesEntropy, paste0(file_base, "treeSpeciesEntropy_vers5.csv"), row.names = FALSE)
# read.csv(paste0(file_base, "treeSpeciesEntropy_vers5.csv"), stringsAsFactors = FALSE)
