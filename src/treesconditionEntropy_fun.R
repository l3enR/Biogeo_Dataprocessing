######################################################
# #example for one plot

#trees from org/VersX_Trees.csv

# condition <- c("livingTrees", "deathwood")
# 
# livingTrees_plot1 <- nrow(trees[trees$Plot == 1,])
# 
# deathwood_plot1 <- deathwood[deathwood$Plot == 1,]
# deathwood_plot1 <- nrow(deathwood_plot1[deathwood_plot1$class == 1 | deathwood_plot1$class == 3,])
# 
# count <- c(NA,NA)
# count[1] <- livingTrees_plot1
# count[2] <- deathwood_plot1
# 
# entropy <- data.frame(condition = condition, count = count, probability = NA, entropy = NA)
# 
# for(i in 1:nrow(entropy)){
#   entropy$probability[i] <- entropy$count[i] / sum(entropy$count)
#   entropy$entropy[i] <- entropy$probability[i] * log(entropy$probability[i], base = 2)
#   if(is.na(entropy$entropy[i])){
#     entropy$entropy[i] <- 0
#   }
# }
# 
# #the entropy of the tree species for plot 1
# H1_treespecies_plot1 <- -sum(entropy$entropy)
######################################################

treecondition_entropy <- function(plotNumber, treeTable, deathwoodTable){
  #' @description Calculates the tree species entropy using a dataframe column of the species in one plot  
  #' 
  #' @param plotNumber The number of the plot as numeric argument
  #' @param treeTable A dataframe containing at least the plotnumber (header = plot) and the tree species (header = species)
  #' @param deathwoodTable A dataframe containing at least the plotnumber (header = plot) and the class (header = class)
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  #get the number of living trees in one plot
  livingTrees <- nrow(treeTable[treeTable$Plot == plotNumber,])
  
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