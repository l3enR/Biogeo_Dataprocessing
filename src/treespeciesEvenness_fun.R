######################################################
# #example for one plot
# entropy_plot1 <- read.csv(paste0(file_base, paste0("entropy/treeSpeciesEntropy_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)
# entropy_plot1 <- entropy_plot1$entropy[entropy_plot1$plotNumber == 1]
# trees_plot1 <- trees[trees$plot == 1,]
# n <- length(unique(trees_plot1$species))
# probability = 1/n
# entropy_val <- probability*log(probability, base = 2)
# max_entropy <- n * entropy_val * -1
# 
# evenness <- entropy$entropy[1]/max_entropy
######################################################

treespecies_evenness <- function(plotNumber, treeTable, entropy){
  #' @description Calculates the tree species evenness using shannon entropy in one plot. The maximum entropy is calculated by expecting the same distribution of the species (all with the same probability).
  #' 
  #' @param plotNumber The number of the plot as numeric argument
  #' @param treeTable A dataframe containing at least the plotnumber (header = plot).
  #' @param entropy The entropy of the plot as a dataframe with at least one column (header = plotNumber)
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  #load the entropy
  entropyInPlot <- entropy$entropy[entropy$plotNumber == plotNumber]
  #load the tree species of one plot
  treesInPlot <- treeTable[treeTable$plot == plotNumber,]
  #for maximum entropy:
  #calculate the number of elements
  n <- length(unique(treesInPlot$species))
  #calculate the probability of every species (all have the same probability)
  prob <- 1/n
  #calculate the logarithm for one species
  entropy_val <- prob * log(prob, base = 2)
  #calculate the max entropy by multiplication of n with the entropy values
  max_entropy <- n * entropy_val * -1
  #evenness
  evenness <- entropyInPlot/max_entropy
  
  return(evenness)
}