######################################################
# #example for one plot

#trees from org/VersX_Trees.csv
# 
# trees_plot1 <- trees[trees$Plot == 1,]
# level <- unique(trees_plot1$level)
# 
# count <- c()
# 
# for(i in 1:length(level)){
#   count[i] <- nrow(trees_plot1[trees_plot1$level == level[i],])
# }
# 
# entropy <- data.frame(level = level, count = count, probability = NA, entropy = NA)
# 
# for(i in 1:nrow(entropy)){
#   entropy$probability[i] <- entropy$count[i] / sum(entropy$count)
#   entropy$entropy[i] <- entropy$probability[i] * log(entropy$probability[i], base = 2)
# }
# 
# #the entropy of the tree species for plot 1
# H1_treespecies_plot1 <- -sum(entropy$entropy)
######################################################

treelevel_entropy <- function(plotNumber, treeTable){
  #' @description Calculating the tree level entropy using a dataframe column of the level characteristics in one plot  
  #' 
  #' @param plotNumber The number of the plot as numeric argument
  #' @param treeTable A dataframe containing at least the plotnumber (header = Plot) and the tree species (header = species)
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  #load the tree species of one plot
  treesInPlot <- treeTable[treeTable$Plot == plotNumber,]
  #extract the unique species
  level <- unique(treesInPlot$level)
  #extract the number of the unique individuals 
  count <- c()
  for(i in 1:length(level)){
    count[i] <- nrow(treesInPlot[treesInPlot$level == level[i],])
  }
  #generating a dataframe containing the results for each individual species
  entropy <- data.frame(level = level, count = count, probability = NA, entropy = NA)
  #calculating the probability and the entropy
  for(i in 1:nrow(entropy)){
    entropy$probability[i] <- entropy$count[i] / sum(entropy$count)
    entropy$entropy[i] <- entropy$probability[i] * log(entropy$probability[i], base = 2)
  }
  #calculating the overall tree species entropy for the plot
  overallTreespEntropy <- -sum(entropy$entropy)
  return(overallTreespEntropy)
}