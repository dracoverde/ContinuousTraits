#You can use code you wrote for the correlation exercise here.
source("ContinuousFunctions")
# tree <- read.tree("____PATH_TO_TREE_OR_SOME_OTHER_WAY_OF_GETTING_A_TREE____")
# discrete.data <- read.csv(file="____PATH_TO_DATA_OR_SOME_OTHER_WAY_OF_GETTING_TRAITS____", stringsAsFactors=FALSE) #death to factors.
# continuous.data <- read.csv(file="____PATH_TO_DATA_OR_SOME_OTHER_WAY_OF_GETTING_TRAITS____", stringsAsFactors=FALSE) #death to factors.

data(whales)
tree <- whales
continuous.data <- rTraitCont(tree, model="BM")

cleaned.continuous <- CleanData(tree, continuous.data)

VisualizeData(tree, cleaned.continuous)


#First, start basic. What is the rate of evolution of your trait on the tree? 

BM1 <- fitContinuous(tree, cleaned.continuous, model="BM")
print(paste("The rate of evolution is", _____, "in units of", _______))
#Important: What are the rates of evolution? In what units?




datafixed <- cbind(rownames(continuous.data), continuous.data[,1])
solution <- rayDISC(tree,datafixed, ntraits=1, charnum=1, model=c("ER"))
OUwieTree <- solution$phy

trait <- as.data.frame(cbind(datafixed, continuous.data[,1]), stringsAsFactors=FALSE)
#OUwie runs:
#This takes longer than you may be used to. 
#We're a bit obsessive about doing multiple starts and in general
#performing a thorough numerical search. It took you 3+ years
#to get the data, may as well take an extra five minutes to 
#get an accurate answer
nodeBased.OUMV <- OUwie(OUwietree,trait,model="OUMV", simmap.tree=FALSE, diagn=FALSE)
print(nodeBased.OUMV)
#What do the numbers mean?

#Now run all OUwie models:
models <- c("BM1","BMS","OU1","OUM","OUMV","OUMA","OUMVA")
results <- lapply(models, RunSingleOUwieModel, phy=OUwietree, data=trait)

