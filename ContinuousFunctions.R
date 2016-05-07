library(ape) #utility fns
library(geiger) #utilty fns
library(OUwie)
library(BAMMtools)

#You can use code you wrote for the correlation exercise here.


VisualizeData <- function(phy, data) {
  #Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
  
  plot.phylo(phy, type="fan", cex=0.2)
  # note to self, don't put the object in quotes
  
  print(paste("The tree has ", Ntip(phy), " terminals and ", 
              Nnode(phy), " internal nodes out of ",Ntip(phy)-2,
              " possible, which means it is ", 
              round(100*(Nnode(phy)-1)/(Ntip(phy)-3), 2),
              "% resolved", sep=""))
  name.check(phy, data, data.names=NULL)
}

CleanData <- function(phy, data) {
  #treedata() in Geiger is probably my favorite function in R.
  treedata(phy, data, sort=FALSE, warnings=TRUE)
}

RunSingleOUwieModel<-function(model, phy, data) {
	print(paste("Now starting model",model))
	return(OUwie(phy, data, model, simmap.tree=FALSE, diagn=FALSE))	
}
