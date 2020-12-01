
##############################DAPC:Switchgrass Ecotype Calling##############################
#First, install package needed to run DAPC analysis
install.packages("adegenet")
library(adegenet)
#Next read in file containing phenotypic data
pheno<-read.csv("pheno_data_file.csv")
#Define prior groups for the DAPC by transforming the data using PCA then implementing k-means, a clustering algorithm
grp <- find.clusters(pheno, max.n.clust=10) #You will pick the number of clusters at this step, we chose n=3 to match the propsed ecotypes
#Run the DAPC using the prior groups defined in the last step
dapc1 <- dapc(pheno, grp$grp)
#Plot the results
scatter(dapc1)
compoplot(dapc1)#plots the data in a STRUCTURE like manner, looking at posterior membership assignment probabilites of each indvidual
#Extract the posterior membership probabilities of each individual and write the results
write.csv(dapc1$posterior,"posterior_mem_prob.csv")