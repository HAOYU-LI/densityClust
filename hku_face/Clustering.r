install.packages("devtools")
devtools::install_github("HAOYU-LI/densityClust")
library(densityClust)

##import Data##
data_1 = read.csv("../pair_name_feature/pair_id_2.csv",header = F)

####################Add names to each data point####################
ppl_names = data_1[,1]
names_list = c()
idx = 0
for (name in ppl_names)
{
  idx = idx + 1
  name = sub("/.*","",ppl_names[idx])
  names_list = c(names_list , name)
  # data_1[idx,] =c(name,data_1[idx,])
}
data_1 = cbind(names_list, data_1)
####################Add names to each data point####################


###########################Run clustering###########################
uniq_names = unique(names_list)
labels_for_data_1 = c()
for (i in 1:length(uniq_names))
{
  tryCatch({
    cat("at i = ", i ," : \n")

    cur_data = data_1[data_1$names_list==uniq_names[i],]

    faceDist <- dist(cur_data[,-c(1:5)])
    faceClust <- densityClust(faceDist, gaussian=TRUE)
    plot(faceClust) # Inspect clustering attributes to define thresholds

    faceClust <- findClusters(faceClust, rho=4, delta=0.9)
    plotMDS(faceClust)
    labels_for_data_1 = c(labels_for_data_1,faceClust$clusters)
    # split(cur_data[,ncol(cur_data)], faceClust$clusters)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}




###########################Run clustering###########################

cur_data = data_1[data_1$names_list==uniq_names[232],]

faceDist <- dist(cur_data[,-c(1:5)])
faceClust <- densityClust(faceDist, gaussian=TRUE)
plot(faceClust) # Inspect clustering attributes to define thresholds

faceClust <- findClusters(faceClust, rho=4, delta=0.8)
plotMDS(faceClust)
plotTSNE(faceClust)

cur_data = cbind(faceClust$clusters,cur_data)

write.csv(data_1)
