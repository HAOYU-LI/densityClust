install.packages("devtools")
devtools::install_github("HAOYU-LI/densityClust")
library(densityClust)

readTruePredLabels <- function(cur_data,pathToTruelab,sep = " ", rhoThre = 5, deltaThre=1, vis=F){
  
  # This function takes current dataset and true label csv file path, 
  # and return a list which contains true_label list and pred_label list.
  # Some true label = -1 mean that the corresponding image cannot be judged by human
  library(densityClust)
  faceDist <- dist(cur_data[,-c(1:5)])
  faceClust <- densityClust(faceDist, gaussian=TRUE)
  if(vis){plot(faceClust)} # Inspect clustering attributes to define thresholds
  
  faceClust <- findClusters(faceClust,rho = rhoThre,delta = deltaThre)
  
  if(vis){plotMDS(faceClust)}
  cur_data = cbind(faceClust$rho,faceClust$delta,cur_data)
  
  true_labels = read.delim(pathToTruelab, sep = sep)
  true_label_idx = c()
  for(i in 1:dim(true_labels)[1])
  {
    true_label_idx = c(true_label_idx,sub("_face.*","",sub("[a-zA-Z]*_[a-zA-Z]*/","",true_labels[i,1])))
  }
  
  true_label_ordered = c()
  for(i in 1:dim(cur_data)[1])
  {
    cur_index = sub("_face.*","",sub("[a-zA-Z]*_[a-zA-Z]*/","",cur_data[i,"V1"]))
    if(cur_index %in% true_label_idx){
      true_label = true_labels[which(true_label_idx %in% c(cur_index)),2]
    }else{
      true_label = -1
    }
    true_label_ordered = c(true_label_ordered,true_label)
  }
  return_lst = list()
  pred_label = c()
  for(index in 1:dim(cur_data)[1])
  {
    if(cur_data$`faceClust$rho`[index] >= rhoThre && cur_data$`faceClust$delta`[index] <= deltaThre )
    {
      pred_label <- c(pred_label,1)
    }else{
      pred_label <- c(pred_label,0)
    }
  }
  # print(cur_data)
  return_lst$pred_label = pred_label
  return_lst$true_label = true_label_ordered
  return (return_lst)
}

performance_metric <- function(true_labels, pred_labels, vis=F){
  ##
  ## return a list of performance assessments.
  ##
  table = table(true_labels, pred_labels)[c(-1),]
  if(dim(table)[1]==3){table[3,2] = sum(table[1,]);table = table[-1,]}
  if(vis){
    print(table)
  }
  ACC = (table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2]) #accuracy
  TPR = table[2,2]/(table[2,2]+table[2,1]) #True positive rate
  TNR = table[1,1]/(table[1,1]+table[1,2]) # True negative rate
  PPV = table[2,2]/(table[2,2]+table[1,2]) #positive predictive value
  FDR = table[1,2]/(table[1,2]+table[2,2]) # False Discovery rate
  F1_score = (2*PPV*TPR)/(PPV+TPR)
  return_lst = list(ACC,TPR,TNR,PPV,FDR,1-FDR,F1_score)
  names(return_lst) = c("Accuracy","TPR","TNR","PPV","FDR","purity","F1_score")  
  return(return_lst)
}



##import Data##
data_1 = read.csv("./pair_id_2.csv",header = F)
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

    faceClust <- findClusters(faceClust, rho=3)
    plotMDS(faceClust)
    labels_for_data_1 = c(labels_for_data_1,faceClust$clusters)
    # split(cur_data[,ncol(cur_data)], faceClust$clusters)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}




###########################Run clustering###########################

library(densityClust)
uniq_names = unique(names_list)
cur_data = data_1[data_1$names_list==uniq_names[232],]


for(thre in c(2,2.2,2.4,2.5,2.7,2.9,3,3.3))
{
  lst = readTruePredLabels(cur_data = cur_data,pathToTruelab = "./true_label_232.txt",rhoThre = thre,deltaThre = 1.4)
  metric_table = performance_metric(true_labels = lst$true_label,pred_labels = lst$pred_label,vis = F)
  cat("density threshold = ",thre,"; with final purity = ",metric_table$purity,".\n")
}









