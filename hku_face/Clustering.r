install.packages("devtools")
devtools::install_github("HAOYU-LI/densityClust")
library(densityClust)

readTruePredLabels <- function(cur_data,pathToFile,sep = " "){
  
  # This function takes current dataset and true label csv file path, 
  # and return a list which contains true_label list and pred_label list.
  # Some true label = -1 mean that the corresponding image cannot be judged by human
  
  true_labels = read.delim(pathToFile, sep = sep)
  true_label_idx = c()
  for(i in 1:dim(true_labels)[1]){
    true_label_idx = c(true_label_idx,sub("_face.*","",sub("[a-zA-Z]*_[a-zA-Z]*/","",true_labels[i,1])))
  }
  
  true_label_ordered = c()
  for(i in 1:dim(cur_data)[1]){
    cur_index = sub("_face.*","",sub("[a-zA-Z]*_[a-zA-Z]*/","",cur_data[i,"V1"]))
    if(cur_index %in% true_label_idx){
      true_label = true_labels[which(true_label_idx %in% c(cur_index)),2]
    }else{
      true_label = -1
    }
    true_label_ordered = c(true_label_ordered,true_label)
  }
  return_lst = list()
  pred_label = cur_data[,"faceClust$clusters"]
  pred_label[pred_label!=1] = 0
  return_lst$pred_label = pred_label
  return_lst$true_label = true_label_ordered
  return (return_lst)
}

performance_metric <- function(true_labels, pred_labels, vis=F){
  ##
  ## return a list of performance assessments.
  ##
  table = table(true_labels, pred_labels)[c(-1),]
  if(dim(table)[1]==3){table = table[-1,]}
  if(vis){
    print(table)
  }
  ACC = (table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2]) #accuracy
  TPR = table[2,2]/(table[2,2]+table[2,1]) #True positive rate
  TNR = table[1,1]/(table[1,1]+table[1,2]) # True negative rate
  PPV = table[2,2]/(table[2,2]+table[1,2]) #positive predictive value
  FDR = table[1,2]/(table[1,2]+table[2,2]) # False Discovery rate
  F1_score = (2*PPV*TPR)/(PPV+TPR)
  return_lst = list()
  return_lst$Accuracy = ACC
  return_lst$TPR = TPR
  return_lst$TNR = TNR
  return_lst$PPV = PPV
  return_lst$FDR = FDR  
  return_lst$F1_score = F1_score
  return(return_lst)
}



##import Data##
data_1 = read.csv("C:/Users/hyli/Desktop/pair_name_feature/pair_id_2.csv",header = F)
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


cur_data = data_1[data_1$names_list==uniq_names[232],]

faceDist <- dist(cur_data[,-c(1:5)])
faceClust <- densityClust(faceDist, gaussian=TRUE)
plot(faceClust) # Inspect clustering attributes to define thresholds

faceClust <- findClusters(faceClust, rho=4, delta=0.8)
plotMDS(faceClust)
# plotTSNE(faceClust)
cur_data = cbind(faceClust$clusters,cur_data)

true_labels = read.delim("c:/Users/hyli/Desktop/true_label_232.txt", sep = " ")

true_label_idx = c()
for(i in 1:dim(true_labels)[1]){
  true_label_idx = c(true_label_idx,sub("_face.*","",sub("[a-zA-Z]*_[a-zA-Z]*/","",true_labels[i,1])))
}

true_label_ordered = c()
for(i in 1:dim(cur_data)[1]){
  cur_index = sub("_face.*","",sub("[a-zA-Z]*_[a-zA-Z]*/","",cur_data[i,"V1"]))
  
  if(cur_index %in% true_label_idx){
    true_label = true_labels[which(true_label_idx %in% c(cur_index)),2]
  }else{
    true_label = -1
  }
  true_label_ordered = c(true_label_ordered,true_label)
}
cur_data = cbind(true_label_ordered,cur_data)


pred_label = cur_data[,2]
pred_label[pred_label!=1] = 0
table = table(true_label_ordered, pred_label) 


ACC = (table[2,1]+table[3,2])/(table[2,2]+table[3,1]+table[2,1]+table[3,2])
TPR = table[3,2]/(table[3,2]+table[3,1])
TNR = table[2,1]/(table[2,1]+table[2,2])





