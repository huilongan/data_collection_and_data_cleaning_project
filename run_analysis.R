 ##final project

#get current work_dir
getwd()
setwd("c:/users/an/desktop/data clean")
#get into the dataset dir
setwd(list.dirs()[4])
#integrate test set
setwd(list.dirs()[2])
#take out data
activity_labels=read.table("./activity_labels.txt")
activity_labels=dplyr::rename(activity_labels,act_label=V1,act_name=V2)
features=read.table("./features.txt")
test_subject=read.table("./test/subject_test.txt")
test_set=read.table("./test/x_test.txt")
test_labels=read.table("./test/y_test.txt")
#make data into one file
library("dplyr")
n_features=length(features[,1])
#change the variable names of test_set
names(test_set)=features[,2]
#find the index of col to be kept
kept_col_index=grep("(mean)|(std)(\\(\\))$",features[,2])
#make one file for test dataset and add a index to indicate weather it is a test data
test_data=test_set[,kept_col_index] %>% mutate(subject=test_subject[,1],act_label=test_labels[,1])%>%
  left_join(activity_labels,"act_label") %>% mutate(test_index=1)

# then go on the training data 

list.files("./train")
train_set=read.table("./train/X_train.txt")
train_labels=read.table("./train/y_train.txt")
train_subject=read.table("./train/subject_train.txt")
names(train_set)=features[,2]
train_data=train_set[,kept_col_index] %>% mutate(subject=train_subject[,1],act_label=train_labels[,1])%>%
  left_join(activity_labels,"act_label") %>% mutate(test_index=0)

#get the final result
data_set=rbind(train_data,test_data)

#clean the variable names
length(kept_col_index)
var_name_unclean=names(data_set)[1:55]

var_name_clean=c()
for (i in var_name_unclean){
  #step 1: remove () symbols
  i=gsub("\\(\\)","",i)
  #step 2: make f or t meaningful
  i=gsub("^(t)","Time.",i)
  i=gsub("^(f)","Freq.",i)
  #step 3: eliminate repeated Body
  i=gsub("([Bb]ody[Bb]ody)|([Bb]ody)","Body",i)
  #step 4: change std to "SD", the abbr. of standardized deviation
  i=gsub("std","SD",i)
  #step 5:change - to .
  i=gsub("-",".",i)
  #finally: store it in a new vector
  var_name_clean=c(var_name_clean,i)
}
names(data_set)[1:55]=var_name_clean

write.csv(data_set,"./clean_data.csv")

library("reshape")
data_set2=select(data_set,-c(act_label,test_index))
data_set2$subject=as.factor(data_set2$subject)
data_set2_melt=melt(data_set2,id=c("act_name","subject"))

data_set2_avg=reshape::cast(data_set2_melt,act_name+subject~variable,mean)
data_set2_avg$act_name=tolower(data_set2_avg$act_name)
write.csv(data_set2_avg,"./avg_data.csv")
