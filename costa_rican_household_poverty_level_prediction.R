library(stringr)
library(dplyr)
library(caTools)
library(caret)
library(randomForest)
library(cluster)
library(purrr)
library(factoextra)
library(ggplot2)
library(corrplot)



setwd("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code")


data_exploration_bar_graphs <- function()
{
  
  mydata = read.csv("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/pre_processed.csv")
  
  df_bar_1 <- mydata %>%
    group_by(target) %>%
    summarise(counts = n())
  
  bar_graph=ggplot(df_bar_1, aes(x = target, y = counts,fill=target)) +
    geom_bar(stat = "identity")+ggtitle('Distribution of individual poverty level')+
    scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a","#33a02c"))+xlab("Poverty level")+ylab("Number of individuals")+labs(fill="Poverty level")+theme(plot.title = element_text(hjust = 0.5))+ 
    geom_text(aes(label = counts), vjust = -0.3)
  print(bar_graph)
  
  
  household_head <- mydata %>%
    group_by(target, if_household_head) %>% 
    summarise(counts = n())
  
  df_bar_2<-household_head %>%
    filter(if_household_head == 1)  
  
  bar_graph2=ggplot(df_bar_2, aes(x = target, y = counts,fill = target)) +
    geom_bar(stat = "identity") + ggtitle('Distribution of Household poverty level')+
    scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a","#33a02c"))+xlab("Poverty level")+ylab("Number of households")+labs(fill="Poverty level")+theme(plot.title = element_text(hjust = 0.5))+ 
    geom_text(aes(label = counts), vjust = -0.3)
  
  print(bar_graph2)
  
  
}


correlation_plots<-function()
{

  mydata <- read.csv("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/pre_processed.csv")
  mydata2<- mydata %>% select(-id,-household_level_identifier,-target)
  mydata3<- mydata2 %>% select(c(1:ncol(mydata2)))
  corr_data_frame<- mydata2 %>% select(c(1:ncol(mydata2)))
  
  
  colnames(mydata3)<-c(1:ncol(mydata3))
  res <- cor(mydata3)
  write.csv(res,"correlation.csv")
  index <- which(res >= "0.98" & res!="1",  arr.ind=TRUE)
  index <- as.data.frame(index)
  index <- index[] + 1
  index["feature_names"] <- row.names(index)
  
  clean_list = list()
  count=1
  for (i in index["feature_names"])
  {
    for(j in i)
    {
      clean_list[count] = gsub("\\..*","",j)
      count=count+1
    }
    
  }
  
  clean_list <- unlist(clean_list)
  index["feature_names"] <- clean_list
  to_extract_column_names <- as.list(index["feature_names"])
  non_dup_features = to_extract_column_names[!duplicated(to_extract_column_names)]
  
  
  extracted_df <- corr_data_frame %>% select(c("size_of_the_household","household_size",
                                               "of_total_individuals_in_the_household",
                                               "total_persons_in_the_household",
                                               "toilet_connected_to_sewer_or_cesspool","if_household_head",
                                               "region_brunca",
                                               "if_widower",
                                               "no_main_source_of_energy_used_for_cooking_no_kitchen",
                                               "if_predominant_material_on_the_outside_wall_is_natural_fibers",
                                               "electricity_from_cooperative",
                                               "if_predominant_material_on_the_roof_is_natural_fibers",
                                               "if_predominant_material_on_the_floor_is_wood",
                                               "if_predominant_material_on_the_roof_is_fiber_cement_mezzanine"))
  
  colnames(extracted_df) <- c(1:ncol(extracted_df))
  corr_matrix <- cor(as.matrix(extracted_df))
  corrplot(corr_matrix, method="circle",type="upper")
}

poverty_level_prediction <- function()
{

oversample <- "YES"   #YES - Oversampling, NO - Undersampling, NONE - none of them 
normal_top_features <- TRUE  #FALSE IF ANSWERING RESEARCH QUESTION 3

df <- read.csv("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/docs/costa-rican-household-poverty-prediction/train_set.csv")

df2 <- data.frame()

lines <- readLines("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/column_names.txt")

new_columns = list()
i=1
for (line in lines)
{
  new_columns[i] <- (str_replace_all(tolower(line),"\\s+","_"))
  i=i+1
}
new_columns = unlist(new_columns, recursive = FALSE)
colnames(df) <- new_columns

write.csv(df,"changed_column_names.csv",row.names = FALSE)


if(FALSE){
# replacing rent not paid with zero
for (i in rownames(df["monthly_rent_payment"]))
{
  if(is.na(df[i,"monthly_rent_payment"]))
  {
    if(df[i,"own_and_fully_paid_house"]==1)
    {
        df[i,"monthly_rent_payment"] <- 0
    }
    
  }
  
  if(is.na(df[i,"number_of_tablets_household_owns"]))
  {
    if(df[i,"owns_a_tablet"]==0)
    {
      df[i,"number_of_tablets_household_owns"] <- 0
    }
  }
  
}

#decide to replace nan currently with zero in monthly_rent_payment or remove such rows
df$monthly_rent_payment[is.na(df$monthly_rent_payment)] <- 0 

}

df$monthly_rent_payment[is.na(df$monthly_rent_payment)] <- 0 
df$number_of_tablets_household_owns <- 0

#remove duplicate columns
df = df[,!duplicated(names(df))]

#remove useless columns
df = subset(df, select = -c(owns_a_tablet,square_of_the_mean_years_of_education_of_adults_greater_than_or_equal_to_18_in_the_household,
                            overcrowding_squared,hogar_total_squared,age_squared,years_of_schooling_squared,if_mobile_phone,
                            if_spouse_or_partner,if_son_or_doughter,if_stepson_or_doughter,if_son_or_doughter_in_law,if_grandson_or_doughter,
                            if_mother_or_father,if_father_or_mother_in_law,if_brother_or_sister,if_brother_or_sister_in_law,if_other_family_member,
                            if_other_non_family_member,dependency_squared,number_of_children_0_to_19_in_household_squared) )




#delete if column contains only a single value
df = df %>% select_if(~ length(unique(.)) > 1)

df$years_behind_in_school[is.na(df$years_behind_in_school)] <- 0
sum_years_behind_in_school_for_household = aggregate(df$years_behind_in_school, by=list(Category=df$household_level_identifier), FUN=mean)
mean_years_of_schooling = aggregate(df$years_of_schooling, by=list(household_group=df$household_level_identifier), FUN=mean)


for (value in sum_years_behind_in_school_for_household$Category)
{

  for (idx in which(df$household_level_identifier == value))
  {
    if(df[idx,"if_household_head"]==1)
    {
      df[idx,"years_behind_in_school"] = sum_years_behind_in_school_for_household$x[which(value == sum_years_behind_in_school_for_household$Category)]
      df[idx,"years_of_schooling"] = mean_years_of_schooling$x[which(value == sum_years_behind_in_school_for_household$Category)]
    }
    
  }
  
}


#replace values by looking at squared values
df$dependency<- as.character(df$dependency)
df$dependency[df$dependency=="yes"] <- 1
df$dependency[df$dependency=="no"] <- 0
df$dependency <- as.numeric(df$dependency)

df$edjefe<- as.character(df$edjefe)
df$edjefe[df$edjefe=="yes"] <- 1
df$edjefe[df$edjefe=="no"] <- 0
df$edjefe <- as.numeric(df$edjefe)

df$edjefa <- as.character(df$edjefa)
df$edjefa[df$edjefa=="yes"] <- 1
df$edjefa[df$edjefa=="no"] <- 0
df$edjefa <- as.numeric(df$edjefa)

df$target[df$target==1] <- "one"
df$target[df$target==2] <- "two"
df$target[df$target==3] <- "three"
df$target[df$target==4] <- "four"

df[is.na(df)] <- 0
df$target = factor(df$target)

write.csv(df,"C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/pre_processed.csv",row.names = FALSE)

data_exploration_bar_graphs()
correlation_plots()

#reduce features required for training by removing columns from highly correlated features
df = subset(df, select =-c(size_of_the_household,total_persons_in_the_household,toilet_connected_to_sewer_or_cesspool,region_brunca,if_widower,electricity_from_cooperative))




rf_df <- df %>% filter(if_household_head==1)
rf_df$target = factor(rf_df$target)

print("TOTAL HOUSEHOLDS NUMBER")
print(nrow(rf_df))


write.csv(rf_df,"C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/rf_df_household_head.csv",row.names = FALSE)


#oversampling

if(oversample=="YES")
{
one_df <- rf_df %>% filter(target=="one")
two_df <- rf_df %>% filter(target=="two")
three_df <- rf_df %>% filter(target=="three")

rf_df<-rbind(rf_df,one_df,one_df,two_df,three_df)
rf_df <- rf_df[sample(nrow(rf_df)),]


print("Number of rows of all classes in after oversampling")
print(nrow(rf_df))

print("oversampled rows 1 class")
print(nrow(one_df)*3)

print("oversampled rows 2 class")
print(nrow(two_df)*2)

print("oversampled rows 2 class")
print(nrow(three_df)*2)


}

#under-sampling
if(oversample=="NO")
{
  four_df <- rf_df %>% filter(target=="four")
  all_four_indexes <- which(rf_df$target=="four")

  print("Number of rows assigned to class 4")
  print(nrow(four_df))
  
  print("Number of rows assigned to class 4 after undersampling")
  print(round(0.35*nrow(four_df),0))
  
  four_df_index <- sample(nrow(four_df), round(0.35*nrow(four_df),0))
  
  four_df <- four_df[four_df_index,]
  rf_df   <- rf_df[-unlist(all_four_indexes),]
  rf_df   <- rbind(rf_df,four_df)
  rf_df   <- rf_df[sample(nrow(rf_df)),]
  
}


write.csv(rf_df,"C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/rf_df.csv",row.names = FALSE)

set.seed(5) 

#train set construction
one_df_train <- rf_df %>% filter(target=="one")
two_df_train <- rf_df %>% filter(target=="two")
three_df_train <- rf_df %>% filter(target=="three")
four_df_train <- rf_df %>% filter(target=="four")


print("SAMPLES BY CLASS - ONE, TWO, THREE, FOUR")
print(nrow(one_df_train))
print(nrow(two_df_train))
print(nrow(three_df_train))
print(nrow(four_df_train))


train_one_index <- sample(nrow(one_df_train), round(0.75*nrow(one_df_train),0))
train_two_index <- sample(nrow(two_df_train), round(0.75*nrow(two_df_train),0))
train_three_index <- sample(nrow(three_df_train), round(0.75*nrow(three_df_train),0))
train_four_index <- sample(nrow(four_df_train), round(0.75*nrow(four_df_train),0))


train_one <-one_df_train[train_one_index,]
train_two <- two_df_train[train_two_index,]
train_three <- three_df_train[train_three_index,]
train_four <- four_df_train[train_four_index,]


test_one <-one_df_train[-train_one_index,]
test_two <- two_df_train[-train_two_index,]
test_three <- three_df_train[-train_three_index,]
test_four_index <- four_df_train[-train_four_index,]

train <- rbind(train_one,train_two,train_three,train_four)
test  <- rbind(test_one,test_two,test_three,test_four_index)


#shuffling train and test
train <- train[sample(nrow(train)),]
test <- test[sample(nrow(test)),]


if(oversample=="NO")
{
  
  write.csv(train,"undersampled_train.csv",row.names = FALSE)
  write.csv(test,"undersampled_test.csv",row.names = FALSE)
  
  mydata = read.csv("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/undersampled_train.csv")
  df_some_plot_1 <- mydata %>%
    group_by(target) %>%
    summarise(counts = n())
  
  bar_graph_undersampled_train=ggplot(df_some_plot_1, aes(x = target, y = counts,fill=target)) +
    geom_bar(stat = "identity")+ggtitle("Distribution of household poverty level in train set after Under-sampling")+
    scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a","#33a02c"))+xlab("Poverty level")+ylab("Number of households")+labs(fill="Poverty level")+theme(plot.title = element_text(hjust = 0.5))+ 
    geom_text(aes(label = counts), vjust = -0.3)
  
  print(bar_graph_undersampled_train)
  
  mydata = read.csv("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/undersampled_test.csv")
  df_some_plot_2<- mydata %>%
    group_by(target) %>%
    summarise(counts = n())
  
  bar_graph_undersampled_test=ggplot(df_some_plot_2, aes(x = target, y = counts,fill=target)) +
    geom_bar(stat = "identity")+ggtitle("Distribution of household poverty level in test set after Under-sampling")+
    scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a","#33a02c"))+xlab("Poverty level")+ylab("Number of households")+labs(fill="Poverty level")+theme(plot.title = element_text(hjust = 0.5))+ 
    geom_text(aes(label = counts), vjust = -0.3)
  
  print(bar_graph_undersampled_test)
  
  
  
}


if(oversample=="YES")
{
  write.csv(train,"oversampled_train.csv",row.names = FALSE)
  write.csv(test,"oversampled_test.csv",row.names = FALSE)
  
  mydata = read.csv("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/oversampled_train.csv")
  df_some_plot_3 <- mydata %>%
    group_by(target) %>%
    summarise(counts = n())
  
  bar_graph_oversampled_train=ggplot(df_some_plot_3, aes(x = target, y = counts,fill=target)) +
    geom_bar(stat = "identity")+ggtitle("Distribution of household poverty level in train set after Oversampling")+
    scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a","#33a02c"))+xlab("Poverty level")+ylab("Number of households")+labs(fill="Poverty level")+theme(plot.title = element_text(hjust = 0.5))+ 
    geom_text(aes(label = counts), vjust = -0.3)
  print(bar_graph_oversampled_train)
  
  mydata = read.csv("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/oversampled_test.csv")
  df_some_plot_4 <- mydata %>%
    group_by(target) %>%
    summarise(counts = n())
  
  bar_graph_oversampled_test=ggplot(df_some_plot_4, aes(x = target, y = counts,fill=target)) +
    geom_bar(stat = "identity")+ggtitle("Distribution of household poverty level in test set after Oversampling")+
    scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a","#33a02c"))+xlab("Poverty level")+ylab("Number of households")+labs(fill="Poverty level")+theme(plot.title = element_text(hjust = 0.5))+ 
    geom_text(aes(label = counts), vjust = -0.3)
  
  print(bar_graph_oversampled_test)
}





train <- train %>% select(-id,-household_level_identifier)
test <- test %>% select(-id,-household_level_identifier)
test_rf <- test %>% select(-target)

write.csv(train,"C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/train.csv",row.names = FALSE)
write.csv(test,"C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/test.csv",row.names = FALSE)

columns_names_train = colnames(train)

if(FALSE)
{
control <- trainControl(method="repeatedcv", number=2, repeats=3, search="grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(target~., data=train, method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
}


if(TRUE){
  
  rf <- randomForest(
    target ~ .,
    data=train
  )
  
  print(rf)
  


  varImpPlot(rf,sort = TRUE)
  

  importance_df = varImp(rf)
  important_columns_list = list()
  features_importances = list()
  
  j=0
  
  indexes = order(importance_df["Overall"])
  for (i in indexes)
  {
    col_name = which(importance_df["Overall"] == importance_df[i,])
    important_columns_list[j] = columns_names_train[col_name]
    features_importances[j] = importance_df[i,"Overall"]
    j=j+1
  }

  names(features_importances) = important_columns_list
  
  if(normal_top_features)
  {
    top_features = tail(important_columns_list,15)
    
  }
  
  if(!normal_top_features)
  {
    top_features = as.list(c("edjefa","edjefe","years_of_education_of_male_head_of_household_squared","dependency","overcrowding","meaneduc","years_of_schooling","total_females_in_the_household","total_males_in_the_household","household_size","no_of_mobile_phones","if_the_household_has_notebook_or_desktop_computer"))        ##### ADD IF HERE AND REPLACE with custom features for Q3
  }
  
  
  unimportant_features <- columns_names_train[which(!columns_names_train %in% top_features)]
  
  
  train_features = append(top_features,"target")

  train <- subset(train, select = unlist(train_features))
  test <- subset(test, select = unlist(train_features))
  test_rf <- subset(test, select = unlist(top_features))
  
  rf <- randomForest(
    target ~ .,
    data=train
  )
  
  importances = varImp(rf)
  importances["features"] = rownames(importances)   
  
  write.csv(importances,"importances.csv",row.names = FALSE)
  
  varImpPlot(rf,sort = TRUE)
  
  plot(rf)
  
  #uncomment for plotting the legend
  #layout(matrix(c(1,2),nrow=1),
  #       width=c(4,1)) 
  #plot(rf, log="y")
  #plot(c(0,1),type="n", axes=F, xlab="", ylab="")
  #legend("top", colnames(rf$err.rate),fill=1:8)
  
  
  pred = predict(rf, newdata=test_rf,type="response",na.action = na.pass)
  
  actual = test["target"]
  
  write.csv(pred,"C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/predicted.csv",row.names = FALSE)
  write.csv(actual,"C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/actual.csv",row.names = FALSE)
  
  pred_df = read.csv("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/predicted.csv")
  actual_df = read.csv("C:/Users/chait/Desktop/Fall 2019 - Courses/STAT 515 - R programing/R final Project/code/actual.csv")
  
  
  print(confusionMatrix(pred_df[,"x"], actual_df[,"target"]))
  
  
}
} #function close


clustering <- function()
{
  
  importance_from_random_forest <- TRUE
  
  if(importance_from_random_forest)
  {
    df_new <- read.csv("importances.csv")
    important_features <- df_new["features"]  
  }
  
  if(!importance_from_random_forest)
  {
    important_features <- data.frame(c("edjefa","edjefe","years_of_education_of_male_head_of_household_squared","dependency","overcrowding","meaneduc","years_of_schooling","total_females_in_the_household","total_males_in_the_household","total_persons_in_the_household","no_of_mobile_phones","if_the_household_has_notebook_or_desktop_computer"))
  }

  rf_df <- read.csv("rf_df_household_head.csv")
  
  df = subset(rf_df, select = unlist(important_features))
  df["target"] = subset(rf_df, select = c(target))
  df[is.na(df)] <- 0
  
  
  set.seed(123)
  
  #train set construction
  one_df_train <- df %>% filter(target=="one")
  two_df_train <- df %>% filter(target=="two")
  three_df_train <- df %>% filter(target=="three")
  four_df_train <- df %>% filter(target=="four")
  
  train_one_index <- sample(nrow(one_df_train),nrow(one_df_train))
  train_two_index <- sample(nrow(two_df_train),nrow(two_df_train))
  train_three_index <- sample(nrow(three_df_train), nrow(three_df_train))
  train_four_index <- sample(nrow(four_df_train), nrow(four_df_train))
  
  train_one <-one_df_train[train_one_index,]
  train_two <- two_df_train[train_two_index,]
  train_three <- three_df_train[train_three_index,]
  train_four <- four_df_train[train_four_index,]
  
  kmeans_df <- rbind(train_one,train_two,train_three,train_four)
  kmeans_df <- kmeans_df[sample(nrow(kmeans_df)),]
  
  
  set.seed(123)

  if(TRUE){
    within_cluster_sum_of_squares <- function(cluster_number) {
      kmeans(scale(subset(kmeans_df, select = -c(id,target))), cluster_number, nstart = 10 )$tot.withinss
    }
    
    cluster_number.values <- 1:15
    within_cluster_sum_of_squares_values <- map_dbl(cluster_number.values, within_cluster_sum_of_squares)
    plot(cluster_number.values, within_cluster_sum_of_squares_values,
         type="b", frame = FALSE, 
         xlab="Number of clusters",
         ylab="Total within-clusters sum of squares")
    
  } 
  
  
  k_means_result <- kmeans(scale(subset(kmeans_df, select = -c(id,target))), centers = 8, nstart = 25)
  str(k_means_result)
  
  kmeans_df["allocated_cluster"] <- k_means_result$cluster
  
  print(fviz_cluster(k_means_result, data = subset(kmeans_df, select = -c(id,target)))+ggtitle(label='Visualization of clusters'))
  
  write.csv(kmeans_df,"cluster_outputs.csv",row.names = FALSE)
  
  print(table(kmeans_df[,"allocated_cluster"],kmeans_df[,"target"]))
  
}

poverty_level_prediction()
clustering()



