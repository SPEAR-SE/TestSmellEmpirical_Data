setwd("")
library(aod)
library(ggplot2)
library(rsq)
library(MASS)
library(Hmisc)
require(broom) # for tidy()
require(knitr) # for kable()
library(tidyr)
library(pROC)
roc <- pROC::roc
require(fmsb)
library(arrangements)
library(xtable)
library(gtools)

library(ggplot2)   # for awesome plotting
library(caret)     # for automating the tuning process

# Model interpretability packages
library(vip)       # for variable importance
library(pdp)       # for variable relationships
library(rms)
library(VIF)
library(rgl)
library(dplyr)


vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    if(!is.na(VIF(lm(form_in, data = in_frame, ...))))
      vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        if(!is.na(vif_add))
          vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    selected <- names(in_dat)
    return(selected)
    
  }
}


# IMPUTE RARE EVENTS BY USE COPY
removingRareEvents <- function(data){
  lengthForNewFeatures <- length(data)
  for (i in 1:lengthForNewFeatures){
    if (length(data[data[,i] > 1,][,i]) > 0){
      data[data[,i] > 1,][,i] = 1
    }
  }
  return(data)
}


# REMOVE CORRELATED AND REDUNDANT PREDICTORS
# using the f
# i) removing factors with zero variance; 
# ii) removing highly correlated factors; 
# iii) and removing redundant factors.
removeRedun <- function(data){
  
  remainedFactors <- names(data)
  
  formulaStr <-paste("", paste(remainedFactors, collapse=" + "), sep=" ~ ")
  formula <-as.formula(formulaStr)
  redundant<-redun(formula, data=data, r2=0.9, nk=0)
  remainedXFeature <- setdiff(remainedFactors,names(redundant))
  
  
  data = data[,remainedXFeature]
  
  
  return(data)
  
}


# NORMALITY ADJUSTMENT BY USING LOG(X+1)
normalityAdjustment <- function(data){
  for (j in 1:length(data)){
    # print(names(data[j]))
    data[,j] = log(data[,j]+1)
  }
  return(data)
}


adjustedRsq <- function(rfit){
  null <- (rfit$null.deviance/-2) 
  proposed <- (rfit$deviance/-2)
  r <- (null - proposed) / null
  return(r)
}



removeZeroVariance <- function(data){
  
  newData<-as.data.frame(data[,(apply(data, 2,function(x) length(unique((x)))) == 1)==FALSE])
  return(newData)
}

subsetBasedOnProjects <- function(data){
  projects = unique(data$App)
  temp_list = list()
  for (i in 1:length(projects)){
    tmp = list(data[(data$App == projects[i]),])
    temp_list <- c(temp_list, tmp)
  }
  names(temp_list) = projects
  return(temp_list)
}


# ===== THIS IS THE MAIN PROGRAM =====
main <- function(data,modelName){
  data = df$accumulo
  smell_prod_index <- which(colnames(data)=="Assertion.Roulette")
  smell_process_added_index <- which(colnames(data)=="Assertion.Roulette_added")
  smell_process_removed_index <- which(colnames(data)=="Assertion.Roulette_removed")
  smell_process_removed_index_end <- which(colnames(data)=="tt")
  

  # ==== traditional metrics ======
  
  # Data composition
  LOC <- data$LOC
  pre_bug <- data$pre_bug
  post_bug <-data$post_bug
  post_bug[post_bug >= 1] <- 1
  codeChurn<-data$code_churned
  deletedLine<-data$deleted_line
  fileChurn<-data$file_churned
  # experience <- data$experience
  ts_coupling_prod <-data$st
  tt_coupling_prod <-data$tt

  
  # Data
  SMELL_SNAP_ALL <- data[c(smell_prod_index:(smell_process_added_index-1))]
  SMELL_ADDED_ALL <-  data[c(smell_process_added_index:(smell_process_removed_index-1))]
  SMELL_REMOVED_ALL <- data[c(smell_process_removed_index:(smell_process_removed_index_end-1))]

  # Additive
  # BASE <- as.data.frame(cbind(LOC, pre_bug,codeChurn, deletedLine, fileChurn,experience, ts_coupling_prod, tt_coupling_prod))
  BASE <- as.data.frame(cbind(LOC, pre_bug,codeChurn, deletedLine, fileChurn, ts_coupling_prod, tt_coupling_prod))
  BASE_SMELL_SNAP_ALL <- as.data.frame(cbind(BASE,SMELL_SNAP_ALL))
  BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL <- as.data.frame(cbind(BASE,SMELL_SNAP_ALL,SMELL_ADDED_ALL,SMELL_REMOVED_ALL))
  
  # # Stepwise
  # BASE_processed <-  preprocess(BASE)
  # BASE_processed$post <- post_bug
  # MODEL_1 <-  buildModel(BASE_processed)
  # modelPerformance(MODEL_1,BASE_processed,paste(modelName,"_BASE"))
  # 
  # BASE_SMELL_SNAP_ALL_processed <-  preprocess(BASE_SMELL_SNAP_ALL)
  # BASE_SMELL_SNAP_ALL_processed$post <- post_bug
  # MODEL_2 <-  buildModel(BASE_SMELL_SNAP_ALL_processed)
  # modelPerformance(MODEL_2,BASE_SMELL_SNAP_ALL_processed,paste(modelName,"_BASE_PRODUCT"))
  
  BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL_processed <-  preprocess(BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL)
  BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL_processed$post <- post_bug
  # MODEL_3 <-  buildModel(BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL_processed)
  # modelPerformance(MODEL_3,BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL_processed,paste(modelName,"_BASE_PRODUCT_PROCESS"))
  
  # Effect
  base = effectSize(BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL_processed, 1)
  base_1.1 = effectSize(BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL_processed, 1.1)
  base_1.5 = effectSize(BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL_processed, 1.5)
  base_1.5 = effectSize(BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL_processed, 10)
  write.csv(cbind(base,base_1.1,base_1.5),paste(modelName,"_effect.csv"))
  

  return(MODEL_3)
}

crossProject <- function(){
  
}


standardizeToTheirMean <- function(data)
{
 return(t(as.matrix(apply(data, 2,function(x) mean(x)))))
}

getBaseIndex<- function(meanData){
  d <- names(as.data.frame(meanData))
  n = c("deletedLine","fileChurn", "ts_coupling_prod", "tt_coupling_prod")
  for(i in seq(length(n),0)){
    if(n[i]%in% d){
      return(grep(n[i],d))
    }
  }
  
}

effectSize <- function(data, increaseEffect){
  data = BASE_SMELL_SNAP_ALL_ADDED_ALL_REMOVED_ALL_processed
  increaseEffect = 1000
  predictors <- data[1:length(data)-1]
  target <- data[length(data)]
  meanData = standardizeToTheirMean(predictors)

  # effect size modelling

  model <- buildModel(data)
  indexB = getBaseIndex(meanData)
  indexA = grep("added",colnames(meanData))
  indexR = grep("removed",colnames(meanData))
  

  BASE = t(as.matrix(meanData[,c(1:indexB)]))
  PRODUCT = t(as.matrix(meanData[,c((indexB+1):(indexA[1]-1))]))
  PROCESS_REMOVED = t(as.matrix(meanData[,c(indexR)]))
  PROCESS_ADDED = t(as.matrix(meanData[,c(indexA)]))
  
  baseEffect = cbind(BASE, PRODUCT,PROCESS_ADDED,PROCESS_REMOVED)

  # pBase = predict(model, as.data.frame(baseEffect), type = "response")
  newData = data[,FALSE]
  for (i in seq((indexB+1), length(baseEffect))){
    increased <- baseEffect
    increased[,i] <- increased[,i]*increaseEffect
    newData <- rbind(newData, increased)
  }
  predictions <- predict(model, newData, type = "response")
  names(predictions) <- c(colnames(PRODUCT),colnames(PROCESS_REMOVED),colnames(PROCESS_ADDED))
    
  lrt <- anova(model, test="LRT") # compute LRT
  lrt <- lrt[(lrt[,5] <= 0.05) == TRUE,]
  lrt <- lrt[rownames(lrt)[1:length(rownames(lrt))] %in% rownames(lrt)[2:length(rownames(lrt))],]
  
  
  predictions = as.data.frame(predictions)
  selectedRows = (rownames(predictions) %in% rownames(lrt))
  rowNames = rownames(predictions)[selectedRows]
  predictions <- as.data.frame(predictions[selectedRows,])
  rownames(predictions) <- rowNames
  colnames(predictions) <- increaseEffect
  return(predictions)
}


modelPerformance <- function(model, d, modelName){
  library(pROC)
  prediction = predict(model, as.data.frame(d), type = "response")
  d$prediction=prediction
  ROC <- roc(post > 0 ~ prediction, data = d)
  LRT <- anova(model, test="LRT") # compute LRT
  LRT <- LRT[(LRT$`Pr(>Chi)` <= 0.05) == TRUE,] # get significant 
  LRT <- LRT[2:nrow(LRT),] # remove NA in first row
  OR = as.data.frame(summary(model)$coefficients)[((rownames(summary(model)$coefficients) %in% rownames(LRT))),]
  resultOR = cbind("modelType"=modelName,data.frame("feature"=rownames(LRT),"OR"=OR[,1],"X"=as.matrix(LRT$Deviance),"DF"=as.matrix(LRT$Df)))
  resultAUC = rbind(data.frame("modelType"=modelName,"auc" = ROC$auc, "null deviance"=model$null.deviance, "residual deviance"=model$deviance))
  
  write.csv(resultOR, paste(modelName,"_OR_X.csv"), row.names = FALSE)
  write.csv(resultAUC, paste(modelName,"_AUC.csv"), row.names = FALSE)
}

scaling <- function(df_){
  data = df_[9:(length(df_)-1)]
  data = abs(data)
  data[is.na(data)] <- 0
  rowName =  names(data)
  data <- normalityAdjustment(data)
  data$post = df_$post_bug
  names(data)[names(data) == "deleted_line"] <- "deletedLine"
  names(data)[names(data) == "file_churned"] <- "fileChurn"
  names(data)[names(data) == "st"] <- "ts_coupling_prod"
  names(data)[names(data) == "tt"] <- "tt_coupling_prod"
  names(data)[names(data) == "code_churned"] <- "codeChurn"
  return(data)
  
}

preprocess <- function(df){
  data = df
  data[is.na(data)] <- 0
  data = abs(data)
  rowName =  names(data)
  
  # [2] get current model
  data <- removeZeroVariance(data)
  # data <- select(data,-c(nearZeroVar(data,freqCut = nrow(data)/3)))
  
  data <- normalityAdjustment(data)
  
  # [4] remove multicollnearity
  selected <- vif_func(data)
  data <- data[(names(data) %in% selected) == TRUE]
  
  # [5] stepwise elimination to remove redundant factors - if a predictor can be predicted using other predictors then we remove
  data = removeRedun(data) # no redundant variables
  
  return(data)
}

buildModel <- function(data){
  predictorsName = names(data)[1:length(data)-1]
  form <- paste(predictorsName, collapse = '+')
  form_eq <- formula(paste('post > 0', '~', form)) # setting the regression formula
  model <- glm(form_eq, data=data, family=binomial(link='logit')) # compute regression
  return(model)
}

crossProjectPredictionHelper <- function(model, currentModel,df){
  crossProjectPrediction(model,paste(currentModel,"accumulo","data"), df$accumulo)
  crossProjectPrediction(model,paste(currentModel,"bookkeeper","data"), df$bookkeeper)
  crossProjectPrediction(model,paste(currentModel,"camel","data"), df$camel)
  crossProjectPrediction(model,paste(currentModel,"cassandra","data"), df$cassandra)
  crossProjectPrediction(model,paste(currentModel,"cxf","data"), df$cxf)
  crossProjectPrediction(model,paste(currentModel,"flink","data"), df$flink)
  crossProjectPrediction(model,paste(currentModel,"hive","data"), df$hive)
  crossProjectPrediction(model,paste(currentModel,"kafka","data"), df$kafka)
  crossProjectPrediction(model,paste(currentModel,"karaf","data"), df$karaf)
  crossProjectPrediction(model,paste(currentModel,"wicket","data"), df$wicket)
  crossProjectPrediction(model,paste(currentModel,"zookeeper","data"), df$zookeeper)
  crossProjectPrediction(model,paste(currentModel,"groovy","data"), df$groovy)
  crossProjectPrediction(model,paste(currentModel,"hadoop","data"), df$hadoop)
}

crossProjectPrediction <- function(model, modelName, testData){
  library(pROC)
  testData = scaling(testData)
  testData$prediction = predict(model, newdata=testData, type = "response")
  ROC <- roc(post > 0 ~ prediction, data = testData)
  LRT <- anova(model, test="LRT") # compute LRT
  LRT <- LRT[(LRT$`Pr(>Chi)` <= 0.05) == TRUE,] # get significant 
  LRT <- LRT[2:nrow(LRT),] # remove NA in first row
  
  OR = as.data.frame(summary(model)$coefficients)[((rownames(summary(model)$coefficients) %in% rownames(LRT))),]
  resultOR = cbind("modelType"=modelName,data.frame("feature"=rownames(LRT),"OR"=OR[,1],"X"=as.matrix(LRT$Deviance),"DF"=as.matrix(LRT$Df)))
  resultAUC = rbind(data.frame("modelType"=modelName,"auc" = ROC$auc, "null deviance"=model$null.deviance, "residual deviance"=model$deviance))
  
  write.csv(resultOR, paste(modelName,"_OR_X.csv"), row.names = FALSE)
  write.csv(resultAUC, paste(modelName,"_AUC.csv"), row.names = FALSE)
  
}

# MAIN

df <- read.csv("D:\\M.Sc\\testSmellRevision\\data_model\\final.csv")
df <- df[complete.cases(df$post_bug), ]
projectName <- df$project
df <- subsetBasedOnProjects(df)

model_accumulo <-  main(df$accumulo,"accumulo")
model_bookkeeper <- main(df$bookkeeper,"bookkeeper")
model_camel <- main(df$camel,"camel")
model_cassandra <- main(df$cassandra,"cassandra")
model_cxf <- main(df$cxf,"cxf")
model_flink <- main(df$flink,"flink")
model_hive <- main(df$hive,"hive")
model_kafka <- main(df$kafka,"kafka")
model_karaf <- main(df$karaf,"karaf")
model_wicket <- main(df$wicket,"wicket")
model_zookeeper <- main(df$zookeeper,"zookeeper")
model_groovy <- main(df$groovy,"groovy")
model_hadoop <- main(df$groovy,"hadoop")


crossProjectPredictionHelper(model_accumulo,"accumulo_model",df)
crossProjectPredictionHelper(model_bookkeeper,"bookkeeper_model",df)
crossProjectPredictionHelper(model_camel,"camel_model",df)
crossProjectPredictionHelper(model_cassandra,"cassandra_model",df)
crossProjectPredictionHelper(model_cxf,"cxf_model",df)
crossProjectPredictionHelper(model_flink,"flink_model",df)
crossProjectPredictionHelper(model_hive,"hive_model",df)
crossProjectPredictionHelper(model_kafka,"kafka_model",df)
crossProjectPredictionHelper(model_karaf,"karaf_model",df)
crossProjectPredictionHelper(model_wicket,"wicket_model",df)
crossProjectPredictionHelper(model_zookeeper,"zookeeper_model",df)
crossProjectPredictionHelper(model_groovy,"groovy_model",df)
crossProjectPredictionHelper(model_hadoop,"hadoop_model",df)










