
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
require(gridExtra)
library("Hmisc")

totalDensityTimeSeriesPlot <- function(df,x = c(1:19)){
  projects = unique(df$smell)
  projects = list(projects[x]) %>% unlist()
  myplots <-  list()
  resultDF = data.frame()
  final = data.frame()
  for (p in projects){
    temp = df[df$smell == p,][c(1,3)]
    x_axis = temp[1]
    temp[1] = seq(1, 7, 1) 
    names(temp)[1] = "date"
    names(temp)[2] = p
    resultDF = reshape2::melt(temp,id="date")
    final = rbind(final, resultDF)
  }
  g = ggplot(final,aes(x=final$date,y=final$value,color=final$variable, group=final$variable))+  geom_point( show.legend = F) +
    geom_line(aes(linetype = final$variable), show.legend = T)+ theme(legend.title=element_blank())+ ylab("Density")+ xlab("Time")   +
    xlim( "1/1/2016", "7/1/2016", "1/1/2017", "7/1/2017", "1/1/2018",  "7/1/2018", "1/1/2019") +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5),legend.position = "bottom") 
  myplots[[p]] = ggplotGrob(g)
  return(myplots)
}

plotTimeSeries <- function(csv){
  setwd("")
  df = read.csv(csv)
  myplots1 = list()
  
  myplots = totalDensityTimeSeriesPlot(df,c(3,7,8))
  myplots1 = c(myplots1,myplots)
  myplots = totalDensityTimeSeriesPlot(df,c(9,10,11,16))
  myplots1 = c(myplots1,myplots)
  myplots = totalDensityTimeSeriesPlot(df,c(12,5,1,13))
  myplots1 = c(myplots1,myplots)
  myplots = totalDensityTimeSeriesPlot(df,c(6,15,18,14,17))
  myplots1 = c(myplots1,myplots)
  g = grid.arrange(grobs=myplots1[c(1:4)], ncol=2, common.legend = TRUE, legend="bottom")
  ggsave((paste("Time_Series_Combined_Raw",csv,".pdf")), g, width = 8, height = 8, dpi = 300)
}

plotTimeSeries("avg_norm.csv")
plotTimeSeries("avg_raw.csv")

correlationHelper <- function(){
  added_corr <- added$r
  added_corr <- as.data.frame(as.table(added_corr))
  added_corr <- added_corr[added_corr$Var1 == "exp",]
  added_p <- added$P
  added_p <- as.data.frame(as.table(added_p))
  added_p <- added_p[added_p$Var1 == "exp",] 
  added_result <- cbind(added_corr[added_p$Freq < 0.05,],added_p[added_p$Freq < 0.05,]$Freq)
  names(added_result) <- c("Metric 1", "Metric 2","Correlation","P-value")
  return(added_result)
}




getCorrelationBetweenTestSmellAndExp <- function(){
  setwd("")
  
  df_added = read.csv("RQ1_exp_A_compiled.csv")
  df_added_finer = df_added[c(6:28)]
  
  df_deleted = read.csv("RQ1_exp_D_compiled.csv")
  df_deleted_finer = df_deleted[c(6:28)]
  df_deleted_finer[,c(1:22)] = df_deleted_finer[,c(1:22)] * -1
  
  df_modified_A = read.csv("RQ1_exp_M_A_compiled.csv")
  df_modified_A_finer = df_modified_A[c(6:28)]
  
  df_modified_D = read.csv("RQ1_exp_M_D_compiled.csv")
  df_modified_D_finer = df_modified_D[c(6:28)]
  
  df_combined_finer = rbind(df_deleted_finer,df_added_finer,df_modified_A_fined,df_modified_D_finer)
  df_combined_modified = rbind(df_modified_A_fined,df_modified_D_finer)
  df_combined_finer_deleted = rbind(df_deleted_finer,df_modified_D_finer)
  df_combined_finer_added = rbind(df_added_finer,df_modified_A_finer)
  
  added <- rcorr(as.matrix(df_added_finer[(colSums(df_deleted_finer) != 0)]), type="spearman")
  deleted <- rcorr(as.matrix(df_deleted_finer[(colSums(df_deleted_finer) != 0)]), type="spearman")
  modified_D <- rcorr(as.matrix(df_modified_D_finer[(colSums(df_modified_D_finer) != 0)]), type="spearman")
  modified_A <- rcorr(as.matrix(df_modified_A_finer[(colSums(df_modified_A_finer) != 0)]), type="spearman")
  combined <- rcorr(as.matrix(df_combined_finer[(colSums(df_combined_finer) != 0)]), type="spearman")
  combined_added <- rcorr(as.matrix(df_combined_finer_added[(colSums(df_combined_finer_added) != 0)]), type="spearman")
  combined_removed <- rcorr(as.matrix(df_combined_finer_deleted[(colSums(df_combined_finer_deleted) != 0)]), type="spearman")
  combined_modified <- rcorr(as.matrix(df_combined_modified[(colSums(df_combined_modified) != 0)]), type="spearman")
  
  added_compiled = getMatrix(added)
  deleted_compiled = getMatrix(deleted)
  modified_D_compiled = getMatrix(modified_D)
  modified_A_compiled = getMatrix(modified_A)
  combined_compiled = getMatrix(combined)
  combined_added_compiled = getMatrix(combined_added)
  combined_removed_compiled = getMatrix(combined_removed)
  combined_modified_compiled = getMatrix(combined_modified)
  
  
  result = rbind(
    added_compiled[added_compiled$name == "total",],
    deleted_compiled[deleted_compiled$name == "total",],
    modified_D_compiled[modified_D_compiled$name == "total",],
    modified_A_compiled[modified_A_compiled$name == "total",],
    combined_added_compiled[combined_added_compiled$name == "total",],
    combined_removed_compiled[combined_removed_compiled$name == "total",]
  )
  result = cbind(result, c("added_compiled","deleted_compiled",
       "modified_D_compiled","modified_A_compiled",
       "combined_added_compiled",
       "combined_removed_compiled"))
  
  
  write.csv(added_compiled,"addedExpCorr.csv",row.names = FALSE)
  write.csv(deleted_compiled,"deletedExpCorr.csv",row.names = FALSE)
  write.csv(modified_D_compiled,"modifiedDeletedExpCorr.csv",row.names = FALSE)
  write.csv(modified_A_compiled,"modifiedAddedExpCorr.csv",row.names = FALSE)
  write.csv(combined_compiled,"combinedExpCorr.csv",row.names = FALSE)
  
  write.csv(combined_added_compiled,"combinedAddedExpCorr.csv",row.names = FALSE)
  write.csv(combined_removed_compiled,"combinedRemovedExpCorr.csv",row.names = FALSE)
  write.csv(combined_modified_compiled,"combinedModifiedExpCorr.csv",row.names = FALSE)
  
  write.csv(result,"corr_result.csv", row.names=FALSE)
  
  }


getMatrix <- function(df){
  c_value = as.data.frame(df[1])
  c_value = c_value[c(1:(length(c_value)-1)),]
  p_value = as.data.frame(df[3])
  p_value = p_value[c(1:(length(p_value)-1)),]
  
  name = rownames(c_value[p_value$P.exp < 0.05,])
  c = c_value[p_value$P.exp < 0.05,]$r.exp
  p = p_value[p_value$P.exp < 0.05,]$P.exp
  return(as.data.frame(cbind(name,c,p)))
}

addArrow <- function(g){
  l = length(g$project)
  for (i in seq(1,l)){
    g[i,][2:length(g)][,((g[i,][2:length(g)]) > 0)] =
      lapply(g[i,][2:length(g)][,((g[i,][2:length(g)]) > 0)],  function(x) paste(x,"\\%"," {\\color{red}{$\\nearrow$}}"))
    g[i,][2:length(g)][,((g[i,][2:length(g)]) < 0)] =
      lapply(g[i,][2:length(g)][,((g[i,][2:length(g)]) < 0)], function(x) paste(x,"\\%"," {\\color{green}{$\\searrow$}}"))
  }
  return(g)
}

getTableBetween2016And2019 <- function(){
  setwd("")
  df_raw_diff = read.csv("2016_2019_raw_round.csv")
  df_raw_diff[is.na(df_raw_diff)] <- 0
  df_norm_diff = read.csv("2016_2019_norm_round.csv")
  df_norm_diff[is.na(df_norm_diff)] <- 0
  df_norm = read.csv("RQ1_Norm_Individual_round.csv")
  df_raw = read.csv("RQ1_Raw_Individual_round.csv")
  
  raw_2016 = df_raw[df_raw$version == "2016-01-01",][c(1:4),][,c(1,3:length(df_raw))]
  raw_2019 = df_raw[df_raw$version == "2019-01-01",][c(1:4),][,c(1,3:length(df_raw))]
  raw_diff = addArrow(df_raw_diff[c(1:4),])
  tab = getTableBetween2016And2019Helper(raw_diff,raw_2016,raw_2019)
  latexHelper(tab)
  
  raw_2016 = df_raw[df_raw$version == "1/1/2016",][c(5:8),][,c(1,3:length(df_raw))]
  raw_2019 = df_raw[df_raw$version == "1/1/2019",][c(5:8),][,c(1,3:length(df_raw))]
  raw_diff = addArrow(df_raw_diff[c(5:8),])
  tab = getTableBetween2016And2019Helper(raw_diff,raw_2016,raw_2019)
  latexHelper(tab)
  
  raw_2016 = df_raw[df_raw$version == "1/1/2016",][c(9:12),][,c(1,3:length(df_raw))]
  raw_2019 = df_raw[df_raw$version == "1/1/2019",][c(9:12),][,c(1,3:length(df_raw))]
  raw_diff = addArrow(df_raw_diff[c(9:12),])
  tab = getTableBetween2016And2019Helper(raw_diff,raw_2016,raw_2019)
  latexHelper(tab)

  norm_2016 = df_norm[df_norm$version == "1/1/2016",][c(1:4),][,c(1,3:length(df_norm))]
  norm_2019 = df_norm[df_norm$version == "1/1/2019",][c(1:4),][,c(1,3:length(df_norm))]
  norm_diff = addArrow(df_norm_diff[c(1:4),])
  tab = getTableBetween2016And2019Helper(norm_diff,norm_2016,norm_2019)
  latexHelper(tab)
  
  norm_2016 = df_norm[df_norm$version == "1/1/2016",][c(5:8),][,c(1,3:length(df_norm))]
  norm_2019 = df_norm[df_norm$version == "1/1/2019",][c(5:8),][,c(1,3:length(df_norm))]
  norm_diff = addArrow(df_norm_diff[c(5:8),])
  tab = getTableBetween2016And2019Helper(norm_diff,norm_2016,norm_2019)
  latexHelper(tab)
  
  norm_2016 = df_norm[df_norm$version == "1/1/2016",][c(9:12),][,c(1,3:length(df_norm))]
  norm_2019 = df_norm[df_norm$version == "1/1/2019",][c(9:12),][,c(1,3:length(df_norm))]
  norm_diff = addArrow(df_norm_diff[c(9:12),])
  tab = getTableBetween2016And2019Helper(norm_diff,norm_2016,norm_2019)
  latexHelper(tab)
}



getTableBetween2016And2019Helper <- function(g1,g2,g3){
  x = order(c(1:ncol(g1[-1]),1:ncol(g2[-1]), 1:ncol(g3[-1])))
  d_ <- cbind(g2[-1],g3[-1],g1[-1])[,x]
  d_[is.na(d_)] <- 0
  d_ = cbind(g1[,1],d_)
  d_ = as.data.frame(t(d_))
  colnames(d_) <- NULL
  result <- list()
  for (i in seq(2,nrow(d_)-1,3)){
    x = order(c(1:ncol(d_[i,]), 1:ncol(d_[i+1,]),1:ncol(d_[i+2,])))
    temp <- cbind(d_[i,], d_[i+1,],d_[i+2,])[,x]
    result <-  rbind(result, temp)
  }
  return(result)
}

latexHelper <-  function(tab){
  rowNames = c("AR","CTL","CI","ET","ECT","GF","MG","PS","RA","SE","ST","EG","LT","DA","UT","IT","RO","MNT")
  rownames(tab) = rowNames
  print(xtable(tab,type="latex"))
}





