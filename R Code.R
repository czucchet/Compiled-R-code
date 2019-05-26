plot.nas = function(data, nom = T, top = 5) {
  sum.nas = rep(NA, 1)
  if(nom == T){
    for(i in 1:ncol(data)){
      sum.nas[i] = sum(is.na(data[,i]))
      plot(sum.nas)
    }
  }
  else {
    for(i in 1:ncol(data)){
      sum.nas[i] = sum(is.na(data[,i]))/length(data[,i])
      plot(sum.nas)
    }
  }  
  res.mat = matrix(NA, ncol = 2, nrow = top)
  res.mat[,1] = names(data[,order(sum.nas, decreasing =T)[1:top]])
  res.mat[,2] = as.numeric(sum.nas[order(sum.nas, decreasing =T)[1:top]]) 
  res.mat = res.mat[res.mat[,2] != 0,]
  print(res.mat)
  res.mat <<- res.mat
}  
##############################################################
change.data = function(data, thresh) {
  x = rep(NA,ncol(data))
  high.nas = rep(NA,1)
  low.nas = rep(NA,1)
  for(i in 1:ncol(data)){
    x[i] = sum(is.na(data[,i]))  
  }
  high.nas = which(x>thresh)  
  low.nas =  which(x<thresh)
  clean.data = data[,low.nas] 
  other.data = data[,high.nas] 
  assign('clean.data', clean.data, envir = globalenv())
  assign('other.data', other.data, envir = globalenv())
}
#############################################################    
plot.no.unique = function(data, check = 1) {        #### error when selecting a check value of greater than 1
  count.unique = rep(NA, 1)
  for(i in 1:ncol(data)){
    count.unique[i] = length(unique(data[,i]))
  }
  pch = rep("X" , ncol(data))
  x = which(count.unique == check)
  label.name = names(data[,x])
  summary.mat = matrix(NA, nrow = length(x), ncol = 6)
  colnames(summary.mat) = names(summary(data[,label.name[1]]))
  rownames(summary.mat) = label.name
  pch[x] = "+"
  col = rep('black', ncol(data))
  col[x] = "red"
  plot(count.unique, pch = pch, col = col, xlab = 'Column names of data')
  print(label.name)
  for(i in 1:length(x)) {
    names(summary.mat[[i]]) = label.name[i]
    summary.mat[i,] = summary(data[,label.name[i]])
  }
  print(summary.mat)		
}
##############################################################
change.no.uniques = function(data, thresh) {
  x = rep(NA,ncol(data))
  high.unique = rep(NA,1)
  low.unique = rep(NA,1)
  for(i in 1:ncol(data)){
    x[i] = length(unique(data[,i]))  
  }
  high.unique = which(x>thresh)  
  low.unique =  which(x<thresh)
  cleaner.data = data[,high.unique] 
  other.1.data = data[,low.unique] 
  assign('cleaner.data', cleaner.data, envir = globalenv())
  assign('other.1.data', other.1.data, envir = globalenv())
}
################################################################
clus.plot = function(data,clusters, method = kmodes) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:clusters){ 
    wss[i] <- sum(method(data, 
                         modes=i)$withinss)
  }
  plot(1:clusters, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}
################################################################
full_iterate_way<-function(data,lookup.data,lookup.col,return.col) {
  data$added.col = 0
  for(j in 1:nrow(lookup)){
    val <- lookup[j, lookup.col]
    label <- lookup[j, return.col]
  }
  data[k, "bin_num"] <- label
  print(data)
}
###################################################################
add.hour = function(data, column, format ="%d/%m/%Y %H:%M"){
  times =  data.frame(as.POSIXlt(data[,column], format=format))
  hour = rep(NA, nrow(times))
  for(i in 1:nrow(times)){
    hour[i] = as.numeric(substr(times[i,], 12,13))
  }
  data$hour = hour
}
###################################################################
model.test.3 = function(model.1, model.2, model.3, test.y){ 
  length.model.1 = length(model.1)
  length.model.2 = length(model.2)
  length.model.3 = length(model.3)
  if(((sum(length.model.1,length.model.2,length.model.3))/3) != length(model.1)){
    stop("Length of models used are not the same. Please use models that all have the same length")
  }
  models.used = 3
  ensemble.mat = as.matrix(expand.grid(c('model.1', 'model.2', 'model.3'),
                                       c('model.1', 'model.2', 'model.3'),
                                       c('model.1', 'model.2', 'model.3')))
  ensemble.list = rep(list(matrix(NA, ncol = (models.used + 4), nrow = length(model.1))), nrow(ensemble.mat))
  results.mean.vector = rep(NA, nrow(ensemble.mat))
  results.med.vector = rep(NA, nrow(ensemble.mat))
  results.med.mean.vector = rep(NA, nrow(ensemble.mat))
  for(i in 1:nrow(ensemble.mat)) {
    ensemble.list[[i]][,1] = get(ensemble.mat[i,1])
    ensemble.list[[i]][,2] = get(ensemble.mat[i,2])
    ensemble.list[[i]][,3] = get(ensemble.mat[i,3])
    ensemble.list[[i]][,4] = apply(ensemble.list[[i]][,1:3], 1, function(x)mean(x))
    ensemble.list[[i]][,5] = apply(ensemble.list[[i]][,1:3], 1, function(x)median(x))
    ensemble.list[[i]][,6] = test.y
    ensemble.list[[i]][,7] = apply(ensemble.list[[i]][,4:5], 1, function(x)mean(x))
    ##ensemble.list[[i]][,8] = ensemble.list[[i]][,4] - ensemble.list[[i]][,6]                                                        
    ##ensemble.list[[i]][,9] = ensemble.list[[i]][,5] - ensemble.list[[i]][,6]
  }
  for(i in 1:length(results.mean.vector)){
    results.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,4] - ensemble.list[[i]][,6])^2))
  }
  for(i in 1:length(results.med.vector)){
    results.med.vector[i] =  sqrt(mean((ensemble.list[[i]][,5] - ensemble.list[[i]][,6])^2))
  }  
  for(i in 1:length(results.med.mean.vector)){
    results.med.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,7] - ensemble.list[[i]][,6])^2))
  }
  best.mean = which.min(results.mean.vector)   
  best.med = which.min(results.med.vector)   
  best.mean.med = which.min(results.med.mean.vector)   
  best.mean.model = ensemble.mat[best.mean,]
  best.med.model = ensemble.mat[best.med,]
  best.mean.med.model = ensemble.mat[best.mean.med,] 
  results.list = rep(list(matrix(NA, nrow = length(model.1), ncol = (models.used + 4))), nrow(ensemble.mat))
  
  results.list = list(best.mean.based.model = c(ensemble.mat[best.mean,], as.numeric(results.mean.vector[best.mean])),
                      best.median.based.model = c(ensemble.mat[best.med,], as.numeric(results.med.vector[best.med])),
                      best.median.mean.based.model = c(ensemble.mat[best.mean.med,], 
                                                       as.numeric(results.med.mean.vector[best.mean.med])))
  print(results.list)
}
####################################################
model.test.4 = function(model.1, model.2, model.3, model.4, test.y){ 
  length.model.1 = length(model.1)
  length.model.2 = length(model.2)
  length.model.3 = length(model.3)
  length.model.4 = length(model.4)
  if(((sum(length.model.1,length.model.2,length.model.3,length.model.3))/4) != length(model.1)){
    stop("Length of models used are not the same. Please use models that all have the same length")
  }
  model.1 = model.1
  model.2 = model.2
  model.3 = model.3
  model.4 = model.4	
  models.used = 4
  ensemble.mat = as.matrix(expand.grid(c('model.1', 'model.2', 'model.3','model.4'),
                                       c('model.1', 'model.2', 'model.3','model.4'),
                                       c('model.1', 'model.2', 'model.3','model.4'),
                                       c('model.1', 'model.2', 'model.3','model.4')))
  ensemble.list = rep(list(matrix(NA, ncol = (models.used + 4), nrow = length(model.1))), nrow(ensemble.mat))
  results.mean.vector = rep(NA, nrow(ensemble.mat))
  results.med.vector = rep(NA, nrow(ensemble.mat))
  results.med.mean.vector = rep(NA, nrow(ensemble.mat))
  for(i in 1:nrow(ensemble.mat)) {
    ensemble.list[[i]][,1] = get(ensemble.mat[i,1])
    ensemble.list[[i]][,2] = get(ensemble.mat[i,2])
    ensemble.list[[i]][,3] = get(ensemble.mat[i,3])
    ensemble.list[[i]][,4] = get(ensemble.mat[i,4])
    ensemble.list[[i]][,5] = apply(ensemble.list[[i]][,1:4], 1, function(x)mean(x))
    ensemble.list[[i]][,6] = apply(ensemble.list[[i]][,1:4], 1, function(x)median(x))
    ensemble.list[[i]][,7] = test.y
    ensemble.list[[i]][,8] = apply(ensemble.list[[i]][,1:4], 1, function(x){
      a = mean(x)
      c = abs(x - a)
      d = order(c)
      (1*x[d[1]]+ 0*x[d[2]] + 0*x[d[3]] + 0*x[d[4]])
    })
    ##ensemble.list[[i]][,8] = ensemble.list[[i]][,4] - ensemble.list[[i]][,6]                                                        
    ##ensemble.list[[i]][,9] = ensemble.list[[i]][,5] - ensemble.list[[i]][,6]
  }
  for(i in 1:length(results.mean.vector)){
    results.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,5] - ensemble.list[[i]][,7])^2))
  }
  for(i in 1:length(results.med.vector)){
    results.med.vector[i] =  sqrt(mean((ensemble.list[[i]][,6] - ensemble.list[[i]][,7])^2))
  }  
  for(i in 1:length(results.med.mean.vector)){
    results.med.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,8] - ensemble.list[[i]][,7])^2))
  }
  best.mean = which.min(results.mean.vector)   
  best.med = which.min(results.med.vector)   
  best.mean.med = which.min(results.med.mean.vector)   
  best.mean.model = ensemble.mat[best.mean,]
  best.med.model = ensemble.mat[best.med,]
  best.mean.med.model = ensemble.mat[best.mean.med,] 	  
  results.list = list(best.mean.based.model = c(ensemble.mat[best.mean,], as.numeric(results.mean.vector[best.mean])),
                      best.median.based.model = c(ensemble.mat[best.med,], as.numeric(results.med.vector[best.med])),
                      best.median.mean.based.model = c(ensemble.mat[best.mean.med,], 
                                                       as.numeric(results.med.mean.vector[best.mean.med])))
  print(results.list)
  assign('list.results',ensemble.list, envir = globalenv())
}
###########################################################
model.test.5 = function(model.1, model.2, model.3, model.4,model.5, test.y){ 
  require(foreach)
  require(doSNOW)
  require(snow)
  length.model.1 = length(model.1)
  length.model.2 = length(model.2)
  length.model.3 = length(model.3)
  length.model.4 = length(model.4)
  length.model.5 = length(model.5)
  if(((sum(length.model.1,length.model.2,length.model.3,length.model.4,length.model.5))/5) != length(model.1)){
    stop("Length of models used are not the same. Please use models that all have the same length")
  }
  model.1 = model.1
  model.2 = model.2
  model.3 = model.3
  model.4 = model.4
  model.5 = model.5
  models.used = 5
  ensemble.mat = as.matrix(expand.grid(c('model.1', 'model.2', 'model.3','model.4','model.5'),
                                       c('model.1', 'model.2', 'model.3','model.4','model.5'),
                                       c('model.1', 'model.2', 'model.3','model.4','model.5'),
                                       c('model.1', 'model.2', 'model.3','model.4','model.5'),
                                       c('model.1', 'model.2', 'model.3','model.4','model.5')))
  ensemble.list = rep(list(matrix(NA, ncol = (models.used + 4), nrow = length(model.1))), nrow(ensemble.mat))
  results.mean.vector = rep(NA, nrow(ensemble.mat))
  results.med.vector = rep(NA, nrow(ensemble.mat))
  results.med.mean.vector = rep(NA, nrow(ensemble.mat))
  for(i in 1:nrow(ensemble.mat)) {
    ensemble.list[[i]][,1] = get(ensemble.mat[i,1])
    ensemble.list[[i]][,2] = get(ensemble.mat[i,2])
    ensemble.list[[i]][,3] = get(ensemble.mat[i,3])
    ensemble.list[[i]][,4] = get(ensemble.mat[i,4])
    ensemble.list[[i]][,5] = get(ensemble.mat[i,5])
    ensemble.list[[i]][,6] = apply(ensemble.list[[i]][,1:5], 1, function(x)mean(x))
    ensemble.list[[i]][,7] = apply(ensemble.list[[i]][,1:5], 1, function(x)median(x))
    ensemble.list[[i]][,8] = test.y
    ensemble.list[[i]][,9] = apply(ensemble.list[[i]][,6:7], 1, function(x){
      a = mean(x)
      c = abs(x - a)
      d = order(c)
      (1*x[d[1]]+ 0*x[d[2]] + 0*x[d[3]] + 0*x[d[4]])
    })
  }
  for(i in 1:length(results.mean.vector)){
    results.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,6] - ensemble.list[[i]][,8])^2))
  }
  for(i in 1:length(results.med.vector)){
    results.med.vector[i] =  sqrt(mean((ensemble.list[[i]][,7] - ensemble.list[[i]][,8])^2))
  }  
  for(i in 1:length(results.med.mean.vector)){
    results.med.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,9] - ensemble.list[[i]][,8])^2))
  }
  best.mean = which.min(results.mean.vector)   
  best.med = which.min(results.med.vector)   
  best.mean.med = which.min(results.med.mean.vector)   
  best.mean.model = ensemble.mat[best.mean,]
  best.med.model = ensemble.mat[best.med,]
  best.mean.med.model = ensemble.mat[best.mean.med,] 	  
  results.list = list(best.mean.based.model = c(ensemble.mat[best.mean,], as.numeric(results.mean.vector[best.mean])),
                      best.median.based.model = c(ensemble.mat[best.med,], as.numeric(results.med.vector[best.med])),
                      best.median.mean.based.model = c(ensemble.mat[best.mean.med,], 
                                                       as.numeric(results.med.mean.vector[best.mean.med])))
  print(results.list)
}
#################################################
predict.model = function(model.a, model.b, model.c, model.d, model.e, test.x){
  pred.mat = matrix(NA, ncol = 6, nrow = nrow(test.x))
  pred.mod.a = predict(model.a,test.x) 
  pred.mod.b = predict(model.b,test.x) 
  pred.mod.c = predict(model.c,test.x) 
  pred.mod.d = predict(model.d,test.x) 
  pred.mod.e = predict(model.e,test.x) 
  pred.mat[,1] = pred.mod.a
  pred.mat[,2] = pred.mod.b
  pred.mat[,3] = pred.mod.c
  pred.mat[,4] = pred.mod.d
  pred.mat[,5] = pred.mod.e
  pred.mat[,6] = apply(pred.mat[,1:5], 1, function(x){
    a = mean(x)
    c = abs(x - a)
    d = order(c)
    (1*x[d[1]]+ 0*x[d[2]] + 0*x[d[3]] + 0*x[d[4]])
  })
  new.preds = pred.mat[,6]
  assign('new.preds', new.preds, envir = globalenv())
}
########################################################
train.test.reg = function(data, y,samp= 0.7 ) {
  y.vars = names(data[y])
  x.vars = names(data[,-y])
  train_ind = floor(sample(nrow(data), samp * nrow(data)))
  trainer = data[train_ind, ]
  tester =  data[-train_ind, ]
  train.x = data[train_ind, -y]
  train.y = as.vector(data.frame(data[train_ind, ]))
  train.y = as.factor(train.y[,y])
  test.x = data[-train_ind, -y] 
  test.y = as.vector(data.frame(data[-train_ind, ]))
  test.y = as.vector(test.y[,y])
#  Formula = formula(paste(y.vars,sep="~",
#                          paste(x.vars, collapse = " + ")))
  assign('trainer', trainer, envir = globalenv())
  assign('trainer', trainer, envir = globalenv())
  assign('tester', tester, envir = globalenv())
  assign('train.x', train.x, envir = globalenv())
  assign('train.y', train.y, envir = globalenv())
  assign('test.x', test.x, envir = globalenv())
  assign('test.y', test.y, envir = globalenv())
#  assign('Formula', Formula, envir = globalenv())
}
######################################################
train.test.class = function(data, y,samp= 0.7,equalsplit = F) {
  y.vars = names(data[y])
  x.vars = names(data[,-y])
####### #  if(equalsplit != F){ ####### 
#     min.seg = min(table(data[,y]))
#     init_samp = floor(sample(nrow(data[which(data[,y] == 1),]), min.seg))
#     init_samp_v2 = floor(sample(nrow(data[which(data[,y] == "1"),]), min.seg))
#     out_1 = data[which(data[,y] == 1),]
#     out_1v2 = data[which(data[,y] == "1"),]
#     out_0 = data[which(data[,y] == 0),]
#     out_0v2 = data[which(data[,y] == "0"),]
#     out_1 = out_1[init_samp,]
#     data = rbind(out_0,out_1)  
#     train_ind = floor(sample(nrow(data), samp * nrow(data)))
#     }
# #  else {
#     train_ind = floor(sample(nrow(data), samp * nrow(data)))
########  #    }####### 
  train_ind = floor(sample(nrow(data), samp * nrow(data)))
  trainer = data[train_ind, ]
  trainer[,y] = as.factor(trainer[[y]])
  #  if(levels(trainer[[y]])[1] == "0" && levels(trainer[[y]])[2] == "1"){
  #    trainer[,y] = ifelse(trainer[,y] == "1", "Yes","No")
  # }
  trainer[,y] = as.factor(trainer[[y]])
  tester =  data[-train_ind, ]
  train.x = data[train_ind, -y]
  train.y = as.vector(data.frame(data[train_ind,y]))
  #train.y = as.factor(as.vector(train.y[,y]))
  #if(levels(train.y)[1] == "0" && levels(train.y)[2] == "1"){
  #  train.y = ifelse(train.y == "1", "Yes","No")
  #}
  test.x = data[-train_ind, -y] 
  test.y = as.vector(data.frame(data[-train_ind, ]))
  test.y = as.factor(as.vector(test.y[,y]))
  #if(levels(test.y)[1] == "0" && levels(test.y)[2] == "1"){
  # test.y = ifelse(test.y == "1", "Yes","No")
  #}
  #test.y.num = as.vector(test.y)
  #Formula = formula(paste(y.vars,sep="~",
  #                        paste(x.vars, collapse = " + ")))
  assign('trainer', trainer, envir = globalenv())
  assign('tester', tester, envir = globalenv())
  assign('train.x', train.x, envir = globalenv())
  assign('train.y', train.y, envir = globalenv())
  assign('test.x', test.x, envir = globalenv())
  assign('test.y', test.y, envir = globalenv())
}
######################################################
train_test_class_2 = function(data, y,samp= 0.7) {
   y.vars = names(data[y])
   x.vars = names(data[,-y])
   train_ind = floor(sample(nrow(data), samp * nrow(data)))
   trainer = as.data.frame(data[train_ind, ])
   trainer[,y] = as.factor(trainer[[y]]) 
   train.x = trainer[,-y]
   train.y = trainer[,y]
   tester =  as.data.frame(data[-train_ind, ])
   tester[,y] = as.factor(tester[[y]])
   test.x = tester[,-y]
   test.y = tester[,y]
   assign('trainer', trainer, envir = globalenv())
   assign('tester', tester, envir = globalenv())
   assign('train.x', train.x, envir = globalenv())
   assign('train.y', train.y, envir = globalenv())
   assign('test.x', test.x, envir = globalenv())
   assign('test.y', test.y, envir = globalenv())
}
######################################################
par.process = function(clusters) {
  require(doParallel)
  require(foreach)
  require(doSNOW)
  no_cores = detectCores() - clusters
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  }
getDoParWorkers() # tells you how many cores are actually running
library(foreach)
library(doSNOW)
stopCluster(cl)  
######################################################
#Test out parallel
a <- matrix(1, ncol=10^4*2, nrow=10^4)
system.time(mean(a))
######################################################
model.test.6 = function(model.1, model.2, model.3, model.4,model.5,model.6, test.y){ 
  require(foreach)
  require(doSNOW)
  require(snow)
  require(Rcpp)
  length.model.1 = length(model.1)
  length.model.2 = length(model.2)
  length.model.3 = length(model.3)
  length.model.4 = length(model.4)
  length.model.5 = length(model.5)
  length.model.6 = length(model.6)
  if(((sum(length.model.1,length.model.2,length.model.3,length.model.4,length.model.5,length.model.6))/6) != length(model.1)){
    stop("Length of models used are not the same. Please use models that all have the same length")
  }
  models.used = 6
  ensemble.mat = as.matrix(expand.grid(c('model.1', 'model.2', 'model.3','model.4','model.5','model.6'),
                                       c('model.1', 'model.2', 'model.3','model.4','model.5','model.6'),
                                       c('model.1', 'model.2', 'model.3','model.4','model.5','model.6'),
                                       c('model.1', 'model.2', 'model.3','model.4','model.5','model.6'),
                                       c('model.1', 'model.2', 'model.3','model.4','model.5','model.6'),
                                       c('model.1', 'model.2', 'model.3','model.4','model.5','model.6')))
  ensemble.list = rep(list(matrix(NA, ncol = (models.used + 7), nrow = length(model.1))), nrow(ensemble.mat))
  results.mean.vector = rep(NA, nrow(ensemble.mat))
  results.med.vector = rep(NA, nrow(ensemble.mat))
  results.med.mean.vector = rep(NA, nrow(ensemble.mat))
  results.trim10.mean.vector = rep(NA, nrow(ensemble.mat))
  results.trim20.mean.vector = rep(NA, nrow(ensemble.mat))
  results.trim30.mean.vector = rep(NA, nrow(ensemble.mat))
  for(i in 1:nrow(ensemble.mat)) {
    ensemble.list[[i]][,1] = get(ensemble.mat[i,1])
    ensemble.list[[i]][,2] = get(ensemble.mat[i,2])
    ensemble.list[[i]][,3] = get(ensemble.mat[i,3])
    ensemble.list[[i]][,4] = get(ensemble.mat[i,4])
    ensemble.list[[i]][,5] = get(ensemble.mat[i,5])
    ensemble.list[[i]][,6] = get(ensemble.mat[i,6])
    ensemble.list[[i]][,7] = apply(ensemble.list[[i]][,1:6], 1, function(x)mean(x))
    ensemble.list[[i]][,8] = apply(ensemble.list[[i]][,1:6], 1, function(x)median(x))
    ensemble.list[[i]][,9] = test.y
    ensemble.list[[i]][,10] = apply(ensemble.list[[i]][,7:8], 1, function(x)mean(x))
    ensemble.list[[i]][,11] = apply(ensemble.list[[i]][,1:6], 1, function(x)mean(x, trim = 0.1))
    ensemble.list[[i]][,12] = apply(ensemble.list[[i]][,1:6], 1, function(x)mean(x, trim = 0.2))
    ensemble.list[[i]][,13] = apply(ensemble.list[[i]][,1:6], 1, function(x)mean(x, trim = 0.3))
  }
  for(i in 1:length(results.mean.vector)){
    results.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,7] - ensemble.list[[i]][,9])^2))
  }
  for(i in 1:length(results.med.vector)){
    results.med.vector[i] =  sqrt(mean((ensemble.list[[i]][,8] - ensemble.list[[i]][,9])^2))
  }  
  for(i in 1:length(results.med.mean.vector)){
    results.med.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,10] - ensemble.list[[i]][,9])^2))
  }
  for(i in 1:length(results.trim10.mean.vector)){
    results.trim10.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,11] - ensemble.list[[i]][,9])^2))
  }
  for(i in 1:length(results.trim20.mean.vector)){
    results.trim20.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,12] - ensemble.list[[i]][,9])^2))
  }
  for(i in 1:length(results.trim30.mean.vector)){
    results.trim30.mean.vector[i] =  sqrt(mean((ensemble.list[[i]][,13] - ensemble.list[[i]][,9])^2))
  }
  best.mean = which.min(results.mean.vector)   
  best.med = which.min(results.med.vector)   
  best.mean.med = which.min(results.med.mean.vector)   
  best.trim.mean10 = which.min(results.trim10.mean.vector)   
  best.trim.mean20 = which.min(results.trim20.mean.vector)   
  best.trim.mean30 = which.min(results.trim30.mean.vector)   
  best.mean.model = ensemble.mat[best.mean,]
  best.med.model = ensemble.mat[best.med,]
  best.mean.med.model = ensemble.mat[best.mean.med,] 	
  best.mean.10.model = ensemble.mat[best.trim.mean10,] 	
  best.mean.20.model = ensemble.mat[best.trim.mean20,] 	
  best.mean.30.model = ensemble.mat[best.trim.mean30,] 	
  results.list = list(best.mean.based.model =        c(ensemble.mat[best.mean,], as.numeric(results.mean.vector[best.mean])),
                      best.median.based.model =      c(ensemble.mat[best.med,], as.numeric(results.med.vector[best.med])),
                      best.median.mean.based.model = c(ensemble.mat[best.mean.med,], as.numeric(results.med.mean.vector[best.mean.med])),
                      best.mean10.based.model =      c(ensemble.mat[best.trim.mean10,], as.numeric(results.trim10.mean.vector[best.trim.mean10])),
                      best.mean20.based.model =      c(ensemble.mat[best.trim.mean20,], as.numeric(results.trim20.mean.vector[best.trim.mean20])),
                      best.mean30.based.model =      c(ensemble.mat[best.trim.mean30,], as.numeric(results.trim30.mean.vector[best.trim.mean30])))
  print(results.list)
}
#########################################################
windsor = function (x){
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(0.05, 1-0.05))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}
###########################################################
som.model = function(data,x.var, y.var,dim = 10, scale = F, hexagonal = T, rlength = 100, alphas = c(0.05, 0.01)) {
  require(kohonen)
  if(scale != F){
    som.data = as.matrix(scale(table(data[,x.var], data[,y.var])))
  }
  else{som.data = as.matrix(table(data[,x.var], data[,y.var]))
  }
  if(hexagonal == T){
    som_grid <- somgrid(xdim = dim, ydim=dim, topo="hexagonal")
  }
  else{
    som_grid <- somgrid(xdim = dim, ydim=dim, topo="rectangular")
  }
  som_model <- som(som.data, 
                   grid=som_grid, 
                   rlen=rlength, 
                   alpha=alphas, 
                   keep.data = TRUE,
                   n.hood="circular")
  pdf("trainingProgress.pdf",width=7,height=5)
  plot(som_model, type="changes")
  dev.off()
  pdf("nodeCounts.pdf",width=7,height=5)
  plot(som_model, type="count")
  dev.off()
  pdf("neighbourDistance.pdf",width=7,height=5)
  plot(som_model, type="dist.neighbours")
  dev.off()
  pdf("codeVectors.pdf",width=7,height=5)
  plot(som_model, type="codes")
  dev.off()
}
##########################################################
save.object = function(object){
  saveRDS(object, 'object')
}
##########################################################
save.data = function(data,name){
  save(data,file = 'name')
}
##########################################################
load.data = function(file.name){
  save(data,file = 'name')
}
##########################################################

#IN PROGRESS
clean.data = function(file.location, ) {
  require(dplyr)
  if(class(file.location)!="character") {
    stop("file location must be defined as a string")
  }
  work.dir = getwd()
  setwd(paste(work.dir,"/",file.location, sep=""))
  no.files = length(dir())
  for(i in 1:no.files) {
    assign(paste(file,i,sep=""),  read.csv(dir()[i]), envir =)
  }    
}
###########################################################
sql.connect = function(server, database) {
  if(class(server)!="character") {
    stop("server location and database must be defined as a string")
  }
  server.name <- server
  database.name <- database
  myconn <- odbcDriverConnect(paste("DRIVER=SQL Server;Trusted_Connection=Yes;DATABASE=", 
                                    database.name, ";SERVER=", server.name, sep=""))
}
###########################################################
produce.cluster = function(data, x.var, y.var, no.clusters, normal = T, genetic = F, pcluto = F){
  require(skmeans)
  names.col = names(data[,c(x.var,y.var)])
  data.cluster = table(data[,x.var], data[,y.var])
  if(normal == T && genetic != F){
    stop("Please choose one method of clustering. You have currently chosen both normal and genetic clustering")
  }
  if(normal == T && pcluto != F){
    stop("Please choose one method of clustering. You have currently chosen both normal and pcluto clustering")
  }
  if(genetic != F && pcluto != F){
    stop("Please choose one method of clustering. You have currently chosen both genetic and pcluto clustering")
  }
  if(normal == T){
    sk.cluster = skmeans(data.cluster,no.clusters,control = list(verbose = TRUE))
  }
  if(genetic != F && normal != T){
    sk.cluster = skmeans(data.cluster,no.clusters,method = 'genetic', control = list(verbose = TRUE))
  }
  if(pcluto != F && normal != T){
    sk.cluster = skmeans(data.cluster,no.clusters,method = 'pcluto', control = list(verbose = TRUE))
  }
  mat.clus = matrix(NA, ncol = 2, length(sk.cluster$clus))
  mat.clus[,1] = names(sk.cluster$cluster)
  mat.clus[,2] = sk.cluster$cluster
  mat.clus = data.frame(mat.clus)
  names(mat.clus) = names.col
  data.clus.join = merge(x = data.cluster, y = mat.clus, by = names.col[2], all.y = TRUE)
  cluster.results = data.clus.join[!duplicated(data.clus.join[,x.var]),]
  ncol.cluster.results = ncol(cluster.results)  
  final.cluster = cluster.results[,c(x.var,ncol.cluster.results)]
  assign(final.cluster, 'final.cluster', envir = globalenv())
}
#############################################################
mapk <- function (k, actual, predicted){
  require(Metrics)
  scores <- rep(0, length(actual))
  for (i in 1:length(scores)){
    scores[i] <- apk(k, actual[[i]], predicted[[i]])
  }
  score <- mean(scores)
  score
}
#############################################################
winsor.mean = function(x){
  Min <- which.min(x)
  Max <- which.max(x)
  ord <- order(x)
  x[Min] <- x[ord][2]
  x[Max] <- x[ord][length(x)-1]
  mean(x)}
#############################################################
rm(list = setdiff(ls(), lsf.str()))
#############################################################
scrape.url = function(website){
  require(XML)
  url = website
  mydata <<- readHTMLTable(url, stringsAsFactors = FALSE)
}
#############################################################
adelaide =  data.frame(mydata[[1]][,3])
adelaide = as.character(adelaide[,1])
x = unlist(strsplit(adelaide, ","))
y = gsub(" ", "", x, fixed = T)
last = seq(1, length(y), 2)
first = seq(2, length(y), 2)
first.names = y[first]
first.names = paste(first.names, "_", sep = "")
last.names = y[last]
ade.mat = matrix(NA,nrow = length(last.names), ncol = 7)
ade.mat[,2] = first.names
ade.mat[,3] = last.names
end.text = rep(".html", nrow(ade.mat))
ade.mat[,4] = end.text
bit.2 = "/"
bit.1 = rep(NA, nrow = ade.mat)
first.bit = rep(NA, nrow = ade.mat)
for(i in 1:nrow(ade.mat)){
  first.bit[i] = "http://afltables.com/afl/stats/players/"
}
ade.mat[,5] = first.bit
for(i in 1:nrow(ade.mat)){
  bit.1[i] = substr(ade.mat[i,2], 1,1)
}
bit = rep(NA, nrow = ade.mat)
for(i in 1:length(bit.1)){
  bit[i] = paste(bit.1[i],bit.2,sep="")
}
ade.mat[,1] = bit
for(i in 1:nrow(ade.mat)){ 
  ade.mat[i,6] = paste(ade.mat[i,1],ade.mat[i,2],ade.mat[i,3],ade.mat[i,4],sep = "")
}
for(i in 1:nrow(ade.mat)){ 
  ade.mat[i,7] = paste(ade.mat[i,5],ade.mat[i,6],sep = "")
}
for(i in 1:nrow(ade.mat)){
  assign(paste("x.",i,sep=""), ade.mat[i,7])
}
library(XML)
for(i in 1:nrow(ade.mat)){tra
  assign(paste("url.",i,sep=""), ade.mat[i,7])
}

###############################################################
result.output = function(object.name, len.objects, thresh){
  object.names =  paste(as.character(object.name),., sep="")
  results.obj = rep(NA, len.objects)
  for(i in 1:len.objects){
    results.obj[i] = length(get(paste(object.names,i,sep="")))
  }
  length.res = length(results.obj[which(results.obj == thresh)])
  which.thresh = rep(NA,length.res)
  which.thresh =  results.obj[which(results.obj == thresh)]   
  print(which.thresh)
}
###############################################################
detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
###############################################################



split.date = function(data, col.1, col.2, time = F){
  
  
  
  
}




###############################################################
oddcount <- function(x) {
  k <- 0 # assign 0 to k
  for (n in x) {
  if (n %% 2 == 1) k <- k+1 # %% is the modulo operator
    }
  return(k)
  }
##############################################################
findruns <- function(x,k) {
  n <- length(x)
  runs <- NULL
  for (i in 1:(n-k+1)) {
    if (all(x[i:(i+k-1)]==1)) runs <- c(runs,i)
  }
  return(runs)
}
###############################################################
LogLoss<-function(actual, predicted){
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
  }
###############################################################
MultiLogLoss <- function(act, pred){
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  return(ll);
  }
###############################################################
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
################################################################target = 0.5
model.test.4.class = function(model.1, model.2, model.3, model.4, test.y){ 
  length.model.1 = length(model.1)
  length.model.2 = length(model.2)
  length.model.3 = length(model.3)
  length.model.4 = length(model.4)
  if(((sum(length.model.1,length.model.2,length.model.3,length.model.3))/4) != length(model.1)){
    stop("Length of models used are not the same. Please use models that all have the same length")
  }
  model.1 = model.1
  model.2 = model.2
  model.3 = model.3
  model.4 = model.4	
  models.used = 4
  ensemble.mat = as.matrix(expand.grid(c('model.1', 'model.2', 'model.3','model.4'),
                                       c('model.1', 'model.2', 'model.3','model.4'),
                                       c('model.1', 'model.2', 'model.3','model.4'),
                                       c('model.1', 'model.2', 'model.3','model.4')))
  ensemble.list = rep(list(matrix(NA, ncol = (models.used + 4), nrow = length(model.1))), nrow(ensemble.mat))
  results.mean.vector = rep(NA, nrow(ensemble.mat))
  results.med.vector = rep(NA, nrow(ensemble.mat))
  results.med.mean.vector = rep(NA, nrow(ensemble.mat))
  for(i in 1:nrow(ensemble.mat)) {
    ensemble.list[[i]][,1] = get(ensemble.mat[i,1])
    ensemble.list[[i]][,2] = get(ensemble.mat[i,2])
    ensemble.list[[i]][,3] = get(ensemble.mat[i,3])
    ensemble.list[[i]][,4] = get(ensemble.mat[i,4])
    ensemble.list[[i]][,5] = apply(ensemble.list[[i]][,1:4], 1, function(x)mean(x))

#  ensemble.list[[i]][,5] = ifelse(ensemble.list[[i]][,5] > target,1,0)
    ensemble.list[[i]][,6] = apply(ensemble.list[[i]][,1:4], 1, function(x)median(x))
#   ensemble.list[[i]][,6] = ifelse(ensemble.list[[i]][,6] > target,1,0)
    ensemble.list[[i]][,7] = test.y
    ensemble.list[[i]][,8] = apply(ensemble.list[[i]][,1:4], 1, function(x){
      a = mean(x)
      c = abs(x - a)
      d = order(c)
      (1*x[d[1]]+ 0*x[d[2]] + 0*x[d[3]] + 0*x[d[4]])
    })
    #ensemble.list[[i]][,8] = ifelse(ensemble.list[[i]][,8] > target,1,0)
    ##ensemble.list[[i]][,8] = ensemble.list[[i]][,4] - ensemble.list[[i]][,6]                                                        
    ##ensemble.list[[i]][,9] = ensemble.list[[i]][,5] - ensemble.list[[i]][,6]
  }
  for(i in 1:length(results.mean.vector)){
    results.mean.vector[i] =  mean(ensemble.list[[i]][,5] == ensemble.list[[i]][,7])
  }
  for(i in 1:length(results.med.vector)){
    results.med.vector[i] =  mean(ensemble.list[[i]][,6] == ensemble.list[[i]][,7])
  }  
  for(i in 1:length(results.med.mean.vector)){
    results.med.mean.vector[i] =  mean(ensemble.list[[i]][,8] == ensemble.list[[i]][,7])
  }
  best.mean = which.max(results.mean.vector)   
  best.med = which.max(results.med.vector)   
  best.mean.med = which.max(results.med.mean.vector)   
  best.mean.model = ensemble.mat[best.mean,]
  best.med.model = ensemble.mat[best.med,]
  best.mean.med.model = ensemble.mat[best.mean.med,] 	  
  results.list = list(best.mean.based.model = c(ensemble.mat[best.mean,], as.numeric(results.mean.vector[best.mean])),
                      best.median.based.model = c(ensemble.mat[best.med,], as.numeric(results.med.vector[best.med])),
                      best.closest.mean.model = c(ensemble.mat[best.mean.med,], 
                                                  as.numeric(results.med.mean.vector[best.mean.med])))
  print(results.list)
  }
###############################################################
model.test.4.logclass = function(model.1, model.2, model.3, model.4, test.y){ 
  length.model.1 = length(model.1)
  length.model.2 = length(model.2)
  length.model.3 = length(model.3)
  length.model.4 = length(model.4)
  if(((sum(length.model.1,length.model.2,length.model.3,length.model.3))/4) != length(model.1)){
    stop("Length of models used are not the same. Please use models that all have the same length")
  }
  MultiLogLoss <- function(act, pred){
    eps = 1e-15;
    nr <- nrow(pred)
    pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
    pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
    ll = sum(act*log(pred) + (1-act)*log(1-pred))
    ll = ll * -1/(nrow(act))      
    return(ll);
  }
  models.used = 4
  ensemble.mat = as.matrix(expand.grid(c('model.1', 'model.2', 'model.3','model.4'),
                                       c('model.1', 'model.2', 'model.3','model.4'),
                                       c('model.1', 'model.2', 'model.3','model.4'),
                                       c('model.1', 'model.2', 'model.3','model.4')))
  ensemble.list = rep(list(matrix(NA, ncol = (models.used + 8), nrow = length(model.1))), nrow(ensemble.mat))
  results.mean.vector = rep(NA, nrow(ensemble.mat))
  results.med.vector = rep(NA, nrow(ensemble.mat))
  results.med.mean.vector = rep(NA, nrow(ensemble.mat))
  #col 5 goes with col 10
  #col 6 goes with col 11
  #col 9 goes with col 12
  for(i in 1:nrow(ensemble.mat)) {
    ensemble.list[[i]][,1] = get(ensemble.mat[i,1])
    ensemble.list[[i]][,2] = get(ensemble.mat[i,2])
    ensemble.list[[i]][,3] = get(ensemble.mat[i,3])
    ensemble.list[[i]][,4] = get(ensemble.mat[i,4])
    ensemble.list[[i]][,5] = apply(ensemble.list[[i]][,1:4], 1, function(x)mean(x))
    ensemble.list[[i]][,6] = apply(ensemble.list[[i]][,1:4], 1, function(x)median(x))
    ensemble.list[[i]][,7] = test.y
    ensemble.list[[i]][,8] = 1 - ensemble.list[[i]][,7]
    ensemble.list[[i]][,9] = apply(ensemble.list[[i]][,1:4], 1, function(x){
      a = mean(x)
      c = abs(x - a)
      d = order(c)
      (1*x[d[1]]+ 0*x[d[2]] + 0*x[d[3]] + 0*x[d[4]])
    })
    ensemble.list[[i]][,10] = 1 - ensemble.list[[i]][,5]
    ensemble.list[[i]][,11] = 1 - ensemble.list[[i]][,6]                                                        
    ensemble.list[[i]][,12] = 1 - ensemble.list[[i]][,9]
  }
  #MultiLogLoss(rbind(ensemble.list[[i]][,7],ensemble.list[[i]][,8]),ensemble.list[[i]][,5],ensemble.list[[i]][,10]))
  for(i in 1:length(results.mean.vector)){
    results.mean.vector[i] =  MultiLogLoss(cbind(ensemble.list[[i]][,7],ensemble.list[[i]][,8]),cbind(ensemble.list[[i]][,5],ensemble.list[[i]][,10]))
  }
  for(i in 1:length(results.med.vector)){
    results.med.vector[i] =  MultiLogLoss(cbind(ensemble.list[[i]][,7],ensemble.list[[i]][,8]),cbind(ensemble.list[[i]][,6],ensemble.list[[i]][,11]))
  }  
  for(i in 1:length(results.med.mean.vector)){
    results.med.mean.vector[i] =  MultiLogLoss(cbind(ensemble.list[[i]][,7],ensemble.list[[i]][,8]),cbind(ensemble.list[[i]][,9],ensemble.list[[i]][,12]))
  }
  best.mean = which.min(results.mean.vector)   
  best.med = which.min(results.med.vector)   
  best.mean.med = which.min(results.med.mean.vector)   
  best.mean.model = ensemble.mat[best.mean,]
  best.med.model = ensemble.mat[best.med,]
  best.mean.med.model = ensemble.mat[best.mean.med,] 	  
  results.list = list(best.mean.based.model = c(ensemble.mat[best.mean,], as.numeric(results.mean.vector[best.mean])),
                      best.median.based.model = c(ensemble.mat[best.med,], as.numeric(results.med.vector[best.med])),
                      best.closest.mean.model = c(ensemble.mat[best.mean.med,], 
                                                  as.numeric(results.med.mean.vector[best.mean.med])))
  print(results.list)
  assign('list.results',ensemble.list[[best.med]], envir = globalenv())
  }
#######################################################################
  scatter.3d = function(data, x.axis, y.axis, z.axis,pch.plot = 19, title = 'insert.title'  color = F, names = F, facet = F){
    require(scatterplot3d)
    if(color == F && names == F && facet == F){
      graph.plot = with(data, {
        scatterplot3d(x.axis, y.axis, z.axis,
                      pch = pch.plot,
                      type = 'h',
                      main = title,
                      xlab = "x.axis",
                      ylab = "y.axis",
                      zlab = "z.axis")})
    }
    if(color != F && names == F && facet == F){
      graph.plot = with(data, {
        scatterplot3d(x.axis, y.axis, z.axis,
                      color = color,
                      pch = pch.plot,
                      type = 'h',
                      main = title,
                      xlab = "x.axis",
                      ylab = "y.axis",
                      zlab = "z.axis")})
    }
    if(color != F && names != F && facet ==F) {
      graph.plot = with(data, {
        scatterplot3d(x.axis, y.axis, z.axis,
                      color = color,
                      pch = pch.plot,
                      type = 'h',
                      main = title,
                      xlab = "x.axis",
                      ylab = "y.axis",
                      zlab = "z.axis")
        s3d.coords <- s3d$xyz.convert(x.axis, y.axis, z.axis) # convert 3D coords to 2D projection
        text(s3d.coords$x, s3d.coords$y,   # x and y coordinates
             labels=row.names(data),             # text to plot
             cex=.5, pos=4)})
    }
    if(color != F && names != F && facet != F){
      mtcars$pcolor[mtcars$cyl==4] <- "red"
      mtcars$pcolor[mtcars$cyl==6] <- "blue"
      mtcars$pcolor[mtcars$cyl==8] <- "darkgreen"
      with(mtcars, {
        s3d <- scatterplot3d(disp, wt, mpg,        # x y and z axis
                             color=pcolor, pch=19,        # circle color indicates no. of cylinders
                             type="h", lty.hplot=2,       # lines to the horizontal plane
                             scale.y=.75,                 # scale y axis (reduce by 25%)
                             main="3-D Scatterplot Example 4",
                             xlab="Displacement (cu. in.)",
                             ylab="Weight (lb/1000)",
                             zlab="Miles/(US) Gallon")
        s3d.coords <- s3d$xyz.convert(disp, wt, mpg)
        text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
             labels=row.names(mtcars),       # text to plot
             pos=4, cex=.5)                  # shrink text 50% and place to right of points)
        # add the legend
        legend("topleft", inset=.05,      # location and inset
               bty="n", cex=.5,              # suppress legend box, shrink text 50%
               title="Number of Cylinders",
               c("4", "6", "8"), fill=c("red", "blue", "darkgreen"))
      })
    }

    
    ####################################### Different code - for AFL ########################
    library(XML)
    library(RCurl)
    ####Season stats
    #1999-2014 Years
    mat.afl = matrix(NA, nrow = length(1999:2014), ncol = 2)
    mat.afl[,1] = 1999:2014
    for(i in 1:length(1999:2014)){
      years = 1999:2014
      mat.afl[i,2] = paste("http://afltables.com/afl/stats/",years[i],"s.html", sep = "")
    }
    for(i in 1:length(1999:2014)){
      assign(paste("url.",i,sep=""), mat.afl[i,2])
    }
    for(i in 1:length(1999:2014)){
      assign(paste("year.",i,sep=""), readHTMLTable(get(paste("url.",i,sep="")),stringsAsFactors = F))   
    }
    
    
    
    url = htmlParse("http://afltables.com/afl/stats/2014.html#1")
    adelaide = readHTMLTable(url, stringsAsFactors = F)
    
    
    ######################### Different script #################################
    library(RODBC)
    server.name <- "AU-AUSSQL040"
    database.name <- "DA_TAC_2015"
    myconn <- odbcDriverConnect(paste("DRIVER=SQL Server;Trusted_Connection=Yes;DATABASE=", 
                                      database.name, ";SERVER=", server.name, sep=""))
    tac.data <- sqlQuery(myconn, "SELECT <INSERT COLUMN NAMES>
                         FROM [DA_TAC_2015].[dbo].[PREP_PaymentProcess_Mod_Timestamp]
                         WHERE	Service_Status NOT IN ('RECEIVED','INPUT')")
    
    remove.global = function(){
      number.objects = length(ls())
      x = rep(NA,number.objects)
      for(i in 1:number.objects){
        x[i] = paste(ls()[i]))
      }
      print
    }
################################################################################
class.check = function(data){
  classes = rep(NA, ncol(data))
    for(i in 1:ncol(data)){
      classes[i] = class(data[,i])  
    }  
    class_profile = table(classes)
    class.list = list(class_detail = classes, class_profile = table(classes))
    name.classes = names(table(classes))
    name.results = paste(name.classes, "results")
    name.results.list = rep(list(rep(NA, 1)),length(table(classes)))         
    for(i in 1:length(name.classes)){
      name.results.list[[i]] = order(data[,name.classes[i]]) 
    ensemble.list = rep(list(matrix(NA, ncol = 1, nrow = length(model.1))), nrow(ensemble.mat))
      
    }
      }
################################################################################    
    numbers = function(big, small){
       if((big + small) != 6 ){
          stop("The combination of big and small numbers does not total 6. Please re-select your numbers again")
       }
       big_population = seq(25,100,25)
       small_population = seq(1,10,1)
       target_create = round(runif(1,100,999))
       big_selection =   sample(big_population, big, replace = T)
       small_selection = sample(small_population, small, replace = T)
       numbers_list = list(Target = sprintf("The target is %i", target_create), Numbers = c(big_selection,small_selection)) 
       print(numbers_list)   
    }
################################################################################        
    number = function(big) {
       how_many = big
      too_many = big - 6
       if(big > 6){
          stop(sprintf("Please only select up to a maximum of 6 big numbers.You have selected %i numbers,
   which is %i more than is allowed.",how_many,too_many))
    }
       big_population = seq(25,100,25)
       small_population = seq(1,10,1)
       target_create = round(runif(1,100,999))
       big_selection =   sample(big_population,big , replace = T)
       small_selection = sample(small_population, (6 - big), replace = T)
       print(list(Target = sprintf("The target is %i", target_create), Numbers = c(big_selection,small_selection))) 
    }
################################################################################        
    letter = function(vowels){
       not_const = c('A','E','I','O','U')
       const = LETTERS[!(LETTERS %in% not_const)]
       constanants = sample(const,(9 - vowels) , replace = T)
       vowel = sample(not_const,vowels , replace = T) 
       print(c(constanants,vowel))
    }
################################################################################
#Soccer function
    soccer_scrape = function(limit){
       suppressMessages(library(httr))
       suppressMessages(library(XML))
       suppressMessages(library(data.table))
       suppressMessages(library(caroline))
       v_url = rep(NA, limit) 
       v_get = rep(list(matrix(NA, ncol = 3, nrow = 20)), length(limit))
       v_player_list = rep(list(matrix(NA, ncol = 3, nrow = 20)), length(limit))
       v_tranfers_info = rep(list(matrix(NA, ncol = 16, nrow = 30)), length(limit))
       for(i in 1:limit){
          v_url[i] = paste("http://www.transfermarkt.com/anthony-martial/profil/spieler/",i,sep = "")
          v_get[[i]] = GET(v_url[i])
          v_player_list[[i]] =  readHTMLTable(rawToChar(v_get[[i]]$content), stringsAsFactors = F)[[1]]
          v_player_list[[i]][,3] = rep(names(v_player_list[[i]])[2],nrow(v_player_list[[i]]))
          v_tranfers_info[[i]] = readHTMLTable(rawToChar(v_get[[i]]$content), stringsAsFactors = F)[[2]]
          v_tranfers_info[[i]][,14] = rep(names(v_player_list[[i]])[2],nrow(v_tranfers_info[[i]]))
       }
       v_players <<- rbindlist(v_player_list)
       v_transfers <<- rbindlist(v_tranfers_info)
       write.delim(v_players,'v_players.txt', sep = "|")
       write.delim(v_transfers,'v_transfers.txt', sep = "|")
    }
################################################################################    
    soccer_scrape = function(start, limit){
       suppressMessages(library(httr))
       suppressMessages(library(XML))
       suppressMessages(library(data.table))
       suppressMessages(library(caroline))
       v_url = rep(NA, length(start:limit))
       v_get = rep(list(matrix(NA, ncol = 3, nrow = 20)), length(start:limit))
       #v_get_all = rep(list(matrix(NA, ncol = 3, nrow = 20)), length(start:limit))
       v_get_2 = rep(list(matrix(NA, ncol = 3, nrow = 20)), length(start:limit))
       v_name = rep(list(matrix(NA, ncol = 1, nrow = 20)), length(start:limit))
       v_name_2 = rep(list(matrix(NA, ncol = 1, nrow = 40)), length(start:limit))
       #v_name_all = rep(list(matrix(NA, ncol = 1, nrow = 40)), length(start:limit))
       v_player_list = rep(list(matrix(NA, ncol = 2, nrow = 50)), length(start:limit))
       v_player_list_perf = rep(list(matrix(NA, ncol = 18, nrow = 50)), length(start:limit))
       url_default = 'http://www.transfermarkt.com/silvio-adzic/profil/spieler/1'
       url_default_2 = 'http://www.transfermarkt.com/charles-grech/leistungsdatendetails/spieler/300005/saison//verein/0/liga/0/wettbewerb//pos/0/trainer_id/0/plus/1'
       v_url = paste("http://www.transfermarkt.com/anthony-martial/profil/spieler/",start:limit,sep = "")
       v_url_2 = paste("http://www.transfermarkt.com/charles-grech/leistungsdatendetails/spieler/", start:limit,"/saison//verein/0/liga/0/wettbewerb//pos/0/trainer_id/0/plus/1",sep = "")
       for(i in 1:length(start:limit)){
          v_get[[i]] = GET(v_url[i])
          v_get_2[[i]] = GET(v_url_2[i])
          #v_get_all = GET(v_url[i])
          if(v_get[[i]][1] == "http://www.transfermarkt.com/spieler-statistik/wertvollstespieler/marktwertetop"){
             v_get[[i]] = GET(url_default)
          }
          if(v_get_2[[i]][1] == "http://www.transfermarkt.com/spieler-statistik/wertvollstespieler/marktwertetop"){	
             v_get_2[[i]] = GET(url_default_2)
          }
          if(length(readHTMLTable(rawToChar(v_get[[i]]$content), stringsAsFactors = F)) < 2){
             v_get[[i]] = GET(url_default)
          }
          if(length(readHTMLTable(rawToChar(v_get_2[[i]]$content), stringsAsFactors = F)) < 2){
             v_get_2[[i]] = GET(url_default_2)
          }
          v_player_list[[i]] = readHTMLTable(rawToChar(v_get[[i]]$content), stringsAsFactors = F)[[1]]
          v_player_list_perf[[i]] = readHTMLTable(rawToChar(v_get_2[[i]]$content), stringsAsFactors = F)[[2]]
          names(v_player_list[[i]]) = c('v1','v2')
          names(v_player_list_perf[[i]]) = c('v1','v2','v3','v4','v5','v6','v7','v8','v9','v10','v11','v12','v13','v14','v15','v16','v17')
          v_name[[i]] = data.frame(rep(as.character(unlist(v_get[[i]][1])), nrow(v_player_list[[i]])))
          v_name_2[[i]] = data.frame(rep(as.character(unlist(v_get[[i]][1])), nrow(v_player_list_perf[[i]])))
          #v_name_all[[i]] = data.frame(rep(as.character(unlist(v_get_all[[i]][1])), nrow(v_player_list[[i]])))
       }
       v_players = rbindlist(v_player_list, fill = T)
       v_players_perf = rbindlist(v_player_list_perf, fill = T)
       v_names_players_perf <<- rbindlist(v_name_2, fill = T)
       v_names_players <<- rbindlist(v_name, fill = T)
       v_players_all <<- cbind(v_players,v_names_players)
       v_players_perf <<- cbind(v_players_perf,v_names_players_perf)
       v_players_perf_file_name = paste("v_players_perf_",start,"_",limit,".txt", sep = "")
       v_players_all_file_name = paste("v_players_all_",start,"_",limit,".txt", sep = "")
       write.delim(v_players_perf,v_players_perf_file_name, sep = "~")   
       write.delim(v_players_all,v_players_all_file_name, sep = "~")
    }    
    
## Start    
footy_scrape = function(start,end){    
   if(start > end){
      stop("You must ensure that your starting year is before your end year. For instance, having a start of 2014 and an end year of 2015. However,having an start year of 2015 and an end year of 2014, is not acceptable")
   }
   if(start < 1997){
      stop("Analysis only designed to run from 1997, where data is more complete. Please choose a starting year that is either from or beyond 1997.")
   }
    suppressMessages(library(httr))
    suppressMessages(library(XML))
    suppressMessages(library(data.table))
    suppressMessages(library(caroline))
    ##PUlling yearly stats for each player in each team that played for that season
    different_years = 1995:1996
    afl_12 = c("Carlton","Collingwood","Essendon","Fitzroy","Footscray","Geelong","Hawthorn","Melbourne","North Melbourne","Richmond","St Kilda","Sydney")
    afl_14 = c("Brisbane","Carlton","Collingwood","Essendon","Fitzroy","Footscray","Geelong","Hawthorn","Melbourne","North Melbourne","Richmond","St Kilda","Sydney","West Coast")
    afl_15 = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fitzroy","Footscray","Geelong","Hawthorn","Melbourne","North Melbourne","Richmond","St Kilda","Sydney","West Coast")
    afl_16_96 = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fitzroy","Footscray","Fremantle","Geelong","Hawthorn","Melbourne","North Melbourne","Richmond","St Kilda","Sydney","West Coast")
    afl_16 = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle","Geelong","Hawthorn","Melbourne","North Melbourne","Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")
    afl_17 = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle","Geelong","Gold Coast","Hawthorn","Melbourne","North Melbourne","Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")
    afl_18 = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle","Geelong","Gold Coast","GWS Giants","Hawthorn","Melbourne","North Melbourne","Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")
    field_names = c("Number","Name","Games_Played","Kicks","Marks","Handballs","Disposals","Disposal_Average","Goals","Behinds","Hitouts","Tackles","Running_Bounces","Inside_50s","Clearances","Clangers","Free_Kicks_For","Free_Kicks_Against","Brownlow_Votes","Contested_Possessions","Uncontested_Possessions","Contested_Marks","Marks_Inside_50","One_percenters","Bounces","Goal_assist","Percentage_of_game_played","Sub_On_Off", "Team","Year")  
    v_years_range = start:end
    urls_to_search = paste("http://afltables.com/afl/stats/",v_years_range,".html", sep = "")
    for(i in 1:length(start:end)){
       v_get = GET(urls_to_search[[i]])
       v_results = readHTMLTable(rawToChar(v_get$content), stringsAsFactors = F)
       v_results_len = length(v_results)
       v_results_new = v_results[2:v_results_len]
       sel_table =  ifelse(v_results_len == 17, "afl_16",  
                        ifelse(v_results_len == 18,"afl_17","afl_18"))
       for(j in 1:length(get(sel_table))){
          v_results_new[[j]][,(ncol(v_results_new[[j]])+1)] = get(sel_table)[j]
                 }
       v_results_new_temp <<-  rbindlist(v_results_new)
       assign(paste("results_",v_years_range[i],sep="") ,v_results_new_temp)
      }
     v_tables <<- paste("results_",v_years_range,sep="")
     v_results_final_list = rep(list(matrix(NA, ncol = 1000, nrow = 30)), length(v_years_range))
   for(h in 1:length(v_years_range)){
        v_results_final_list[[h]] = get(v_tables[h])
        v_results_final_list[[h]] = data.frame(v_results_final_list[[h]])
        v_results_final_list[[h]][,(ncol(v_results_final_list[[h]])+1)] = v_years_range[h]
                }     
          v_results_final = rbindlist(v_results_final_list)
          names(v_results_final) = field_names
          v_results_final[v_results_final == "?"] <- 0
          player_stats <<- v_results_final
   }    
## End     

## Status bar ##
testit <- function(x = sort(runif(20)), ...)
{
   pb <- txtProgressBar(...)
   for(i in c(0, x, 1)) {Sys.sleep(0.0005); setTxtProgressBar(pb, i)}
   close(pb)
}
## end ##
# Cluster function 1.0.1
cluster_select = function(data,y, scale_data = NULL,max_clusters = 15, ...){
   ## not sure this check is working as yet
   # if(class(dim) != "character" | class(expr) != "character"){
   #    stop("You have not stated either the dim or expr arguments as string. Please ensure these arguments are defined with speech marks.")
   # }
   if(!is.null(scale_data))   {   
      data = scale(data)
   }
   data = data[,-y]
   ##################### K MEANS ##########################################
   wss <- (nrow(data)-1)*sum(apply(data,2,var))
   for (i in 2:max_clusters) wss[i] <- sum(kmeans(data,centers=i)$betweenss/kmeans(data,centers=i)$totss)
   wss[1] = 0
   drop = rep(NA,length(wss) -1)
   drop[1] = 0.1
   for(i in 2:(length(wss)-1))   drop[i] <- round((wss[i+1] - wss[i])/wss[i],3)
   neg_drop = which(drop< 0.05)
   kmeans_cluster_selection = sprintf("The optimal number of cluster is %i for kmeans", neg_drop[1])
   ##################### Partioning around medoids ########################
   suppressMessages(library(fpc))
   pamk_best = pamk(data, criterion = "ch")
   medoids_cluster_selection = sprintf("The optimal number of clusters is %i for medoids", pamk_best$nc)
   ##################### Calinkski ########################################
   suppressMessages(library(vegan))
   calinski_fit = cascadeKM(data, 1,max_clusters)
   calinski_cluster_selection =  sprintf("The optimal number of clusters is %i for calinski", as.numeric(which.max(calinski_fit$results[2,])))     
   ##################### BIC, parameterised gaussian mixture model ########
   suppressMessages(library(mclust))
   BIC_fit = Mclust(as.matrix(data), G=1:max_clusters)
   BIC_cluster_selection =  sprintf("The optimal number of clusters is %i for BIC", dim(BIC_fit$z)[2])     
   ##################### Affinity proporgation (AP) #######################
   suppressMessages(library(apcluster))
   ap_fit <- apcluster(negDistMat(r=2), data)
   AP_cluster_selection =  sprintf("The optimal number of clusters is %i for AP", length(ap_fit@clusters))     
   ##################### Gap statistic ####################################
   suppressMessages(library(cluster))
   gapstat_fit = suppressMessages(clusGap(data, kmeans, max_clusters, B = 100, verbose = interactive()))
   ##################### nb clust ####################################   
   suppressMessages(library(NbClust))  
   nb_kmeans_fit <- NbClust(data,  distance = "euclidean", 
                            min.nc=2, max.nc=max_clusters, method = "kmeans", 
                            index = "kl" , alphaBeale = 0.1)
   nb_ward_fit <- NbClust(data,  distance = "euclidean", 
                          min.nc=2, max.nc=max_clusters, method = "ward.D", 
                          index = "kl", alphaBeale = 0.1)
   nb_kmeans_cluster_selection =  sprintf("The optimal number of clusters is %i for nb kmeans", as.numeric(names(sort(table(as.vector(nb_kmeans_fit$Best.nc[1])),decreasing=TRUE)[1])))     
   nb_ward_cluster_selection =  sprintf("The optimal number of clusters is %i for nb ward", as.numeric(names(sort(table(as.vector(nb_ward_fit$Best.nc[1])),decreasing=TRUE)[1])))     
   ##################### Cluster selection ###########################
   Cluster_method = c('kmeans', 'medoids','calinski','BIC','AP','Nb kmeans', 'Nb ward')
   Cluster_result <<- c(neg_drop[1], pamk_best$nc,as.numeric(which.max(calinski_fit$results[2,])),dim(BIC_fit$z)[2],length(ap_fit@clusters),as.numeric(names(sort(table(as.vector(nb_kmeans_fit$Best.nc[1])),decreasing=TRUE)[1])),as.numeric(names(sort(table(as.vector(nb_ward_fit$Best.nc[1])),decreasing=TRUE)[1])))
   Cluster_results = data.frame(Cluster_method,Cluster_result)
   results_table = table(as.vector(Cluster_result))
   max_result = max(results_table)
   Optimal_clusters =  sprintf("The optimal number of clusters is %i across all models checked above", as.numeric(names(which(results_table == max_result))))
   Median_clusters =  sprintf("The median number from all cluster methods run is %i ", median(Cluster_result))
      Results = list(Cluster_results = Cluster_results, Optimal_clusters = Optimal_clusters,Median_clusters = Median_clusters)
   ifelse(length(as.numeric(names(which(results_table == max_result)))) == 6,"No optimal cluster selected",print(Results))
   k_search = median(Cluster_result)
   library(skmeans)
   data = as.matrix(data)
   cluster_skmeans <<- skmeans(data,k_search)   
   }

############### check unique values ###############
l <- sapply(trainer, function(x) unique(x))
###############



### Summarise syntax ###########
users %>%
   group_by(PWC_USERID, PWC_PERIOD) %>%
   summarize(line_count = length(PWC_PERIOD))


x = users %>%
   group_by(PWC_USERID, PWC_PERIOD) %>%
   summarize(line_count = length(PWC_PERIOD))



############ Batch Forecasting  ##################
## a) Line for line code  ##
library(dplyr)
library(lubridate)
library(forecast)
library(reshape2)
users$Year = as.character(year(as.Date(users$PWC_EFFECTDATE,"%d/%m/%Y")))
jnl_summ = users %>%
   group_by(PWC_JNLID,PWC_PERIOD, PWC_USERID) %>%
   summarize(line_count = length(unique(PWC_JNLID)))
user_list = unique(jnl_summ$PWC_USERID)
cross_tab_users = table(jnl_summ$PWC_USERID,jnl_summ$PWC_PERIOD)
period_list = sort(as.numeric(unique(jnl_summ$PWC_PERIOD)))
ts_data = data.frame(period_list)
n_users = nrow(cross_tab_users)
for(i in 1:n_users)   ts_data[,i+1] = cross_tab_users[i,]
names(ts_data) = c('Period', user_list)
ts_jnls <- ts(ts_data[,-1],f=12,s=1)
ns <- ncol(ts_jnls)
h <- 12
fcast <- matrix(NA,nrow=h,ncol=ns)
fcast_low_inner <- matrix(NA,nrow=h,ncol=ns)
fcast_high_inner <- matrix(NA,nrow=h,ncol=ns)
fcast_low_outer <- matrix(NA,nrow=h,ncol=ns)
fcast_high_outer <- matrix(NA,nrow=h,ncol=ns)
# complete data prep, formatting for mean forecasts
for(i in 1:ns) fcast[,i] <- forecast(ts_jnls[,i],h=h)$mean
jnl_fcast = round(fcast,2)
jnl_fcast = data.frame(jnl_fcast)
names(jnl_fcast) = user_list
fcast_period_list = (period_list+h)
jnl_fcast = data.frame(cbind(Period = fcast_period_list,jnl_fcast))
jnl_fcast_all = rbind(ts_data,jnl_fcast)
jnl_fcast_all_tab = melt(jnl_fcast_all, id = c("Period"))
jnl_fcast_all_tab$forecast_metric = "Mean"
# end: complete data prep, formatting for mean forecasts
for(i in 1:ns) fcast_low_inner[,i] <- forecast(ts_jnls[,i],h=h)$lower[,1]
jnl_fcast_low_inner = round(fcast_low_inner,2)
jnl_fcast_low_inner = data.frame(jnl_fcast_low_inner)
names(jnl_fcast_low_inner) = user_list
jnl_fcast_low_inner = data.frame(cbind(Period = fcast_period_list,jnl_fcast_low_inner))
jnl_fcast_all_low_inner = rbind(ts_data,jnl_fcast_low_inner)
jnl_fcast_all_tab_low_inner = melt(jnl_fcast_all_low_inner, id = c("Period"))
jnl_fcast_all_tab_low_inner$forecast_metric = "Inner lower"
jnl_fcast_all_tab_low_inner =  jnl_fcast_all_tab_low_inner[!(Period  %in% period_list),]
# end: complete data prep, formatting for inner lower forecasts
for(i in 1:ns) fcast_low_outer[,i] <- forecast(ts_jnls[,i],h=h)$lower[,2]
jnl_fcast_low_outer = round(fcast_low_outer,2)
jnl_fcast_low_outer = data.frame(jnl_fcast_low_outer)
names(jnl_fcast_low_outer) = user_list
jnl_fcast_low_outer = data.frame(cbind(Period = fcast_period_list,jnl_fcast_low_outer))
jnl_fcast_all_low_outer = rbind(ts_data,jnl_fcast_low_outer)
jnl_fcast_all_tab_low_outer = melt(jnl_fcast_all_low_outer, id = c("Period"))
jnl_fcast_all_tab_low_outer$forecast_metric = "Outer lower"
jnl_fcast_all_tab_low_outer =  jnl_fcast_all_tab_low_outer[!(Period  %in% period_list),]
# end: complete data prep, formatting for outer lower forecasts
for(i in 1:ns) fcast_high_inner[,i] <- forecast(ts_jnls[,i],h=h)$upper[,1]
jnl_fcast_high_inner = round(fcast_high_inner,2)
jnl_fcast_high_inner = data.frame(jnl_fcast_high_inner)
names(jnl_fcast_high_inner) = user_list
jnl_fcast_high_inner = data.frame(cbind(Period = fcast_period_list,jnl_fcast_high_inner))
jnl_fcast_all_high_inner = rbind(ts_data,jnl_fcast_high_inner)
jnl_fcast_all_tab_high_inner = melt(jnl_fcast_all_high_inner, id = c("Period"))
jnl_fcast_all_tab_high_inner$forecast_metric = "Inner higher"
jnl_fcast_all_tab_high_inner =  jnl_fcast_all_tab_high_inner[!(Period  %in% period_list),]
# end: complete data prep, formatting for Inner higher forecasts
for(i in 1:ns) fcast_high_outer[,i] <- forecast(ts_jnls[,i],h=h)$upper[,2]
jnl_fcast_high_outer = round(fcast_high_outer,2)
jnl_fcast_high_outer = data.frame(jnl_fcast_high_outer)
names(jnl_fcast_high_outer) = user_list
jnl_fcast_high_outer = data.frame(cbind(Period = fcast_period_list,jnl_fcast_high_outer))
jnl_fcast_all_high_outer = rbind(ts_data,jnl_fcast_high_outer)
jnl_fcast_all_tab_high_outer = melt(jnl_fcast_all_high_outer, id = c("Period"))
jnl_fcast_all_tab_high_outer$forecast_metric = "Outer higher"
jnl_fcast_all_tab_high_outer =  jnl_fcast_all_tab_high_outer[!(Period  %in% period_list),]
# end: complete data prep, formatting for Inner higher forecasts
fcast_all = rbind(jnl_fcast_all_tab,jnl_fcast_all_tab_low_inner,jnl_fcast_all_tab_low_outer,jnl_fcast_all_tab_high_inner,jnl_fcast_all_tab_high_outer)
## a) End: Line for line code  ##

## b) In function format  ##
# To be completed

############### Clustering number Selection ##################
## a) Line for line code  ##
library(skmeans)
library(mclust)
fsli_summ = users %>%
   group_by(PWC_USERID,R_Code.PWC_FSLI) %>%
   summarize(line_count = length(R_Code.PWC_FSLI))
user_list = unique(fsli_summ$PWC_USERID)

cross_tab_fsli = as.data.frame.matrix(table(fsli_summ$PWC_USERID,fsli_summ$R_Code.PWC_FSLI))
cross_tab_fsli_lines = data.frame(as.data.frame.matrix(table(users$PWC_USERID,users$R_Code.PWC_FSLI)))
cross_tab_fsli_lines_mat = as.matrix(table(users$PWC_USERID,users$R_Code.PWC_FSLI)) 

skmeans(cross_tab_fsli_lines_mat,5) 
x = Mclust(cross_tab_fsli_lines_mat) 

nrow_users = nrow(cross_tab_fsli_lines) 
ncol_users = ncol(cross_tab_fsli_lines) 
fsli_lines_mat = matrix(NA,nrow = nrow_users,ncol = ncol_users) 
for(i in 1:ncol_users){ 
   fsli_lines_mat[,i] = cross_tab_fsli_lines[,i]
}
############### Clustering Selection ##################
cluster_create = function(data, clusters, row_ids,  scale = NULL,iter = 5,results = "all", ...){
   ###### 1.Loading packages ###############
   suppressMessages(library(skmeans))
   suppressMessages(library(cluster))
   suppressMessages(library(caroline))
   ###### 2. Setting conditionals ###########
   if(!is.null(scale)){
      data = scale(data)
   }
   ###### 3a) K MEANS ###########
   kmeans_wss = sum(kmeans(data,centers=clusters)$betweenss/kmeans(data,centers=clusters)$totss)
   kmeans_results = matrix(NA, nrow = nrow(data), ncol = iter)
   kmeans_results[,1] = kmeans(data,centers=clusters)$cluster
   for(i in 2:iter) {
      kmeans_wss[i] = sum(kmeans(data,centers=clusters)$betweenss/kmeans(data,centers=clusters)$totss)
      kmeans_results[,i] = kmeans(data,centers=clusters)$cluster
   }
   kmeans_max_result = which.max(kmeans_wss)
   kmeans_result_df = data.frame(row_ids, cluster = kmeans_results[,kmeans_max_result], method = rep("kmeans", length(row_ids)))
   rm(kmeans_results);rm(kmeans_wss);rm(kmeans_max_result)
   ###### 3b) sk means ########## 
   data_mat = as.matrix(data)
   sk_fit = skmeans(data_mat,clusters)
   skmeans_result_df = data.frame(row_ids, cluster = sk_fit$cluster, method = rep("skmeans", length(row_ids)))   
   rm(data_mat);rm(sk_fit)
   ###### 3c) Heirachical ########
   hier_dist = hclust(dist(data))
   hier_fit = cutree(hier_dist,clusters) 
   hier_result_df = data.frame(row_ids, cluster = hier_fit, method = rep("hierachical", length(row_ids)))
   rm(hier_dist);rm(hier_fit)
   ###### 3d) Divisive cluster ###
   diana_fit = diana(data, stand = TRUE)
   diana_create = cutree(as.hclust(diana_fit), k = clusters)
   diana_result_df = data.frame(row_ids, cluster = diana_create, method = rep("diana", length(row_ids)))
   rm(diana_fit);rm(diana_create)
   ###### 4. Results ###########
   results_bind = rbind(kmeans_result_df,skmeans_result_df,hier_result_df,diana_result_df)
   if(results == "all"){
      write.delim(results_bind, "results_cluster.txt", sep = "|")  
   }
   else{
      results_filter = results_bind[which(results_bind$method %in% results),]
      write.delim(results_filter, "results_cluster.txt", sep = "|")  
   }
}


################### player stats ####################
library(XML)
library(httr)
library(data.table)

teams_list = c("http://afltables.com/afl/stats/alltime/adelaide.html","http://afltables.com/afl/stats/alltime/brisbanel.html","http://afltables.com/afl/stats/alltime/carlton.html","http://afltables.com/afl/stats/alltime/collingwood.html","http://afltables.com/afl/stats/alltime/essendon.html","http://afltables.com/afl/stats/alltime/fremantle.html","http://afltables.com/afl/stats/alltime/geelong.html","http://afltables.com/afl/stats/alltime/goldcoast.html"  ,"http://afltables.com/afl/stats/alltime/gws.html","http://afltables.com/afl/stats/alltime/hawthorn.html","http://afltables.com/afl/stats/alltime/melbourne.html","http://afltables.com/afl/stats/alltime/kangaroos.html"  ,"http://afltables.com/afl/stats/alltime/padelaide.html","http://afltables.com/afl/stats/alltime/richmond.html","http://afltables.com/afl/stats/alltime/stkilda.html","http://afltables.com/afl/stats/alltime/swans.html"  ,"http://afltables.com/afl/stats/alltime/westcoast.html","http://afltables.com/afl/stats/alltime/bullldogs.html")
players_list = rep(list(matrix(NA, ncol = 11, nrow = 3000)), length(teams_list))
for(i in 1:length(teams_list)){
   get_team = GET(teams_list[i])
   players_list[[i]] =  data.frame(readHTMLTable(rawToChar(get_team$content), stringsAsFactors = F))  
   }
player_info = rbindlist(players_list)

##########################################
###Caret Syntax
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control, tuneLength=10)
importance <- varImp(model, scale=FALSE)
control <- trainControl(method="repeatedcv", number=10, repeats=3,allowParallel = TRUE,classProbs = TRUE,summaryFunction = twoClassSummary)

'%!in%' <- function(x,y)!('%in%'(x,y))

library(caret)
# load the dataset
data(iris)
# prepare resampling method
control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(7)
fit <- train(Species~., data=iris, method="rf", metric="logLoss", trControl=control)

library(ElemStatLearn)
library(gbm)

set.seed(1375)

# formula: the formula to pass to gbm()
# data: the data set to use
# column: the class column to use
classPlots <- function (formula, data, column) {
   
   class_column <- as.character(data[,column])
   class_values <- names(table(class_column))
   class_indexes <- sapply(class_values, function(x) which(class_column == x))
   split_data <- lapply(class_indexes, function(x) marketing[x,])
   object <- lapply(split_data, function(x) gbm(formula, data = x))
   rel.inf <- lapply(object, function(x) summary.gbm(x, plotit=FALSE))
   
   nobjs <- length(class_values)
   for( i in 1:nobjs ) {
      tmp <- rel.inf[[i]]
      tmp.names <- row.names(tmp)
      tmp <- tmp$rel.inf
      names(tmp) <- tmp.names
      
      barplot(tmp, horiz=TRUE, col='red',
              xlab="Relative importance", main=paste0("Class = ", class_values[i]))
   }
   rel.inf
}

par(mfrow=c(1,2))
classPlots(Income ~ Marital + Age, data = marketing, column = 2)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
fit.c50 <- train(Class~., data=dataset, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(Class~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)

## install packages 
install.packages('caret')
install.packages('forecast')
install.packages('ggplot2')
install.packages('data.table')      
install.packages('httr')
install.packages('XML')
install.packages('caroline')
install.packages('dplyr')
install.packages('reshape')
install.packages('tidyquant')
install.packages('tidyverse')


twitter_scrape = function(search, no_tweets, tzone = "Australia/Melbourne", date_filter = NULL) {
   suppressMessages(library(twitteR));   suppressMessages(library(ROAuth));   suppressMessages(library(httr));   suppressMessages(library(lubridate));   suppressMessages(library(caroline))
   ############# API Access Keys #################
   consumer_key = 	"vt1yhzs7IlBOM5ZbZESxwM30X"
   consumer_secret =	"AVtPQ69T8Y1ZSpniXaVzzWSXRmtU3cHnIFoqjF2QboIwrp2NVc"
   access_token = "891240874201317377-2qSILkzxQ3g8sYYxWff8e6udo7f8jou"
   access_token_secret = "V22btYDnz99grNWwBEpUpB37pS1Qt88I26cam1ef6lLpn"
   setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
   ############# Scraping of data #################
   tweets_post = searchTwitter(search, n = no_tweets)
   tweets_post_df = rbindlist(lapply(tweets_post,as.data.frame))
   tweets_post_df$Date_Time = with_tz(ymd_hms(tweets_post_df$created, tz = "UTC"), tzone = tzone);   tweets_post_df$Date = ymd(str_sub(tweets_post_df$Date_Time,1,10));   tweets_post_df$Weekday = weekdays(tweets_post_df$Date_Time);   tweets_post_df$Hour = hour(tweets_post_df$Date_Time);   tweets_post_df$Minute = minute(tweets_post_df$Date_Time);   tweets_post_df$Day = day(tweets_post_df$Date_Time);   tweets_post_df$Month = month(tweets_post_df$Date_Time);   tweets_post_df$Dayofyear = yday(tweets_post_df$Date_Time)
   if(!is.null(date_filter)) {
      tweets_post_df = tweets_post_df[which(tweets_post_df$Date > date_filter), ]
      }
   search_result = paste("tweet_data_",search,".txt",sep = "")
   write.delim(tweets_post_df,search_result,sep = "|")
}	

twitter_token <- create_token(
   app = "lets see how this goes desc",
   consumer_key = 	"vt1yhzs7IlBOM5ZbZESxwM30X",
   consumer_secret =	"AVtPQ69T8Y1ZSpniXaVzzWSXRmtU3cHnIFoqjF2QboIwrp2NVc")
elders_tweets <- search_tweets(search, n = 10000)   
check = users_data(elders_tweets)

suppressMessages(library(DBI));suppressMessages(library(RSQLite));suppressMessages(library(data.table));   suppressMessages(library(lubridate));   suppressMessages(library(stringr))
# Initialize a temporary in memory database and copy a data.frame into it
#con <- dbConnect(RSQLite::SQLite(), ":memory:")
con <- dbConnect(SQLite(), "Elders_twitter.sqlite")
search = "EldersLimited"
search = "seekjobs"
tzone = "Australia/Adelaide"
repeat {
   tweets_post = searchTwitter(search, n = 10000,retryOnRateLimit=200,tweet_mode="extended")
   tweets_post_df = rbindlist(lapply(tweets_post,as.data.frame))
   tweets_post_df$Date_Time = with_tz(ymd_hms(tweets_post_df$created, tz = "UTC"), tzone = tzone);   tweets_post_df$Date = ymd(str_sub(tweets_post_df$Date_Time,1,10));   tweets_post_df$Weekday = weekdays(tweets_post_df$Date_Time);   tweets_post_df$Hour = hour(tweets_post_df$Date_Time);   tweets_post_df$Minute = minute(tweets_post_df$Date_Time);   tweets_post_df$Day = day(tweets_post_df$Date_Time);   tweets_post_df$Month = month(tweets_post_df$Date_Time);   tweets_post_df$Dayofyear = yday(tweets_post_df$Date_Time)
   dbWriteTable(con, "tweets_post", tweets_post,append = T)
   tweets_df = dbGetQuery(con, "SELECT * FROM tweets_post")
   tweets_df_unique = tweets_df[duplicated(tweets_df), ]
   dbWriteTable(con, "tweets_post_uniq", tweets_df_unique)
   #write.delim(tweets_post,paste("tweet_", day(Sys.time()),"_",hour(Sys.time()),"_",second(Sys.time()),".txt", sep = ""))
   Sys.sleep(1200)
}

dbListTables(con)
dbGetQuery(con, "SELECT * FROM tweets_post")
dbDisconnect(con)

require(dplyr)
require(dbplyr)
my_db <- src_sqlite( "my_db.sqlite3", create = TRUE)                 # create src
copy_to( my_db, iris, "my_table", temporary = FALSE)                 # create table
db_insert_into( con = my_db$con, table = "my_table", values = newdf) # insert into

check = DBI::dbGetQuery(con, "SELECT * FROM my_table")
as.POSIXlt(1504183364, origin="1970-01-01")

read_folder <- function(infolder) {
   data_frame(file = dir(infolder, full.names = TRUE)) %>%
      mutate(text = map(file, read_lines)) %>%
      transmute(id = basename(file), text) %>%
      unnest(text)
}
raw_text <- data_frame(folder = dir(training_folder, full.names = TRUE)) %>%
   unnest(map(folder, read_folder)) %>%
   transmute(newsgroup = basename(folder), id, text)

# users analysis
?geocode

tweet_token <- function(data) {
   require(tidytext)
   data(stop_words)
   tweets = data[,c("text","tweet_id")]
   mentions = data[,c("mentions_screen_name","tweet_id")]
   hashtags = data[,c("hashtags","tweet_id")]
   tweet_words = tweets %>%
      unnest_tokens(word, text)
   tweet_mentions = mentions %>%
      unnest_tokens(word, mentions_screen_name)
   tweet_hashtags = hashtags %>%
      unnest_tokens(word, hashtags)
   tweet_mentions = na.omit(tweet_mentions)
   tweet_hashtags = na.omit(tweet_hashtags)
   twitter_stop_words = data.frame(word = c("https","t.co","ufqyrmqwuh","rt","82ejseumye"), lexicon = rep("CZ", length(word)))    
   twitter_stop_words$word = as.character(twitter_stop_words$word);twitter_stop_words$lexicon = as.character(twitter_stop_words$lexicon)
   stop_words_new = rbind(stop_words, twitter_stop_words)
   tweet_words_wo_stop = anti_join(tweet_words,stop_words_new)
}


# 6 - Topic modelling
library(topicmodels)
data("AssociatedPress")
AssociatedPress
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics
ap_documents <- tidy(ap_lda, matrix = "gamma")

#5 Converting to and from non-tidy formats
data("AssociatedPress", package = "topicmodels")
AssociatedPress
terms <- Terms(AssociatedPress)
ap_td <- tidy(AssociatedPress)

library(tm.plugin.webmining)
library(purrr)
download_articles <- function(symbol) {
   WebCorpus(GoogleFinanceSource(paste0("ASX:", symbol)))
}
company = "Elders Ltd"
symbol = "ELD"
stock_articles <- data_frame(company = company,
                             symbol = symbol) %>%
   mutate(corpus = map(symbol, download_articles))
stock_tokens <- stock_articles %>%
   unnest(map(corpus, tidy)) %>%
   unnest_tokens(word, text) %>%
   select(company, datetimestamp, word, id, heading)


res <- try(log("a"),silent = TRUE)
class(res) == "try-error" 

#n-grams and correlations - will need to clean/filter out url coming into tweets
# to add to stop words - https t.co ufqyrmqwuh rt 82ejseumye. REMEMBER: To replace amp with and

tweet_bigrams <- tweets_unique[,c("text", "time_zone","tweet_id")] %>%
   unnest_tokens(bigram, text, token = "ngrams", n = 2)
tweet_bigrams %>%
   count(bigram, sort = TRUE)
bigrams_separated = tweet_bigrams %>%
   separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered = bigrams_separated %>%
   filter(!word1 %in% stop_words_new$word) %>%
   filter(!word2 %in% stop_words_new$word)
bigram_counts <- bigrams_filtered %>% 
   count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
   unite(bigram, word1, word2, sep = " ")


tweet_bigrams <- tweets_unique[,c("text", "time_zone","tweet_id")] %>%
   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
   count(bigram, sort = TRUE) %>%
   separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered = tweet_bigrams %>%
   filter(!word1 %in% stop_words_new$word, 
          !word2 %in% stop_words_new$word)

bigram_counts <- bigrams_filtered %>% 
   count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
   unite(bigram, word1, word2, sep = " ")

################# 1.0 Clean data and set up geo locations. Still need to send cleaning tables up to global environment ######
geocode_prep = function(tweets, users){
   suppressMessages(library(dplyr)); suppressMessages(library(DBI));  suppressMessages(library(RSQLite));suppressMessages(library(stringr));suppressMessages(library(ggraph));suppressMessages(library(ggmap));suppressMessages(library(stringr))
   users$description = str_replace_all(users$description, "[\r\n]" , "")
   tweets$text = str_replace_all(tweets$text, "[\r\n]" , "")
   tweets$text = str_replace(tweets$text,"&amp;","&")
   tweets$text = str_replace(tweets$text,"&amp;","&")
   users$created_at = as.POSIXlt(users$created_at, origin="1970-01-01")
   db_check_loc = try(dbGetQuery(con, "SELECT user_id FROM Location_IDs"),silent = TRUE)
   db_check_tz = try(dbGetQuery(con, "SELECT user_id FROM Time_Zone_IDs"),silent = TRUE)
   if(class(db_check_tz) == "try-error"){
      users_tz = unique(users[ ,"time_zone"])
      users_tz = data.frame(time_zone = na.omit(users_tz));users_tz$time_zone = as.character(users_tz$time_zone)
      tz_lon_lat = geocode(users_tz$time_zone)
      users_tz$lon = tz_lon_lat$lon
      users_tz$lat = tz_lon_lat$lat
      dbWriteTable(con, "Time_Zone_IDs", users_tz,overwrite = T)
   }
   if(class(db_check_tz) != "try-error"){
      TimeZone_IDs = dbGetQuery(con, "SELECT * FROM Time_Zone_IDs")
      users_tz = unique(users[ ,"time_zone"])
      users_tz = data.frame(time_zone = na.omit(users_tz));users_tz$time_zone = as.character(users_tz$time_zone)
      users_tz_new = anti_join(users_tz,TimeZone_IDs)
      if(nrow(users_tz_new) != 0){
         tz_lon_lat_new = geocode(users_tz_new$time_zone) 
         users_tz_new$lon = tz_lon_lat_new$lon    
         users_tz_new$lat = tz_lon_lat_new$lat             
         dbWriteTable(con, "Time_Zone_IDs", users_tz,append = T)
      }
   }
   if(class(db_check_loc) == "try-error"){
      users_location = unique(users[ ,c("user_id","location")])
      users_location = na.omit(users_location)
      users_long_lat = geocode(users_location$location)
      users_location$lon = users_long_lat$lon
      users_location$lat = users_long_lat$lat
      dbWriteTable(con, "Location_IDs", users_tz,overwrite = T)
   }
   if(class(db_check_loc) != "try-error"){   
      Location_IDs = dbGetQuery(con, "SELECT * FROM Location_IDs")
      users_location = unique(users[ ,c("user_id","location")])
      users_location = na.omit(users_location);names(users_location) = c("user_id","location")
      users_location_new = anti_join(users_location,Location_IDs)
      if(nrow(users_tz_new) != 0){
         location_lon_lat_new = geocode(users_location_new$location) 
         users_location_new$lon = tz_lon_lat_new$lon    
         users_location_new$lat = tz_lon_lat_new$lat  
         dbWriteTable(con, "Location_IDs", users_location_new,append = T)
      }
   }
}      
################# 1.1 Get tokens on words(after removing for stop words), hashtags and mentions ######
tweet_token <- function(data) {
   data(stop_words)
   tweets_words = data[,c("text","tweet_id")]
   mentions = data[,c("mentions_screen_name","tweet_id")]
   hashtags = data[,c("hashtags","tweet_id")]
   tweet_words = tweets_words %>%
      unnest_tokens(word, text)
   tweet_mentions = mentions %>%
      unnest_tokens(word, mentions_screen_name)
   tweet_hashtags = hashtags %>%
      unnest_tokens(word, hashtags)
   tweet_mentions = na.omit(tweet_mentions)
   tweet_hashtags = na.omit(tweet_hashtags)
   twitter_stop_words = data.frame(word = c("https","t.co","ufqyrmqwuh","rt","82ejseumye"), lexicon = rep("CZ", length(word)))    
   twitter_stop_words$word = as.character(twitter_stop_words$word);twitter_stop_words$lexicon = as.character(twitter_stop_words$lexicon)
   stop_words_new = rbind(stop_words, twitter_stop_words)
   tweet_words_wo_stop = anti_join(tweet_words,stop_words_new)
   write.delim(tweet_words_wo_stop,'tweet_words_wo_stop.txt', sep = "|")
   write.delim(tweet_mentions,'tweet_mentions.txt', sep = "|")
   write.delim(tweet_hashtags,'tweet_hashtags.txt', sep = "|")
}
################# 1.2 Sentiment analysis over tweets. Remember that sentiment scores must be added on to final tweets table  #############
tweet_sentiment = function(data){
   require(tidytext);require(pacman);require(sentimentr)
   nrc = get_sentiments("nrc")
   data(stop_words)
   tweets = data[,c("text","tweet_id")]
   sentiment_scores <<- sentiment_by(data$text)
   tweets_unique$sentiment_score = sentiment_scores$sentiment
   tweet_words = tweets %>%
      unnest_tokens(word, text)
   twitter_stop_words = data.frame(word = c("https","t.co","ufqyrmqwuh","rt","82ejseumye"), lexicon = rep("CZ", length(word)))    
   twitter_stop_words$word = as.character(twitter_stop_words$word);twitter_stop_words$lexicon = as.character(twitter_stop_words$lexicon)
   stop_words_new = rbind(stop_words, twitter_stop_words)
   tweet_words_wo_stop = anti_join(tweet_words,stop_words_new)
   tweet_words_sentiment = left_join(tweet_words_wo_stop,nrc) 
}	
#1.4 Clustering 
cluster_user = function(data,y){
   cluster_data = unique(data[,c("user_id","followers_count","friends_count","listed_count","favourites_count","statuses_count")])
   cluster_select = function(data,y, scale_data = NULL,max_clusters = 15, ...){
      if(!is.null(scale_data))   {   
         data = scale(data)
      }
      data = data[,-y]
      ##################### K MEANS ##########################################
      wss <- (nrow(data)-1)*sum(apply(data,2,var))
      for (i in 2:max_clusters) wss[i] <- sum(kmeans(data,centers=i)$betweenss/kmeans(data,centers=i)$totss)
      wss[1] = 0
      drop = rep(NA,length(wss) -1)
      drop[1] = 0.1
      for(i in 2:(length(wss)-1))   drop[i] <- round((wss[i+1] - wss[i])/wss[i],3)
      neg_drop = which(drop< 0.05)
      kmeans_cluster_selection = sprintf("The optimal number of cluster is %i for kmeans", neg_drop[1])
      ##################### Partioning around medoids ########################
      suppressMessages(library(fpc))
      pamk_best = pamk(data, criterion = "ch")
      medoids_cluster_selection = sprintf("The optimal number of clusters is %i for medoids", pamk_best$nc)
      ##################### Calinkski ########################################
      suppressMessages(library(vegan))
      calinski_fit = cascadeKM(data, 1,max_clusters)
      calinski_cluster_selection =  sprintf("The optimal number of clusters is %i for calinski", as.numeric(which.max(calinski_fit$results[2,])))     
      ##################### BIC, parameterised gaussian mixture model ########
      suppressMessages(library(mclust))
      BIC_fit = Mclust(as.matrix(data), G=1:max_clusters)
      BIC_cluster_selection =  sprintf("The optimal number of clusters is %i for BIC", dim(BIC_fit$z)[2])     
      ##################### Affinity proporgation (AP) #######################
      suppressMessages(library(apcluster))
      ap_fit <- apcluster(negDistMat(r=2), data)
      AP_cluster_selection =  sprintf("The optimal number of clusters is %i for AP", length(ap_fit@clusters))     
      ##################### Gap statistic ####################################
      suppressMessages(library(cluster))
      gapstat_fit = suppressMessages(clusGap(data, kmeans, max_clusters, B = 100, verbose = interactive()))
      ##################### nb clust ####################################   
      suppressMessages(library(NbClust))  
      nb_kmeans_fit <- NbClust(data,  distance = "euclidean", 
                               min.nc=2, max.nc=max_clusters, method = "kmeans", 
                               index = "kl" , alphaBeale = 0.1)
      nb_ward_fit <- NbClust(data,  distance = "euclidean", 
                             min.nc=2, max.nc=max_clusters, method = "ward.D", 
                             index = "kl", alphaBeale = 0.1)
      nb_kmeans_cluster_selection =  sprintf("The optimal number of clusters is %i for nb kmeans", as.numeric(names(sort(table(as.vector(nb_kmeans_fit$Best.nc[1])),decreasing=TRUE)[1])))     
      nb_ward_cluster_selection =  sprintf("The optimal number of clusters is %i for nb ward", as.numeric(names(sort(table(as.vector(nb_ward_fit$Best.nc[1])),decreasing=TRUE)[1])))     
      ##################### Cluster selection ###########################
      Cluster_method = c('kmeans', 'medoids','calinski','BIC','AP','Nb kmeans', 'Nb ward')
      Cluster_result <<- c(neg_drop[1], pamk_best$nc,as.numeric(which.max(calinski_fit$results[2,])),dim(BIC_fit$z)[2],length(ap_fit@clusters),as.numeric(names(sort(table(as.vector(nb_kmeans_fit$Best.nc[1])),decreasing=TRUE)[1])),as.numeric(names(sort(table(as.vector(nb_ward_fit$Best.nc[1])),decreasing=TRUE)[1])))
      Cluster_results = data.frame(Cluster_method,Cluster_result)
      results_table = table(as.vector(Cluster_result))
      max_result = max(results_table)
      Optimal_clusters =  sprintf("The optimal number of clusters is %i across all models checked above", as.numeric(names(which(results_table == max_result))))
      Median_clusters =  sprintf("The median number from all cluster methods run is %i ", median(Cluster_result))
      Results = list(Cluster_results = Cluster_results, Optimal_clusters = Optimal_clusters,Median_clusters = Median_clusters)
      ifelse(length(as.numeric(names(which(results_table == max_result)))) == 6,"No optimal cluster selected",print(Results))
      k_search = median(Cluster_result)
      library(skmeans)
      data = as.matrix(data)
      cluster_skmeans <<- skmeans(data,k_search)   
      cluster_results <<- as.vector(cluster_skmeans$cluster)
   }   
   cluster_select(cluster_data,y)
   user_cluster <<- data.frame(user_id = data[,y],cluster_results = cluster_results)
}

g <- graph(c(
   1, 2, 1, 3, 1, 4, 
   2, 3, 2, 6, 3, 1, 
   3, 5, 4, 2, 4, 1, 
   4, 5, 5, 2, 5, 6, 
   6, 3, 6, 4), 
   directed=TRUE)

M = get.adjacency(g, sparse = FALSE)
M = t(M / rowSums(M))
n = nrow(M)

U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
beta=0.85
A = beta*M+(1-beta)*U
e = eigen(A)
v <- e$vec[,1]
v <- as.numeric(v) / sum(as.numeric(v))
v

page.rank(g)$vector

library(expm)
n = nrow(M)
U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
beta=0.85
A = beta*M+(1-beta)*U
r = matrix(data=rep(1/n, n), nrow=n, ncol=1)
t(A%^%100 %*% r)


### Page rank code 
library(igraph)

page_rank = function(data){
   mentions = data[,c("mentions_screen_name","tweet_id")]
   users = data[,c("tweet_id","screen_name")]
   tweet_mentions = mentions %>%
      unnest_tokens(word, mentions_screen_name)
   mention_direct = left_join(tweet_mentions,users)   
   mention_results = na.omit(mention_direct)
   mention_matrix  =  get.adjacency(graph.edgelist(as.matrix(mention_results[,c("screen_name","word")] ), directed=T))
   mention_graph =  graph_from_adjacency_matrix(mention_matrix, mode = "directed")
   mention_pagerank <<- page.rank(mention_graph)
   } 

game_results$home_goals  = as.numeric(sapply(strsplit(game_results$home_team_qtr_score,".",fixed = T), `[`, 1))
class_indexes <- lapply(x, function(x) nrow(x) == 22 & ncol(x) == 25)
str_count(s,coll("("))
head(as.numeric(sapply(strsplit(player_info$Seasons,",",fixed = T), `[`, 2)))

url = GET("http://afltables.com/afl/seas/ladders/laddersyby.html")
mydata = readHTMLTable(rawToChar(url$content), stringsAsFactors = F)
current_format =  which(head(lapply(mydata , function(x) dim(x)[1])) == 18)  
current_table = which(lapply(mydata , function(x) sum(x == '?')) == 0)
select_elements = which(current_format %in% current_table)

mtcars[order(mtcars$mpg,decreasing = T),]

num_convert = function(data, columns_to_convert){
   for(i in columns_to_convert){
      data[,i] = as.numeric(data[,i])
   }
   num_data <<- data
}

xgb_grid_1 = expand.grid(
       nrounds = 1000,
        eta = c(0.01, 0.001, 0.0001),
        max_depth = c(2, 4, 6, 8, 10),
        gamma = 1
    )


observation_level_variable_importance <- function(train_data, live_data, outcome_name, eta = 0.2, 
                                                  max_depth=4, max_rounds=3000, number_of_factors=2) {
   
   # install.packages('dplyr')
   require(dplyr)
   # install.packages('xgboost')
   require(xgboost)
   
   set.seed(1234)
   split <- sample(nrow(train_data), floor(0.9*nrow(train_data)))
   train_data_tmp <- train_data[split,]
   val_data_tmp <- train_data[-split,]
   
   feature_names <- setdiff(names(train_data_tmp), outcome_name)
   dtrain <- xgb.DMatrix(data.matrix(train_data_tmp[,feature_names]), 
                         label=train_data_tmp[,outcome_name], missing=NaN)
   dval <- xgb.DMatrix(data.matrix(val_data_tmp[,feature_names]), 
                       label=val_data_tmp[,outcome_name], missing=NaN)
   watchlist <- list(eval = dval, train = dtrain)
   param <- list(  objective = "binary:logistic",
                   eta = eta,
                   max_depth = max_depth,
                   subsample= 0.9,
                   colsample_bytree= 0.9
   )
   
   xgb_model <- xgb.train ( params = param,
                            data = dtrain,
                            eval_metric = "auc",
                            nrounds = 3000,
                            missing=NaN,
                            verbose = 1,
                            print_every_n = 10,
                            early_stopping_rounds = 20,
                            watchlist = watchlist,
                            maximize = TRUE)
   
   original_predictions <- predict(xgb_model, 
                                   data.matrix(live_data[,feature_names]), 
                                   outputmargin=FALSE, missing=NaN)
   
   # strongest factors
   new_preds <- c()
   for (feature in feature_names) {
      print(feature)
      live_data_trsf <- live_data
      # neutralize feature to population mean
      if (sum(is.na(train_data[,feature])) > (nrow(train_data)/2)) {
         live_data_trsf[,feature] <- NA
      } else {
         live_data_trsf[,feature] <- mean(train_data[,feature], na.rm = TRUE)
      }
      predictions <- predict(object=xgb_model, data.matrix(live_data_trsf[,feature_names]),
                             outputmargin=FALSE, missing=NaN)
      new_preds <- cbind(new_preds, original_predictions - predictions)
   }
   
   positive_features <- c()
   negative_features <- c()
   
   feature_effect_df <- data.frame(new_preds)
   names(feature_effect_df) <- c(feature_names)
   
   for (pred_id in seq(nrow(feature_effect_df))) {
      vector_vals <- feature_effect_df[pred_id,]
      vector_vals <- vector_vals[,!is.na(vector_vals)]
      positive_features <- rbind(positive_features, 
                                 c(colnames(vector_vals)[order(vector_vals, 
                                                               decreasing=TRUE)][1:number_of_factors]))
      negative_features <- rbind(negative_features, 
                                 c(colnames(vector_vals)[order(vector_vals, 
                                                               decreasing=FALSE)][1:number_of_factors]))
   }
   
   positive_features <- data.frame(positive_features)
   names(positive_features) <- paste0('Pos_', names(positive_features))
   negative_features <- data.frame(negative_features)
   names(negative_features) <- lt;- paste0('Neg_', names(negative_features))
   
   return(data.frame(original_predictions, positive_features, negative_features))
} 


random_splits <- runif(nrow(y_train))
train_df <- y_train[random_splits < .75,]
dim(train_df)

## [1] 367   6

test_df <- y_train[random_splits >= .75,]
outcome_name <- 'Top_4'

preds <- observation_level_variable_importance(train_data = train_df, 
                                               live_data = test_df, 
                                               outcome_name = outcome_name)


correlation_table(data = heart_disease, str_target = "has_heart_disease")

"http://probabilistic-footy.monash.edu/~footy/data/fixture.2016.txt"


setwd("C:/Users/Chris Zucchet/Documents")
library(readr)
library(RSQLite)
player_stats=  read_delim("player_stats_modern_30_10.txt", delim = "|")


s = read_delim("http://probabilistic-footy.monash.edu/~footy/data/fixture.2016.txt", delim = "\t",skip = 20)
a = read_delim("http://probabilistic-footy.monash.edu/~footy/data/fixture.2016.txt", delim = " ",skip = 18,col_names =F)
a$X1 = sapply(strsplit(as.character(a$X1), "\\."), "[[", 1)
a$date = sapply(strsplit(as.character(a$X5), "\\:"), "[[", 1)
a$time = sapply(strsplit(as.character(a$X5), "\\:"), "[[", 2)
a$home_score = paste(substr(sapply(strsplit(as.character(a$X6), '\\#'), '[', 2),2,10),".",a$X7,".",sapply(strsplit(as.character(a$X8), ")"), "[[", 1),sep ="")
a$away_score = paste(substr(as.character(a$X9),2,10),".",a$X10,".",sapply(strsplit(as.character(a$X11), ")"), "[[", 1),sep ="")



b = read_delim("http://probabilistic-footy.monash.edu/~footy/data/fixture.2017.txt", delim = " ",skip = 18,col_names =F)
b$X1 = sapply(strsplit(as.character(b$X1), "\\."), "[[", 1)
b$date = sapply(strsplit(as.character(b$X5), "\\:"), "[[", 1)
b$time = sapply(strsplit(as.character(b$X5), "\\:"), "[[", 2)
b$home_score = paste(substr(sapply(strsplit(as.character(b$X6), '\\#'), '[', 2),2,10),".",b$X7,".",sapply(strsplit(as.character(b$X8), ")"), "[[", 1),sep ="")
b$away_score = paste(substr(as.character(b$X9),2,10),".",b$X10,".",sapply(strsplit(as.character(b$X11), ")"), "[[", 1),sep ="")
b$Year = rep("2017",nrow(b))
names(b) = c("Round","Home_team","Away_team","Venue","X5","X6","X7","X8","X9","X10","X11","date","time","home_score","away_score","Year")
b_filter = b[,c("Round","Home_team","Away_team","Venue","date","time","home_score","away_score","Year")]
b_filter$winner = ifelse(as.numeric(sapply(strsplit(as.character(b$home_score), "\\."), "[[", 3)) >
                            as.numeric(sapply(strsplit(as.character(b$away_score), "\\."), "[[", 3)),b$Home_team,b$Away_team)
b_filter$winner_location =   ifelse(as.numeric(sapply(strsplit(as.character(b$home_score), "\\."), "[[", 3)) > 
                                       as.numeric(sapply(strsplit(as.character(b$away_score), "\\."), "[[", 3)),"Home","Away")

b_filter[b_filter =="G_W_Sydney"] <- "GWS Giants";b_filter[b_filter =="W_Bulldogs"] <- "Western Bulldogs";b_filter[b_filter =="W_Coast"] <- "West Coast";b_filter[b_filter =="Gold_Coast"] <- "Gold Coast"
b_filter[b_filter =="Kangaroos"] <- "North Melbourne";b_filter[b_filter =="P_Adelaide"] <- "Port Adelaide";b_filter[b_filter =="St_Kilda"] <- "St Kilda"

team_list =c("Adelaide","Carlton","Collingwood","Western Bulldogs","Melbourne","St Kilda" ,"Port Adelaide","Sydney","Essendon","Hawthorn","Brisbane","Gold Coast","West Coast","Geelong","Fremantle" ,"Richmond","North Melbourne" ,"GWS Giants")
team_ids = c("01","03","04","07","11","15","13","16","05","10","19","20","18","09","08","14","12", "21")
home_df = data.frame(Home_team  = team_list, home_id = team_ids);home_df$Home_team = as.character(home_df$Home_team)
away_df = data.frame(Away_team  = team_list, away_id = team_ids);away_df$Away_team = as.character(away_df$Away_team)
b_filter_temp = left_join(b_filter,home_df);b_filter_temp$home_id = as.character(b_filter_temp$home_id)
b_filter_temp2 = left_join(b_filter_temp,away_df);b_filter_temp2$away_id = as.character(b_filter_temp2$away_id)
b_filter_temp2$teams = ifelse(b_filter_temp2$home_id>b_filter_temp2$away_id, paste(b_filter_temp2$away_id,b_filter_temp2$home_id,sep = ""),paste(b_filter_temp2$ home_id,b_filter_temp2$away_id,sep = ""))
b_filter_temp2$web_id = paste("https://afltables.com/afl/stats/games/",b_filter_temp2$Year,"/",b_filter_temp2$teams,b_filter_temp2$date,".html", sep ="")
b_filter_temp2$ID = paste(b_filter_temp2$Year,"_",b_filter_temp2$Round,"_",b_filter_temp2$date,"_",b_filter_temp2$time,"_",b_filter_temp2$Venue, sep="")
b_filter_temp2$Diff = as.numeric(sapply(strsplit(as.character(b_filter_temp2$home_score), '\\.'), '[', 3)) - as.numeric(sapply(strsplit(as.character(b_filter_temp2$away_score), '\\.'), '[', 3))
b_filter_temp2$Diff_Home = ifelse(b_filter_temp2$winner == b_filter_temp2$Home_team, b_filter_temp2$Diff*1,b_filter_temp2$Diff*-1)
b_filter_temp2$Diff_Away = ifelse(b_filter_temp2$winner == b_filter_temp2$Home_team, b_filter_temp2$Diff*-1,b_filter_temp2$Diff*1)

con <- dbConnect(SQLite(), "AFL.sqlite")

for(i in 1:length(b_filter_temp2$web_id)){
   v_get = GET(b_filter_temp2$web_id[i])
   v_results = readHTMLTable(rawToChar(v_get$content), stringsAsFactors = F)
   cond <- sapply(v_results, function(x) class(x)== "data.frame")
   v_df = v_results[cond]
   cond2 <- sapply(v_df, function(x) nrow(x) == 22 &ncol(x) == 25)
   v_df2 = v_df[cond2]
   v_df2[[1]]$Team = rep(b_filter_temp2$Home_team[[i]], nrow(v_df2[[1]]));v_df2[[1]]$ID = rep(b_filter_temp2$ID[[i]], nrow(v_df2[[1]]));v_df2[[1]]$Home_Away = rep("Home", nrow(v_df2[[1]]))
   v_df2[[2]]$Team = rep(b_filter_temp2$Away_team[[i]], nrow(v_df2[[2]]));v_df2[[2]]$ID = rep(b_filter_temp2$ID[[i]], nrow(v_df2[[2]]));v_df2[[2]]$Home_Away = rep("Away", nrow(v_df2[[2]]))
   cond3 <- sapply(v_df, function(x) nrow(x) == 22 &ncol(x) == 7)
   v_df3 = v_df[cond3]
   v_df3[[1]] =  v_df3[[1]][,c("Player","Age","Career Games (W-D-L W%)") ]
   v_df3[[2]] =  v_df3[[2]][,c("Player","Age","Career Games (W-D-L W%)") ]
   v_df3 = rbindlist(v_df3)
   v_df3$Years = sapply(strsplit(as.character(v_df3$Age), '\\y'), '[', 1)
   v_df3$Days = sapply(strsplit(sapply(strsplit(as.character(v_df3$Age), 'd'), '[', 1), '\\y'), '[', 2)
   v_df3$PercentWon = sapply(strsplit(sapply(strsplit(as.character(v_df3$`Career Games (W-D-L W%)`), '\\ '), '[', 3), '\\%'), '[', 1)
   v_df3$Games = sapply(strsplit(as.character(v_df3$`Career Games (W-D-L W%)`), '\\ '), '[', 1)
   v_df3$Age_Years = round(as.numeric(v_df3$Years)+(as.numeric(v_df3$Days)/365),2)
   v_df4 = v_df3[,c("Player","Age_Years","Games","PercentWon")]
   home_data =  b_filter_temp2[i,c("ID","Year","Round","Away_team","Venue","Diff_Home")];names(home_data) = c("ID","Year","Round","Opponent","Venue","Diff")
   away_data =  b_filter_temp2[i,c("ID","Year","Round","Home_team","Venue","Diff_Away")];names(away_data) = c("ID","Year","Round","Opponent","Venue","Diff")
   v_df2[[1]] = left_join(v_df2[[1]],home_data)
   v_df2[[2]] = left_join(v_df2[[2]],away_data)
   v_df5 = rbindlist(v_df2)
   v_all = left_join(v_df5,v_df4);v_all[v_all == "?"] <- 0
   db_afl = "AFLData"
   db_afl_statement = paste("SELECT * FROM ",db_afl,sep = "")
   db_check_exist = try(dbGetQuery(con, db_afl_statement),silent = TRUE)
   if(class(db_check_exist) == "try-error"){
      dbWriteTable(con, db_afl, v_all,overwrite = T)
   }
   if(class(db_check_exist) != "try-error"){
      dbWriteTable(con, db_afl, v_all,append = T)
   }
}

afl %>%
   group_by(ID) %>%
   summarise(amt = sum(Diff)) %>%
   filter(amt != 0) 

write.delim(afl,'afl_17.txt',delim = "|")

col_change = 3:27
for(i in col_change) {
   player_performance[,i] = as.numeric(as.character(player_performance[,i]))
}

library(minerva)
res_mine = mine(df_exp)
sprintf("MIC: %s", res_mine$MIC[1, 2])
mine_ts = mine(df_time_series)



school_column <- cash[,1]
schools =  names(table(cash$PWC_COMPCODE))
school_indexes <- sapply(schools, function(x) which(school_column == x))
split_data <- lapply(school_indexes, function(x) cash[x,])
split_data_new <- lapply(school_indexes, function(x) cumsum(cash[x,"PWC_RNET"]))
append_df = rbindlist(split_data);append_cumsum = rbindlist(split_data_new)
append_df$cum_cash = append_cumsum


qtr_column <- append_df[,2]
quarter =  names(table(append_df$YEAR_QUARTER))
quarter_indexes <- sapply(quarter, function(x) which(qtr_column == x))
qtr_data <- lapply(quarter_indexes, function(x) append_df[x,])
qtr_results <- unlist(sapply(qtr_data, function(x) percent_rank(x$cum_cash)))
qtr_df = rbindlist(qtr_data);qtr_df$percentile = qtr_results
qtr_df$top_80 = ifelse(qtr_df$percentile >= 0.8,1,0)


library(sparklyr);library(dplyr)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_202') # For sendmailR
java_path <- normalizePath('C:/Program Files/Java/jdk1.8.0_202')
Sys.setenv(JAVA_HOME=java_path)
sc <- spark_connect(master = "local")


cluster_select_v2 = function(data,y, var_cols, scale_data = NULL,max_clusters = 15,zero_val = T, ...){
   library(skmeans);library(caroline)
   if(!is.null(scale_data)){   
      data = scale(data)
   }
   if(zero_val == T){
      cluster_other = data[apply(data[,var_cols], 1, function(x) var(x)) == 0,]
      cluster_all = data[apply(data[,var_cols], 1, function(x) var(x)) != 0,]  
      cluster_data = cluster_all[,-y]
   }
   cluster_all = data[apply(data[,var_cols], 1, function(x) var(x)) != 0,]  
   cluster_data = cluster_all[,-y]
   ##################### K MEANS ##########################################
   wss <- (nrow(cluster_data)-1)*sum(apply(cluster_data,2,var))
   for (i in 2:max_clusters) wss[i] <- sum(kmeans(cluster_data,centers=i)$betweenss/kmeans(cluster_data,centers=i)$totss)
   wss[1] = 0
   drop = rep(NA,length(wss) -1)
   drop[1] = 0.1
   for(i in 2:(length(wss)-1))   drop[i] <- round((wss[i+1] - wss[i])/wss[i],3)
   neg_drop = which(drop< 0.05)
   kmeans_cluster_selection = sprintf("The optimal number of cluster is %i for kmeans", neg_drop[1])
   Cluster_method = 'kmeans'
   Cluster_result <<- neg_drop[1]
   Cluster_results = data.frame(Cluster_method,Cluster_result)
   results_table = table(as.vector(Cluster_result))
   max_result = max(results_table)
   Optimal_clusters =  sprintf("The optimal number of clusters is %i across all models checked above", as.numeric(names(which(results_table == max_result))))
   Results = list(Cluster_results = Cluster_results, Optimal_clusters = Optimal_clusters)
   k_search = median(Cluster_result)
   mat_data = as.matrix(cluster_data)
   cluster_skmeans <<- skmeans(mat_data,k_search)
   if(zero_val == T) {
      cluster_var_results = data.frame(Employee_ID = cluster_all$EMPLOYEE_ID,Cluster =  as.vector(cluster_skmeans$cluster))
      last_cluster = max(cluster_var_results$Cluster)
      cluster_novar_results = data.frame(Employee_ID = cluster_other$EMPLOYEE_ID,Cluster =  rep(last_cluster+1, nrow(cluster_other)))
      cluster_all_results = rbind(cluster_var_results,cluster_novar_results)
   }
   cluster_all_results = data.frame(Employee_ID = cluster_all$EMPLOYEE_ID,Cluster =  as.vector(cluster_skmeans$cluster))
   write.delim(cluster_all_results,'cluster_results.txt',sep = "|")
}  


library(taskscheduleR)
myscript = system.file("extdata", "payer_stats.R", package = "taskscheduleR")
taskscheduler_create(taskname = "extracts", rscript = myscript,
                     schedule = "MINUTE") 
system.file("extdata", "payers_stats.log", package = "taskscheduleR")

mylog <- system.file("extdata", "payer_stats.log", package = "taskscheduleR")
cat(readLines(mylog), sep = "\n")  

sender <- "christopher.zucchet@gmail.com"
recipients <- c("chris.zucchet@pwc.com")
send.mail(from = sender,
          to = recipients,
          subject = "Subject of the email",
          body = "Body of the email",
          smtp = list(host.name = "smtp.gmail.com",  
                      user.name = "christopher.zucchet@gmail.com",            
                      passwd = "1036athl", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)


sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)
find.java <- function() {
   for (root in c("HLM", "HCU")) for (key in c("Software\\JavaSoft\\Java Runtime Environment", 
                                               "Software\\JavaSoft\\Java Development Kit")) {
      hive <- try(utils::readRegistry(key, root, 2), 
                  silent = TRUE)
      if (!inherits(hive, "try-error")) 
         return(hive)
   }
   hive
}

sender <- "christopher.zucchet@gmail.com" # Replace with a valid address
recipients <- c("chris.zucchet@pwc.com") # Replace with one or more valid addresses
email <- send.mail(from = sender,
                   to = recipients,
                   subject="Subject of the email",
                   body = "Body of the email",
                   smtp = list(host.name = "aspmx.l.google.com", port = 25),
                   authenticate = FALSE,
                   send = FALSE)



data %>%
   summarise_if( is.numeric, e1071::skewness )


iris %>% as_tibble() %>% mutate_if(is.factor, as.character)


require(corrplot) 

correlations = cor( select_if( data, is.numeric )  )  

corrplot(correlations, method = 'number', order = 'hclust')  


a = recipe(medv~., data = BostonHousing)

basic = recipe(data = BostonHousing,medv~.) %>%
   step_scale(all_numeric(),-chas) 
   prep(basic, training = BostonHousing)

bake(basic, BostonHousing) 


sum_old = BostonHousing %>%
   summarise_if( is.numeric, e1071::skewness ) %>%
   mutate( BostonHousing = 'untransformed') %>%
   select( BostonHousing, everything() )

bake(basic, newdata = BostonHousing)

cs_data = cleanest_data %>%
   filter(cleanest_data$state %in% c("failed","canceled","successful")) %>%
   mutate(camp_len_days =  as.numeric(round(difftime(cs_data$deadline,cs_data$launched),2))) %>%
   filter(camp_len_days <= 10000) %>%
   filter(goal <= 10000000)%>%
   select(-one_of(c("ID","name","pledged","usd.pledged","backers")))

rec_obj = recipe(state~., data = cs_data) %>%
   step_other(category,main_category,currency,threshold = 0.02) %>%
   step_other(country,threshold = 0.005) %>%
   step_dummy(country,currency)%>%
   step_date(launched)%>%
   #   step_num2factor(deadline_year)%>%
   step_dummy(launched_dow,launched_month)%>%
   step_log(goal)%>%
   step_center(camp_len_days,goal) %>%
   step_scale(camp_len_days,goal) %>%
   prep(data = cs_data)

x_train_tbl <- bake(rec_obj, newdata = cs_data) %>% select(-category,-main_category,-deadline,-launched,-state)
x_test_tbl  <- bake(rec_obj, newdata = cs_data) %>% select(-state) 
y_train_vec <- bake(rec_obj, newdata = cs_data) %>% select(state) 
y_test_vec  <- bake(rec_obj, newdata = cs_data) %>% select(state) 

cs_data %>%
   select(state, goal) %>%
   mutate(
      state = state %>% as.factor() %>% as.numeric(),
      LogCamp = log(goal)
   ) %>%
   correlate() %>%
   focus(state) %>%
   fashion()


xgb.data.train <- xgb.DMatrix(as.matrix(train[, colnames(train) != "Class"]), label = train$Class)

xgb.bench.acc = microbenchmark(
   xgb.model.acc <- xgb.train(data = xgb.data.train
                              , params = list(objective = "binary:logistic", eta = 0.1
                                              , max.depth = 7, min_child_weight = 100
                                              , subsample = 1, colsample_bytree = 1
                                              , nthread = 3, eval_metric = "auc"
                              )
                              , watchlist = list(test = xgb.data.test), nrounds = 500
                              , early_stopping_rounds = 40, print_every_n = 20
   )
   , times = 5L
)
print(xgb.bench.acc)
print(xgb.model.acc$bestScore)

#Get feature importance
xgb.feature.imp = xgb.importance(model = xgb.model.acc)

# Make predictions on test set for ROC curve
xgb.test.acc = predict(xgb.model.acc
                       , newdata = as.matrix(test[, colnames(test) != "Class"])
                       , ntreelimit = xgb.model.acc$bestInd)
auc.xgb.acc = roc(test$Class, xgb.test.acc, plot = TRUE, col = "blue")
print(auc.xgb.acc)  

rm(list=ls())


watchlist <- list(train=dtrain, test=dtest)
bstdiff <- xgb.train(data=dtrain, max.depth=5, 
                     booster = "gblinear", nthread = 5, 
                     nrounds = 1000,
                     early_stopping_rounds = 50,
                     gamma = 1,
                     watchlist=watchlist, objective = "binary:logistic", verbose = 2)
xgb.save(bstdiff, "model_xgboost_afl")



line_up = read_html(url) %>%
   html_nodes(xpath = '//*[@id="tteamlist"]')%>%
   html_nodes("div")%>%
   html_nodes("ul")%>%
   html_text() %>%
   str_replace_all("\n","") %>%
   str_replace_all("\r","") %>%
   str_trim() %>%
   as.data.frame()%>%
   rename("Teams" = ".")%>%
   mutate(Teams = as.character(Teams))%>%
   filter(str_detect(Teams, "Buy Tickets") != T)

# get all files ending in csv
files <- list.files(pattern = "csv$")
# read in data
dat <- lapply(files,function(x) read_txt(x, delim = "|"))
names(dat) <- gsub("\\.csv", "", files) # remove file extension

library(purrr)
dat2 <- map(files, ~read_delim(.x, delim = "|"))
names(dat2) <- gsub("\\.txt", "", files)



line_up = read_html(url) %>%
   html_nodes(xpath = '//*[@id="tteamlist"]')%>%
   html_nodes("div")%>%
   html_nodes("ul")%>%
   html_text() %>%
   str_replace_all("\n","") %>%
   str_replace_all("\r","") %>%
   str_trim() %>%
   as.data.frame()%>%
   rename("Teams" = ".")%>%
   mutate(Teams = as.character(Teams)) %>%
   filter(str_detect(Teams, "Buy Tickets") != T) 


player_numbers =  lapply(line_up,function(x) as.numeric(gsub("([0-9]+).*$", "\\1", x))) %>%
   map(~ na.omit(.x)) %>%
   map_df(~data.frame(.x)) %>%
   map(filter )


afl_list = line_up
a = afl_list %>%
   map(~tolower(.x))

afl_list =  lapply(afl_list,function(x) str_trim(x, side = "both")) %>% 
   keep(~length(.x) > 10) 

a =  lapply(line_up,function(x) as.numeric(gsub("([0-9]+).*$", "\\1", x))) %>%
   keep(~!is.na(.x))

by_cyl %>%  map(~1:nrow(.x["mpg"]))   

a$x[is.na(a$x)] <- 0


# 2.2 Network architechture example on imdb data - 2 class problem
library(keras)
install_keras()

#imdb example - #binary classification problem
imdb <- dataset_imdb(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb

# Stage 1: Data prep to allow for algo to run. 
vectorize_sequences <- function(sequences, dimension = 10000) {
   results <- matrix(0, nrow = length(sequences), ncol = dimension)
   for (i in 1:length(sequences))
      results[i, sequences[[i]]] <- 1
   results
   }
x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)
# Need to convert results to numeric
y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)
# Next: Make call on architechture of deep learning model
model <- keras_model_sequential() %>%
   layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
   layer_dense(units = 16, activation = "relu") %>%
   layer_dense(units = 1, activation = "sigmoid")
# Next: Select an optimiser and a loss function
model %>% compile(
   optimizer = "rmsprop",
   loss = "binary_crossentropy",
   metrics = c("accuracy")
)
#Next: Set aside a validation set
val_indices <- 1:10000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]
#Next: Train model
model %>% compile(
   optimizer = "rmsprop",
   loss = "binary_crossentropy",
   metrics = c("accuracy")
)
history <- model %>% fit(
   partial_x_train,
   partial_y_train,
   epochs = 10,
   batch_size = 512,
   validation_data = list(x_val, y_val)
)
str(history)
#Next: Review results to note where overfitting is taking place - in this case, at the 4th epoch, the results have not gotten better.
#As such, retrain model with updated epochs
model <- keras_model_sequential() %>%
   layer_dense(units = 32, activation = "relu", input_shape = c(10000)) %>%
   layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
   optimizer = "rmsprop",
   loss = "binary_crossentropy",
   metrics = c("accuracy")
)
model %>% fit(x_train, y_train, epochs = 3, batch_size = 512)
results <- model %>% evaluate(x_test, y_test)
#Results show about 88%. With stronger methods, this can go up to 95%.
#Next make prediction on model
model %>% predict(x_test[1:10,])

#Rueters dataset example - Multiclass problem
reuters <- dataset_reuters(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% reuters
x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)
#Do: One hot encoding
one_hot_train_labels <- to_categorical(train_labels)
one_hot_test_labels <- to_categorical(test_labels)
# Model definition. As number of classes is 46, going with units when defining the model won't cover enough of the problem, leading to information loss
#in layers being trained later on. As such, set units to be equal to 64. If you were to use 16 units, perfomance would drop by 8%
model <- keras_model_sequential() %>%
   layer_dense(units = 64, activation = "relu", input_shape = c(10000)) %>%
   layer_dense(units = 64, activation = "relu") %>%
   layer_dense(units = 46, activation = "softmax")
#Change in loss function - this time using categorical cross entropy. Rest is model creation
model %>% compile(
   optimizer = "rmsprop",
   loss = "categorical_crossentropy",
   metrics = c("accuracy")
   )
val_indices <- 1:1000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]
y_val <- one_hot_train_labels[val_indices,]
partial_y_train = one_hot_train_labels[-val_indices,]
history <- model %>% fit(
   partial_x_train,
   partial_y_train,
   epochs = 20,
   batch_size = 512,
   validation_data = list(x_val, y_val)
)


model <- keras_model_sequential() %>%
   layer_dense(units = 1024, activation = "relu", input_shape = c(10000)) %>%
   layer_dense(units = 46, activation = "softmax")
model %>% compile(
   optimizer = "rmsprop",
   loss = "categorical_crossentropy",
   metrics = c("accuracy")
)
history <- model %>% fit(
   partial_x_train,
   partial_y_train,
   epochs = 12,
   batch_size = 512,
   validation_data = list(x_val, y_val)
)
results <- model %>% evaluate(x_test, one_hot_test_labels)

predictions <- model %>% predict(x_test)

# Regression problem - Boston housing
library(keras)
dataset <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset
# Do feature wise normalisation
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)
build_model <- function() {
   model <- keras_model_sequential() %>%
      layer_dense(units = 64, activation = "relu",
                  input_shape = dim(train_data)[[2]]) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 1)
   model %>% compile(
      optimizer = "rmsprop",
      loss = "mse",
      metrics = c("mae")
   )
}
# Dataset too small to rely on creating and using a training and validation set - use k fold cross validation instead
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 100
all_scores <- c()
for (i in 1:k) {
   cat("processing fold #", i, "\n")
   val_indices <- which(folds == i, arr.ind = TRUE)
   val_data <- train_data[val_indices,]
   val_targets <- train_targets[val_indices]
   partial_train_data <- train_data[-val_indices,]
   partial_train_targets <- train_targets[-val_indices]
   model <- build_model()
   model %>% fit(partial_train_data, partial_train_targets,
                 epochs = num_epochs, batch_size = 1, verbose = 0)
   results <- model %>% evaluate(val_data, val_targets, verbose = 0)
   all_scores <- c(all_scores, results$mean_absolute_error)
}

num_epochs <- 500
all_mae_histories <- NULL
for (i in 1:k) {
   cat("processing fold #", i, "\n")
   val_indices <- which(folds == i, arr.ind = TRUE)
   val_data <- train_data[val_indices,]
   val_targets <- train_targets[val_indices]
   partial_train_data <- train_data[-val_indices,]
   partial_train_targets <- train_targets[-val_indices]
   model <- build_model()
   history <- model %>% fit(
      partial_train_data, partial_train_targets,
      validation_data = list(val_data, val_targets),
      epochs = num_epochs, batch_size = 1, verbose = 0
   )
   mae_history <- history$metrics$val_mean_absolute_error
   all_mae_histories <- rbind(all_mae_histories, mae_history)
}
average_mae_history <- data.frame(
   epoch = seq(1:ncol(all_mae_histories)),
   validation_mae = apply(all_mae_histories, 2, mean)
)
model <- build_model()
model %>% fit(train_data, train_targets,
              epochs = 80, batch_size = 16, verbose = 0)
result <- model %>% evaluate(test_data, test_targets)


#covnet example

library(keras)
model <- keras_model_sequential() %>%
   layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                 input_shape = c(28, 28, 1)) %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")
model <- model %>%
   layer_flatten() %>%
   layer_dense(units = 64, activation = "relu") %>%
   layer_dense(units = 10, activation = "softmax")

mnist <- dataset_mnist()
c(c(train_images, train_labels), c(test_images, test_labels)) %<-% mnist
train_images <- array_reshape(train_images, c(60000, 28, 28, 1))
train_images <- train_images / 255
test_images <- array_reshape(test_images, c(10000, 28, 28, 1))
test_images <- test_images / 255
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)
model %>% compile(
   optimizer = "rmsprop",
   loss = "categorical_crossentropy",
   metrics = c("accuracy")
)
model %>% fit(
   train_images, train_labels,
   epochs = 5, batch_size=64
)
results <- model %>% evaluate(test_images, test_labels)
results

#Dogs vs Cats classification
original_dataset_dir <- "C:/Users/ChrisZ/Downloads/train"
base_dir <- "C:/Users/ChrisZ/Downloads/cats_and_dogs_small"
dir.create(base_dir)
train_dir <- file.path(base_dir, "train")
dir.create(train_dir)
validation_dir <- file.path(base_dir, "validation")
dir.create(validation_dir)
test_dir <- file.path(base_dir, "test")
dir.create(test_dir)
train_cats_dir <- file.path(train_dir, "cats")
dir.create(train_cats_dir)
train_dogs_dir <- file.path(train_dir, "dogs")
dir.create(train_dogs_dir)
validation_cats_dir <- file.path(validation_dir, "cats")
dir.create(validation_cats_dir)
validation_dogs_dir <- file.path(validation_dir, "dogs")
dir.create(validation_dogs_dir)
test_cats_dir <- file.path(test_dir, "cats")
dir.create(test_cats_dir)
test_dogs_dir <- file.path(test_dir, "dogs")
dir.create(test_dogs_dir)
fnames <- paste0("cat.", 1001:1500, ".jpg")
fnames <- paste0("cat.", 1:1000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_cats_dir))

file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_cats_dir))
fnames <- paste0("cat.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_cats_dir))
fnames <- paste0("dog.", 1:1000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_dogs_dir))
fnames <- paste0("dog.", 1001:1500, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_dogs_dir))
fnames <- paste0("dog.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_dogs_dir))

library(keras)
model <- keras_model_sequential() %>%
   layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                 input_shape = c(150, 150, 3)) %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_flatten() %>%
   layer_dense(units = 512, activation = "relu") %>%
   layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
   loss = "binary_crossentropy",
   optimizer = optimizer_rmsprop(lr = 1e-4),
   metrics = c("acc")
)


train_datagen <- image_data_generator(rescale = 1/255)
validation_datagen <- image_data_generator(rescale = 1/255)
train_generator <- flow_images_from_directory(
   train_dir,
   train_datagen,
   target_size = c(150, 150),
   batch_size = 20,
   class_mode = "binary"
)

validation_generator <- flow_images_from_directory(
   validation_dir,
   validation_datagen,
   target_size = c(150, 150),
   batch_size = 20,
   class_mode = "binary"
   )

batch <- generator_next(train_generator)

history <- model %>% fit_generator(
   train_generator,
   steps_per_epoch = 100,
   epochs = 30,
   validation_data = validation_generator,
   validation_steps = 50
   )

datagen <- image_data_generator(
   rescale = 1/255,
   rotation_range = 40,
   width_shift_range = 0.2,
   height_shift_range = 0.2,
   shear_range = 0.2,
   zoom_range = 0.2,
   horizontal_flip = TRUE,
   fill_mode = "nearest"
   )

model <- keras_model_sequential() %>%
   layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                 input_shape = c(150, 150, 3)) %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
   layer_flatten() %>%
   layer_dropout(rate = 0.5) %>%
   layer_dense(units = 512, activation = "relu") %>%
   layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
   loss = "binary_crossentropy",
   optimizer = optimizer_rmsprop(lr = 1e-4),
   metrics = c("acc")
)
datagen <- image_data_generator(
   rescale = 1/255,
   rotation_range = 40,
   width_shift_range = 0.2,
   height_shift_range = 0.2,
   shear_range = 0.2,
   zoom_range = 0.2,
   horizontal_flip = TRUE
   )

test_datagen <-
   image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
   train_dir,
   datagen,
   target_size = c(150, 150),
   batch_size = 32,
   class_mode = "binary"
)
validation_generator <- flow_images_from_directory(
   validation_dir,
   test_datagen,
   target_size = c(150, 150),
   batch_size = 32,
   class_mode = "binary"
)
history <- model %>% fit_generator(
   train_generator,
   steps_per_epoch = 100,
   epochs = 100,
   validation_data = validation_generator,
   validation_steps = 50
)
model %>% save_model_hdf5("cats_and_dogs_small_2.h5")

conv_base <- application_vgg16(
   weights = "imagenet",
   include_top = FALSE,
   input_shape = c(150, 150, 3)
)

base_dir <- "C:/Users/ChrisZ/Downloads/cats_and_dogs_small"
train_dir <- file.path(base_dir, "train")
validation_dir <- file.path(base_dir, "validation")
test_dir <- file.path(base_dir, "test")

datagen <- image_data_generator(rescale = 1/255)
batch_size <- 20
extract_features <- function(directory, sample_count) {
   features <- array(0, dim = c(sample_count, 4, 4, 512))
   labels <- array(0, dim = c(sample_count))
   generator <- flow_images_from_directory(
      directory = directory,
      generator = datagen,
      target_size = c(150, 150),
      batch_size = batch_size,
      class_mode = "binary"
   )
   i <- 0
   while(TRUE) {
      batch <- generator_next(generator)
      inputs_batch <- batch[[1]]
      labels_batch <- batch[[2]]
      features_batch <- conv_base %>% predict(inputs_batch)
      index_range <- ((i * batch_size)+1):((i + 1) * batch_size)
      features[index_range,,,] <- features_batch
      labels[index_range] <- labels_batch
      i <- i + 1
      if (i * batch_size >= sample_count)
         break
   }
   list(
      features = features,
      labels = labels
   )
}
train <- extract_features(train_dir, 2000)
validation <- extract_features(validation_dir, 1000)
test <- extract_features(test_dir, 1000)

reshape_features <- function(features) {
   array_reshape(features, dim = c(nrow(features), 4 * 4 * 512))
   }
train$features <- reshape_features(train$features)
validation$features <- reshape_features(validation$features)
test$features <- reshape_features(test$features)

model <- keras_model_sequential() %>%
   layer_dense(units = 256, activation = "relu",
               input_shape = 4 * 4 * 512) %>%
   layer_dropout(rate = 0.5) %>%
   layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
   optimizer = optimizer_rmsprop(lr = 2e-5),
   loss = "binary_crossentropy",
   metrics = c("accuracy")
)
history <- model %>% fit(
   train$features, train$labels,
   epochs = 30,
   batch_size = 20,
   validation_data = list(validation$features, validation$labels)
)


library(keras)
model = load_model_hdf5("cats_and_dogs_small_2.h5")

samples <- c("The cat sat on the mat.", "The dog ate my homework.")
token_index <- list()
for (sample in samples)
   for (word in strsplit(sample, " ")[[1]])
      if (!word %in% names(token_index))
         token_index[[word]] <- length(token_index) + 2
max_length <- 10
results <- array(0, dim = c(length(samples),
                            max_length,
                            max(as.integer(token_index))))
for (i in 1:length(samples)) {
   sample <- samples[[i]]
   words <- head(strsplit(sample, " ")[[1]], n = max_length)
   for (j in 1:length(words)) {
      index <- token_index[[words[[j]]]]
      results[[i, j, index]] <- 1
   }
}

samples <- c("The cat sat on the mat.", "The dog ate my homework.")
ascii_tokens <- c("", sapply(as.raw(c(32:126)), rawToChar))
token_index <- c(1:(length(ascii_tokens)))
names(token_index) <- ascii_tokens
max_length <- 50
results <- array(0, dim = c(length(samples), max_length, length(token_index)))
for (i in 1:length(samples)) {
   sample <- samples[[i]]
   characters <- strsplit(sample, "")[[1]]
   for (j in 1:length(characters)) {
      character <- characters[[j]]
      results[i, j, token_index[[character]]] <- 1
   }
}


library(keras)
samples <- c("The cat sat on the mat.", "The dog ate my homework.")
tokenizer <- text_tokenizer(num_words = 1000) %>%
   fit_text_tokenizer(samples)

tokenizer <- text_tokenizer(num_words = 1000) %>%
   fit_text_tokenizer(samples)
sequences <- texts_to_sequences(tokenizer, samples)
one_hot_results <- texts_to_matrix(tokenizer, samples, mode = "binary")
word_index <- tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")


library(hashFunction)
samples <- c("The cat sat on the mat.", "The dog ate my homework.")
dimensionality <- 1000
max_length <- 10
results <- array(0, dim = c(length(samples), max_length, dimensionality))
for (i in 1:length(samples)) {
   sample <- samples[[i]]
   words <- head(strsplit(sample, " ")[[1]], n = max_length)
   for (j in 1:length(words)) {
      index <- abs(spooky.32(words[[i]])) %% dimensionality
      results[[i, j, index]] <- 1
   }
}

max_features <- 10000
maxlen <- 20
imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb
x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test <- pad_sequences(x_test, maxlen = maxlen)


model <- keras_model_sequential() %>%
   layer_embedding(input_dim = 10000, output_dim = 8,
                   input_length = maxlen) %>%
   layer_flatten() %>%
   layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
   optimizer = "rmsprop",
   loss = "binary_crossentropy",
   metrics = c("acc")
)
summary(model)
history <- model %>% fit(
   x_train, y_train,
   epochs = 10,
   batch_size = 32,
   validation_split = 0.2
)

imdb_dir <- "C:/Users/ChrisZ/Downloads/aclImdb"
train_dir <- file.path(imdb_dir, "test")
labels <- c()
texts <- c()
for (label_type in c("neg", "pos")) {
   label <- switch(label_type, neg = 0, pos = 1)
   dir_name <- file.path(train_dir, label_type)
   for (fname in list.files(dir_name, pattern = glob2rx("*.txt"),
                            full.names = TRUE)) {
      texts <- c(texts, readChar(fname, file.info(fname)$size))
      labels <- c(labels, label)
      }
   }

library(keras)
maxlen <- 100
training_samples <- 200
validation_samples <- 10000
max_words <- 10000
tokenizer <- text_tokenizer(num_words = max_words) %>%
   fit_text_tokenizer(texts)

sequences <- texts_to_sequences(tokenizer, texts)
word_index = tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")
data <- pad_sequences(sequences, maxlen = maxlen)
labels <- as.array(labels)
cat("Shape of data tensor:", dim(data), "\n")
cat('Shape of label tensor:', dim(labels), "\n")
indices <- sample(1:nrow(data))
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1):
                                 (training_samples + validation_samples)]
x_train <- data[training_indices,]
y_train <- labels[training_indices]
x_val <- data[validation_indices,]
y_val <- labels[validation_indices]

glove_dir = "C:/Users/ChrisZ/Downloads/glove.6B"
lines <- readLines(file.path(glove_dir, "glove.6B.100d.txt"))
embeddings_index <- new.env(hash = TRUE, parent = emptyenv())
for (i in 1:length(lines)) {
   line <- lines[[i]]
   values <- strsplit(line, " ")[[1]]
   word <- values[[1]]
   embeddings_index[[word]] <- as.double(values[-1])
}
cat("Found", length(embeddings_index), "word vectors.\n")



model <- keras_model_sequential() %>%
   layer_embedding(input_dim = max_features, output_dim = 32) %>%
   layer_lstm(units = 32) %>%
   layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
   optimizer = "rmsprop",
   loss = "binary_crossentropy",
   metrics = c("acc")
   )
history <- model %>% fit(
   input_train, y_train,
   epochs = 10,
   batch_size = 128,
   validation_split = 0.2
   )

dir.create("C:/Users/ChrisZ/Downloads/jena_climate", recursive = TRUE)
download.file(
   "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
   "C:/Users/ChrisZ/Downloads/jena_climate/jena_climate_2009_2016.csv.zip"
   )
unzip(
   "C:/Users/ChrisZ/Downloads/jena_climate/jena_climate_2009_2016.csv.zip",
   exdir = "C:/Users/ChrisZ/Downloads/jena_climate"
   )
library(tibble)
library(readr)
data_dir <- "C:/Users/ChrisZ/Downloads/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)
glimpse(data)

sequence_generator <- function(start) {
   value <- start - 1
   function() {
      value <<- value + 1
      value
   }
}

data <- data.matrix(data[,-1])

train_data <- data[1:200000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
   if (is.null(max_index)) max_index <- nrow(data) - delay - 1
   i <- min_index + lookback
   function() {
      if (shuffle) {
         rows <- sample(c((min_index+lookback):max_index), size = batch_size)
      } else {
         if (i + batch_size >= max_index)
            i <<- min_index + lookback
         rows <- c(i:min(i+batch_size, max_index))
         i <<- i + length(rows)
      }
      samples <- array(0, dim = c(length(rows),
                                  lookback / step,
                                  dim(data)[[-1]]))
      targets <- array(0, dim = c(length(rows)))
      for (j in 1:length(rows)) {
         indices <- seq(rows[[j]] - lookback, rows[[j]],
                        length.out = dim(samples)[[2]])
         samples[j,,] <- data[indices,]
         targets[[j]] <- data[rows[[j]] + delay,2]
      }
      list(samples, targets)
   }
}
library(keras)
lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128


train_gen <- generator(
   data,
   lookback = lookback,
   delay = delay,
   min_index = 1,
   max_index = 200000,
   shuffle = TRUE,
   step = step,
   batch_size = batch_size
)

val_gen = generator(
   data,
   lookback = lookback,
   delay = delay,
   min_index = 200001,
   max_index = 300000,
   step = step,
   batch_size = batch_size
)

test_gen <- generator(
   data,
   lookback = lookback,
   delay = delay,
   min_index = 300001,
   max_index = NULL,
   step = step,
   batch_size = batch_size
)

val_steps <- (300000 - 200001 - lookback) / batch_size
test_steps <- (nrow(data) - 300001 - lookback) / batch_size

mean(abs(preds - targets))


evaluate_naive_method <- function() {
   batch_maes <- c()
   for (step in 1:val_steps) {
      c(samples, targets) %<-% val_gen()
      preds <- samples[,dim(samples)[[2]],2]
      mae <- mean(abs(preds - targets))
      batch_maes <- c(batch_maes, mae)
   }
   print(mean(batch_maes))
}
evaluate_naive_method()
celsius_mae <- 0.29 * std[[2]]


library(keras)
model <- keras_model_sequential() %>%
   layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>%
   layer_dense(units = 32, activation = "relu") %>%
   layer_dense(units = 1)
model %>% compile(
   optimizer = optimizer_rmsprop(),
   loss = "mae"
)
history <- model %>% fit_generator(
   train_gen,
   steps_per_epoch = 500,
   epochs = 20,
   validation_data = val_gen,
   validation_steps = val_steps
)

model <- keras_model_sequential() %>%
   layer_gru(units = 32, input_shape = list(NULL, dim(data)[[-1]])) %>%
   layer_dense(units = 1)
model %>% compile(
   optimizer = optimizer_rmsprop(),
   loss = "mae"
)
history <- model %>% fit_generator(
   train_gen,
   steps_per_epoch = 500,
   epochs = 10,
   validation_data = val_gen,
   validation_steps = val_steps
)


model <- keras_model_sequential() %>%
   layer_gru(units = 32, dropout = 0.2, recurrent_dropout = 0.2,
             input_shape = list(NULL, dim(data)[[-1]])) %>%
   layer_dense(units = 1)
model %>% compile(
   optimizer = optimizer_rmsprop(),
   loss = "mae"
)
history <- model %>% fit_generator(
   train_gen,
   steps_per_epoch = 500,
   epochs = 40,
   validation_data = val_gen,
   validation_steps = val_steps
)

library(keras)
max_features <- 10000
maxlen <- 500
imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb
x_train <- lapply(x_train, rev)
x_test <- lapply(x_test, rev)
x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test <- pad_sequences(x_test, maxlen = maxlen)
model <- keras_model_sequential() %>%
   layer_embedding(input_dim = max_features, output_dim = 128) %>%
   layer_lstm(units = 32) %>%
   layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
   optimizer = "rmsprop",
   loss = "binary_crossentropy",
   metrics = c("acc")
)
history <- model %>% fit(
   x_train, y_train,
   epochs = 10,
   batch_size = 128,
   validation_split = 0.2
)

model <- keras_model_sequential() %>%
   layer_embedding(input_dim = max_features, output_dim = 32) %>%
   bidirectional(
      layer_lstm(units = 32)
   ) %>%
   layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
   optimizer = "rmsprop",
   loss = "binary_crossentropy",
   metrics = c("acc")
)
history <- model %>% fit(
   x_train, y_train,
   epochs = 10,
   batch_size = 128,
   validation_split = 0.2
)

model <- keras_model_sequential() %>%
   bidirectional(
      layer_gru(units = 32), input_shape = list(NULL, dim(data)[[-1]])
   ) %>%
   layer_dense(units = 1)
model %>% compile(
   optimizer = optimizer_rmsprop(),
   loss = "mae"
)
history <- model %>% fit_generator(
   train_gen,
   steps_per_epoch = 500,
   epochs = 40,
   validation_data = val_gen,
   validation_steps = val_steps
)


model <- keras_model_sequential() %>%
   layer_embedding(input_dim = max_features, output_dim = 128,
                   input_length = max_len) %>%
   layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>%
   layer_max_pooling_1d(pool_size = 5) %>%
   layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>%
   layer_global_max_pooling_1d() %>%
   layer_dense(units = 1)
summary(model)
model %>% compile(
   optimizer = optimizer_rmsprop(lr = 1e-4),
   loss = "binary_crossentropy",
   metrics = c("acc")
   )
history <- model %>% fit(
   x_train, y_train,
   epochs = 10,
   batch_size = 128,
   validation_split = 0.2
   )


model <- keras_model_sequential() %>%
   layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu",
                 input_shape = list(NULL, dim(data)[[-1]])) %>%
   layer_max_pooling_1d(pool_size = 3) %>%
   layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
   layer_max_pooling_1d(pool_size = 3) %>%
   layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
   layer_global_max_pooling_1d() %>%
   layer_dense(units = 1)
model %>% compile(
   optimizer = optimizer_rmsprop(),
   loss = "mae"
)
history <- model %>% fit_generator(
   train_gen,
   steps_per_epoch = 500,
   epochs = 20,
   validation_data = val_gen,
   validation_steps = val_steps
)

model <- keras_model_sequential() %>%
   layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu",
                 input_shape = list(NULL, dim(data)[[-1]])) %>%
   layer_max_pooling_1d(pool_size = 3) %>%
   layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
   layer_gru(units = 32, dropout = 0.1, recurrent_dropout = 0.5) %>%
   layer_dense(units = 1)
summary(model)
model %>% compile(
   optimizer = optimizer_rmsprop(),
   loss = "mae"
)
history <- model %>% fit_generator(
   train_gen,
   steps_per_epoch = 500,
   epochs = 20,
   validation_data = val_gen,
   validation_steps = val_steps
)

library(keras)
text_vocabulary_size <- 10000
ques_vocabulary_size <- 10000
answer_vocabulary_size <- 500
text_input <- layer_input(shape = list(NULL),
                          dtype = "int32", name = "text")
encoded_text <- text_input %>%
   layer_embedding(input_dim = 64, output_dim = text_vocabulary_size) %>%
   layer_lstm(units = 32)
question_input <- layer_input(shape = list(NULL),
                              dtype = "int32", name = "question")
encoded_question <- question_input %>%
   layer_embedding(input_dim = 32, output_dim = ques_vocabulary_size) %>%
   layer_lstm(units = 16)
concatenated <- layer_concatenate(list(encoded_text, encoded_question))

answer <- concatenated %>%
   layer_dense(units = answer_vocabulary_size, activation = "softmax")
model <- keras_model(list(text_input, question_input), answer)
model %>% compile(
   optimizer = "rmsprop",
   loss = "categorical_crossentropy",
   metrics = c("acc")
)


num_samples <- 1000
max_length <- 100
random_matrix <- function(range, nrow, ncol) {
   matrix(sample(range, size = nrow * ncol, replace = TRUE),
          nrow = nrow, ncol = ncol)
}
text <- random_matrix(1:text_vocabulary_size, num_samples, max_length)
question <- random_matrix(1:ques_vocabulary_size, num_samples, max_length)
answers <- random_matrix(0:1, num_samples, answer_vocabulary_size)
model %>% fit(
   list(text, question), answers,
   epochs = 10, batch_size = 128
)
model %>% fit(
   list(text = text, question = question), answers,
   epochs = 10, batch_size = 128
)

library(keras)
seq_model <- keras_model_sequential() %>%
   layer_dense(units = 32, activation = "relu", input_shape = c(64)) %>%
   layer_dense(units = 32, activation = "relu") %>%
   layer_dense(units = 10, activation = "softmax")
input_tensor <- layer_input(shape = c(64))
output_tensor <- input_tensor %>%
   layer_dense(units = 32, activation = "relu") %>%
   layer_dense(units = 32, activation = "relu") %>%
   layer_dense(units = 10, activation = "softmax")
model <- keras_model(input_tensor, output_tensor)

model %>% compile(
   optimizer = "rmsprop",
   loss = "categorical_crossentropy"
   )


library(keras)
text_vocabulary_size <- 10000
ques_vocabulary_size <- 10000
answer_vocabulary_size <- 500
text_input <- layer_input(shape = list(NULL),
                          dtype = "int32", name = "text")
encoded_text <- text_input %>%
   layer_embedding(input_dim = 64, output_dim = text_vocabulary_size) %>%
   layer_lstm(units = 32)
question_input <- layer_input(shape = list(NULL),
                              dtype = "int32", name = "question")
encoded_question <- question_input %>%
   layer_embedding(input_dim = 32, output_dim = ques_vocabulary_size) %>%
   layer_lstm(units = 16)
concatenated <- layer_concatenate(list(encoded_text, encoded_question))
answer <- concatenated %>%
   layer_dense(units = answer_vocabulary_size, activation = "softmax")
model <- keras_model(list(text_input, question_input), answer)
model %>% compile(
   optimizer = "rmsprop",
   loss = "categorical_crossentropy",
   metrics = c("acc")
   )


library(keras)
text_vocabulary_size <- 10000
ques_vocabulary_size <- 10000
answer_vocabulary_size <- 500
text_input <- layer_input(shape = list(NULL),
                          dtype = "int32", name = "text")
encoded_text <- text_input %>%
   layer_embedding(input_dim = 64, output_dim = text_vocabulary_size) %>%
   layer_lstm(units = 32)
question_input <- layer_input(shape = list(NULL),
                              dtype = "int32", name = "question")
encoded_question <- question_input %>%
   layer_embedding(input_dim = 32, output_dim = ques_vocabulary_size) %>%
   layer_lstm(units = 16)
concatenated <- layer_concatenate(list(encoded_text, encoded_question))

answer <- concatenated %>%
   layer_dense(units = answer_vocabulary_size, activation = "softmax")
model <- keras_model(list(text_input, question_input), answer)
model %>% compile(
   optimizer = "rmsprop",
   loss = "categorical_crossentropy",
   metrics = c("acc")
   )

num_samples <- 1000
max_length <- 100
random_matrix <- function(range, nrow, ncol) {
   matrix(sample(range, size = nrow * ncol, replace = TRUE),
          nrow = nrow, ncol = ncol)
}
text <- random_matrix(1:text_vocabulary_size, num_samples, max_length)
question <- random_matrix(1:ques_vocabulary_size, num_samples, max_length)
answers <- random_matrix(0:1, num_samples, answer_vocabulary_size)
model %>% fit(
   list(text, question), answers,
   epochs = 10, batch_size = 128
)
model %>% fit(
   list(text = text, question = question), answers,
   epochs = 10, batch_size = 128
)


library(keras)
vocabulary_size <- 50000
num_income_groups <- 10
posts_input <- layer_input(shape = list(NULL),
                           dtype = "int32", name = "posts")
embedded_posts <- posts_input %>%
   layer_embedding(input_dim = 256, output_dim = vocabulary_size)
base_model <- embedded_posts %>%
   layer_conv_1d(filters = 128, kernel_size = 5, activation = "relu") %>%
   layer_max_pooling_1d(pool_size = 5) %>%
   layer_conv_1d(filters = 256, kernel_size = 5, activation = "relu") %>%
   layer_conv_1d(filters = 256, kernel_size = 5, activation = "relu") %>%
   layer_max_pooling_1d(pool_size = 5) %>%
   layer_conv_1d(filters = 256, kernel_size = 5, activation = "relu") %>%
   layer_conv_1d(filters = 256, kernel_size = 5, activation = "relu") %>%
   layer_global_max_pooling_1d() %>%
   layer_dense(units = 128, activation = "relu")

age_prediction <- base_model %>%
   layer_dense(units = 1, name = "age")
income_prediction <- base_model %>%
   layer_dense(num_income_groups, activation = "softmax", name = "income")
gender_prediction <- base_model %>%
   layer_dense(units = 1, activation = "sigmoid", name = "gender")
model <- keras_model(
   posts_input,
   list(age_prediction, income_prediction, gender_prediction)
   )
model %>% compile(
   optimizer = "rmsprop",
   loss = c("mse", "categorical_crossentropy", "binary_crossentropy")
)
model %>% compile(
   optimizer = "rmsprop",
   loss = list(
      age = "mse",
      income = "categorical_crossentropy",
      gender = "binary_crossentropy"
   )
)


model %>% compile(
   optimizer = "rmsprop",
   loss = c("mse", "categorical_crossentropy", "binary_crossentropy"),
   loss_weights = c(0.25, 1, 10)
)
model %>% compile(
   optimizer = "rmsprop",
   loss = list(
      age = "mse",
      income = "categorical_crossentropy",
      gender = "binary_crossentropy"
   ),
   loss_weights = list(
      age = 0.25,
      income = 1,
      gender = 10
   )
)

model %>% fit(
   posts, list(
      age = age_targets,
      income = income_targets,
      gender = gender_targets
   ),
   epochs = 10, batch_size = 64
)


library(keras)
branch_a <- input %>%
   layer_conv_2d(filters = 128, kernel_size = 1,
                 activation = "relu", strides = 2)
branch_b <- input %>%
   layer_conv_2d(filters = 128, kernel_size = 1,
                 activation = "relu") %>%
   layer_conv_2d(filters = 128, kernel_size = 3,
                 activation = "relu", strides = 2)
branch_c <- input %>%
   layer_average_pooling_2d(pool_size = 3, strides = 2) %>%
   layer_conv_2d(filters = 128, kernel_size = 3,
                 activation = "relu")
branch_d <- input %>%
   layer_conv_2d(filters = 128, kernel_size = 1,
                 activation = "relu") %>%
   layer_conv_2d(filters = 128, kernel_size = 3,
                 activation = "relu") %>%
   layer_conv_2d(filters = 128, kernel_size = 3,
                 activation = "relu", strides = 2)
output <- layer_concatenate(list(
   branch_a, branch_b, branch_c, branch_d
   ))

output <- input %>%
   layer_conv_2d(filters = 128, kernel_size = 3,
                 activation = "relu", padding = "same") %>%
   layer_conv_2d(filters = 128, kernel_size = 3,
                 activation = "relu", padding = "same") %>%
   layer_conv_2d(filters = 128, kernel_size = 3,
                 activation = "relu", padding = "same")
output <- layer_add(list(output, input))
output <- input %>%
   layer_conv_2d(filters = 128, kernel_size = 3,
                 activation = "relu", padding = "same") %>%
   layer_conv_2d(filters = 128, kernel_size = 3,
                 activation = "relu", padding = "same") %>%
   layer_max_pooling_2d(pool_size = 2, strides = 2)
residual <- input %>%
   layer_conv_2d(filters = 128, kernel_size = 1,
                 strides = 2, padding = "same")
output <- layer_add(list(output, residual))

library(keras)
xception_base <- application_xception(weights = NULL,
                                      include_top = FALSE)
left_input <- layer_input(shape = c(250, 250, 3))
right_input <- layer_input(shape = c(250, 250, 3))
left_features = left_input %>% xception_base()
right_features <- right_input %>% xception_base()
merged_features <- layer_concatenate(
   list(left_features, right_features)
   )
   

library(keras)
callbacks_list <- list(
   callback_early_stopping(
      monitor = "acc",
      patience = 1
   ),
   callback_model_checkpoint(
      filepath = "my_model.h5",
      monitor = "val_loss",
      save_best_only = TRUE
   )
)
model %>% compile(
   optimizer = "rmsprop",
   loss = "binary_crossentropy",
   metrics = c("acc")
)
model %>% fit(
   x, y,
   epochs = 10,
   batch_size = 32,
   callbacks = callbacks_list,
   validation_data = list(x_val, y_val)
   )   
   


library(keras)
max_features <- 2000
max_len <- 500
imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb
x_train <- pad_sequences(x_train, maxlen = max_len)
x_test = pad_sequences(x_test, maxlen = max_len)

model <- keras_model_sequential() %>%
   layer_embedding(input_dim = max_features, output_dim = 128,
                   input_length = max_len, name = "embed") %>%
   layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>%
   layer_max_pooling_1d(pool_size = 5) %>%
   layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>%
   layer_global_max_pooling_1d() %>%
   layer_dense(units = 1)
summary(model)
model %>% compile(
   optimizer = "rmsprop",
   loss = "binary_crossentropy",
   metrics = c("acc")
)

dir.create("my_log_dir")

tensorboard("my_log_dir")
callbacks = list(
   callback_tensorboard(
      log_dir = "my_log_dir",
      histogram_freq = 1,
      embeddings_freq = 1,
   )
)
history <- model %>% fit(
   x_train, y_train,
   epochs = 20,
   batch_size = 128,
   validation_split = 0.2,
   callbacks = callbacks
)
   
library(keras)
library(stringr)
path <- get_file(
   "nietzsche.txt",
   origin = "https://s3.amazonaws.com/text-datasets/nietzsche.txt"
)
text <- tolower(readChar(path, file.info(path)$size))
cat("Corpus length:", nchar(text), "\n")


maxlen <- 60
step <- 3
text_indexes <- seq(1, nchar(text) - maxlen, by = step)
sentences <- str_sub(text, text_indexes, text_indexes + maxlen - 1)
next_chars <- str_sub(text, text_indexes + maxlen, text_indexes + maxlen)
cat("Number of sequences: ", length(sentences), "\n")
chars <- unique(sort(strsplit(text, "")[[1]]))
cat("Unique characters:", length(chars), "\n")
char_indices <- 1:length(chars)
names(char_indices) <- chars
cat("Vectorization...\n")
x <- array(0L, dim = c(length(sentences), maxlen, length(chars)))
y <- array(0L, dim = c(length(sentences), length(chars)))
for (i in 1:length(sentences)) {
   sentence <- strsplit(sentences[[i]], "")[[1]]
   for (t in 1:length(sentence)) {
      char <- sentence[[t]]
      x[i, t, char_indices[[char]]] <- 1
   }
   next_char <- next_chars[[i]]
   y[i, char_indices[[next_char]]] <- 1
}

model <- keras_model_sequential() %>%
   layer_lstm(units = 128, input_shape = c(maxlen, length(chars))) %>%
   layer_dense(units = length(chars), activation = "softmax")
optimizer <- optimizer_rmsprop(lr = 0.01)
model %>% compile(
   loss = "categorical_crossentropy",
   optimizer = optimizer
   )

sample_next_char <- function(preds, temperature = 1.0) {
   preds <- as.numeric(preds)
   preds <- log(preds) / temperature
   exp_preds <- exp(preds)
   preds <- exp_preds / sum(exp_preds)
   which.max(t(rmultinom(1, 1, preds)))
   }

for (epoch in 1:60) {
   cat("epoch", epoch, "\n")
   model %>% fit(x, y, batch_size = 128, epochs = 1)
   start_index <- sample(1:(nchar(text) - maxlen - 1), 1)
   seed_text <- str_sub(text, start_index, start_index + maxlen - 1)
   cat("--- Generating with seed:", seed_text, "\n\n")
   for (temperature in c(0.2, 0.5, 1.0, 1.2)) {
      cat("------ temperature:", temperature, "\n")
      cat(seed_text, "\n")
      generated_text <- seed_text
      for (i in 1:400) {
         sampled <- array(0, dim = c(1, maxlen, length(chars)))
         generated_chars <- strsplit(generated_text, "")[[1]]
         for (t in 1:length(generated_chars)) {
            char <- generated_chars[[t]]
            sampled[1, t, char_indices[[char]]] <- 1
         }
         preds <- model %>% predict(sampled, verbose = 0)
         next_index <- sample_next_char(preds[1,], temperature)
         next_char <- chars[[next_index]]
         generated_text <- paste0(generated_text, next_char)
         generated_text <- substring(generated_text, 2)
         cat(next_char)
      }
      cat("\n\n")
   }
}

library(keras)
k_set_learning_phase(0)
model <- application_inception_v3(
   weights = "imagenet",
   include_top = FALSE,
)
layer_contributions <- list(
   mixed2 = 0.2,
   mixed3 = 3,
   mixed4 = 2,
   mixed5 = 1.5
)

layer_dict <- model$layers
names(layer_dict) <- lapply(layer_dict, function(layer) layer$name)
loss <- k_variable(0)
for (layer_name in names(layer_contributions)) {
   coeff <- layer_contributions[[layer_name]]
   activation <- layer_dict[[layer_name]]$output
   scaling <- k_prod(k_cast(k_shape(activation), "float32"))
   loss <- loss + (coeff * k_sum(k_square(activation)) / scaling)
}

dream <- model$input
grads <- k_gradients(loss, dream)[[1]]
grads <- grads / k_maximum(k_mean(k_abs(grads)), 1e-7)
outputs <- list(loss, grads)
fetch_loss_and_grads <- k_function(list(dream), outputs)
eval_loss_and_grads <- function(x) {
   outs <- fetch_loss_and_grads(list(x))
   loss_value <- outs[[1]]
   grad_values <- outs[[2]]
   list(loss_value, grad_values)
}
gradient_ascent <- function(x, iterations, step, max_loss = NULL) {
   for (i in 1:iterations) {
      c(loss_value, grad_values) %<-% eval_loss_and_grads(x)
      if (!is.null(max_loss) && loss_value > max_loss)
         break
      cat("...Loss value at", i, ":", loss_value, "\n")
      x <- x + (step * grad_values)
   }
   x
}

step <- 0.01
num_octave <- 3
octave_scale <- 1.4
iterations <- 20
max_loss <- 10
base_image_path <- "..."
img <- preprocess_image(base_image_path)
original_shape <- dim(img)[-1]
successive_shapes <- list(original_shape)
for (i in 1:num_octave) {
   shape <- as.integer(original_shape / (octave_scale ^ i))
   successive_shapes[[length(successive_shapes) + 1]] <- shape
}
successive_shapes <- rev(successive_shapes)
original_img <- img
shrunk_original_img <- resize_img(img, successive_shapes[[1]])

for (shape in successive_shapes) {
   cat("Processsing image shape", shape, "\n")
   img <- resize_img(img, shape)
   img <- gradient_ascent(img,
                          iterations = iterations,
                          step = step,
                          max_loss = max_loss)
   upscaled_shrunk_original_img <- resize_img(shrunk_original_img, shape)
   same_size_original <- resize_img(original_img, shape)
   lost_detail <- same_size_original - upscaled_shrunk_original_img
   img <- img + lost_detail
   shrunk_original_img <- resize_img(original_img, shape)
   save_img(img, fname = sprintf("dream_at_scale_%s.png",
                                 paste(shape, collapse = "x")))
   }

resize_img <- function(img, size) {
   image_array_resize(img, size[[1]], size[[2]])
}
save_img <- function(img, fname) {
   img <- deprocess_image(img)
   image_array_save(img, fname)
}
preprocess_image <- function(image_path) {
   image_load(image_path) %>%
      image_to_array() %>%
      array_reshape(dim = c(1, dim(.))) %>%
      inception_v3_preprocess_input()
}
deprocess_image <- function(img) {
   img <- array_reshape(img, dim = c(dim(img)[[2]], dim(img)[[3]], 3))
   img <- img / 2
   img <- img + 0.5
   img <- img * 255
   dims <- dim(img)
   img <- pmax(0, pmin(img, 255))
   dim(img) <- dims
   img
}

loss <- distance(style(reference_image) - style(generated_image)) +
   distance(content(original_image) - content(generated_image))

library(keras)
target_image_path <- "img/portrait.png"
style_reference_image_path <- "img/transfer_style_reference.png"
img <- image_load(target_image_path)
width <- img$size[[1]]
height <- img$size[[2]]
img_nrows <- 400
img_ncols <- as.integer(width * img_nrows / height)


preprocess_image <- function(path) {
   img <- image_load(path, target_size = c(img_nrows, img_ncols)) %>%
      image_to_array() %>%
      array_reshape(c(1, dim(.)))
   imagenet_preprocess_input(img)
}
deprocess_image <- function(x) {
   x <- x[1,,,]
   x[,,1] <- x[,,1] + 103.939
   x[,,2] <- x[,,2] + 116.779
   x[,,3] <- x[,,3] + 123.68
   x <- x[,,c(3,2,1)]
   x[x > 255] <- 255
   x[x < 0] <- 0
   x[] <- as.integer(x)/255
   x
}

target_image <- k_constant(preprocess_image(target_image_path))
style_reference_image <- k_constant(
   preprocess_image(style_reference_image_path)
)
combination_image <- k_placeholder(c(1, img_nrows, img_ncols, 3))
input_tensor <- k_concatenate(list(target_image, style_reference_image,
                                   combination_image), axis = 1)
model <- application_vgg19(input_tensor = input_tensor,
                           weights = "imagenet",
                           include_top = FALSE)
cat("Model loaded\n")

content_loss <- function(base, combination) {
   k_sum(k_square(combination - base))
}
gram_matrix <- function(x) {
   features <- k_batch_flatten(k_permute_dimensions(x, c(3, 1, 2)))
   gram <- k_dot(features, k_transpose(features))
   gram
}
style_loss <- function(style, combination){
   S <- gram_matrix(style)
   C <- gram_matrix(combination)
   channels <- 3
   size <- img_nrows*img_ncols
   k_sum(k_square(S - C)) / (4 * channels^2 * size^2)
}

total_variation_loss <- function(x) {
   y_ij <- x[,1:(img_nrows - 1L), 1:(img_ncols - 1L),]
   y_i1j <- x[,2:(img_nrows), 1:(img_ncols - 1L),]
   y_ij1 <- x[,1:(img_nrows - 1L), 2:(img_ncols),]
   a <- k_square(y_ij - y_i1j)
   b <- k_square(y_ij - y_ij1)
   k_sum(k_pow(a + b, 1.25))
}

outputs_dict <- lapply(model$layers, `[[`, "output")
names(outputs_dict) <- lapply(model$layers, `[[`, "name")
content_layer <- "block5_conv2"
style_layers = c("block1_conv1", "block2_conv1",
                 "block3_conv1", "block4_conv1",
                 "block5_conv1")
total_variation_weight <- 1e-4
style_weight <- 1.0
content_weight <- 0.025
loss <- k_variable(0.0)
layer_features <- outputs_dict[[content_layer]]
target_image_features <- layer_features[1,,,]
combination_features <- layer_features[3,,,]
loss <- loss + content_weight * content_loss(target_image_features,
                                             combination_features)
for (layer_name in style_layers) {
   layer_features <- outputs_dict[[layer_name]]
   style_reference_features <- layer_features[2,,,]
   combination_features <- layer_features[3,,,]
   sl <- style_loss(style_reference_features, combination_features)
   loss <- loss + ((style_weight / length(style_layers)) * sl)
}
loss <- loss +
   (total_variation_weight * total_variation_loss(combination_image))

grads <- k_gradients(loss, combination_image)[[1]]
fetch_loss_and_grads <-
   k_function(list(combination_image), list(loss, grads))
eval_loss_and_grads <- function(image) {
   image <- array_reshape(image, c(1, img_nrows, img_ncols, 3))
   outs <- fetch_loss_and_grads(list(image))
   list(
      loss_value = outs[[1]],
      grad_values = array_reshape(outs[[2]], dim = length(outs[[2]]))
   )
}
library(R6)
Evaluator <- R6Class("Evaluator",
                     public = list(
                        loss_value = NULL,
                        grad_values = NULL,
                        initialize = function() {
                           self$loss_value <- NULL
                           self$grad_values <- NULL
                        },
                        loss = function(x) {
                           loss_and_grad <- eval_loss_and_grads(x)
                           self$loss_value <- loss_and_grad$loss_value
                           self$grad_values <- loss_and_grad$grad_values
                           self$loss_value
                        },
                        grads = function(x) {
                           grad_values <- self$grad_values
                           self$loss_value <- NULL
                           self$grad_values <- NULL
                           grad_values
                        }
                     )
)

library(keras)
latent_dim <- 32
height <- 32
width <- 32
channels <- 3
generator_input <- layer_input(shape = c(latent_dim))
generator_output <- generator_input %>%
   layer_dense(units = 128 * 16 * 16) %>%
   layer_activation_leaky_relu() %>%
   layer_reshape(target_shape = c(16, 16, 128)) %>%
   layer_conv_2d(filters = 256, kernel_size = 5,
                 padding = "same") %>%
   layer_activation_leaky_relu() %>%
   layer_conv_2d_transpose(filters = 256, kernel_size = 4,
                           strides = 2, padding = "same") %>%
   layer_activation_leaky_relu() %>%
   layer_conv_2d(filters = 256, kernel_size = 5,
                 padding = "same") %>%
   layer_activation_leaky_relu() %>%
   layer_conv_2d(filters = 256, kernel_size = 5,
                 padding = "same") %>%
   layer_activation_leaky_relu() %>%
   layer_conv_2d(filters = channels, kernel_size = 7,
                 activation = "tanh", padding = "same")
generator <- keras_model(generator_input, generator_output)

discriminator_input <- layer_input(shape = c(height, width, channels))
discriminator_output <- discriminator_input %>%
   layer_conv_2d(filters = 128, kernel_size = 3) %>%
   layer_activation_leaky_relu() %>%
   layer_conv_2d(filters = 128, kernel_size = 4, strides = 2) %>%
   layer_activation_leaky_relu() %>%
   layer_conv_2d(filters = 128, kernel_size = 4, strides = 2) %>%
   layer_activation_leaky_relu() %>%
   layer_conv_2d(filters = 128, kernel_size = 4, strides = 2) %>%
   layer_activation_leaky_relu() %>%
   layer_flatten() %>%
   layer_dropout(rate = 0.4) %>%
   layer_dense(units = 1, activation = "sigmoid")
discriminator <- keras_model(discriminator_input, discriminator_output)
discriminator_optimizer <- optimizer_rmsprop(
   lr = 0.0008,
   clipvalue = 1.0,
   decay = 1e-8
)
discriminator %>% compile(
   optimizer = discriminator_optimizer,
   loss = "binary_crossentropy"
)


freeze_weights(discriminator)
gan_input <- layer_input(shape = c(latent_dim))
gan_output <- discriminator(generator(gan_input))
gan <- keras_model(gan_input, gan_output)
gan_optimizer <- optimizer_rmsprop(
   lr = 0.0004,
   clipvalue = 1.0,
   decay = 1e-8
)
gan %>% compile(
   optimizer = gan_optimizer,
   loss = "binary_crossentropy"
)



set.seed(100);train_test_split <- initial_split(matchdata_train, prop = 0.65)
train_tbl <- training(train_test_split);test_tbl = testing(train_test_split) 

example_log_1 %>% #a data.frame with the information in the table above
    eventlog(
        case_id = "patient",
        activity_id = "activity",
        activity_instance_id = "activity_instance",
        lifecycle_id = "status",
        timestamp = "timestamp",
        resource_id = "resource"
    )
patients %>%
 process_map(performance(median, "days"))



library(jsonlite)
train = read_csv("train.csv") %>% data.frame()
tr_device <- paste("[", paste(train$device, collapse = ","), "]") %>% fromJSON(flatten = T)

full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1])
                      
  stringi::stri_rand_strings(n = n, length = 8)

LHS[,new_field] <<-  RHS[,new_field][match(LHS[, key_LHS],RHS[, key_RHS])]      
students <- data.frame(people=c("Lily", "Bo", "Jen", "Omar", "Sara", "Jack", "Ting"), 
                       team=c("Red", "Blue", "Green", "Red", "Blue", "Green", "Red"), number=c(1,2,3,5,2,7,1))
scores <- data.frame(team=c("Black", "Blue", "Green", "Indigo", "Orange", "Red", "Violet", "White", "Yellow", 
                             "Green"), score1=c(90,96,10,88,82,84,95,89,79,93), score2=c(5,5,4,4,5,3,5,5,3,1))
students$score1 <- scores$score1[match(students$team, scores$team)]
  
algae_ctab = dcast(setDT(algae_new), season~speed, value.var='speed', length) %>% mutate(season = as.character(season))


cluster_select = function(data,y,name, var_cols,melt_col,max_clusters = 15, ...){
  library(skmeans);library(caroline);library(reshape)
  data_df = data.frame(data)
  cluster_other = data_df[apply(data_df[,var_cols], 1, function(x) var(x)) == 0,]
  cluster_all = data_df[apply(data_df[,var_cols], 1, function(x) var(x)) != 0,]  
  cluster_data = cluster_all[,-y] # for(i in names(cluster_data)) as.numeric(cluster_data[,i])
  ##################### K MEANS ##########################################
  wss = rep(NA, max_clusters);wss[1] = 0
  for (i in 2:max_clusters){ wss[i] <- sum(kmeans(cluster_data,centers=i)$betweenss/kmeans(cluster_data,centers=i)$totss)}
  drop = rep(NA,length(wss));drop[1] = 0.1;drop[2] = 0.1
  for(i in 3:(length(drop)))   drop[i] <- round((wss[i] - wss[i-1])/wss[i-1],3)
  neg_drop = which(drop< 0.05)
  if(length(neg_drop) == 0){
      warning("No drop in cluster selection below 5%. Look to increase number of maximum clusters")
  }
  if(length(neg_drop) != 0){
    Cluster_results = data.frame(Cluster_method = 'kmeans',Cluster_result = neg_drop[1])
    Results = list(Cluster_results = Cluster_results, Optimal_clusters = neg_drop[1])
    k_search = Cluster_results$Cluster_result
    mat_data = as.matrix(cluster_data)
    cluster_skmeans = skmeans(mat_data,k_search)
    cluster_var_results = data.frame(melt_col = cluster_all[,melt_col],Cluster =  as.vector(cluster_skmeans$cluster)) %>% mutate(melt_col = as.character(melt_col))
    last_cluster = max(cluster_var_results$Cluster)
    cluster_novar_results = data.frame(melt_col = cluster_other[, melt_col] ,Cluster =  rep(last_cluster+1, nrow(cluster_other))) %>% mutate(melt_col = as.character(melt_col))
    cluster_all_results = rbind(cluster_var_results,cluster_novar_results)
    names(cluster_all_results) = c(melt_col,"Cluster")
    file_name = paste("CDM_Cluster_",melt_col,".txt",sep ="")
    write.delim(cluster_all_results,file_name,sep = "|")
  }
} 

dat2 <- map(files_reduce, ~read_csv(.x));names(dat2) <- gsub("\\.csv", "", files_reduce)
gsub("\\.csv", "", files_reduce)

load_files = function(file_ext,filter_file_1=NULL,filter_file_2=NULL ){
   library(purrr);library(readr)
   file_type = paste0(".", "csv","$");files <- list.files(pattern = ".csv$")
   if(!is.null(filter_file_1)){
      files =  files[which(files != list.files(pattern = filter_file_1))]
   }
   if(!is.null(filter_file_2)){
      files = files[which(files != list.files(pattern = filter_file_2))]
   }
   data_all = map(files, ~read.csv(.x)); names(data_all) = gsub("\\.csv", "", files_reduce)
   data_all <<- data_all
}  
load_files(".csv", "hist");for(i in 1:length(data_all)){
   df_i = data_all[[i]]
   assign(names(data_all)[i], df_i)
   rm(df_i)
}


con = dbConnect(SQLite(), "Football_Records.sqlite")
db_check = try(dbGetQuery(con, "SELECT Player FROM Fixture_Detail"),silent = TRUE)
if(class(db_check) == "try-error"){
   dbWriteTable(con, "Fixture_Detail", fixture,overwrite = T)
}
if(class(db_check) != "try-error"){
   dbWriteTable(con, "Fixture_Detail", fixture,append = T)      
}
db_clean = try(dbGetQuery(con, "SELECT * FROM Fixture_Detail"),silent = TRUE)
if(class(db_clean) != "try-error"){
   all_records_temp =  dbGetQuery(con, "SELECT * FROM Fixture_Detail")
   all_records = unique(all_records_temp)
   dbWriteTable(con, "Fixture_Detail", all_records,overwrite = T)
}

#remove special characters from strings
str_replace_all(str_replace_all(game$Player, "[\r\n\t]" , ""), "[[:punct:]]", "")
gsub('[0-9]+', '',str_replace_all(str_replace_all(col_1, "[\r\n\t]" , ""), "[[:punct:]]", "")) 


h2o
library(h2o);h2o.init()
iris_hex <- as.h2o(iris)
test <- h2o.automl(y = "Species", training_frame = iris_hex, max_runtime_secs = 10)
test <- h2o.automl(y = "Species", training_frame = iris_hex, max_runtime_secs = 100)

#accuracy                 0.97333336 0.017638342  0.96666664          1.0  0.96666664 0.93333334         1.0
res =  test@leaderboard
head(res,10)
tail(res,10)
as.vector(res[1])
library(MASS)
boston_hex <- as.h2o(Boston)
bos_test <- h2o.automl(y = "medv", training_frame = boston_hex, max_runtime_secs = 100)

?h2o.predict

library(purrr)
ls_years =  nrow(df_years) %>% 
   rerun(data.frame(matrix(NA, nrow(data_t1),ncol(data_t1)))) %>% 
   map(~set_names(.x,names(data_t1)))

player_stats = function(data, years, start,end){
   data_t1 = data %>% filter(as.numeric(Season) >= start & as.numeric(Season) <= end)
   n_seasons = as.numeric(sort(unique(data_t1$Season)))
   
   df_years_t = data.frame(matrix(NA, length(n_seasons), years)); names(df_years_t) = paste0("year_", 1:years)
   for(i in 1:nrow(df_years_t)){
      years_filt = n_seasons[i:(i+years-1)]
      df_years_t[i,] = years_filt 
   }
   df_years <<- df_years_t %>% na.omit()
   years_min = as.vector(apply(df_years,1,min));years_max = as.vector(apply(df_years,1,max))
   
   ls_years =  nrow(df_years) %>% 
      rerun(data.frame(matrix(NA, nrow(data_t1),ncol(data_t1)))) %>% 
      map(~set_names(.x,names(data_t1)))
   
   for(i in 1:length(ls_years)){
      ls_years[[i]] = data_t1 %>% filter(Season >= years_min[i] & Season <= years_max[i])
      ls_years[[i]] = ls_years[[i]] %>% 
         mutate(team_ID = paste0(sapply(strsplit(Player_Key,"_",fixed = T), `[`, 1),"_", sapply(strsplit(Player_Key,"_",fixed = T), `[`, 3)),
                season_ID = max(Season)) %>%
         group_by(Player_Name,season_ID) %>%
         summarise(n_teams = length(unique(team_ID)),
                   sum_mins = sum(mins),sum_pl_mins = sum(pl_mins),
                   sum_goals = sum(goals),sum_pl_goals = sum(pl_goals),
                   avg_diff_leagues = mean(diff_leagues),mins_pl_ratio = round(sum(pl_mins)/sum(mins),2),goals_pl_ratio = round(sum(pl_goals)/sum(goals),2)
         ) %>% data.frame();ls_years[[i]][is.na(ls_years[[i]])] <- 0
   }
   all_years <<- bind_rows(ls_years)
}




