library('RWeka')
library('datasets')
library('C50')

#Loads the the training and test sets from a folder
load_data_sets <- function(path) {
  tr_path = paste(path, "train.txt", sep="\\")
  te_path = paste(path, "test.txt", sep="\\")
  
  train <- read.csv(tr_path, header=TRUE, sep=',', encoding='latin1', na.strings='?', strip.white=TRUE)
  train[, ncol(train)] <- as.factor(train[, ncol(train)])
  
  test <- read.csv(te_path, header=TRUE, sep=',', encoding='latin1', na.strings='?', strip.white=TRUE)
  test[, ncol(test)] <- as.factor(test[, ncol(test)])
  
  #Update factors for test set
  for(col in colnames(train)){
    if(class(test[[col]]) == 'factor'){
      combined_lvls <- union(levels(train[[col]]), levels(test[[col]]))
      levels(test[[col]]) <- combined_lvls
      levels(train[[col]]) <- combined_lvls
    }
    
  }
  
  return(list(train=train, test=test))
}

#Constructs C4.5 prunned tree
construct_j48 <- function(train_set) {
  target <- colnames(train_set)[ncol(train_set)]
  
  return(J48(as.formula(paste(target, '.', sep='~')), data=train_set))
}

#Constructs C5.0 prunned tree
construct_c50 <- function(train_set) {
  target <- colnames(train_set)[ncol(train_set)]
  
  return(C5.0(as.formula(paste(target, '.', sep='~')), data=train_set, na.action=NULL))
}

#Calculates the err & acc of a test_set
predict_tree <- function(tree, test_set) {
  t <- test_set[, ncol(test_set)]
  p <- predict(tree, test_set)
  
  hits <- 0
  full_len <- nrow(test_set)
  for(i in 1:full_len) {
    if(identical(t[i], p[i])) {
      hits <- hits + 1
    }
  }
  
  acc <- hits/full_len
  return(list(hits=hits,misses=full_len-hits,err=1.0-acc, acc=acc))
}

j48_size <- function(T) {
  o <- capture.output(T)
  o_l <- length(o)
  
  return(as.numeric(gsub('\\D', '', o[o_l - 3])))
}

c50_size <- function(T) {
  return(as.numeric(T$size))
}

add_noise <- function(set, ratio) {
  n_r <- ratio/ncol(set)
  
  s_l <- nrow(set)
  n_l <- floor(s_l*ratio)
  
  for(i in 1:ncol(set)) {
    
    col <- set[, i]
    if(class(col) == 'factor') {
      col[sample(1:s_l, n_l)] <- NA
    }
    set[, i] <- col
  }
  return(set)
}

calculate_noise <- function(set) {
  noise <- 0
  for(i in 1:ncol(set)) {
    noise <- noise + sum(is.na(set[, i]))
  }
  
  return(noise)
}

count_attrb_types <- function(set) {
  disc <- 0
  cont <- 0
  for(i in 1:ncol(set)) {
    if(class(set[, i]) == 'factor')
      disc <- disc + 1
    else
      cont <- cont + 1
  }
  
  return(list(discrete=disc, continuous=cont))
}

