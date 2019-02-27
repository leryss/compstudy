library('ggplot2')
library('reshape2')
library('Cairo')

source('sets_util.R')

test_over_missing <- function(sets, max_noise, save_path=NA) {
  if(missing(max_noise)) {
    max_noise <- 40
  }
  
  te <- sets$test
  j48_ev <- list(acc=list(), size=list())
  c50_ev <- list(acc=list(), size=list())
  
  ratios <- seq(1, max_noise, 0.35)
  for(i in ratios) {
    print(sprintf("Simulating for noise %.2f%%/%.2f%%", i, max_noise))
    tr <- add_noise(sets$train, i/100)
    T1 <- construct_j48(tr)
    T2 <- construct_c50(tr)
    
    p_j48 <- predict_tree(T1, te)
    p_c50 <- predict_tree(T2, te)
    
    j48_ev$acc <- append(j48_ev$acc, p_j48$acc)
    c50_ev$acc <- append(c50_ev$acc, p_c50$acc)
    
    j48_ev$size <- append(j48_ev$size, j48_size(T1))
    c50_ev$size <- append(c50_ev$size, c50_size(T2))
  }
  
  #Create accuracy plot
  data_acc <- data.frame(
    j48 = unlist(j48_ev$acc),
    c50 = unlist(c50_ev$acc),
    Noise = ratios)
  
  plot_acc <- ggplot(data=data_acc) + 
    geom_line(aes(x=Noise, y=j48, colour="J48")) +
    geom_line(aes(x=Noise, y=c50, colour="C50")) +
    geom_point(aes(x=Noise, y=j48, colour="J48"), shape=17) +
    geom_point(aes(x=Noise, y=c50, colour="C50"), shape=19) +
    labs(x = 'Noise') + labs(y = 'Accuracy')
  
  #Create size plot
  data_size <- data.frame(
    j48 = unlist(j48_ev$size),
    c50 = unlist(c50_ev$size),
    Noise = ratios)
  
  plot_size <- ggplot(data=data_size) + 
    geom_line(aes(x=Noise, y=j48, colour="J48")) +
    geom_line(aes(x=Noise, y=c50, colour="C50")) +
    geom_point(aes(x=Noise, y=j48, colour="J48"), shape=17) +
    geom_point(aes(x=Noise, y=c50, colour="C50"), shape=19) +
    labs(x = 'Noise') + labs(y = 'Nr. leaves')
  
  #Save plots
  ggsave(plot=plot_acc, file=paste(save_path, "acc_over_missing.png", sep="\\"), type='cairo')
  ggsave(plot=plot_size, file=paste(save_path, "size_over_missing.png", sep="\\"), type='cairo')
  
}

test_trees_from <- function(data_paths, save_file_path=NA) {
  trees <- list()
  for(path in data_paths) {
    print(sprintf("Testing set %s", path))
    print("    (1/4) Loading data sets")
    sets <- load_data_sets(path)
    
    data_size <- nrow(sets$train) + nrow(sets$test)
    noise <- calculate_noise(sets$train) + calculate_noise(sets$test)
    noise <- data_size/noise
    attrbs <- count_attrb_types(sets$train)
    
    print("    (2/4) Constructing C4.5 tree")
    T1 <- construct_j48(sets$train)
    print("    (3/4) Constructing C5.0 tree")
    T2 <- construct_c50(sets$train)
    
    print("    (4/4) Testing the trees...")
    j48_pred <- predict_tree(T1, sets$test)
    c50_pred <- predict_tree(T2, sets$test)
    
    j48stats <- list(err=j48_pred$err, leaves=j48_size(T1))
    c50stats <- list(err=c50_pred$err, leaves=c50_size(T2))
    
  
    stats <- list(data_path=path, 
                  data_size=data_size,
                  noise=noise,
                  continuous_attrs=attrbs$continuous, 
                  discrete_attrs=attrbs$discrete, 
                  j48=j48stats, 
                  c50=c50stats)
    
    trees <- append(trees, stats)
  }
  
  cnames <- c(c("path", "data_size", "noise", "continuous_attrs", "discrete_attrs"), paste("j48", names(trees$j48), sep="."), paste("c50", names(trees$c50), sep="."))
  result <- data.frame(matrix(unlist(trees), nrow=length(data_paths), byrow=T))
  colnames(result) <- cnames
  
  if(!is.na(save_file_path)){
    old_width <- options()$width
    options(width=1000)
    write(capture.output(result, file=save_file_path))
    options(width=old_width)
  }
  
  return(result)
}






