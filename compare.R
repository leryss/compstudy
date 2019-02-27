rm(list = ls())                          # Removes leftover variables
options(java.parameters = "-Xmx12096m")   # Increases java heap space

source('sets_util.R')
source('tests.R')

accs_and_sizes <- function() {
  small_sets = list(
    "E:\\licenta\\sets\\small\\adult",
    "E:\\licenta\\sets\\small\\car",
    "E:\\licenta\\sets\\small\\iris"
  )
  
  big_sets = list(
    "E:\\licenta\\sets\\big\\income",
    "E:\\licenta\\sets\\big\\forest",
    "E:\\licenta\\sets\\big\\connections"
  )
  
  results <- test_trees_from(small_sets, save_file_path = "E:\\licenta\\stats\\small_stats.txt")
  results <- test_trees_from(big_sets, "E:\\licenta\\stats\\big_stats.txt")
}

accs_and_sizes()

