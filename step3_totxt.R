setwd("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\02_Intermediate\\01_PostTreatment")
all_files <- list.files(getwd())

for (j in all_files){
  
  attempt <- 0
  while(!exists('crf') && attempt <= 3){
    attempt <- attempt + 1
    try(
    load(file.path(getwd(), j)))
  }
  
  prop_score <- crf$W.hat %>% as_tibble()
  write_delim(prop_score, path = file.path("I:\\LFN\\Causal Machine Learning\\Estimation\\02_Propensities", paste(sub(".RData", "", j), '.txt', sep = "")), delim = '\t')
  
  rm(crf)

}
