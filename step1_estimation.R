setwd('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam')

#install.packages('tidyverse')
library(tidyverse)
#install.packages('grf')
library(grf)
#install.packages("future.apply")


# Data Cleaning -----------------------------------------------------------


load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\01_Material\\Group 22\\north.rdata")

# Define data as tibble
df_raw <- north %>% 
  as_tibble()

# Remove original dataframe
rm(north)

# Redfine employment variable
df_raw <- df_raw %>% 
  mutate_at(.vars = vars(contains('EMPL')), .funs = funs(case_when(. == 1 ~ 1,
                                                                   . %in% c(2,3) ~ 0)))


# Fraction of unique observations
length(unique(df_raw$pers))/length(df_raw$pers)

# Drop duplicated rows
df_raw <- df_raw[!duplicated(df_raw),]
df_raw %>% colnames()

# Tratement variable
table(df_raw$ptype)
# Programme type:
# 1: T1
# 2: T2
# 3: E1
# 4: E2
# 0: no programme

# Crosstable Treatment - Duration:
table(df_raw$ptype, df_raw$durat)

# Crosstable Treatment - Treatment (specific programme):
table(df_raw$ptype==0,df_raw$D_0, useNA = 'always')
table(df_raw$ptype==1,df_raw$D_1, useNA = 'always')
table(df_raw$ptype==2,df_raw$D_2, useNA = 'always')
table(df_raw$ptype==3,df_raw$D_3, useNA = 'always')
table(df_raw$ptype==4,df_raw$D_4, useNA = 'always')
# Check where missings in Treatment (specific programme) come from:
df_raw %>% 
  filter(is.na(D_4)) %>%
  select(ptype) %>% 
  table(.)
# missings in D_d are weird as there actually is information regarding the programme captured in ptype (w/o any missings)






# Variables ---------------------------------------------------------------

# Outcome
# Y: EARNX3_1 - EARNX9_4, EMPLX3_1 - EMPLX9_4
# D: pytpe
# X: 
##  - EARN_XO (conf.)
##  - EARNX1_1 - EARNX2_4 (conf.)
##  - UNEM_X0 (conf.)
##  - EM_XO (conf.)
##  - OLF_X0 (conf.)
##  - EMPLX1_1 - EMPLX3_4 (conf.)
##  - age (conf.)
##  - sex (Y)
##  - school (conf.)
##  - VOC_DEG (conf.)
##  - nation (Y)
##  - LMP_CW (conf.)
##  - ShP_CW_1 - ShP_CW_4 (D)
##  - SPECIA_C (D)
##  - REGION (Y)
##  - REG_AL (Y)
##  - REG_PRG (D)
##  - REG_SER (Y)
##  - REG_PRO (Y)
##  - REG_AGRI (Y)
##  - SECT_AL (Y)
##  - PROF_AL (Y)
##  - PROF_XL (Y)
# Others:
##  - durat -> endogenous (but not in our case since we only compare PARTICIPANTS outcome of different programmes)
##  - C_T1, C_T2, C_E1, C_E2 -> endogenous (but not in our case since we only compare PARTICIPANTS outcome of different programmes)
X <- df_raw %>% 
  select(
    # Duration of programme (planned) in Months (conf.)
    EARN_X0,
    # Average monthly earnings prior to treatment (conf.)
    EARNX1_1:EARNX2_4,
    # Average number of months of reg. unemployment in 10 years before 19X1 (conf.)
    UNEM_X0,
    # Average number of months of employment in 10 years before 19X1 (conf.)
    EM_X0,
    # Average number of months out-of-the-labour-force in 10 years before 19X1 (conf.)
    OLF_X0,
    # Employment state prior to treatment (conf.)
    EMPLX1_1:EMPLX2_4,
    # Age in years (conf.)
    age,
    # Sex (1 male, 0 female) 
    sex,
    # Schooling (degrees in years; 8: no degree) (conf.)
    school,
    # Vocational degree (0: None; 1: below university; 2 university) (conf.)
    VOC_DEG,
    # Nationality: 1 Local; 2 other European; 3: Asian; 4 African; 5: American
    nation,
    # Labour market prospects without programme as assessed by case worker (1 very bad, 4 very good) (conf.!!!)
    LMP_CW,
    # Caseworkers share of clients allocated to programme y
    SHP_CW_1:SHP_CW_4,
    # Unemployed in contact with caseworker with additional resources for ALMP
    SPECIA_C,
    # Region of labour office (1-85)
    region,
    # Regional unemployment rate in %
    REG_AL,
    # Regional share of unemployed participating in programmes
    REG_PRG,
    # Regional share of service sector
    REG_SER,
    # Regional share of production sector
    REG_PRO,
    # Regional share of agriculture
    REG_AGRI,
    # Unemployment rate in sector of last occupation
    SECT_AL,
    # Unemployment rate in profession of last occupation
    PROF_AL,
    # Professional unemployment rate (variable not verified by Section XX.12 of Department of …)
    #PROF_XL
    ) %>% 
  colnames()


# Missing values ----------------------------------------------------------
sapply(df_raw, function(x) sum(is.na(x)))



# Estimation --------------------------------------------------------------


IATEs_crf <- function(D1='E1', D2='T1', X=X, df=df_raw, num.trees=2000, tune.parameters="none", year_start = 3, limit = 1, parallel=FALSE, res_name){
  
  # Calculate number of periods
  if(limit == "none"){
    limit <- (10 - year_start)*4*2
  }
   
  
  # Define different outcomes for loop
  Ys <- c(paste('EARNX', rep(year_start:9, each = 4), '_', 1:4, sep = ''), paste('EMPLX', rep(year_start:9, each = 4), '_', 1:4, sep = ''))[1:limit]
  
  # Define covariate names
  X_names <- X
  
  # Define binary treatment variable
  D_levels <- c('0'="No program",'1'="T1",'2'="T2",'3'="E1",'4'="E2")
  df <- transform(df, ptype = D_levels[as.character(ptype)]) %>% as_tibble()
  
  # Instantiate df for estimates
  df_res <- df %>% 
    filter(ptype %in% c(D1, D2)) %>% 
    select(pers)
  
  #list_res <- list()
  
  # Start loop
  for (Y in Ys){
    
    # Select relevant variables according to treatment and dropping missings in target
    df_temp <- df %>% 
      filter(ptype %in% c(D1, D2)) %>% 
      mutate(D = ifelse(ptype %in% D1, 1, 0)) %>% 
      filter(!is.na(eval(parse(text = Y))))
    
    # Define outcome, treatment and covariates (confounders)
    O <- df_temp %>% select(Y) %>% as_vector()
    D <- df_temp %>% select(D) %>% as_vector()
    X <- df_temp[,X_names]
    
    # Estimate causal forest
    crf <- causal_forest(X = X, Y = O, W = D,
                         num.trees = num.trees,
                         tune.parameters = tune.parameters,
                         seed = 333)
    
    # Extract treatment effects (IATEs) and estimate variance
    #hte <- predict(
    #  object = crf, 
    #  estimate.variance = TRUE
    #)
    
    # Attach results to filtered df
    #df_temp <- df_temp %>% select(pers)
    #df_temp[,Y] <- hte$predictions
    #df_temp[,paste(Y,'se',sep = '_')] <- sqrt(hte$variance.estimates)
    
    # Join results to results from prior iteration
    #df_res <- df_res %>% left_join(df_temp, by = 'pers')
    
    print(Y)
    
  
  
  # Parallelization of different estimates becomes possible with this tweak
  if(parallel){
    
    list_name <- paste(Y)
    #list_res[[list_name]] <- crf
    save(crf, file = file.path(getwd(), '03_Results', '02_Intermediate', '01_PostTreatment', '01_SanityCheck', paste(res_name, '_', list_name, '.RData', sep = "")))
  }
    
  }
  
  #return(list_res)
}



comb <- list(
  list(c('T1', 'T2', 'E1', 'E2'), c('No program'))
  #list(c('T1'), c('No program')),
  #list(c('T2'), c('No program')),
  #list(c('E1'), c('No program')),
  #list(c('E2'), c('No program'))
  #list(c('T1'), c('E1')),
  #list(c('T1'), c('E2')),
  #list(c('T2'), c('E1')),
  #list(c('T2'), c('E2'))
  #list(c('T1','T2'), c('E1','E2'))
  )



lapply(comb, function(x){
  
  res_name <- sub(pattern = ' ', replacement = '', paste(paste(x[[1]], collapse = ""), 'vs', paste(x[[2]], collapse = ""), sep = ""))
  IATEs_crf(D1 = x[[1]], D2 = x[[2]], X=X, df=df_raw, num.trees = 2000, year_start = 3, limit = "none", parallel = TRUE, res_name=res_name)
  #save(res, file = file.path(getwd(), paste(res_name, '.RData', sep = "")))
  print('Estimation done!')
  
  
})

res_name <- sub(pattern = ' ', replacement = '', paste('GRF_', paste('T1', collapse = ""), 'vs', paste('E1', collapse = ""), sep = ""))
IATEs_crf(D1 = 'T1', D2 = 'E1', X=X, df=df_raw, num.trees = 100, year_start = 8, limit = 3, parallel = TRUE)
#plan(multiprocess)
#system.time(list_res <- {future_lapply(setNames(comb, comb), function(x) IATEs_crf(D1 = x[[1]], D2 = x[[2]], X=X, df = df_raw, num.trees = 4000, year_start = 3, limit = "none", parallel = TRUE))})
#system.time({temp2 <- lapply(setNames(comb, comb), function(x) IATEs_crf(D1 = x[[1]], D2 = x[[2]], X=X, df = df_raw, num.trees = 200, year_start = 8, limit = 2, parallel = TRUE))})



E1vsT1 <- IATEs('E1', 'T1', df_raw, year_start = 3, limit = "none")
E2vsT2 <- IATEs('E2', 'T2', df_raw, year_start = 3, limit = "none")
E1vsT2 <- IATEs('E1', 'T2', X, df_raw, year_start = 3, limit = "none")
temp <- IATEs(c('E1', 'E2'), 'No program', X, df_raw, year_start = 9, limit = 1)

save(E2vsT2, file = file.path('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\', 'E2vsT2.RData'))







# Placebo Test ------------------------------------------------------------
## Variables ==============================================================

# Outcome
# Y: EARNX3_1 - EARNX9_4, EMPLX3_1 - EMPLX9_4
# D: pytpe
# X: 
##  - EARN_XO (conf.)
##  - EARNX1_1 - EARNX2_4 (conf.)
##  - UNEM_X0 (conf.)
##  - EM_XO (conf.)
##  - OLF_X0 (conf.)
##  - EMPLX1_1 - EMPLX3_4 (conf.)
##  - age (conf.)
##  - sex (Y)
##  - school (conf.)
##  - VOC_DEG (conf.)
##  - nation (Y)
##  - LMP_CW (conf.)
##  - ShP_CW_1 - ShP_CW_4 (D)
##  - SPECIA_C (D)
##  - REGION (Y)
##  - REG_AL (Y)
##  - REG_PRG (D)
##  - REG_SER (Y)
##  - REG_PRO (Y)
##  - REG_AGRI (Y)
##  - SECT_AL (Y)
##  - PROF_AL (Y)
##  - PROF_XL (Y)
# Others:
##  - durat -> endogenous (but not in our case since we only compare PARTICIPANTS outcome of different programmes)
##  - C_T1, C_T2, C_E1, C_E2 -> endogenous (but not in our case since we only compare PARTICIPANTS outcome of different programmes)
X <- df_raw %>% 
  select(
    # Duration of programme (planned) in Months (conf.)
    EARN_X0,
    # Average monthly earnings prior to treatment (conf.)
    EARNX1_1:EARNX1_4,
    # Average number of months of reg. unemployment in 10 years before 19X1 (conf.)
    UNEM_X0,
    # Average number of months of employment in 10 years before 19X1 (conf.)
    EM_X0,
    # Average number of months out-of-the-labour-force in 10 years before 19X1 (conf.)
    OLF_X0,
    # Employment state prior to treatment (conf.)
    EMPLX1_1:EMPLX1_4,
    # Age in years (conf.)
    age,
    # Sex (1 male, 0 female) 
    sex,
    # Schooling (degrees in years; 8: no degree) (conf.)
    school,
    # Vocational degree (0: None; 1: below university; 2 university) (conf.)
    VOC_DEG,
    # Nationality: 1 Local; 2 other European; 3: Asian; 4 African; 5: American
    nation,
    # Labour market prospects without programme as assessed by case worker (1 very bad, 4 very good) (conf.!!!)
    LMP_CW,
    # Caseworkers share of clients allocated to programme y
    SHP_CW_1:SHP_CW_4,
    # Unemployed in contact with caseworker with additional resources for ALMP
    SPECIA_C,
    # Region of labour office (1-85)
    region,
    # Regional unemployment rate in %
    REG_AL,
    # Regional share of unemployed participating in programmes
    REG_PRG,
    # Regional share of service sector
    REG_SER,
    # Regional share of production sector
    REG_PRO,
    # Regional share of agriculture
    REG_AGRI,
    # Unemployment rate in sector of last occupation
    SECT_AL,
    # Unemployment rate in profession of last occupation
    PROF_AL,
    # Professional unemployment rate (variable not verified by Section XX.12 of Department of …)
    #PROF_XL
  ) %>% 
  colnames()





## Estimation =============================================================
setwd('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\02_Intermediate\\02_Placebo')

IATEs_crf_placebo <- function(D1='E1', D2='T1', X=X, df=df_raw, num.trees=2000, tune.parameters="none", year_start = 3, limit = 1, parallel=FALSE, res_name){
  
  # Calculate number of periods
  #if(limit == "none"){
  #  limit <- (10 - year_start)*4*2
  #}
  
  
  # Define different outcomes for loop
  Ys <- c(paste('EARNX', year_start, '_', 1:3, sep = ''), paste('EMPLX', year_start, '_', 1:3, sep = ''))
  
  # Define covariate names
  X_names <- X
  
  # Define binary treatment variable
  D_levels <- c('0'="No program",'1'="T1",'2'="T2",'3'="E1",'4'="E2")
  df <- transform(df, ptype = D_levels[as.character(ptype)]) %>% as_tibble()
  
  # Instantiate df for estimates
  df_res <- df %>% 
    filter(ptype %in% c(D1, D2)) %>% 
    select(pers)
  
  #list_res <- list()
  
  # Start loop
  for (Y in Ys){
    
    # Select relevant variables according to treatment and dropping missings in target
    df_temp <- df %>% 
      filter(ptype %in% c(D1, D2)) %>% 
      mutate(D = ifelse(ptype %in% D1, 1, 0)) %>% 
      filter(!is.na(eval(parse(text = Y))))
    
    # Define outcome, treatment and covariates (confounders)
    O <- df_temp %>% select(Y) %>% as_vector()
    D <- df_temp %>% select(D) %>% as_vector()
    X <- df_temp[,X_names]
    
    # Estimate causal forest
    crf <- causal_forest(X = X, Y = O, W = D,
                         num.trees = num.trees,
                         tune.parameters = tune.parameters,
                         seed = 333)
    
    # Extract treatment effects (IATEs) and estimate variance
    #hte <- predict(
    #  object = crf, 
    #  estimate.variance = TRUE
    #)
    
    # Attach results to filtered df
    #df_temp <- df_temp %>% select(pers)
    #df_temp[,Y] <- hte$predictions
    #df_temp[,paste(Y,'se',sep = '_')] <- sqrt(hte$variance.estimates)
    
    # Join results to results from prior iteration
    #df_res <- df_res %>% left_join(df_temp, by = 'pers')
    
    print(Y)
    
    
    
    # Parallelization of different estimates becomes possible with this tweak
    if(parallel){
      
      list_name <- paste(Y)
      #list_res[[list_name]] <- crf
      save(crf, file = file.path(getwd(), paste(res_name, '_', list_name, '.RData', sep = "")))
    }
    
  }
  
  #return(list_res)
}



comb <- list(
  #list(c('T1', 'T2', 'E1', 'E2'), c('No program')),
  #list(c('T1'), c('No program')),
  #list(c('T2'), c('No program')),
  #list(c('E1'), c('No program')),
  #list(c('E2'), c('No program'))
  list(c('T1'), c('E1')),
  list(c('T1'), c('E2')),
  list(c('T2'), c('E1')),
  list(c('T2'), c('E2'))
  #list(c('T1','T2'), c('E1','E2'))
)



lapply(comb, function(x){
  
  res_name <- sub(pattern = ' ', replacement = '', paste(paste(x[[1]], collapse = ""), 'vs', paste(x[[2]], collapse = ""), sep = ""))
  IATEs_crf_placebo(D1 = x[[1]], D2 = x[[2]], X=X, df=df_raw, num.trees = 2000, year_start = 2, limit = "none", parallel = TRUE, res_name=res_name)
  #save(res, file = file.path(getwd(), paste(res_name, '.RData', sep = "")))
  print('Estimation done!')
  
  
})


# Sample Code -------------------------------------------------------------

# Generate data.
n = 2000; p = 10 # 2000 observations and 10 covariates
X = matrix(rnorm(n*p), n, p)
X.test = matrix(0, 101, p)
X.test[,1] = seq(-2, 2, length.out = 101)

# Train a causal forest.
W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0)) # treatment partially determined by X_1
cor.test(W, X[,1]) # significant correlation between W and X_1
cor.test(W, X[,2]) # no significant correlation between W and X_2
cor.test(W, X[,3]) # no significant correlation between W and X_3
# ...

Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
cor.test(Y, X[,1]) # significant correlation between Y and X_1
cor.test(Y, X[,2]) # significant correlation between Y and X_2
cor.test(Y, X[,3]) # significant correlation between Y and X_3
cor.test(Y, X[,4]) # no significant correlation between Y and X_4
# ...
# -> X_1 acts as confounder!

tau.forest = causal_forest(X, Y, W)


# Estimate treatment effects for the training data using out-of-bag prediction.
tau.hat.oob = predict(tau.forest)
dim(tau.hat.oob)
hist(tau.hat.oob$predictions)


# Estimate treatment effects for the test sample.
tau.hat = predict(tau.forest, X.test) # IATEs
plot(X.test[,1], tau.hat$predictions, ylim = range(tau.hat$predictions, 0, 2),
     xlab = "x", ylab = "tau", type = "l")
lines(X.test[,1], pmax(0, X.test[,1]), col = 2, lty = 2)

# Estimate the conditional average treatment effect on the full sample (CATE).
average_treatment_effect(tau.forest, target.sample = "all") 
ate <- average_treatment_effect(tau.forest, target.sample = "all") 
# Alternative approach w/o correction
predict(tau.forest, estimate.variance = TRUE) %>% 
  as_tibble() %>% 
  select(predictions, variance.estimates) %>%
  mutate(se = variance.estimates) %>% 
  summarise(ate = mean(predictions),
            se = sqrt(mean(se)/nrow(.)))
  aversummarise(ate = mean(predictions),
            se = sqrt(mean(variance.estimates)/(nrow(.)-1)))


# Estimate the conditional average treatment effect on the treated sample (CATT).
# Here, we don't expect much difference between the CATE and the CATT, since
# treatment assignment was randomized.
average_treatment_effect(tau.forest, target.sample = "treated")


# Add confidence intervals for heterogeneous treatment effects; growing more
# trees is now recommended.
tau.forest = causal_forest(X, Y, W, num.trees = 4000)
tau.hat = predict(tau.forest, X.test, estimate.variance = TRUE)
sigma.hat = sqrt(tau.hat$variance.estimates)

ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2)
plot(X.test[,1], tau.hat$predictions, ylim = ylim, xlab = "x", ylab = "tau", type = "l")
lines(X.test[,1], tau.hat$predictions + 1.96 * sigma.hat, col = 1, lty = 2)
lines(X.test[,1], tau.hat$predictions - 1.96 * sigma.hat, col = 1, lty = 2)
lines(X.test[,1], pmax(0, X.test[,1]), col = 2, lty = 1)


# In some examples, pre-fitting models for Y and W separately may
# be helpful (e.g., if different models use different covariates).
# In some applications, one may even want to get Y.hat and W.hat
# using a completely different method (e.g., boosting).

# Generate new data.
n = 4000; p = 20
X = matrix(rnorm(n * p), n, p)
TAU = 1 / (1 + exp(-X[, 3]))
W = rbinom(n ,1, 1 / (1 + exp(-X[, 1] - X[, 2])))
Y = pmax(X[, 2] + X[, 3], 0) + rowMeans(X[, 4:6]) / 2 + W * TAU + rnorm(n)

forest.W = regression_forest(X, W, tune.parameters = "all")
W.hat = predict(forest.W)$predictions

forest.Y = regression_forest(X, Y, tune.parameters = "all")
Y.hat = predict(forest.Y)$predictions

forest.Y.varimp = variable_importance(forest.Y)

# Note: Forests may have a hard time when trained on very few variables
# (e.g., ncol(X) = 1, 2, or 3). We recommend not being too aggressive
# in selection.
selected.vars = which(forest.Y.varimp / mean(forest.Y.varimp) > 0.2)

tau.forest = causal_forest(X[, selected.vars], Y, W,
                           W.hat = W.hat, Y.hat = Y.hat,
                           tune.parameters = "all")


# Work with GATE calculation


