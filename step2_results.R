setwd('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\02_Intermediate')

#install.packages('tidyverse')
library(tidyverse)
#install.packages('grf')
library(grf)
source(file = "Q:\\Meine Bibliotheken\\Research\\06_Environments\\ggplot_template.R")


# Question (ii) -----------------------------------------------------------

comb <- list(
  list(c('T1', 'T2', 'E1', 'E2'), c('No program'))
  #list(c('T1'), c('No program')),
  #list(c('T2'), c('No program'))
  #list(c('E1'), c('No program')),
  #list(c('E2'), c('No program'))
  #list(c('T1'), c('E1')),
  #list(c('T1'), c('E2')),
  #list(c('T2'), c('E1')),
  #list(c('T2'), c('E2'))
  #list(c('T1','T2'), c('E1','E2'))
)

year_start <- 3
limit <- "none"
if(limit == "none"){
  limit <- (10 - year_start)*4*2
}

Ys <- c(paste('EARNX', rep(year_start:9, each = 4), '_', 1:4, sep = ''), paste('EMPLX', rep(year_start:9, each = 4), '_', 1:4, sep = ''))[1:limit]

for (j in comb){
  
  df_res <- enframe(Ys, value = "DESCRIPTION", name = NULL) %>% 
    mutate(ATE=NA,
           SE=NA) %>% 
    mutate_if(is.logical, as.numeric)
  
  res_name <- sub(pattern = ' ', replacement = '', paste(paste(j[[1]], collapse = ""), 'vs', paste(j[[2]], collapse = ""), sep = ""))
  
  
  for (Y in Ys){
    
    # Load Causal Random Forest for respective period and outcome variable
    # Try several times if loading fails the first time
    attempt <- 0
    while(!exists('crf') && attempt <= 3){
      attempt <- attempt + 1
      try(
        load(file = file.path(getwd(), '01_PostTreatment', paste(res_name, '_', Y, '.RData', sep = "")))
      )
    }
    
    
    
    # Estimate from IATEs the respective ATEs and standard errors
    ate <- average_treatment_effect(crf, target.sample = 'all')
    
    # Remove big causal_forest object and wait for a couple of seconds
    rm(crf)
    Sys.sleep(3)
    
    # Attach results to filtered df
    df_res[df_res['DESCRIPTION'] == Y, 'ATE'] <- ate['estimate']
    df_res[df_res['DESCRIPTION'] == Y, 'SE'] <- ate['std.err']
    
    print(Y)
    
  }
  
  df_res <- df_res %>%
    mutate(
      CI_up = ATE + qnorm(0.975)*SE,
      CI_low = ATE - qnorm(0.975)*SE
    )
  
  save(df_res, file = file.path('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\02_Intermediate\\01_PostTreatment\\01_SanityCheck', paste('Q2_', res_name, '_ATEs.RData', sep=""), fsep = '\\'))
  
  
  
  
}


source(file = "Q:\\Meine Bibliotheken\\Research\\06_Environments\\ggplot_template.R")
      
  
df_res %>% 
  filter(str_detect(DESCRIPTION, 'EMPL')) %>% 
  mutate(PERIOD = as.numeric(row.names(.))) %>% 
  ggplot() + 
  geom_line(aes(x = PERIOD, y = ATE, group=0)) +
  geom_ribbon(aes(ymin=CI_low,ymax=CI_up,x=PERIOD),alpha=0.3) +
  geom_hline(yintercept = 0, col = "red", linetype="dashed") +
  scale_x_continuous(labels = 1:nrow(df_res), breaks = 1:nrow(df_res)) +
  #labs(title = title) +
  #xlab(x_lab) +
  #ylab(y_lab) +
  theme_jod  
  


# Question (iii) ----------------------------------------------------------


comb <- list(
  #list(c('T1', 'T2', 'E1', 'E2'), c('No program'))
  #list(c('T1'), c('No program')),
  #list(c('T2'), c('No program')),
  #list(c('E1'), c('No program')),
  #list(c('E2'), c('No program')),
  #list(c('T1'), c('E1')),
  #list(c('T1'), c('E2')),
  #list(c('T2'), c('E1')),
  #list(c('T2'), c('E2'))
  list(c('T1','T2'), c('E1','E2'))
)

year_start <- 3
limit <- "none"
if(limit == "none"){
  limit <- (10 - year_start)*4*2
}

Ys <- c(paste('EARNX', rep(year_start:9, each = 4), '_', 1:4, sep = ''), paste('EMPLX', rep(year_start:9, each = 4), '_', 1:4, sep = ''))[1:limit]

for (j in comb){
  
  df_res <- enframe(Ys, value = "DESCRIPTION", name = NULL) %>% 
    mutate(ATE=NA,
           SE=NA) %>% 
    mutate_if(is.logical, as.numeric)
  
  res_name <- sub(pattern = ' ', replacement = '', paste(paste(j[[1]], collapse = ""), 'vs', paste(j[[2]], collapse = ""), sep = ""))
  
  
  for (Y in Ys){
    
    # Load Causal Random Forest for respective period and outcome variable
    # Try several times if loading fails the first time
    attempt <- 0
    while(!exists('crf') && attempt <= 3){
      attempt <- attempt + 1
      try(
        load(file = file.path(getwd(), paste(res_name, '_', Y, '.RData', sep = "")))
      )
    }
    
    
    
    # Estimate from IATEs the respective ATEs and standard errors
    ate <- average_treatment_effect(crf, target.sample = 'all')
    
    # Remove big causal_forest object and wait for a couple of seconds
    rm(crf)
    Sys.sleep(3)
    
    # Attach results to filtered df
    df_res[df_res['DESCRIPTION'] == Y, 'ATE'] <- ate['estimate']
    df_res[df_res['DESCRIPTION'] == Y, 'SE'] <- ate['std.err']
    
    print(Y)
    
  }
  
  df_res <- df_res %>%
    mutate(
      CI_up = ATE + qnorm(0.975)*SE,
      CI_low = ATE - qnorm(0.975)*SE
    )
  
  save(df_res, file = file.path('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results', paste('Q3_', res_name, '_ATEs.RData', sep=""), fsep = '\\'))
  
  
  
  
}



# Question (iv) -----------------------------------------------------------

load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\01_Material\\Group 22\\north.rdata")
df_raw <- north
rm(north)

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
    ) #%>% 
    #colnames()

# Get GATEs and related statistics
# GATEs of interest:
# - Unem_X0: Average number of months of reg. unemployment in 10 years before 19X1
#   -> levels: above median/below median
# - sex: Sex (1 male, 0 female)
hist(X$UNEM_X0)
median(X$UNEM_X0, na.rm = TRUE)
#   -> levels: male/female
table(X$sex)
# - Voc_deg: Vocational degree (0: None; 1: below university; 2 university)
#   -> levels: degree/no-degree
table(X$VOC_DEG)
# - nation: Nationality: 1 Local; 2 other European; 3: Asian; 4 African; 5: American
#   -> levels: local/non-local
table(X$nation)
# - SPECIA_C: Unemployed in contact with caseworker with additional resources for ALMP
#   -> levels: yes/no
table(X$SPECIA_C)


GATEs_crf <- function(D1 = c('T1', 'T2'), D2 = c('E1', 'E2'), year_start = 3, limit = 2){
  
  # Create empty list for saving all GATEs over all periods
  list_res <- list()

  
  # Calculate number of periods
  if(limit == "none"){
    limit <- (10 - year_start)*4*2
  }
  
  # Define different outcomes for loop
  Ys <- c(paste('EARNX', rep(year_start:9, each = 4), '_', 1:4, sep = ''), paste('EMPLX', rep(year_start:9, each = 4), '_', 1:4, sep = ''))[1:limit]
  
  # Loop over periods
  for (Y in Ys){
    
    # Load crf
    load(file = file.path(getwd(),  paste0(paste(D1, collapse = ""), 'vs', paste(D2, collapse = ""), '_', Y, '.RData')))
    
    # Extract relevant covariates
    X <- crf$X.orig
    
    # Add covaraites relevant for GATE estimation
    X <- X %>%
      mutate(UNEM_X0_4gate = factor(UNEM_X0>median(UNEM_X0, na.rm = TRUE),
                                    levels = c(TRUE, FALSE),
                                    labels = c('long-term', 'short-term')),
             sex_4gate = factor(sex,
                                levels = c(1, 2),
                                labels = c('female', 'male')),
             VOC_DEG_4gate = factor(ifelse(VOC_DEG == 0, 0, 1),
                                    levels = c(0, 1),
                                    labels = c('no_degree', 'degree')),
             nation_4gate = factor(ifelse(nation == 1, 1, 0),
                                   levels = c(0, 1),
                                   labels = c('foreign', 'local')),
             SPECIA_C_4gate = factor(SPECIA_C,
                                   levels = c(0, 1),
                                   labels = c('no', 'yes')),
      )
    
    # Create empty list for saving results of one period
    df_res <- data.frame(X = rep(NA, X %>% select(contains('4gate')) %>% summarise_all(function(x) length(levels(x))) %>% sum()), 
                         LEVEL = NA, 
                         GATE = NA,
                         SE = NA,
                         CI_low = NA, 
                         CI_up = NA)
    

    counter <- 1
    # Define relevant covariates for GATE calculation
    cov_names <- names(X %>% select(contains('4gate')))
    
    for (i in seq_along(cov_names)) {
      for (j in levels(X[[cov_names[i]]])) {
        tmp <- average_treatment_effect(
          forest = crf,
          target.sample = 'all',
          #subset = replace_na(X[, cov_names[i]] == j, FALSE)[,1],
          subset = (X[, cov_names[i]] == j)[,1]
        )
        df_res$X[[counter]] <- str_remove(cov_names[i], '_4gate')
        df_res$LEVEL[[counter]] <- j
        df_res$GATE[[counter]] <- tmp[['estimate']]
        df_res$SE[[counter]] <- tmp[['std.err']]
        df_res$CI_low[[counter]] <- tmp[['estimate']] - qnorm(0.975) * tmp[['std.err']]
        df_res$CI_up[[counter]] <- tmp[['estimate']] + qnorm(0.975) * tmp[['std.err']]
        counter <- counter + 1
      }
    }
    
    list_res[[Y]] <- df_res
    print(Y)
  }
  
  return(list_res)
  
}



Q4_T1T2vsE1E2 <- GATEs_crf(year_start = 3, limit = "none")
save(Q4_T1T2vsE1E2, file = 'Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_T1T2vsE1E2_GATEs.RData')

lapply(Q4_T1T2vsE1E2, function(x) names(x))

DESCRIPTIONs <- names(Q4_T1T2vsE1E2)
LENGTH <- nrow(Q4_T1T2vsE1E2[[1]])
df_res <- NULL
for (i in seq_along(Q4_T1T2vsE1E2)){
  
  DESCRIPTION <- rep(DESCRIPTIONs[i],LENGTH)
  ESTIMATES <- Q4_T1T2vsE1E2[[i]]
  df_res <- rbind(tibble(DESCRIPTION, ESTIMATES), df_res)
  
}

save(df_res, file = 'Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_T1T2vsE1E2_GATEs.RData')


GATE_plot <- function(df, Y_type, X_type, X_group = "", title = "", y_lab = "GATEs", x_lab = "Quarter after treatment"){
  
  df %>% 
    filter(str_detect(DESCRIPTION, Y_type)) %>% 
    filter(X == X_type) %>% 
    arrange(LEVEL, DESCRIPTION) %>% 
    mutate(PERIOD = rep(1:(as.numeric(nrow(.))/2), 2)) %>% 
    ggplot() + 
    geom_line(aes(x = PERIOD, y = GATE, color=LEVEL)) +
    geom_ribbon(aes(ymin=CI_low,ymax=CI_up, x=PERIOD, fill=LEVEL), alpha=0.3,) +
    geom_hline(yintercept = 0, col = "red", linetype="dashed") +
    scale_x_continuous(labels = 1:28, breaks = 1:28) +
    #facet_wrap(~ LEVEL) +
    labs(title = title, color = X_group, fill = X_group) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_jod
  
  
  
}

GATE_plot(df_res, Y_type = 'EARN', X_type = 'UNEM_X0', X_group = "Prior Unemployment")
  

  


# Question (v) ------------------------------------------------------------
setwd('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\02_Intermediate\\01_PostTreatment')

comb <- list(
  #list(c('T1', 'T2', 'E1', 'E2'), c('No program')),
  #list(c('T1'), c('No program')),
  #list(c('T2'), c('No program')),
  #list(c('E1'), c('No program')),
  #list(c('E2'), c('No program')),
  list(c('T1'), c('E1')),
  list(c('T1'), c('E2')),
  list(c('T2'), c('E1')),
  list(c('T2'), c('E2'))
  #list(c('T1','T2'), c('E1','E2'))
)

year_start <- 2


# Define different outcomes for loop
Ys <- c(paste('EARNX', year_start, '_', 1:3, sep = ''), paste('EMPLX', year_start, '_', 1:3, sep = ''))

for (j in comb){
  
  df_res <- enframe(Ys, value = "DESCRIPTION", name = NULL) %>% 
    mutate(ATE=NA,
           SE=NA) %>% 
    mutate_if(is.logical, as.numeric)
  
  res_name <- sub(pattern = ' ', replacement = '', paste(paste(j[[1]], collapse = ""), 'vs', paste(j[[2]], collapse = ""), sep = ""))
  
  
  for (Y in Ys){
    
    # Load Causal Random Forest for respective period and outcome variable
    # Try several times if loading fails the first time
    attempt <- 0
    while(!exists('crf') && attempt <= 3){
      attempt <- attempt + 1
      try(
        load(file = file.path(getwd(), paste(res_name, '_', Y, '.RData', sep = "")))
      )
    }
    
    
    
    # Estimate from IATEs the respective ATEs and standard errors
    ate <- average_treatment_effect(crf, target.sample = 'all')
    
    # Remove big causal_forest object and wait for a couple of seconds
    rm(crf)
    Sys.sleep(3)
    
    # Attach results to filtered df
    df_res[df_res['DESCRIPTION'] == Y, 'ATE'] <- ate['estimate']
    df_res[df_res['DESCRIPTION'] == Y, 'SE'] <- ate['std.err']
    
    print(Y)
    
  }
  
  df_res <- df_res %>%
    mutate(
      CI_up = ATE + qnorm(0.975)*SE,
      CI_low = ATE - qnorm(0.975)*SE,
      CI_up10 = ATE + qnorm(0.995)*SE,
      CI_low10 = ATE - qnorm(0.995)*SE
    ) %>% 
    bind_rows(tibble(DESCRIPTION = c('EMPLX2_4','EARNX2_4'),
                     ATE = c(NA, NA),
                     SE = c(NA, NA),
                     CI_up = c(NA, NA),
                     CI_low = c(NA, NA),
                     CI_up10 = c(NA, NA),
                     CI_low10 = c(NA, NA)))
  
  save(df_res, file = file.path('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results', paste('Q4_placebo_', res_name, '_ATEs.RData', sep=""), fsep = '\\'))
  
}





## Plots ==================================================================


placebo_plot <- function(df1, df2, title = "", y_lab = "ATE", x_lab = "Quarter prior/after treatment"){
  
  df <- df1 %>% 
    bind_rows(df2) %>% 
    mutate(Y_TYPE = str_sub(DESCRIPTION, 1, 4)) %>% 
    arrange(DESCRIPTION) %>% 
    mutate(PERIOD = rep(1:32,2))

  
  quantile_t <- df %>% 
    select(ATE) %>% 
    as_vector() %>% 
    quantile(., probs = 0.66, na.rm = TRUE)
  
  df %>% 
    mutate(PLACEBO = ifelse(PERIOD <= 4, TRUE, FALSE)) %>% 
    ggplot() + 
    geom_line(aes(x = PERIOD, y = ATE, group=0, col=PLACEBO)) +
    geom_ribbon(aes(ymin=CI_low, ymax=CI_up, x=PERIOD, fill=PLACEBO), alpha=0.4) +
    geom_ribbon(aes(ymin=CI_low10, ymax=CI_up10, x=PERIOD, fill=PLACEBO), alpha=0.3) +
    geom_hline(yintercept = 0, col = "red", linetype="dashed") +
    geom_vline(xintercept = 4, col = "darkgreen") +
    geom_text(aes(x=4, y=0, label='Treatment'), size=3, angle=90, vjust=-0.4, hjust=0) +
    scale_color_manual(values=c("black", "darkgreen")) +
    
    scale_fill_manual(values=c("black", "darkgreen")) +
    facet_wrap(~Y_TYPE, scales = 'free_y') + 
    scale_x_continuous(labels = -3:28, breaks = 1:32) +
    labs(title = title) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_jod +
    theme(legend.position = "none")
  

  
}
setwd("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results")

load(file.path(getwd(), "Q4_placebo_E1vsNoprogram_ATEs.RData"))
df1 <- df_res
load(file.path(getwd(), "Q2_E1vsNoprogram_ATEs.RData"))
df2 <- df_res
placebo_plot(df1, df2)


placebo_plot2 <- function(df, Y_type, title = "", y_lab = "ATE", x_lab = "Quarter prior to treatment"){
  
  quantile_t <- df %>% 
    filter(str_detect(DESCRIPTION, Y_type)) %>%
    select(ATE) %>% 
    as_vector() %>% 
    quantile(., probs = 0.66, na.rm = TRUE)
  
  df %>% 
    filter(str_detect(DESCRIPTION, Y_type)) %>% 
    { if(!("COMPARISON" %in% colnames(df))) 
      mutate(., PERIOD = as.numeric(row.names(.)))
      else .} %>%  
    ggplot() + 
    geom_line(aes(x = PERIOD, y = ATE, group=0)) +
    geom_ribbon(aes(ymin=CI_low, ymax=CI_up, x=PERIOD), alpha=0.3) +
    geom_ribbon(aes(ymin=CI_low10, ymax=CI_up10, x=PERIOD), alpha=0.6) +
    geom_hline(yintercept = 0, col = "red", linetype="dashed") +
    geom_vline(xintercept = 4, col = "darkgreen") +
    geom_text(aes(x=4, y=quantile_t, label='Treatment'), size=3, angle=90, vjust=-0.4, hjust=0) +
    scale_x_continuous(labels = -3:0, breaks = 1:4) +
    {if("COMPARISON" %in% colnames(df)) facet_wrap(~COMPARISON, scales = 'free_x')} + 
    labs(title = title) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_jod +
    theme(legend.position = "none")
  
  
  
}
placebo_plot2(df_res, Y_type = "EARN")
