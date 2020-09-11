setwd('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\05_Presentation')

#install.packages('tidyverse')
library(tidyverse)
#install.packages('grf')
library(grf)
source(file = "Q:\\Meine Bibliotheken\\Research\\06_Environments\\ggplot_template.R")


# Functions ---------------------------------------------------------------

ATE_plot <- function(df, Y_type, title = "", y_lab = "ATE", x_lab = "Quarter after treatment", line_col = "", return_data = FALSE){
  
  
  df_name <- deparse(substitute(df))
  
  df_res <- df %>% 
    filter(str_detect(DESCRIPTION, Y_type)) %>% 
    mutate(PERIOD = as.numeric(row.names(.))) %>% 
    mutate(COMPARISON = df_name)
  
  
  
  
  fig_res <- df %>% 
    filter(str_detect(DESCRIPTION, Y_type)) %>% 
    { if(!("COMPARISON" %in% colnames(df))) 
      mutate(., PERIOD = as.numeric(row.names(.)))
      else .} %>%    
    ggplot() + 
    geom_line(aes(x = PERIOD, y = ATE, group=0)) +
    geom_ribbon(aes(ymin=CI_low,ymax=CI_up,x=PERIOD),alpha=0.3) +
    geom_hline(yintercept = 0, col = "red", linetype="dashed") +
    scale_x_continuous(labels = 1:28, breaks = 1:28) +
    {if("COMPARISON" %in% colnames(df)) facet_wrap(~COMPARISON, scales = 'free_x')} + 
    labs(title = title) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_jod
  
  if(return_data){
    return(df_res)
  }
  
  else fig_res
  
}

GATE_plot <- function(df, Y_type, X_type, X_group = "", title = "", y_lab = "GATE", x_lab = "Quarter after treatment"){
  
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
    geom_text(aes(x=4, y=0, label='Program start'), size=2, angle=90, vjust=-0.4, hjust=0) +
    scale_color_manual(values=c("black", "darkgreen")) +
    
    scale_fill_manual(values=c("black", "darkgreen")) +
    facet_wrap(~Y_TYPE, scales = 'free_y', nrow = 2) + 
    scale_x_continuous(labels = -3:28, breaks = 1:32) +
    labs(title = title) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_jod +
    theme(legend.position = "none")
  
  
  
}


# Question (ii) -----------------------------------------------------------

# Are the programmes effective in raising employment and earnings of their participants? 
# If they do so, by how much?


#* Any Program against No Program ------------------------------------------
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_ATEs.RData")
ALLvsNO <- df_res

ATE_plot(ALLvsNO, Y_type = 'EMPL', title = "Average Treatment Effect on employment\nof any program against no program")
ggsave(
  filename = 'Q2_ATEs_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

ATE_plot(ALLvsNO, Y_type = 'EARN', title = "Average Treatment Effect on earnings\nof any program against no program")
ggsave(
  filename = 'Q2_ATEs_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


#* T1 against No Program ------------------------------------------
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_T1vsNoprogram_ATEs.RData")
T1vsNO <- df_res

ATE_plot(T1vsNO, Y_type = 'EMPL', title = "Average Treatment Effect on employment of T1 against no program")
ggsave(
  filename = 'Q2_T1vsNo_ATEs_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

ATE_plot(T1vsNO, Y_type = 'EARN', title = "Average Treatment Effect on earnings of T1 against no program")
ggsave(
  filename = 'Q2_T1vsNo_ATEs_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


#* T2 against No Program ------------------------------------------
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_T2vsNoprogram_ATEs.RData")
T2vsNO <- df_res

ATE_plot(T2vsNO, Y_type = 'EMPL', title = "Average Treatment Effect on employment of T2 against no program")
ggsave(
  filename = 'Q2_T2vsNo_ATEs_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

ATE_plot(T2vsNO, Y_type = 'EARN', title = "Average Treatment Effect on earnings of T2 against no program")
ggsave(
  filename = 'Q2_T2vsNo_ATEs_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


#* E1 against No Program ------------------------------------------
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_E1vsNoprogram_ATEs.RData")
E1vsNO <- df_res

ATE_plot(E1vsNO, Y_type = 'EMPL', title = "Average Treatment Effect on employment of E1 against no program")
ggsave(
  filename = 'Q2_E1vsNo_ATEs_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

ATE_plot(E1vsNO, Y_type = 'EARN', title = "Average Treatment Effect on earnings of E1 against no program")
ggsave(
  filename = 'Q2_E1vsNo_ATEs_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


#* E2 against No Program ------------------------------------------
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_E2vsNoprogram_ATEs.RData")
E2vsNO <- df_res

ATE_plot(E2vsNO, Y_type = 'EMPL', title = "Average Treatment Effect on employment of E2 against no program")
ggsave(
  filename = 'Q2_E2vsNo_ATEs_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

ATE_plot(E2vsNO, Y_type = 'EARN', title = "Average Treatment Effect on earnings of E2 against no program")
ggsave(
  filename = 'Q2_E2vsNo_ATEs_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)




#* Individual programs against each other ----------------------------------
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_T1vsNoprogram_ATEs2.RData")
T1vsNo <- df_res
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_T2vsNoprogram_ATEs2.RData")
T2vsNo <- df_res
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_E1vsNoprogram_ATEs2.RData")
E1vsNo <- df_res
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_E2vsNoprogram_ATEs2.RData")
E2vsNo <- df_res
df_comp <- bind_rows(ATE_plot(T1vsNo, Y_type = 'EMPL', return_data = TRUE),
                     ATE_plot(T2vsNo, Y_type = 'EMPL', return_data = TRUE), 
                     ATE_plot(E1vsNo, Y_type = 'EMPL', return_data = TRUE), 
                     ATE_plot(E2vsNo, Y_type = 'EMPL', return_data = TRUE))

ATE_plot(df_comp, Y_type = 'EMPL', title = "Average Treatment Effect on employment of individual programs against non-participants")
ggsave(
  filename = 'Q2_T1-E1-T2-E2vsNo_ATEs_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


df_comp <- bind_rows(ATE_plot(T1vsNo, Y_type = 'EARN', return_data = TRUE),
                     ATE_plot(T2vsNo, Y_type = 'EARN', return_data = TRUE), 
                     ATE_plot(E1vsNo, Y_type = 'EARN', return_data = TRUE), 
                     ATE_plot(E2vsNo, Y_type = 'EARN', return_data = TRUE))
ATE_plot(df_comp, Y_type = 'EARN', title = "Average Treatment Effect on earnings of individual programs against each other")
ggsave(
  filename = 'Q3_T1-E1-T2-E2vsNo_ATEs_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)




# Question (iii) ----------------------------------------------------------

# Is there evidence of programme heterogeneity? 
# Is there a superior programme?


#* Training against employment ---------------------------------------------
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T1T2vsE1E2_ATEs2.RData")
T1T2vsE1E2 <- df_res

ATE_plot(T1T2vsE1E2, Y_type = 'EMPL', title = "Average Treatment Effect on employment\nof training against employment programs")
ggsave(
  filename = 'Q3_T1T2vsE1E2_ATEs_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

ATE_plot(T1T2vsE1E2, Y_type = 'EARN', title = "Average Treatment Effect on earnings\nof training against employment programs")
ggsave(
  filename = 'Q3_T1T2vsE1E2_ATEs_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)



#* Individual programs against each other ----------------------------------
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T1vsE1_ATEs.RData")
T1vsE1 <- df_res
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T1vsE2_ATEs.RData")
T1vsE2 <- df_res
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T2vsE1_ATEs.RData")
T2vsE1 <- df_res
load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T2vsE2_ATEs.RData")
T2vsE2 <- df_res
df_comp <- bind_rows(ATE_plot(T1vsE1, Y_type = 'EMPL', return_data = TRUE),
                     ATE_plot(T2vsE1, Y_type = 'EMPL', return_data = TRUE), 
                     ATE_plot(T1vsE2, Y_type = 'EMPL', return_data = TRUE), 
                     ATE_plot(T2vsE2, Y_type = 'EMPL', return_data = TRUE))

ATE_plot(df_comp, Y_type = 'EMPL', title = "Average Treatment Effect on employment of individual programs against each other")
ggsave(
  filename = 'Q3_T1vsE1vsT2vsE2_ATEs_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


df_comp <- bind_rows(ATE_plot(T1vsE1, Y_type = 'EARN', return_data = TRUE),
                     ATE_plot(T2vsE1, Y_type = 'EARN', return_data = TRUE), 
                     ATE_plot(T1vsE2, Y_type = 'EARN', return_data = TRUE), 
                     ATE_plot(T2vsE2, Y_type = 'EARN', return_data = TRUE))
ATE_plot(df_comp, Y_type = 'EARN', title = "Average Treatment Effect on earnings of individual programs against each other")
ggsave(
  filename = 'Q3_T1vsE1vsT2vsE2_ATEs_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)



# Question (iv) -----------------------------------------------------------

# Is there evidence of effect heterogeneity? 
# Are the right people been allocated to the right programmes?

load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_T1T2vsE1E2_GATEs.RData')
#* Prior Unemployment ------------------------------------------------------
GATE_plot(df_res, Y_type = 'EMPL', X_type = 'UNEM_X0', X_group = "Prior Unemployment", title = "Group Average Treatment Effect on employment\nof training against employment programs")
ggsave(
  filename = 'Q4_T1T2vsE1E2_GATEs_UNEMXO_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)
GATE_plot(df_res, Y_type = 'EARN', X_type = 'UNEM_X0', X_group = "Prior Unemployment", title = "Group Average Treatment Effect on earnings\nof training against employment programs")
ggsave(
  filename = 'Q4_T1T2vsE1E2_GATEs_UNEMXO_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

#* Sex ---------------------------------------------------------------------
GATE_plot(df_res, Y_type = 'EMPL', X_type = 'sex', X_group = "Sex", title = "Group Average Treatment Effect on employment\nof training against employment programs")
ggsave(
  filename = 'Q4_T1T2vsE1E2_GATEs_SEX_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

GATE_plot(df_res, Y_type = 'EARN', X_type = 'sex', X_group = "Sex", title = "Group Average Treatment Effect on earnings\nof training against employment programs")
ggsave(
  filename = 'Q4_T1T2vsE1E2_GATEs_SEX_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)



#* Nationality -------------------------------------------------------------
GATE_plot(df_res, Y_type = 'EMPL', X_type = 'nation', X_group = "Nationality", title = "Group Average Treatment Effect on employment\nof training against employment programs")
ggsave(
  filename = 'Q4_T1T2vsE1E2_GATEs_NATION_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

GATE_plot(df_res, Y_type = 'EARN', X_type = 'nation', X_group = "Nationality", title = "Group Average Treatment Effect on earnings\nof training against employment programs")
ggsave(
  filename = 'Q4_T1T2vsE1E2_GATEs_NATION_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


#* Vocational degree -------------------------------------------------------
GATE_plot(df_res, Y_type = 'EMPL', X_type = 'VOC_DEG', X_group = "Vocational degree", title = "Group Average Treatment Effect on employment\nof training against employment programs")
ggsave(
  filename = 'Q4_T1T2vsE1E2_GATEs_DEGREE_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

GATE_plot(df_res, Y_type = 'EARN', X_type = 'VOC_DEG', X_group = "Vocational degree", title = "Group Average Treatment Effect on earnings\nof training against employment programs")
ggsave(
  filename = 'Q4_T1T2vsE1E2_GATEs_DEGREE_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 5.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)







# Question (v) ------------------------------------------------------------

# Are your findings robust to common specification issues (sensitivity analysis)?


#* Training against Employment ----------------------------------------------
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_placebo_T1T2vsE1E2_ATEs.RData')
df1 <- df_res
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T1T2vsE1E2_ATEs.RData')
df2 <- df_res

placebo_plot(df1, df2, title = 'Placebo analysis: training against employment')
ggsave(
  filename = 'Q5_T1T2vsE1E2.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


#* T1 against E1 -----------------------------------------------------------
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_placebo_T1vsE1_ATEs.RData')
df1 <- df_res
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T1vsE1_ATEs.RData')
df2 <- df_res

placebo_plot(df1, df2, title = 'Placebo analysis: T1 against E1')
ggsave(
  filename = 'Q5_T1vsE1.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

#* T1 against E2 -----------------------------------------------------------
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_placebo_T1vsE2_ATEs.RData')
df1 <- df_res
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T1vsE2_ATEs.RData')
df2 <- df_res

placebo_plot(df1, df2, title = 'Placebo analysis: T1 against E2')
ggsave(
  filename = 'Q5_T1vsE2.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

#* T2 against E1 -----------------------------------------------------------
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_placebo_T2vsE1_ATEs.RData')
df1 <- df_res
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T2vsE1_ATEs.RData')
df2 <- df_res

placebo_plot(df1, df2, title = 'Placebo analysis: T2 against E1')
ggsave(
  filename = 'Q5_T2vsE1.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)

#* T2 against E2 -----------------------------------------------------------
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_placebo_T2vsE2_ATEs.RData')
df1 <- df_res
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q3_T2vsE2_ATEs.RData')
df2 <- df_res

placebo_plot(df1, df2, title = 'Placebo analysis: T2 against E2')
ggsave(
  filename = 'Q5_T2vsE2.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


#* T1 against Noprogram -----------------------------------------------------------
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_placebo_T1vsNoprogram_ATEs.RData')
df1 <- df_res
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_T1vsNoprogram_ATEs.RData')
df2 <- df_res

placebo_plot(df1, df2, title = 'Placebo analysis: T1 against No program')
ggsave(
  filename = 'Q5_T1vsNo.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)



#* T2 against Noprogram -----------------------------------------------------------
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_placebo_T2vsNoprogram_ATEs.RData')
df1 <- df_res
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_T2vsNoprogram_ATEs.RData')
df2 <- df_res

placebo_plot(df1, df2, title = 'Placebo analysis: T2 against No program')
ggsave(
  filename = 'Q5_T2vsNo.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)



#* E1 against Noprogram -----------------------------------------------------------
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_placebo_E1vsNoprogram_ATEs.RData')
df1 <- df_res
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_E1vsNoprogram_ATEs.RData')
df2 <- df_res

placebo_plot(df1, df2, title = 'Placebo analysis: E1 against No program')
ggsave(
  filename = 'Q5_E1vsNo.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)



#* E2 against Noprogram -----------------------------------------------------------
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q4_placebo_E2vsNoprogram_ATEs.RData')
df1 <- df_res
load('Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\03_Results\\Q2_E2vsNoprogram_ATEs.RData')
df2 <- df_res

placebo_plot(df1, df2, title = 'Placebo analysis: E2 against No program')
ggsave(
  filename = 'Q5_E2vsNo.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


# Additional Plots --------------------------------------------------------

#* Variable Importance -----------------------------------------------------

#** Employment --------------------------------------------------------------
load("Q:/Meine Bibliotheken/Research/01_Promotion/01_Courses/03_CausalMachineLearning/04_Exam/03_Results/02_Intermediate/01_PostTreatment/01_SanityCheck/T1T2E1E2vsNoprogram_EMPLX9_4.RData")

colnames(crf$X.orig) %>% 
  enframe() %>% 
  mutate(VAR_IMP = variable_importance(crf)) %>% 
  ggplot() + 
  geom_col(aes(x=value, y=VAR_IMP[,1])) +
  labs(title = 'Importance of covariates in explaining employment effect heterogeneity') +
  xlab('Covariates') +
  ylab('Variable importance') +
  theme_jod
ggsave(
  filename = 'Appendix_VARIMP_T1T2E1E2vsNo_EMPL.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)


#** Earnings ----------------------------------------------------------------
load("Q:/Meine Bibliotheken/Research/01_Promotion/01_Courses/03_CausalMachineLearning/04_Exam/03_Results/02_Intermediate/01_PostTreatment/01_SanityCheck/T1T2E1E2vsNoprogram_EARNX9_4.RData")

colnames(crf$X.orig) %>% 
  enframe() %>% 
  mutate(VAR_IMP = variable_importance(crf)) %>% 
  ggplot() + 
  geom_col(aes(x=value, y=VAR_IMP[,1])) +
  labs(title = 'Importance of covariates in explaining earning effect heterogeneity') +
  xlab('Covariates') +
  ylab('Variable importance') +
  theme_jod
ggsave(
  filename = 'Appendix_VARIMP_T1T2E1E2vsNo_EARN.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 9.5,
  height = 4,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)




#* Employment Variable -----------------------------------------------------

load("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\01_Courses\\03_CausalMachineLearning\\04_Exam\\01_Material\\Group 22\\north.rdata")

# Define data as tibble
df_raw <- north %>% 
  as_tibble()

# Remove original dataframe
rm(north)

# Drop duplicated rows
df_raw <- df_raw[!duplicated(df_raw),]

# Redfine employment variable
df_raw <- df_raw %>% 
  mutate_at(.vars = vars(contains('EMPL')), .funs = funs(case_when(. == 1 ~ 'employed',
                                                                   . %in% c(2,3) ~ 'not employed')))


give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}


df_raw %>% 
  select(contains('EMPL'), ptype) %>% 
  mutate(INDIVIDUAL = row.names(.)) %>% 
  gather(PERIOD, EMPLOYMENT, EMPLX1_1:EMPLX9_4, na.rm = FALSE) %>% 
  bind_cols(df_raw %>% 
              select(contains('EARNX')) %>% 
              mutate(INDIVIDUAL = row.names(.)) %>% 
              gather(PERIOD, EARNINGS, EARNX1_1:EARNX9_4, na.rm = FALSE)) %>% 
  mutate(PERIOD = str_extract(PERIOD, regex('X(.){3,4}$'))) %>% 
  select(INDIVIDUAL, PERIOD, ptype, EMPLOYMENT, EARNINGS) %>% 
  mutate(EMPLOYMENT = as.factor(EMPLOYMENT),
         TREATMENT = as.factor(ifelse(ptype==0, 'Non-participant', 'Participant'))) %>% 
  ggplot(aes(x = EMPLOYMENT, y = EARNINGS, fill=TREATMENT, color=TREATMENT)) +
  geom_boxplot(alpha = 0.5) + 
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75), size = 2, color = 'black') +
  facet_wrap(~PERIOD) +
  theme_jod
ggsave(
  filename = 'Appendix_OUTCOMES.jpeg',
  plot = last_plot(),
  device = 'jpeg',
  path = getwd(),
  scale = 1,
  width = 14.5,
  height = 7,
  units = 'in',
  dpi = 320,
  limitsize = TRUE,
)



