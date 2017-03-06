#learning the SEM package
library(sem)
library(lavaan)
library(qgraph)
library(semPlot)
library(psych)


#example of SEM estimation in package lavaan
data("PoliticalDemocracy")
PD<-PoliticalDemocracy
head(PD)

model <- '
  # measurement model (latent and observed variables)
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  
  # regression
  dem60 ~ ind60
  dem65~ ind60 + dem60
  
  #residual correlations
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
  
'

  #fit the SEM
  fit <-sem(model, data = PD) 
  
  summary(fit, standardized = T, rsq = T)
  #Reulst:
  #want a greater P-value (counterintuitive)
  #want high r-square

  #check any missing paths: high mi values suggest there is a path missed
  modindices(fit)
  
  #variance table
  vartable(fit)
  
  
  ##visaulize the path diagram
  semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = T, layout = "tree")
  
  
## Now, try the Chinese data
  rm(fit)
  rm(model)
  rm(PD)
  rm(PoliticalDemocracy)
#load the Chinese data
  library(haven)
  individual_data_for_R <- read_dta("C:/Users/Dong/Desktop/Ongoing Project-R/ChineseMinorityClass-MPSA2017/ChineseMinorityClass/individual_data_for_R.dta")

#estimate the model
  uyghur_class<- '
  # measurement model 

  # regressions
  job_class~ edu_1
  income_quantile ~ job_class + edu_1
  current_class_1 ~ job_class + edu_1 + income_quantile 
  
  #residual correlations

 '
  # + age_1 + male_1 + ccp_1 + Uyghur + current_urban_hukou_1 + local_hukou_1
  
  fit <- sem(uyghur_class, data = individual_data_for_R)
  
  summary(fit, standardized = T, fit.measures = T, rsq =T)
  
  modindices(fit)
  
  semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = T, layout = "tree")
  
  
  #label direct and indirect effect
  uyghur_class_1<- '
  # measurement model 
  
  # regressions
  job_class~ a*edu_1
  income_quantile ~ b*job_class + c*edu_1
  current_class_1 ~ d*job_class + e*edu_1 + f*income_quantile 
  
  #direct and indirect effect
  indirect := a*d + b*f + c*f
  direct := e 
  total := e +(a*d + b*f + c*f)

  #residual correlations

  '
  fit_1<- sem(uyghur_class_1, data = individual_data_for_R)
  
  summary(fit_1, standardized = T, fit.measures = T, rsq =T)
  
  semPaths(fit_1, "std", edge.label.cex = 0.5, curvePivot = T, layout = "tree")
  