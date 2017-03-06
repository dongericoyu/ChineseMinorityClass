#learning the SEM package
library(sem)
library(lavaan)
library(qgraph)
library(semPlot)



#example of SEM estimation in package lavaan
data("PoliticalDemocracy")
PD<-PoliticalDemocracy
head(PD)

model <- '
  # measurement model
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

  
  ##visaulize the path diagram
  semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = T, layout = "tree")
  
  
  