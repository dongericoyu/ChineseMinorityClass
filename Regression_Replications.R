#<<label=setup>>==
  if (!require(checkpoint)) install.packages("checkpoint")
checkpoint("2017-03-08")

library(checkpoint)
library(haven)
library(readr)
library(stringr)
library(ggplot2)
library(mi)
library(betareg)
library(truncnorm)
library(lme4)
library(mitools)
library(interplot)
library(dotwhisker)
library(broom)
library(dplyr)
library(purrr)
library(stargazer)

#read data into R
CLDS2014<- read_dta("C:/Users/Dong/Desktop/Ethnic Minority and Public Good-2016/individual_data_for_R.dta")


#[dotwhisker]plotting results from regression models
#basic model sets 1.Retrospective Mobility- All of China – Han/Minority, Han/Uyghur, Han/Zhuang, Uyghur/Zhuang
#1(1) Han v.s. Minority
retro_HM_China <- glm(retro_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Han, data = CLDS2014)

#1(2)Han v.s. Uyghur
retro_HU_China <- glm(retro_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Uyghur_Han, data = CLDS2014)


#1(3)Han v.s. Zhuang
retro_HZ_China <- glm(retro_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 +  Zhuang_Han, data = CLDS2014)


#1(4)Uyghur v.s. Zhuang
retro_UZ_China <- glm(retro_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Uyghur_Zhuang, data = CLDS2014)



#plotting the  coefficients of model

#groupped IVS 
# Define order for predictors that can be grouped
reordered_vars <- c("job_class", "edu_1", "income_quantile", "age_1", "male_1", "ccp_1", "current_urban_hukou_1",
                    "local_hukou_1", "Han", "Uyghur_Han", "Zhuang_Han", "Uyghur_Zhuang")


# Generate a tidy data frame
m1234_df <- rbind(tidy(retro_HM_China) %>% mutate(model = "retro_HM_China"),      # tidy results &
                  tidy(retro_HU_China) %>% mutate(model = "retro_HU_China"),      # add a variable to
                  tidy(retro_HZ_China) %>% mutate(model = "retro_HZ_China"),
                  tidy(retro_UZ_China) %>% mutate(model = "retro_UZ_China")) %>%  # identify model.
  by_2sd(mtcars) %>%                                        # rescale coefficients
  mutate(term = factor(term, levels = reordered_vars)) %>%  # make term a factor &
  group_by(model) %>% arrange(term) %>%                     # reorder
  relabel_predictors(c(job_class = "Occupation_class",                      # relabel predictors
                       edu_1 = "Education", 
                       income_quantile = "Income", 
                       age_1 = "Age", 
                       male_1 = "Male", 
                       ccp_1 = "CCP members",
                       current_urban_hukou_1 = "Urban Hukou",
                       local_hukou_1 = "Local Hukou",
                       Han = "Han", 
                       Uyghur_Han = "Uyghur v.s. Han",
                       Zhuang_Han = "Zhuang v.s. Han",
                       Uyghur_Zhuang = "Uyghur v.s. Zhuang"))

m1234_df


# Save finalized plot to an object 
retro_set1_group<-dwplot(m1234_df, dodge_soze = 1) + 
  relabel_y_axis(c("Occupation_class", "Education", "Income", 
                   "Age", "Male", "CCP members", "Urban Hukou", "Local Hukou", "Han", 
                   "Uyghur v.s. Han", "Zhuang v.s. Han", "Uyghur v.s. Zhuang")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Figure 1. Predicting Retrospective Mobility") +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, 0), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Create list of brackets (label, topmost included predictor, bottommost included predictor)
four_brackets <- list( c("SES", "Occupation_class", "Income"), 
                       c("Demographics", "Age", "CCP members"),
                       c("Hukou Status", "Urban Hukou", "Local Hukou"),
                       c("Ethnicity", "Han", "Uyghur v.s. Zhuang"))

retro_set1_gplot <- retro_set1_group %>% add_brackets(four_brackets)

# to save to file (not run)
grid.arrange(retro_set1_gplot)    # to display

##creating table by stargazer
table1<-stargazer(retro_HM_China, retro_HU_China, retro_HZ_China,retro_UZ_China, title="Table 1. Regression Results of Retrospective Mobility",
                  align=TRUE, dep.var.labels=c("Han v.s. Minority","Han v.s. Uyghur", "Han v.s. Zhuang", "Uyghur v.s. Zhuang"),
                  covariate.labels=c("Occupation_class", "Education", "Income", 
                                     "Age", "Male", "CCP members", "Urban Hukou", "Local Hukou", "Han", 
                                     "Uyghur v.s. Han", "Zhuang v.s. Han", "Uyghur v.s. Zhuang"),
                  omit.stat=c("LL","ser","f"), no.space=TRUE)


#basic model sets 2.Prospective Mobility- All of China – Han/Minority, Han/Uyghur, Han/Zhuang, Uyghur/Zhuang
#1(1) Han v.s. Minority
prosp_HM_China <- glm(prosp_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Han, data = CLDS2014)

#1(2)Han v.s. Uyghur
prosp_HU_China <- glm(prosp_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Uyghur_Han, data = CLDS2014)


#1(3)Han v.s. Zhuang
prosp_HZ_China <- glm(prosp_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 +  Zhuang_Han, data = CLDS2014)


#1(4)Uyghur v.s. Zhuang
prosp_UZ_China <- glm(prosp_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Uyghur_Zhuang, data = CLDS2014)



#plotting the  coefficients of model

#groupped IVS 
# Define order for predictors that can be grouped
reordered_vars_pros<- c("job_class", "edu_1", "income_quantile", "age_1", "male_1", "ccp_1", "current_urban_hukou_1",
                        "local_hukou_1", "Han", "Uyghur_Han", "Zhuang_Han", "Uyghur_Zhuang")


# Generate a tidy data frame
m5678_df <- rbind(tidy(prosp_HM_China) %>% mutate(model = "prosp_HM_China"),      # tidy results &
                  tidy(prosp_HU_China) %>% mutate(model = "prosp_HU_China"),      # add a variable to
                  tidy(prosp_HZ_China) %>% mutate(model = "prosp_HZ_China"),
                  tidy(prosp_UZ_China) %>% mutate(model = "prosp_UZ_China")) %>%  # identify model.
  by_2sd(mtcars) %>%                                        # rescale coefficients
  mutate(term = factor(term, levels = reordered_vars)) %>%  # make term a factor &
  group_by(model) %>% arrange(term) %>%                     # reorder
  relabel_predictors(c(job_class = "Occupation_class",                      # relabel predictors
                       edu_1 = "Education", 
                       income_quantile = "Income", 
                       age_1 = "Age", 
                       male_1 = "Male", 
                       ccp_1 = "CCP members",
                       current_urban_hukou_1 = "Urban Hukou",
                       local_hukou_1 = "Local Hukou",
                       Han = "Han", 
                       Uyghur_Han = "Uyghur v.s. Han",
                       Zhuang_Han = "Zhuang v.s. Han",
                       Uyghur_Zhuang = "Uyghur v.s. Zhuang"))

m5678_df


# Save finalized plot to an object 
prosp_set1_group<-dwplot(m5678_df, dodge_soze = 1) + 
  relabel_y_axis(c("Occupation_class", "Education", "Income", 
                   "Age", "Male", "CCP members", "Urban Hukou", "Local Hukou", "Han", 
                   "Uyghur v.s. Han", "Zhuang v.s. Han", "Uyghur v.s. Zhuang")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Figure 2. Predicting prospspective Mobility") +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, 0), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Create list of brackets (label, topmost included predictor, bottommost included predictor)
four_brackets <- list( c("SES", "Occupation_class", "Income"), 
                       c("Demographics", "Age", "CCP members"),
                       c("Hukou Status", "Urban Hukou", "Local Hukou"),
                       c("Ethnicity", "Han", "Uyghur v.s. Zhuang"))

prosp_set1_gplot <- prosp_set1_group %>% add_brackets(four_brackets)

# to save to file (not run)
grid.arrange(prosp_set1_gplot)    # to display

#creating table
table2<-stargazer(prosp_HM_China, prosp_HU_China, prosp_HZ_China,prosp_UZ_China, title="Regression Results of Prospective Mobility",
                  align=TRUE, dep.var.labels=c("Han v.s. Minority","Han v.s. Uyghur", "Han v.s. Zhuang", "Uyghur v.s. Zhuang"),
                  covariate.labels=c("Occupation_class", "Education", "Income", 
                                     "Age", "Male", "CCP members", "Urban Hukou", "Local Hukou", "Han", 
                                     "Uyghur v.s. Han", "Zhuang v.s. Han", "Uyghur v.s. Zhuang"),
                  omit.stat=c("LL","ser","f"), no.space=TRUE)



#basic model sets 3.Retrospective and Prospective Mobility- ii.	Xinjiang – just Han/Uyghur (Wu & Song 2014 check) iii.	Guangxi – just Zhuang/Han 

#3(1)Han v.s. Uyghur - retros
retro_HU_XJ <- glm(retro_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Uyghur_Han + Xinjiang, data = CLDS2014)
#3(2)Han v.s. Uyghur - prosp
prosp_HU_XJ <- glm(prosp_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Uyghur_Han + Xinjiang, data = CLDS2014)


#3(3)Han v.s. Zhuang - retros
retro_HZ_GX <- glm(retro_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Zhuang_Han + Guangxi, data = CLDS2014)
#3(4)Han v.s. Zhuang - prosp
prosp_HZ_GX <- glm(prosp_mobility_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Zhuang_Han + Guangxi, data = CLDS2014)


#plotting the  coefficients of model

#groupped IVS 
# Define order for predictors that can be grouped
reordered_vars_set3<- c("job_class", "edu_1", "income_quantile", "age_1", "male_1", "ccp_1", "current_urban_hukou_1",
                        "local_hukou_1","Uyghur_Han", "Xinjiang","Zhuang_Han","Guangxi" )


# Generate a tidy data frame
mset3_df <- rbind(tidy(retro_HU_XJ) %>% mutate(model = "retro_HU_XJ"),      # tidy results &
                  tidy(prosp_HU_XJ) %>% mutate(model = "prosp_HU_XJ"),      # add a variable to
                  tidy(retro_HZ_GX) %>% mutate(model = "retro_HZ_GX"),
                  tidy(prosp_HZ_GX) %>% mutate(model = "prosp_HZ_GX")) %>%  # identify model.
  by_2sd(mtcars) %>%                                        # rescale coefficients
  mutate(term = factor(term, levels = reordered_vars_set3)) %>%  # make term a factor &
  group_by(model) %>% arrange(term) %>%                     # reorder
  relabel_predictors(c(job_class = "Occupation_class",                      # relabel predictors
                       edu_1 = "Education", 
                       income_quantile = "Income", 
                       age_1 = "Age", 
                       male_1 = "Male", 
                       ccp_1 = "CCP members",
                       current_urban_hukou_1 = "Urban Hukou",
                       local_hukou_1 = "Local Hukou",
                       Uyghur_Han = "Uyghur v.s. Han",
                       Xinjiang = "Xinjiang",
                       Zhuang_Han = "Zhuang v.s. Han",
                       Guangxi = "Guangxi"))

mset3_df


# Save finalized plot to an object 
set3_group<-dwplot(mset3_df, dodge_soze = 1) + 
  relabel_y_axis(c("Occupation_class", "Education", "Income", 
                   "Age", "Male", "CCP members", "Urban Hukou", "Local Hukou",
                   "Uyghur v.s. Han", "Xinjiang", "Zhuang v.s. Han", "Guangxi")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Figure 3. Uyghur and Zhuang comparing to Han, with Regional Control") +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Create list of brackets (label, topmost included predictor, bottommost included predictor)
four_brackets_set3 <- list( c("SES", "Occupation_class", "Income"), 
                            c("Demographics", "Age", "CCP members"),
                            c("Hukou Status", "Urban Hukou", "Local Hukou"),
                            c("Ethnicity and Region", "Uyghur v.s. Han", "Guangxi"))

set3_gplot <- set3_group %>% add_brackets(four_brackets_set3)

# to save to file (not run)
grid.arrange(set3_gplot)    # to display

#creating table
table3<-stargazer(retro_HU_XJ, prosp_HU_XJ, retro_HZ_GX, prosp_HZ_GX, title="Regression Results of Uyghur and Zhuang comparing to Han",
                  align=TRUE, dep.var.labels=c("Han v.s. Uyghur(Retro)","Han v.s. Uyghur(Prosp) ", "Han v.s. Zhuang(Retro)", "Uyghur v.s. Zhuang(Prosp"),
                  covariate.labels=c("Occupation_class", "Education", "Income", 
                                     "Age", "Male", "CCP members", "Urban Hukou", "Local Hukou", 
                                     "Uyghur v.s. Han", "Xinjiang", "Zhuang v.s. Han", "Guangxi"),
                  omit.stat=c("LL","ser","f"), no.space=TRUE)


#basic model sets 4.Perception of Social Class

#4(1)Han v.s. Uyghur - class
class_HM <- glm(current_class_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Han, data = CLDS2014)
#4(2)Han v.s. Uyghur - class
class_HU <- glm(current_class_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Uyghur_Han, data = CLDS2014)


#4(3)Han v.s. Zhuang - class
class_HZ <- glm(current_class_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Zhuang_Han, data = CLDS2014)
#4(4)Han v.s. Zhuang - class
class_UZ <- glm(current_class_1 ~ job_class + edu_1 + income_quantile + age_1 + male_1 + ccp_1 + current_urban_hukou_1 + local_hukou_1 + Uyghur_Zhuang, data = CLDS2014)


#plotting the  coefficients of model

#groupped IVS 
# Define order for predictors that can be grouped
reordered_vars_set4<- c("job_class", "edu_1", "income_quantile", "age_1", "male_1", "ccp_1", "current_urban_hukou_1",
                        "local_hukou_1", "Han", "Uyghur_Han", "Zhuang_Han", "Uyghur_Zhuang")


# Generate a tidy data frame
mset4_df <- rbind(tidy(class_HM) %>% mutate(model = "class_HM"),      # tidy results &
                  tidy(class_HU) %>% mutate(model = "class_HU"),      # add a variable to
                  tidy(class_HZ) %>% mutate(model = "class_HZ"),
                  tidy(class_UZ) %>% mutate(model = "class_UZ")) %>%  # identify model.
  by_2sd(mtcars) %>%                                        # rescale coefficients
  mutate(term = factor(term, levels = reordered_vars_set4)) %>%  # make term a factor &
  group_by(model) %>% arrange(term) %>%                     # reorder
  relabel_predictors(c(job_class = "Occupation_class",                      # relabel predictors
                       edu_1 = "Education", 
                       income_quantile = "Income", 
                       age_1 = "Age", 
                       male_1 = "Male", 
                       ccp_1 = "CCP members",
                       current_urban_hukou_1 = "Urban Hukou",
                       local_hukou_1 = "Local Hukou",
                       Han = "Han", 
                       Uyghur_Han = "Uyghur v.s. Han",
                       Zhuang_Han = "Zhuang v.s. Han",
                       Uyghur_Zhuang = "Uyghur v.s. Zhuang"))

mset4_df


# Save finalized plot to an object 
class_set4_group<-dwplot(mset4_df, dodge_soze = 1) + 
  relabel_y_axis(c("Occupation_class", "Education", "Income", 
                   "Age", "Male", "CCP members", "Urban Hukou", "Local Hukou", "Han", 
                   "Uyghur v.s. Han", "Zhuang v.s. Han", "Uyghur v.s. Zhuang")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Figure 4. Self-perception of Social Class") +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Create list of brackets (label, topmost included predictor, bottommost included predictor)
four_brackets <- list( c("SES", "Occupation_class", "Income"), 
                       c("Demographics", "Age", "CCP members"),
                       c("Hukou Status", "Urban Hukou", "Local Hukou"),
                       c("Ethnicity", "Han", "Uyghur v.s. Zhuang"))

class_set4_gplot <- class_set4_group %>% add_brackets(four_brackets)

# to save to file (not run)
grid.arrange(class_set4_gplot)    # to display

#creating table
table4<-stargazer(class_HM, class_HU, class_HZ, class_UZ, title="Regression Results of Self-Perception of Social Class",
                  align=TRUE, dep.var.labels=c("Han v.s. Minorities","Han v.s. Uyghur", "Han v.s. Zhuang", "Uyghur v.s. Zhuang"),
                  covariate.labels=c("Occupation_class", "Education", "Income", 
                                     "Age", "Male", "CCP members", "Urban Hukou", "Local Hukou", "Han", 
                                     "Uyghur v.s. Han", "Zhuang v.s. Han", "Uyghur v.s. Zhuang"),
                  omit.stat=c("LL","ser","f"), no.space=TRUE)


