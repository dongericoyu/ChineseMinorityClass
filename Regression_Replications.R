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

