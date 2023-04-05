#Davis's Code
rm(list=ls())
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr
updateR()
install.packages('caret', dependencies = TRUE)
install.packages('gtable', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('gower', dependencies = TRUE)
library(lattice)
library(ggplot2)
library(tibble)
library(vctrs)
library(caret)

version
library(broom)
library(lubridate)
library(tidyverse)
library(gtsummary)
library(broom)
library(visdat)
library(rstatix)
library(reshape2)
library(pheatmap)
pacman::p_load(shiny, flextable, rio, sjmisc, sjlabelled, rpart.plot, caret)
library(shiny)
library(flextable)
library(officer)
library(dplyr)
library(GGally)
library(rio)
library(janitor)
library("RColorBrewer")


setwd("D:/Proposal writing/Data folder")
the_data <- read.csv("clean data.csv", 
                     header=TRUE, 
                     sep=",",
                     stringsAsFactors = TRUE)

str(the_data)
View(the_data)
summary(the_data)
names(the_data)

##selecting only numeric variables to help in running the correlation matrix
data_nm <- select_if(the_data, is.numeric)
data_num <- within(data_nm, rm(snum, time_infection))
names(data_num)

##generating data values for all the categorical variables
the_data1 <- the_data %>%
  mutate(cat_sx_outcome = case_when(sx_outcome == c("Failure")~0, 
                                    sx_outcome==c("Success")~ 1)) %>%
  mutate(gender= case_when(sex==c("F")~0,
                           sex==c("M")~1)) %>%
  mutate(c_section= case_when(csection==c("C-section")~0,
                              csection==c("SVD")~1)) %>%
  mutate(birthseason= case_when(season_birth==c("Dry")~0,
                                season_birth==c("Rainy")~1)) %>%
  mutate(Region= case_when(region==c("Central")~0,
                           region==c("Northern")~1,
                           region==c("Eastern")~2,
                           region==c("Western")~3)) %>%
  mutate(birthplace= case_when(plac_b==c("Health Centre")~0,
                               plac_b==c("Clinic")~1,
                               plac_b==c("Home")~2,
                               plac_b==c("Hospital")~3)) %>%
  mutate(gestation= case_when(gest==c("Preterm")~0,
                              gest==c("Term")~1,
                              gest==c("Unknown")~2)) %>%
  mutate(HC_etiology= case_when(hydro_etiology==c("Hydranencephaly")~0,
                                hydro_etiology==c("Cyst")~1,
                                hydro_etiology==c("Anatomic Malformation")~2,
                                hydro_etiology==c("Schizencephaly")~3,
                                hydro_etiology==c("Aqueduct Stenosis")~4,
                                hydro_etiology==c("Dandy-Walker")~5,
                                hydro_etiology==c("PHH")~6,
                                hydro_etiology==c("Tumor")~7,
                                hydro_etiology==c("Congenital Hydrocephalus")~8,
                                hydro_etiology==c("Holoprosencephaly")~9,
                                hydro_etiology==c("PIH")~10)) %>%
  mutate(hydro_group= case_when(hydro_gp==c("NPIH")~0,
                                hydro_gp==c("PIH")~1))

names(the_data1)
table(the_data1$hydro_group)

#adding value labels to data
the_data1 <- the_data1 %>% add_labels(gender, labels=c("Female"=0, "Male"=1)) %>%
  add_labels(cat_sx_outcome, labels = c("Failure"=0, "Success"=1)) %>%
  add_labels(csection, labels = c("C-section"=0, "SVD"=1)) %>%
  add_labels(Region, labels = c("Central"=0, "Northern"=1, "Eastern"=2, "Western"=3)) %>%
  add_labels(birthplace, labels = c("Health Centre"=0, "Clinic"=1, "Home"=2, "Hospital"=3)) %>%
  add_labels(gestation, labels = c("Preterm"=0, "Term"=1, "Unknown"=2)) %>%
  add_labels(hydro_group, labels = c("NPIH"=0, "PIH"=1)) %>%
  add_labels(HC_etiology, labels = c("Hydranencephaly"=0, "Cyst"=1, "Anatomic Malformation"=2, "Schizencephaly"=3, "Aqueduct Stenosis"=4, "Dandy-Walker"=5, "PHH"=6, "Tumor"=7, "Congenital Hydrocephalus"=8, "Holoprosencephaly"=9, "PIH"=10))


#getting the value labels
get_labels(the_data1$gender)
get_labels(the_data1$birthplace)
get_labels(the_data1$hydro_group)

ncol(the_data1)
nrow(the_data1)

#dropping data variables that have been transformed
the_data2 <- subset(the_data1, select= -c(hydro_gp, sx_outcome, season_birth, sex, csection, place_of_delivery, time_infection, region, plac_b, gest, hydro_etiology))

##You need rownames here
kk<-the_data1$snum
ncol(the_data1)
rownames(the_data2)<-kk
View(kk)

print(head(the_data2))
names(the_data2)


#univariate plots
hist(the_data1$child_weight, main = "Child's weight", xlab = "weight", ylab = "count", col="red")
picking<-the_data1[, 5:19]
boxplot(picking)
#dropping useless data frames
rm(data_nm, picking)

##bivariate plots showing distribution
plot(the_data1$region, the_data1$csf_vol)
plot(the_data1$time_admission, the_data1$csf_vol)

ggplot(the_data1, aes(brain_vol, child_weight)) + geom_point(aes(color=cat_sx_outcome, size=time_admission))

print(head(the_data1))


###Note if you are set on using this on everything you just need to change everything to 0 and 1 for categorical


##You need to make sure you do this
#i <- sapply(the_data2, is.factor)
#the_data1[i] <- lapply(the_data2[i], as.character)

#ia <- sapply(the_data2, is.integer)
#the_data1[ia] <- lapply(the_data2[ia], as.numeric)

##correction matrix for all the numeric variables
#Weid odd function is not automatically detecting
#method1
cor.mat <- data_num %>% cor_mat()
cor.mat
cor_pvalues <- cor.mat %>% cor_get_pval() #getting significant levels
cor_pvalues
View(cor_pvalues)

cor.mat %>%
  pull_lower_triangle() %>%
  cor_plot(label = FALSE)
View(cor.mat)

#correlation matrix plots
cor_plot(cor.mat)
cor_plot(cor_pvalues, method = "number",
         label = TRUE,
         insignificant = "blank")

##drawing a heatmap of correlation values and p-values respectively
pheatmap(as.matrix(cor.mat[,-1]), display_numbers=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, labels_row = c("time_headincrease", "time_admission", "head_circum", "child_weigh", "wbc", "brain_vol", "csf_vol"))

pheatmap(as.matrix(cor_pvalues[,-1]), display_numbers=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, labels_row = c("time_headincrease", "time_admission", "head_circum", "child_weigh", "wbc", "brain_vol", "csf_vol"))


names(data_num)

##LOGISTIC REGRESSION
names(the_data2)

#checking missing values
visdat::vis_miss(the_data2) #i.e no missingness observed

#categorical variables

##obtaining p-values and R squared using ranked linear regression method below for all the variables
#cat_sx_outcome
#using ranked linear regression to obtain p_values and R squared



table(the_data2$gest)
lm(Score ~ cat_sx_outcome, data = the_data2) %>% summary()
lm(Score ~ HC_etiology, data = the_data2) %>% summary()
lm(Score ~ birthplace, data = the_data2) %>% summary()
lm(Score ~ gestation, data = the_data2) %>% summary()
lm(Score ~ gender, data = the_data2) %>% summary()
lm(Score ~ c_section, data = the_data2) %>% summary()
lm(Score ~ Region, data = the_data2) %>% summary()
lm(Score ~ birthseason, data = the_data2) %>% summary()
lm(Score ~ time_headincrease, data = the_data2) %>% summary()
lm(Score ~ child_weight, data = the_data2) %>% summary()
lm(Score ~ csf_vol, data = the_data2) %>% summary()
lm(Score ~ brain_vol, data = the_data2) %>% summary()
lm(Score ~ time_admission, data = the_data2) %>% summary()
lm(Score ~ wbc, data = the_data2) %>% summary()
lm(Score ~ head_circum, data = the_data2) %>% summary()

lm(rank(hydro_group) ~ cat_sx_outcome, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ birthplace, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ gestation, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ gender, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ c_section, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ Region, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ birthseason, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ child_weight, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ time_admission, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ wbc, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ head_circum, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ Score, data = the_data2) %>% summary()
lm(rank(hydro_group) ~ hydro_group, data = the_data2) %>% summary()


lm(rank(cat_sx_outcome) ~ cat_sx_outcome, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ birthplace, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ gestation, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ gender, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ c_section, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ Region, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ birthseason, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ child_weight, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ time_admission, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ wbc, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ head_circum, data = the_data2) %>% summary()

names(the_data2)
#gender
#using ranked linear regression to obtain p_values and R squared
lm(rank(gender) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(gender) ~ birthplace, data = the_data2) %>% summary()
lm(rank(gender) ~ gestation, data = the_data2) %>% summary()
lm(rank(gender) ~ gender, data = the_data2) %>% summary()
lm(rank(gender) ~ c_section, data = the_data2) %>% summary()
lm(rank(gender) ~ Region, data = the_data2) %>% summary()
lm(rank(gender) ~ birthseason, data = the_data2) %>% summary()
lm(rank(gender) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(gender) ~ child_weight, data = the_data2) %>% summary()
lm(rank(gender) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(gender) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(gender) ~ time_admission, data = the_data2) %>% summary()
lm(rank(gender) ~ wbc, data = the_data2) %>% summary()
lm(rank(gender) ~ head_circum, data = the_data2) %>% summary()
lm(rank(gender) ~ cat_sx_outcome, data = the_data2) %>% summary()


#birthseason
#using ranked linear regression to obtain p_values and R squared
lm(rank(birthseason) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(birthseason) ~ birthplace, data = the_data2) %>% summary()
lm(rank(birthseason) ~ gestation, data = the_data2) %>% summary()
lm(rank(birthseason) ~ gender, data = the_data2) %>% summary()
lm(rank(birthseason) ~ c_section, data = the_data2) %>% summary()
lm(rank(birthseason) ~ Region, data = the_data2) %>% summary()
lm(rank(birthseason) ~ birthseason, data = the_data2) %>% summary()
lm(rank(birthseason) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(birthseason) ~ child_weight, data = the_data2) %>% summary()
lm(rank(birthseason) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(birthseason) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(birthseason) ~ time_admission, data = the_data2) %>% summary()
lm(rank(birthseason) ~ wbc, data = the_data2) %>% summary()
lm(rank(birthseason) ~ head_circum, data = the_data2) %>% summary()
lm(rank(birthseason) ~ cat_sx_outcome, data = the_data2) %>% summary()


#c_section
#using ranked linear regression to obtain p_values and R squared
lm(rank(c_section) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(c_section) ~ birthplace, data = the_data2) %>% summary()
lm(rank(c_section) ~ gestation, data = the_data2) %>% summary()
lm(rank(c_section) ~ gender, data = the_data2) %>% summary()
lm(rank(c_section) ~ c_section, data = the_data2) %>% summary()
lm(rank(c_section) ~ Region, data = the_data2) %>% summary()
lm(rank(c_section) ~ birthseason, data = the_data2) %>% summary()
lm(rank(c_section) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(c_section) ~ child_weight, data = the_data2) %>% summary()
lm(rank(c_section) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(c_section) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(c_section) ~ time_admission, data = the_data2) %>% summary()
lm(rank(c_section) ~ wbc, data = the_data2) %>% summary()
lm(rank(c_section) ~ head_circum, data = the_data2) %>% summary()
lm(rank(c_section) ~ cat_sx_outcome, data = the_data2) %>% summary()

#region, gestation, HC_etiology, birthplace


#Region
#using ranked linear regression to obtain p_values and R squared
lm(rank(Region) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(Region) ~ birthplace, data = the_data2) %>% summary()
lm(rank(Region) ~ gestation, data = the_data2) %>% summary()
lm(rank(Region) ~ gender, data = the_data2) %>% summary()
lm(rank(Region) ~ c_section, data = the_data2) %>% summary()
lm(rank(Region) ~ Region, data = the_data2) %>% summary()
lm(rank(Region) ~ birthseason, data = the_data2) %>% summary()
lm(rank(Region) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(Region) ~ child_weight, data = the_data2) %>% summary()
lm(rank(Region) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(Region) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(Region) ~ time_admission, data = the_data2) %>% summary()
lm(rank(Region) ~ wbc, data = the_data2) %>% summary()
lm(rank(Region) ~ head_circum, data = the_data2) %>% summary()
lm(rank(Region) ~ cat_sx_outcome, data = the_data2) %>% summary()

#gestation
#using ranked linear regression to obtain p_values and R squared
lm(rank(gestation) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(gestation) ~ birthplace, data = the_data2) %>% summary()
lm(rank(gestation) ~ gestation, data = the_data2) %>% summary()
lm(rank(gestation) ~ gender, data = the_data2) %>% summary()
lm(rank(gestation) ~ c_section, data = the_data2) %>% summary()
lm(rank(gestation) ~ Region, data = the_data2) %>% summary()
lm(rank(gestation) ~ birthseason, data = the_data2) %>% summary()
lm(rank(gestation) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(gestation) ~ child_weight, data = the_data2) %>% summary()
lm(rank(gestation) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(gestation) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(gestation) ~ time_admission, data = the_data2) %>% summary()
lm(rank(gestation) ~ wbc, data = the_data2) %>% summary()
lm(rank(gestation) ~ head_circum, data = the_data2) %>% summary()
lm(rank(gestation) ~ cat_sx_outcome, data = the_data2) %>% summary()

#HC_etiology
#using ranked linear regression to obtain p_values and R squared
lm(rank(HC_etiology) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ birthplace, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ gestation, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ gender, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ c_section, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ Region, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ birthseason, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ child_weight, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ time_admission, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ wbc, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ head_circum, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ cat_sx_outcome, data = the_data2) %>% summary()

#birthplace
#using ranked linear regression to obtain p_values and R squared
lm(rank(birthplace) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(birthplace) ~ birthplace, data = the_data2) %>% summary()
lm(rank(birthplace) ~ gestation, data = the_data2) %>% summary()
lm(rank(birthplace) ~ gender, data = the_data2) %>% summary()
lm(rank(birthplace) ~ c_section, data = the_data2) %>% summary()
lm(rank(birthplace) ~ Region, data = the_data2) %>% summary()
lm(rank(birthplace) ~ birthseason, data = the_data2) %>% summary()
lm(rank(birthplace) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(birthplace) ~ child_weight, data = the_data2) %>% summary()
lm(rank(birthplace) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(birthplace) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(birthplace) ~ time_admission, data = the_data2) %>% summary()
lm(rank(birthplace) ~ wbc, data = the_data2) %>% summary()
lm(rank(birthplace) ~ head_circum, data = the_data2) %>% summary()
lm(rank(birthplace) ~ cat_sx_outcome, data = the_data2) %>% summary()


##using linear regression on both continuous variables to obtain R squared
#time taken from birth to notice a head increase
lm(time_headincrease ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ child_weight, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ csf_vol, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ brain_vol, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ time_admission, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ wbc, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ head_circum, data=the_data2) %>% broom::glance()

#child_weight
lm(child_weight ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(child_weight ~ child_weight, data=the_data2) %>% broom::glance()
lm(child_weight ~ csf_vol, data=the_data2) %>% broom::glance()
lm(child_weight ~ brain_vol, data=the_data2) %>% broom::glance()
lm(child_weight ~ time_admission, data=the_data2) %>% broom::glance()
lm(child_weight ~ wbc, data=the_data2) %>% broom::glance()
lm(child_weight ~ head_circum, data=the_data2) %>% broom::glance()

#csf_vol
lm(csf_vol ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(csf_vol ~ child_weight, data=the_data2) %>% broom::glance()
lm(csf_vol ~ csf_vol, data=the_data2) %>% broom::glance()
lm(csf_vol ~ brain_vol, data=the_data2) %>% broom::glance()
lm(csf_vol ~ time_admission, data=the_data2) %>% broom::glance()
lm(csf_vol ~ wbc, data=the_data2) %>% broom::glance()
lm(csf_vol ~ head_circum, data=the_data2) %>% broom::glance()

#brain_vol
lm(brain_vol ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(brain_vol ~ child_weight, data=the_data2) %>% broom::glance()
lm(brain_vol ~ csf_vol, data=the_data2) %>% broom::glance()
lm(brain_vol ~ brain_vol, data=the_data2) %>% broom::glance()
lm(brain_vol ~ time_admission, data=the_data2) %>% broom::glance()
lm(brain_vol ~ wbc, data=the_data2) %>% broom::glance()
lm(brain_vol ~ head_circum, data=the_data2) %>% broom::glance()

#time_admission
lm(time_admission ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(time_admission ~ child_weight, data=the_data2) %>% broom::glance()
lm(time_admission ~ csf_vol, data=the_data2) %>% broom::glance()
lm(time_admission ~ brain_vol, data=the_data2) %>% broom::glance()
lm(time_admission ~ time_admission, data=the_data2) %>% broom::glance()
lm(time_admission ~ wbc, data=the_data2) %>% broom::glance()
lm(time_admission ~ head_circum, data=the_data2) %>% broom::glance()

#wbc
lm(wbc ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(wbc ~ child_weight, data=the_data2) %>% broom::glance()
lm(wbc ~ csf_vol, data=the_data2) %>% broom::glance()
lm(wbc ~ brain_vol, data=the_data2) %>% broom::glance()
lm(wbc ~ time_admission, data=the_data2) %>% broom::glance()
lm(wbc ~ wbc, data=the_data2) %>% broom::glance()
lm(wbc ~ head_circum, data=the_data2) %>% broom::glance()

#head_circum
lm(head_circum ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(head_circum ~ child_weight, data=the_data2) %>% broom::glance()
lm(head_circum ~ csf_vol, data=the_data2) %>% broom::glance()
lm(head_circum ~ brain_vol, data=the_data2) %>% broom::glance()
lm(head_circum ~ time_admission, data=the_data2) %>% broom::glance()
lm(head_circum ~ wbc, data=the_data2) %>% broom::glance()
lm(head_circum ~ head_circum, data=the_data2) %>% broom::glance()


###time_headcircumference,
###time_admission
###child_weight
###wbc
###csf_vol
###Hydro_etiology
###birthplace
###csection

#using ranked linear regression to obtain p_values and R squared
lm(rank(HC_etiology) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ birthplace, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ c_section, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ child_weight, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ time_admission, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ wbc, data = the_data2) %>% summary()

lm(time_headincrease ~ HC_etiology, data = the_data2) %>% summary()
lm(time_headincrease ~ birthplace, data = the_data2) %>% summary()
lm(time_headincrease ~ c_section, data = the_data2) %>% summary()
lm(time_headincrease ~ time_headincrease, data = the_data2) %>% summary()
lm(time_headincrease ~ child_weight, data = the_data2) %>% summary()
lm(time_headincrease ~ csf_vol, data = the_data2) %>% summary()
lm(time_headincrease ~ time_admission, data = the_data2) %>% summary()
lm(time_headincrease ~ wbc, data = the_data2) %>% summary()

lm(time_admission ~ HC_etiology, data = the_data2) %>% summary()
lm(time_admission ~ birthplace, data = the_data2) %>% summary()
lm(time_admission ~ c_section, data = the_data2) %>% summary()
lm(time_admission ~ time_headincrease, data = the_data2) %>% summary()
lm(time_admission ~ child_weight, data = the_data2) %>% summary()
lm(time_admission ~ csf_vol, data = the_data2) %>% summary()
lm(time_admission ~ time_admission, data = the_data2) %>% summary()
lm(time_admission ~ wbc, data = the_data2) %>% summary()

lm(child_weight ~ HC_etiology, data = the_data2) %>% summary()
lm(child_weight ~ birthplace, data = the_data2) %>% summary()
lm(child_weight ~ c_section, data = the_data2) %>% summary()
lm(child_weight ~ time_headincrease, data = the_data2) %>% summary()
lm(child_weight ~ child_weight, data = the_data2) %>% summary()
lm(child_weight ~ csf_vol, data = the_data2) %>% summary()
lm(child_weight ~ time_admission, data = the_data2) %>% summary()
lm(child_weight ~ wbc, data = the_data2) %>% summary()

lm(wbc ~ HC_etiology, data = the_data2) %>% summary()
lm(wbc ~ birthplace, data = the_data2) %>% summary()
lm(wbc ~ c_section, data = the_data2) %>% summary()
lm(wbc ~ time_headincrease, data = the_data2) %>% summary()
lm(wbc ~ child_weight, data = the_data2) %>% summary()
lm(wbc ~ csf_vol, data = the_data2) %>% summary()
lm(wbc ~ time_admission, data = the_data2) %>% summary()
lm(wbc ~ wbc, data = the_data2) %>% summary()

lm(csf_vol ~ HC_etiology, data = the_data2) %>% summary()
lm(csf_vol ~ birthplace, data = the_data2) %>% summary()
lm(csf_vol ~ c_section, data = the_data2) %>% summary()
lm(csf_vol ~ time_headincrease, data = the_data2) %>% summary()
lm(csf_vol ~ child_weight, data = the_data2) %>% summary()
lm(csf_vol ~ csf_vol, data = the_data2) %>% summary()
lm(csf_vol ~ time_admission, data = the_data2) %>% summary()
lm(csf_vol ~ wbc, data = the_data2) %>% summary()

lm(rank(birthplace) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(birthplace) ~ birthplace, data = the_data2) %>% summary()
lm(rank(birthplace) ~ c_section, data = the_data2) %>% summary()
lm(rank(birthplace) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(birthplace) ~ child_weight, data = the_data2) %>% summary()
lm(rank(birthplace) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(birthplace) ~ time_admission, data = the_data2) %>% summary()
lm(rank(birthplace) ~ wbc, data = the_data2) %>% summary()

lm(rank(c_section) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(c_section) ~ birthplace, data = the_data2) %>% summary()
lm(rank(c_section) ~ c_section, data = the_data2) %>% summary()
lm(rank(c_section) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(c_section) ~ child_weight, data = the_data2) %>% summary()
lm(rank(c_section) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(c_section) ~ time_admission, data = the_data2) %>% summary()
lm(rank(c_section) ~ wbc, data = the_data2) %>% summary()

##IMPORTING
p_value <- import("D:/Proposal writing/Data folder/p_value.xlsx")
R_value <- import("D:/Proposal writing/Data folder/Rfinal.xlsx")
View(p_value)
View(R_value)


#density plot for R values
dat <- stack(R_value[,-1])
ggplot(dat, aes(x=values, fill=ind)) + geom_density(alpha = 0.5, position = "identity") + coord_cartesian(ylim = c(0, 20))

#heatmap for R values
pheatmap(as.matrix(R_value[,-1]), display_numbers=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, col = colorRampPalette(brewer.pal(3, "YlOrRd"))(256), labels_row = c("time_headincrease", "time_admission", "child_weight", "wbc", "csf_vol", "Birthplace", "Hydro_etiology", "csection"))

#Heatmap for p_values
pheatmap(as.matrix(p_value[,-1]), display_numbers=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, col = colorRampPalette(brewer.pal(3, "OrRd"))(256), labels_row = c("time_headincrease", "time_admission", "head_circumference", "child_weight", "wbc", "Brain volume", "csf volume", "sx_outcome", "Score", "Birthplace", "Hydro_etiology", "sex", "csection", "season_birth", "region", "Gestation", "Hydro_group"))

##importing adjusted p-values
adj_pvalue <- import("D:/Proposal writing/data cleaning/adj p_value.xlsx")
View(adj_pvalue)
#drawing a heatmap for adjusted values: colors are...Greys, Oranges, Greens, Purples, RdBu, YlOrRd, YlOrBr, YlGnBu. YlGn, RdYlBu, RdPu, PuRd, OrRd
pheatmap(as.matrix(adj_pvalue[,-1]),  display_numbers=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, col = colorRampPalette(brewer.pal(3, "OrRd"))(256), labels_row = c("time_headincrease", "time_admission", "head_circumference", "child_weight", "wbc", "Brain volume", "csf volume", "Score", "Birthplace", "sex", "season_birth", "region", "Gestation"))
pheatmap(as.matrix(adj_pvalue[,-1]),  display_numbers=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, col = colorRampPalette(brewer.pal(3, "YlGnBu"))(256), labels_row = c("time_headincrease", "time_admission", "head_circumference", "child_weight", "wbc", "Brain volume", "csf volume", "Score", "Birthplace", "sex", "season_birth", "region", "Gestation"))
pheatmap(as.matrix(adj_pvalue[,-1]),  display_numbers=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, col = colorRampPalette(brewer.pal(3, "YlOrRd"))(256), labels_row = c("time_headincrease", "time_admission", "head_circumference", "child_weight", "wbc", "Brain volume", "csf volume", "Score", "sx_outcome", "Birthplace", "Hydro_etiology", "sex", "Csection", "season_birth", "region", "Gestation", "Hydro_group"))


#getting the adjusted p-values
p <- c(0.00194, 0.721,  0.0213, 0.021, 0.141, 0.00419, 0.011, 0.4689,  0.03627, 0.001, 0.2164, 0.001, 0.1686, 0.3818, 0.9416, 0.001, 0.887, 0.001, 0.631, 0.0807, 0.0641, 0.001,  0.1609, 0.2127, 0.001, 0.5978, 0.001, 0.9003, 0.07542, 0.9035, 0.001, 0.568, 0.822, 0.0917, 0.549, 0.536, 0.8696, 0.2799, 0.9233, 0.4028, 0.4187, 0.7075, 0.2009, 0.9085, 0.3424, 0.761, 0.001, 0.001, 0.029, 0.0707, 0.7681, 0.6309, 0.1598, 0.02673, 0.3201, 0.4996, 0.07094, 0.3924, 0.307, 0.00486, 0.001, 0.7856, 0.06204, 0.001, 0.7835, 0.001, 0.5488, 0.5175, 0.1311, 0.001, 0.213, 0.085, 0.2894, 0.775, 0.001, 0.1622, 0.135, 0.03689, 0.1664, 0.2049, 0.2874, 0.001, 0.03298, 0.001, 0.001, 0.765, 0.001, 0.4964, 0.3504, 0.4766, 0.001, 0.891, 0.001, 0.001, 0.55, 0.001, 0.425, 0.001, 0.603, 0.001, 0.06078, 0.4012, 0.8022, 0.4422, 0.1791, 0.4896, 0.1239, 0.2717, 0.001, 0.6609, 0.001, 0.2948, 0.01726, 0.5779, 0.001, 0.7567, 0.001, 0.228, 0.001, 0.9991, 0.001, 0.3985, 0.412, 0.8681, 0.192, 0.8987, 0.8229, 0.01797, 0.2472, 0.001, 0.6012, 0.6571, 0.08527, 0.1746, 0.001, 0.6709)
p.adjust(p, method = "bonferroni")

#drawing table
qflextable(the_data2)
#drawing a table in R
library(gtsummary)
Hydro_data%>%tbl_summary(statistic = list(
  all_continuous() ~ "{mean} ({sd})",
  all_categorical() ~ "{n} / {N} ({p}%)"
),
digits = all_continuous() ~ 2,
missing_text = "(Missing)")

Hydro_data%>%tbl_summary(by=cat_sx_outcome, 
                         statistic = list(
                           all_continuous() ~ "{mean} ({sd})",
                           all_categorical() ~ "{n} / {N} ({p}%)"
                         ),
                         digits = all_continuous() ~ 2,
                         missing_text = "(Missing)"
)

#Generating dataset to use for modelling
Hydro_data <- within(the_data2, rm(snum, c_section, HC_etiology, child_weight, hydro_group))
Hydrodata <- within(the_data, rm(snum, csection, hydro_etiology, place_of_delivery, time_infection, hydro_gp))
 
names(Hydrodata)
View(Hydrodata)


##classification and regression tree (CART)
# For decision tree model
library(rpart)
#install.packages("rlang", dependencies = TRUE)
library(rlang)
#install.packages(pkgs = "caret", 
                 #dependencies = c("Depends", "Imports"))
library(ggplot2)
library(lattice)
library(caret)
library(tidyverse)
# For data visualization
library(rpart.plot)

#pulling out data for an outcome variable from the whole dataset
outcome <- Hydrodata$sx_outcome
View(outcome)

#splitting/sampling of data (training and testing sets)
set.seed(234)
train = sample(1:nrow(Hydrodata), 200)
hydro.train = Hydrodata[train,]
hydro.test = Hydrodata[-train,]

#outcome data for the testing set
outcome.test = outcome[-train]


#building a classification tree
model1 = rpart(sx_outcome ~., data = hydro.train, method = "class", cp=0.008)

# Visualizing the unpruned tree
#rpart.plot(model1)
plot(model1, uniform=TRUE,
     main="Classification Tree for outcomes after surgical intervation")
text(model1, use.n=TRUE, all=TRUE, cex=.9)
# create attractive plot of tree
par(xpd = NA)
plot(model1)
text(model1, digits = 2)

model1$variable.importance

#making predictions
pred.tree = predict(model1, hydro.test, type="class")

#cross validation of predicted with actual values to see the performance
table(pred.tree, outcome.test) #there is a 23% misclassification and a 77% accuracy score

#confusionMatrix
confusionMatrix(pred.tree, outcome.test)

#getting a dataframe
data.frame(pred.tree, outcome.test)


#Here we consider whether pruning the tree might lead to improved results;
#cp(complex parameter) Pruning selects the cp (complexity parameter) value associated with a shorter tree that minimizes the cross-validated error rate (xerror)
plotcp(model1)
printcp(model1)

# Plot model accuracy vs different values of
# cp (complexity parameter)
plotcp(model1)

# Explicitly request the lowest cp value
model1$cptable[which.min(model1$cptable[,"xerror"]),"CP"]

bestcp <-model1$cptable[which.min(model1$cptable[,"xerror"]),"CP"]
pruned.tree <- prune(model1, cp = 0.017241) #bestcp had 0 splits and so used the second best cp with 5 splits
#rpart.plot(pruned.tree)

plot(pruned.tree)
text(pruned.tree, cex = 0.9, xpd = TRUE)

# prediction 
pred.prune = predict(pruned.tree, hydro.test, type="class")
table(pred.prune, outcome.test) #Both the train and testing set had the same misclassification of 22.5%

#Tree prunning using caret train method
#Pruning can be easily performed in the caret package workflow, which invokes the rpart method for automatically testing different possible values of cp, then choose the optimal cp that maximize the cross-validation (“cv”) accuracy, and fit the final best CART model that explains the best our data.
#You can use the following arguments in the function train() [from caret package]:
#trControl, to set up 10-fold cross validation
#tuneLength, to specify the number of possible cp values to evaluate. Default value is 3, here we’ll use 10

#or 
set.seed(123)

model2 <- train(sx_outcome ~., data = hydro.train, method = "rpart",
                trControl = trainControl(method = "cv", number = 10),
                tuneLength = 10)

print(model2)

# Plot model accuracy vs different values of
# cp (complexity parameter)
plot(model2)

# Print the best tuning parameter cp that
# maximizes the model accuracy
model2$bestTune


# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
rpart.plot(model2$finalModel)
#text(model2$finalModel,  digits = 3)

# Make predictions on the test data
predicted.classes <- model2 %>% predict(hydro.test)
data.frame(hydro.test, predicted.classes)

# Compute model accuracy rate on test data
mean(predicted.classes == hydro.test$sx_outcome)

confusionMatrix(hydro.test$sx_outcome, predicted.classes)





