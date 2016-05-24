


#########################################################################################################


# load packages of useful functions

library(ggplot2)
library(psych)
library(reshape2)
library(plyr)
library(gridExtra)
library(lme4)
library(dplyr)
library(broom)
library(MuMIn)
library(lmerTest)
library(grid)



#########################################################################################################


# working with data prepared previously -- start by locating the working directory:-


# set working directory

# -- what is the current directory?

getwd()

# -- where are the data?

setwd("/Users/robdavies/Dropbox/teaching_PG-statistics/demo data")
# -- replace the file path given in quotes with an address appropriate for your computer
# e.g. setwd("my computer/my name/my hard drive")



#########################################################################################################


# read in the data


# we are focusing on data frames collecting together the item or word stimulus attribute, participant attribute and response
# data for the ML lexical decision study


# note that the subjects.behaviour.items has data on lexical decision responses to words and to nonwords

# subjects.behaviour.items

ML.all <- read.csv("subjects.behaviour.items-310114.csv", header=T, na.strings = "-999")
summary(ML.all)


# -- if you want to examine effects of variables like word frequency (e.g. LgSUBTLCD), you need to analyze the subjects.behaviour.words-310114.csv 
# database, replacing ML.all in the code that follows with ML.words

ML.words <- read.csv("subjects.behaviour.words-310114.csv", header=T, na.strings = "-999")
summary(ML.words)



# examine the data  ######################################################################################


# start with the items data

summary(ML.all)


# clearly there can be no -ve RTs -- we can remove them through at least two methods 
# -- one way is through conditional subsetting
# -- set the threshold at 200ms: any shorter must be error or incorrect button press registration

ML.all.correct <- subset(ML.all, RT > 200)

length(ML.all$RT)
length(ML.all.correct$RT)

# > length(ML.all$RT)
# [1] 10880
# > length(ML.all.correct$RT)
# [1] 10254

summary(ML.all.correct)


# clearly the RT distribution will be skewed even after we have removed errors
# -- we can deal with the skew using a widely adopted method: log10 transformation of RTs

ML.all.correct$logrt <- log10(ML.all.correct$RT)



# preparation  #######################################################################################


# inspect the data

summary(ML.all.correct)


# -- we will need to start by standardizing the numeric variables we shall be using as predictors:
# -- because we will want to look at potential interaction effects
# -- and in class three we discussed how examining interactions using multiplicative product terms
# can give rise to multicollinearity unless we first center variables on their means

ML.all.correct$zAge <- scale(ML.all.correct$Age, scale = TRUE, center = TRUE)
ML.all.correct$zTOWRE_wordacc <- scale(ML.all.correct$TOWRE_wordacc, scale = TRUE, center = TRUE)
ML.all.correct$zTOWRE_nonwordacc <- scale(ML.all.correct$TOWRE_nonwordacc, scale = TRUE, center = TRUE)
ML.all.correct$zLength <- scale(ML.all.correct$Length, scale = TRUE, center = TRUE)
ML.all.correct$zOrtho_N <- scale(ML.all.correct$Ortho_N, scale = TRUE, center = TRUE)

# -- note I am not standardizing all variables, just those I am interested in



# linear mixed-effects modelling  #######################################################################################


# we change our approach to take into account the fact that the data have been collected according to a repeated measures design
# -- every person responded to every stimulus
# so that the observations are not independent
# -- every person's responses is likely to correlate more with each other than with the responses of other people
# we can take this clustering -- this structure -- into account quite easily in linear mixed effects models


summary(ML.all.correct)


# we will use the lmer() function to perform the linear modelling
# -- note that there are alternate methods for running and displaying regression models: ols()


# start with an empty model, just 1, the intercept, included as a predictor of logrt

ML.all.correct.lmer.0  <- lmer(logrt ~
                                 
                                 (1|subjectID) + (1|item_name),      
                               
                               data = ML.all.correct, REML = FALSE)

summary(ML.all.correct.lmer.0)


# add as predictors the set of participant attribute variables

ML.all.correct.lmer.1  <- lmer(logrt ~
                                 
                                 zAge + zTOWRE_wordacc + zTOWRE_nonwordacc +
                                 
                                 (1|subjectID) + (1|item_name),     
                               
                               data = ML.all.correct, REML = FALSE)

summary(ML.all.correct.lmer.1)


# add the item attribute variables

ML.all.correct.lmer.2  <- lmer(logrt ~
                                 
                                 zAge + zTOWRE_wordacc + zTOWRE_nonwordacc +
                                 
                                 item_type +
                                 
                                 zLength + zOrtho_N+
                                 
                                 (1|subjectID) + (1|item_name),     
                               
                               data = ML.all.correct, REML = FALSE)

summary(ML.all.correct.lmer.2)


# we can compare model fits using information theoretic measures like AIC and BIC
# -- we would usually select the model with a BIC or AIC that is lower
# -- lower ie closer to negative infinity
# -- ie a smaller positive number is better than a larger positive number e.g. 10 is better than 100
# -- a larger negative number is better than a smaller negative number e.g. -100 is better than -10
# see slides and workbook for the explanation
# -- the more negative number is broadly closer to reality


# compare model fits:

BIC(ML.all.correct.lmer.0, ML.all.correct.lmer.1, ML.all.correct.lmer.2)
AIC(ML.all.correct.lmer.0, ML.all.correct.lmer.1, ML.all.correct.lmer.2)


# we can also use the likelihood ratio test comparison:-

anova(ML.all.correct.lmer.0, ML.all.correct.lmer.1)    	
anova(ML.all.correct.lmer.1, ML.all.correct.lmer.2)			


# get confidence intervals for effects estimates for last model

summary(ML.all.correct.lmer.2)
confint(ML.all.correct.lmer.2, method = "Wald")  


# get amrginal and conditional GLMM R-sq (Barton, 2016; Johnson, 2014):

r.squaredGLMM(ML.all.correct.lmer.0)
r.squaredGLMM(ML.all.correct.lmer.1)
r.squaredGLMM(ML.all.correct.lmer.2)







