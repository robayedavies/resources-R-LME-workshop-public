


#########################################################################################################


# load packages of useful functions

library(ggplot2)
library(psych)
library(reshape2)
library(plyr)
library(gridExtra)
library(lme4)



#########################################################################################################


# working with data prepared previously -- start by locating the working directory:-


# set working directory

# -- what is the current directory?

getwd()

# -- where are the data?

setwd("/Users/robdavies/Dropbox/teaching_PG-statistics/demo data")



#########################################################################################################


# read in the data


# we are focusing on data frames collecting together the item or word stimulus attribute, participant attribute and response
# data for the ML lexical decision study


# note that the subjects.behaviour.items has data on both words and non words


# subjects.behaviour.items

ML.all <- read.csv("subjects.behaviour.items-310114.csv", header=T, na.strings = "-999")
summary(ML.all)



# examine the data  ######################################################################################


# start with the items data

summary(ML.all)


# we note the presence of negative RTs -- corresponding to errors in DMDX datafiles
# we note also that the subjects and items values ought to have been assessed in previous work


# so here we focus on the RT distributions, we start with the items data

phist <- ggplot(ML.all, aes(x=RT))
phist + geom_histogram()


# clearly there can be no -ve RTs -- we can remove them through at least two ways 
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


# plot the distribution of log10 RTs in the subsetted dataframe

pRT3 <- ggplot(ML.all.correct, aes(x = logrt))
pRT3 <- pRT3 + geom_density() + ggtitle("correct log RTs") 
pRT3 <- pRT3 + theme(axis.title.x = element_text(size=25)) + theme(axis.text.x = element_text(size=20)) + theme(title = element_text(size=30))
pRT3



# linear modelling  #######################################################################################


# we adopt the linear modelling -- model comparison approach
# -- this may be more appropriate for exploratory than confirmatory analyses


# note that at this point we are ignoring the fact that the data have been collected according to a repeated measures design
# -- every person responded to every stimulus
# so that the observations are not independent
# -- every person's responses is likely to correlate more with each other than with the responses of other people
# we can take this clustering -- this structure -- into account quite easily in linear mixed effects models
# -- explored in later weeks


summary(ML.all.correct)


# we will use the lm() function to perform the linear modelling
# -- note that there are alternate methods for running and displaying regression models: ols()


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


# start with an empty model, just 1, the intercept, included as a predictor of logrt

ML.all.correct.lm.0  <- lm(logrt ~
                             
                             1,      
                           
                           data = ML.all.correct)

summary(ML.all.correct.lm.0)


# add as predictors the set of participant attribute variables

ML.all.correct.lm.1  <- lm(logrt ~
                             
                             zAge + zTOWRE_wordacc + zTOWRE_nonwordacc,     
                           
                           data = ML.all.correct)

summary(ML.all.correct.lm.1)


# add the item attribute variables

ML.all.correct.lm.2  <- lm(logrt ~
                             
                             zAge + zTOWRE_wordacc + zTOWRE_nonwordacc +
                             
                             item_type +
                             
                             zLength + zOrtho_N,     
                           
                           data = ML.all.correct)

summary(ML.all.correct.lm.2)


# we can compare model fits using information theoretic measures like AIC and BIC
# -- we would usually select the model with a BIC or AIC that is lower
# -- lower ie closer to negative infinity
# -- ie a smaller positive number is better than a larger positive number e.g. 10 is better than 100
# -- a larger negative number is better than a smaller negative number e.g. -100 is better than -10
# see slides and workbook for the explanation
# -- the more negative number is broadly closer to reality


# compare model fits:

BIC(ML.all.correct.lm.0, ML.all.correct.lm.1, ML.all.correct.lm.2)
AIC(ML.all.correct.lm.0, ML.all.correct.lm.1, ML.all.correct.lm.2)



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
anova(ML.all.correct.lmer.2, ML.all.correct.lmer.3)	


# get confidence intervals for effects estimates for last model

summary(ML.all.correct.lmer.2)
confint(ML.all.correct.lmer.2, method = "Wald")  



