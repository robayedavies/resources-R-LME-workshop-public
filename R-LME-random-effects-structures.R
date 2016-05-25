# load libraries of packages #############################################################################


library(ggplot2)
library(gridExtra)
library(rms)
library(plyr)
library(reshape2)
library(psych)
library(car)
library(languageR)
library(lme4)
library(lmerTest)



# load data files  #######################################################################################


# 250215 note data provenance: /Users/robdavies/Dropbox/resources R/demonstration data/lexical decision data - ML student project
# -- revision of: 402-class-9-110314.R


# set the working directory

setwd("/Users/robdavies/Dropbox/teaching_PG-statistics/demo data")
# work out where your files are and substitute for the path for my files location, above


# read in the data


# we are focusing on data frames collecting together the item or word stimulus attribute, participant attribute and response
# information


# note that the subjects.behaviour.items has data on both words and non words
# while subjects.behaviour.words has data required for an analysis of responses to words only


# subjects.behaviour.items

ML.all <- read.csv("subjects.behaviour.items-310114.csv", header=T, na.strings = "-999")
summary(ML.all)

# subjects.behaviour.words -- commented out because not needed for the class demonstration
# 
# ML.words <- read.csv("subjects.behaviour.words-310114.csv", header=T, na.strings = "-999")
# summary(ML.words)



# examine the data  ######################################################################################



# -- items -- data on responses to words and non-words


# the first concern is the presence of error RTs ie negative RTs
# -- look at the minimum value in the summary for RTs
summary(ML.all)


# clearly there can be no -ve RTs -- we can remove them through at least two ways 
# -- one way is through conditional subsetting
# -- set the threshold at 200ms: any shorter must be error or incorrect button press registration

ML.all.correct <- subset(ML.all, RT > 200)

# check the impact of the subsetting

length(ML.all$RT)
length(ML.all.correct$RT)

# > length(ML.all$RT)
# [1] 10880
# > length(ML.all.correct$RT)
# [1] 10254

# how many observations were excluded?

length(ML.all$RT) - length(ML.all.correct$RT)


# inspect the nomissing database

summary(ML.all.correct)


# the RT distribution is skewed -- we can deal with the skew using a widely adopted method: log10 transformation of RTs

ML.all.correct$logrt <- log10(ML.all.correct$RT)



# centre predictor variables  ################################################################################


# -- it is convenient, computationally, and for later interpretation, to center continuous numeric predictor
# variables on their means (Cohen, Cohen, Aiken & West, 2003)
# -- it eliminates multicollinearity due to scaling, a problem where we may seek to estimate interactions
# between two numeric predictors
# -- it renders more intuitively interpretable variables that may not have a 'true' zero
# -- it makes the variables easier to handle for lme4 estimation


# centre by standardizing the key subject and item variables -- ML.all.correct
# -- note lexicality will also be included in the predictor set but as an uncentred factor

ML.all.correct$zAge <- scale(ML.all.correct$Age, center = TRUE, scale = TRUE)
ML.all.correct$zTOWRE_wordacc <- scale(ML.all.correct$TOWRE_wordacc, center = TRUE, scale = TRUE)
ML.all.correct$zTOWRE_nonwordacc <- scale(ML.all.correct$TOWRE_nonwordacc, center = TRUE, scale = TRUE)
ML.all.correct$zLength <- scale(ML.all.correct$Length, center = TRUE, scale = TRUE)
ML.all.correct$zOrtho_N <- scale(ML.all.correct$Ortho_N, center = TRUE, scale = TRUE)
ML.all.correct$zBG_Mean <- scale(ML.all.correct$BG_Mean, center = TRUE, scale = TRUE)



# we then move to linear mixed-effects modelling ###############################################################


# I assume you have:
# -- removed error RTs
# -- log10 transformed remaining correct RTs
# -- centred numeric predictor variables


# -- inspect the data for analysis

summary(ML.all.correct)


# we might start by examining the effects of predictors in a series of additions of sets of predictors
# we can hold the random effects, of subjects and items on intercepts, constant, while varying the fixed effects
# we can compare the relative fit of models to data using the anova() likelihood ratio test
# in this series of models, we will use ML fitting when comparing models varying in the fixed effects
# but constant in the random effects


# start by examining just an empty model with random effects of subjects and items on intercepts only
# -- here, the fixed effect will be just the overall intercept

full.lmer.0 <- lmer(logrt ~
                      
                      (1|subjectID) + (1|item_name),
                    
                    data = ML.all.correct, REML = F)
summary(full.lmer.0)

# -- results are copied here so that you can check your results:

# > summary(full.lmer.0)
# Linear mixed model fit by maximum likelihood  
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: logrt ~ (1 | subjectID) + (1 | item_name)
# Data: ML.all.correct
# 
# AIC      BIC   logLik deviance df.resid 
# -17980.8 -17951.8   8994.4 -17988.8    10250 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.5446 -0.6442 -0.1498  0.4667  5.2431 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# item_name (Intercept) 0.002681 0.05178 
# subjectID (Intercept) 0.003044 0.05517 
# Residual              0.009281 0.09634 
# Number of obs: 10254, groups:  item_name, 320; subjectID, 34
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  2.823356   0.009941 40.520000     284   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# move on to adding in subject attribute variables
# -- the fixed effects component has changed but the random effects stay the same

full.lmer.1 <- lmer(logrt ~
                      
                      zAge + zTOWRE_wordacc + zTOWRE_nonwordacc + 
                      
                      (1|subjectID) + (1|item_name),
                    
                    data = ML.all.correct, REML = FALSE)

summary(full.lmer.1)

# -- note I copy the results in but don't bother to copy the results for the correlation of fixed effects
# 
# > summary(full.lmer.1)
# Linear mixed model fit by maximum likelihood  
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: logrt ~ zAge + zTOWRE_wordacc + zTOWRE_nonwordacc + (1 | subjectID) +      (1 | item_name)
# Data: ML.all.correct
# 
# AIC      BIC   logLik deviance df.resid 
# -17981.9 -17931.3   8998.0 -17995.9    10247 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.5397 -0.6439 -0.1489  0.4675  5.2441 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# item_name (Intercept) 0.002681 0.05178 
# subjectID (Intercept) 0.002458 0.04958 
# Residual              0.009281 0.09634 
# Number of obs: 10254, groups:  item_name, 320; subjectID, 34
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)        2.8233188  0.0090345 42.1200000 312.505   <2e-16 ***
#   zAge               0.0130997  0.0088762 33.9600000   1.476    0.149    
# zTOWRE_wordacc     0.0001089  0.0114635 33.9700000   0.010    0.992    
# zTOWRE_nonwordacc -0.0174850  0.0113883 33.9700000  -1.535    0.134    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) zAge   zTOWRE_w
# zAge         0.012                
# zTOWRE_wrdc -0.006  0.126         
# zTOWRE_nnwr  0.013  0.093 -0.644  


# add the item attribute variables: item type (words or nonwords), item length, orthographic neighbourhood size

full.lmer.2  <- lmer(logrt ~
                       
                       zAge + zTOWRE_wordacc + zTOWRE_nonwordacc +
                       
                       item_type +
                       
                       zLength + zOrtho_N+
                       
                       (1|subjectID) + (1|item_name),     
                     
                     data = ML.all.correct, REML = FALSE)

summary(full.lmer.2)

# > summary(full.lmer.2)
# Linear mixed model fit by maximum likelihood  
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: logrt ~ zAge + zTOWRE_wordacc + zTOWRE_nonwordacc + item_type +  
#   zLength + zOrtho_N + (1 | subjectID) + (1 | item_name)
# Data: ML.all.correct
# 
# AIC      BIC   logLik deviance df.resid 
# -18319.5 -18247.1   9169.7 -18339.5    10244 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.5521 -0.6479 -0.1575  0.4713  5.2553 
# 
# Random effects:
#   Groups    Name        Variance  Std.Dev.
# item_name (Intercept) 0.0006949 0.02636 
# subjectID (Intercept) 0.0024557 0.04955 
# Residual              0.0092889 0.09638 
# Number of obs: 10254, groups:  item_name, 320; subjectID, 34
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)        2.866e+00  8.859e-03  3.907e+01 323.552  < 2e-16 ***
#   zAge               1.318e-02  8.871e-03  3.397e+01   1.486 0.146521    
# zTOWRE_wordacc     1.078e-04  1.146e-02  3.398e+01   0.009 0.992550    
# zTOWRE_nonwordacc -1.747e-02  1.138e-02  3.398e+01  -1.534 0.134173    
# item_typeword     -8.655e-02  3.517e-03  3.035e+02 -24.610  < 2e-16 ***
#   zLength            8.645e-03  2.198e-03  3.044e+02   3.934 0.000104 ***
#   zOrtho_N           2.254e-03  2.195e-03  3.046e+02   1.027 0.305203    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) zAge   zTOWRE_w zTOWRE_n itm_ty zLngth
# zAge         0.012                                       
# zTOWRE_wrdc -0.006  0.126                                
# zTOWRE_nnwr  0.014  0.093 -0.644                         
# item_typwrd -0.200  0.000  0.000    0.001                
# zLength     -0.008  0.000  0.000    0.000    0.031       
# zOrtho_N    -0.011  0.000  0.000    0.000    0.047  0.602


# add interactions

full.lmer.3  <- lmer(logrt ~
                       
                       (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc)*
                       
                       (item_type +
                          
                          zLength + zOrtho_N)+
                       
                       (1|subjectID) + (1|item_name),     
                     
                     data = ML.all.correct, REML = FALSE)

summary(full.lmer.3)

# > summary(full.lmer.3)
# Linear mixed model fit by maximum likelihood  
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: logrt ~ (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc) * (item_type +  
#                                                                   zLength + zOrtho_N) + (1 | subjectID) + (1 | item_name)
# Data: ML.all.correct
# 
# AIC      BIC   logLik deviance df.resid 
# -18416.0 -18278.5   9227.0 -18454.0    10235 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.3763 -0.6531 -0.1526  0.4747  5.2171 
# 
# Random effects:
#   Groups    Name        Variance  Std.Dev.
# item_name (Intercept) 0.0006963 0.02639 
# subjectID (Intercept) 0.0024665 0.04966 
# Residual              0.0091826 0.09583 
# Number of obs: 10254, groups:  item_name, 320; subjectID, 34
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      2.866e+00  8.876e-03  3.900e+01 322.933  < 2e-16 ***
#   zAge                             1.343e-02  8.947e-03  3.500e+01   1.501 0.142322    
# zTOWRE_wordacc                   1.075e-02  1.156e-02  3.500e+01   0.931 0.358510    
# zTOWRE_nonwordacc               -2.974e-02  1.148e-02  3.500e+01  -2.590 0.013898 *  
#   item_typeword                   -8.657e-02  3.513e-03  3.040e+02 -24.642  < 2e-16 ***
#   zLength                          8.708e-03  2.195e-03  3.050e+02   3.967 9.10e-05 ***
#   zOrtho_N                         2.301e-03  2.193e-03  3.050e+02   1.049 0.294854    
# zAge:item_typeword              -3.892e-04  1.960e-03  9.903e+03  -0.199 0.842597    
# zAge:zLength                     8.947e-04  1.223e-03  9.923e+03   0.731 0.464617    
# zAge:zOrtho_N                    1.301e-03  1.223e-03  9.911e+03   1.064 0.287500    
# zTOWRE_wordacc:item_typeword    -2.084e-02  2.545e-03  9.899e+03  -8.189 4.44e-16 ***
#   zTOWRE_wordacc:zLength           2.671e-03  1.590e-03  9.897e+03   1.680 0.092997 .  
# zTOWRE_wordacc:zOrtho_N         -7.182e-04  1.596e-03  9.900e+03  -0.450 0.652720    
# zTOWRE_nonwordacc:item_typeword  2.385e-02  2.532e-03  9.900e+03   9.418  < 2e-16 ***
#   zTOWRE_nonwordacc:zLength       -5.378e-03  1.582e-03  9.904e+03  -3.400 0.000677 ***
#   zTOWRE_nonwordacc:zOrtho_N      -1.901e-03  1.588e-03  9.905e+03  -1.197 0.231376    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation matrix not shown by default, as p = 16 > 12.
# Use print(x, correlation=TRUE)  or
# vcov(x)	 if you need it



# we then move to linear mixed-effects modelling ###############################################################


# we can compare model fits using information theoretic measures like AIC and BIC
# -- we would usually select the model with a BIC or AIC that is lower
# -- lower ie closer to negative infinity
# -- ie a smaller positive number is better than a larger positive number e.g. 10 is better than 100
# -- a larger negative number is better than a smaller negative number e.g. -100 is better than -10
# see slides and workbook for the explanation
# -- the more negative number is broadly closer to reality


# compare model fits:

BIC(full.lmer.0, full.lmer.1, full.lmer.2, full.lmer.3)
AIC(full.lmer.0, full.lmer.1, full.lmer.2, full.lmer.3)


# we can also use the likelihood ratio test comparison:-

anova(full.lmer.0, full.lmer.1, full.lmer.2, full.lmer.3)  

# > anova(full.lmer.0, full.lmer.1, full.lmer.2, full.lmer.3)
# Data: ML.all.correct
# Models:
#   full.lmer.0: logrt ~ (1 | subjectID) + (1 | item_name)
# full.lmer.1: logrt ~ zAge + zTOWRE_wordacc + zTOWRE_nonwordacc + (1 | subjectID) + 
#   full.lmer.1:     (1 | item_name)
# full.lmer.2: logrt ~ zAge + zTOWRE_wordacc + zTOWRE_nonwordacc + item_type + 
#   full.lmer.2:     zLength + zOrtho_N + (1 | subjectID) + (1 | item_name)
# full.lmer.3: logrt ~ (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc) * (item_type + 
# full.lmer.3:     zLength + zOrtho_N) + (1 | subjectID) + (1 | item_name)
# Df    AIC    BIC logLik deviance    Chisq Chi Df Pr(>Chisq)    
# full.lmer.0  4 -17981 -17952 8994.4   -17989                               
# full.lmer.1  7 -17982 -17931 8998.0   -17996   7.1782      3    0.06643 .  
# full.lmer.2 10 -18320 -18247 9169.7   -18340 343.5400      3    < 2e-16 ***
#   full.lmer.3 19 -18416 -18278 9227.0   -18454 114.5129      9    < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# get confidence intervals for effects estimates for last model

summary(full.lmer.3)
confint(full.lmer.3, method = "Wald")  


# week six workbook reference [3] #######################################################################


# compare models of varying complexity -- and evaluate on exactly the same basis as the lm models

anova(full.lmer.0, full.lmer.1, full.lmer.2, full.lmer.3)  

# we could use the model summary for the last model for report
# we cannot get p-values for model effect estimates directly but can get confidence intervals
# -- if 95% confidence intervals do not include 0 then the effect can be claimed as significant

summary(full.lmer.3)
confint(full.lmer.3, method = "Wald") 



################################################################################


# We must now consider the random effects on subjects or items.
#
# We can compare models with the same fixed effects as read.lmer, REML=T models, but varying random effects
# on intercepts of subjects or items or both, comparing models using the likelihood ratio test (LRT)

# we compare models that include: (i.) both random effects of subjects and items on intercepts, as specified for model 3 
# (ii.) just the random effect of subjects on intercepts; and (iii.) just the random effect of items


# week six workbook reference [4] #######################################################################


# (1) REML=TRUE model with random effects of subjects and items on intercepts

full.lmer.3  <- lmer(logrt ~
                       
                       (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc)*
                       
                       (item_type + zLength + zOrtho_N) +
                       
                       (1|subjectID) + (1|item_name),     
                     
                     data = ML.all.correct, REML = TRUE)
summary(full.lmer.3)


# (2) REML=TRUE model with random effects of just subjects on intercepts

full.lmer.3.s  <- lmer(logrt ~
                         
                         (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc)*
                         
                         (item_type + zLength + zOrtho_N) +
                         
                         (1|subjectID),     
                       
                       data = ML.all.correct, REML = TRUE)
summary(full.lmer.3.s)


# (3) REML=TRUE model with random effects of just items on intercepts

full.lmer.3.i  <- lmer(logrt ~
                         
                         (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc)*
                         
                         (item_type + zLength + zOrtho_N) +
                         
                         (1|item_name),     
                       
                       data = ML.all.correct, REML = TRUE)
summary(full.lmer.3.i)


# Now compare the models with varying random effects using the likelihood ratio test.


# Is the random effect of subjects on intercepts justified? -- compare models with and without the random effect of subjects on intercepts

anova(full.lmer.3, full.lmer.3.i)

# > anova(full.lmer.3, full.lmer.3.i)
# Data: ML.all.correct
# Models:
#   full.lmer.3.i: logrt ~ (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc) * (item_type + 
#   full.lmer.3.i:     zLength + zOrtho_N) + (1 | item_name)
# full.lmer.3: logrt ~ (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc) * (item_type + 
#   full.lmer.3:     zLength + zOrtho_N) + (1 | subjectID) + (1 | item_name)
# Df    AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# full.lmer.3.i 18 -16193 -16063 8114.5   -16229                            
# full.lmer.3   19 -18416 -18278 9227.0   -18454  2225      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Is the random effect of items on intercepts justified? -- compare models with and without the random effect of items on intercepts

anova(full.lmer.3, full.lmer.3.s)

# > anova(full.lmer.3, full.lmer.3.s)
# Data: ML.all.correct
# Models:
#   full.lmer.3.s: logrt ~ (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc) * (item_type + 
#   full.lmer.3.s:     zLength + zOrtho_N) + (1 | subjectID)
# full.lmer.3: logrt ~ (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc) * (item_type + 
#   full.lmer.3:     zLength + zOrtho_N) + (1 | subjectID) + (1 | item_name)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# full.lmer.3.s 18 -18092 -17962   9064   -18128                             
# full.lmer.3   19 -18416 -18278   9227   -18454 326.06      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# week six workbook reference [5] #######################################################################


# we can then evaluate whether it would be justified to include the random effects of subjects on the slopes of the effects of the 
# fixed effects vaariables.


# That is, we examine if there is significant variation between subjects in the shape of the effects of theoretical interest. 
# The LRT calculations have so far shown that a mixed-effects model (REML=T) with the fixed effects of 
# model 3, and both random effects of subjects and items, is justified. We are now interested in whether random effects of subjects 
# on slopes (so-called random slopes) should also be included in the model. 


# it may not make sense to fit a model with random slopes for all predictor variables, especially where some predictors evidently 
# have no significant impact on the outcome variable

# -- for now we are going to specify a model with random effects of subjects on the slopes of the fixed effects of the item
# variables: item type, length, and orthographic neighbourhood size

full.lmer.3.slopes  <- lmer(logrt ~
                              
                              (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc)*
                              
                              (item_type + zLength + zOrtho_N) +
                              
                              (item_type + zLength + zOrtho_N + 1|subjectID) + (1|item_name),     
                            
                            data = ML.all.correct, REML = FALSE)
summary(full.lmer.3.slopes)
confint(full.lmer.3.slopes, method = "Wald")

# > summary(full.lmer.3.slopes)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: logrt ~ (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc) * (item_type +  
# zLength + zOrtho_N) + (item_type + zLength + zOrtho_N + 1 |      subjectID) + (1 | item_name)
# Data: ML.all.correct
# 
# AIC      BIC   logLik deviance df.resid 
# -18740.2 -18537.6   9398.1 -18796.2    10226 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.3528 -0.6455 -0.1528  0.4591  5.4008 
# 
# Random effects:
#   Groups    Name          Variance  Std.Dev. Corr             
# item_name (Intercept)   7.340e-04 0.027092                  
# subjectID (Intercept)   3.018e-03 0.054936                  
# item_typeword 1.483e-03 0.038506 -0.43            
# zLength       1.032e-05 0.003212  0.78 -0.90      
# zOrtho_N      1.526e-05 0.003906  0.44 -1.00  0.90
# Residual                8.784e-03 0.093724                  
# Number of obs: 10254, groups:  item_name, 320; subjectID, 34
# 
# Fixed effects:
#                                   Estimate Std. Error t value
# (Intercept)                      2.8673455  0.0097556  293.92
# zAge                             0.0134423  0.0098701    1.36
# zTOWRE_wordacc                   0.0111710  0.0127478    0.88
# zTOWRE_nonwordacc               -0.0296577  0.0126649   -2.34
# item_typeword                   -0.0873559  0.0075026  -11.64
# zLength                          0.0089692  0.0022906    3.92
# zOrtho_N                         0.0026117  0.0023197    1.13
# zAge:item_typeword              -0.0005010  0.0071144   -0.07
# zAge:zLength                     0.0008601  0.0013264    0.65
# zAge:zOrtho_N                    0.0012614  0.0013837    0.91
# zTOWRE_wordacc:item_typeword    -0.0213781  0.0091916   -2.33
# zTOWRE_wordacc:zLength           0.0027968  0.0017216    1.62
# zTOWRE_wordacc:zOrtho_N         -0.0005204  0.0018010   -0.29
# zTOWRE_nonwordacc:item_typeword  0.0238701  0.0091318    2.61
# zTOWRE_nonwordacc:zLength       -0.0053559  0.0017122   -3.13
# zTOWRE_nonwordacc:zOrtho_N      -0.0018776  0.0017913   -1.05
# 
# > confint(full.lmer.3.slopes, method = "Wald")
# 2.5 %       97.5 %
#   (Intercept)                      2.8482248044  2.886466108
# zAge                            -0.0059027425  0.032787314
# zTOWRE_wordacc                  -0.0138141889  0.036156181
# zTOWRE_nonwordacc               -0.0544804624 -0.004834965
# item_typeword                   -0.1020607740 -0.072651083
# zLength                          0.0044796160  0.013458743
# zOrtho_N                        -0.0019348814  0.007158352
# zAge:item_typeword              -0.0144450893  0.013443025
# zAge:zLength                    -0.0017395403  0.003459724
# zAge:zOrtho_N                   -0.0014505493  0.003973376
# zTOWRE_wordacc:item_typeword    -0.0393933764 -0.003362846
# zTOWRE_wordacc:zLength          -0.0005775413  0.006171083
# zTOWRE_wordacc:zOrtho_N         -0.0040503341  0.003009439
# zTOWRE_nonwordacc:item_typeword  0.0059719947  0.041768130
# zTOWRE_nonwordacc:zLength       -0.0087116601 -0.002000068
# zTOWRE_nonwordacc:zOrtho_N      -0.0053884456  0.001633199


# Is the random effect of subjects on the slopes of the item type, length and orthographic neighbourhood effects justified?
# -- compare models with and without the random effect of items on intercepts

anova(full.lmer.3, full.lmer.3.slopes)

# > anova(full.lmer.3, full.lmer.3.slopes)
# Data: ML.all.correct
# Models:
# full.lmer.3: logrt ~ (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc) * (item_type + 
# full.lmer.3:     zLength + zOrtho_N) + (1 | subjectID) + (1 | item_name)
# full.lmer.3.slopes: logrt ~ (zAge + zTOWRE_wordacc + zTOWRE_nonwordacc) * (item_type + 
# full.lmer.3.slopes:     zLength + zOrtho_N) + (item_type + zLength + zOrtho_N + 1 | 
# full.lmer.3.slopes:     subjectID) + (1 | item_name)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# full.lmer.3        19 -18416 -18278 9227.0   -18454                             
# full.lmer.3.slopes 28 -18740 -18538 9398.1   -18796 342.25      9  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




