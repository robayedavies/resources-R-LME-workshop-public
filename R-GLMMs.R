


#####################################################################################################################################################################


# start by loading the libraries of packages you will need:
#   if you get an error because a package has not been installed then you can install it either by command or
#   using the R-studio install() function


library(ggplot2)
library(grid)
library(reshape2)
library(plyr)
library(psych)
library(rms)
library(languageR)
library(lme4)
library(effects)



#####################################################################################################################################################################


# Work out where your data files are and set the working directory for the analyses in this script

getwd()

setwd("[insert working directory address information here]")



#####################################################################################################################################################################


# Read in the data -- main data

all.merged <- read.csv(file="noun-verb-learning-study.csv", head=TRUE,sep=",", na.strings = "-999")

# inspect 

summary(all.merged)


# -- note that accuracy gets treated as a number, a continuous numeric variable, because correct vs. incorrect responses are coded using numbers
# -- but we need the accuracy variable to be treated as a categorical variable

all.merged$accuracy <- as.factor(all.merged$accuracy)



#####################################################################################################################################################################


# -- we can start by plotting the outcome variable for each experimental condition:
# -- from blocks 1 to 12 of learning trials
# -- where stimuli to be learnt are: noun only; verb only; noun and verb together

pdf("gavagai-accuracy-histogram.pdf", w = 12, h = 6)

paccuracy <- ggplot(data = all.merged, aes(x = accuracy, fill = accuracy))
paccuracy <- paccuracy + geom_histogram() + theme_bw()
paccuracy <- paccuracy + theme(axis.title.x = element_text(size=25)) + theme(axis.text.x = element_text(size=20))
paccuracy <- paccuracy + theme(axis.title.y = element_text(size=25)) + theme(axis.text.y = element_text(size=20))
paccuracy <- paccuracy + ggtitle("Response accuracy") + theme(title = element_text(size=30))
paccuracy <- paccuracy + facet_grid(Experiment ~ block)
paccuracy

dev.off()



#####################################################################################################################################################################


# generalized linear mixed-effects models ###########################################################################################################################


# we can use to multilevel modelling to take into account the random effects of subjects on the effects of block and experimental condition:                                                                          

# we can get generalized mixed effect models incorporating random effects of subjects and item objects and actions
# we can first examine models varying in fixed effects with constant random effects (of subjects or items on intercepts)
# we can then examine random effects -- we should start with just random intercepts then allow for random slopes in the 
# fixed effects


# -- models varying in fixed effects --


# empty model

all.merged.glmm0 <- glmer(accuracy ~ 
                            
                            (1|Subjecta) + (1|targetobject) + (1|targetaction),    
                          
                          data = all.merged, family = binomial)

summary(all.merged.glmm0)


# add effect of experimental condition, coded as ``Experiment'' variable, a factor with levels ``nounonly'', ``verbonly'', ``nounverb''

all.merged.glmm1 <- glmer(accuracy ~ 
                            
                            Experiment +
                            
                            (1|Subjecta) + (1|targetobject) + (1|targetaction),    
                          
                          data = all.merged, family = binomial)
summary(all.merged.glmm1)


# add effect of experimental condition and block

all.merged.glmm2 <- glmer(accuracy ~ 
                            
                            Experiment + block +
                            
                            (1|Subjecta) + (1|targetobject) + (1|targetaction),    
                          
                          data = all.merged, family = binomial)
summary(all.merged.glmm2)


# add effect of experimental condition and block interaction

all.merged.glmm3 <- glmer(accuracy ~ 
                            
                            Experiment*block +
                            
                            (1|Subjecta) + (1|targetobject) + (1|targetaction),    
                          
                          data = all.merged, family = binomial)
summary(all.merged.glmm3)


# How do we know if increasing model complexity by adding predictors actually helps us to account for variation in outcome values?

anova(all.merged.glmm0, all.merged.glmm1, all.merged.glmm2, all.merged.glmm3)


# -- now compare models varying in random effects but having the same fixed effects --
# -- note I am just going to assume we need both random effects of subjects and of items on intercepts

# add random slopes -- allow correlations between random slopes and intercepts

all.merged.glmm4 <- glmer(accuracy ~ 
                            
                            Experiment*block +
                            
                            (block + 1|Subjecta) + (block + 1|targetobject) + (block + 1|targetaction),    
                          
                          data = all.merged, family = binomial)
summary(all.merged.glmm4)

# -- but if we look at the model summary we see something odd:

# > summary(all.merged.glmm4)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: accuracy ~ Experiment * block + (block + 1 | Subjecta) + (block +      1 | targetobject) + (block + 1 | targetaction)
# Data: all.merged
# 
# AIC      BIC   logLik deviance df.resid 
# 16500.3  16613.3  -8235.1  16470.3    13809 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -10.0695  -0.9979   0.4049   0.8602   1.5032 
# 
# Random effects:
#   Groups       Name        Variance  Std.Dev. Corr 
# Subjecta     (Intercept) 4.414e-02 0.210087      
# block       2.576e-02 0.160486 -0.68
# targetobject (Intercept) 4.830e-03 0.069501      
# block       4.952e-05 0.007037 1.00 
# targetaction (Intercept) 1.255e-02 0.112039      
# block       4.329e-06 0.002081 1.00 
# Number of obs: 13824, groups:  Subjecta, 48; targetobject, 9; targetaction, 9
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -0.26701    0.14383  -1.856   0.0634 .  
# Experimentnounverb       -0.01944    0.17076  -0.114   0.9094    
# Experimentverbonly        0.26595    0.18433   1.443   0.1491    
# block                     0.18715    0.04205   4.450 8.58e-06 ***
#   Experimentnounverb:block  0.01172    0.05940   0.197   0.8435    
# Experimentverbonly:block -0.12866    0.05925  -2.172   0.0299 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# -- correlations between the random effects of items on intercepts and the slopes of the block effect suggest that the level
# of complexity we specify in this model cannot really be justified
# -- note also that the variances for the random effects of items on the slopes of the block effect are very small
# -- we should see if a simpler model, excluding the correlations can be estimated


# try running the model removing just the problematic correlations -- item-wise -- between random effects

all.merged.glmm5 <- glmer(accuracy ~ 
                            
                            Experiment*block +
                            
                            (block + 1|Subjecta) + (1|targetobject) + (1|targetaction) + 
                            (block + 0|targetobject) + (block + 0|targetaction),    
                          
                          data = all.merged, family = binomial)
summary(all.merged.glmm5)

# > summary(all.merged.glmm5)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: accuracy ~ Experiment * block + (block + 1 | Subjecta) + (1 | targetobject) + (1 | targetaction) + (block + 0 | targetobject) + (block + 0 | targetaction)
# Data: all.merged
# 
# AIC      BIC   logLik deviance df.resid 
# 16496.7  16594.7  -8235.4  16470.7    13811 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -10.0574  -0.9978   0.4056   0.8597   1.4927 
# 
# Random effects:
# Groups           Name      Variance  Std.Dev. Corr 
# Subjecta       (Intercept) 4.472e-02 0.211469      
#                block       2.577e-02 0.160537 -0.68
# targetobject   (Intercept) 8.443e-03 0.091888      
# targetaction   (Intercept) 1.531e-02 0.123738      
# targetobject.1 block       9.146e-05 0.009564      
# targetaction.1 block       5.180e-06 0.002276      
# Number of obs: 13824, groups:  Subjecta, 48; targetobject, 9; targetaction, 9
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -0.26690    0.15475  -1.725   0.0846 .  
# Experimentnounverb       -0.01936    0.17988  -0.108   0.9143    
# Experimentverbonly        0.26598    0.20307   1.310   0.1903    
# block                     0.18712    0.04214   4.441 8.97e-06 ***
#   Experimentnounverb:block  0.01170    0.05943   0.197   0.8439    
# Experimentverbonly:block -0.12867    0.05967  -2.156   0.0311 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# we can check if increasing model complexity improves model fit:-
# -- in all these comparisons, we are comparing models with the same fixed effects but different random effects


# first, compare a model (glmm3) with just random effects of subjects or items on intercepts vs. a model with 
# random effects of subjects or items on intercepts and on the slopes of the fixed effect of block, allowing also
# for correlations between all random intercepts and slopes effects

anova(all.merged.glmm3, all.merged.glmm4)

# > anova(all.merged.glmm3, all.merged.glmm4)
# Data: all.merged
# Models:
# all.merged.glmm3: accuracy ~ Experiment * block + (1 | Subjecta) + (1 | targetobject) + 
# all.merged.glmm3:     (1 | targetaction)
# all.merged.glmm4: accuracy ~ Experiment * block + (block + 1 | Subjecta) + (block + 
# all.merged.glmm4:     1 | targetobject) + (block + 1 | targetaction)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# all.merged.glmm3  9 16888 16956 -8434.9    16870                             
# all.merged.glmm4 15 16500 16613 -8235.1    16470 399.49      6  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# # the models are significantly different indicating that a model including random intercepts and slopes, and taking into
# account correlations between the random effects is a useful improvement, fitting the data better, compared to a model
# with just random intercepts


# -- we have seen however that for items, the correlations between random effects of items (objects or actions) on intercepts
# and on slopes are quite extreme -- correlations of 1 -- suggesting that there are not sufficient data to support the level
# of complexity specified for model glmm4
# -- can we simplify the model by removing the correlations?
# -- we will know that removing the correlations is alright if there is *no* difference between the likelihood of the full
# model (random effects of subjects, random effects of items on intercepts, and on slopes, and the correlations) and the less 
# complex model (random effects of subjects, random effects of items on intercepts and on slopes, no correlations)
# -- this is because if there is no difference between the models then the specification of correlations is not doing anything
# useful

# compare models with random intercepts and slopes, and either correlations between all random intercepts and slopes (glmm4) or r
# random effects and just 
# correlations between intercepts and slopes for subjects random effects not for items random effects (glmm5)
# -- if there is no difference between these models in relative fit then the item random effects correlations do not add anything 
# to model utility 

anova(all.merged.glmm4, all.merged.glmm5)

# > anova(all.merged.glmm4, all.merged.glmm5)
# Data: all.merged
# Models:
#   all.merged.glmm5: accuracy ~ Experiment * block + (block + 1 | Subjecta) + (1 | 
#   all.merged.glmm5:     targetobject) + (1 | targetaction) + (block + 0 | targetobject) + 
#   all.merged.glmm5:     (block + 0 | targetaction)
# all.merged.glmm4: accuracy ~ Experiment * block + (block + 1 | Subjecta) + (block + 
#   all.merged.glmm4:     1 | targetobject) + (block + 1 | targetaction)
# Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# all.merged.glmm5 13 16497 16595 -8235.4    16471                         
# all.merged.glmm4 15 16500 16613 -8235.1    16470 0.4755      2     0.7884

# there is no difference so probably we do not need the item-wise random effects correlations

# so it looks like random slopes improve model fit, but there is no difference between the utility of models with and without item-wise
# correlations between random slopes and intercepts -- see the bolker comment, above, for discussion over why this might be expected
# -- it is better to have correlations between subjects-wise random intercepts and slopes so we keep that in: select model glmm5


# note that we can remove the correlation between random intercepts and slopes as do not seem to be contributing to the model
# -- all else being equal, we can prefer the simpler model

summary(all.merged.glmm5)
confint(all.merged.glmm5, method = "Wald")

> summary(all.merged.glmm5)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: accuracy ~ Experiment * block + (block + 1 | Subjecta) + (1 |  
# targetobject) + (1 | targetaction) + (block + 0 | targetobject) +      (block + 0 | targetaction)
# Data: all.merged
# 
# AIC      BIC   logLik deviance df.resid 
# 16496.7  16594.7  -8235.4  16470.7    13811 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -10.0574  -0.9978   0.4056   0.8597   1.4927 
# 
# Random effects:
#   Groups         Name        Variance  Std.Dev. Corr 
# Subjecta       (Intercept) 4.472e-02 0.211469      
# block       2.577e-02 0.160537 -0.68
# targetobject   (Intercept) 8.443e-03 0.091888      
# targetaction   (Intercept) 1.531e-02 0.123738      
# targetobject.1 block       9.146e-05 0.009564      
# targetaction.1 block       5.180e-06 0.002276      
# Number of obs: 13824, groups:  Subjecta, 48; targetobject, 9; targetaction, 9
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -0.26690    0.15475  -1.725   0.0846 .  
# Experimentnounverb       -0.01936    0.17988  -0.108   0.9143    
# Experimentverbonly        0.26598    0.20307   1.310   0.1903    
# block                     0.18712    0.04214   4.441 8.97e-06 ***
#   Experimentnounverb:block  0.01170    0.05943   0.197   0.8439    
# Experimentverbonly:block -0.12867    0.05967  -2.156   0.0311 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Exprmntn Exprmntv block  Exprmntn:
#   Exprmntnnvr -0.819                                   
# Exprmntvrbn -0.762  0.677                            
# block       -0.329  0.279    0.251                   
# Exprmntnnv:  0.230 -0.401   -0.175   -0.700          
# Exprmntvrb:  0.232 -0.197   -0.339   -0.706  0.495   
# > confint(all.merged.glmm5, method = "Wald")
# 2.5 %      97.5 %
#   (Intercept)              -0.5702025  0.03640946
# Experimentnounverb       -0.3719192  0.33319044
# Experimentverbonly       -0.1320364  0.66399534
# block                     0.1045304  0.26971482
# Experimentnounverb:block -0.1047718  0.12817001
# Experimentverbonly:block -0.2456217 -0.01171756
