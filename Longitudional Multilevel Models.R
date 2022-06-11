# Longitudional Multilevel Models  --------------------------------------

###################
# Clear Workspace #
###################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(lme4)

# With longitudinal models, the data structure takes the following form: repeated measurements (level 1) nested within the individual (level 2), and possibly 
# individual nested within some higher-level cluster (e.g. school) at level 3. Compared to regular multilevel modelling, the primary difference is that now we have 
# three different types of predictors: a time predictor, time-varying predictors, and time-invariant predictors. Given their unique role in longitudinal modeling, it 
# is worth spending just a bit of time defining each of these predictor types.

# The data to be used in the following examples come from the realm of educational testing. Examinees were given a language assessment at six equally spaced times. As 
# always, the data must first be read into R. In this case, the data are in the person-level data structure, in a file called Lang. This file includes the total 
# language achievement test score measured over six different measurement occasions (the outcome variable), four language subtest scores (writing process and 
# features, writing applications, grammar, and goals), and variables indicating student ID and school ID. Restructuring person-level data into person period format 
# in R can be accomplished by creating a new data frame from the person-level data using the stack command. All time-invariant variables will need to be copied into
# the new data file, while time-variant variables (e.g. all test scores measured over the six measurement occasions) will need to be stacked in order to create 
# person period format. The following R commands will read the Lang file into R and then rearrange the data into the necessary format

# Load Data ---------------------------------------------------------------

Lang <- read.csv("C:/R Portfolio/Language.csv")
glimpse(Lang)

LangPP <- data.frame(ID = Lang$ID, 
                     school = Lang$school, 
                     Process = Lang$Process,
                     Application = Lang$Application, 
                     Grammar = Lang$Grammar,
                     Goal = Lang$ Goal4RitScoref08,
                     stack(Lang, 
                     select = LangScore1:LangScore6))
glimpse(LangPP)

# We may wish to rename the values variable to Language. The values variable is the seventh column, so in order to rename it we would use the following R code:

names(LangPP)[7] <- "Language"
names(LangPP)

# We may also wish to recode the dedicated time variable, ind. Currently, this  variable is not recorded numerically, but takes on the values “LangScore1,” 
# “LangScore2,” “LangScore3,” “LangScore4,” “LangScore5,” “LangScore6”. Thus we may wish to recode the values to make a continuous numeric time predictor, as follows:

LangPP <- LangPP %>% mutate(Time = recode(ind, 
                                 "LangScore1" = '0',
                                 "LangScore2" = '1',
                                 "LangScore3" = '2',
                                 "LangScore4" = '3',
                                 "LangScore5" = '4',
                                 "LangScore6" = '5'))  

# If we want to predict language over time in a longitudinal random intercepts model, we can use:

Model1.0 <- lmer(Language ~ Time + (1|ID), 
                 data = LangPP)

summary(Model1.0)

Linear mixed model fit by REML ['lmerMod']
Formula: Language ~ Time + (1 | ID)
Data: LangPP

REML criterion at convergence: 133939.4

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-6.6230 -0.5147  0.0230  0.5580  4.7314 

Random effects:
  Groups   Name        Variance Std.Dev.
ID       (Intercept) 232.87   15.260  
Residual              52.25    7.229  
Number of obs: 18228, groups:  ID, 3038

Fixed effects:
  Estimate Std. Error t value
(Intercept) 194.9503     0.3064  636.36
Time1         8.0142     0.1855   43.21
Time2         8.1001     0.1855   43.67
Time3        14.5820     0.1855   78.62
Time4        13.3845     0.1855   72.17
Time5        18.2047     0.1855   98.15

Correlation of Fixed Effects:
  (Intr) Time1  Time2  Time3  Time4 
Time1 -0.303                            
Time2 -0.303  0.500                     
Time3 -0.303  0.500  0.500              
Time4 -0.303  0.500  0.500  0.500       
Time5 -0.303  0.500  0.500  0.500  0.500

# It is important to point out that these results indicate a statistically significant positive relationship between time and performance on the language assessment 
# (t=99.45), such that scores increased over time. Also, the confidence intervals for the fixed and random effects all exclude 0, indicating that they are different 
# from 0 in the population, i.e. statistically significant.

# Adding predictors to the model also remains the same as in earlier examples, regardless of whether they are time varying or time invariant. For example, in order 
# to add Grammar, which is time varying, as a predictor of total language scores over time, we would use the following:

Model1.1 <- lmer(Language ~ Time + Grammar +(1|ID), LangPP)

summary(Model1.1)

Linear mixed model fit by REML ['lmerMod']
Formula: Language ~ Time + Grammar + (1 | ID)
Data: LangPP

REML criterion at convergence: 128795.9

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-6.8388 -0.5399  0.0241  0.5762  4.5512 

Random effects:
  Groups   Name        Variance Std.Dev.
ID       (Intercept) 36.87    6.072   
Residual             52.25    7.228   
Number of obs: 18216, groups:  ID, 3036

Fixed effects:
  Estimate Std. Error t value
(Intercept) 71.438086   1.094880   65.25
Time1        8.013834   0.185526   43.20
Time2        8.099473   0.185526   43.66
Time3       14.578393   0.185526   78.58
Time4       13.381094   0.185526   72.12
Time5       18.202240   0.185526   98.11
Grammar      0.630888   0.005523  114.23

Correlation of Fixed Effects:
  (Intr) Time1  Time2  Time3  Time4  Time5 
Time1   -0.085                                   
Time2   -0.085  0.500                            
Time3   -0.085  0.500  0.500                     
Time4   -0.085  0.500  0.500  0.500              
Time5   -0.085  0.500  0.500  0.500  0.500       
Grammar -0.988  0.000  0.000  0.000  0.000  0.000

# From these results, we see that, again, Time is positively related to scores on the language assessment, indicating that they increased over time. In addition, 
# Grammar is also statistically significantly related to language test scores (t=114.23), meaning that higher grammar test scores were associated with higher 
# language scores. 

# We could add a third level of data structure to this model by including information regarding schools, within which examinees are nested. To fit this model we 
# use the following code in R:

Model1.3 <- lmer(Language ~ Time + (1|school/ID), 
                 data = LangPP)

summary(Model1.3)

Linear mixed model fit by REML ['lmerMod']
Formula: Language ~ Time + (1 | school/ID)
Data: LangPP

REML criterion at convergence: 133414.1

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-6.6758 -0.5142  0.0251  0.5596  4.7260 

Random effects:
  Groups    Name        Variance Std.Dev.
ID:school (Intercept) 187.91   13.708  
school    (Intercept)  69.11    8.313  
Residual               52.25    7.229  
Number of obs: 18228, groups:  ID:school, 3038; school, 35

Fixed effects:
  Estimate Std. Error t value
(Intercept) 195.0724     1.4830  131.54
Time1         8.0142     0.1855   43.21
Time2         8.1001     0.1855   43.67
Time3        14.5820     0.1855   78.62
Time4        13.3845     0.1855   72.17
Time5        18.2047     0.1855   98.15

Correlation of Fixed Effects:
  (Intr) Time1  Time2  Time3  Time4 
Time1 -0.063                            
Time2 -0.063  0.500                     
Time3 -0.063  0.500  0.500              
Time4 -0.063  0.500  0.500  0.500       
Time5 -0.063  0.500  0.500  0.500  0.500

# Using the anova() command, we can compare the fit of the three-level and two-level versions of this model.

anova(Model1.0, Model1.3)

Data: LangPP
Models:
  Model1.0: Language ~ Time + (1 | ID)
Model1.3: Language ~ Time + (1 | school/ID)
npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)    
Model1.0    8 133945 134008 -66965   133929                         
Model1.3    9 133425 133496 -66704   133407 521.99  1  < 2.2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Given that the AIC for Model1.3 is lower than that for Model1.0 where school is not included as a variable, and the chi-square test for deviance is significant 
# (χ2=521.99, p<.001), we can conclude that inclusion of the school level of the data leads to better model fit.
