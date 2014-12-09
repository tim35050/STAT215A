library(knitr) 
library(ggplot2)
library(dplyr)
library(foreach)
library(doParallel)
library(reshape)
library(reshape2)
library(corrplot)
library(rgl)
library(MASS)

fit.feat.sub <- fit_feat[1:5, ]
fit.feat.sub[1:5,10880:10900]
ridge <- lm.ridge(y ~ x1+x2+x3, lambda = seq(0, .1, .001))