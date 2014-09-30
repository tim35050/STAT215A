# STAT 215A
# TIMOTHY MEYERS
# LAB 0

# DECLARE LIBRARIES
library('ggplot2')

# LOAD DATA
data(USArrests)
statecoord <- read.table("stateCoord.txt")

# CLEANSE AND MERGE DATA FRAMES
rownames(statecoord) = sub("-"," ",rownames(statecoord))
dfmerged <- merge(USArrests, statecoord, by=0)

# PLOT ASSAULT VS MURDER
assault.murder <- ggplot(dfmerged, aes(Assault, Murder))
assault.murder = assault.murder + geom_point()
assault.murder

# PLOT URBAN POPULATION VS RAPE
urbanpop.rape <- ggplot(dfmerged, aes(UrbanPop, Rape))
urbanpop.rape <- urbanpop.rape + geom_point()
urbanpop.rape

# ADD COLUMN TO IDENTIFY OUTLIER THEN PLOT AGAIN
dfmerged$highlight <- ifelse(dfmerged$Rape > 40 & dfmerged$UrbanPop < 50, 
                             "highlight", "normal")
urbanpop.rape <- urbanpop.rape + geom_point(aes(colour = highlight))
urbanpop.rape

# PLOT ASSAULT VS POPULATION USING ROW NAMES
assault.murder.text <- ggplot(dfmerged, 
                              aes(x=Assault, y=Murder, label=Row.names))
assault.murder.text <- assault.murder.text + geom_text()
assault.murder.text

# PLOT URBAN POPULATION VS RAPE USING ROW NAMES
urbanpop.rape.text <- ggplot(dfmerged, 
                             aes(x=UrbanPop, y=Rape, label=Row.names))
urbanpop.rape.text <- urbanpop.rape.text + geom_text()
urbanpop.rape.text

# FIT LINEAR REGRESSION OF URBAN POPULATION ON RAPE
fit <- lm(Rape ~ UrbanPop, data=dfmerged)
summary(fit)
plot(fit)

# MAKE DATA FRAME COLUMNS AVAILABLE AND PLOT THEM WITH 
# OVERLAID REGRESSION LINE
attach(dfmerged)
plot(UrbanPop, Rape)
abline(fit, col = 'blue')

# MAKE NEW DATA FRAME WITHOUT OUTLIER (IDENTIFIED AS ROW 2),
# REDO REGRESSION AND OVERLAY NEW REGRESSION LINE 
df.merged.no.outlier <- dfmerged[-2,]
fit.no.outlier <- lm(Rape ~ UrbanPop, data=df.merged.no.outlier)
abline(fit.no.outlier, col='red')

# CREATE FINAL PRESENTABLE GRAPH
plot(UrbanPop, 
     Rape, 
     xlab="Urban Population of US States (percent)", 
     ylab="Rape Incidents (thousands)")
title(main="Rape Incidents by Urban Population across US States")
legend(32, 42, 
       c("With Outlier", "Without Outlier"), 
       lty=c(1,1), 
       lwd=c(2.5,2.5), 
       col=c("blue","red"))
abline(fit, col='blue')
abline(fit.no.outlier, col='red')
