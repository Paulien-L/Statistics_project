#Project 6: Sleep deprivation

sleep <- read.table(file = file.choose(), header = TRUE)

attach(sleep)

#Boxplot
library(ggplot2)
plot1 <- ggplot(sleep, aes(x=factor(Days),y=Reaction)) + geom_boxplot(colour="black",fill="white")
plot1

#Spaghetti plot
interaction.plot(sleep$Days,sleep$Subject,sleep$Reaction, xlab="Days", ylab="Reaction time",legend=F)

#Mean, sd, var
sleep.mean=tapply(Reaction,list(Days),mean)
sleep.sd=tapply(Reaction,list(Days),sd)
sleep.var=tapply(Reaction,list(Days),var)

#Correlation Reaction and Days
sleep2 <- reshape(sleep,timevar = "Days", idvar = "Subject", direction = "wide")
view(sleep2)

cor(sleep2[,2:11])

#Trellis plot: individual linear regression
library(lattice)
cf<-sapply(sleep$Subject, function(x) coef(lm(Reaction~Days, data=subset(sleep, Subject==x))))

Sx<-reorder(Subject, cf[1,])

plot3<-xyplot(Reaction ~ Days|Sx,data=sleep,
       type=c("p","r"),auto.key=T,aspect="xy",
       par.settings=list(axis.text=list(cex=0.6),fontsize=list(text=8, points=10)),
       scales=list(x=list(at=c(0,1,2,3,4,5,6,7,8,9),labels=c("0","1","2","3","4","5","6","7","8","9"))))

#Histograms
#coef intercept and slope:
coef <- by(sleep, sleep$Subject, function(data) coef(lm(Reaction ~ Days, data=data)))
coef1 <- unlist(coef)
names(coef1) <- NULL
coef2=matrix(coef1,length(coef1)/2,2,byrow = TRUE)
#r2:
r.squared <- by(sleep, sleep$Subject, function(data) summary(lm(Reaction ~ Days, data=data))$r.squared )
squared1<- as.vector(unlist(r.squared))
#hist:
par(mfrow=c(3,1))
hist(coef2[,1],xlab="Intercept",col="lightblue",main="individual intercepts")
hist(coef2[,2],xlab="Slope",col="lightblue",main="individual slopes")
hist(squared1,xlab="R squared",col="lightblue",main="individual R squared")

#Individual regression lines
par(mfrow=c(1,1))
mean.int=mean(coef2[,1])
mean.slope=mean(coef2[,2])

plot(sleep$Days, sleep$Reaction, type="n")
box()
for(i in 1:18)
  curve(cbind(1,x)%*%coef2[i,1:2], add=T, col="gray")
curve(cbind(1,x)%*%c(mean.int[1],mean.slope[1]),add=T,lwd=2)

#REML model (lme4, arm package)
library(lme4)
library(arm)
sleep.lmer <- lmer(Reaction ~ Days + (1 + Days | Subject), data=sleep)
coef.lmer=coef(sleep.lmer)
display(sleep.lmer)
summary(sleep.lmer)

#Confidence intervals for fixed effects with bootstrap
sleep.lmer.conf <- confint(sleep.lmer,method="boot",boot.type ="perc",oldNames = FALSE,nsim=500)

#Getting p-values (package: pbkrtest)
library(pbkrtest)
sleep.lmer.df.KR <- get_ddf_Lb(sleep.lmer, fixef(sleep.lmer))
sleep.lmer.coef <- coef(summary(sleep.lmer))
sleep.lmer.pKR <- cbind(sleep.lmer.coef,2 * (1 - pt(abs(sleep.lmer.coef[,3]), sleep.lmer.df.KR)))

#Comparing individual linear regression estimates with LMM estimates
ind.coef=coef(sleep.lmer)$Subject

#Separate scatter plots
par(mfrow=c(1,2))
plot(coef2[,1],coef2[,2], xlab="Intercept", ylab="Slope", main="Individual regression estimates")
plot(ind.coef[,1],ind.coef[,2], xlab="Intercept", ylab="Slope", main="LMM estimates")

#Overlayed scatter plots with arrows
par(mfrow=c(1,1))
plot(coef2[,1],coef2[,2], xlab="Intercept", ylab="Slope", main="Individual OLS vs. LMM estimates")
points(ind.coef[,1],ind.coef[,2], col=4)
legend("bottomleft",inset=.05,legend=c("OLS","LMM"), fill=c("black","blue"))
arrows(coef2[,1],coef2[,2],x1=ind.coef[,1],y1=ind.coef[,2], col="gray")
