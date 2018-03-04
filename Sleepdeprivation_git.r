#Statistics project: sleep deprivation

#Summary statistics
#Boxplot
plot1 <- ggplot(sleep, aes(x=factor(Days),y=Reaction)) + geom_boxplot(colour="black",fill="white")
plot1
#Overall increasing trend in reaction time as days progress
#But variance in reaction time seems to increase as days progress as well
#Very notably many outliers on day 9

#Spaghetti plot
interaction.plot(sleep$Days,sleep$Subject,sleep$Reaction, xlab="Days", ylab="Reaction time",legend=F)
#Overall again an increasing trend as days progress
#Some individuals with swings and one or two where reaction time on day 9 is lower (presumably outliers)
#Also two individuals where reaction time is much higher than others

#Mean, sd, var
sleep.mean=tapply(Reaction,list(Days),mean)
sleep.sd=tapply(Reaction,list(Days),sd)
sleep.var=tapply(Reaction,list(Days),var)
#Note increase in mean as days progress, but also in sd and var
#So reaction time is slower on average as days progress
#But the differences between the individuals in our sample also get larger

#Reshaping the data into a wide form
sleep2 <- reshape(sleep,timevar = "Days", idvar = Subject, direction = "wide")
view(sleep2)

#Correlation between reaction time and measurement day
cor(sleep2[,2:10])
#In general, the correlation between days increases as days are further away in time

#Linear regression per person
#Model: Yij = p0i + p1i(Dayij) + eij
#Where 	Yij = reaction time of subject i at Dayij
#		poi = intercept of subject i on day 0
#		p1i = slope of subject i
#		eij = error term of subject i at Dayij

# Displaying the linear regression per person
cf<-sapply(sleep$Subject, function(x) coef(lm(Reaction~Days, data=subset(sleep, Subject==x))))
Sx<-reorder(sleep$Subject, cf[1,])

library(lattice)

xyplot(Reaction ~ Days|Sx,data=sleep,
       type=c("p","r"),auto.key=T,aspect="xy",
       par.settings=list(axis.text=list(cex=0.6),fontsize=list(text=8, points=10)),
       scales=list(x=list(at=c(0,1,2,3,4,5,6,7,8,9),labels=c("0","1","2","3","4","5","6","7","8","9"))))
#Good fit of a linear model
#Slope and intercept vary a lot among subjects

