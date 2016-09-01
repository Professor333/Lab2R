#Create two folders, one for wilcox and one for T-tests, set your WD to T-test folder first
setwd("C:/Users/Ryan/Development/R/BIO 614/Lab2/T-test")
library(car)

a=read.csv("1sampleTtest1tailed.csv", header = TRUE)
attach(a)
a=a$x
a
hist(a)
#Collect histogram
qqPlot(a)
#COllect QQ

boxplot(a)
means1=mean(a)
points(means1,col="red",pch=18)
#Collect boxplot

b=read.csv("1sampleTtest2tailed.csv", header = T)
attach(b)
b=b$x


par(mfrow= c(1,2))

boxplot(a)
means1=mean(a)
points(means1,col="red",pch=18)

boxplot(b)
means2=mean(b)
points(means2,col="blue",pch=18)
#Collect boxpliots

var.test(a, b) #This is the F-test
#Collect F value and p value

c=read.csv("2sampleTtest1tailed1.csv", header = T)
attach(c)
c=c$x

d=read.csv("2sampleTtest1tailed2.csv", header = T)
attach(d)
d=d$x

e=read.csv("2sampleTtest1tailedNEV1.csv", header = T)
attach(e)
e=e$x

f=read.csv("2sampleTtest1tailedNEV2.csv", header = T)
attach(f)
f=f$x

g=read.csv("2sampleTtest2tailed1.csv", header = T)
attach(g)
g=g$x

h=read.csv("2sampleTtest2tailed2.csv", header = T)
attach(h)
h=h$x

i=read.csv("2sampleTtest2tailedNEV1.csv", header = T)
attach(i)
i=i$x

j=read.csv("2sampleTtest2tailedNEV2.csv", header = T)
attach(j)
j=j$x

dev.off()
par(mfrow= c(2,5))

hist(a)
hist(b)
hist(c)
hist(d)
hist(e)
hist(f)
hist(g)
hist(h)
hist(i)
hist(j)
#Collect Histograms for normailty observation
qqPlot(a)
qqPlot(b)
qqPlot(c)
qqPlot(d)
qqPlot(e)
qqPlot(f)
qqPlot(g)
qqPlot(h)
qqPlot(i)
qqPlot(j)
#Collect qqplots for normailty observation


#Normalcy Pass Question 1 #Sample mean greater than pop. mean? Yes
t.test(a, alternative = "greater", mu = 2000, paired = FALSE, var.equal = FALSE)
#Are my samples normal?
dev.off()
hist(a)
#Histogram looks normal
qqPlot(a)
#QQplot looks normal

#Do the means in the boxplot look sig different? No, explain
boxplot(a)
means1=mean(a)
points(means1,col="red",pch=18)

#Problem 2: Sample mean different from pop. mean?
t.test(b, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE)
#Yes, sample mean is different.

#Are my samples normal?
hist(b)
#Collect PLot
qqPlot(b)
#Collect Plot
#Yes, sample appears to be normal

#Do the means in the box plot look sig different? No
boxplot(b)
means2=mean(b)
points(means2,col="red",pch=18)
mean(b)
means2
#Note, the point on the box plot does not look like it is a negative, 
#but it is, see the last two calls

#Problem 3 Uses variable c and d

#Is my first sample mean less than my second sample mean? Yes
t.test(c, d, alternative = "less", mu = 0, paired = FALSE, var.equal = TRUE)

#Are my samples normal? D looks a little bit off of normal
par(mfrow=c(1,2))
hist(c)
hist(d)
#Collect Histograms
qqPlot(c)
qqPlot(d)
#Collect QQplots

#Do they have equal variances? Why? (show code and p-value)
var.test(c,d)
#True ratio of variances is not equal...ratio looks decent, 
#but the p-value is terrible
#Ratio of variances: 0.9031173
#P-value: 0.6132
#Maybe include the code for defining c and d

#Am I perfomring one-tailed or two-tailed? One-tailed, explain. One-tailed is the answer.

#Do the means in box plots look significantly different? No it doesn't
boxplot(c)
means3=mean(c)
points(means3,col="red",pch=18)

boxplot(d)
means4=mean(d)
points(means4,col="red",pch=18)
#Collect Plots

#Problem 4 uses variables g and h
#Is first sample mean different from second sample mean? Yes
t.test(g, h, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE)



#Problem 5 uses variables e and f
#Is first sample mean greater than second sample mean? Yes
t.test(g, h, alternative = "greater", mu = 0, paired = FALSE, var.equal = FALSE)


#Problem 6 uses variables i and j
#Is first sample mean different from second sample mean? Yes
t.test(i, j, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE)


setwd("C:/Users/Ryan/Development/R/BIO 614/Lab2/Wilcox")
aa=read.csv("wilcox1.csv", header = T)
attach(aa)
aa=aa$x

bb=read.csv("wilcox2.csv", header = T)
attach(bb)
bb=bb$x

#Problem 7	Wilcox. Q: Is first sample mean different from second sample mean? Yes
t.test(aa, bb, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE)

#Problem 8 Generate random numbers
randomA=rnorm(100, mean=0, sd=2000)
randomB=rnorm(100, mean=100, sd=4000)

t.test(randomA, randomB, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE)
#Collect P-value
t.test(randomA, randomB, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE)
#collect P-value
t.test(randomA, randomB, alternative = "greater", mu = 100, paired = FALSE, var.equal = FALSE)
#Collect P-value
t.test(randomA, randomB, alternative = "greater", mu = 100, paired = FALSE, var.equal = TRUE)
#Collect P-value

p1=0.9013
p2=0.9012
p3=0.6363
p4=0.6364
pvalues=c(p1,p2,p3,p4)
plot(pvalues)


