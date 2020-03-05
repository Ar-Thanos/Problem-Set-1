#Below is HW Problem Set 1
#Question 1
numbers <- double()
for(i in 1:265){    #Create data set
  numbers[i] <- 0
}
for(i in 266:314){
  numbers[i] <- 1
}
for(i in 315:335){
  numbers[i] <- 2
}
for(i in 336:(335+19)){
  numbers[i] <- 3
}

for(i in (335+19):(335+29)){
  numbers[i] <- 4
}
for(i in (335+29):(335+39)){
  numbers[i] <- 5
}

num <- 374
numbers[num+1] <- 6
numbers[num+2] <- 6
numbers[num+3] <- 7
numbers[num+4] <- 7
numbers[num+5] <- 8
numbers[num+6] <- 8
numbers[num+7] <- 8
numbers[num+8] <- 8
num <- num+8
numbers[num+1] <- 9
numbers[num+2] <- 9
numbers[num+3] <- 10
numbers[num+4] <- 11
numbers[num+5] <- 11
numbers[num+6] <- 11
numbers[num+7] <- 11
numbers[num+8] <- 12
numbers[num+9] <- 12
numbers[num+10] <- 12
numbers[num+11] <- 13
numbers[num+12] <- 14
numbers[num+13] <- 14
IQR(numbers)
boxplot(numbers)
quantile(numbers)
counts <- table(numbers)
barplot(counts)
pdf("Problem_1_Bar_Chart.pdf")
dev.off()
pdf("Problem_1_Bar_Chart_.pdf")
barplot(counts, xlab = "Number of convictions", ylab = "Frequency")
dev.off()
barplot(counts, xlab = "Number of convictions", ylab = "Frequency")
pdf("Problem_1_Bar_Chart_Final.pdf")
hist(counts, right = FALSE, breaks = xlab = "Number of convictions", ylab = "Frequency")
barplot(counts, xlab = "Number of convictions", ylab = "Frequency")
dev.off()
#Problem 2
hemoglobinData <- read.csv(
url("http://whitlockschluter.zoology.ubc.ca/wp-content/
    data/chapter02/chap02e3cHumanHemoglobinElevation.csv"))
head(hemoglobinData)
table(hemoglobinData$population)
pdf("Problem_2_Hemoglobin_Box_Plot")
dev.off()
hemoglobinData <- read.csv(
url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e3cHumanHemoglobinElevation.csv"))
table(hemoglobinData$population)
barplot(table(hemoglobinData$population), xlab = "Countries", ylab = "Male Population")
pdf("Problem_2_Hemoglobin_MaleByCountry_BarChart")
barplot(table(hemoglobinData$population), xlab = "Countries", ylab = "Male Population")
dev.off()

table(hemoglobinData$hemoglobin)
barplot(table(hemoglobinData$hemoglobin))
pdf("Problem_2_Part_3_Histogram")
hist(hemoglobinData$hemoglobin, right = FALSE, breaks = seq(10,26,by=0.2), 
     xlab = "Hemoglobin Concentration", main = "Hemoglobin Concentration Counts")
dev.off()
#Problem 3
males <- double() #Generating list
females <- double() #Generating List

for(i in 1:30){
  females[i] = 0
}
for(i in 1:38){
  males[i] = 0
}
for(i in 31:55){
  females[i] = 1
}
for(i in 39:55){
  males[i] = 1
}
for(i in 56:58){
  females[i] = 2
}
for(i in 56:62){
  males[i] = 2
}
for(i in 59:64){
  females[i] = 3
}
for(i in 63:68){
  males[i] = 3
}
for(i in 65:72){
  females[i] = 4
}
for(i in 69:72){
  males[i] = 4
}
for(i in 73:76){
  females[i] = 5
}
for(i in 73:82){
  males[i] = 5
}
for(i in 83:84){
  males[i] = 6
}
for(i in 77:80){
  females[i] = 7
}
for(i in 81:81){
  females[i] = 8
}
mean(females)
mean(males)
var(females)
var(males)
#Problem 4
birdAbundanceData <- read.csv(
url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e2bDesertBirdAbundance.csv"))
head(birdAbundanceData)
mean(birdAbundanceData$abundance)
var(birdAbundanceData$abundance)
sd(birdAbundanceData$abundance)
(sd(birdAbundanceData$abundance)/mean(birdAbundanceData$abundance))*100 #Coefficient
#Problem 7
numbBeatles <- c(51, 45, 61, 76, 11, 117, 7, 132,52, 149)
mean(numbBeatles)
sd(numbBeatles)
sqrt(sd(numbBeatles)/length(numbBeatles)) #Standard Error
#Problem 9
dist <- rexp(100, rate = 0.05)
pdf("Problem9_Hist")
hist(dist, xlab = "Species Abundances", ylab = "Frequency of species abundance")
dev.off()
mean(dist)
sd(dist)
#Now to increase the size to 1000!
dist <- rexp(1000, rate = 0.05)
mean(dist)
sd(dist)
#For the huge sampling distribution step
sample <- double()
for(i in 1:10000){
  temp <- rexp(100, rate = 0.05)
  sample[i] <- mean(temp)
}
pdf("Problem9_Hist_SampleDistribution")
hist(sample, xlab = "Mean Species Abundances", ylab = "Frequency of means")
dev.off()
mean(sample)
sd(sample)
#For an even bigger sampling distrubtion 
sample <- double()
for(i in 1:10000){
  temp <- rexp(500, rate = 0.05)
  sample[i] <- mean(temp)
}
pdf("Problem9_Hist_SampleDistribution_500")
hist(sample, xlab = "Mean Species Abundances", ylab = "Frequency of means")
dev.off()
mean(sample)
sd(sample)
#Problem 10 - The end is in sight
#Making a null distribution
for(i in 1:10000){
  tempSample <- sample(c("Y", "N"), size = 220, 
              prob = c(0.14, 0.86), replace = TRUE) #give # L's and R's
  results18[i] <- sum(tempSample == "Y") 
  #Counts number of times Yes to cancer appears
}
pdf("Problem10_Null_PartA")
hist(results18, right = FALSE, 
     freq = FALSE, xlab="Number of individuals diagnosed with cancer on the set")
dev.off()
fracWithCancer <- sum(results18 >= 91)/length(results18)
fracWithCancer
#NOW TO DO IT WITH CHI SQUARE!
for(i in 1:10000){
  tempSample <- sample(c("Y", "N"), size = 220,
            prob = c(0.14, 0.86), replace = TRUE) #give vector of 18 Ls or Rs
  results18[i] <- ((sum(tempSample == "Y")-30.8)^2)/30.8
  + ((sum(tempSample == "N")-189.2)^2)/189.2 #Counts number of times R appears
}
pdf("Problem10_Null_PartB")
hist(results18, right = FALSE, freq = FALSE, xlab="Chi_Square Values")
dev.off()
(91-30.8)^2/30.8 + (129-189)^2/189 #Test_Stat_Chi_Square
fracWithCancer <- sum(results18 >= 136.7113)/length(results18)
fracWithCancer
#Now with the R-chisq
cancerStat <- c(91, 129) #I assume we just input the people with and without cancer
chisq.test(cancerStat, p = c(0.14,0.86)) #expected prob of developing cancer normally