sleep <- read.csv('SleepStudyData.csv')


##Basic functions, getting overview of the data###
summary(sleep)
colnames(sleep)
unique(sleep$Enough)
unique(sleep$Hours)
unique(sleep$PhoneReach)
unique(sleep$PhoneTime)
unique(sleep$Tired)
unique(sleep$Breakfast)


######I will only use the best data/pattern on the blog####


############
###plots####
############

colors <- c ('red', 'green','blue','cyan','orange','yellow')

#frequency 

#i could have use a histogram but the range is 1 to 5, i feel barplot is sufficient
barplot(table(sleep$Tired), main="Frequency of Tired Levels", 
        xlab="Tired Levels", ylab="Frequency", col=colors)

barplot(table(sleep$Enough), main="Enough", 
        xlab="Yes/No", ylab="Frequency", col=colors)

barplot(table(sleep$PhoneReach), main="PhoneReach", 
        xlab="Yes/No", ylab="Frequency", col=colors)

barplot(table(sleep$PhoneTime), main="PhoneTime", 
        xlab="Yes/No", ylab="Frequency", col=colors)

barplot(table(sleep$Breakfast), main="Breakfast", 
        xlab="Yes/No", ylab="Frequency", col=colors)

hist(sleep$Hours, main="Hours of Sleep Frequency", xlab="Range", ylab="Frequency",col=colors);

#######################################
########variable vs fatigue levels#######
#######################################


boxplot(Hours ~ Tired, data=sleep, main="Hours of sleep vs Tired Levels", 
        xlab="Tired Levels", ylab="Hours of sleep", col=colors)

plot(sleep$Hours, sleep$Tired, main ="Hours and Tired Levels", xlab="Hours of Sleep", 
     ylab="Tired Levels")



boxplot(Tired ~ Enough, data=sleep, main="Enough sleep?", 
        xlab="Yes/No", ylab="Tired Levels", col=colors)


boxplot(Tired ~ PhoneReach, data=sleep, main="Phone within arm reach when sleeping?", 
        xlab="Yes/No", ylab="Tired Levels", col=colors)

boxplot(Tired ~ PhoneTime, data=sleep, main="Use phone within 30 mins of sleep?", 
        xlab="Yes/No", ylab="Tired Levels", col=colors)

boxplot(Tired ~ Enough, data=sleep, main="Enough sleep?", 
        xlab="Yes/No", ylab="Tired Levels", col=colors)

boxplot(Tired ~ Breakfast, data=sleep, main="Did you eat breakfast?", 
        xlab="Yes/No", ylab="Tired Levels", col=colors)


### using phone/ sleeping next to phone does not seem to have an impact on tired levels.


###MEAN, will not use median because sample is kind of small###

mhours <- tapply(sleep$Tired, sleep$Hours, mean)
barplot(mhours, main='Average Tired Levels on Hours Sleep', xlab='Hours Sleep', ylab='Tired Level', col=colors)

mtired <- tapply(sleep$Hours, sleep$Tired, mean)
barplot(mtired, main='Average Hours on Tired Levels', xlab='Tired Levels', ylab='Hours of Sleep', col=colors)

mbreakfast <- tapply(sleep$Tired, sleep$Breakfast, mean)
barplot(mbreakfast, main='Average Tired Levels on Breakfast', xlab='Yes/No', ylab='Tired Level', col=colors)

mReach <- tapply(sleep$Tired, sleep$PhoneReach, mean)
barplot(mReach, main='Average Tired Levels on PhoneReach', xlab='Yes/No', ylab='Tired Level', col=colors)

mTime <- tapply(sleep$Tired, sleep$PhoneTime, mean)
barplot(mTime, main='Average Tired Levels on PhoneTime', xlab='Yes/No', ylab='Tired Level', col=colors)



###Digging deeper - 
##Note the sample size is really small because of the restriction
##so I won't include this on blog.

barplot(table(sleep[sleep$Hours>=6 & sleep$Hours <9, ]$Breakfast), main="6-8 Hours of Sleep and Breakfast", 
        xlab="Did you eat breakfast?", ylab="Frequency", col=colors)

barplot(table(sleep[sleep$Hours>=6 & sleep$Hours <9 & sleep$Breakfast=="Yes",]$Tired), main="6-8 Hrs and Eat Breakfast", 
        xlab="Tired Levels", ylab="Frequency", col=colors)

barplot(table(sleep[sleep$Hours>=6 & sleep$Hours <9 & sleep$Breakfast=="No",]$Tired), main="6-8 Hrs and No Breakfast", 
        xlab="Tired Levels", ylab="Frequency", col=colors)

boxplot(Hours ~ Breakfast, data=sleep, main="Did you eat breakfast?", 
        xlab="Yes/No", ylab="Hours of Sleep", col=colors)


boxplot
###hypothesis testing####

##People that skip breakfast feel more tired than those that eat

PermutationTestSecond::Permutation(sleep, "Breakfast", "Tired", 1000, "Yes", "No")

##People that sleep with their phone next to them are tired are more tired than those that doesn't

PermutationTestSecond::Permutation(sleep, "PhoneReach", "Tired", 1000, "No", "Yes")

##People that use their phone within 30 minutes are more tired than those that doesn't


PermutationTestSecond::Permutation(sleep, "PhoneTime", "Tired", 1000, "No", "Yes")

#People that are not tired sleep more hours than those that are tired.

PermutationTestSecond::Permutation(sleep, "Tired", "Hours", 1000, "5", "1")

