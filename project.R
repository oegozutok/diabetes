library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)

# import data, check head
data=read.csv('diabetes_binary_health_indicators_BRFSS2015.csv')
head(data)

# diabetes binary column

data = data %>% mutate(Diabetes_binary=(Diabetes_012==1))
data$Diabetes_binary=as.integer(data$Diabetes_binary)

yesDiabetes <- filter(Data, Data$Diabetes_binary == 1)
noDiabetes <- filter(Data, Data$Diabetes_binary == 0)

# education barplot

##1. Never attended school or only kindergarten
##2. Grades 1 through 8 (Elementary)
##3. Grades 9 through 11 (Some high school)
##4. Grade 12 or GED (High school graduate)
##5. College 1 year to 3 years (Some college or technical school)
##6. College 4 years or more (College graduate)

yesKindergartenOrNone <- filter(yesDiabetes, yesDiabetes$Education == 1)
yesElementary <- filter(yesDiabetes, yesDiabetes$Education == 2)
yesSomeHighSchool <- filter(yesDiabetes, yesDiabetes$Education == 3)
yesHighSchoolGraduate <- filter(yesDiabetes, yesDiabetes$Education == 4)
yesSomeCollege <- filter(yesDiabetes, yesDiabetes$Education == 5)
yesCollegeGraduate <- filter(yesDiabetes, yesDiabetes$Education == 6)

noKindergartenOrNone <- filter(noDiabetes, noDiabetes$Education == 1)
noElementary <- filter(noDiabetes, noDiabetes$Education == 2)
noSomeHighSchool <- filter(noDiabetes, noDiabetes$Education == 3)
noHighSchoolGraduate <- filter(noDiabetes, noDiabetes$Education == 4)
noSomeCollege <- filter(noDiabetes, noDiabetes$Education == 5)
noCollegeGraduate <- filter(noDiabetes, noDiabetes$Education == 6)


barplot(table(noDiabetes$Education), main="Diabetes Based on Education",
        names.arg=c("1","2","3","4","5","6"), ylim = c(0, 90000) ,width = 0.05, col = "light blue", xlab = "Education Level", ylab = "Number of People")
par(new=TRUE)
barplot(table(yesDiabetes$Education),
        names.arg=c(""),ylim = c(0,90000), width = 0.05, col = "pink")
legend("topleft",cex=.5, pt.cex = 1, c("1: Never attended school or only kindergarten", "2: Grades 1 through 8 (Elementary) "
                                       ,"3: Grades 9 through 11 (Some high school)", "4: Grade 12 or GED (High school graduate)"
                                       , "5: College 1 year to 3 years (Some college or technical school)", "6: College 4 years or more (College graduate)") )
legend("left", c("No Diabetes", "Diabetes"), fill=c("light blue", "pink"))

#age barplot

yesAge1<- filter(yesDiabetes, yesDiabetes$Age == 1)
yesAge2<- filter(yesDiabetes, yesDiabetes$Age == 2)
yesAge3<- filter(yesDiabetes, yesDiabetes$Age == 3)
yesAge4<- filter(yesDiabetes, yesDiabetes$Age == 4)
yesAge5<- filter(yesDiabetes, yesDiabetes$Age == 5)
yesAge6<- filter(yesDiabetes, yesDiabetes$Age == 6)
yesAge7<- filter(yesDiabetes, yesDiabetes$Age == 7)
yesAge8<- filter(yesDiabetes, yesDiabetes$Age == 8)
yesAge9<- filter(yesDiabetes, yesDiabetes$Age == 9)
yesAge10<- filter(yesDiabetes, yesDiabetes$Age == 10)
yesAge11<- filter(yesDiabetes, yesDiabetes$Age == 11)
yesAge12<- filter(yesDiabetes, yesDiabetes$Age == 12)
yesAge13<- filter(yesDiabetes, yesDiabetes$Age == 13)

noAge1<- filter(noDiabetes, noDiabetes$Age == 1)
noAge2<- filter(noDiabetes, noDiabetes$Age == 2)
noAge3<- filter(noDiabetes, noDiabetes$Age == 3)
noAge4<- filter(noDiabetes, noDiabetes$Age == 4)
noAge5<- filter(noDiabetes, noDiabetes$Age == 5)
noAge6<- filter(noDiabetes, noDiabetes$Age == 6)
noAge7<- filter(noDiabetes, noDiabetes$Age == 7)
noAge8<- filter(noDiabetes, noDiabetes$Age == 8)
noAge9<- filter(noDiabetes, noDiabetes$Age == 9)
noAge10<- filter(noDiabetes, noDiabetes$Age == 10)
noAge11<- filter(noDiabetes, noDiabetes$Age == 11)
noAge12<- filter(noDiabetes, noDiabetes$Age == 12)
noAge13<- filter(noDiabetes, noDiabetes$Age == 13)

noAgeLength1<- length(noAge1$Age)
noAgeLength2<- length(noAge2$Age)
noAgeLength3<- length(noAge3$Age)
noAgeLength4<- length(noAge4$Age)
noAgeLength5<- length(noAge5$Age)
noAgeLength6<- length(noAge6$Age)
noAgeLength7<- length(noAge7$Age)
noAgeLength8<- length(noAge8$Age)
noAgeLength9<- length(noAge9$Age)
noAgeLength10<- length(noAge10$Age)
noAgeLength11<- length(noAge11$Age)
noAgeLength12<- length(noAge12$Age)
noAgeLength13<- length(noAge13$Age)

yesAgeLength1<- length(yesAge1$Age)
yesAgeLength2<- length(yesAge2$Age)
yesAgeLength3<- length(yesAge3$Age)
yesAgeLength4<- length(yesAge4$Age)
yesAgeLength5<- length(yesAge5$Age)
yesAgeLength6<- length(yesAge6$Age)
yesAgeLength7<- length(yesAge7$Age)
yesAgeLength8<- length(yesAge8$Age)
yesAgeLength9<- length(yesAge9$Age)
yesAgeLength10<- length(yesAge10$Age)
yesAgeLength11<- length(yesAge11$Age)
yesAgeLength12<- length(yesAge12$Age)
yesAgeLength13<- length(yesAge13$Age)

barplot(table(noDiabetes$Age), main="Diabetes Based on Age",
        names.arg=c("1","2","3","4","5","6","7","8","9","10","11","12","13"), ylim = c(0, 30000) ,width = 0.05, col = "light blue", xlab = "Age Level", ylab = "Number of People")
par(new=TRUE)
barplot(table(yesDiabetes$Age),
        names.arg=c(""),ylim = c(0,30000), width = 0.05, col = "pink")
legend("topleft",cex=.5, pt.cex = 1, c("1: Age 18 to 24", "2: Age 25 to 29", "3: Age 30 to 34", "4: Age 35 to 39", "5: Age 40 to 44", "6: Age 45 to 49", "7: Age 50 to 54", "8: Age 55 to 59", "9: Age 60 to 64", "10: Age 65 to 69", "11: Age 70 to 74", "12: Age 75 to 79", "13: Age 80 or older") )
legend("topright", c("No Diabetes", "Diabetes"), fill=c("light blue", "pink"))

table(
  ratio1<- yesAgeLength1/noAgeLength1,
  ratio2<- yesAgeLength2/noAgeLength2,
  ratio3<- yesAgeLength3/noAgeLength3,
  ratio4<- yesAgeLength4/noAgeLength4,
  ratio5<- yesAgeLength5/noAgeLength5,
  ratio6<- yesAgeLength6/noAgeLength6,
  ratio7<- yesAgeLength7/noAgeLength7,
  ratio8<- yesAgeLength8/noAgeLength8,
  ratio9<- yesAgeLength9/noAgeLength9,
  ratio10<- yesAgeLength10/noAgeLength10,
  ratio11<- yesAgeLength11/noAgeLength11,
  ratio12<- yesAgeLength12/noAgeLength12,
  ratio13<- yesAgeLength13/noAgeLength13)
highRatio <- ratio8 + ratio9 + ratio10 + ratio11
lowRatio <- ratio1  + ratio2 + ratio3 + ratio4

highRatio
lowRatio
yesAge1

# bmi histogram
yesDiabetes <- filter(data, data$Diabetes_binary == 1)
noDiabetes <- filter(data, data$Diabetes_binary == 0)

bmiNoDiabetes <- noDiabetes$BMI
bmiYesDiabetes <- yesDiabetes$BMI

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,160,203, max = 255, alpha = 80, names = "lt.pink")

hist(bmiYesDiabetes, col = c2, ylim = c(0,80000),xlim = c(10,60), xlab = "BMI", main = "BMI Frequency between Diabetes and No Diabetes")
par(new = TRUE)
hist(bmiNoDiabetes, col = c1, ylim = c(0,80000),xlim = c(10,60), xlab ="", main = "")
legend("topright", c("No Diabetes", "Diabetes"), fill=c(c1, c2))

# gender barplot
yesDiabetes <- filter(data, data$Diabetes_binary == 1)
noDiabetes <- filter(data, data$Diabetes_binary == 0)

femaleYes <- filter(yesDiabetes, yesDiabetes$Sex == 1)
femaleNo <- filter(noDiabetes, noDiabetes$Sex == 1)
maleYes <- filter(yesDiabetes, yesDiabetes$Sex == 0)
maleNo <- filter(noDiabetes, noDiabetes$Sex == 0)

length(femaleYes$Sex)
length(femaleNo$Sex)

barplot(height = length(femaleNo$Sex))

counts <- table(yesDiabetes$Sex)
counts2 <- table(noDiabetes$Sex)

barplot(counts2, main="Diabetes Based on Gender",
        names.arg=c("Female", "Male"), width = 0.05, col= c(c1,c1))
barplot(counts,
        names.arg=c("", ""), width = 0.05, col=c(c2 , c2), add=T)
legend("topright", c("No Diabetes", "Diabetes"), fill=c(c1, c2))

# compute correlation matrix of variables
# melt matrix

cormatr=round(cor(data),3)
melted_corm <- melt(cormatr)

# generate correlation heatmap
ggplot(data = melted_corm, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# bmi t-test
t.test(bmiYesDiabetes,bmiNoDiabetes)

# age t-test
ageNoDiabetes = noDiabetes$Age
ageYesDiabetes = yesDiabetes$Age
t.test(ageYesDiabetes,ageNoDiabetes)

# education t-test
edNoDiabetes = noDiabetes$Education
edYesDiabetes = yesDiabetes$Education
t.test(edYesDiabetes,edNoDiabetes)

# chi square test for gender
data.frame(df, stringsAsFactors = TRUE)
malec=c(nrow(maleNo),nrow(maleYes))
femalec=c(nrow(femaleNo),nrow(femaleYes))
genderdf=data.frame(femalec,malec)
rownames(genderdf)=c('no diabetes','diabetes')
chisq.test(genderdf)