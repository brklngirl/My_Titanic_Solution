## PLAN:

## - Study the data set
## - check for missing values
## - replace NA's
## - change variables types if needed
## - create additional vaues from what is given if needed
## - try and spot patterns
## - join both datasets into one

## - create a model to predict missing Age
## - create FamSize, AgeGroup variables
## - create initial glm(), find what variables contribute the most/the least; 
## - test model against test part of a train set

## - perform text analytics on Name, Tix, Cabin
## - find matches among people in same cabins
## - does survival depends on CabinCat?
## - does survival depends on a ticket number
## - what do letters in tickets mean
## - improve model

## set working directory
setwd("C:/Users/Lenovo/Documents/R/R WD")

##read in some data; read.csv or read.table by default reads our columns as factors

titanic.train <- read.csv(file = "train.csv", header = T, stringsAsFactors = F, na.strings = c(" "))
titanic.test <- read.csv(file = "test.csv", header = T, stringsAsFactors = F,na.strings = c(" "))

is.factor(titanic.train$Sex)
is.factor(titanic.train$Embarked)

# now we need to combine both data sets
# we create a new column T/F in both sets to differentiate them later if a part of a train or test set
titanic.train$IsTrainSet <- T
titanic.test$IsTrainSet <- F

## to check that T/F populated till the end, lets run tail() function
tail(titanic.train$IsTrainSet)

## now we are going to combine them, and both data sets have to line up, 
## but train dataset has one extra column:
ncol(titanic.train)
ncol(titanic.test)

#get column names, compare they spelled the same way:
names(titanic.train)
names(titanic.test)

#we add missing column to the test dataset and fill it with NA's
titanic.test$Survived <- NA

## now if we check number of columns we will get same number for both datasets
ncol(titanic.test)

## next we will combine both sets
titanic.full <- rbind(titanic.train, titanic.test)

## lets check tail again to see all the data till the end has populated
tail(titanic.full)

## lets check our full dataset isn't missing any values:
table(titanic.full$IsTrainSet)

## Lets study our data
str(titanic.full)

## There are plenty of NA's
colSums(is.na(titanic.train) | titanic.train == '')
## in train set the only column contains NA is Age, 177 missing values

## lets check test set
colSums(is.na(titanic.test) | titanic.test == '')

## let's combine those results, because when we will fill na's we will do it for the whole data frame anyways
colSums(is.na(titanic.full) | titanic.full == '')

## Cabin has the most NA's (1014), then Age (263), Emb and Fare have 2 and 1 NA respectively
## missmap function from Amelia package will visualize our missing values
install.packages("Amelia")
library("Amelia")
missmap(titanic.full, main = "Titanic Dataset - Missing Value", col = c("green", "black"), legend = F)

## from this chart we can see that Age is stored as NA's (= populated), while Cabin was not populated
## thus stored as missing values 

## Next step, we will try and fill missing values
## we'll beging with missing Fare - only one value
## Let's extract all date for that missing value (using filter() from dplyr package)
install.packages("dplyr")
library("dplyr")
filter(titanic.full, is.na(titanic.full$Fare)==T)

## this is a male passenger from 3rd class, who embarked in a port S
## we want to see what is a typical Fare was paid by similar passengers
install.packages("ggplot2")
library("ggplot2")

ggplot(filter(titanic.full, Pclass==3 & Embarked=="S"), aes(Fare)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "darkblue", linetype = "dashed", size =2) + 
  geom_vline(aes(xintercept = mean(Fare, na.rm = T)), colour = "red", linetype = "dashed", size =2) +
  ggtitle(("Fare distribution of 3rd class passengers embarked in Southampton")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

## the mean and median are very different, hovewer, we can see that majority of people
## who fit our criteria (a huge spike) paid meadian value; so we 'll go with it
titanic.full$Fare[is.na(titanic.full$Fare == T)] = median(filter(titanic.full, Pclass ==3 & Embarked == "S")$Fare, na.rm =T)

## I also want to check that there are no abnormalities among Fare values
summary(titanic.full$Fare)  

## We see that some rows indicate a Fare value = 0, let's see further
filter(titanic.full, Fare == 0)

## there are 17 instances, all are male btw 19 -40 y.o, mostly 36-40, traveling alone, emb in Southhampton
## 2 are from train set, among others only one person survived
## we will leave it as is for now





## Next I want to query just a missing Embarked values of a titanic.full
## so I want to filter all the Embarked that is NA , and i want only the Embarked column to come back
## based on a table that we built previously for Embarked column, there are 2 such values
filter(titanic.full, Embarked == "NA" | Embarked == '')

## both missing values belong to 2 ladies in 1st class with the same ticket number and cabin, @ Fare = $80
## now we want to replace them with something
## let's see the frequency of embarkation at each port by passengers of Pclass 1
table(filter(titanic.full, Pclass == 1)$Embarked)

## While Southhampton is the most frequent, Cherbourg is not that far down from it
## Let's confirm, that people at which port have paid a Fare close to what our ladies paid

ggplot(filter(titanic.full, is.na(Embarked) == F & Embarked != '' & Pclass ==1), aes(Embarked, Fare)) +
  geom_boxplot(aes(colour = Embarked)) +
  geom_hline(aes(yintercept =80), colour = 'red', linetype = 'dashed', size =2) +
  ggtitle("Fare distribution among 1st Class passengers") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## according to our boxplot, passengers from S port paid on avg $50,
## while passengers from C did pay about $80 as our ladies did, so we use this info to  fill blanks

titanic.full$Embarked[titanic.full$Embarked == ''] <- "C"

##So to check, we run table() again, blank values should be replaced
table(titanic.full$Embarked)

## Now we'll get to studying Age data
summary(titanic.full$Age)

## let's see if there is any difference in age between different Pclasses

ggplot(titanic.full, aes(Pclass, Age)) +
  geom_boxplot(aes(fill=factor(Pclass), alpha = 0.5)) +
  ggtitle("Age distribution within Pclasses") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Based on that chart, we can see that passengers in higher classes tend to be older
## Thus we want to fill NA's for each class based on avg value of that class

impute.age <- function (age, class) {
  vector <- age
  
  for ( i in 1:length(age)) {
    if(is.na(age[i])) {
      if(class[i]==1) {
        vector[i] <- round(mean(filter(titanic.full, Pclass == 1)$Age, na.rm =T), 0)
      } else if (class[i]==2) {
        vector[i] <- round(mean(filter(titanic.full, Pclass == 2)$Age, na.rm =T), 0)
    } else {
      vector[i] <- round(mean(filter(titanic.full, Pclass == 3)$Age, na.rm =T), 0)
  } 
    } else {
  vector[i] <- age[i]
    }
  } 
  return(vector)
}
  
imputed.age <- impute.age(titanic.full$Age, titanic.full$Pclass)
titanic.full$Age <- imputed.age

##if now we make a table(is.na(titanic.full$Age)) we should not have any missing values
table(is.na(titanic.full$Age))

## let's check if any gender had better chances at survival
table(titanic.train$Sex, titanic.train$Survived)


## let's find out if certain cabin class passengers got more chance of surviving
## would like to see it as proportions? just a thought for a future
prop.table(table(titanic.train$Pclass, titanic.train$Survived))

## I want to see if there is a difference btw sex survival by different Pclass
install.packages("scales")
install.packages("dplyr")
library("scales")
library("dplyr")

ggplot(titanic.full, aes(x = Pclass, y = Survived)) +
  geom_bar(aes(fill = factor(Sex)), stat = "identity", position="fill") +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1)) +
  ylab("Percentage") +
  ggtitle("Difference between gender survival by PClass") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
### Some day I will add lables there too :)

## We can see visually from our stacked barplot that more females than males srvvd in all Pclasses 
ggplot(filter(titanic.full, !is.na(Survived)), aes(x= Sex, y = Survived, group = factor(Pclass))) +
  geom_bar(aes(fill = factor(Pclass)), stat = "identity", position = "stack") +
  geom_point(aes(color = factor(Pclass)))

## However, we want to see, what share of all F/M in each Pclass actually survived
aggregate(Survived ~ Pclass + Sex, data = titanic.full,FUN = function(x) {sum(x) /length (x)})

## Thus, 97%, 92% and 50% of female srvvd in each respective Pclass,
## versus 37%, 16%, 13% for male passengers

## Females had a better chance at surviving, in any cabin class, less so in 3rd cabin class
## equal amount of woment perished/survived in 3rd cabin class;
## while males had a worse chances of survival, particuarly in 3rd class cabins

## ---Feature Engineering---

## Next we'll create a column for famil size
titanic.full$FamSize <- 1 + titanic.full$SibSp + titanic.full$Parch
## no need: titanic.full$Is_Alone <- ifelse(titanic.full$FamSize == 1, 1, 0)

table(titanic.full$FamSize)

## More than half of passengers are traveling alone, biggest family has 11 members
## we will group them together
titanic.full$FamGroup <- ifelse(titanic.full$FamSize == 1, "Alone", ifelse(titanic.full$FamSize <= 4, "Medium", "Large"))

## next we'll create Age groups, considering child turns adult when reaches 18 y.o
titanic.full$Age_Group <- ifelse(titanic.full$Age < 18, "Child",
                                 ifelse(titanic.full$Age < 40, "Young Adult", 
                                 ifelse(titanic.full$Age < 60, "Adult", "Senior")))
## no need: titanic.full$Is_Child <- ifelse(titanic.full$Age_Group == "Child", 1, 0)

aggregate(Survived ~ Age_Group + Sex, data = titanic.full, FUN = sum) 
aggregate(Survived ~ Age_Group + Sex, data = titanic.full, FUN = function(x) {sum(x)/length(x)})
## all senior females survived

## no need: titanic.full$Is_Child_or_Woman <- ifelse(titanic.full$Age_Group == "Child"| titanic.full$Sex == "female", 1, 0)

## Let's dig dipper into fare (EDA)
ggplot(filter(titanic.full, Fare <=160), aes(Pclass, Fare)) +
  geom_boxplot(aes(fill=factor(Pclass), alpha = 0.5)) +
  ggtitle("Fare distribution within Pclasses") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## based on the chart above we will break down our people into groups by Fare they paid
titanic.full$FareGroup <- ifelse(titanic.full$Fare <=15, "cheap", 
                                 ifelse(titanic.full$Fare <= 33, "Below Median", 
                                        ifelse(titanic.full$Fare <= 80, "Above Median", "Expensive") ) )

prop.table(table(titanic.full$FareGroup, titanic.full$Survived), margin = 1)
## 94% of people with Fare less than $6 perished, but this doesnt give us any new info, 
## since these are probably passengers who have a $0 fare
## so we adjuatsed "cheap" group to incl all fares below $15
## thus, 75% of people in that group perished, from people who paid $16-$80, chance are appx. equal,
## expensive tix (above $81 had 77% of survival)

aggregate(Survived ~ FareGroup + Pclass, data = titanic.full, FUN = sum)
aggregate(Survived ~ FareGroup + Pclass, data = titanic.full, FUN = function(x){sum(x) / length(x)})

## Now we'll extract titles from name column
titanic.full$Title <- NA
titanic.full$Title <- unlist(regmatches(x = titanic.full$Name, regexpr(pattern = "[[:alpha:]]+\\.", text = titanic.full$Name))) 

## To see how many different titles we have, we need to convert our new column to factors
titanic.full$Title <- as.factor(titanic.full$Title)

## Lets see how many different titles there are, and check for validity 
str(titanic.full$Title)
table(titanic.full$Title, titanic.full$Sex)

##Now we will group outliers among titles into "other" group, and similar ones into the larger buckets
TL <- levels(titanic.full$Title)
levels(titanic.full$Title) <- ifelse(TL %in%  c("Don.", "Jonkheer.", "Major.", "Sir.", "Rev.", "Capt.", "Col."), "Mr.",
                                     ifelse(TL %in% c("Dona.", "Countess.", "Lady.", "Mme."), "Mrs.",
                                            ifelse(TL %in% c("Ms.", "Mlle."), "Miss.", TL)))

## Let's check, if we got the desired large groups of Title
table(titanic.full$Title, titanic.full$Sex)

mosaicplot(~Title + Survived, data = titanic.full, main = "Survival rate based on Title", shade = T)

ggplot(filter(titanic.full, is.na(Survived)==F), aes(Title)) +
  geom_bar(aes(fill = factor(Survived)), alpha = 0.9, position = "fill") +
  facet_wrap(~Pclass) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Pclass and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Ttitle Mr. has the worst survival rate among all titles 

mosaicplot(~FamGroup + Survived, data = titanic.full, main = "Survival rate based on Family Size", shade = T)

ggplot(filter(titanic.full, is.na(Survived)==F), aes(Title)) +
  geom_bar(aes(fill = factor(Survived)), alpha = 0.9, position = "fill") +
  facet_wrap(~FamGroup) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Family Size and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Large Families have the worst survival rates for all titles

## we select only columns from train set that we think we can use in our model
## then we split our new dataset into two chunks, one to create a model
## and then test it on a train set

train <- titanic.full[c(2,3, 5, 6, 12, 16, 17, 20, 21)]
fit <- train[1:700,]
test <- train[701:891,]

test_og <- filter(titanic.full, is.na(Survived==T))
## now we create a model
model <- glm(Survived ~ ., family = binomial(link='logit'), data = fit )

step(model)

## by using summary() we obtain results of our model
summary(model)

## next we run anova() function to analyze the deviance
anova(model, test = "Chisq")

## Now we are going to assess the predictive ability of our model
prob_pred <- predict(model, newdata = test, type = 'response') 
y_pred <- ifelse(prob_pred > 0.5,1,0)

table(test$Survived, y_pred >0.5) ## confusion matrix

error <- mean(y_pred != test$Survived) ## missclasification error
paste('Accuracy', round(1 - error, 4)) ## 

## However, keep in mind that this result is somewhat dependent on the manual split of the data 
## that I made earlier, therefore if you wish for a more precise score, 
## you would be better off running some kind of cross validation such as k-fold cross validation.
## As a last step, we are going to plot the ROC curve and calculate the AUC (area under the curve) 
## which are typical performance measurements for a binary classifier.
## The ROC is a curve generated by plotting the true positive rate (TPR) against the false positive rate
## (FPR) at various threshold settings while the AUC is the area under the ROC curve. As a rule of thumb,
## model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.

install.packages("ROCR")
library("ROCR")
fit_pred <- prediction(prob_pred, test$Survived)
fit_perf <- performance(fit_pred, "tpr", "fpr")
plot(fit_perf, col = "green", lwd =2, main = "ROC Curve")
abline(a=0, b=1, lwd = 2, lty =2, col = "grey")

auc <- performance(fit_pred, "auc")
auc <- auc@y.values[[1]]

round(auc, 4)

## we got pretty good result auc = 0.9097
## the closer it is to 1 - the better

##  now we predict test set results
prob_pred <- predict(model, newdata = test_og)
y_pred <-ifelse(prob_pred > 0.5, 1, 0)
results <- data.frame(PassengerID = c(892:1309), Survived = y_pred)
write.csv(results, file = "TitanicGlmPrediction.csv", row.names = F, quote = F)

rm(list = ls())
