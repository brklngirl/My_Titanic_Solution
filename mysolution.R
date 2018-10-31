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
  
## Next I want to query just an Embarked column of a titanic.full
## so I want to filter all the Embarked that is NA , and i want only the Embarked column to come back
## based on a table that we built previously for Embarked column, there are 2 such values

## now we want to replace them with something, let's for now do the most frequent value (or median)
titanic.full[titanic.full$Embarked == '', "Embarked"] <- 'S'

##So to check, we run table() again, blank values should be replaced with S
table(titanic.full$Embarked)

##Let's check Age column, if it has a lot of missing values
table(is.na(titanic.full$Age))

## it may be helpful to find median of both datasets as well as a global one
## lets query full dataset for Age and replace with something - median right now

age.median <- median(titanic.full$Age, na.rm = T)
train.age.median <- median(titanic.train$Age, na.rm = T)
test.age.median <-median(titanic.test$Age, na.rm = T)

print(train.age.median)
print(test.age.median)
print(age.median)

titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

### titanic.train$Age[is.na(titanic.train$Age)] <- train.age.median
### titanic.test$Age[is.na(titanic.test$Age)] <- test.age.median

##if now we make a table(is.na(titanic.full$Age)) we should not have any missing values

## Lets check Fare column
table(is.na(titanic.full$Fare))

## Since we have only 1 missing value, Lets fill it in quickly with median as well; 
## in a future we need to address some $0 fares as well 

fare.median <- median(titanic.full$Fare, na.rm = T)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

## Lets check Fare column again, to see that missing value was filled
table(is.na(titanic.full$Fare))

## let's check if any gender got luckier a surviving
table(titanic.train$Sex, titanic.train$Survived)

## let's find out if certain cabin class passengers got more chance of surviving
## would like to see it as proportions? just a thought for a future
table (titanic.train$Pclass, titanic.train$Survived)

## Females had a better chance at surviving, in any cabin class, less so in 3rd cabin class
## equal amount of woment perished/survived in 3rd cabin class;
## so males had a smaller chance of survival anywhere, particuarly in 3rd class cabins
## Number of male survived in 1st cabin class is roughly half the amount of those who perished

## Next we'll create a column for famil size
titanic.full$FamSize <- 1 + titanic.full$SibSp + titanic.full$Parch
titanic.full$Is_Alone <- ifelse(titanic.full$FamSize == 1, 1, 0)

## next we'll create Age groups
titanic.full$Age_Group <- ifelse(titanic.full$Age < 17, "Child",
                                 ifelse(titanic.full$Age < 40, "Young Adult", 
                                 ifelse(titanic.full$Age < 60, "Adult", "Senior")))
titanic.full$Is_Child <- ifelse(titanic.full$Age_Group == "Child", 1, 0)

titanic.full$Is_Child_or_Woman <- ifelse(titanic.full$Age_Group == "Child"| titanic.full$Sex == "female", 1, 0)

titanic.full$Title <- NA
titanic.full$Title <- unlist(regmatches(x = titanic.full$Name, regexpr(pattern = "[[:alpha:]]+\\.", text = titanic.full$Name))) 

## To see how many different titles we have, we need to convert our new column to factors
titanic.full$Title <- as.factor(titanic.full$Title)

## Lets see how many different titles there are, ad check for validity 
str(titanic.full$Title)
TL <- levels(titanic.full$Title)
table(titanic.full$Title, titanic.full$Age_Group) 
table(titanic.full$Title, titanic.full$Sex)

##Now we will group outliers among titles into "other" group, and similar ones into the larger buckets
levels(titanic.full$Title) <- ifelse(TL %in%  c("Don.", "Jonkheer.", "Major.", "Sir.", "Rev.", "Capt.", "Dona.", "Col.", "Countess.", "Dr."), "Other", 
                                     ifelse(TL %in% c("Lady.", "Ms.", "Mme."), "Mrs.",
                                            ifelse(TL == "Mlle.", "Miss.", TL)))

## Let's check, if we got the desired large groups of Titles
table(titanic.full$Title, titanic.full$Sex)

## we select only columns from train set that we think wecan use in our model
## then we split our new dataset into two chunks, one to create a model
## and then test it on a train set

train <- titanic.full[c(2,3, 5, 6, 12, 14, 15, 16, 19)]
train.fit <- train[1:700,]
train.test <- train[701:891,]

## now we create a model
model <- glm(Survived ~ ., family = binomial(link='logit'), data = train.fit )

## by using summary() we obtain results of our model
summary(model)

## next we run anova() function to analyze the deviance
anova(model, test = "Chisq")

## Now we are going to assess the predictive ability of our model
fitted.res <- predict(model, newdata = subset(train.test, type = 'response')) 
fitted.res <- ifelse(fitted.res > 0.4,1,0)

accur <- mean(fitted.res != train.test$Survived)
print(paste('Accuracy',1-accur))

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
p <- predict(model, newdata = subset(train.test, select = c(2, 3, 4, 5, 6, 7, 8, 9)), type = "response")
pr <- prediction(p, train.test$Survived)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
## we got pretty good result auc = 0.9156
## the closer it is to 1 - the better

## now we will use our model on an original test set
train.fit <- train[1:891,]
train.test <- train[892:1309,]

## now we recreate a model
model <- glm(Survived ~ ., family = binomial(link='logit'), data = train.fit )
summary(model)
anova(model, test = "Chisq")


p <- predict(model, newdata = subset(train.test, select = c(2, 3, 4, 5, 6, 7, 8, 9)), type = "response")
pr <- prediction(p, train.test$Survived)






rm(list = ls())
