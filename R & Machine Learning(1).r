
library(caret)

# o cualquier otra liberia que necesitemos

library(magrittr) 
library(dplyr)

#install.packages("caret")
#install.packages("magrittr")
#install.packages("dplyr")
#install.packages('e1071', dependencies=TRUE)

installed.packages()

library(dslabs)
data(heights)

y <-heights$sex
x <-heights$height

set.seed(2)
test_index <-createDataPartition(y, times=1, p=0.5, list =FALSE)

train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>%  summarize(mean(height), sd(height))


y_hat <- ifelse(x>62, "Male", "Female") %>% factor(levels = levels(test_set$sex))

 

mean(y == y_hat)

cutoff<- seq(61,70)


accuracy <- purrr::map_dbl(cutoff, function(x){
    y_hat<- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
    mean(y_hat == train_set$sex)
})

max(accuracy)
best_cutoff<-cutoff[which.max(accuracy)]
print("best_cutoff:")
best_cutoff

y_hat <- ifelse(test_set$height>best_cutoff, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))


y_hat<-factor(y_hat)


mean(y_hat == test_set$sex)

table(predicted = y_hat, actual=test_set$sex)

test_set %>%
    mutate(y_hat = y_hat) %>%
    group_by(sex) %>%
    summarize(accuracy = mean(y_hat == sex))

prev <- mean(y=="Male")
prev

confusionMatrix(data = y_hat, reference=test_set$sex)

install.packages("HistData")

library(HistData)

galton_heights<-GaltonFamilies %>%
filter(childNum == 1 & gender == "male") %>%
select(father,childHeight) %>%
rename(son=childHeight)

y<-galton_heights$son

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set<-galton_heights %>% slice (-test_index)
test_set<-galton_heights %>% slice (test_index)

avg<- mean(train_set$son)
avg

mean((avg - test_set$son)^2)

fit<-lm(son ~ father, data=train_set)
fit$coef

y_hat<-fit$coef[1]+fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

y_hat<-predict(fit, test_set)
mean((y_hat - test_set$son)^2)

# Read in `iris` data
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), 
                 header = FALSE) 

# Print first lines
head(iris)

# Add column names
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

# Check the result
iris

# Create index to split based on labels  
index <- createDataPartition(iris$Species, p=0.75, list=FALSE)

# Subset training set with index
iris.training <- iris[index,]

# Subset test set with index
iris.test <- iris[-index,]

# Train a model
model_knn <- train(iris.training[, 1:4], iris.training[, 5], method='knn')

# Predict the labels of the test set
predictions<-predict.train(object=model_knn,iris.test[,1:4], type="raw")

# Evaluate the predictions
table(predictions)

# Confusion matrix 
confusionMatrix(predictions,iris.test[,5])

library(caret)
install.packages("rpart.plot")
library(rpart.plot)

data_url <- c("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data")
download.file(url = data_url, destfile = "car.data")
 
car_df <- read.csv("car.data", sep = ',', header = FALSE)

str(car_df)

head(car_df)

set.seed(3033)
intrain <- createDataPartition(y = car_df$V7, p= 0.7, list = FALSE)
training <- car_df[intrain,]
testing <- car_df[-intrain,]

#check dimensions of train & test set
dim(training); dim(testing);

anyNA(car_df)

summary(car_df)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(V7 ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

testing[1,]

predict(dtree_fit, newdata = testing[1,])

test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred, testing$V7 )  #check accuracy

set.seed(3333)
dtree_fit_gini <- train(V7 ~., data = training, method = "rpart",
                   parms = list(split = "gini"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit_gini

prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)

test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$V7 )  #check accuracy
