# Machine Learning
# Correlation: statistical relationsnip between 2 data points
# Num only
df <- read.csv("student-mat.csv", sep = ";")
num.cols <- sapply(df, is.numeric)
#filter
cor.data <- cor(df[, num.cols]) 
print(cor.data)
corrgram(df)
corrgram(df, order=T, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)
ggplot(df, aes(x = G3)) + geom_histogram(bins = 20, alpha = 0.5, fill = "blue")

# splitting up data into training and testing set
# require caTools package

set.seed(101)
# Split up the sample 
sample <- sample.split(df$G3, SplitRatio = 0.7) # we are passing in the column we are trying to predict i.e.: df$G3
# 70% of data goes to train
train <- subset(df, sample == T)
# 30% will go to test
test <- subset(df, sample == F)

# Train and Build model
model <- lm(G3 ~ ., train) # looking for all the features, hence you use train instead of individual columns

# Run Model

# Interpret the Model
print(summary(model))

# Residuals: difference between actual values of variable you're predicting and the predicting values of your regression
# Coefficients
res <- residuals(model) # this would be numeric
res <- as.data.frame(res) # converting it to dataframe
class(res)
ggplot(res, aes(res)) + geom_histogram(fill = "blue", alpha = 0.5)

# Plot(model)
plot(model) # will print out #nts advanced statistical visualizations 

# PREDICTIONS
G3.predictions <- predict(model, test)

results <- cbind(G3.predictions, test$G3)
colnames(results) <- c("predicted", "actual")
results <- as.data.frame(results)
print(head(results, 4))

# Take Care of Negative Balues
to_zero <- function(x){
  if(x < 0){
    return(0)
  }
  else{
    return(x)
  }
}

# Apply Zero function
results$predicted <- sapply(results$predicted, to_zero)

# Mean Squared Error (measure of how off you are)
mse <- mean((results$actual - results$predicted)^2)
print(mse)

## RMSE
print("Squared Root of MSE:")
print(mse^0.5)

##### Sum of squared Error
SSE <- sum((results$predicted - results$actual)^2)
#### Sum of the squared total
SST <- sum( (mean(df$G3) - results$actual)^2)
R2 <- 1 - SSE/SST
