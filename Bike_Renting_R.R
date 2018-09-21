rm(list=ls())
setwd('D:/Data Science/edWisor/Project 2')
getwd()

x = c("ggplot2", "DMwR", "randomForest", "e1071", "rpart", "scales", "fastDummies", "caTools")
lapply(x, require, character.only = TRUE)
data = read.csv('./data/day.csv', header = T)


str(data)
cnames = c("temp", "atemp", "hum", "windspeed")
for (i in 1:length(cnames)){
  assign(paste0("gn",i),ggplot(data, aes_string(x = cnames[i])) + 
           geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
           scale_y_continuous(breaks=pretty_breaks(n=10)) + 
           scale_x_continuous(breaks=pretty_breaks(n=10))+
           theme_bw() + xlab(cnames[i]) + ylab("Frequency") + ggtitle("Distribution Plot") +
           theme(text=element_text(size=20)))
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=2)
  
# Creating Dummy variables for non-binary variables and dropping irrelevant variables
data_proper = fastDummies::dummy_cols(data, select_columns = c("weekday", "season", "weathersit", "mnth"))
drop_col = c("weekday","weekday0", "season","season1", "weathersit","weathersit1", "mnth","mnth1", "instant", "dteday", "casual", "registered", "temp")
data_proper = data_proper[,!names(data_proper) %in% drop_col]
#data_nodummy = data[,!names(data) %in% drop_col]

#MAPE Evaluation Function
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}



#Divide the data into train and test
set.seed(1234)
train_index = sample(1:nrow(data_proper), 0.8 * nrow(data_proper))
train = data_proper[train_index,]
test = data_proper[-train_index,]
X_train = train[, !names(train) %in% c("cnt")]
y_train = train$cnt
X_test = test[, !names(test) %in% c("cnt")]
y_test = test$cnt


######### Regression Models ###########

######## Multiple linear regression

reg_lm = lm(cnt~., train)

# Predicting on test set
y_pred_lm = predict(reg_lm, X_test)

# Evaluating on test set
MAPE(y_test, y_pred_lm)

####### SVR
reg_svr = svm(cnt ~., train, type = "eps-regression" , kernel = "linear", gamma = 0.0001, cost = 1000)

# Predicting on test set
y_pred_svr = predict(reg_svr, X_test)

# Evaluating on test set
MAPE(y_test,y_pred_svr)


########### Decision Tree Regression
reg_dt = rpart(cnt~., train, control = rpart.control(minsplit = 1))

# Predicting on test set
y_pred_dt = predict(reg_dt, X_test)

# Evaluating on test set
MAPE(y_test, y_pred_dt)



############## Random Forest Regression
reg_rf = randomForest(x = X_train, y = y_train, ntree = 600)

# Predicting on test set
y_pred_rf = predict(reg_rf, X_test)

# Evaluating on test set
MAPE(y_test, y_pred_rf)



######## Random Forerst seems to be working best