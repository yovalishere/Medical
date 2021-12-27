fat<-read.csv("fat.csv",sep=",",header=TRUE)

####################### Introduction ##########################
dim(fat)

########################### Prep ################################
n=dim(fat)[1]
n1=round(n/10)
set.seed(7406)
flag=sort(sample(1:n,n1))
fat1train=fat[-flag,]
fat1test=fat[flag,]

dim(fat1test)
dim(fat1train)

########################### Exploratory ##########################
corr<-round(cor(fat1train),2) # calculate correlation
# Heatmap
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = corr, col = col, symm = TRUE)

#install.packages("corrplot")
library(corrplot)
corrplot(corr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

################################# Model #####################

# 1. Linear regression with all predictors
model1<-lm(brozek~.,data=fat1train)
summary(model1)

# 2. Linear regression with the best subset of k = 5 predictors variables;
library(leaps);
fat.leaps <- regsubsets(brozek ~ ., data= fat1train, nbest= 100, really.big= TRUE)

## Record useful information from the output
fat.models <- summary(fat.leaps)$which
fat.models.size <- as.numeric(attr(fat.models, "dimnames")[[1]])
fat.models.rss <- summary(fat.leaps)$rss

# Best subset with k=5
op5 <- which(fat.models.size == 5) 
flag5 <- op5[which.min(fat.models.rss[op5])]

# Auto-find the best subset with k=5
mod5selectedmodel <- fat.models[flag5,]; 
mod5Xname <- paste(names(mod5selectedmodel)[mod5selectedmodel][-1], collapse="+"); 
mod5form <- paste ("brozek ~", mod5Xname);
## To auto-fit the best subset model with k=5 to the data
model2 <- lm( as.formula(mod5form), data= fat1train)

summary(model2)
# Model 2: training error 
MSEmod2train <- mean(resid(model2)^2)

# 3. Linear regression with variables (stepwise) selected using AIC
model3  <- step(model1); 
summary(model3)

# 4.  Ridge regression
library(MASS);
fat.ridge <- lm.ridge( brozek ~ ., data = fat1train, lambda= seq(0,100,0.001))
indexopt <-  which.min(fat.ridge$GCV);  
fat.ridge$coef[,indexopt]
ridge.coeffs = fat.ridge$coef[,indexopt]/ fat.ridge$scales
intercept = -sum( ridge.coeffs  * colMeans(fat1train[,2:18] )  )+ mean(fat1train[,1]);
c(intercept, ridge.coeffs)

# 5. LASSO
library(lars)
fat.lars <- lars(as.matrix(fat1train[,2:18]), fat1train[,1], type= "lasso", trace= TRUE)
summary(fat.lars)
plot(fat.lars)
Cp1  <- summary(fat.lars)$Cp
index1 <- which.min(Cp1)
coef(fat.lars)[index1,]
fat.lars$beta[index1,]
lasso.lambda <- fat.lars$lambda[index1]
coef.lars1 <- predict(fat.lars, s=lasso.lambda, type="coef", mode="lambda")
coef.lars1$coef
LASSOintercept = mean(fat1train[,1]) -sum( coef.lars1$coef  * colMeans(fat1train[,2:18]))
c(LASSOintercept, coef.lars1$coef)

#6. Principal component regression
library(pls)
fat.pca <- pcr(brozek~., data=fat1train, validation="CV")
validationplot(fat.pca);
summary(fat.pca); 
ncompopt <- which.min(fat.pca$validation$adj)

# 7. Partial least squares.
library(pls)
fat.pls <- plsr(brozek ~ ., data = fat1train, validation="CV")
mod7ncompopt <- which.min(fat.pls$validation$adj)
validationplot(fat.pls)

# Summary of training errors of the models
MSEmod1train <-   mean( (resid(model1) )^2);
MSEmod2train <- mean(resid(model2)^2);
MSEmod3train <- mean(resid(model3)^2);
yhat4train <- as.matrix(fat1train[,2:18]) %*% as.vector(ridge.coeffs) + intercept
MSEmod4train <- mean((yhat4train - fat1train$brozek)^2); 
pred5train  <- predict(fat.lars, as.matrix(fat1train[,2:18]), s=lasso.lambda, type="fit", mode="lambda");
yhat5train <- pred5train$fit; 
MSEmod5train <- mean((yhat5train - fat1train$brozek)^2)
ypred6train <- predict(fat.pca, ncomp = ncompopt, newdata = fat1train[,2:18]); 
MSEmod6train <- mean( (ypred6train - fat1train$brozek)^2)
ypred7train <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fat1train[,2:18]); 
MSEmod7train <- mean( (ypred7train - fat1train$brozek)^2); 

TrainErr=cbind(MSEmod1train, MSEmod2train, MSEmod3train, MSEmod4train, MSEmod5train, MSEmod6train, MSEmod7train) 
colnames(TrainErr) <- c("linearRegression(all)", "linearRegression(k=5)", "Stepwise", "Ridge", "LASSO",
                     "Principal component regression", "Partial least square")
TrainErr.df=as.data.frame(TrainErr)


plot(as.vector(TrainErr),xlab='Models',ylab='Training errors', main='Training errors of models')

############################### Testing error #####################################
ytrue    <- fat1test$brozek;
# Model 1: testing error 
pred1a <- predict(model1, fat1test[,2:18]);
MSEmod1test <-   mean((pred1a - ytrue)^2);
## Model 2:  testing error 
pred2 <- predict(model2, fat1test[,2:18]);
MSEmod2test <-   mean((pred2 - ytrue)^2);
## Model 3:  testing error 
pred3 <- predict(model3, fat1test[,2:18]);
MSEmod3test <-  mean((pred3 - ytrue)^2);
## Model 4:  testing error 
pred4test <- as.matrix(fat1test[,2:18]) %*% as.vector(ridge.coeffs) + intercept;
MSEmod4test <-  mean((pred4test - ytrue)^2)
## Model 5:  training error for lasso  
pred5test <- predict(fat.lars, as.matrix(fat1test[,2:18]), s=lasso.lambda, type="fit", mode="lambda");
yhat5test <- pred5test$fit; 
MSEmod5test <- mean( (yhat5test - ytrue)^2); 
## 6B(v) Testing Error with the optimal choice of PCs
ypred6test <- predict(fat.pca, ncomp = ncompopt, newdata = fat1test[,2:18]); 
MSEmod6test <- mean( (ypred6test - ytrue)^2);
## 7(iii) Testing Error with the optimal choice of "mod7ncompopt" 
ypred7test <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fat1test[,2:18]); 
MSEmod7test <- mean( (ypred7test - ytrue)^2); 
#Summary
TestErr=cbind(MSEmod1test, MSEmod2test, MSEmod3test, MSEmod4test, MSEmod5test, MSEmod6test, MSEmod7test) 
colnames(TestErr) <- c("linearRegression(all)", "linearRegression(k=5)", "Stepwise", "Ridge", "LASSO",
                        "Principal component regression", "Partial least square")
TestErr.df=as.data.frame(TestErr)
plot(as.vector(TestErr),xlab='Models',ylab='Testing errors', main='Testing errors of models')

################################## MC Cross Validation #################################
B= 100; ### number of loops
TEALL = NULL; ### Final TE values
for (b in 1:B){
  ### randomly select 25 observations as testing data in each loop
  flag <- sort(sample(1:n, n1));
  fattrain <- fat[-flag,]; # Train
  fattest <- fat[flag,]; # Test
  ytrue    <- fattest$brozek;
  
  # Model 1: lm
  model1<-lm(brozek~.,data=fattrain)
  pred1a <- predict(model1, fattest[,2:18]);
  te1 <-   mean((pred1a - ytrue)^2);
  
  
  # Model 2: lm with best subset of k = 5 
  library(leaps)
  fat.leaps <- regsubsets(brozek ~ ., data= fattrain, nbest= 100, really.big= TRUE)
  ## Record useful information from the output
  fat.models <- summary(fat.leaps)$which
  fat.models.size <- as.numeric(attr(fat.models, "dimnames")[[1]])
  fat.models.rss <- summary(fat.leaps)$rss
  # Best subset with k=5
  op5 <- which(fat.models.size == 5) 
  flag5 <- op5[which.min(fat.models.rss[op5])]
  # Auto-find the best subset with k=5
  mod5selectedmodel <- fat.models[flag5,]; 
  mod5Xname <- paste(names(mod5selectedmodel)[mod5selectedmodel][-1], collapse="+"); 
  mod5form <- paste ("brozek ~", mod5Xname);
  ## To auto-fit the best subset model with k=5 to the data
  model2 <- lm( as.formula(mod5form), data= fattrain)
  pred2 <- predict(model2, fattest[,2:18]);
  te2 <-   mean((pred2 - ytrue)^2);
  
  # Model 3: Stepwise with AIC
  model3  <- step(model1) 
  pred3 <- predict(model3, fattest[,2:18]);
  te3 <-  mean((pred3 - ytrue)^2);
  
  # Model 4: Ridge regression
  library(MASS)
  fat.ridge <- lm.ridge( brozek ~ ., data = fattrain, lambda= seq(0,100,0.001))
  indexopt <-  which.min(fat.ridge$GCV);  
  fat.ridge$coef[,indexopt]
  ridge.coeffs = fat.ridge$coef[,indexopt]/ fat.ridge$scales
  intercept = -sum( ridge.coeffs  * colMeans(fattrain[,2:18] )  )+ mean(fattrain[,1]);
  pred4test <- as.matrix(fattest[,2:18]) %*% as.vector(ridge.coeffs) + intercept;
  te4 <-  mean((pred4test - ytrue)^2)
  
  # 5. LASSO
  library(lars)
  fat.lars <- lars(as.matrix(fattrain[,2:18]), fattrain[,1], type= "lasso", trace= TRUE)
  Cp1  <- summary(fat.lars)$Cp
  index1 <- which.min(Cp1)
  coef(fat.lars)[index1,]
  fat.lars$beta[index1,]
  lasso.lambda <- fat.lars$lambda[index1]
  coef.lars1 <- predict(fat.lars, s=lasso.lambda, type="coef", mode="lambda")
  coef.lars1$coef
  LASSOintercept = mean(fattrain[,1]) -sum( coef.lars1$coef  * colMeans(fattrain[,2:18]))
  pred5test <- predict(fat.lars, as.matrix(fattest[,2:18]), s=lasso.lambda, type="fit", mode="lambda");
  yhat5test <- pred5test$fit; 
  te5 <- mean( (yhat5test - ytrue)^2);
  
  #6. Principal component regression
  library(pls)
  fat.pca <- pcr(brozek~., data=fattrain, validation="CV")
  validationplot(fat.pca);
  ncompopt <- which.min(fat.pca$validation$adj)
  ypred6test <- predict(fat.pca, ncomp = ncompopt, newdata = fattest[,2:18]); 
  te6 <- mean( (ypred6test - ytrue)^2);
  
  # 7. Partial least squares.
  library(pls)
  fat.pls <- plsr(brozek ~ ., data = fattrain, validation="CV")
  mod7ncompopt <- which.min(fat.pls$validation$adj)
  validationplot(fat.pls)
  ypred7test <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fattest[,2:18]); 
  te7 <- mean( (ypred7test - ytrue)^2); 
  
  ### you can write your own R code here to first fit each model to "fattrain"
  ### then get the testing error (TE) values on the testing data "fattest"
  ### Suppose that you save the TE values for these five models as
  ### te1, te2, te3, te4, te5, te6, te7, respectively, within this loop
  ### Then you can save these 5 Testing Error values by using the R code
  ###
  TEALL = rbind( TEALL, cbind(te1, te2, te3, te4, te5, te6, te7) );
}
dim(TEALL); ### This should be a Bx7 matrices
### if you want, you can change the column name of TEALL
colnames(TEALL) <- c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7");
## You can report the sample mean and sample variances for the seven models
apply(TEALL, 2, mean);
apply(TEALL, 2, var);

TestEALL_mean<-as.data.frame(apply(TEALL, 2, mean))
TestEALL_var<-as.data.frame(apply(TEALL, 2, var))

#TrainEALL_mean
# Plot Train_mean

TestEALL_mean <- cbind(models = rownames(TestEALL_mean), TestEALL_mean)
rownames(TestEALL_mean) <- 1:nrow(TestEALL_mean)
names(TestEALL_mean)[names(TestEALL_mean) == "apply(TEALL, 2, mean)"] <- "Test_mean"
TestEALL_mean

TestEALL_mean$models <- factor(TestEALL_mean$models, levels = TestEALL_mean$models)
TestEALL_mean

models<-c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7")
library(ggplot2)
# geom_point() is for scatter plot
TestEALL_mean_plot<-ggplot(TestEALL_mean, aes(as.factor(models),Test_mean)) + geom_point()
TestEALL_mean_plot+ xlab('Models')+ylab('Test_mean')+ggtitle('Testing_error_mean of models')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plot TE_var
TestEALL_var <- cbind(models = rownames(TestEALL_var), TestEALL_var)
rownames(TestEALL_var) <- 1:nrow(TestEALL_var)
names(TestEALL_var)[names(TestEALL_var) == "apply(TEALL, 2, var)"] <- "Test_var"

TestEALL_var$models <- factor(TestEALL_var$models, levels = TestEALL_var$models)

library(ggplot2)
# geom_point() is for scatter plot
TestEALL_var_plot<-ggplot(TestEALL_var, aes(as.factor(models),Test_var)) + geom_point()
TestEALL_var_plot+ xlab('Models')+ylab('Test_var')+ggtitle('Testing_error_var of models') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### END ###