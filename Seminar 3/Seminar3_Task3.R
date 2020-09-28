# --- Preparation --- #

# Reading in necessary libraries
library(glmnet)
library(ISLR)

# Setting Working Directory
setwd("...")

# ---- TASK 3 ---- #

# Reading in data
raw = read.csv("proteomdataset.csv", header=T, dec = ".")
attach(raw)

#correlations
cor = data.frame(round(cor(raw), 2))  # Create correlation matrix
c = cor[1, ]                          # Extract relevant array of only first row of correlation matrix
c.sort = sort(c, decreasing=T)        # Sorting array to find largest and lowest value

# Significant in model X10, X1 X20, X110, X127, X146 and X58 and X47.
model1 = glm(InflBiomarker ~ X10 + X1 + X20 + X110 + X127 + X146 + X58 + X47) # Model with chosen high and low protein expressions
summary(model1)

# Revised model after summary of model including only significant proteins
model2 = glm(InflBiomarker ~ X10 + X1 + X20 + X110) 
summary(model2)

# Test Gamma family
model3 = glm(InflBiomarker ~ X10 + X1 + X20 + X110, family=Gamma(link="identity"))
summary(model3)

model4 = glm(InflBiomarker ~ X10 + X1 + X20 + X110, family=Gamma(link="log"))
summary(model4)

model5 = glm(InflBiomarker ~ X10 + X1 + X20 + X110, family=Gamma(link="inverse"))
summary(model5)
 
# Test inverse gaussian family
model6 = glm(InflBiomarker ~ X10 + X1 + X20 + X110, family=inverse.gaussian(link="identity"))
summary(model6)
 
model7 = glm(InflBiomarker ~ X10 + X1 + X20 + X110, family=inverse.gaussian(link="log"))
summary(model7)

model8 = glm(InflBiomarker ~ X10 + X1 + X20 + X110, family=inverse.gaussian(link="inverse"))
summary(model8)
 
# Regression diagnostics of model4
plot(model4)
 
# Backward elimination
model.all = glm(InflBiomarker ~. , data=raw) # -5864.9 AIC
step(model.all, direction="backward") # -5865 AIC
 
#forward selection
model.for = glm(InflBiomarker ~ 1, data=raw)
step(model.for, direction="forward", scope=formula(model.all)) # -5925 AIC
  
#both forward and backward
step(model.all2, direction="both", scope=formula(model.all)) # -5888 AIC

# These models give very low AIC value, but have too many variable to
# realistically be included in a model to test for inflammation.
  
# Ridge regression
x = model.matrix(InflBiomarker ~. -1, data=raw)
y = raw$InflBiomarker

ridgefit = glmnet(x, y, alpha=0, standardize=TRUE)
plot(ridgefit, xvar="lambda", labels=T)

# Validation
ridgecv = cv.glmnet(x, y, alpha=0)
plot(ridgecv)
coef(ridgecv)

# Lasso regression
lassofit = glmnet(x, y, standardize = T)
plot(lassofit, xvar="lambda", label=T)
lassocv = cv.glmnet(x, y)
plot(lassofit)
coef(lassofit)


ind=sample(2, nrow(raw), replace=T, prob=c(0.8, 0.2))
tdata = raw[ind==1,] # Training data
vdata = raw[ind==2,] # Validation data

# Regr.model
result = lm(InflBiomarker ~. , data=raw)
pred = predict(model2, vdata)
pred

plot(coef(ridgecv), raw2[1, ])
x = model.matrix(InflBiomarker~., raw)[, -1]
y = na.omit(raw$InflBiomarker)  
grid = 10^seq(10, -2, lengt=100)
ridge.mord = glmnet(x, y, alpha=0, lambda=grid)
predict(ridge.mord, s=100, type="coefficients")[1:151, ]  # Using 50 lambda from grid
  
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=0)
bestlam = cv.out$lambda.min
bestlam # 16.31549

ridge.pred = predict(ridge.mord, s=bestlam, newx=x[test,])
mean((ridge.pred - y.test)^2)
out = glmnet(x, y, alpha=0)
i = predict(out, type="coefficients", s=bestlam)[1:151,]
i = i[i!=0]  #Significant
ii = i[2:151]
sort(ii, decreasing=T)

model.ridge = glm(InflBiomarker ~ X1 + X5 + X10 + X20 + X110 + X97 + X111, family = Gamma(link="log"))

# Lasso regression
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
bestlam = cv.out$lambda.min
bestlam
  
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2)
out = glmnet(x,y, alpha=1, lambda = grid)

lasso.coef = predict(out, type="coefficients", s=bestlam)[1:151,]
lasso.coef
lasso.coef[lasso.coef!=0]
model.lasso = glm(InflBiomarker ~ X1 + X5 + X10 + X20 + X48 + X54 + X63 + X110)
summary(model.lasso)
anova(model4, model.lasso)

# Final model
model.lassogamma = glm(InflBiomarker ~ X1 + X5 + X10 + X16 + X20 + X48 + X54 + X63 + X110, family=Gamma(link="log"))
summary(model.lassogamma)

# Source for lasso regression code: Allaen Kei at Youtube.com/watch?v=pjQqsoLYeNM&t

ax = barplot(8,model.lassogamma)
    

