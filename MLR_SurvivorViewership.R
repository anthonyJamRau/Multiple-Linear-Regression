## reading in the data and data cleaning

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/viewers.csv')
data2 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')
data2 <- data2[c(2,15,16)]
data <- na.omit(data)
temp1 <- aggregate(data$rating_18_49,by = list(data$season),FUN = mean)
temp2 <- aggregate(data$share_18_49,by = list(data$season),FUN = mean)
names(temp1)[1] <- "season"
names(temp1)[2] <- "mean_rating"
names(temp2)[1] <- "season"
names(temp2)[2] <- "mean_share"

temp3 <- merge(x = temp1, y = temp2, by = "season")
final_df <- merge(x = temp3,y = data2, by = "season")
final_df_r1 <- final_df[-c(1),]

#initial plot of the data

ggplot(final_df, aes(x=season)) + 
  geom_line(aes(y=viewers_finale, color="finale views")) + 
  geom_line(aes(y=viewers_premier, color="premier views")) + 
  geom_line(aes(y=mean_rating, color="mean_rating")) + 
  geom_line(aes(y=mean_share, color="mean_share"))


full_model <- lm(formula = viewers_finale ~ mean_rating + mean_share + viewers_premier,data = final_df)
reduced1 <- lm(formula = viewers_finale ~ mean_rating + mean_share,data = final_df)
reduced2 <- lm(formula = viewers_finale ~viewers_premier + mean_share,data = final_df)

# ended up going with reduced 3 after diagnostics and this is the model after removing point 1 from the data
reduced3 <- lm(formula = viewers_finale ~ mean_rating + viewers_premier,data = final_df_r1)
null_model <- lm(viewers_finale ~ 1, data = final_df)


lmx1 <- lm(formula = viewers_finale ~ mean_rating,data = final_df)
lmx2 <- lm(formula = viewers_finale ~ mean_share,data = final_df)
lmx3 <- lm(formula = viewers_finale ~ viewers_premier,data = final_df)

## finding a good MLR model
#to test if at least one coefficient in the model is different from 0
anova(null_model,full_model)

# to test level of contribution to the model that viewers_premier has when other two predictors are part of the model too
anova(reduced1,full_model)

# to test level of contribution to the model that mean_rating has when other two predictors are part of the model too
anova(reduced2,full_model)

# to test level of contribution to the model that mean_rating has when other two predictors are part of the model too
anova(reduced3,full_model)

# full model summary - very high r squared
summary(full_model)



## multicollinearity diagnostics
### conclude that model is the best when all three predictors are in it, but need to test for multicollinearity now
unit_length_scale <- function(x, center = TRUE) {
  if (length(unique(x)) == 1) {
    x / sqrt(sum(x^2))
  } else{
    if (center) {
      (x - mean(x)) / sqrt(sum( (x - mean(x)) ^ 2))
    } else{
      x / sqrt(sum(x^2))
    }
  }
}
final_df_scaled <- apply(final_df[-c(1)], 2, unit_length_scale)
X<- final_df[-c(1,5)]
X2<- final_df[-c(1,3,5)]
X_scaled <- apply(X, 2, unit_length_scale)
X_scaled2 <- apply(X2, 2, unit_length_scale)
y <- final_df[c(5)]
y_scaled <- apply(y, 2, unit_length_scale)
XtX_s <- t(X_scaled) %*% X_scaled
XtX_s2 <- t(X_scaled2) %*% X_scaled2

  # there is no correlation between chosen predictors that is close to 1. x2 and x3 have 0.86 though...

## VIF
XtX_s_inv <- solve(XtX_s)
XtX_s_inv2 <- solve(XtX_s2)
diag(XtX_s_inv)
  # combined effect of the predictors dependence on one another are all below 10, so there is nothing significant here

eigen_XtX_s <- eigen(XtX_s)
eigen_XtX_s2 <- eigen(XtX_s2)
round(lambda <- eigen_XtX_s$values, digits = 5)
round(lambda2 <- eigen_XtX_s2$values, digits = 5)
  # closest eigenvalue for x3 is 0.13   = 0.07x1 - 0.734x2 + 0.67x3.... x3 and x7 are more very correlated but lets find k to see if significant
  # next is x2 = 0.84x1 + - 0.32x2 - 0.44x3.. adds close to zero as well

max(lambda2)/lambda2
 # k = 18.311 < 100, so there is no indication of multicollinearity in eigensystem analysis

## variance proportion
V <- eigen_XtX_s$vectors
lm_full <- lm(formula = viewers_finale ~ mean_rating + viewers_premier,data = final_df)
vif_all <- car::vif(lm_full)
var_prop <- function(V,lambda,vif_all){
  round(t(V ^ 2 %*% diag(1 / lambda2)) %*% diag(1/vif_all), 5)
}
var_prop(V=V, lambda= lambda2, vif_all=vif_all)
  # indication of multicollinearity for full model- try to see if we fit a model without suspect regressors x2 or x3



## ridge
delta <- seq(0, 100, by = 0.01)
final_df_scaled <- as.data.frame(final_df_scaled)
ridge_fit <- MASS::lm.ridge(viewers_finale ~ mean_rating + mean_share + viewers_premier, data = final_df_scaled, lambda = delta)
matplot(coef(ridge_fit), type = "l", lwd = 3, 
        xlab = "delta", ylab = "Cofficients", main = "Ridge Trace")
abline(v = which(delta == 75), col = "orange", lty = 2)
  # ridge trace plot- coefficients are not converging with a small value for delta, so try PCA to combine columns

## PCA
(lambda[1]+lambda[2])/sum(lambda) ## the first two principal components account for 95 percent of the variation in response variable (ith PC has variance lambda(i))

pca_output <- prcomp(X_scaled)
## PC Loadings (matrix V)
pca_output$rotation  ## equation of transformed cols

round(Z <- X_scaled %*% V, 3) ## regressors data after rotation
colnames(Z) <- paste0("z", 1:3); df <- as.data.frame(cbind("y" = final_df$viewers_finale, Z))
pcr_fit <- lm(y ~ z1 + z2, data = df)

## comparing the full model with the new PCA model which doesnt have multicollinearity
pcr_fit$fitted.values
full_model$fitted.values
pcr_fit$coefficients
full_model$coefficients

## residual diagnostics - doing both the pca model and the full MLR model
## residual analysis- find studentized
e_pcr <- pcr_fit$residuals
sqrt_ms_res_pcr <- summary(pcr_fit)$sigma
h_pcr <- hatvalues(pcr_fit)
r_pcr <- e_pcr / ((sqrt_ms_res_pcr)*(1-h_pcr))
r_studentized_pcr <- rstandard(pcr_fit)

## normal probability plot of studentized residuals- PCR
qqnorm(r_studentized_pcr,pch=16,main="QQ-plot of R-student residuals- PCR")
qqline(r_studentized_pcr,col="steelblue",lwd=2)
  ## normal plot of studentized residuals is somewhat heavy tailed

e <- reduced3_with1$residuals
sqrt_ms_res <- summary(reduced3_with1)$sigma
h <- hatvalues(reduced3_with1)
r <- e / ((sqrt_ms_res)*(1-h))
r_studentized <- rstandard(reduced3_with1)

## normal probability plot of studentized residuals - MLR reduced 3
qqnorm(r_studentized,pch=16,main="QQ-plot of R-student residuals- MLR without mean share")
qqline(r_studentized,col="steelblue",lwd=2)
    ## normal

plot(x = pcr_fit$fitted.values, y = r_studentized_pcr, xlim = c(0, 80), 
     ylim = c(-2.5, 5), xlab = "fitted y", ylab = "R-student", 
     main = "residual vs. fitted - PCR")
abline(h = 0, col = "red", lwd = 2)
# some fanning

plot(x = reduced3$fitted.values, y = r_studentized, xlim = c(0, 80), 
     ylim = c(-2.5, 5), xlab = "fitted y", ylab = "R-student", 
     main = "residual vs. fitted for MLR without mean share")
abline(h = 0, col = "red", lwd = 2)
# normal

# partial regression for x1
y_x1 <- lm(viewers_finale ~ mean_rating,data = final_df_r1)
y_x1onallothers <- lm(viewers_premier ~ mean_rating,data = final_df_r1)
plot(y_x1onallothers$residuals, y_x1$residuals, main = "Partial regression plot viewers premier",
     xlab = "e(x2|x1)", ylab = "e(y|x1)", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# partial regression for x2
y_x1 <- lm(viewers_finale ~ viewers_premier,data = final_df_r1)
y_x2onallothers <- lm(mean_rating ~ viewers_premier,data = final_df_r1)
plot(y_x2onallothers$residuals, y_x1$residuals, main = "Partial regression plot mean rating",
     xlab = "e(x2|x1)", ylab = "e(y|x1)", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)


## influential points and leverage for the MLR model reduced 3
reduced3_with1 <- lm(formula = viewers_finale ~ mean_rating + viewers_premier,data = final_df)

standardized_d <- e/sqrt_ms_res# fit pt 1 has standardized res > 3... pt 1 outlier in dataset?
press_res_e <- e/(1-h) # fit pts 1 and 2 have high press residuals
r_student <- MASS::studres(reduced3_with1)

hat_ii <- hatvalues(reduced3_with1)
p <- 3; n <- 39
influential_pts <- hat_ii[hat_ii>p/n]
# high leverage points


# cooks distance
Dii_pcr <- cooks.distance(reduced3_with1)
Dii_pcr[Dii_pcr > 1]

round(sort(Dii_pcr, decreasing = TRUE), 3) #pt 1 influential

# dfbeta
(dfbeta <- dfbetas(reduced3_with1))
apply(dfbeta, 2, 
      function(x) x[abs(x) >  2 / sqrt(n)]) #nothing here 

#dffit
(dffit <- dffits(reduced3_with1))

dffit[abs(dffit) > 2 * sqrt(p/n)] # deleting pts 1  will affect the fitted response values by over 3 standard deviations

# cov ratios
(covra <- covratio(reduced3_with1))

covra[covra > (1 + 3*p/n)] #pt 2, 5, 15 will  actually improve model estimation
covra[covra < (1 - 3*p/n)] #pt 1 will decrease perfomance of model estimation 

#final fit of model
ggplot(data = final_df_r1, aes(x = season, y = viewers_finale)) +
  geom_point() + 
  geom_line(aes(y=reduced3$fitted.values, color="model fit")) + 
  xlim(0, 40) + ylim(0, 60) + 
  ggtitle("MLR model fit")
