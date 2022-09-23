Mario,Holli,Zack


library(dplyr)
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}
is.na(NCHILD) <- which(NCHILD == 9999999) 
fam_size <- FAMSIZE
norm_hh_income <- norm_varb(HHINCOME)
norm_children <- norm_varb(NCHILD)
norm_fam_size <- norm_varb (FAMSIZE)

is.na(NCHILD) <- which(NCHILD == 9999999) 
fam_size <- FAMSIZE
norm_hh_income <- as.numeric(norm_hh_income)
norm_children <- as.numeric(norm_children)
norm_fam_size <- as.numeric(norm_fam_size)

data_use_prelim <- data.frame(norm_hh_income,norm_children,norm_fam_size)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]


summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

cl_data_n <- as.numeric(cl_data)

model_ols1 <- lm(cl_data_n ~ train_data$norm_hh_income + train_data$norm_fam_size + train_data$norm_children)

y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 1])
mean(y_hat[cl_data_n == 2])
mean(y_hat[cl_data_n == 3])
mean(y_hat[cl_data_n == 4])
mean(y_hat[cl_data_n == 5])

# maybe try classifying one at a time with OLS

cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_hh_income + train_data$norm_fam_size + train_data$norm_children)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])

## Bronx
cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_hh_income + train_data$norm_fam_size + train_data$norm_children)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])

## Manhattan
cl_data_n1 <- as.numeric(cl_data_n == 2)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_hh_income + train_data$norm_fam_size + train_data$norm_children)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 2])
mean(y_hat_v1[cl_data_n1 == 0])

##  SI
cl_data_n1 <- as.numeric(cl_data_n == 3)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_hh_income + train_data$norm_fam_size + train_data$norm_children)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 3])
mean(y_hat_v1[cl_data_n1 == 0])

## Brooklyn
cl_data_n1 <- as.numeric(cl_data_n == 4)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_hh_income + train_data$norm_fam_size + train_data$norm_children)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 4])
mean(y_hat_v1[cl_data_n1 == 0])

##  Queens 
cl_data_n1 <- as.numeric(cl_data_n == 5)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_hh_income + train_data$norm_fam_size + train_data$norm_children)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 5])
mean(y_hat_v1[cl_data_n1 == 0])

##  based on the statistical summary of the proportion of residency by borough in the CL_data
##  we decided to use household income, family size, and number of children to estimate the borough of residency under the hypothesis that homes with more children and larger family sizes seek more affordable housing options.
##  13.23% of population in Bronx
##  13.18% of population in Manhattan
##  5.37% of Population in Staten Island 
##  35% of Population in Brooklyn
##  31% in Population Queens
