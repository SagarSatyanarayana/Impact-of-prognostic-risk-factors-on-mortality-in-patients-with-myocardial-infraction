#install.packages("timereg")
library(timereg)

data(TRACE)
attach(TRACE)
names(TRACE)




## Staus 0-alive; 9-dead from myocardinal infraction; 7-dead from other causes.

### Q-1

## deaths within 3 years
three = TRACE[TRACE$status == 9 & TRACE$time <= 3, ]
nrow(three)
# 631 patients died within 3 years of myocardial infraction

## death within one month
one = TRACE[TRACE$status == 9 & TRACE$time <= (1/12), ]
nrow(one)
# 179 patients died within the first month of myocardial infraction

## percentage of censoring in the data
cen = TRACE[TRACE$status %in% (0 | 7), ]
nrow(cen)/nrow(TRACE)

table(TRACE$status)
#51.118% % of patients are censored in this dataset


### Q-2

## Fit the nonparametric additive hazards regression model with sex, clinical heart failure (CHF), ventricular fibrillation (VF), diabetes, and age as regression covariates.



## change status to 0 and 1 

TRACE$status[TRACE$status>7] = 1
table(TRACE$status)


fit_aalen = aalen(Surv(TRACE$time, TRACE$status==1)~TRACE$sex+TRACE$chf+TRACE$vf+TRACE$diabetes+TRACE$age, data = TRACE, max.time = 8)
summary(fit_aalen)

### Q-3 estimated cumulative coefficients along with 95% confidence intervals

par(mfrow=c(2,3))
plot(fit_aalen,  xlab = "time in years")


### Q-4 hypothesis testing for time-varying effects

## Kolomogororov-Smirnov test 

# Constant effects
#                         Kolmogorov-Smirnov test        p-value H_0:constant effect
# sex                            0.10100                       0.298
# diabetes                       0.10700                       0.613

# time-varying effects
#                          Kolmogorov-Smirnov test      p-value H_0:constant effect
# chf                            0.17600                       0.001
# vf                             0.46100                       0.000
# age                            0.00584                       0.012


## Cramer von mises test

# Constant effects
#                          Cramer von Mises test         p-value H_0:constant effect
# sex                           2.94e-02                       0.168
# diabetes                      1.22e-02                       0.780

# time-varying effects
#                          Cramer von Mises test         p-value H_0:constant effect
# chf                           1.39e-01                       0.000
# vf                            5.84e-01                       0.000
# age                           7.72e-05                       0.026

### Q-5 semiparametric additive model
## Sex and diabetes were found to be the constant effects and are modeled as constant effects. Clinical Heart Failure, Ventricular Fibrillation and age are modeled as time-varying effects.

fit_sm = aalen(Surv(TRACE$time, TRACE$status==1)~const(TRACE$sex)+TRACE$chf+TRACE$vf+const(TRACE$diabetes)+TRACE$age, data = TRACE, max.time = 8)
summary(fit_sm)

## Estimates for constant effects along with 95% confidence intervals
#                       Coef.  SE Robust    SE    z    P-val    lower2.5%   upper97.5%
# const(TRACE$sex)      0.0159 0.00877   0.00897 1.77 7.67e-02  -0.00129     0.0331
# const(TRACE$diabetes) 0.0808 0.01790   0.01680 4.80 1.57e-06   0.04570     0.1160

## plot with 95 % confidence interval

par(mfrow= c(2,2))
plot(fit_sm, sim.ci = 2, xlab = "time in years")


### Q-6

## significant effects on myocardial infection
#                       Supremum-test of significance      p-value H_0: B(t)=0

# TRACE$chf                           10.10                   0
# TRACE$vf                             6.53                   0
# TRACE$age                           14.80                   0


### Q-8
## check for model fit based on cumulative residuals

fit_res = aalen(Surv(TRACE$time, TRACE$status==1)~TRACE$sex+TRACE$chf+TRACE$vf+TRACE$diabetes+TRACE$age, data = TRACE, max.time = 8, residuals = 1, n.sim = 1000)

resi = cum.residuals(fit_res, data = TRACE, cum.resid = 1)

plot(resi, score = 2, xlab = "age")

# Model does not fit well; as the plot varies away from the cumulative residuals

## categorizing age into 3 groups-- lower, middle and upper

X= model.matrix(~1+cut(TRACE$age, 3, include.lowest = TRUE), data = TRACE)
colnames(X) = c("lower age", "middle age", "upper age")

resi_age = cum.residuals(fit_res, data = TRACE, X, n.sim = 1000)

par(mfrow=c(2,2))
plot(resi_age, score = 1)
summary(resi_age)     

# the fit does not seem to be better when age is split into 3 groups (lower, middle and upper)