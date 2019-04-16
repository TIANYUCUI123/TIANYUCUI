***set up for work place****
clear all
set more off, perm
set scrollbufsize 2000000
****set working directory***
cd "C:\Users\cuiti\Master Study\Second Semester\econometrics\TIANYUCUI\ps5"
***set the observation***
set obs 10000
*****exercise1: generate the variables to prepare for the regression*****
generate X1 = runiform(1,3)
generate X2 = runiform(0,1)
generate X3=rbinomial(10000,0.3)
gen eps = rnormal(2,1)
gen Y = 0.5 + 1.2*X1 + -0.9*X2 + 0.1*X3 + eps
* Basic Summary Stats
su Y
egen Y_mean = mean(Y)
gen Y_dum = (Y>Y_mean)
tab Y_dum

****exercise 2: ols regression******
 correlate Y X1
 *the correlation of Y and X is 0.1416
 display 0.1416-1.2
 ***do the regression
 reg Y X1 X2 X3
 

 *****exercise 3&4: write the probit model and logit model****
 
probit  Y_dum  X1 X2 X3
logit Y_dum X1 X2 X3 

*****calculate the marginal effeccts of probit model and logit model****
probit  Y_dum  X1 X2 X3
quietly probit $ylist $xlist
margins, dydx(*) atmeans
margins, dydx(*)

logit Y_dum X1 X2 X3 
quietly logit $ylist $xlist
margins, dydx(*) atmeans
margins, dydx(*)
