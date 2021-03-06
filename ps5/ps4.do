clear all
set more off, perm
set scrollbufsize 2000000

*****import the database*****
import excel "C:\Users\cuiti\Master Study\Second Semester\econometrics\TIANYUCUI\ps4\Koop-Tobias.xlsx", sheet("Koop-Tobias") firstrow clear

*convert data into panel data
xtset PERSONID TIMETRND
bysort PERSONID: gen t = _n
*Represent the panel dimension of wages for 5 randomly selected individuals
tabulate TIMETRND LOGWAGE if PERSONID == 5
tabulate TIMETRND LOGWAGE if PERSONID == 15
tabulate TIMETRND LOGWAGE if PERSONID == 155
tabulate TIMETRND LOGWAGE if PERSONID == 1555
tabulate TIMETRND LOGWAGE if PERSONID == 1333

*Exercise 2*
*Random effect model
xtreg LOGWAGE EDUC POTEXPER, re

*Exercise 3* 
******************Fixed effect model******************
*Between Estimator 
xtreg LOGWAGE EDUC POTEXPER,be 
*Within Estimator 
xtreg LOGWAGE EDUC POTEXPER,fe
*As we take the first difference in Stata, the default setting is to take the difference for balanced data-
*for example when personid=1, Stata only take difference between t=5 and t=6, and omit other timetrend. Therefore, 
*we use the command xtsset to rearrange the data
xtset PERSONID t
gen logwage_D = D.LOGWAGE
gen educ_D = D.EDUC
gen potexper_D = D.POTEXPER
xtreg logwage_D educ_D potexper_D, fe


