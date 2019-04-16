clear all
set more off, perm
set scrollbufsize 2000000
*****set the working dictionary*******
cd "C:\Users\cuiti\Master Study\Second Semester\econometrics\TIANYUCUI\ps5"
*****import the database*****
import excel "C:\Users\cuiti\Master Study\Second Semester\econometrics\TIANYUCUI\ps5\data1.xlsx", sheet("data1") firstrow clear
drop A
save "C:\Users\cuiti\Desktop\data1.dta",replace

******import the data2*****
import excel "C:\Users\cuiti\Master Study\Second Semester\econometrics\TIANYUCUI\ps5\data2.xlsx", sheet("data2") firstrow clear
drop A
save "C:\Users\cuiti\Desktop\data2.dta",replace
********merge the database*****
merge 1:m hhid using "C:\Users\cuiti\Desktop\data1.dta"

summarize PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub 

tabulate choice
tabulate Income choice


******question2******
**rename the product and price***
drop hhid Fs3_4 Fs5 Fam_Size college whtcollar retired _merge
gen n = 4410
gen v1 = _n
rename (PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub )(c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
reshape long c,i(v1) j(price)
gen dum = cond(price == choice,1,0)

asclogit dum c,case(v1) alternatives(price)
est sto c_logit
estat mfx

asclogit dum, case(v1) alternatives(price) casevar(Income)
est sto m_logit
estat mfx

******question3******
asclogit dum, case(v1) alternatives(price) casevar(Income)
est sto m_logit
estat mfx
 ****question4-clogit*****
asclogit dum, case(v1) alternatives(price) casevar(Income)
est sto m_logit
estat mfx
*****question4-mlogit*****
********question5*****
asmixlogit dum, case(v1)  alternatives(price)casevar(Income) 
estimate store mixlogit
drop if choice == 10
drop if price == 10
asmixlogit dum,  casevar(Income) alternative(price) case(v1)
estimate store mixlogitpartial
hausman mixlogitpartial mixlogit, alleqs constant
