clear all
set more off, perm
set scrollbufsize 2000000


*****import the database*****
import excel "C:\Users\cuiti\Desktop\data1.xlsx", sheet("data1") firstrow clear
drop A
save "C:\Users\cuiti\Desktop\data1.dta",replace

******import the data2*****
import excel "C:\Users\cuiti\Desktop\data2.xlsx", sheet("data2") firstrow clear
drop A
save "C:\Users\cuiti\Desktop\data2.dta",replace
********merge the database*****
merge 1:m hhid using "C:\Users\cuiti\Desktop\data1.dta"

summarize PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub 

tabulate choice
tabulate Income choice


******question2******
**rename the product and price***
drop hhid
rename (PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub )(c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
reshape long c,i(v1) j(price)

gen dum = cond(price == choice,1,0)

asclogit dum c,case(v1) alternatives(price)

est sto c_logit
estat mfx
******question3******
 mlogit Income  PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub
 
 ****question4-clogit*****

*****question4-mlogit*****
 mfx, predict(p outcome(2.5)) varlist(PBB_Stk)
 mfx, predict(p outcome(2.5)) varlist(PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub)
 mfx, predict(p outcome(7.5)) varlist(PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub)
 mfx, predict(p outcome(12.5)) varlist(PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub)
 mfx, predict(p outcome(17.5)) varlist(PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub)
 mfx, predict(p outcome(27.5)) varlist(PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub)
 mfx, predict(p outcome(7.5)) varlist(PBB_Stk    PHse_Stk   PImp_Stk   PPk_Tub    PHse_Tub   PPk_Stk    PFl_Stk    PGen_Stk   PSS_Tub    PFl_Tub)
