///////////////////////only keep full twin-pair and calcaulate the intraclass correlation coefficients/////////////////////////////////////


bysort fam_nb: generate a=_n
bysort fam_nb: egen b= sum(a)
drop if b ==1
drop a b

drop if GBI17_sum_rec==.
bysort fam_nb: generate a=_n
bysort fam_nb: egen b= sum(a)
drop if b ==1
drop a b

drop if zygocity ==0
loneway GBI17_sum_rec fam_nb if zygocity ==1
loneway GBI17_sum_rec fam_nb if zygocity ==2

loneway GBI17_sum_rec fam_nb if zygocity ==1 & separatedall ==0
loneway GBI17_sum_rec fam_nb if zygocity ==2 & separatedall ==0

loneway GBI17_sum_rec fam_nb if zygocity ==1 & separatedall ==1
loneway GBI17_sum_rec fam_nb if zygocity ==2 & separatedall ==1

loneway GBI17_sum_rec fam_nb if zygocity ==1 & separatedall ==2
loneway GBI17_sum_rec fam_nb if zygocity ==2 & separatedall ==2

loneway GBI17_sum_rec fam_nb if zygocity ==1 & separatedall ==3
loneway GBI17_sum_rec fam_nb if zygocity ==2 & separatedall ==3


//////////////////////univariate ANOVA and linear regression/////////////////////////////////////
anova GBI17_sum_rec zygocity
anova GBI22_sum_rec zygocity

anova GBI17_sum_rec smokstat
anova GBI22_sum_rec smokstat


anova GBI17_sum_rec secondary
anova GBI22_sum_rec secondary


anova GBI17_sum_rec i.work
anova GBI22_sum_rec i.work

anova GBI17_sum_rec parental_edu
anova GBI22_sum_rec parental_edu


regress log_GB1_17 i.separatedall, cluster(fam_nb) 
regress log_GB1_17 i.separatedall sex i.zygocity  i.work i.secondary i.edu_m_mr_11 i.edu_f_fr_11 i.smokstat, cluster(fam_nb)
regress log_GB1_17 i.movingtime17, cluster(fam_nb)
regress log_GB1_17 i.movingtime17 sex i.zygocity  i.work i.secondary i.smokstat sparated_17  i.edu_m_mr_11 i.edu_f_fr_11  , cluster(fam_nb)
regress log_GB1_17 distance_17, cluster(fam_nb)
regress log_GB1_17 distance_17 sex i.zygocity  i.work i.secondary i.smokstat sparated_17  i.edu_m_mr_11 i.edu_f_fr_11 , cluster(fam_nb)



regress log_GB1_22 i.separatedall, cluster(fam_nb)
regress log_GB1_22 i.separatedall sex i.zygocity  i.work i.secondary i.smokstat i.edu_m_mr_11 i.edu_f_fr_11 age_response, cluster(fam_nb)
regress log_GB1_22 i.movingtime17, cluster(fam_nb)
regress log_GB1_22 i.movingtime17 sex i.zygocity  i.work i.secondary i.smokstat sparated_17  i.edu_m_mr_11 i.edu_f_fr_11  age_response , cluster(fam_nb)
regress log_GB1_22 distance_17, cluster(fam_nb)
regress log_GB1_22 distance_17 sex i.zygocity  i.work i.secondary i.smokstat sparated_17  i.edu_m_mr_11 i.edu_f_fr_11  age_response , cluster(fam_nb)


regress GBI_change i.separatedall , cluster(fam_nb)
regress GBI_change i.separatedall sex i.zygocity  i.work i.secondary i.smokstat i.edu_m_mr_11 i.edu_f_fr_11 age_response, cluster(fam_nb)
regress GBI_change i.movingtime17 , cluster(fam_nb)
regress GBI_change i.movingtime17 sex i.zygocity  i.work i.secondary i.smokstat sparated_17 i.edu_m_mr_11 i.edu_f_fr_11  age_response , cluster(fam_nb)
regress GBI_change distance_17, cluster(fam_nb)
regress GBI_change distance_17 sex i.zygocity  i.work i.secondary i.smokstat sparated_17 i.edu_m_mr_11 i.edu_f_fr_11  age_response , cluster(fam_nb)


//////////////////////sensitivity analysis with mental health outcome at age 14/////////////////////////////////////
drop if mi(separatedall, sr14_hyp_imp, sr14_agg, sr14_inatt, sr14_dep, sr14_anx) 
regress sr14_hyp_imp i.separatedall sex i.zygocity i.edu_m_mr_11 i.edu_f_fr_11, cluster(fam_nb)
regress sr14_agg i.separatedall sex i.zygocity i.edu_m_mr_11 i.edu_f_fr_11, cluster(fam_nb)
regress sr14_inatt i.separatedall sex i.zygocity i.edu_m_mr_11 i.edu_f_fr_11, cluster(fam_nb)
regress sr14_dep i.separatedall sex i.zygocity i.edu_m_mr_11 i.edu_f_fr_11, cluster(fam_nb)
regress sr14_anx i.separatedall sex i.zygocity i.edu_m_mr_11 i.edu_f_fr_11, cluster(fam_nb)



//////// reshape for repeated measured method ///////////////
rename log_GB1_22  log_GB1_2
rename log_GB1_17  log_GB1_1
keep   log_GB1_2   log_GB1_1 person_nb fam_nb separatedall  work secondary smokstat sex zygocity birthyr age_moved edu_m_mr_11 edu_f_fr_11 movingtime17  age_response sparated_17
reshape long  log_GB1_, i(person_nb) j(j)

//////repeated measured mixed method/////////////////////////
xtmixed log_GB1_ i.separatedall##j sex i.zygocity i.secondary i.work  i.smokstat i.edu_m_mr_11 i.edu_f_fr_11  age_response  || person_nb:, vce(robust)
contrast i.separatedall##j

lincom 1.separatedall+2.j#1.separatedall
lincom 2.separatedall+2.j#2.separatedall
lincom 3.separatedall+2.j#3.separatedall

label define j5 1"At age 17" 2"In young adulthood                          "
label value j j5
margins i.separatedall#j
marginsplot, x(j) xtitle(GBI assessment (Log-transformed)) title(Predictive margin with 95%CI)


/////////////boy////////////////////
xtmixed log_GB1_ i.separatedall##j i.zygocity i.secondary i.work  i.smokstat i.edu_m_mr_11 i.edu_f_fr_11  age_response  || person_nb: if sex==1, vce(robust)
contrast i.separatedall##j

lincom 1.separatedall+2.j#1.separatedall
lincom 2.separatedall+2.j#2.separatedall
lincom 3.separatedall+2.j#3.separatedall


/////////////girl////////////////////
xtmixed log_GB1_ i.separatedall##j i.zygocity i.secondary i.work  i.smokstat i.edu_m_mr_11 i.edu_f_fr_11  age_response  || person_nb: if sex==2, vce(robust)
contrast i.separatedall##j

lincom 1.separatedall+2.j#1.separatedall
lincom 2.separatedall+2.j#2.separatedall
lincom 3.separatedall+2.j#3.separatedall


/////////////movingtimes////////////////////
xtmixed log_GB1_ i.movingtime17##j sex i.zygocity i.secondary i.work  i.smokstat i.edu_m_mr_11 i.edu_f_fr_11  age_response i.sparated_17 || person_nb:, vce(robust)
contrast i.movingtime17##j

lincom 1.movingtime17+2.j#1.movingtime17
lincom 2.movingtime17+2.j#2.movingtime17
lincom 3.movingtime17+2.j#3.movingtime17

label define j4 1"At age 17" 2"In young adulthood                          "
label value j j4

margins i.movingtime17#j
marginsplot, x(j) xtitle(GBI assessment (Log-transformed)) title(Predictive margin with 95%CI)













