******************No Opinion Project************************
*1. Decomposition analysis: Table 3 Results
*Need to update pathway*
use "C:\Users\yunmeilu\Box\MTF police\MTF police\Data\12th\mtf_form3_12th_all.dta", replace

*Clean up variables
recode race_f (2=1 "White") (1=2 "Black") (missing=3 "Other"), gen(race3)
recode sch_c1 (1=0 "none") (2/3=1 "missed 1-2 days")(4/7=2 "missed 3+days"), gen (sch_c3)
recode p_edu_n (1/2=2 "lower than high sch") (3/4=1 "high sch")(5/6=0 "college and above"), gen (p_edu_n3)
recode year (1976/1985=1 "Cohort76-85") (2009/2018=2 "Cohort09-18") (else=.), gen(cohort2)

gen vic=.
replace vic=0 if any_p=="No"
replace vic=1 if any_p=="Yes"

egen region2 = group(region1)

*logistic regression first
svyset [pweight=weight]

*Table 2 Output
svy: logit pol_p1_no i.sex1 i.race3 i.rural5 i.ideo2 i.sch_c3 i.p_edu_n3 vic i.region2 happy1_n care_soc1_n if cohort2==1
estimates store m1, title("C1980")
svy: logit pol_p1_no i.sex1 i.race3 i.rural5 i.ideo2 i.sch_c3 i.p_edu_n3 vic i.region2 happy1_n care_soc1_n if cohort2==2
estimates store m2, title("C2015")
cd "C:\Users\yunmeilu\Box\MTF police\No opinion project\output"
esttab m1 m2 using logit_10.15.26.csv, cells(b(star fmt(3)) se(par fmt(3))) stats(N, fmt(a)) 


*Get Ready for Decomposition Analysis
*oaxaca deocmposition: recode categorical variable
tabulate race3, generate(race_in) nofreq
tabulate rural5, generate(rural_in) nofreq
tabulate ideo2, generate(ideo_in) nofreq
tabulate sch_c3, generate(sch_in) nofreq
tabulate p_edu_n3, generate(pedu_in) nofreq
tabulate region1, generate(region_in) nofreq


ssc install oaxaca, replace 
svyset [pweight=weight]

*Table 3 Output: 
oaxaca pol_p1_no (race: normalize(b.race_in1 race_in2 race_in3)) sex1 rural5 sch_c3 p_edu_n vic (ideo: normalize(b.ideo_in1 ideo_in2 ideo_in3 ideo_in4 ideo_in5)) (region: normalize(b.region_in1 region_in2 region_in3 region_in4)) happy1_n care_soc1_n, by (cohort2) svy logit threefold(reverse) detail  /*the threefold(revser) specify Cohort 1 as the reference category*/

**Just for reference, decomposition without grouping categories**
*oaxaca pol_p1_no normalize(b.race_in1 race_in2 race_in3) sex1 rural5 sch_c3 p_edu_n vic normalize(b.ideo_in1 ideo_in2 ideo_in3 ideo_in4 ideo_in5) normalize(b.region_in1 region_in2 region_in3 region_in4) happy1_n care_soc1_n, by (cohort2) svy logit threefold(reverse) detail


********************************************************************************
*Supplemental Analysis Using Cohort 2 as reference
*Appendix C Output: 
oaxaca pol_p1_no (race: normalize(b.race_in1 race_in2 race_in3)) sex1 rural5 sch_c3 p_edu_n vic (ideo: normalize(b.ideo_in1 ideo_in2 ideo_in3 ideo_in4 ideo_in5)) (region: normalize(b.region_in1 region_in2 region_in3 region_in4)) happy1_n care_soc1_n, by (cohort2) svy logit threefold detail

********************************************************************************
*Supplemental Analysis Using Linear Probability Models
*Appendix F Output: 

svy: regress pol_p1_no i.sex1 i.race3 i.rural5 i.ideo2 i.sch_c3 i.p_edu_n3 vic i.region2 happy1_n care_soc1_n if cohort2==1
estimates store m1, title("C1980")
svy: regress pol_p1_no i.sex1 i.race3 i.rural5 i.ideo2 i.sch_c3 i.p_edu_n3 vic i.region2 happy1_n care_soc1_n if cohort2==2
estimates store m2, title("C2015")
cd "C:\Users\yunmeilu\Box\MTF police\No opinion project\output"
esttab m1 m2 using sup_linearprob.csv, cells(b(star fmt(3)) se(par fmt(3))) stats(N, fmt(a)) 

*Decomposition with linear probability model
oaxaca pol_p1_no (race: normalize(b.race_in1 race_in2 race_in3)) sex1 rural5 sch_c3 p_edu_n vic (ideo: normalize(b.ideo_in1 ideo_in2 ideo_in3 ideo_in4 ideo_in5)) (region: normalize(b.region_in1 region_in2 region_in3 region_in4)) happy1_n care_soc1_n, by (cohort2) svy threefold(reverse) detail

********************************************************************************
*supplemental analysis(not shown): Cohort 1976 vs. 1986
recode year (1976/1985=1 "Cohort76-85") (1986/1995=2 "Cohort86-95") (else=.), gen(cohort3)

oaxaca pol_p1_no (race: normalize(b.race_in1 race_in2 race_in3)) sex1 rural5 sch_c3 p_edu_n vic (ideo: normalize(b.ideo_in1 ideo_in2 ideo_in3 ideo_in4 ideo_in5)) (region: normalize(b.region_in1 region_in2 region_in3 region_in4)) happy1_n care_soc1_n, by (cohort3) svy logit threefold(reverse) detail


********************************************************************************
*supplemental analysis (not shown): decomposition for safety concerns*
recode year (1992/2001=1 "Cohort92-01") (2009/2018=2 "Cohort09-18") (else=.), gen(cohort2a)
tabulate us_tosch1 , generate(us_tosch_in) nofreq

oaxaca pol_p1_no normalize(b.us_tosch_in1 us_tosch_in2 us_tosch_in3 us_tosch_in4 us_tosch_in5) (race: normalize(b.race_in1 race_in2 race_in3)) sex1 rural5 sch_c3 p_edu_n vic (ideo: normalize(b.ideo_in1 ideo_in2 ideo_in3 ideo_in4 ideo_in5)) (region: normalize(b.region_in1 region_in2 region_in3 region_in4)) happy1_n care_soc1_n, by (cohort2a) svy logit threefold(reverse) detail
