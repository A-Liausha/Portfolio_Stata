/*
Purpose: Event Study Analysis
Announcement Date: 2022-10-07 (Friday)
Event Date (t=0):   2022-10-10 (Monday, first trading day)
Models: Market Model, Mean-Adjusted Model, Market-Adjusted Model
Windows: [-1,1], [-5,5], [-10,10]
*/

clear
pwd
cd "D:\XXXXXXXXXXXXX"
import delimited "D:\XXXXXXXXXXXXXX\Company Profile155923133\TRD_Co.csv",clear

merge 1:m stkcd using "D:\XXXXXXXXXXXXXX\daily.dta"
keep if _merge == 3  
drop _merge

generate v2 = date(listdt, "YMD")
format %td v2
drop if v2 > date("20211201","YMD")
rename v2 listdt1

sort stkcd

gen trdtt_1 = daily(trddt, "YMD")
format trdtt_1 %td


joinby trddt markettype using "D:\XXXXXXXXX\market.dta", unmatched(both)
keep if _merge == 3
drop _merge
duplicates drop stkcd trdtt_1, force

sort stkcd
sum trdtt_1
sum stkcd
by stkcd: gen count=_N
sum count 

drop if trdtt_1 < date("20211201","YMD")
drop if trdtt_1 > date("20221029","YMD") 

xtset stkcd trdtt_1
gen event_day = date("20221010", "YMD")
format event_day %td


by stkcd: gen datenum=_n
by stkcd: gen target = datenum if trdtt_1 == event_day
egen td = min(target), by(stkcd)
drop target
gen dif = datenum - td

//Market Model for event window [-1,1]
by stkcd: gen event_window = 1 if dif >= -1 & dif <= 1
egen count_event_obs = count (event_window), by(stkcd)
by stkcd: gen estimation_window = 1 if dif <= -11 & dif >= -191
egen count_est_obs = count(estimation_window), by(stkcd)

replace event_window = 0 if event_window == .
replace estimation_window = 0 if estimation_window == .

tab stkcd if count_event_obs < 3
tab stkcd if count_est_obs < 180
drop if count_event_obs < 3
drop if count_est_obs < 180

gen predicted_return = .
egen id = group(stkcd)

sum id, meanonly

forvalues i = 1/`r(max)' {    
list id stkcd if id == `i' & dif == 0
    reg dretwd  dretwdtl if id == `i' & estimation_window == 1
    predict p if id == `i'
    replace predicted_return = p if id == `i' & event_window == 1
    drop p 
	}

sort id trdtt_1
gen abnormal_return=dretwd-predicted_return if event_window==1
by id: egen cumulative_abnormal_return = sum(abnormal_return)
gen CAR = cumulative_abnormal_return if trdtt_1 == event_day

//Market Model for event window [-5,5]
gen event_window_2 = 1 if dif >= -5 & dif <= 5
egen count_event_obs_2 = count (event_window_2), by(stkcd)
replace event_window_2 = 0 if event_window_2 == .
tab stkcd if count_event_obs_2 < 11
drop if count_event_obs_2 < 11
gen predicted_return_2 = .
sum id, meanonly

forvalues i = 1/`r(max)' {    
list id stkcd if id == `i' & dif == 0
    reg dretwd  dretwdtl if id == `i' & estimation_window == 1
    predict p if id == `i'
    replace predicted_return_2 = p if id == `i' & event_window_2 == 1
    drop p 
	}

sort id trdtt_1
gen abnormal_return_2=dretwd-predicted_return_2 if event_window_2==1
by id: egen cumulative_abnormal_return_2 = sum(abnormal_return_2)
gen CAR2 = cumulative_abnormal_return_2 if trdtt_1 == event_day


//Market Model for event window [-10,10]
gen event_window_3 = 1 if dif >= -10 & dif <= 10
egen count_event_obs_3 = count (event_window_3), by(stkcd)
replace event_window_3 = 0 if event_window_3 == .
tab stkcd if count_event_obs_3 < 21
drop if count_event_obs_3 < 21
gen predicted_return_3 = .
sum id, meanonly

forvalues i = 1/`r(max)' {    
list id stkcd if id == `i' & dif == 0
    reg dretwd  dretwdtl if id == `i' & estimation_window == 1
    predict p if id == `i'
    replace predicted_return_3 = p if id == `i' & event_window_3 == 1
    drop p 
	}

	sort id trdtt_1
gen abnormal_return_3=dretwd-predicted_return_3 if event_window_3==1
by id: egen cumulative_abnormal_return_3 = sum(abnormal_return_3)
gen CAR3 = cumulative_abnormal_return_3 if trdtt_1 == event_day

ttest CAR == 0
ttest CAR2 == 0
ttest CAR3 == 0

//Wilcoxon signed-rank test
sum CAR, detail
signrank CAR=0

sum CAR2, detail
signrank CAR2=0

sum CAR3, detail
signrank CAR3=0

//One-sample binomial sign test
signtest CAR=0
signtest CAR2=0
signtest CAR3=0



//* Mean-Adjusted Return Model for event window [-1,1]
gen temp = dretwd if estimation_window == 1
bysort stkcd: egen mean_observation = mean(temp)
drop temp
egen first_non_missing = min(mean_observation / (mean_observation != .)), by(stkcd)
replace mean_observation = first_non_missing if missing(mean_observation)
drop first_non_missing
gen mean_model = mean_observation if event_window == 1
gen abnormal_return_1 = dretwd - mean_model if event_window == 1
by stkcd: egen cumulative_abnormal_return1 = sum(abnormal_return_1)
gen CAR1_mean = cumulative_abnormal_return1 if trdtt_1 == event_day
ttest CAR1_mean == 0

* Mean-Adjusted Return Model for event window [-5,5]
gen temp2 = dretwd if estimation_window == 1
bysort stkcd: egen mean_observation2 = mean(temp2)
drop temp2
egen first_non_missing2 = min(mean_observation2 / (mean_observation2 != .)), by(stkcd)
replace mean_observation2 = first_non_missing2 if missing(mean_observation2)
drop first_non_missing2
gen mean_model2 = mean_observation2 if event_window_2 == 1
gen abnormal_return_2 = dretwd - mean_model2 if event_window_2 == 1
by stkcd: egen cumulative_abnormal_return22 = sum(abnormal_return_2)
gen CAR2_mean = cumulative_abnormal_return22 if trdtt_1 == event_day
ttest CAR2_mean == 0

* Mean-Adjusted Return Model for event window [-10,10]
gen temp3 = dretwd if estimation_window == 1
bysort stkcd: egen mean_observation3 = mean(temp3)
drop temp3
egen first_non_missing3 = min(mean_observation3 / (mean_observation3 != .)), by(stkcd)
replace mean_observation3 = first_non_missing3 if missing(mean_observation3)
drop first_non_missing3
gen mean_model3 = mean_observation3 if event_window_3 == 1
gen abnormal_return_33 = dretwd - mean_model3 if event_window_3 == 1
by stkcd: egen cumulative_abnormal_return33 = sum(abnormal_return_33)
gen CAR3_mean = cumulative_abnormal_return33 if trdtt_1 == event_day
ttest CAR3_mean == 0

//Wilcoxon signed-rank test
sum CAR1_mean, detail 
signrank CAR1_mean=0

sum CAR2_mean, detail 
signrank CAR2_mean=0

sum CAR3_mean, detail 
signrank CAR3_mean=0

//One-sample binomial sign test
signtest CAR1_mean=0
signtest CAR2_mean=0
signtest CAR3_mean=0



//Market-Adjusted Model for event window [-1,1]
by stkcd: gen market1=dretwdtl if event_window==1
gen abnormal_return11=dretwd-market1 if event_window==1
by stkcd: egen cumulative_abnormal_return_11 = sum(abnormal_return11)
gen CAR11 = cumulative_abnormal_return_11 if trdtt_1 == event_day
ttest CAR11 == 0

//Market-Adjusted Model for event window [-5,5]
by stkcd: gen market2=dretwdtl if event_window_2==1
gen abnormal_return22=dretwd-market2 if event_window_2==1
by stkcd: egen cumulative_abnormal_return_22 = sum(abnormal_return22)
gen CAR22 = cumulative_abnormal_return_22 if trdtt_1 == event_day
ttest CAR22 == 0

//Market-Adjusted Model for event window [-10,10]
by stkcd: gen market3=dretwdtl if event_window_3==1
gen abnormal_return33=dretwd-market3 if event_window_3==1
by stkcd: egen cumulative_abnormal_return_33 = sum(abnormal_return33)
gen CAR33 = cumulative_abnormal_return_33 if trdtt_1 == event_day
ttest CAR33 == 0


//Wilcoxon signed-rank test
sum CAR11, detail 
signrank CAR11=0

sum CAR22, detail 
signrank CAR22=0

sum CAR33, detail 
signrank CAR33=0

//One-sample binomial sign test
signtest CAR11=0
signtest CAR22=0
signtest CAR33=0







