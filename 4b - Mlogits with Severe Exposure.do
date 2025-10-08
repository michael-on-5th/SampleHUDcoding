// do file for creating severe disaster indicators and running probit models

// severe defined as > 20 registrationsin a zip code. can be altered for sensitivity analysis

// multinomial logit with annual panel
clear all
frame rename default panel 
use "C:\Users\H54106\Desktop\fha_claims\fhapanel.dta"

frame create details
frame change details 

use "C:\Users\H54106\Desktop\fha_claims\IHPdetails_long.dta", replace

count if valid < 20
count if valid > 19
count
drop if valid < 20

sort zip5 begindisaster
bysort zip5: gen sevdisastercount = _n

bysort zip5: egen max_sevexp = max(sevdisastercount)

forvalues x = 1/11 {
	count if max_exp == `x' & sevdisastercount == 1
	}  
// convert to wide for frame link/merge with fha panel
keep 	disasternumber startyear startmonth startday begindisaster ///
	ihprogramdeclared iaprogramdeclared paprogramdeclared hmprogramdeclared ///
	valid_regs num_inspected total_damage ///
	noDamage damage_10k damage_10k_20k damage_20k_30k damage_30k ///
	num_approved ihp_amt repair_amt rental_amt otherneeds_amt ///
	approved_10k approved_10k_25l approved_25k duplicate_zips zip5 sevdisastercount
	

reshape wide ///
	disasternumber startyear startmonth startday begindisaster ///
	ihprogramdeclared iaprogramdeclared paprogramdeclared hmprogramdeclared ///
	valid_regs num_inspected total_damage ///
	noDamage damage_10k damage_10k_20k damage_20k_30k damage_30k ///
	num_approved ihp_amt repair_amt rental_amt otherneeds_amt ///
	approved_10k approved_10k_25l approved_25k duplicate_zips , /// 
	i(zip5) j(sevdisastercount)

	// check visually how the sorted panel looks, should not be gaps in n disasters per zip
sort ihprogramdeclared1 ihprogramdeclared2 ihprogramdeclared3 ihprogramdeclared4 ihprogramdeclared5 ihprogramdeclared6 ihprogramdeclared7 ihprogramdeclared8 ihprogramdeclared9 

//resort for future merge 
sort zip5 

frame change panel
sort zip5 

frlink m:1 zip5, frame(details) generate(_link_details)


gen exposure1_12m = 0
gen exposure13_24 = 0
gen exposure25_36 = 0

/*
replace exposure13_24 = 0
replace exposure1_12m = 0
replace exposure25_36 = 0
replace exposurelag_last3yrs = 0
*/
quietly {

forvalues x = 1/9 {
	replace exposure1_12m = 1 if ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
		frval(_link_details, valid_regs`x') > 20 & /// more than 20 registrations
		sif_amort <= frval(_link_details, begindisaster`x') & /// amort began at least the same year-month as the disaster
		obs_sif - frval(_link_details, begindisaster`x') >= 0 & /// observation is between 0 and 3 years of disaster declaration
		obs_sif - frval(_link_details, begindisaster`x')  <= 12 
		
	replace exposure13_24 = 1 if ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
		frval(_link_details, valid_regs`x') > 20 & /// more than 20 registrations
		sif_amort <= frval(_link_details, begindisaster`x') & /// amort began at least the same year-month as the disaster
		obs_sif - frval(_link_details, begindisaster`x') >= 13 & /// observation is between 0 and 3 years of disaster declaration
		obs_sif - frval(_link_details, begindisaster`x')  <= 24  
		
	replace exposure25_36 = 1 if ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
		frval(_link_details, valid_regs`x') > 20 & /// more than 20 registrations
		sif_amort <= frval(_link_details, begindisaster`x') & /// amort began at least the same year-month as the disaster
		obs_sif - frval(_link_details, begindisaster`x') >= 25 & /// observation is between 0 and 3 years of disaster declaration
		obs_sif - frval(_link_details, begindisaster`x')  <= 36  
		}
}

generate termYear = year(terminateDate)
generate termMonth = month(terminateDate)
generate termMonth_sif = 1960 - termYear + termMonth

// already have defualt year indicator 

generate outcome = "active"

//replace outcome = "active" 
sort caseN policyYear 
by caseN: generate obscounter = _n 
by caseN: egen mortgageLife = max(obscounter)
gen final_obs = 0
replace final_obs = 1 if obscounter == mortgageLife

replace outcome = "prepay" if ///
	final_obs ///
	& insureStatus == "T"
	
replace outcome = "claim" if ///
	defYearBool == 1

sort caseN policyYear
by caseN: ///
	replace outcome = "dead"  if ///
		outcome[_n-1] == "claim" | ///
		outcome[_n-1] == "prepay"| ///
		outcome[_n-1] == "dead" 
	

gen dead = 0
replace dead = 1 if outcome == "dead"

/*
rename pY py1
replace py1 = 0
replace py1 = 1 if policyYear == 1
*/

gen outcome_num = 1 if outcome == "active"
replace outcome_num = 2 if outcome == "prepay"
replace outcome_num = 3 if outcome == "claim"
// drop exposurelag_last3yrs
gen exposurelag_last3yrs = 1 if exposure1_12m + exposure13_24 + exposure25_36 > 0
replace exposurelag_last3yrs = 0 if exposurelag_last3yrs == .

rename beginYear begAmortYear 
rename unempRate unemprate
//save "C:\Users\H54106\Desktop\fha_claims\fhapanel_mlogit.dta", replace

// run multinomial logit models 
mlogit outcome_num exposure1_12m exposure13_24 exposure25_36 frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1 & begAmortYear > 2009, vce(robust) rrr
estimates store lags3PostRec_severe

mlogit outcome_num   exposurelag_last3yrs frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1 & begAmortYear > 2009, vce(robust) rrr
estimates store lag1PostRec_severe

mlogit outcome_num exposure1_12m exposure13_24 exposure25_36 frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1,  vce(robust) rrr
estimates store lags3Full_severe

mlogit outcome_num   exposurelag_last3yrs frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1 , vce(robust) rrr
estimates store lag1Full_severe


