// do file for creating severe disaster indicators and running probit models

// severe defined as > 20 registrationsin a zip code. can be altered for sensitivity analysis

clear all 

frame rename default panel 
use "C:\Users\H54106\Desktop\fha_claims\fhapanel_base.dta"

frame create details
frame change details

use "C:\Users\H54106\Desktop\fha_claims\IHPdetails_wide.dta"

forvalues x = 1/11 {
	replace incidenttype`x' = "SevereStorms" if incidenttype`x' == "Severe Storm(s)"
	replace incidenttype`x' = "Mud_Landslide" if incidenttype`x' == "Mud/Landslide"
	gen disaster`x'start = mdy(startmonth`x', startday`x', startyear`x')
	}

frame change panel 
drop _link_details
frlink m:1 zip5, frame(details) generate(_link_details)

// unlinked observations are in zip5 that do not have any IHP eligible disaster decs

quietly {

gen exposure1_12m = 0
gen exposure13_24 = 0
gen exposure25_36 = 0
forvalues x = 1/11 {
	replace exposure1_12m = 1 if ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
		sif_amort <= frval(_link_details, begindisaster`x') & /// amort began at least the same year-month as the disaster
		obs_sif - frval(_link_details, begindisaster`x') >= 0 & /// observation is between 0 and 3 years of disaster declaration
		obs_sif - frval(_link_details, begindisaster`x')  <= 12  
		
	replace exposure13_24 = 1 if ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
		sif_amort <= frval(_link_details, begindisaster`x') & /// amort began at least the same year-month as the disaster
		obs_sif - frval(_link_details, begindisaster`x') >= 13 & /// observation is between 0 and 3 years of disaster declaration
		obs_sif - frval(_link_details, begindisaster`x')  <= 24  
		
	replace exposure25_36 = 1 if ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
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
replace outcome = "active"
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

gen exposurelag_last3yrs = 1 if exposure1_12m + exposure13_24 + exposure25_36 > 0
replace exposurelag_last3yrs = 0 if exposurelag_last3yrs == .

// unlinked observations are in zip5 that do not have any IHP eligible disaster decs
		
local dis_types "Earthquake Fire Flood Hurricane Mud_Landslide SevereStorms Tornado Typhoon Volcano"

foreach d of local dis_types {
	generate exp3y_`d' = 0
	forvalues x = 1/11 {
		replace exp3y_`d' = 1 if frval(_link_details, incidenttype`x') == "`d'" & ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
		sif_amort <= frval(_link_details, begindisaster`x') & /// amort began at least the same year-month as the disaster
		obs_sif - frval(_link_details, begindisaster`x') >= 0 & /// observation is between 0 and 3 years of disaster declaration
		obs_sif - frval(_link_details, begindisaster`x')  <= 36 
		}
	}

	// create race indicators 
	
gen latino = ""
replace latino = "Y" if hispanic == "Y" & white == "Y"
replace latino = "Y" if hispanic == "Y" & nonDisc == "Y"
replace latino = "Y" if hispanic == "Y" & amerInd == "" & asian == "" & black == "" & hawaiian == ""   

gen white_nh = ""
replace white_nh = "Y" if white == "Y" & hispanic != "Y"

gen nonDisclosed = "Y" if nonDisc == "Y" & latino == "" & amerInd == "" & asian == "" & black == "" & hawaiian == "" & white == "" 

local vars "amerInd asian black hawaiian latino nonDisclosed white_nh"
 foreach x of local vars {
	gen bin_`x' = 0
	replace bin_`x' = 1 if `x' == "Y"
	}
	
gen multipleRace = bin_amerInd + bin_asian + bin_black + bin_hawaiian + bin_latino + bin_nonDisclosed + bin_white_nh 

count if multipleRace == 0 & policyYear == 1
//  5 -> 5 mortgages have no race indicated and also did not check nonDisclosed. convert to nonDisclosed 

replace bin_nonDisclosed = 1 if multipleRace == 0

local vars "amerInd asian black hawaiian latino nonDisclosed white_nh"
foreach x of local vars {
	replace bin_`x' = 0 if multipleRace > 1 
	}
	

// generate factor race variables
local vars "amerInd asian black hawaiian latino nonDisclosed white_nh "
	gen primaryRace = ""
foreach x of local vars {
	replace primaryRace = "`x'" if bin_`x' == 1 & multipleRace < 2
	}
	replace primaryRace = "multipleRace" if multipleRace > 1
	
encode primaryRace, generate(race)

// generate race-disaster interactions by hand 
local vars "amerInd asian black hawaiian latino nonDisclosed white_nh multipleRace "
foreach x of local vars {
	generate exprace_`x' = 1 & primaryRace == "`x'" & exposurelag_last3yrs == 1 
	}

/* by credit score
https://www.experian.com/blogs/ask-experian/credit-education/score-basics/what-is-a-good-credit-score/
800 to 850: Excellent
740 to 799: Very good
670 to 739: Good
580 to 669: Fair
300 to 579: Poor
*/

generate fico_range = "poor"
replace fico_range = "fair" if fico > 579 & fico < 670
replace fico_range = "good" if fico > 669 & fico < 740
replace fico_range = "veryGood" if fico > 739 & fico < 800
replace fico_range = "excellent" if fico > 799

encode fico_range, generate(ficoRange)

local groups "poor fair good veryGood excellent"
foreach x of local groups {
	generate fico_`x'= 0
	replace fico_`x' = 1 if fico_range == "`x'"
	gen expfico_`x' = 0
	replace expfico_`x' = 1 if fico_range == "`x'" & exposurelag_last3yrs == 1
	}
rename unempRate unemprate
// by disaster type --------------------------------------------------------------------------------------


// run models

// model with race binary indicators as controls ----------------------------------------------------------------------
mlogit outcome_num exposurelag_last3yrs frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1 i.race  if  dead != 1 , vce(robust) rrr
	estimates store raceBinary
	
esttab raceBinary using "C:\Users\H54106\Desktop\fha_claims\result tables\annualMlogit_raceBinary.csv" , b(%7.2g) replace title ("Race Controls")  nogap se star(* .1 ** .05 *** .01) eform
	
// model with race and any disaster intarctions --------------------------------------------------------------------------------
mlogit outcome_num exposurelag_last3yrs#race frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1  if  dead != 1, vce(robust) rrr
	estimates store raceInteractionA

mlogit outcome_num exposurelag_last3yrs##race frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1  if  dead != 1, vce(robust) rrr
	estimates store raceInteractionB

mlogit outcome_num exprace_amerInd exprace_asian exprace_black exprace_hawaiian exprace_latino exprace_nonDisclosed exprace_white_nh exprace_multipleRace ///
 frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1  if  dead != 1, vce(robust) rrr
	estimates store raceInteractionD
	
	
mlogit outcome_num exprace_amerInd exprace_asian exprace_black exprace_hawaiian exprace_latino exprace_nonDisclosed exprace_white_nh exprace_multipleRace ///
	bin_amerInd bin_asian bin_black bin_hawaiian bin_latino bin_nonDisclosed  multipleRace ///
 frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1  if  dead != 1, vce(robust) rrr
	estimates store raceInteractionE	
	
esttab   raceInteractionD raceInteractionE   using "C:\Users\H54106\Desktop\fha_claims\result tables\annualMlogit_raceInteractions.csv" , b(%7.2g) replace title ("Race-Exposure Interactions") ar2 nogap se star(* .1 ** .05 *** .01) eform
	
// model with disaster exposure by type ----------------------------------------------------------------------------------------
mlogit outcome_num exp3y_* frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1 if dead != 1 , vce(robust) rrr
	estimates store disasterType
	
esttab disasterType using "C:\Users\H54106\Desktop\fha_claims\result tables\annualMlogit_disasterType.csv" , b(%7.2g) replace title ("Disaster Type Specific Exposure") ar2 nogap se star(* .1 ** .05 *** .01) eform	

/* model with disaster exposure by type and race----------------------------------------------------------------------------------------
mlogit outcome_num race#exp3y_* frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1 , vce(robust) rrr
	estimates store disasterTypeRace
	*/ 
	
// model with credit score group interactions ----------------------------------------------------------------------------------
mlogit outcome_num expfico_poor expfico_fair expfico_good expfico_veryGood expfico_excellent ///
	fico_poor fico_fair  fico_veryGood fico_excellent ///
 frontDTI  giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1 if  dead != 1, vce(robust) rrr
	estimates store creditGroup
	
esttab creditGroup using "C:\Users\H54106\Desktop\fha_claims\result tables\annualMlogit_creditGroupB.csv" , b(%7.2g) replace title ("Credit Score-Exposure Interactions")  nogap se star(* .1 ** .05 *** .01) eform	


	
/*model with race-disaster type interactions	----------------------------------------------------------------------------------
mlogit outcome_num exposure1_12m exposure13_24 exposure25_36 frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1 & begAmortYear > 2009, vce(robust) 

esttab raceInteraction disasterType using "C:\Users\H54106\Desktop\fha_claims\results_raceInteraction.csv" , b(%7.2g) replace title ("Disaster and Race specific Exposure") ar2 nogap se star(* .1 ** .05 *** .01) eform


/*--------------------------------------------------------------------------------------------------------------------------------

// model with disaster exposure by type interacted with raceBinary
logistic defYearBool frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unempRate ratePMMS py1 exp_*#bin_* if filter == 0, vce(robust)
	estimates store raceDisaster
	
esttab raceDisaster using "C:\Users\H54106\Desktop\fha_claims\table3_results.csv" , b(%7.2g) replace title ("Race by Disaster Exposure") ar2 nogap se star(* .1 ** .05 *** .01) ///
	order (disasterExposure3y disaster1Exposure3y disaster2Exposure3y disaster3plus severeExposure frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unempRate ratePMMS py1 ) eform



































