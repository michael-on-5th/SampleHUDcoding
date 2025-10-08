// do file for creating maps
clear all 

use "C:\Users\H54106\Desktop\fha_claims\sample_tract.dta" 
frame create sumstats
frame change sumstats
use "C:\Users\H54106\Desktop\fha_claims\fha_py1_summStats.dta"
frame change default

frlink 1:1 caseN, frame(sumstats) generate(_link_stats)

frget begAmortDate sif_amort terminateDate insureStatus intRate origMortgAmt hpi ltv fico amerInd asian black hawaiian hispanic nonDisc white , from(_link_stats)

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
	}
// by disaster type --------------------------------------------------------------------------------------

frame create details
frame change details

use "C:\Users\H54106\Desktop\fha_claims\IHPdetails_wide.dta"

forvalues x = 1/11 {
	replace incidenttype`x' = "SevereStorms" if incidenttype`x' == "Severe Storm(s)"
	replace incidenttype`x' = "Mud_Landslide" if incidenttype`x' == "Mud/Landslide"
	}
frame change default 
// create termination sif 

generate term_year = year(terminateDate)
generate term_month = month(terminateDate)
generate sif_term = (term_year - 1960)*12 + term_month 



//link frames 
rename zip zip5 
frlink m:1 zip5, frame(details) generate(_link_details)


// create any disaster exposure vars and disaster type exposure vars -------------------------------------------------------
gen exp_Ever = 0
local dis_types "Earthquake Fire Flood Hurricane Mud_Landslide SevereStorms Tornado Typhoon Volcano"
foreach d of local dis_types {
	generate exp_`d' = 0
	forvalues x = 1/11 {
		replace exp_`d' = 1 if frval(_link_details, incidenttype`x') == "`d'" & ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
		sif_amort <= frval(_link_details, begindisaster`x') & /// amort began at least the same year-month as the disaster
		(sif_term - frval(_link_details, begindisaster`x') >= 0 | /// loan termination is after disaster or loan still active
		insureStatus == "A")
		}
	replace exp_Ever = 1 if exp_`d' == 1
	}

frame create nri 
frame change nri 
use "C:\Users\H54106\Desktop\fha_claims\nriCounty.dta"
rename stcofips fips5
 

frame change default
destring fips5, replace 

// calculate population shares by primary borrower race group 
egen sampleSize = count(caseN)

bysort primaryRace: egen raceGroupN = count(caseN)

gen indivRaceShare = 100/raceGroupN


frlink m:1 fips5, frame(nri) generate(_link_nri)
// need to go back and grab the rest of hlrs that were accidentally deleted
local hlrs "eal_ratng "
foreach x of local hlrs {
	frget `x', from(_link_nri)
	}
	
local riskRatings "risk_ratng cfld_riskr hrcn_riskr rfld_riskr wfir_riskr swnd_riskr erqk_riskr trnd_riskr hail_riskr"
foreach x of local riskRatings {
	frget `x', from(_link_nri)
	}
	

frame copy default tract
frame change tract

collapse ///
	(sum) exp_Ever (sum) exp_Earthquake (sum) exp_Fire (sum) exp_Flood ///
	(sum) exp_Hurricane (sum) exp_Mud_Landslide (sum) exp_SevereStorms ///
	(sum) exp_Tornado (sum) exp_Typhoon (sum) exp_Volcano (count) one, ///
	by(primaryRace )
// i need a command to export this frame into an excel which is the base for table 5.3

local vars "exp_Ever exp_Earthquake exp_Fire exp_Flood exp_Hurricane exp_Mud_Landslide exp_SevereStorms exp_Tornado exp_Typhoon exp_Volcano"
foreach x of local vars {
	generate share_`x' = `x'/one
	}


local riskRatings "risk_ratng cfld_riskr hrcn_riskr rfld_riskr wfir_riskr swnd_riskr erqk_riskr trnd_riskr hail_riskr"
foreach x of local riskRatings {
	tabout ///
		`x' primaryRace  ///
		using C:\Users\H54106\Desktop\fha_claims\nriRiskRating_byRace.csv, ///
		cells(sum indivRaceShare) ///
		f(1p) ///
		clab(Share_of_Race_Group) ///
		layout(row) ///
		sum style(csv) append
		}
	
local hlrs " eal_ratng "
foreach x of local hlrs {
	tabout ///
		`x' primaryRace  ///
		using C:\Users\H54106\Desktop\fha_claims\hlrRating_byRace.csv, ///
		cells(sum indivRaceShare) ///
		f(1p) ///
		clab(Share_of_Race_Group) ///
		layout(row) ///
		sum style(csv) append
		}	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	