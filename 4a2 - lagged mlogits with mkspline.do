// multinomial logit with annual panel
clear all
frame rename default panel 
use "C:\Users\H54106\Desktop\fha_claims\fhapanel.dta"

frame create details
frame change details 

use "C:\Users\H54106\Desktop\fha_claims\IHPdetails_wide.dta"
forvalues x = 1/11 {
	gen disaster`x'start = mdy(startmonth`x', startday`x', startyear`x')
}

frame change panel 
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

rename beginYear begAmortYear 
rename unempRate unemprate
//save "C:\Users\H54106\Desktop\fha_claims\fhapanel_mlogit.dta", replace

// run logit models 
mlogit outcome_num exposure1_12m exposure13_24 exposure25_36 frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1 & begAmortYear > 2009, vce(robust) 
estimates store lags3PostRecb

mlogit outcome_num   exposurelag_last3yrs frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1 & begAmortYear > 2009, vce(robust) 
estimates store lag1PostRecb

mlogit outcome_num exposure1_12m exposure13_24 exposure25_36 frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1,  vce(robust) 
estimates store lags3Fullb

mlogit outcome_num   exposurelag_last3yrs frontDTI fico giftFamilyPrc giftGovtPrc giftnonprofit2006 sellerPrc  mtmLTV unemprate ratePMMS py1   if  dead != 1 , vce(robust) 
estimates store lag1Fullb

/* export table using tabout 
esttab lags3Full lags3PostRec lag1Full lag1PostRec ///
	using "C:\Users\H54106\Desktop\fha_claims\result tables\FirstStageResultsCoeff.csv" , ///
	b(%7.2g) replace title ("Annual Panel First Stage Multinomial Results") ar2 nogap se star(* .1 ** .05 *** .01)	///
	order (exposure1_12m exposure13_24 exposure25_36 exposurelag_last3yrs) eform
	*/
	
	
// simulation 

//drop if begAmortYear < 2010

predict prob*, pr 

/* documentation from stata help predict with respect to running simulation over VA dataset
predict can make out-of-sample predictions even using other datasets.  In particular, you can

            . use ds1
             (fit a model)
            . use two              /* another dataset */
            . predict yhat, ...    /* fill in the predictions */
*/


// generate p-prime, the probablity of claim after netting out disaster exposure *************************************************************************
/* quarterly coefficicents for comparison
local prepay_expos1 = -.4513832
local prepay_expos2 = -.3578245
local prepay_expos3 = -.0296035

local default_expos1 = .3081848
local default_expos2 = .0541854
local default_expos3 = .1449354

// 3 year window coefficients 

local prepay_expos3yr = -.2291256
local default_expos3yr = .1433511

/*local  exposureCoeff1 =  .2048524
 local exposureCoeff2 = .278353 
local exposureCoeff3 = .1833019   
local exposureCoeff4 = .0498203  */
*/ 

// individual year lag coefficients 
local prepay_expos1 = -.1060053
local prepay_expos2 = .0424048
local prepay_expos3 = -.0313357

// 3 year window coefficients 
local default_expos1 = .1218467
local default_expos2 = .1566525
local default_expos3 = .2399171

// generate prob2_prime
gen odds_2 = prob2/ (1-prob2)
gen logOdds_2 = ln(odds_2)
* Log Odds delta = disaster exposure coefficient
gen oddsDelta_2 = (`prepay_expos1' * exposure1_12m + `prepay_expos1' * exposure13_24 + `prepay_expos1' * exposure25_36) /* + (`exposureCoeff2' * disaster2Exposure3y) + (`exposureCoeff3' * disaster3Exposure3y)  + (`exposureCoeff4' * disaster4Exposure3y)*/ 
	// ^^ generates the change in odds for each observation given disaster exposure 
gen logOddsPrime_2 = logOdds_2 - oddsDelta_2
	// gives new odds of claim taking out the effect of disaster exposure 
gen pPrime_2 = 1 / (1 + exp(-logOddsPrime_2))
	// converts from odds back to probablity

// generate prob3_prime
gen odds_3 = prob3/ (1-prob3)
gen logOdds_3 = ln(odds_3)
* Log Odds delta = disaster exposure coefficient
gen oddsDelta_3 = (`default_expos1' * exposure1_12m + `default_expos2' * exposure13_24 + `default_expos3' * exposure25_36) /* + (`exposureCoeff2' * disaster2Exposure3y) + (`exposureCoeff3' * disaster3Exposure3y)  + (`exposureCoeff4' * disaster4Exposure3y)*/ 
	// ^^ generates the change in odds for each observation given disaster exposure 
gen logOddsPrime_3 = logOdds_3 - oddsDelta_3
	// gives new odds of claim taking out the effect of disaster exposure 
gen pPrime_3 = 1 / (1 + exp(-logOddsPrime_3))
	
// begg gray transformations **************************************************************************************************
gen clmRt = (prob3 * (1-prob2))/(1-(prob3*prob2))
gen preRt = (prob2 * (1-prob3))/(1-(prob3*prob2))
gen clmRtPrm = (pPrime_3 * (1-pPrime_2))/(1-(pPrime_3*pPrime_2))
gen preRtPrm = (pPrime_2 * (1-pPrime_3))/(1-(pPrime_3*pPrime_2))

quietly {
/*
gen upbBegin = 0
gen pmt = 0
gen intrst = 0
gen claim = 0
gen prepay = 0
gen upbEnd = 0
gen upbBeginPrm = 0
gen pmtPrm = 0
gen intPrm = 0
gen claimPrm = 0
gen prepayPrm = 0
gen upbEndPrm = 0

*/
replace upbBegin = 0
replace pmt = 0
replace intrst = 0
replace claim = 0
replace prepay = 0
replace upbEnd = 0
replace upbBeginPrm = 0
replace pmtPrm = 0
replace intPrm = 0
replace claimPrm = 0
replace prepayPrm = 0
replace upbEndPrm = 0


// create calcuation of expected claims costs (in dollars) and related variables based on p3y as estimated probability of claim
//set initial vars

	replace upbBegin = origMortgAmt // set upb equal to original mortgage amount 
	replace pmt = upbBegin * ((r*((1+r)^n))/(((1+r)^n)-1)) // pmt amount on quarterly basis, amortization formula
	replace intrst = r * upbBegin  //  
	replace upbEnd = upbBegin + intrst - pmt //upb at end of policy year is upb begin minus payments plus interest
	}
	
quietly {

gen upbBegin = 0
gen pmt = 0
gen intrst = 0
gen claim = 0
gen prepay = 0
gen upbEnd = 0
gen upbBeginPrm = 0
gen pmtPrm = 0
gen intPrm = 0
gen claimPrm = 0
gen prepayPrm = 0
gen upbEndPrm = 0


replace upbBegin = 0
replace pmt = 0
replace intrst = 0
replace claim = 0
replace prepay = 0
replace upbEnd = 0
replace upbBeginPrm = 0
replace pmtPrm = 0
replace intPrm = 0
replace claimPrm = 0
replace prepayPrm = 0
replace upbEndPrm = 0


sort caseN policyYear 

// create calcuation of expected claims costs (in dollars) and related variables based on p3y as estimated probability of claim
//set initial vars

	replace upbBegin = origMortgAmt // set upb equal to original mortgage amount 
	replace pmt = upbBegin * ((r*((1+r)^n))/(((1+r)^n)-1)) // pmt amount on annual basis, amortization formula
	replace intrst = r * upbBegin  //  
	replace upbEnd = upbBegin + intrst - pmt //upb at end of policy year is upb begin minus payments plus interest
	}

// set vars for policy years
forvalues y=1/20 { //looping of first 20 years of mortgage 
	replace upbBegin = upbEnd[_n-1] if policyYear != 1 // upb at beginning of policy year 
	replace pmt = upbBegin * (r*((1+r)^(n-policyYear+1)))/(((1+r)^(n-policyYear+1)-1)) if policyYear == `y' 
	replace intrst = r * upbBegin  if policyYear == `y' // intereat paid on annual basis 
	replace claim = upbBegin * clmRt if policyYear == `y' // expected value of claim payments = probablity of claim x UPB
	replace prepay = upbBegin * preRt if policyYear == `y' // expected value of claim pre-payment = probablity of claim x UPB 
	replace upbEnd = max(upbBegin + intrst - pmt - claim - prepay,0)  if policyYear == `y'
	// if values for claim and prepay are equal to 0 unless those events occur, in which cae upb should be zero. when they occur, value of first argument less than 0
}
	

// generating claim costs and related variables for p-prime (claim amounts with nuetralized disaster exposure) 
// same loop as above, all comments apply here
//set initial vars
	replace upbBeginPrm = origMortgAmt 
	replace pmtPrm = upbBeginPrm * ((r*((1+r)^n))/(((1+r)^n)-1)) 
	replace intPrm= r * upbBeginPrm *.5 
	replace upbEndPrm = upbBeginPrm + intPrm - pmtPrm
	
	
// set vars for policy years 
		forvalues y=1/20 {
			replace upbBeginPrm = upbEndPrm[_n-1] if policyYear != 1
			replace pmtPrm = upbBeginPrm * (r*((1+r)^(n-policyYear+1)))/(((1+r)^(n-policyYear+1)-1)) if policyYear == `y'
			replace intPrm= r * upbBeginPrm  if policyYear == `y'
			replace claimPrm = upbBeginPrm * clmRtPrm if policyYear == `y'
			replace prepayPrm = upbBeginPrm * preRtPrm if policyYear == `y'
			replace upbEndPrm = max(upbBeginPrm + intPrm - pmtPrm - claimPrm - prepayPrm,0)  if policyYear == `y'
		}
// end to quietly command 

// filter out obs with missing variables 
gen filter3 = 0
replace filter3 = 1 if unemprate==. | upbBegin == .

// calculate difference in claim amounts between p (with disasters) and p-prime (no disasters)
 
gen dClm = claim - claimPrm
replace dClm = . if filter3==1
gen  dClmMM = dClm / 1000000 // convert claim costs to millions of dollars 
table begAmortYear policyYear, row col contents(sum dClmMM ) format(%9.2fc)	

// generate value of portfolio simulated claims to compare to historical claims
gen ClmMM = claim/1000000
table begAmortYear policyYear, row col contents(sum ClmMM ) format(%9.2fc)	
	
	
	
	
	
	
	
