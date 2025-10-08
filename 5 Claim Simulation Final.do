// Do file for Calculating p(claim) with neutralized disaster claim effect 



// merge prepay rates to panel dataset if necessary  *****************************************
//step 1: generate merge var in fhapanel 
gen beginyearpy2 = beginYear + (policyYear / 100)
// step 2: load and link prepay frame
frame create prepay
frame change prepay 
use "C:\Users\H54106\Desktop\fha_claims\prepay_ratesb.dta"
frame change default 
frlink m:1 beginyearpy2, frame(prepay) generate(_join)
// step 3: generate new var for prepayRate 
gen prepayRate = frval(_join, prepayrate)

//run code for logistic regression if necessary ***********************************************
// logistic defYearBool frontDTI fico giftGovtPrc giftFamilyPrc sellerPrc mtmLTV scndFincPrc unempRate ratePMMS py1 disasterExposureSevere if filter3 != 1, vce(robust) coef


// run post estimation commands*****************************************************
predict p3y

/* documentation from stata help predict with respect to running simulation over VA dataset
predict can make out-of-sample predictions even using other datasets.  In particular, you can

            . use ds1
             (fit a model)
            . use two              /* another dataset */
            . predict yhat, ...    /* fill in the predictions */
*/

// generate p-prime, the probablity of claim after netting out disaster exposure *************************************************************************

local exposureCoeff1 =  .2048524
/*local exposureCoeff2 = .278353 
local exposureCoeff3 = .1833019   
local exposureCoeff4 = .0498203  */

gen odds = p3y/ (1-p3y)
gen logOdds = ln(odds)
* Log Odds delta = disaster exposure coefficient
gen oddsDelta = (`exposureCoeff1' * disaster1Exposure3y) /* + (`exposureCoeff2' * disaster2Exposure3y) + (`exposureCoeff3' * disaster3Exposure3y)  + (`exposureCoeff4' * disaster4Exposure3y)*/ 
	// ^^ generates the change in odds for each observation given disaster exposure 
gen logOddsPrime = logOdds - oddsDelta
	// gives new odds of claim taking out the effect of disaster exposure 
gen pPrime = 1 / (1 + exp(-logOddsPrime))
	// converts from odds back to probablity
// could this have been done just by subtracting out oddsDelta from odds? eyeballing a few observations in the data suggests it might be 	
	
// begg gray transformations **************************************************************************************************
gen clmRt = (p3y * (1-prepayRate))/(1-(p3y*prepayRate))
gen preRt = (prepayRate * (1-p3y))/(1-(p3y*prepayRate))
gen clmRtPrm = (pPrime * (1-prepayRate))/(1-(pPrime*prepayRate))
gen preRtPrm = (prepayRate * (1-pPrime))/(1-(pPrime*prepayRate))

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

/*
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
*/

// create calcuation of expected claims costs (in dollars) and related variables based on p3y as estimated probability of claim
//set initial vars
forvalues i=1/2 {			// looping over i does not do anything here
	noisily disp as text "Loop "`i'
	replace upbBegin = origMortgAmt // set upb equal to original mortgage amount 
	replace pmt = upbBegin * ((r*((1+r)^n))/(((1+r)^n)-1)) // pmt amount on annual basis, amortization formula
	replace intrst = r * upbBegin *.5 // why multiply by .5 here?
	replace upbEnd = upbBegin + intrst - pmt //upb at end of policy year is upb begin minus payments plus interest
	}
	
	// set vars for policy years
	forvalues i=1/2 { // looping over i does not do anything here
		forvalues y=1/20 { //looping of first 20 years of mortgage 
			replace upbBegin = upbEnd[_n-1] if policyYear != 1 // upb at beginning of policy year 
			replace pmt = upbBegin * (r*((1+r)^(n-policyYear+1)))/(((1+r)^(n-policyYear+1)-1)) if policyYear == `y' 
			replace intrst = r * upbBegin  if policyYear == `y' // intereat paid on annual basis 
			replace claim = upbBegin * clmRt if policyYear == `y' // expected value of claim payments = probablity of claim x UPB
			replace prepay = upbBegin * preRt if policyYear == `y' // expected value of claim pre-payment = probablity of claim x UPB 
			replace upbEnd = max(upbBegin + intrst - pmt - claim - prepay,0)  if policyYear == `y'
			// if values for claim and prepay are equal to 0 unless those events occur, in which cae upb should be zero. when they occur, value of first argument less than 0
		}
	}

// generating claim costs and related variables for p-prime (claim amounts with nuetralized disaster exposure) 
// same loop as above, all comments apply here
//set initial vars
forvalues i=1/2 { 
	noisily disp as text "Prime Loop "`i'
	replace upbBeginPrm = origMortgAmt 
	replace pmtPrm = upbBeginPrm * ((r*((1+r)^n))/(((1+r)^n)-1)) 
	replace intPrm= r * upbBeginPrm *.5 
	replace upbEndPrm = upbBeginPrm + intPrm - pmtPrm
	}
	
// set vars for policy years 
	forvalues i=1/2 {
		forvalues y=1/20 {
			replace upbBeginPrm = upbEndPrm[_n-1] if policyYear != 1
			replace pmtPrm = upbBeginPrm * (r*((1+r)^(n-policyYear+1)))/(((1+r)^(n-policyYear+1)-1)) if policyYear == `y'
			replace intPrm= r * upbBeginPrm  if policyYear == `y'
			replace claimPrm = upbBeginPrm * clmRtPrm if policyYear == `y'
			replace prepayPrm = upbBeginPrm * preRtPrm if policyYear == `y'
			replace upbEndPrm = max(upbBeginPrm + intPrm - pmtPrm - claimPrm - prepayPrm,0)  if policyYear == `y'
		}
	}
} // end to quietly command 

// filter out obs with missing variables 
gen filter3 = 0
replace filter3 = 1 if unempRate==. | upbBegin == .

// calculate difference in claim amounts between p (with disasters) and p-prime (no disasters)
gen dClm = claim - claimPrm
replace dClm = . if filter3==1
gen dClmMM = dClm / 1000000 // convert claim costs to millions of dollars 
table beginYear policyYear, row col contents(sum dClmMM ) format(%9.0fc)






















	