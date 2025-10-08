/* do file for merging zip level IHPdetails_wide and FHA panel
result will be the fhapanel_base, which will contain 
	basic "any disaster" indicator 
	disaster n specific indicators 
	
	
these indicators can be used for sample descriptive stats by exposure
logit regression do files will call this file and use basic binaryies to 
create the relevant exposure intensity indicators specific to that specification 
these panels will can be frame linked to create future measures of exposure/entisinty without clogging main data set
*/

// sample save "C:\Users\H54106\Desktop\fha_claims\fhapanel.dta", replace

// ************************************************************************
clear all 

frame rename default panel 
use "C:\Users\H54106\Desktop\fha_claims\fhapanel.dta"

frame create details_wide
frame change details_wide 

use "C:\Users\H54106\Desktop\fha_claims\IHPdetails_wide.dta"
forvalues x = 1/11 {
	gen disaster`x'start = mdy(startmonth`x', startday`x', startyear`x')
}

frame change panel 
frlink m:1 zip5, frame(details_wide) generate(_link_details)

// unlinked observations are in zip5 that do not have any IHP eligible disaster decs

gen disasterExposure3y = 0
forvalues x = 1/11 {
	replace disasterExposure3y = 1 if ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
		sif_amort <= frval(_link_details, begindisaster`x') & /// amort began at least the same year-month as the disaster
		obs_sif - frval(_link_details, begindisaster`x') >= 0 & /// observation is between 0 and 3 years of disaster declaration
		obs_sif - frval(_link_details, begindisaster`x')  <= 12  
	}
		
//create most recent disaster exposure
forvalues x = 1/11 {
	gen disaster`x'Exposure3y = 0
	replace disaster`x'Exposure3y = 1 if ///
		frval(_link_details, ihprogramdeclared`x') == 1 & /// zip5 is eligible for household assistance
		sif_amort <= frval(_link_details, begindisaster`x') & /// amort began at least the same year-month as the disaster
		obs_sif - frval(_link_details, begindisaster`x') >= 0 & /// observation is between 0 and 3 years of disaster declaration
		obs_sif - frval(_link_details, begindisaster`x')  <= 12  
	gen disaster`x'start = frval(_link_details, disaster`x'start)
	}
				
save "C:\Users\H54106\Desktop\fha_claims\fhapanel_base.dta", replace


/* notes for later

		frval(link_details, valid_regs`x' >= 20) /// 20 or more to indicate severely damaged zip code 
