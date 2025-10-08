// do file for cleaning disaster decs, disaster details and FHA panel for merge

/* three steps
step 1: prepare fha panel for merge. output --> fhapanel.dta
step 2: download and clean disaster declaration data for merge with disaster details -->  ihpdecs_long
step 3: download and clean disaster details data for merge with decs_long, convert to wide --> ihpdetails_wide
*/


////////////////////////////////////////////////////////////////////////////////////////////////////////////
// step 1
///////////////////////////////////////////////////////////////////////////////////////////////////////////
// prepare fha panel for merge
clear all
frame rename default fha_panel
use "\\sasadhoc.hud.gov\E Drive\h51665\FHA_Disaster_Claims\Stata July 2022\panel_Aug29.dta"
// sample use  "\\sasadhoc.hud.gov\E Drive\h51665\FHA_Disaster_Claims\Stata July 2022\panelAug29_sample.dta"

// create fips5 and SIF identifiers

gen begAmortMonth = month(begAmortDate)
gen sif_amort = (beginYear - 1960)*12 + begAmortMonth
generate obs_year = beginYear + policyYear - 1
gen obs_sif = (obs_year - 1960)*12 + begAmortMonth

bysort caseN: gen sif_term = max(obs_sif)
// note for sif_term - more accurately described as last month loan active during sample, includes active loans at end of sample

merge m:1 caseN using "\\sasadhoc.hud.gov\E Drive\h51665\FHA_Disaster_Claims\Stata July 2022\loans_zip.dta" 
/*  Result                           # of obs.
    -----------------------------------------
    not matched                         5,661
        from master                         0  (_merge==1)
        from using                      5,661  (_merge==2)

    matched                         4,453,214  (_merge==3)
    ----------------------------------------- */
keep if _merge == 3
drop  _merge
rename zip zip5 
save "C:\Users\H54106\Desktop\fha_claims\fhapanel.dta", replace

//use "S:\fha_PDMDA\county_disasterdecs.dta"

frame create decs 
frame change decs

import delimited "https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries.csv", encoding(windows-1252) 
count 
 bysort declarationtype: count
/*
--------------------------------------------------------------------------------------------------------------
-> declarationtype = DR
  42,635
--------------------------------------------------------------------------------------------------------------
-> declarationtype = EM
  18,511
--------------------------------------------------------------------------------------------------------------
-> declarationtype = FM
  1,848 */

*create disaster start date variables
split incidentbegindate, parse(T)
split incidentbegindate1, parse(-)
rename incidentbegindate11 startyear
rename incidentbegindate12 startmonth
rename incidentbegindate13 startday
destring startyear startmonth startday, replace
gen begindisaster = ym(startyear, startmonth)

*create disaster end date variables
split incidentenddate, parse(T)
split incidentenddate1, parse(-)
rename incidentenddate11 endyear
rename incidentenddate12 endmonth
rename incidentenddate13 endday
destring endyear endmonth endday, replace
gen enddisaster = ym(endyear, endmonth)

// keep only DR decs
keep if declarationtype == "DR"

// keep only those in time window 
drop if startyear < 2004 | startyear > 2019

//drop tribal and territories
drop if fipsstate == 72 | fipsstate == 78 | fipsstate == 60 | fipsstate == 68 | fipsstate == 66 | fipsstate == 64 | fipsstate == 60


// comment out to keep all DR decs, leave open for only IHP
drop if ihp == 0
/* keep if declarationtype == "DR"
(20,359 observations deleted)

. 
. // keep only those in time window 
. drop if startyear < 2004 | startyear > 2019
(25,776 observations deleted)

. 
. //drop tribal and territories
. drop if fipsstate == 72 | fipsstate == 78 | fipsstate == 60 | fipsstate == 68 | fipsstate == 66 | fipsstate 
> == 64 | fipsstate == 60
(416 observations deleted)

. 
. 
. // comment out to keep all DR decs, leave open for only IHP
. drop if ihp == 0
(11,683 observations deleted)
*/

//want to convert declaration data from long to wide so fips is unique identifer for merge with FHA data
//to do this need to create disaster event counter for each fips county

// check to make sure no disaster decs show up multiple times for same place
duplicates tag fipsstate fipscounty  disasternumber, gen(duplicate_drdecs)
count if duplicate_drdecs == 0
  *33,793
count if duplicate_drdecs == 1
  *28
count if duplicate_drdecs == 2
  *3
count if duplicate_drdecs == 3
  *4
count if duplicate_drdecs == 4
  *0
count if duplicate_drdecs == 5
  *6

// 35 counties with duplicates
//some counties have multiple place codes, so DRs show up in those counties
duplicates tag place disasternumber, gen(duplicate_placedecs)
count if duplicate_placedecs > 0
*no placecodes with duplicate dr decs

bysort fipsst fipsc disasternumber: gen dup_counter = _n 

coun

*identify counties with multiple DR declarations and ihp 
duplicates tag fipsstate fipscounty, gen(multiple_disasters)
count if multiple == 0
count if multiple == 1
count if multiple == 2
count if multiple == 3
count if multiple == 4
count if multiple == 5
count if multiple > 5
 /* 463

. count if multiple > 10
  0

. count if multiple == 6
  196

. count if multiple == 7
  164

. count if multiple == 8
  63

. count if multiple == 9
  40

. count if multiple == 10
  0
*/
bysort disasternumber: gen affected_counter = _n
bysort incidenttype: count if affected_counter == 1 
/*
-> incidenttype = Earthquake
  4
--------------------------------------------------------------------------------------------------------------
-> incidenttype = Fire
  16
--------------------------------------------------------------------------------------------------------------
-> incidenttype = Flood
  40
--------------------------------------------------------------------------------------------------------------
-> incidenttype = Hurricane
  56
--------------------------------------------------------------------------------------------------------------
-> incidenttype = Mud/Landslide
  1
--------------------------------------------------------------------------------------------------------------
-> incidenttype = Severe Storm(s)
  196
--------------------------------------------------------------------------------------------------------------
-> incidenttype = Tornado
  11
--------------------------------------------------------------------------------------------------------------
-> incidenttype = Typhoon
  4
--------------------------------------------------------------------------------------------------------------
-> incidenttype = Volcano
  1

volcano and earthquake not climate related
typhoons only affect territories, not continental States */

gen fips5 = fipss*1000 + fipsc 
bysort fips5: gen disastercount = _n

bysort fips5: egen max_exp = max(disastercount)

forvalues x = 1/10 {
	count if max_exp == `x' & disastercount == 1
	}
/*   number of counties with n = 1/10 disasters eligible for IHP
   771
  528
  373
  176
  109
  44
  24
  12
  3
  1

	*/
rename designatedarea county 

encode incidenttype, gen(disaster_type)

save "C:\Users\H54106\Desktop\fha_claims\IHPdecs_long.dta", replace

 
////////////////////////////////////////////////////////////////////////////////////////////////////////////
// step 2 create details long
///////////////////////////////////////////////////////////////////////////////////////////////////////////
frame create details
frame change details

clear
import delimited "https://www.fema.gov/api/open/v2/HousingAssistanceOwners.csv", encoding(windows-1252)


merge m:1 disasternumber county using "C:\Users\H54106\Desktop\fha_claims\IHPdecs_long.dta"

/*   
    Result                           # of obs.
    -----------------------------------------
    not matched                        43,507
        from master                    43,448  (_merge==1)
        from using                         59  (_merge==2)

    matched                            80,361  (_merge==3)
    -----------------------------------------

*/

keep if _merge == 3
drop if state == "MP"
//check for duplicate zips within disaster declarations
duplicates tag state zipcode  disasternumber, gen(duplicate_zips)
sort duplicate_zips zipcode

/* duplicate analysis shows that zip codes cross counties as well as cities, so each combo has its own entry
 need to collapse by zipcode 
 also disaster aid data is down to the state-county-city-zip 
 some zip codes span multiple city designations, while some error in application matching city to zip
 collapse everything into common zip code
 average inspected damage is number of valid inspected damage / valid registrations, 
	sum these and recalculate average damage after collapse 
*/

collapse ///
	(min) fips5 (mean) startyear (mean) startmonth (mean) startday (mean) begindisaster (mean) disaster_type ///
	(mean) ihprogramdeclared (mean) iaprogramdeclared (mean) paprogramdeclared (mean) hmprogramdeclared ///
	(sum) valid_regs=validregistrations ///
	(sum) num_inspected=totalinspected ///
	(sum) total_damage=totaldamage ///
	(sum) noDamage=nofemainspecteddamage /// 
	(sum) damage_10k=femainspecteddamagebetween1and10 /// 
	(sum) damage_10k_20k=femainspecteddamagebetween10001a /// 
	(sum) damage_20k_30k=femainspecteddamagebetween20001a ///
	(sum) damage_30k=femainspecteddamagegreaterthan30 ///
	(sum) num_approved=approvedforfemaassistance ///
	(sum) ihp_amt=totalapprovedihpamount ///
	(sum) repair_amt=repairreplaceamount ///
	(sum) rental_amt=rentalamount ///
	(sum) otherneeds_amt=otherneedsamount /// 
	(sum) approved_10k=approvedbetween1and10000 /// 
	(sum) approved_10k_25l=approvedbetween10001and25000 /// 
	(sum) approved_25k=approvedbetween25001andmax, ///
	by(state zipcode disasternumber)
	
// recheck duplicate zips
duplicates tag state zipcode  disasternumber, gen(duplicate_zips)
sort duplicate_zips zipcode

drop in 1/20

rename zipcode zip5

bysort zip5 : gen disastercount = _n

bysort zip5: egen max_exp = max(disastercount)

forvalues x = 1/11 {
	count if max_exp == `x' & disastercount == 1
	}


save "C:\Users\H54106\Desktop\fha_claims\IHPdetails_long_incomplete.dta", replace

// remerge incident_type and begin/end dates
clear
use "C:\Users\H54106\Desktop\fha_claims\IHPdecs_long.dta", replace

duplicates drop disasternumber, force
keep disasternumber incidenttype incidentbegindate incidentenddate
save "C:\Users\H54106\Desktop\fha_claims\incident_type.dta", replace
clear 
use  "C:\Users\H54106\Desktop\fha_claims\IHPdetails_long_incomplete.dta"

merge m:1 disasternumber using "C:\Users\H54106\Desktop\fha_claims\incident_type.dta"
keep if _merge == 3
save "C:\Users\H54106\Desktop\fha_claims\IHPdetails_long.dta", replace


// convert to wide for frame link/merge with fha panel
keep 	disasternumber incidenttype incidentbegindate incidentenddate ///
	startyear startmonth startday begindisaster ///
	ihprogramdeclared iaprogramdeclared paprogramdeclared hmprogramdeclared ///
	valid_regs num_inspected total_damage ///
	noDamage damage_10k damage_10k_20k damage_20k_30k damage_30k ///
	num_approved ihp_amt repair_amt rental_amt otherneeds_amt ///
	approved_10k approved_10k_25l approved_25k duplicate_zips zip5 disastercount



// frame copy details sif_zip

reshape wide ///
	disasternumber incidenttype incidentbegindate incidentenddate ///
	startyear startmonth startday begindisaster ///
	ihprogramdeclared iaprogramdeclared paprogramdeclared hmprogramdeclared ///
	valid_regs num_inspected total_damage ///
	noDamage damage_10k damage_10k_20k damage_20k_30k damage_30k ///
	num_approved ihp_amt repair_amt rental_amt otherneeds_amt ///
	approved_10k approved_10k_25l approved_25k duplicate_zips , /// 
	i(zip5) j(disastercount)


	// check visually how the sorted panel looks, should not be gaps in n disasters per zip
sort ihprogramdeclared1 ihprogramdeclared2 ihprogramdeclared3 ihprogramdeclared4 ihprogramdeclared5 ihprogramdeclared6 ihprogramdeclared7 ihprogramdeclared8 ihprogramdeclared9 ihprogramdeclared10 ihprogramdeclared11

//resort for future merge 
sort zip5 
save "C:\Users\H54106\Desktop\fha_claims\IHPdetails_wide.dta", replace

/*
// create details wide by disaster and date
frame change sif_zip
drop disastercount  

bysort zip5 begindisaster: gen disastercount = _n

bysort zip5: egen max_exp = max(disastercount)

reshape wide ///
	disasternumber incidenttype startyear startmonth startday  ///
	ihprogramdeclared iaprogramdeclared paprogramdeclared hmprogramdeclared ///
	valid_regs num_inspected total_damage ///
	noDamage damage_10k damage_10k_20k damage_20k_30k damage_30k ///
	num_approved ihp_amt repair_amt rental_amt otherneeds_amt ///
	approved_10k approved_10k_25l approved_25k duplicate_zips , /// 
	i(zip5 begindisaster) j(disastercount )

save "C:\Users\H54106\Desktop\fha_claims\IHPdetails_sif_zip.dta", replace
*/
