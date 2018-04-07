capture cd "/Users/roizultan/Documents/Academic/Group reciprocity/Data"
capture cd "C:\Users\Itay's Laptop\Desktop\Thesis"
capture cd "C:\Users\Itay's Laptop\Desktop\Thesis\"
import excel "Data_baseline.xlsx", sheet("Sheet1") firstrow clear

drop q*
drop self* other*
drop dictator*
renvars, lower
egen session = group(sessionid)
egen groupid = group(session group)
egen sid = group(session subject)

egen correct = rowmean(q*)
replace correct = correct / 2
egen correct_group = rowmean(groupcorr*)

drop sessionid treatment subjects participate input* chosen* item* random* raven* q* groupcorrect* all_* ///
	time* checker* counter fake* option leavestage k avg* j trustfeed* ew

rename svo_angle ew
rename svo_type ex
	
bysort sid: egen svo_angle = max(ew)
bysort sid: egen svo_type = max(ex)
bysort sid: egen sent = max(trustgame)
bysort sid: egen returned = max(trustgameresponder)

drop ew ex trustgame*

******************************************

rename allocation give1
rename allocationdummy give2
foreach i of numlist 1 2 {
  rename other`i'team otherteam`i'
  rename other`i'num othernum`i'
}

gen kindness = returned / sent
gen discrimination = abs(give1 - give2)

bysort session pair period: egen partner_team = sum(group)
replace partner_team = partner_team - group
bysort session pair period: egen partner_id = sum(ingroup)
replace partner_id = partner_id - ingroup

gen direct1 = otherteam1 == partner_team & othernum1 == partner_id
gen direct2 = otherteam2 == partner_team & othernum2 == partner_id
gen otherdirect1 = otherteam2 == partner_team & othernum2 == partner_id
gen otherdirect2 = otherteam1 == partner_team & othernum1 == partner_id
gen greciprocity1 = otherteam1 == partner_team & othernum1 != partner_id
gen greciprocity2 = otherteam2 == partner_team & othernum2 != partner_id
gen othergreciprocity1 = otherteam2 == partner_team & othernum2 != partner_id
gen othergreciprocity2 = otherteam1 == partner_team & othernum1 != partner_id
gen in_group1 = otherteam1 == group
gen in_group2 = otherteam2 == group
gen otherin_group1 = otherteam2 == group
gen otherin_group2 = otherteam1 == group

******************************************

*** DISCRIMINATION ***

gen conddirect = direct1 + direct2
gen condgrec = greciprocity1 + greciprocity2
gen condin = in_group1 + in_group2

egen condition = group(conddirect condgrec condin) 

xtmixed discrimination roletrust##condition || sid:
margins, over(roletrust condition) pwcompare(group)

******************************************

*** In-group favouritism and trust ***

gen inbiasround = in_group1 * (give1 - give2) + in_group2 * (give2 - give1) if condin
bysort sid: egen inbias = mean(inbiasround)
replace inbias = . if period > 1
bysort groupid: egen groupbias = mean(inbias) if period == 1
bysort groupid: egen groupkindness = mean(kindness) if roletrust == 2
bysort groupid: egen groupsend = mean(sent) if roletrust == 1

*** The only (weakly) significant relation is sending at the group level on mean ingroup favouritism (p=0.099).

*** History of meeting the trust partner ***

bysort sid: gen history = sum(conddirect)
recode history (2=1)
*** When we interact history with group reciprocity none of the relevant coefficients is significant

******************************************

reshape long give otherteam othernum direct otherdirect greciprocity othergreciprocity in_group otherin_group, i(sid period) j(recipient)



******************************************

//added ingroup to xtmixed and to over() in 2 places
reg give c.kindness##direct c.kindness##grec c.kindness##in_group c.kindness##otherdirect ///
c.kindness##othergrec  c.kindness##otherin if roletrust==1, cluster(sid)
margins, over(direct grec in_group) at(kindness=(0(0.1)1.5) otherdir=0 othergrec=0 otherin=0)
marginsplot, recast(line) recastci(rline) ciopts(lwidth(vthin)) name(sender, replace) ///
	legend(region(color(none)) row(1) order(1 "Neutral" 2 "In-group" 4 "Direct" 3 "Group")) ///
	title("") ytitle("") xtitle("Share sent back")
graph export Trustors.pdf, replace
margins, dydx(kindness) over(direct grec in_group) at(otherdir=0 othergrec=0 otherin=0) 

//added ingroup to xtmixed and to over() in 2 places
reg give c.sent##direct c.sent##grec c.sent##in_group c.sent##otherdirect ///
	c.sent##othergrec  c.sent##otherin if roletrust==2, cluster(sid)
margins, over(direct grec in_group) at(sent=(0(30)150) otherdir=0 othergrec=0 otherin=0)
marginsplot, recast(line) recastci(rline) ciopts(lwidth(vthin)) name(receiver, replace) ///
	legend(region(color(none)) row(1) order(1 "Neutral" 2 "In-group" 4 "Direct" 3 "Group")) ///
	title("") ytitle("") xtitle("Received")
graph export Trustees.pdf, replace
margins, dydx(sent) over(direct grec in_group) at(otherdir=0 othergrec=0 otherin=0) 

*** raw ***

egen catkind = cut(kindness), at(0 1 3.1) //at(0(0.5)1.5)
egen category = group(in_group direct grec)
graph bar give if roletrust == 1, over(catkind) over(category, relabel(1 "Neutral" 2 "Group" 3 "Direct" 4 "Ingroup")) ///
	over(roletrust, relabel(1 "Recipient type")) ylabel(,nogrid) ytitle("Mean allocation") yline(35, lpattern(dash)) ///
	legend(region(color(none)) order(1 "Returned less than sent" 2 "Returned at least as much as sent")) name(means, replace)
graph export means.pdf, replace

 
******************************************

*** Group affiliation predicts kindness! ***

anova sent groupid if period == 1 & roletrust == 1 & recipient == 1
anova kindness groupid if period == 1 & roletrust == 1 & recipient == 1

