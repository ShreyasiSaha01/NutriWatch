*** Lower case
ren *, lower

*** Shorten file
keep caseid v001 v021 v006 v007 v009 v010 v011 v012 v013 v024 v025 sdist ssmod sweight ///
v106 v113 v116 v130 s116 v136 v190  v191 v212 v714 v731 v732 v218 s259 s308c s309 v212 ///
v457 hw57 m15 m4 m5 bord b4 b5 b8 hw70 hw71 hw72 hw73 m18 m19 m19a

save "E:\Admin\Our Files\Project20232024\ChildMarriage202324\kid20012023.dta"
drop if b5==0

*** Age at first marriage
gen afm=int((s308c - v011 ) / 12) if s308c<9997
replace afm=s309 if s308c==9997 | s308c ==9998
label variable afm "Age at fist marriage"
recode afm (0/17=1 "Below 18 years")(18/25=2 "18-25 years")(26/35=3 "26-35 years")(36/49=4 "36-49 years"), gen(rafm)
tab1 afm rafm

*** Anthropometric outcome of children
***New WHO***
*** Stunting
gen haz=.
replace haz=hw70 if hw70<9996

gen stunt=.
replace stunt=1 if haz<-200
replace stunt=0 if haz>=-200

la define stunt 0 "Normal" 1 "Stunting"
la variable stunt "Whether child stunted"
la value stunt stunt

**Underweight***
gen waz=.
replace waz=hw71 if hw71<9996

gen uweight=.
replace uweight=1 if waz<-200
replace uweight=0 if waz>=-200

la define uweight 0 "Normal" 1 "Underweight"
la variable uweight  "Whether child underweight"
la value uweight  uweight

***Wasting***
gen whz=.
replace whz=hw72 if hw72<9996

gen waste=.
replace waste=1 if whz<-200
replace waste=0 if whz>=-200

la define waste 0 "Normal" 1 "Wasted"
la variable waste  "Whether child is wasted"
la value waste waste

tab1 waste stunt uweight

*** Composite Index of anthropometric failure
gen ciaf=0
replace ciaf = 1 if stunt==1 & uweight==1 & waste==1
label variable ciaf "Composite Index of anthropometric failure"
label define ciaf 0 "No" 1 "Yes"
label value ciaf ciaf
tab ciaf

*** Breastfeeding
recode m4 (94 98= 0 "Never breastfeed")(else = 1 "Breastfed"), gen(bfed)
label variable bfed "Whether breastfed"

*** Teen pregnancy
gen agepreg = v012 - (b8+1)
label variable agepreg "Age at time of delivery"
tab agepreg
recode agepreg (.=.)(18/100=0 "No teen pregnancy")(0/17 = 1 "Teen pregnancy"), gen(teen)
label variable teen "Teen pregnancy"
tab teen
gen teen1 =teen+1
replace teen1 = 0 if afm>=18
label variable teen1 "Teen marriage & pregnancy"
label define teen1 0 "Adult marriage" 1 "Teen married" 2 "Teen pregnancy", modify
label value teen1 teen1

*** Weight
gen wt=sweight/10000
gen iwt=int(wt)
label variable iwt "Frequency weights"

*** Anemia
replace hw57=.a if hw57==9
recode hw57 (1/3=1 "Anemic")(4=0 "Not anemic"), gen(bhw57)
tab bhw57

*** Missing AFM
replace afm = . if afm==98
replace rafm = . if rafm==98

*** Aridity index
gen daridity= aridity2020- aridity2015
label variable daridity "Increase in aridity: 2015-2020"

gen dtemp= temp2020- temp2010
label variable dtemp "Increase in mean temperature: 2010-2020"

*** Socio-religious group
gen src=6
replace src=1 if s116==1 & v130==1
replace src=2 if s116==2 & v130==1
replace src=3 if s116==3 & v130==1
replace src=4 if s116==4 & v130==1
replace src=5 if v130==2
label variable src "Socio-religious community"
label define src 1 "HSC" 2 "HST" 3 "HOBC" 4 "HGen" 5 "Muslim" 6 "Others"
label value src src
tab src

*** Improved sanitation and drinking water
drop if v116==97
recode v116 (11 12 13  21 22 41=1 "Improved sanitation") ///
(14 15 23 31 44 96 = 0 "Unimproved sanitation") (97=.), gen(impsan)
recode v113 (11 12 13 14 21 31 41 51 61 62 71 92=1 "Improved water") ///
(32 42 43 72 96 = 0 "Unimproved water") (97=.), gen(impwater)
tab1 impsan impwater

label define teen1 0 "Adult marriage" 1 "Adolescent marriage" 2 "Adolescent pregnancy", modify
recode teen1 (0=0 "Adult marriage")(1/2=1 "Child marriage"), gen(cmarriage)

*** save file as marriage280124
drop teen teen1
tab v013 cmarriage, nofreq col
recode v013 (15/19 =1 "15-19 years") (20/24 =2 "20-24 years")(25/29 =3 "25-29 years") (30/34 =4 "30-34 years") (35/49 =5 "35-49 years"), gen(rv013)
move rv013 v013
save, replace
clear

*** save file as pregnancy280124
drop if cmarriage==0
drop cmarriage teen1
label define teen 0 "Adult pregnancy" 1 "Adolescent pregnancy", modify
ren teen preg
tab preg
save, replace
clear

*** Leave one out instrument
drop if afm==.
save, replace
gen temp=1 
egen safm=sum(afm), by(v021)
egen nafm=sum(temp), by(v021)
gen pafm= (safm - afm)/(nafm - 1)
drop temp
su pafm

recode v731 (0 1=0 "Curently not working")(2 3 =1 "Currently working"), gen(rv731)
label variable rv731 "Current work status"
gen lv191 = log(v190)

recode v457 (4=0 "Not anemic") (else=1 "Anemic"), gen(rbv457)
label variable rbv457 "Recoded binary Anemia level"

*** Recode aridity2015 varibale
recode aridity2015 (2.89536/50 =1 "Arid")(else =0 "Humid") , gen(arid2015)
replace arid2015=. if aridity2015==.
label variable arid2015 "Aridity"
tab arid2015,miss

*** Child marriage vs adult marriage
*** Model: Probit
probit ciaf cmarriage i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region [fw=iwt]

probit stunt cmarriage i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region [fw=iwt]

probit waste cmarriage i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region [fw=iwt]

probit uweight cmarriage i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region [fw=iwt]

*** Model: IV probit
ivprobit ciaf i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

ivprobit stunt i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

ivprobit waste i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

ivprobit uweight i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2020 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

* Wald test for exogeniety
If the Wald test is insignificant you cannot reject the null hypothesis of no endogeniety. 
Therefore, you are better off using a single probit equation.No need for instrumental variables
according to this test statistics because there is no endogeneity.


****
*****
*** Heterogeniety Analysis

**Regional Variations

bysort region: ivprobit ciaf i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater (cmarriage=pafm) [fw=iwt], first

bysort region: ivprobit stunt i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater (cmarriage=pafm) [fw=iwt], first

bysort region: ivprobit waste i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater (cmarriage=pafm) [fw=iwt], first

bysort region: ivprobit uweight i.v025 i.v106 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater (cmarriage=pafm) [fw=iwt], first

**Socio religious communities
bysort src: ivprobit ciaf i.v025 i.v106 lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

bysort src: ivprobit stunt i.v025 i.v106 lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first
 

bysort src: ivprobit waste i.v025 i.v106 lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

bysort src: ivprobit uweight i.v025 i.v106 lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

**Wealth
bysort v190: ivprobit ciaf i.v025 i.v106 ib4.src v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

bysort v190: ivprobit stunt i.v025 i.v106 ib4.src v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

bysort v190: ivprobit waste i.v025 i.v106 ib4.src  v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

bysort v190: ivprobit uweight i.v025 i.v106 ib4.src  v212 i.rbv457 b4 bfed daridity ///
irrigation temp2020 nightlight i.impwater i.impsan i.region (cmarriage= pafm) [fw=iwt], first


**Education
bysort v106: ivprobit ciaf i.v025 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

bysort v106: ivprobit stunt i.v025 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

bysort v106: ivprobit waste i.v025 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (cmarriage=pafm) [fw=iwt], first

bysort v106: ivprobit uweight i.v025 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2020 nightlight i.impwater i.impsan i.region (cmarriage= pafm) [fw=iwt], first


*** Endogenous treatment effects model
egen lafm=sum(afm), by(v021)
eteffects (ciaf i.v025 i.v106 i.rv731 ib4.src lv191 v212 v457 b4 bfed bhw57 daridity irrigation temp2015 nightlight i.impsan i.impwater, probit)(cmarriage i.v025 ib4.src i.v190  nightlight  lafm) [fw=iwt]
eteffects (stunt i.v025 i.v106 i.rv731 ib4.src lv191 v212 v457 b4 bfed bhw57 daridity irrigation temp2015 nightlight i.impsan i.impwater, probit)(cmarriage i.v025 ib4.src i.v190  nightlight  lafm) [fw=iwt]
eteffects (waste i.v025 i.v106 i.rv731 ib4.src lv191 v212 v457 b4 bfed bhw57 daridity irrigation temp2015 nightlight i.impsan i.impwater, probit)(cmarriage i.v025 ib4.src i.v190  nightlight  lafm) [fw=iwt]
eteffects (uweight i.v025 i.v106 i.rv731 ib4.src lv191 v212 v457 b4 bfed bhw57 daridity irrigation temp2015 nightlight i.impsan i.impwater, probit)(cmarriage i.v025 ib4.src i.v190  nightlight  lafm) [fw=iwt]

*** Not necessary
*** Model: Endogenous probit
eprobit ciaf i.v106 ib4.src v191 v212 v457 b4 bfed darid irrigation temp2015 nightlight [fw=iwt], endog(cmarriage=s259)  
*** Within child marriage
recode teen1 (0=.)(1=0 "Adult pregnancy")(2=1 "Adolescent pregnancy"), recode(teen2)
*** Model: Endogenous probit
eprobit ciaf i.v106 ib4.src v191 v212 v457 b4 bfed daridity irrigation temp2015 nightlight[fw=iwt], endog(teen2=pafm) 




****************************************************************************
***Pregnancy

recode v731 (0 1=0 "Curently not working")(2 3 =1 "Currently working"), gen(rv731)
label variable rv731 "Current work status"
gen lv191 = log(v190)

recode v457 (4=0 "Not anemic") (else=1 "Anemic"), gen(rbv457)
label variable rbv457 "Recoded binary Anemia level"

*** Recode aridity2015 varibale
recode aridity2015 (2.89536/50 =1 "Arid")(else =0 "Humid") , gen(arid2015)
replace arid2015=. if aridity2015==.
label variable arid2015 "Aridity"
tab arid2015,miss

*** Child marriage child pregnancy vs child marriage adolescent pregnancy
*** Model: Probit
probit ciaf preg i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region [fw=iwt]

probit stunt preg i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region [fw=iwt]

probit waste preg i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region [fw=iwt]

probit uweight preg i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region [fw=iwt]

*** Model: IV probit
ivprobit ciaf i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

ivprobit stunt i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

ivprobit waste i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

ivprobit uweight i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2020 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first


*** Heterogeniety Analysis

**Regional Variations

bysort region: ivprobit ciaf i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater (preg=lafm1) [fw=iwt], first

bysort region: ivprobit stunt i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater (preg=lafm1) [fw=iwt], first

bysort region: ivprobit waste i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater (preg=lafm1) [fw=iwt], first

bysort region: ivprobit uweight i.v025 i.v106 ib4.src lv191 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater (preg=lafm1) [fw=iwt], first

**Socio religious communities
bysort src: ivprobit ciaf i.v025 i.v106 lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

bysort src: ivprobit stunt i.v025 i.v106 lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first
 

bysort src: ivprobit waste i.v025 i.v106 lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

bysort src: ivprobit uweight i.v025 i.v106 lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

**Wealth
bysort v190: ivprobit ciaf i.v025 i.v106 ib4.src v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

bysort v190: ivprobit stunt i.v025 i.v106 ib4.src v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

bysort v190: ivprobit waste i.v025 i.v106 ib4.src  v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

bysort v190: ivprobit uweight i.v025 i.v106 ib4.src  v212 i.rbv457 b4 bfed daridity ///
irrigation temp2020 nightlight i.impwater i.impsan i.region (preg=lafm1) [fw=iwt], first


**Education
bysort v106: ivprobit ciaf i.v025 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

bysort v106: ivprobit stunt i.v025 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

bysort v106: ivprobit waste i.v025 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2015 nightlight i.impsan i.impwater i.region (preg=lafm1) [fw=iwt], first

bysort v106: ivprobit uweight i.v025 ib4.src lv191 v212 i.rbv457 b4 bfed daridity ///
irrigation temp2020 nightlight i.impwater i.impsan i.region (preg=lafm1) [fw=iwt], first



