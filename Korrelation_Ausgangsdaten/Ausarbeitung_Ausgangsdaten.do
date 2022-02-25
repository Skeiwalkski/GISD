
tempfile GISD

use "C:\git_projects\GISD\Outfiles\2018\Bund\Gemeinde\Gemeinde_long.dta", clear

keep Gemeindekennziffer GISD_Score Jahr
rename GISD_Score GISD_2018

save `GISD'


use "C:\git_projects\GISD\Outfiles\2019\Bund\Gemeinde\Gemeinde_long.dta", clear

keep Gemeindekennziffer GISD_Score Jahr
rename GISD_Score GISD_2019

merge 1:1 Gemeindekennziffer Jahr using `GISD'
drop _merge
sort Gemeindekennziffer Jahr
save `GISD', replace


use "C:\git_projects\GISD\Outfiles\2020\Bund\Gemeinde\Gemeinde_long.dta", clear

keep Gemeindekennziffer GISD_Score Jahr
rename GISD_Score GISD_2020

merge 1:1 Gemeindekennziffer Jahr using `GISD'
drop _merge
sort Gemeindekennziffer Jahr
save `GISD', replace


use "C:\git_projects\GISD\Outfiles\2021_v2\Bund\Gemeinde\Gemeinde_long.dta", clear

keep Gemeindekennziffer GISD_Score Jahr
rename GISD_Score GISD_2021_v2

merge 1:1 Gemeindekennziffer Jahr using `GISD'
drop _merge
sort Gemeindekennziffer Jahr
save `GISD', replace


use "C:\git_projects\GISD\Outfiles\2022\Bund\Gemeinde\Gemeinde_long.dta"

keep Gemeindekennziffer GISD_Score Jahr
rename GISD_Score GISD_2022

merge 1:1 Gemeindekennziffer Jahr using `GISD'
drop _merge
sort Gemeindekennziffer Jahr
save `GISD', replace


****************************************
log using GISD.log, replace
corr GISD_2018 GISD_2019 GISD_2020 GISD_2021_v2 GISD_2022

corr GISD_2018 GISD_2019 GISD_2020 GISD_2021_v2 GISD_2022 if Jahr == 2014

bysort Jahr: corr GISD_2018 GISD_2019 GISD_2020 GISD_2021_v2 GISD_2022
log close

********************************************************************************
*Arbeitslosigkeit
clear

tempfile Arbeitslosigkeit

use "C:\git_projects\GISD\Outfiles\2022\Stata\Impdata_check_2019.dta"
keep Gemeindekennziffer Jahr Arbeitslosigkeit
rename Gemeindekennziffer Kennziffer
rename Arbeitslosigkeit Alos19_

destring Kennziffer, replace

duplicates drop
drop if Jahr > 2014
sort Kennziffer Jahr
save `Arbeitslosigkeit', replace


import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\A_Arbeitslosigkeit.xls", sheet("Daten") cellrange(A2:W4931) firstrow clear

drop B U V W 
rename A Kennziffer
destring Kennziffer, replace

drop if C != "Gemeinde"
drop C

local new_name "Alos17_1998 Alos17_1999 Alos17_2000 Alos17_2001 Alos17_2002 Alos17_2003 Alos17_2004 Alos17_2005 Alos17_2006 Alos17_2007 Alos17_2008 Alos17_2009 Alos17_2010 Alos17_2011 Alos17_2012 Alos17_2013 Alos17_2014"
local old_name "D E F G H I J K L M N O P Q R S T"

rename (`old_name') (`new_name')

reshape long Alos17_, i(Kennziffer) j(Jahr)

destring Kennziffer, replace
sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Arbeitslosigkeit'
drop _merge
save `Arbeitslosigkeit', replace


import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\A_Arbeitslosigkeit.xls", sheet("Daten") cellrange(A2:U4540) firstrow clear

drop B U 
rename A Kennziffer
destring Kennziffer, replace

drop if C != "Gemeinde"
drop C

local new_name "Alos15_1998 Alos15_1999 Alos15_2000 Alos15_2001 Alos15_2002 Alos15_2003 Alos15_2004 Alos15_2005 Alos15_2006 Alos15_2007 Alos15_2008 Alos15_2009 Alos15_2010 Alos15_2011 Alos15_2012 Alos15_2013 Alos15_2014"
local old_name "D E F G H I J K L M N O P Q R S T"

rename (`old_name') (`new_name')

reshape long Alos15_, i(Kennziffer) j(Jahr)

destring Kennziffer, replace
sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Arbeitslosigkeit'
drop _merge
save `Arbeitslosigkeit', replace


import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\A_Arbeitslosigkeit.xls", sheet("Daten") cellrange(A2:T4544) firstrow clear

drop B
rename A Kennziffer
destring Kennziffer, replace

drop if C != "Gemeinde"
drop C

local new_name "Alos14_1998 Alos14_1999 Alos14_2000 Alos14_2001 Alos14_2002 Alos14_2003 Alos14_2004 Alos14_2005 Alos14_2006 Alos14_2007 Alos14_2008 Alos14_2009 Alos14_2010 Alos14_2011 Alos14_2012 Alos14_2013 Alos14_2014"
local old_name "D E F G H I J K L M N O P Q R S T"

rename (`old_name') (`new_name')

reshape long Alos14_, i(Kennziffer) j(Jahr)

replace Alos14_ = Alos14_ *10

destring Kennziffer, replace
sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Arbeitslosigkeit'
drop _merge

log using A_Arbeitslosigkeit.log, replace
corr Alos14_ Alos15_ Alos17_ Alos19_
log close





********************************************************************************
*Beschäftigtenquote
clear

tempfile Beschäftigtenquote

*2019
import excel "C:\git_projects\GISD\Data\INKAR_1998_2019\A_Beschaeftigtenquote.xlsx", sheet("Daten") cellrange(A2:V4620) firstrow

drop B C R S T U V
rename A Kennziffer
destring Kennziffer, replace

local new_name "Beschquote19_2001 Beschquote19_2002 Beschquote19_2003 Beschquote19_2004 Beschquote19_2005 Beschquote19_2006 Beschquote19_2007 Beschquote19_2008 Beschquote19_2009 Beschquote19_2010 Beschquote19_2011 Beschquote19_2012 Beschquote19_2013 Beschquote19_2014"
local old_name "D E F G H I J K L M N O P Q"

rename (`old_name') (`new_name')

reshape long Beschquote19_, i(Kennziffer) j(Jahr)

destring Kennziffer, replace
sort Kennziffer Jahr

save `Beschäftigtenquote', replace

*2017
import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\A_Beschaeftigtenquote.xls", sheet("Daten") cellrange(A2:T4530) firstrow clear

drop B R S T
rename A Kennziffer
destring Kennziffer, replace

drop if C != "Gemeinde"
drop C

local new_name "Beschquote17_2001 Beschquote17_2002 Beschquote17_2003 Beschquote17_2004 Beschquote17_2005 Beschquote17_2006 Beschquote17_2007 Beschquote17_2008 Beschquote17_2009 Beschquote17_2010 Beschquote17_2011 Beschquote17_2012 Beschquote17_2013 Beschquote17_2014"
local old_name "D E F G H I J K L M N O P Q"

rename (`old_name') (`new_name')

reshape long Beschquote17_, i(Kennziffer) j(Jahr)

merge 1:1 Kennziffer Jahr using `Beschäftigtenquote'
drop _merge
save `Beschäftigtenquote', replace

*2015
import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\A_Beschaeftigtenquote.xls", sheet("Daten") cellrange(A2:R4540) firstrow clear

drop B R
rename A Kennziffer
destring Kennziffer, replace

drop if C != "Gemeinde"
drop C

local new_name "Beschquote15_2001 Beschquote15_2002 Beschquote15_2003 Beschquote15_2004 Beschquote15_2005 Beschquote15_2006 Beschquote15_2007 Beschquote15_2008 Beschquote15_2009 Beschquote15_2010 Beschquote15_2011 Beschquote15_2012 Beschquote15_2013 Beschquote15_2014"
local old_name "D E F G H I J K L M N O P Q"

rename (`old_name') (`new_name')

reshape long Beschquote15_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Beschäftigtenquote'
drop _merge
save `Beschäftigtenquote', replace

*2014
import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\A_Beschaeftigtenquote.xls", sheet("Daten") cellrange(A2:Q4544) firstrow clear

drop B
rename A Kennziffer
destring Kennziffer, replace

drop if C != "Gemeinde"
drop C

local new_name "Beschquote14_2001 Beschquote14_2002 Beschquote14_2003 Beschquote14_2004 Beschquote14_2005 Beschquote14_2006 Beschquote14_2007 Beschquote14_2008 Beschquote14_2009 Beschquote14_2010 Beschquote14_2011 Beschquote14_2012 Beschquote14_2013 Beschquote14_2014"
local old_name "D E F G H I J K L M N O P Q"

rename (`old_name') (`new_name')

reshape long Beschquote14_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Beschäftigtenquote'
drop _merge

log using A_Beschäftigtenquote.log, replace
corr Beschquote14_ Beschquote15_ Beschquote17_ Beschquote19_
log close




********************************************************************************
*Bruttoverdienst
clear

tempfile Bruttoverdienst

*2019
import excel "C:\git_projects\GISD\Data\INKAR_1998_2019\A_Bruttoverdienst.xlsx", sheet("Daten") cellrange(A2:W403) firstrow

drop B C S T U V W
rename A Kennziffer
destring Kennziffer, replace

local new_name "Brutto19_2000 Brutto19_2001 Brutto19_2002 Brutto19_2003 Brutto19_2004 Brutto19_2005 Brutto19_2006 Brutto19_2007 Brutto19_2008 Brutto19_2009 Brutto19_2010 Brutto19_2011 Brutto19_2012 Brutto19_2013 Brutto19_2014"
local old_name "D E F G H I J K L M N O P Q R"

rename (`old_name') (`new_name')

reshape long Brutto19_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

save `Bruttoverdienst', replace


*2017
import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\A_Bruttoverdienst.xls", sheet("Daten") cellrange(A2:U403) firstrow clear

drop B C S T U
rename A Kennziffer
destring Kennziffer, replace

local new_name "Brutto17_2000 Brutto17_2001 Brutto17_2002 Brutto17_2003 Brutto17_2004 Brutto17_2005 Brutto17_2006 Brutto17_2007 Brutto17_2008 Brutto17_2009 Brutto17_2010 Brutto17_2011 Brutto17_2012 Brutto17_2013 Brutto17_2014"
local old_name "D E F G H I J K L M N O P Q R"

rename (`old_name') (`new_name')

reshape long Brutto17_, i(Kennziffer) j(Jahr)

merge 1:1 Kennziffer Jahr using `Bruttoverdienst'
drop _merge
save `Bruttoverdienst', replace


*2015
import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\A_Bruttoverdienst.xls", sheet("Daten") cellrange(A2:S404) firstrow clear

drop B C S
rename A Kennziffer
destring Kennziffer, replace

local new_name "Brutto15_2000 Brutto15_2001 Brutto15_2002 Brutto15_2003 Brutto15_2004 Brutto15_2005 Brutto15_2006 Brutto15_2007 Brutto15_2008 Brutto15_2009 Brutto15_2010 Brutto15_2011 Brutto15_2012 Brutto15_2013 Brutto15_2014"
local old_name "D E F G H I J K L M N O P Q R"

rename (`old_name') (`new_name')

reshape long Brutto15_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Bruttoverdienst'
drop _merge
save `Bruttoverdienst', replace


*2014
import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\A_Bruttoverdienst.xls", sheet("Daten") cellrange(A2:R4946) firstrow clear

drop B C
rename A Kennziffer
destring Kennziffer, replace

drop if Kennziffer > 16077

local new_name "Brutto14_2000 Brutto14_2001 Brutto14_2002 Brutto14_2003 Brutto14_2004 Brutto14_2005 Brutto14_2006 Brutto14_2007 Brutto14_2008 Brutto14_2009 Brutto14_2010 Brutto14_2011 Brutto14_2012 Brutto14_2013 Brutto14_2014"
local old_name "D E F G H I J K L M N O P Q R"

rename (`old_name') (`new_name')

reshape long Brutto14_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Bruttoverdienst'
drop _merge

log using A_Bruttoverdienst.log, replace
corr Brutto14_ Brutto15_ Brutto17_ Brutto19_
log close




********************************************************************************
*Beschäftigte mit akad. Abschluss
clear

tempfile BeschakAbschl

*2019
use "C:\git_projects\GISD\Outfiles\2022\Stata\Impdata_check_2019.dta"

keep Kreis Jahr BeschaeftigtemitakadAbschluss
rename Kreis Kennziffer
rename BeschaeftigtemitakadAbschluss BeschakAbschl19_

duplicates drop
drop if Jahr != 2014

sort Kennziffer Jahr

save `BeschakAbschl', replace


*2017
import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\B_BeschaeftigtemitakadAbschluss.xls", sheet("Daten") cellrange(A2:I403) firstrow clear

drop B C G H I
rename A Kennziffer
destring Kennziffer, replace

local new_name "BeschakAbschl17_2012 BeschakAbschl17_2013 BeschakAbschl17_2014"
local old_name "D E F"

rename (`old_name') (`new_name')

reshape long BeschakAbschl17_, i(Kennziffer) j(Jahr)

merge 1:1 Kennziffer Jahr using `BeschakAbschl'
drop _merge
save `BeschakAbschl', replace


*2015
import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\B_BeschaeftigtemitakadAbschluss.xls", sheet("Daten") cellrange(A2:G404) firstrow clear

drop B C G
rename A Kennziffer
destring Kennziffer, replace

local new_name "BeschakAbschl15_2012 BeschakAbschl15_2013 BeschakAbschl15_2014"
local old_name "D E F"

rename (`old_name') (`new_name')

reshape long BeschakAbschl15_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `BeschakAbschl'
drop _merge
save `BeschakAbschl', replace


*2014
import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\B_BeschaeftigtemitakadAbschluss.xls", sheet("Daten") cellrange(A2:F4984) firstrow clear

drop B C
rename A Kennziffer
destring Kennziffer, replace

drop if Kennziffer > 16077

local new_name "BeschakAbschl14_2012 BeschakAbschl14_2013 BeschakAbschl14_2014"
local old_name "D E F"

rename (`old_name') (`new_name')

reshape long BeschakAbschl14_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `BeschakAbschl'
drop _merge

log using B_BeschaeftigtemitakadAbschluss.log, replace
corr BeschakAbschl14_ BeschakAbschl15_ BeschakAbschl17_ BeschakAbschl19_
log close




********************************************************************************
*Beschäftigte ohne Abschluss
clear

tempfile BeschohneAbschl

*2019
use "C:\git_projects\GISD\Outfiles\2022\Stata\Impdata_check_2019.dta"

keep Kreis Jahr BeschaeftigteohneAbschluss
rename Kreis Kennziffer
rename BeschaeftigteohneAbschluss BeschohneAbschl19_

duplicates drop
drop if Jahr != 2014

sort Kennziffer Jahr

save `BeschohneAbschl', replace


*2017
import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\B_BeschaeftigteohneAbschluss.xls", sheet("Daten") cellrange(A2:I403) firstrow clear

drop B C G H I
rename A Kennziffer
destring Kennziffer, replace

local new_name "BeschohneAbschl17_2012 BeschohneAbschl17_2013 BeschohneAbschl17_2014"
local old_name "D E F"

rename (`old_name') (`new_name')

reshape long BeschohneAbschl17_, i(Kennziffer) j(Jahr)

merge 1:1 Kennziffer Jahr using `BeschohneAbschl'
drop _merge
save `BeschohneAbschl', replace


*2015
import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\B_BeschaeftigteohneAbschluss.xls", sheet("Daten") cellrange(A2:G404) firstrow clear

drop B C G
rename A Kennziffer
destring Kennziffer, replace

local new_name "BeschohneAbschl15_2012 BeschohneAbschl15_2013 BeschohneAbschl15_2014"
local old_name "D E F"

rename (`old_name') (`new_name')

reshape long BeschohneAbschl15_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `BeschohneAbschl'
drop _merge
save `BeschohneAbschl', replace


*2014
import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\B_BeschaeftigteohneAbschluss.xls", sheet("Daten") cellrange(A2:F4946) firstrow clear

drop B C
rename A Kennziffer
destring Kennziffer, replace

drop if Kennziffer > 16077

local new_name "BeschohneAbschl14_2012 BeschohneAbschl14_2013 BeschohneAbschl14_2014"
local old_name "D E F"

rename (`old_name') (`new_name')

reshape long BeschohneAbschl14_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `BeschohneAbschl'
drop _merge

log using B_BeschaeftigteohneAbschluss.log, replace
corr BeschohneAbschl14_ BeschohneAbschl15_ BeschohneAbschl17_ BeschohneAbschl19_
log close





********************************************************************************
*Schulabgänger mit Hochschulreife
clear

tempfile SchulemitAbi

*2019
import excel "C:\git_projects\GISD\Data\INKAR_1998_2019\B_SchulabgaengermitHochschulreife.xlsx", sheet("Daten") cellrange(A2:AB403) firstrow

drop B C D E F X Y Z AA AB
rename A Kennziffer
destring Kennziffer, replace

local new_name "SchulemitAbi19_1998 SchulemitAbi19_1999 SchulemitAbi19_2000 SchulemitAbi19_2001 SchulemitAbi19_2002 SchulemitAbi19_2003 SchulemitAbi19_2004 SchulemitAbi19_2005 SchulemitAbi19_2006 SchulemitAbi19_2007 SchulemitAbi19_2008 SchulemitAbi19_2009 SchulemitAbi19_2010 SchulemitAbi19_2011 SchulemitAbi19_2012 SchulemitAbi19_2013 SchulemitAbi19_2014"
local old_name "G H I J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long SchulemitAbi19_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

save `SchulemitAbi', replace


*2017
import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\B_SchulabgaengermitHochschulreife.xls", sheet("Daten") cellrange(A2:Z403) firstrow clear

drop B C D E F X Y Z
rename A Kennziffer
destring Kennziffer, replace

local new_name "SchulemitAbi17_1998 SchulemitAbi17_1999 SchulemitAbi17_2000 SchulemitAbi17_2001 SchulemitAbi17_2002 SchulemitAbi17_2003 SchulemitAbi17_2004 SchulemitAbi17_2005 SchulemitAbi17_2006 SchulemitAbi17_2007 SchulemitAbi17_2008 SchulemitAbi17_2009 SchulemitAbi17_2010 SchulemitAbi17_2011 SchulemitAbi17_2012 SchulemitAbi17_2013 SchulemitAbi17_2014"
local old_name "G H I J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long SchulemitAbi17_, i(Kennziffer) j(Jahr)

merge 1:1 Kennziffer Jahr using `SchulemitAbi'
drop _merge
save `SchulemitAbi', replace


*2015
import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\B_SchulabgaengermitHochschulreife.xls", sheet("Daten") cellrange(A2:X404) firstrow clear

drop B C D E F X
rename A Kennziffer
destring Kennziffer, replace

local new_name "SchulemitAbi15_1998 SchulemitAbi15_1999 SchulemitAbi15_2000 SchulemitAbi15_2001 SchulemitAbi15_2002 SchulemitAbi15_2003 SchulemitAbi15_2004 SchulemitAbi15_2005 SchulemitAbi15_2006 SchulemitAbi15_2007 SchulemitAbi15_2008 SchulemitAbi15_2009 SchulemitAbi15_2010 SchulemitAbi15_2011 SchulemitAbi15_2012 SchulemitAbi15_2013 SchulemitAbi15_2014"
local old_name "G H I J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long SchulemitAbi15_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `SchulemitAbi'
drop _merge
save `SchulemitAbi', replace


*2014
import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\B_SchulabgaengermitHochschulreife.xls", sheet("Daten") cellrange(A2:W4946) firstrow clear

drop B C D E F
rename A Kennziffer
destring Kennziffer, replace

drop if Kennziffer > 16077

local new_name "SchulemitAbi14_1998 SchulemitAbi14_1999 SchulemitAbi14_2000 SchulemitAbi14_2001 SchulemitAbi14_2002 SchulemitAbi14_2003 SchulemitAbi14_2004 SchulemitAbi14_2005 SchulemitAbi14_2006 SchulemitAbi14_2007 SchulemitAbi14_2008 SchulemitAbi14_2009 SchulemitAbi14_2010 SchulemitAbi14_2011 SchulemitAbi14_2012 SchulemitAbi14_2013 SchulemitAbi14_2014"
local old_name "G H I J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long SchulemitAbi14_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `SchulemitAbi'
drop _merge

log using B_SchulabgaengermitHochschulreife.log, replace
corr SchulemitAbi14_ SchulemitAbi15_ SchulemitAbi17_ SchulemitAbi19_
log close




********************************************************************************
*Schulabgänger ohne Abschluss
clear

tempfile SchuleohneAbschluss

*2019
import excel "C:\git_projects\GISD\Data\INKAR_1998_2019\B_SchulabgaengerohneAbschluss.xlsx", sheet("Daten") cellrange(A2:AB403) firstrow

drop B C D E F X Y Z AA AB
rename A Kennziffer
destring Kennziffer, replace

local new_name "SchuleohneAbschl19_1998 SchuleohneAbschl19_1999 SchuleohneAbschl19_2000 SchuleohneAbschl19_2001 SchuleohneAbschl19_2002 SchuleohneAbschl19_2003 SchuleohneAbschl19_2004 SchuleohneAbschl19_2005 SchuleohneAbschl19_2006 SchuleohneAbschl19_2007 SchuleohneAbschl19_2008 SchuleohneAbschl19_2009 SchuleohneAbschl19_2010 SchuleohneAbschl19_2011 SchuleohneAbschl19_2012 SchuleohneAbschl19_2013 SchuleohneAbschl19_2014"
local old_name "G H I J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long SchuleohneAbschl19_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

save `SchuleohneAbschluss', replace


*2017
import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\B_SchulabgaengerohneAbschluss.xls", sheet("Daten") cellrange(A2:Z403) firstrow clear

drop B C D E F X Y Z
rename A Kennziffer
destring Kennziffer, replace

local new_name "SchuleohneAbschl17_1998 SchuleohneAbschl17_1999 SchuleohneAbschl17_2000 SchuleohneAbschl17_2001 SchuleohneAbschl17_2002 SchuleohneAbschl17_2003 SchuleohneAbschl17_2004 SchuleohneAbschl17_2005 SchuleohneAbschl17_2006 SchuleohneAbschl17_2007 SchuleohneAbschl17_2008 SchuleohneAbschl17_2009 SchuleohneAbschl17_2010 SchuleohneAbschl17_2011 SchuleohneAbschl17_2012 SchuleohneAbschl17_2013 SchuleohneAbschl17_2014"
local old_name "G H I J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long SchuleohneAbschl17_, i(Kennziffer) j(Jahr)

merge 1:1 Kennziffer Jahr using `SchuleohneAbschluss'
drop _merge
save `SchuleohneAbschluss', replace


*2015
import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\B_SchulabgaengerohneAbschluss.xls", sheet("Daten") cellrange(A2:X404) firstrow clear

drop B C D E F X
rename A Kennziffer
destring Kennziffer, replace

local new_name "SchuleohneAbschl15_1998 SchuleohneAbschl15_1999 SchuleohneAbschl15_2000 SchuleohneAbschl15_2001 SchuleohneAbschl15_2002 SchuleohneAbschl15_2003 SchuleohneAbschl15_2004 SchuleohneAbschl15_2005 SchuleohneAbschl15_2006 SchuleohneAbschl15_2007 SchuleohneAbschl15_2008 SchuleohneAbschl15_2009 SchuleohneAbschl15_2010 SchuleohneAbschl15_2011 SchuleohneAbschl15_2012 SchuleohneAbschl15_2013 SchuleohneAbschl15_2014"
local old_name "G H I J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long SchuleohneAbschl15_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `SchuleohneAbschluss'
drop _merge
save `SchuleohneAbschluss', replace


*2014
import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\B_SchulabgaengerohneAbschluss.xls", sheet("Daten") cellrange(A2:W4946) firstrow clear

drop B C D E F
rename A Kennziffer
destring Kennziffer, replace

drop if Kennziffer > 16077

local new_name "SchuleohneAbschl14_1998 SchuleohneAbschl14_1999 SchuleohneAbschl14_2000 SchuleohneAbschl14_2001 SchuleohneAbschl14_2002 SchuleohneAbschl14_2003 SchuleohneAbschl14_2004 SchuleohneAbschl14_2005 SchuleohneAbschl14_2006 SchuleohneAbschl14_2007 SchuleohneAbschl14_2008 SchuleohneAbschl14_2009 SchuleohneAbschl14_2010 SchuleohneAbschl14_2011 SchuleohneAbschl14_2012 SchuleohneAbschl14_2013 SchuleohneAbschl14_2014"
local old_name "G H I J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long SchuleohneAbschl14_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `SchuleohneAbschluss'
drop _merge

log using B_SchulabgaengerohneAbschluss.log, replace
corr SchuleohneAbschl14_ SchuleohneAbschl15_ SchuleohneAbschl17_ SchuleohneAbschl19_
log close






********************************************************************************
*Einkommensteuer
clear

tempfile Einkommensteuer

*2019
import excel "C:\git_projects\GISD\Data\INKAR_1998_2019\E_Einkommensteuer.xlsx", sheet("Daten") cellrange(A2:AB4620) firstrow

drop B C D E F G H I X Y Z AA AB
rename A Kennziffer
destring Kennziffer, replace

local new_name "Einkommensteuer19_2001 Einkommensteuer19_2002 Einkommensteuer19_2003 Einkommensteuer19_2004 Einkommensteuer19_2005 Einkommensteuer19_2006 Einkommensteuer19_2007 Einkommensteuer19_2008 Einkommensteuer19_2009 Einkommensteuer19_2010 Einkommensteuer19_2011 Einkommensteuer19_2012 Einkommensteuer19_2013 Einkommensteuer19_2014"
local old_name "J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long Einkommensteuer19_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

save `Einkommensteuer', replace


*2017
import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\E_Einkommensteuer.xls", sheet("Daten") cellrange(A2:T4530) firstrow clear

drop B R S T
rename A Kennziffer
destring Kennziffer, replace

drop if C != "Gemeinde"
drop C

local new_name "Einkommensteuer17_2001 Einkommensteuer17_2002 Einkommensteuer17_2003 Einkommensteuer17_2004 Einkommensteuer17_2005 Einkommensteuer17_2006 Einkommensteuer17_2007 Einkommensteuer17_2008 Einkommensteuer17_2009 Einkommensteuer17_2010 Einkommensteuer17_2011 Einkommensteuer17_2012 Einkommensteuer17_2013 Einkommensteuer17_2014"
local old_name "D E F G H I J K L M N O P Q"

rename (`old_name') (`new_name')

reshape long Einkommensteuer17_, i(Kennziffer) j(Jahr)

merge 1:1 Kennziffer Jahr using `Einkommensteuer'
drop _merge
save `Einkommensteuer', replace


*2015
import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\E_Einkommenssteuer.xls", sheet("Daten") cellrange(A2:X4540) firstrow clear

drop B D E F G H I X
rename A Kennziffer
destring Kennziffer, replace

drop if C != "Gemeinde"
drop C

local new_name "Einkommensteuer15_2001 Einkommensteuer15_2002 Einkommensteuer15_2003 Einkommensteuer15_2004 Einkommensteuer15_2005 Einkommensteuer15_2006 Einkommensteuer15_2007 Einkommensteuer15_2008 Einkommensteuer15_2009 Einkommensteuer15_2010 Einkommensteuer15_2011 Einkommensteuer15_2012 Einkommensteuer15_2013 Einkommensteuer15_2014"
local old_name "J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long Einkommensteuer15_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Einkommensteuer'
drop _merge
save `Einkommensteuer', replace


*2014
import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\E_Einkommenssteuer.xls", sheet("Daten") cellrange(A2:W4984) firstrow clear

drop B D E F G H I
rename A Kennziffer
destring Kennziffer, replace

drop if C != "Gemeinde"
drop C

local new_name "Einkommensteuer14_2001 Einkommensteuer14_2002 Einkommensteuer14_2003 Einkommensteuer14_2004 Einkommensteuer14_2005 Einkommensteuer14_2006 Einkommensteuer14_2007 Einkommensteuer14_2008 Einkommensteuer14_2009 Einkommensteuer14_2010 Einkommensteuer14_2011 Einkommensteuer14_2012 Einkommensteuer14_2013 Einkommensteuer14_2014"
local old_name "J K L M N O P Q R S T U V W"

rename (`old_name') (`new_name')

reshape long Einkommensteuer14_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Einkommensteuer'
drop _merge

log using E_Einkommenssteuer.log, replace
corr Einkommensteuer14_ Einkommensteuer15_ Einkommensteuer17_ Einkommensteuer19_
log close





********************************************************************************
*Haushaltseinkommen
clear

tempfile Haushaltseinkommen

*2019
import excel "C:\git_projects\GISD\Data\INKAR_1998_2019\E_Haushaltseinkommen.xlsx", sheet("Daten") cellrange(A2:W403) firstrow

drop B C S T U V W
rename A Kennziffer
destring Kennziffer, replace

local new_name "HHInc19_2000 HHInc19_2001 HHInc19_2002 HHInc19_2003 HHInc19_2004 HHInc19_2005 HHInc19_2006 HHInc19_2007 HHInc19_2008 HHInc19_2009 HHInc19_2010 HHInc19_2011 HHInc19_2012 HHInc19_2013 HHInc19_2014"
local old_name "D E F G H I J K L M N O P Q R"

rename (`old_name') (`new_name')

reshape long HHInc19_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

save `Haushaltseinkommen', replace


*2017
import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\E_Haushaltseinkommen.xls", sheet("Daten") cellrange(A2:U403) firstrow clear

drop B C S T U
rename A Kennziffer
destring Kennziffer, replace

local new_name "HHInc17_2000 HHInc17_2001 HHInc17_2002 HHInc17_2003 HHInc17_2004 HHInc17_2005 HHInc17_2006 HHInc17_2007 HHInc17_2008 HHInc17_2009 HHInc17_2010 HHInc17_2011 HHInc17_2012 HHInc17_2013 HHInc17_2014"
local old_name "D E F G H I J K L M N O P Q R"

rename (`old_name') (`new_name')

reshape long HHInc17_, i(Kennziffer) j(Jahr)

merge 1:1 Kennziffer Jahr using `Haushaltseinkommen'
drop _merge
save `Haushaltseinkommen', replace


*2015
import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\E_Haushaltseinkommen.xls", sheet("Daten") cellrange(A2:S404) firstrow clear

drop B C S
rename A Kennziffer
destring Kennziffer, replace

local new_name "HHInc15_2000 HHInc15_2001 HHInc15_2002 HHInc15_2003 HHInc15_2004 HHInc15_2005 HHInc15_2006 HHInc15_2007 HHInc15_2008 HHInc15_2009 HHInc15_2010 HHInc15_2011 HHInc15_2012 HHInc15_2013 HHInc15_2014"
local old_name "D E F G H I J K L M N O P Q R"

rename (`old_name') (`new_name')

reshape long HHInc15_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Haushaltseinkommen'
drop _merge
save `Haushaltseinkommen', replace


*2014
import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\E_Haushaltseinkommen.xls", sheet("Daten") cellrange(A2:R4984) firstrow clear

drop B C
rename A Kennziffer
destring Kennziffer, replace

drop if Kennziffer > 16077

local new_name "HHInc14_2000 HHInc14_2001 HHInc14_2002 HHInc14_2003 HHInc14_2004 HHInc14_2005 HHInc14_2006 HHInc14_2007 HHInc14_2008 HHInc14_2009 HHInc14_2010 HHInc14_2011 HHInc14_2012 HHInc14_2013 HHInc14_2014"
local old_name "D E F G H I J K L M N O P Q R"

rename (`old_name') (`new_name')

reshape long HHInc14_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Haushaltseinkommen'
drop _merge

log using E_Haushaltseinkommen.log, replace
corr HHInc14_ HHInc15_ HHInc17_ HHInc19_
log close





********************************************************************************
*Schuldnerquote
clear

tempfile Schuldnerquote

*2019
import excel "C:\git_projects\GISD\Data\INKAR_1998_2019\E_Schuldnerquote.xlsx", sheet("Daten") cellrange(A2:S403) firstrow

drop B C O P Q R S
rename A Kennziffer
destring Kennziffer, replace

local new_name "Schuldner19_2004 Schuldner19_2005 Schuldner19_2006 Schuldner19_2007 Schuldner19_2008 Schuldner19_2009 Schuldner19_2010 Schuldner19_2011 Schuldner19_2012 Schuldner19_2013 Schuldner19_2014"
local old_name "D E F G H I J K L M N"

rename (`old_name') (`new_name')

reshape long Schuldner19_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

save `Schuldnerquote', replace


*2017
import excel "C:\git_projects\GISD\Data\INKAR_1998_2017\E_Schuldnerquote.xls", sheet("Daten") cellrange(A2:Q403) firstrow clear

drop B C O P Q
rename A Kennziffer
destring Kennziffer, replace

local new_name "Schuldner17_2004 Schuldner17_2005 Schuldner17_2006 Schuldner17_2007 Schuldner17_2008 Schuldner17_2009 Schuldner17_2010 Schuldner17_2011 Schuldner17_2012 Schuldner17_2013 Schuldner17_2014"
local old_name "D E F G H I J K L M N"

rename (`old_name') (`new_name')

reshape long Schuldner17_, i(Kennziffer) j(Jahr)

merge 1:1 Kennziffer Jahr using `Schuldnerquote'
drop _merge
save `Schuldnerquote', replace


*2015
import excel "C:\git_projects\GISD\Data\INKAR_1998_2015\E_Schuldnerquote.xls", sheet("Daten") cellrange(A2:O404) firstrow clear

drop B C O
rename A Kennziffer
destring Kennziffer, replace

local new_name "Schuldner15_2004 Schuldner15_2005 Schuldner15_2006 Schuldner15_2007 Schuldner15_2008 Schuldner15_2009 Schuldner15_2010 Schuldner15_2011 Schuldner15_2012 Schuldner15_2013 Schuldner15_2014"
local old_name "D E F G H I J K L M N"

rename (`old_name') (`new_name')

reshape long Schuldner15_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Schuldnerquote'
drop _merge
save `Schuldnerquote', replace


*2014
import excel "C:\git_projects\GISD\Data\INKAR_1998_2014\E_Schuldnerquote.xls", sheet("Daten") cellrange(A2:N4984) firstrow clear

drop B C
rename A Kennziffer
destring Kennziffer, replace

drop if Kennziffer > 16077

local new_name "Schuldner14_2004 Schuldner14_2005 Schuldner14_2006 Schuldner14_2007 Schuldner14_2008 Schuldner14_2009 Schuldner14_2010 Schuldner14_2011 Schuldner14_2012 Schuldner14_2013 Schuldner14_2014"
local old_name "D E F G H I J K L M N"

rename (`old_name') (`new_name')

reshape long Schuldner14_, i(Kennziffer) j(Jahr)

sort Kennziffer Jahr

merge 1:1 Kennziffer Jahr using `Schuldnerquote'
drop _merge

log using E_Schuldnerquote.log, replace
corr Schuldner14_ Schuldner15_ Schuldner17_ Schuldner19_
log close