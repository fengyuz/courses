/*
proc import out=proj1_ori ;
datafile="~/biostat699_sas/frail";
dbms=xlsx replace;
sheet="Data";
getnames=yes;
run;
*/

/*
data proj1_ori;
  	infile "~/biostat699_sas/frail.csv" dsd firstobs=2 dlm=",";
	* input dyad_id depr $ male $ age race_eth $ educ_cat $ income bmi sr_hlth frail_2010 $ frail_2012 $ @@;
run;
*/



proc import datafile="~/biostat699_sas/frail.csv"
     out=proj1_ori
     dbms=csv
     replace;
     getnames=yes;
run;

data proj1;
	set proj1_ori;
run;

data proj1;
set proj1;
if race_eth="Non-Hispanic White" then race=1;
if race_eth="African-American" then race=2;
if race_eth="Hispanic" then race=3;
if race_eth="Other" then race=4;
if educ_cat="Less than high school" then ed=1;
if educ_cat="GED or HS graduate" then ed=2;
if educ_cat="Some college" then ed=3;
if educ_cat="College and above" then ed=4;
run;



proc format;
value race 1="Non-Hispanic White"
			2="African-American"
			3="Hispanic"
			4="Other";
value ed	1="Less than high school"
			2="GED or HS graduate"
			3="Some college"
			4="College and above";
run;


proc sort data=proj1;
by dyad_id male;
run;

data proj1_sp;
set proj1;


proc transpose data=proj1 out=proj1_sp prefix=depr_;
by dyad_id;
id male;
var depr;
run;

proc transpose data=proj1 out=proj1_tmp prefix=sex_;
by dyad_id;
id male;
var male;
run;


data proj1_sp;
merge proj1_sp (drop=_name_) proj1_tmp (drop=_name_);
by dyad_id;
run;


proc transpose data=proj1 out=proj1_tmp prefix=age_;
by dyad_id;
id male;
var age;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=race_;
by dyad_id;
id male;
var race;
run;



data proj1_sp;
merge proj1_sp proj1_tmp;
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=ed_;
by dyad_id;
id male;
var ed;
run;

data proj1_sp;
merge proj1_sp proj1_tmp;
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=bmi_;
by dyad_id;
id male;
var bmi;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=sr_hlth_;
by dyad_id;
id male;
var sr_hlth;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=frail_2010_;
by dyad_id;
id male;
var frail_2010;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=frail_2012_;
by dyad_id;
id male;
var frail_2012;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_);
by dyad_id;
run;



proc transpose data=proj1 out=proj1_tmp prefix=income_;
by dyad_id;
id male;
var income;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (rename=(income_1=income) drop=_name_ income_0);
by dyad_id;
run;

data proj1_sp;
set proj1_sp;
drop _name_;
run;

data proj1_sp_uni;
set proj1;
run;

proc transpose data=proj1 out=proj1_tmp prefix=depr_;
by dyad_id;
id male;
var depr;
run;

data proj1_sp_uni;
merge proj1_sp_uni proj1_tmp (drop=_name_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=frail_2010_;
by dyad_id;
id male;
var frail_2010;
run;

data proj1_sp_uni;
merge proj1_sp_uni proj1_tmp (drop=_name_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=age_;
by dyad_id;
id male;
var age;
run;

data proj1_sp_uni;
merge proj1_sp_uni proj1_tmp (drop=_name_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=ed_;
by dyad_id;
id male;
var ed;
run;


data proj1_sp_uni;
merge proj1_sp_uni proj1_tmp (drop=_name_);
by dyad_id;
run;

data proj1_sp_uni;
set proj1_sp_uni;
if male = 0 then depr_self = depr_0; 
if male = 1 then depr_self = depr_1; 
if male = 0 then depr_spouse = depr_1; 
if male = 1 then depr_spouse = depr_0; 
if male = 0 then frail_2010_self = frail_2010_0; 
if male = 1 then frail_2010_self = frail_2010_1; 
if male = 0 then frail_2010_spouse = frail_2010_1; 
if male = 1 then frail_2010_spouse = frail_2010_0; 
if male = 0 then age_self = age_0; 
if male = 1 then age_self = age_1; 
if male = 0 then age_spouse = age_1; 
if male = 1 then age_spouse = age_0; 
if male = 0 then ed_self = ed_0; 
if male = 1 then ed_self = ed_1; 
if male = 0 then ed_spouse = ed_1; 
if male = 1 then ed_spouse = ed_0; 
age_sp_diff = age_spouse-age_self;
*college = (ed="4");
run;


data proj1_long_uni;
set proj1;
time=2010; frail=frail_2010; output;
time=2012; frail=frail_2012; output;
run;



libname perm "~/biostat699_sas/";

data perm.proj1_sp;
set proj1_sp;
run;



title "Interdep, no corr, no interact";
proc logistic data=proj1_sp_uni plots(maxpoints=none only)=oddsratio;
	class dyad_id depr_self (ref="0") depr_spouse (ref="0") frail_2010_self (ref="0") frail_2010_spouse (ref="0")  frail_2012;
	model frail_2012 (event="1") = income age_self age_spouse ed_self ed_spouse depr_self depr_spouse frail_2010_self frail_2010_spouse;
	oddsratio "Frailty in 2010 (self)" frail_2010_self;
	oddsratio "Frailty in 2010 (spouse)" frail_2010_spouse;
	oddsratio "Depression in 2010 (self)" depr_self;
	oddsratio "Depression in 2010 (spouse)" depr_spouse;
run;

title "Interdep, corr, no interact";
proc genmod data=proj1_sp_uni;
	class dyad_id depr_self (ref="0") depr_spouse (ref="0") frail_2010_self (ref="0") frail_2010_spouse (ref="0")  frail_2012 (ref="0");
	model frail_2012 = income age_self age_spouse ed_self ed_spouse depr_self depr_spouse frail_2010_self frail_2010_spouse
		/ dist=bin;
	repeated subject=dyad_id /type=un corrw;
	estimate "log O.R. income(1k)" income 1 / exp;
	estimate "log O.R. age (self)" age_self 1 / exp;
	estimate "log O.R. age (spouse)" age_spouse 1 / exp;
	estimate "log O.R. ed (self)" ed_self 1 / exp;
	estimate "log O.R. ed (spouse)" ed_spouse 1 / exp;
	estimate "log O.R. depr(self)" depr_self 1 -1 / exp;
	estimate "log O.R. depr(spouse)" depr_spouse 1 -1 / exp;
	estimate "log O.R. frail 2010 (self)" frail_2010_self 1 -1 / exp;
	estimate "log O.R. frail 2010 (spouse)" frail_2010_spouse 1 -1 / exp;
	* estimate "log O.R. hs grad" ed 1 0 0 -1 / exp;
	* estimate "log O.R. some coll" ed 0 1 0 -1 / exp;
	* estimate "log O.R. coll grad" ed 0 0 1 -1 / exp;
run;

/*

title "No interdep, no corr, no interact";
proc logistic data=proj1;
class depr (ref="0") frail_2010 (ref="0") frail_2012 (ref="0");
model frail_2012 (event="1") = age income ed depr frail_2010;
run;

title "Interdep, no corr, interact";
proc logistic data=proj1_sp_uni;
	class dyad_id depr_self (ref="0") depr_spouse (ref="0") frail_2010_self (ref="0") frail_2010_spouse (ref="0")  frail_2012 (ref="0") male (ref="0");
	model frail_2012 (event="1") = income age_self age_spouse ed_self ed_spouse depr_self depr_self*male frail_2010_self frail_2010_self*male depr_spouse depr_spouse*male frail_2010_spouse frail_2010_spouse*male;
run;




proc genmod data=proj1_long_uni;
	class dyad_id depr (ref="0") frail (ref="0");
	model frail = age income ed depr
		/ dist=bin;
	repeated subject=dyad_id /type=un corrw;
	estimate "log O.R. age" age 1 / exp;
	estimate "log O.R. income(1k)" income 1 / exp;
	estimate "log O.R. ed" ed 1 / exp;
	estimate "log O.R. depr(self)" depr 1 -1 / exp;
run;




/*


proc genmod data=proj1;
	class dyad_id depr (ref="0") frail_2010 (ref="0") frail_2012 (ref="0");
	model frail_2012 = age income depr frail_2010 ed
		/ dist=bin;
	repeated subject=dyad_id /type=un corrw;
	estimate "log O.R. age" age 1 / exp;
	estimate "log O.R. income(1k)" income 1 / exp;
	estimate "log O.R. depr" depr 1 -1 / exp;
	estimate "log O.R. frail_2010" frail_2010 1 -1 / exp;
	estimate "log O.R. ed" ed 1 / exp;
run;



/*
proc logistic data=proj1_sp;
class depr_0 depr_1 frail_2010_0 frail_2010_1 frail_2012_0 frail_2012_1;
model frail_2012_0 = age_0 income depr_0 frail_2010_0 depr_1 frail_2010_1;
run;



proc logistic data=proj1_sp;
class depr_0 depr_1 frail_2010_0 frail_2010_1 frail_2012;
model frail_2012 = age income depr_0 frail_2010_0 depr_1 frail_2010_1;
run;
*/

