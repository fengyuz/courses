proc import datafile = '~/biostat651_sas/Table 9.13 Car insurance.xls'
	DBMS = XLS
	out = insurance;
	range = "Sheet1$A3:E35";
	getnames=yes;
run;

data insurance1;
	set insurance;
	log_n = log(n);
	rate=y/n;
run;

title "2(a) Claim rate vs AGE, Car, DIST";
proc sgplot data=insurance1;
	scatter x=age y=rate;
run;
quit;

proc sgplot data=insurance1;
	scatter x=car y=rate;
run;
quit;

proc sgplot data=insurance1;
	scatter x=district y=rate;
run;
quit;

title "2(b): Categorical variables (no interaction)";
proc genmod data = insurance1;
	class car age district / param = ref ref = first;
	model y = car age district / dist = poisson link = log offset = log_n pscale;
run;

title "2(b): Categorical variables (with interaction)";
proc genmod data = insurance1;
	class car age district / param = ref ref = first;
	model y = car age district car*age age*district district*car / dist = poisson link = log offset = log_n;
run;


title "2(c),(e): Continuous variables";
proc genmod data = insurance1;
	model y = car age district / dist = poisson link = log offset = log_n pscale;
run;

proc import datafile = '~/biostat651_sas/Table 9.1 British doctors smoking and coronary death.xls'
	DBMS = XLS
	out = coronary;
	range = "Sheet1$A3:D13";
	getnames=yes;
run;

data coronary1;
	set coronary;
	log_personyears=log(person_years);
	smokingcat=(smoking="smoker");
	agecat=(age="35 to 44")*0 + (age="45 to 54")*1 + (age="55 to 64")*2 + (age="65 to 74")*3 + (age="75 to 84")*4;
	agecatsq=agecat*agecat;
run;

data coronary2_dead(drop=i);
	set coronary1;
	isDead = 1;
	do i = 1 to deaths;
	output;
	end;
run;

data coronary2_alive(drop=i);
	set coronary1;
	isDead = 0;
	do i = 1 to person_years-deaths;
	output;
	end;
run;

data coronary2;
	set coronary2_alive coronary2_dead;
run;

title "3(a),(b): Poisson model";
proc genmod data=coronary1;
	model deaths = smokingcat agecat agecatsq smokingcat*agecat
		/ dist = poisson link=log offset=log_personyears ;
	contrast "Smoking main effect & interaction" 
		smokingcat 1,
		smokingcat*agecat 1;
run;

title "3(a),(b): Binomial model";
proc genmod data=coronary1;
	model deaths/person_years = smokingcat agecat agecatsq smokingcat*agecat
		/ dist = binomial link=logit;
	contrast "Smoking main effect & interaction" 
		smokingcat 1,
		smokingcat*agecat 1;
run;

title "3(a),(b): Bernoulli model";
proc genmod data=coronary2 descending;
	model isDead = smokingcat agecat agecatsq smokingcat*agecat
		/ dist = binomial link=logit;
	contrast "Smoking main effect & interaction" 
		smokingcat 1,
		smokingcat*agecat 1;
run;