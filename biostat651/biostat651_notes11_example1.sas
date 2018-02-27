/*****************************************************

	Matched cohort

******************************************************/

data trial1;
  	infile "~/BIOSTAT651/skin_mpc.txt";
	input center treat $ sex $ age improve init_grade @@;
	female=(sex="f");
	treated=(treat="t");
run;

proc print data=trial1; run;

/*********************************************
	Ignore  center
**********************************************/
proc logistic data=trial1 descending;
	model improve = treated female age init_grade;
run;

/*********************************************
	Include  center as a covariate
**********************************************/
proc logistic data=trial1 descending;
	class center;
	model improve = treated female age init_grade center;
run;

/*********************************************
	Conditional logistic regression
**********************************************/

data treat_1;
	set trial1;
	if (treated=0) then delete;
	treated1=treated; drop treated;
	female1=female; drop female;
	age1=age; drop age;
	init_grade1=init_grade; drop init_grade;
	improve1=improve; 
run;

data treat_0;
	set trial1;
	if (treated=1) then delete;
	treated0=treated; drop treated;
	female0=female; drop female;
	age0=age; drop age;
	init_grade0=init_grade; drop init_grade;
	improve0=improve; 
run;

proc sort data=treat_1; by center; run;
proc sort data=treat_0; by center; run;

data matched1;
	merge treat_0 treat_1; by center;
	if (improve0=improve1) then delete;
	treated_10=treated1-treated0;
	female_10=female1-female0;
	age_10=age1-age0;
	init_grade_10=init_grade1-init_grade0;
run;

proc logistic data=matched1 descending;
	model improve1 = treated_10 female_10 age_10 init_grade_10 / noint;
run;

/*********************************************
	Strata by center
**********************************************/
proc logistic data=trial1 descending;
	strata center;
	model improve = treated female age init_grade;
run;


/*****************************************************

	Matched case control

******************************************************/

data study1;
	infile "~/BIOSTAT651/end_cancer.txt";
	input id case age estrogen gall_dis hyper drug_use @@;
run;

/*****************************
	Model with AGE
*******************************/

/* Data Transformation */

data case1;
	set study1;
	if (case=1);
	age1=age;
	estrogen1=estrogen;
	gall_dis1=gall_dis;
	hyper1=hyper;
	drug_use1=drug_use;
	keep id age1 estrogen1 gall_dis1 hyper1 drug_use1;
run;

data case0;
	set study1;
	if (case=0);
	age0=age;
	estrogen0=estrogen;
	gall_dis0=gall_dis;
	hyper0=hyper;
	drug_use0=drug_use;
	keep id age0 estrogen0 gall_dis0 hyper0 drug_use0;
run;

proc sort data=case1; by id; run;
proc sort data=case0; by id; run;

data case_control;
	merge case0 case1; by id;
	age_diff=age1-age0;
	estrogen_diff=estrogen1-estrogen0;
	gall_dis_diff=gall_dis1-gall_dis0;
	hyper_diff=hyper1-hyper0;
	drug_use_diff=drug_use1-drug_use0;
	outcome=1;
run;


/* Logistic regression without strata */
proc logistic data=case_control descending;
	model outcome = estrogen_diff gall_dis_diff hyper_diff drug_use_diff age_diff/ noint;
run;

/* Logistic regression with strata */
proc logistic data=study1 descending;
	model case = estrogen gall_dis hyper drug_use age;
	strata id;
run;


/*****************************
	Model without AGE
*******************************/
proc logistic data=study1 descending;
	model case = estrogen gall_dis hyper drug_use ;
	strata id;
run;
