

/*

NAME: 	Mammography Experience Study (MEEXP.DAT)
SIZE: 	412 observations, 7 variables

LIST OF VARIABLES:

Variable	Description					Codes/Values		Name
______________________________________________________________________________________________

1		Identification Code				1-412			OBS
2		Mammograph Experience				0 = Never		ME
								1 = Within One Year
								2 = Over One Year Ago
3		"You do not need a mammogram unless		1 = Strongly Agree	SYMPT
		you develop symptoms"				2 = Agree
								3 = Disagree
								4 = Stongly Disagree
4		Perceived benefit of mammography		5 - 20			PB
5		Mother or Sister with a history 		0 = No, 1 = Yes		HIST
		of breast cancer 
6		"Has anyone taught you how to 			0 = No, 1 = Yes		BSE
		examine your own breasts: that is BSE"
7		"How likely is it that a mammogram 		1= Not likely		DETC
		could find a new case of 			2 = Somewhat likely 
		breast cancer"					3 = Very likely
_______________________________________________________________________________________________
*The variable PB is the sum of five scaled responses, each on a four point scale.  
A low value is indicative of a woman with strong agreement with the benefits of mammography.

*/


data mam1;
 infile "~/BIOSTAT651/mammography_HL.txt";
 input idnum mamm_exp symp_req perc_ben fam_hist taught_BSE mamm_det;
run;


data mam2;
 set mam1;
 mamm_exp_=0*(mamm_exp=0)+1*(mamm_exp=2)+2*(mamm_exp=1);

run;

/************************************* 
	mammography vs family history 
*************************************/

proc freq data=mam2;
 tables fam_hist*mamm_exp_ / nocol nopercent;
run;


/************************************* 
	gen logit model  
*************************************/
 
proc logistic data=mam2 descending;
 model mamm_exp_ = fam_hist / link=genlogit;
 contrast "Wald" fam_hist 1 1;
run;

/************************************* 
	Proportional odds 
*************************************/
 
proc logistic data=mam2 ;
 model mamm_exp_ = fam_hist ; 
run;

/************************************* 
	No proportionality assumption 
*************************************/
 
 proc logistic data=mam2 ;
 model mamm_exp_ = fam_hist /UNEQUALSLOPES  ; 
run;


/************************************* 
	Proportional odds , genmod
*************************************/

proc genmod; 
 model mamm_exp_ = fam_hist/ dist=multinomial link=clogit aggregate; 
run;
