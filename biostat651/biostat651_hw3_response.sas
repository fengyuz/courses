data copd;
	input cases subjects smoking polluted;
	cards;
20      50      0       0
10      50      0       1
30      50      1       0
32      50      1       1 
;
run;

data adelaide;
	infile "~/Adelaide.txt";
	input year dept $ survivors total;
	deptArt=(dept="ART");
	deptMed=(dept="MED");
	deptEng=(dept="ENG");
	year1900=year-1900;
	rate=survivors/total;
run;

title "Full model";

proc genmod data=copd;
	model cases / subjects=smoking polluted smoking*polluted / dist=binomial 
		link=logit;
run;

title "Reduced model";

proc genmod data=copd;
	model cases / subjects=smoking / dist=binomial link=logit;
run;

title "Adelaide";

proc genmod data=adelaide  plots=(RESCHI(XBETA) RESDEV(XBETA) LEVERAGE DOBS);
 model survivors / total=year1900 deptArt deptMed deptEng / dist=binomial link=logit;
run;


title2 "Calculate VIF";
proc reg data=adelaide plots=none;
        model rate=year1900 deptArt deptMed deptEng / vif;
run;

proc logistic data=adelaide;
	model survivors / total=year1900 deptArt deptMed deptEng;
run;

proc logistic data=adelaide;
	model survivors / total=;
run;

/* proc reg data=adelaide; */
/* model survivors = year1900 deptArt deptMed deptEng / vif collin; */
/* run; */