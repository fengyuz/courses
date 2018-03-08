proc import out=babies
     datafile = "~/biostat699_sas/proj3/babies.csv"
     dbms=csv
     replace;
     getnames=yes;
run;

proc mixed data = babies;
	title "Random intercept and slope";
	class partid MeetsSatFatRec MeetsAddedSugarRec MeetsFruitRec MeetsRedMeatRec MeetsHSFSRec MeetsLegumRec MDQ_5_or_more;
	model measure = WeeksSinceLMP WeeksSinceLMPsq WeeksSinceLMPcb
		/ solution noint;
	random WeeksSinceLMP WeeksSinceLMPsq
		/ subject=partid type=un solution;
run;

/*
proc mixed data = babies;
	title "Random intercept and slope";
	class partid MeetsSatFatRec MeetsAddedSugarRec MeetsFruitRec MeetsRedMeatRec MeetsHSFSRec MeetsLegumRec MDQ_5_or_more;
	model measure = WeeksSinceLMP WeeksSinceLMPsq 
		/ solution noint;
	repeated / subject=partid type=un;
run;

proc mixed data = babies;
	class partid MeetsSatFatRec MeetsAddedSugarRec MeetsFruitRec MeetsRedMeatRec MeetsHSFSRec MeetsLegumRec MDQ_5_or_more;
	* model measure = WeeksSinceLMP WeeksSinceLMPsq WeeksSinceLMP*MeetsFruitRec WeeksSinceLMP*MeetsRedMeatRec WeeksSinceLMP*MeetsHSFSRec WeeksSinceLMP*MeetsLegumRec WeeksSinceLMP*MeetsSatFatRec WeeksSinceLMP*MeetsAddedSugarRec/ solution noint;
	model measure = WeeksSinceLMP WeeksSinceLMPsq / solution noint;
	random WeeksSinceLMP
		 / type=un subject=partid solution;
run;

proc genmod data=babies;
	class partid MDQ_5_or_more BMI_CAT;
	model measure = WeeksSinceLMP WeeksSinceLMPsq
		/ noint dist=normal;
run;




     
	
