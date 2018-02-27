libname bs653 "~/biostat653";

data ratuniv;
set bs653.rat;
time=0; wt=Y1; output;
time=1; wt=Y2; output;
time=2; wt=Y3; output;
time=3; wt=Y4; output;
time=4; wt=Y5; output;
drop Y1 Y2 Y3 Y4 Y5;
run;

proc sort data = ratuniv;
	by group time;
run;

proc means data = ratuniv noprint;
	var wt;
	by group time;
	output out = ratmean 
		Mean(wt) = meanWt;
run;

proc sgplot data = ratmean;
	series x = time y = meanWt / markers group = group;
run;

proc mixed data=ratuniv method=ml;
class ID Group(ref="1");
model wt=time Group*time/solution chisq outp=ratuniv_pred;
repeated/type=un subject=ID;
run;

proc sgplot data = ratuniv_pred;
	series x = time y = pred / markers group = group;
run;

data ratuniv_spline;
set ratuniv;
time_2 = max(time-2, 0);
run;

proc mixed data=ratuniv_spline method=ml;
class ID Group(ref="1");
model wt=time time_2 Group*time Group*time_2/solution chisq outp=ratuniv_spline_pred;
repeated/type=un subject=ID;
contrast "time_2 and Group*time_2" time_2 1, Group*time_2 1 -1 0, Group*time_2 1 0 -1/e;
run;

proc sgplot data = ratuniv_spline_pred;
	series x = time y = pred / markers group = group;
run;
