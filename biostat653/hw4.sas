data exercise;
	infile "~/biostat653/exercise.txt";
	input id program y1 y2 y3 y4 y5 y6 y7;
run;

data exercise_uni;
	set exercise;
	time = 0; strength = y1; output;
	time = 2; strength = y2; output;
	time = 4; strength = y3; output;
	time = 6; strength = y4; output;
	time = 8; strength = y5; output;
	time = 10; strength = y6; output;
	time = 12; strength = y7; output;
	drop y1 y2 y3 y4 y5 y6 y7;
run;

proc sort data = exercise_uni;
	by program time;
run;

proc means data=exercise_uni noprint;
	var strength;
	by program time;
	output out=exercise_mean
		mean(strength) = strength_mean;
run;

proc sgplot data=exercise_mean;
	title "Mean strength over time by group";
  	styleattrs
		datacontrastcolors=(black) 
     	datalinepatterns=(dot solid);
	series x = time y = strength_mean / markers group = program;
run;

proc mixed data = exercise_uni;
	title "Random intercept only";
	class id program;
	model strength = program time time*program / solution;
	random intercept
		 / type=un subject=id g gcorr v vcorr solution;
run;

proc mixed data = exercise_uni;
	title "Random intercept and slope";
	class id program;
	model strength = program time time*program / solution;
	random intercept time
		 / type=un subject=id g gcorr v vcorr solution;
run;

data exercise_id24;
	set exercise_uni;
	if id = 24;
run;

proc reg data=exercise_id24;
	title "OLS for id=24";
	model strength = time;
run;
	