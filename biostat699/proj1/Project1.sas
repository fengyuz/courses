proc import out=proj1 
datafile="M:\Biostats 699\Project 1\Project 1 Data.xlsx"
dbms=xlsx replace;
sheet="Data";
getnames=yes;
run;

proc freq data=proj1;
tables race_eth educ_cat;
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

proc freq data=proj1;
format race race.
		ed ed.;
tables race ed;
run;

proc sort data=proj1;
by dyad_id male;
run;

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
merge proj1_sp (drop=_name_ _label_) proj1_tmp (drop=_name_ _label_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=age_;
by dyad_id;
id male;
var age;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_ _label_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=race_;
by dyad_id;
id male;
var race;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_ _label_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=ed_;
by dyad_id;
id male;
var ed;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_ _label_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=bmi_;
by dyad_id;
id male;
var bmi;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_ _label_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=sr_hlth_;
by dyad_id;
id male;
var sr_hlth;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_ _label_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=frail_2010_;
by dyad_id;
id male;
var frail_2010;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_ _label_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=frail_2012_;
by dyad_id;
id male;
var frail_2012;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (drop=_name_ _label_);
by dyad_id;
run;

proc transpose data=proj1 out=proj1_tmp prefix=income_;
by dyad_id;
id male;
var income;
run;

data proj1_sp;
merge proj1_sp proj1_tmp (rename=(income_1=income) drop=_name_ _label_ income_0);
by dyad_id;
run;

libname perm "M:\Biostats 699\Project 1\";
data perm.proj1_sp;
run;
















































data toe;
set "M:\Biostats 653\HW5\toenail.sas7bdat";
run;

*14.1;
proc genmod data=toe desc;
class id trt;
model y=month trt*month / dist=bin link=logit;
repeated subject=id/logor=exch;
run;

*14.4;
proc genmod data=toe desc;
class id trt (ref=FIRST);
model y=month trt*month / dist=bin link=logit;
repeated subject=id/type=exch;
run;

*14.5;
proc glimmix method=quad (qpoints=50) data=toe;
class id trt;
model y (event='1')= month month*trt /dist=bin link=logit s;
random intercept /subject=id type=un g gcorr;
run;

*14.10;
proc glimmix method=quad (qpoints=2) data=toe;
class id trt;
model y (event='1')= month month*trt /dist=bin link=logit s;
random intercept /subject=id type=un g gcorr;
run;
proc glimmix method=quad (qpoints=5) data=toe;
class id trt;
model y (event='1')= month month*trt /dist=bin link=logit s;
random intercept /subject=id type=un g gcorr;
run;
proc glimmix method=quad (qpoints=10) data=toe;
class id trt;
model y (event='1')= month month*trt /dist=bin link=logit s;
random intercept /subject=id type=un g gcorr;
run;
proc glimmix method=quad (qpoints=20) data=toe;
class id trt;
model y (event='1')= month month*trt /dist=bin link=logit s;
random intercept /subject=id type=un g gcorr;
run;
proc glimmix method=quad (qpoints=30) data=toe;
class id trt;
model y (event='1')= month month*trt /dist=bin link=logit s;
random intercept /subject=id type=un g gcorr;
run;
proc glimmix method=quad (qpoints=50) data=toe;
class id trt;
model y (event='1')= month month*trt /dist=bin link=logit s;
random intercept /subject=id type=un g gcorr;
run;









*4.2;
data HW4;
infile "M:\Biostats 653\HW4\exercise.txt";
input id group day0 day2 day4 day6 day8 day10 day12;
run;

data HW4_long;
set HW4;
day=0;
strength=day0;
output;
day=2;
strength=day2;
output;
day=4;
strength=day4;
output;
day=6;
strength=day6;
output;
day=8;
strength=day8;
output;
day=10;
strength=day10;
output;
day=12;
strength=day12;
output;
drop day0 day2 day4 day6 day8 day10 day12;
run;
proc sort data=HW4_long;
by day group;
run;
proc means data=HW4_long noprint;
by day group;
var strength;
output out=meansout (drop=_type_ _freq_) mean=mean stderr=stderr;
run; 
proc sort data=meansout;
by group;
run;
*4.1;
*reps;
symbol1 value = circle color = black interpol = join;
*wt;
symbol2 value = triangle color = black interpol = join;
proc gplot data=meansout;
plot mean*day=group /nolegend;
run;

*4.3;
proc mixed method=ml data=HW4_long;
class id group;
model strength = group day group*day/solution noint;
random intercept day /type=un subject=id g gcorr v vcorr solution;
repeated/ subject=id rcorr;
run;

*4.4;
proc mixed method=ml data=HW4_long;
class id group;
model strength = group day group*day/solution noint;
random intercept /type=un subject=id g gcorr v vcorr solution;
repeated/ subject=id rcorr;
run;
*-2logl_full=812.4
-2logl_red=872.8;

*4.8;
proc mixed method=ml data=HW4_long;
class id group;
model strength = group day group*day/solution noint outpred=HW4_pred;
random intercept day /type=un subject=id g gcorr v vcorr solution;
repeated/ subject=id rcorr;
run;

proc mixed method=ml data=HW4_long;
class id group;
model strength = group day group*day/solution noint;
random intercept day /type=un subject=id g gcorr v vcorr solution;
repeated/ subject=id rcorr;
ods listing exclude SolutionF;
ods output SolutionF=fixed1;
ods listing exclude SolutionR;
ods output SolutionR=rand1;
run;

data fixed1;
set fixed1;
keep group effect estimate;
run;

proc sort data=fixed1;
by group;
run;

data fixed12;
set fixed1;
by group;
retain fixint fixslope;
if effect="group" then fixint=estimate;
if effect="day*group" then fixslope=estimate;
if last.group then do;
output;
fixint=.;
fixslope=.;
end;
drop effect estimate;
run;

data fixed12;
set fixed12;
fixslope=fixslope+.1687;
run;

data rand1;
set rand1;
group=1;
if id>16 then group=2;
keep id group effect estimate;
run;

proc sort data=rand1;
by id;
run;

data rand12;
set rand1;
by id;
retain randint randslope;
if effect="Intercept" then randint=estimate;
if effect="day" then randslope=estimate;
if last.id then do;
output;
randint=.;
randslope=.;
end;
drop effect estimate;
run;

proc sort data=rand12;
by group id;
run;

data both1;
merge fixed12 rand12;
by group;
beta0i=fixint+randint;
beta1i=fixslope+randslope;
run;

proc print data=both1;
run;

*4.9;
data HW4_24;
set HW4_long;
if id=24;
group_day=group*day;
run;

proc glm data=HW4_24;
model strength = day;
run;




















data rat;
set "M:\Biostats 653\HW3\rat.sas7bdat";
rename y1=base y2=week1 y2=week1 y3=week2 y4=week3 y5=week4;
run;

*6.2;
data rat_long;
set rat;
time=0;
wt=base;
output;
time=1;
wt=week1;
output;
time=2;
wt=week2;
output;
time=3;
wt=week3;
output;
time=4;
wt=week4;
output;
drop base week1 week2 week3 week4;
run;

*6.1;
proc sort data=rat_long;
by time group;
run;
proc means data=rat_long noprint;
by time group;
var wt;
output out=meansout (drop=_type_ _freq_) mean=mean stderr=stderr;
run; 
proc sort data=meansout;
by group;
run;
*control;
symbol1 value = circle color = black interpol = join;
*thiouracil;
symbol2 value = triangle color = black interpol = join;
*thyroxin;
symbol3 value = square color = black interpol = join;
proc gplot data=meansout;
plot mean*time=group /nolegend;
run;

*6.3;
proc mixed data=rat_long;
class id group (ref=first);
model wt=time group time*group/solution chisq outpm=rat_results;
repeated/type=un subject=id r rcorr;
run;

*6.4;
proc sort data=rat_results;
by group;
run;
symbol1 value = circle color = black interpol = join;
*thiouracil;
symbol2 value = triangle color = black interpol = join;
*thyroxin;
symbol3 value = square color = black interpol = join;
proc gplot data=rat_results;
plot pred*time=group /nolegend;
run;

*6.6;
data rat_long;
set rat_long;
time_2=max(time-2,0);
run;
*test if rate of change differs among groups;
proc mixed data=rat_long;
class id group (ref=first);
model wt=time time_2 group time*group time_2*group/solution chisq;
repeated/type=un subject=id r rcorr;
contrast "interaction" time*group 0 1 -1, time*group 1 -1 0, time_2*group 0 -1 1, time_2*group 1 -1 0;
run;

*6.7;
proc mixed data=rat_long method=ml;
class id group (ref=first);
model wt=time group time*group/solution chisq;
repeated/type=un subject=id r rcorr;
run;
proc mixed data=rat_long method=ml;
class id group (ref=first);
model wt=time time_2 group time*group time_2*group/solution chisq;
repeated/type=un subject=id r rcorr;
run;
*-2logl_red=809.4;
*-2logl_full=825.6;
*2logl_full-2logl_red=825.6-809.4=16.2 with 3 df;

*6.8;
*pairwise tests;
proc mixed data=rat_long;
class id group (ref=first);
model wt=time time_2 group time*group time_2*group/solution chisq;
repeated/type=un subject=id r rcorr;
constrast 'grp1 v grp2' time*group 1 0 -1 time_2*group 1 0 -1/e chisq;
constrast 'grp1 v grp3' time*group 0 1 -1 time_2*group 0 1 -1/e chisq;
constrast 'grp2 v grp3' time*group 1 -1 0 time_2*group 1 -1 0/e chisq;
run;













*5.1;
data HW2;
	infile "M:\Biostats 653\HW2\cholesterol.txt";
	input treat id chol0 chol6 chol12 chol20 chol24;
	newid=_N_;
	run;
*5.2;
proc means data=HW2 mean std var;
by treat;
var chol0 chol6 chol12 chol20 chol24;
run;
*5.4;
data HW2_long;
set HW2;
time=0;
chol=chol0;
output;
time=6;
chol=chol6;
output;
time=12;
chol=chol12;
output;
time=20;
chol=chol20;
output;
time=24;
chol=chol24;
output;
drop chol0 chol6 chol12 chol20 chol24;
run;

proc sort data=HW2_long;
by time treat;
run;
*5.3;
proc means data=HW2_long noprint;
by time treat;
var chol;
output out=meansout (drop=_type_ _freq_) mean=mean stderr=stderr;
run; 

proc sort data=meansout;
by treat;
run;

goptions reset=global;

symbol1 value = circle color = black interpol = join;
symbol2 value = triangle color = blue interpol = join;
proc gplot data=meansout;
plot mean*time=treat /nolegend;
run;
*5.5;
proc mixed data=HW2_long;
class newid treat time;
model chol=time treat time*treat/solution chisq;
repeated/type=un subject=newid r rcorr;
run;

proc mixed data=HW2_long;
class newid treat time;
model chol=time*treat/noint solution;
repeated/type=un subject=newid r rcorr;
contrast 'interaction' time*treat 1 -1 0 0 0 -1 1 0 0 0, time*treat 1 0 -1 0 0 -1 0 1 0 0, 
time*treat 1 0 0 -1 0 -1 0 0 1 0, time*treat 1 0 0 0 -1 -1 0 0 0 1/e chisq;
run;
*5.7;
proc mixed data=HW2_long;
class newid treat (ref=LAST) time (ref=FIRST);
model chol=time treat time*treat/solution;
repeated/type=un subject=newid r rcorr;
run;
*5.8;
proc mixed data=HW2_long;
class newid treat (ref=LAST) time (ref=FIRST);
model chol=time*treat/noint solution;
repeated/type=un subject=newid r rcorr;
contrast 'interaction' time*treat 1 -1 0 0 0 -1 1 0 0 0, time*treat 1 0 -1 0 0 -1 0 1 0 0, 
time*treat 1 0 0 -1 0 -1 0 0 1 0, time*treat 1 0 0 0 -1 -1 0 0 0 1/e chisq;
run;






proc reg data=HW1_6;
model IQ=age;
run;

proc anova data=HW1_6;
class lead_exp;
model age=lead_exp;
run;

proc reg data=HW1_6;
model IQ=lead_cur lead_prev sex lead_curXsex lead_prevXsex age;
run;

proc reg data=HW1_6;
model IQ=lead_cur lead_prev sex lead_curXsex lead_prevXsex /vif;
run;

*HW1.7;

data HW1_7;
	infile "M:\Biostats 653\HW1\lead.txt";
	input ID lead_0 lead_1 lead_4 lead_6;
	run;

*HW1.7a;
proc means data=HW1_7 mean std var;
var lead_0 lead_1 lead_4 lead_6;
run; 

*HW1.7b;
data HW1_7_long;
set HW1_7;
time=0;
lead=lead_0;
output;
time=1;
lead=lead_1;
output;
time=4;
lead=lead_4;
output;
time=6;
lead=lead_6;
output;
run;

symbol1 value = circle color = black interpol = join repeat = 50;
proc gplot data=HW1_7_long;
plot lead*time = ID / nolegend;
run;

proc sort data=HW1_7_long;
by time;
run;

proc means data=HW1_7_long noprint;
by time;
var lead;
output out=meansout (drop=_type_ _freq_) mean=mean stderr=stderr;
run; 

proc gplot data=meansout;
plot mean*time /nolegend;
run;

*HW1.7c;
proc corr data=HW1_7 cov;
var lead_0 lead_1 lead_4 lead_6;
run;























data HW6;
 infile "C:\Users\smithcat\Desktop\HW 6\Adelaide.txt";
 input year dept $ survivors total;
 year_2dig=year-1900;
 art=(dept="ART");
 med=(dept="MED");
 eng=(dept="ENG");
run;

proc import datafile = "Adelaide_ind.xlsx"
out=HW6 dbms=xlsx replace;
getnames=yes; 
run;

data HW3;
set HW3;

do j=1 to survivors;
id=j; 
Y=1;
output;
end;

do j=1 to (total-survivors); 
Y=0;
id=j+survivors; 
output;
end;

drop total survivors;

run;

proc iml;
use HW3;
read all var {Y} into Y;
read all var {year_2dig art med eng} into X;

n=nrow(Y);
one_n=j(n,1,1);
X=one_n||X;
q=ncol(X);
beta=j(q,1,0);
tol=.00001;
epsilon=99;
j_max=30;
j=0;

do while (epsilon>tol & j<=j_max);
eta=X*beta;
mu=exp(eta)/(1+exp(eta));
v=mu*T(1-mu);
V=diag(v);
Z=eta+inv(V)*(Y-mu);
beta_new=inv(T(X)*V*X)*T(X)*V*Z;
epsilon=sqrt(T(beta_new-beta)*(beta_new-beta));
beta=beta_new;
j=j+1;
beta_t=t(beta);
print j beta_t epsilon;
end;

I=t(X)*V*X;
V_beta = inv(I);
SE = sqrt(vecdiag(V_beta));
wald_a = (beta/SE)#(beta/SE);
p_wald_a=1-probchi(wald_a,1);
print SE wald_a p_wald_a;

**WALD;
C={0 0 1 0 0,
   0 0 0 1 0,
   0 0 0 0 1};
wald_b=T(C*beta)*inv(C*inv(I)*T(C))*C*beta;
p_wald_b=1-probchi(wald_b,3);
print wald_b p_wald_b;

**SCORE;
proc iml;
use HW3;
read all var {Y} into Y;
read all var {year_2dig art med eng} into X;

n=nrow(Y);
one_n=j(n,1,1);
X=one_n||X;
X0=X[,1:2];
q0=ncol(X0);
beta0=j(q0,1,0);
tol=.00001;
epsilon=99;
j_max=30;
j=0;

do while (epsilon>tol & j<=j_max);
eta0=X0*beta0;
mu0=exp(eta0)/(1+exp(eta0));
v0=mu0*T(1-mu0);
V0=diag(v0);
Z0=eta0+inv(V0)*(Y-mu0);
beta_new0=inv(T(X0)*V0*X0)*T(X0)*V0*Z0;
epsilon=sqrt(T(beta_new0-beta0)*(beta_new0-beta0));
beta0=beta_new0;
j=j+1;
beta_t0=t(beta0);
print j beta_t0 epsilon;
end;

beta0_all=beta0//j(3,1,0);
eta0_=X*beta0_all;
mu0_=exp(eta0_)/(1+exp(eta0_));
U0=T(X)*(Y-mu0_);
v0_=mu0_*T(1-mu0_);
V0_=diag(v0_);
J0=T(X)*V0_*X;
score=T(U0)*inv(J0)*U0;
p_score=1-probchi(score,3);
print score p_score;


I=t(X)*V*X;
V_beta = inv(I);
SE = sqrt(vecdiag(V_beta));
wald_a = (beta/SE)#(beta/SE);
p_wald_a=1-probchi(wald_a,1);
print SE wald_a p_wald_a;

*1d**;
proc logistic data=HW3;
model Y = year_2dig art med eng; 
run;
*-2logL=884.03;

proc logistic data=HW3;
model Y = year_2dig; 
run;
*-2logL=902.04;

*2c;
proc genmod data=HW3;
model Y = year_2dig art med eng art*year_2dig med*year_2dig eng*year_2dig /dist=bin;
contrast "2c" art*year_2dig 1, med*year_2dig 1, eng*year_2dig 1;
run;

*3a;
*data HW6;
*set HW6;
 *sci=(dept="SCI");
 *art=(dept="ART");
 *med=(dept="MED");
 *eng=(dept="ENG");
 *y38=(year=1938);
 *y39=(year=1939);
 *y40=(year=1940);
 *y41=(year=1941);
 *y42=(year=1942);
 *y43=(year=1943);
 *y44=(year=1944);
 *y45=(year=1945);
 *y46=(year=1946);
 *y47=(year=1947);
*run;

*proc genmod data=HW6;
*model survivors/total = art med eng sci y38 y39 y40 y41 y42 y43 y44 y45 y46 y47 /dist=bin noint;
*run;

*proc genmod data=HW6;
*model survivors/total = art med eng sci y38 y39 y40 y41 y42 y43 y44 y45 y46 y47 art*med
art*eng art*sci art*y38/dist=bin;
*run;

proc genmod data=HW6;
class dept year_2dig /param=ref ref=first;
model survivors/total = dept year_2dig dept*year_2dig/dist=bin;
run;

proc genmod data=HW6;
class dept /param=ref ref=first;
model survivors/total = dept year_2dig dept*year_2dig/dist=bin;
run;

proc genmod data=HW6;
model survivors / total = year_2dig art med eng /dist=bin; 
run;

proc logistic data=HW3;
model Y = year_2dig art med eng; */ lackfit influence rsq;
run;

proc genmod data=HW3;
model Y = year_2dig art med eng / dist=bin;
contrast "Wald" art 1, med 1, eng 1 / wald;
run;

proc logistic data=HW6;
model survive = year_2dig art med eng;
run;

proc genmod data=HW3 plots=(reschi(xbeta) resdev(xbeta) leverage cooksd);
model survivors / total = year_2dig art med eng / dist=bin influence;
output out=HW3 xbeta=eta leverage=leverage reschi=reschi resdev=resdev stdreschi=stdreschi
stdresdev=stdresdev COOKSD=cooksd hesswgt=w;
run;

proc reg data=hw3 plots=none;
weight W;
model survivors = year_2dig art med eng / vif;
run;
