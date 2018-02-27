/* data tumor; */
/*  input treatment$ sex$ response$ frequency; */
/*  datalines; */
/* sequential male progressive 28 */
/* sequential male no_change 45 */
/* sequential male partial_remission 29 */
/* sequential male complete_remission 26 */
/* sequential female progressive 4 */
/* sequential female no_change 12 */
/* sequential female partial_remission 5 */
/* sequential female complete_remission 2 */
/* alternating male progressive 41 */
/* alternating male no_change 44 */
/* alternating male partial_remission 20 */
/* alternating male complete_remission 20 */
/* alternating female progressive 12 */
/* alternating female no_change 7 */
/* alternating female partial_remission 3 */
/* alternating female complete_remission 1 */
/* ; */
/* run; */


/* Generated Code (IMPORT) */
/* Source File: tumor.xls */
/* Source Path: /folders/myfolders */
/* Code generated on: 3/30/17, 9:21 PM */

%web_drop_table(tumor);


FILENAME REFFILE '/folders/myfolders/tumor.xls';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLS
	OUT=tumor;
	Range="Sheet1$A1:D17";
	GETNAMES=YES;
RUN;


data tumor2;
 set tumor;
 response_num=0*(response="progressive")+1*(response="no_change")+2*(response="partial_remission")+3*(response = "complete_remission");
 treatment_num = 0 * (treatment = "sequential") + 1 * (treatment = "alternating");
 sex_num = 0 * (sex = "male") + 1 * (sex = "female");
run;

proc genmod data=tumor2;
      freq frequency;
      model response_num = treatment_num sex_num / dist=multinomial
                            link=cumlogit
                            aggregate;
   run;




%web_open_table(tumor);




/*  */
/* proc genmod data = tumor;  */
/*  freq response_num; */
/*  class treatment_num sex_num */
/*  model response_num = treatment_num sex_num / dist=multinomial link=clogit aggregate;  */
/* run; */




%web_open_table(tumor);