

OPTIONS NODATE NOCENTER LS=70 PS=70;




data death1;
 input i Y;
datalines;
1   10
2   12
3   8
4  7
5  2
6    4
7   15
8  6
9  5
10  19
 ;
run;

proc means data=death1; 
 var i Y;
run;


proc iml;
 use death1;
 read all var {i} into idno;
 read all var {Y} into Y;
 q=2;
 n=nrow(Y);
 max_iter=25;
 theta=j(2,1,0);
 theta[1]=log(n/sum(Y));
 theta[2]=0; 
 
 tol=10**(-5);

 U_=j(2,1,0);
 J_=j(2,2,0);

 diff=100;

 j=0;
 print j theta;
 do while ((j <= max_iter) & (diff > tol));
   a=exp(theta[2]);
   b=exp(theta[1]);
   U_=j(2,1,0);
   J_=j(2,2,0);
   log_L=0;

   do i=1 to n;
    U_[1] = U_[1] + 1 - b*(Y[i]**a);
	U_[2] = U_[2] + 1 + a*log(Y[i]) - b*a*log(Y[i])*(Y[i]**a);
    J_[1,1] = J_[1,1] + b*(Y[i]**a);
    J_[2,2] = J_[2,2] -a*log(Y[i]) + b*a*log(Y[i])*(Y[i]**a)*(1+a*log(Y[i]));
    J_[2,1] = J_[2,1] + b*a*log(Y[i])*(Y[i]**a);
    J_[1,2]=J_[2,1];
    log_L =  log_L + theta[1] + theta[2] + (a-1)*log(Y[i]) - b*Y[i]**a;
   end;
    
   theta_new = theta + inv(J_)*U_;
   diff = sqrt(t(theta_new-theta)*(theta_new-theta));
   theta=theta_new;
   U_length=sqrt(t(U_)*U_);
   j=j+1;
   print j theta_new diff U_length log_L;
 
 end;

 SE_theta=sqrt(vecdiag(inv(J_)));
 print SE_theta;

 beta=theta[1];
 beta_L=beta-1.96*SE_theta[1];
 beta_U=beta+1.96*SE_theta[1];

 alpha=theta[2];
 alpha_L=alpha-1.96*SE_theta[2];
 alpha_U=alpha+1.96*SE_theta[2];

 print beta beta_L beta_U;
 print alpha alpha_L alpha_U;

 lambda=exp(beta); lambda_L=exp(beta_L); lambda_U=exp(beta_U);
 gamma=exp(alpha); gamma_L=exp(alpha_L); gamma_U=exp(alpha_U);

 print lambda lambda_L lambda_U;
 print gamma gamma_L gamma_U;

quit;




