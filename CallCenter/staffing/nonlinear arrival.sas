/*          Workdays            */
/*          Normal            */
PROC IMPORT OUT= WORK.A 
            DATAFILE= "C:\Users\huangke.PHIBRED\Desktop\dat.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
proc nlmixed data=a tech=newrap maxit=100000 maxfu=100000 ;  
bounds s1>0, s2>0;
parms A1=140, mu1=10, s1=.9, A2=500, mu2=15, s2=2;
pi=constant("pi");
meangrp = A1/(2*pi)**(1/2)/s1*exp(-(hour-mu1)**2/(2*s1**2))+A2/(2*pi)**(1/2)/s2*exp(-(hour-mu2)**2/(2*s2**2));
model arrivals ~ normal(meangrp, s2e);
run;
proc nlmixed data=a tech=newrap maxit=100000 maxfu=100000 ;  
bounds s1>0, s2>0, s3>0;
parms A1=350, mu1=10, s1=1.3, A2=190, mu2=14.5, s2=2, A3=800, mu3=19, s3=6;
pi=constant("pi");
meangrp = A1/(2*pi)**(1/2)/s1*exp(-(hour-mu1)**2/(2*s1**2))
         +A2/(2*pi)**(1/2)/s2*exp(-(hour-mu2)**2/(2*s2**2))
         +A3/(2*pi)**(1/2)/s3*exp(-(hour-mu3)**2/(2*s3**2));
model arrivals ~ normal(meangrp, s2e);
    predict arrivals - meangrp  out=r;
run;
proc sql ;
 create table plot as
 select *,
 std(pred) as stdresid format = 8.5
 from r;
 quit; 
data plot; set plot; predict=arrivals-pred; studentresid=pred/stdresid; run;
   title"Studentized Residuals for # of Arrivals";
proc sgplot data=plot;
    scatter x=predict y =studentresid/markerattrs=(symbol=circle color=black size=10);
    refline 0;
    yaxis label = "Residual";
    xaxis label = "Predicted";
run;



/*          MONDAYS            */
/*          Normal            */
PROC IMPORT OUT= WORK.A1 
            DATAFILE= "C:\Users\huangke.PHIBRED\Desktop\dat1.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;


proc nlmixed data=a tech=newrap maxit=100000 maxfu=100000 ;  
bounds s1>0, s2>0;
parms A1=140, mu1=10, s1=.9, A2=500, mu2=15, s2=2;
pi=constant("pi");
meangrp = A1/(2*pi)**(1/2)/s1*exp(-(hour-mu1)**2/(2*s1**2))+A2/(2*pi)**(1/2)/s2*exp(-(hour-mu2)**2/(2*s2**2));
model arrivals ~ normal(meangrp, s2e);
run;

proc nlmixed data=a tech=newrap maxit=100000 maxfu=100000 ;  
bounds s1>0, s2>0;
pi=constant("pi");
*meangrp = 1/(2*pi)**(1/2)/s1*exp(-(hour-mu1)**2/(2*s1**2))+1/(2*pi)**(1/2)/s2*exp(-(hour-mu2)**2/(2*s2**2));
meangrp = 1/(2*pi)**(1/2)/(s1+us1)*exp(-(hour-(mu1+umu1))**2/(2*(s1+us1)**2))
         +1/(2*pi)**(1/2)/(s2+us2)*exp(-(hour-(mu2+umu2))**2/(2*(s2+us2)**2));
model arrivals ~ normal(meangrp, s2e);
random us1 us2 umu1 umu2 ~ normal([0,0,0,0],
[s2s1,
 0   ,s2s2,
 0   ,0   ,s2mu1,
 0   ,0   ,0    ,s2mu2]) subject=day;
run;

PROC IMPORT OUT= WORK.A2 
            DATAFILE= "C:\Users\huangke.PHIBRED\Desktop\dat2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
proc nlmixed data=a2 tech=newrap maxit=100000 maxfu=100000 ;  
bounds s1>0, s2>0;
parms A1=140, mu1=10, s1=.9, A2=500, mu2=15, s2=2;
pi=constant("pi");
meangrp = A1/(2*pi)**(1/2)/s1*exp(-(hour-mu1)**2/(2*s1**2))+A2/(2*pi)**(1/2)/s2*exp(-(hour-mu2)**2/(2*s2**2));
model arrivals ~ normal(meangrp, s2e);
run;
