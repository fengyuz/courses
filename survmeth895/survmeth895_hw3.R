sampsize=20
k=5
ybar=10
ssquare=5
nsimul=1000
result=matrix(0,nsimul,3)
for (i in 1:nsimul){
tmp=rnorm(sampsize-1)
chisq=sum(tmp*tmp)
sigmasq=(sampsize-1)*ssquare/chisq;
mu=ybar+sqrt(sigmasq/
sampsize)*rnorm(1)
ybark=mu+sqrt(sigmasq/k)*rnorm(1)
result[i,1]=sigmasq
result[i,2]=mu
result[i,3]=ybark
}
