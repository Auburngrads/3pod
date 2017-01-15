xyllik <-
function(rx,ry,m,s)
{
kx=rx-m;
ns=length(s);
ll=numeric(0);
for(i in 1:ns)
{
pms=(2*ry-1)*s;
ll=c(ll,sum(log(pnorm(kx/pms))));
}
return(ll);
}
