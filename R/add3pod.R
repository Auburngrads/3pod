add3pod <-
function(dtt,ylm,sim=F)
{
tf=F;
id=dtt$ID;
nid=length(id); 
x=dtt$X;
s1=c("r","I1\\(i\\)","I1\\(ii\\)","I1\\(iii\\)","I1\\(iv\\)","I2\\(ib\\)","I2\\(ic\\)","I2\\(id\\)","II1","II2","III1","III2");
s2=c("",rep("I1",4),rep("I2",3),rep("II",2),rep("III",2)); ns=length(s1);
for(i in 1:ns) id=gsub(s1[i],s2[i],id);
u=id[1]; vee=numeric(0);
# vee = index of the first test that isn't I1, i.e., vee=numeric(0) when you're still in I1
if(nid > 1){for(i in 2:nid)	if(id[i]!=u) {vee=c(vee,i); u=id[i];}}
nv=length(vee);
ul=c(vee-1,nid); ll=c(1,vee); 
ml=(ll+ul)/2; lab=unique(id);
iv=2; if(nv<=1)iv=1;

text(ml,rep(ylm[2],nv+1),lab,cex=.9);
lt=c(2,2); if(nv > 2) lt=c(lt,rep(4,nv-2)); lt=lt+1;
if(nv > 0)
{
	abline(v=vee-.5,lty=lt);
	j=m.update(dtt[1:(vee[iv]-1),]);
	M0=j$M0; 
	m1=j$m1; 
	w0=which(x==M0); 
	w1=which(x==m1);
		
	if(nv > 1)
	{
	lines(c(w0[[1]],vee[iv]-.5),c(M0,M0),col=3,lty=4); lines(c(w1[[1]],vee[iv]-.5),c(m1,m1),col=4,lty=4);
	} else
	{
		j=m.update(dtt); 
		M0=j$M0; 
		m1=j$m1; 
		w0=which(x==M0); 
		w1=which(x==m1);
		if(M0 > m1)
		{
		lines(c(w0[[1]],nid+1),c(M0,M0),col=3,lty=4); lines(c(w1[[1]],nid+1),c(m1,m1),col=4,lty=4);
		#tf=T
		}
	}
}
	kp=0;
	if(sim)
	{
	k=1;
	if(nv >= 1)k=vee[iv]-1;
	for(j in k:nid) {	jj=m.update(dtt[1:j,]); M0=jj$M0; m1=jj$m1; uv=c(M0,m1); if(any(is.na(uv))) break; if(M0 > m1) kp=j; if(kp > 0) break;	}
	}
return(c(tf,kp))
}
