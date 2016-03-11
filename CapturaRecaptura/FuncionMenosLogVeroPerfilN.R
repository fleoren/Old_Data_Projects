FuncionMenosLogVeroPerfilN<-function(N,y)
{
p<-sum(y)/(2*N)
MenosLogLmax<-FuncionMenosLogVeroBin(c(N,p),y)
return(MenosLogLmax)
}
