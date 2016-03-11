
FuncionMenosLogVeroBin<-function(VectorParametros,y)
{
N<-VectorParametros[1]
p<-VectorParametros[2]
if(N>=(y[1]+y[3]) & p>0 & p<1)
 {
 MenosLogL<--(sum(y)*log(p)+(2*N-sum(y))*log(1-p)+lgamma(N+1)-lgamma(N-y[1]-y[3]+1))
 }
else
 {
 MenosLogL<-10^100
 }
return(MenosLogL)
}
