FuncionObjetivoEntregable<-function(N,muestra,vc,MenosLogLmaxOpt)
{
Constante<-MenosLogLmaxOpt-log(vc)
y<-FuncionMenosLogVeroPerfilN(N,muestra)-Constante
return(y)
}
