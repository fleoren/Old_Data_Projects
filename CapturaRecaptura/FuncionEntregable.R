FuncionEntregable<-function(N0,p0,c,Li,Ls,NumPuntos)
{
#Simulación de muestra
y1<-rbinom(1,N0,p0)
y2<-rbinom(1,y1,p0)
y3<-rbinom(1,N0-y1,p0)
Muestra<-c(y1,y2,y3)

#Optimización
#viN<-c(N0,p0)
#Opt<-nlm(FuncionMenosLogVeroBin,viN,Muestra)
Opt<-nlm(FuncionMenosLogVeroPerfilN,N0,Muestra)
EMV<-Opt$estimate

#Intervalo de verosimilitud de nivel c=0.15
a<-Li
b<-Ls
Raiz1<-uniroot(FuncionObjetivoEntregable,
               c(a,EMV[1]),
               muestra=Muestra,
               vc=c,
               MenosLogLmaxOpt=Opt$minimum)
LimiteInferior<-Raiz1$root
Raiz2<-uniroot(FuncionObjetivoEntregable,
               c(EMV[1],b),
               muestra=Muestra,
               vc=c,
               MenosLogLmaxOpt=Opt$minimum)
LimiteSuperior<-Raiz2$root


#Función de verosimilitud perfil relativa de N
ValoresN<-seq(a,b,length.out=NumPuntos)
MenosLogLmax<-c()
for(i in 1:length(ValoresN))
{
MenosLogLmax[i]<-FuncionMenosLogVeroPerfilN(ValoresN[i],Muestra)
}
RelativaN<-exp(Opt$minimum-MenosLogLmax)

#Gráfica de la verosimilitud perfil relativa
plot(ValoresN,RelativaN,type="l",lty=1,lwd=2,col=5,xlab="N",
     ylab="Verosimilitud Perfil Relativa",cex.lab=1.5,cex.axis=1.2,
     xlim=c(a,b)
     )
MenosLogLmaxN0<-FuncionMenosLogVeroPerfilN(N0,Muestra)
RelativaN0<-exp(Opt$minimum-MenosLogLmaxN0)
segments(N0,0,N0,RelativaN0,col="black",lty=1)
segments(LimiteInferior,c,LimiteSuperior,c,col="blue",lty=1)
segments(LimiteInferior,0,LimiteInferior,c,col="blue",lty=2)
segments(LimiteSuperior,0,LimiteSuperior,0.15,col="blue",lty=2)
text(EMV[1],c,"*",cex=2.5,col="red")
title(main=paste("Datos: y=(",
            Muestra[1],
            ",",
            Muestra[2],
            ",",
            Muestra[3],
            "), N=",
            N0,
            ", p=",
            p0),
       sub=paste("ENV=",
            round(EMV[1]),
            ", IV(",
            c,
            ")=[",
            round(LimiteInferior,0),
            ",",
            round(LimiteSuperior,0),
            "]"),
       cex.main=1.25,cex.sub=1.25
)
}

#source("FuncionMenosLogVeroBin.R",local = FALSE)
#source("FuncionMenosLogVeroPerfilN.R",local = FALSE)
#source("FuncionObjetivoEntregable.R",local = FALSE)
#FuncionEntregable(400,0.25,0.15,200,700,1000)


