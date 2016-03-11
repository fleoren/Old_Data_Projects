clear
clc

ph=0.005; %proba de contagio
pv=0.010; %proba de vacunación de un humano
pz= 0.005; %proba de recuperación (plus immunity) de infectado

H=1000;
Z=0.01*H;

H=H-Z;
totH(1)=H-Z; totZ(1)=Z; totI(1)=0; I=0;

tmax=500;


for t=2:1:tmax 

    for i=1:1:totH(t-1)
        x=rand;
        
        if x<pv
            H=H-1;
            I=I+1;
        end
        
        if and(x>pv,x<pv+ph)
            H=H-1;
            Z=Z+1;
        end
     
    end %primer renglón de la matriz P
    
    
    for j=1:1:totZ(t-1)
        x=rand;
        
        if x<pz
           Z=Z-1;
           I=I+1;
        end
    end %segundo renglón de la matriz P
   
% el tercer renglón de la matriz P tiene un estado absorbente, por lo
% tanto, no necesita código

if Z>0
totZ(t)=Z;
else
    Z=0;
    totZ(t)=0;
end

if H>0
totH(t)=H;
else
    H=0;
    totH(t)=0;
end

totI(t)=I;

end % código para la matriz P


subplot(3,1,1)
plot(0,0)
hold

for k=1:1:t
plot(k,totH(k),'*');
end

subplot(3,1,2)
plot(0,0)
hold
for k=1:1:t
plot(k,totZ(k),'*');
end

subplot(3,1,3)
plot(0,0)
hold
for k=1:1:t
plot(k,totI(k),'*');
end