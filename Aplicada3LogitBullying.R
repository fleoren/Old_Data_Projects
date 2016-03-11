setwd('/Users/Fernanda/Dropbox/Batmelon/CIMAT/captura')


# Script file: Parte logit Proyecto 2 Aplicada III

setwd('/Users/Fernanda/Dropbox/Aplicada3/Proyecto2/Bueno')

install.packages("Hmisc")
library(foreign)
library(MASS)
library(Hmisc)

#data <- read.spss('schools.sav')
#data <- read.xls('Titanic.xls')
datos95 <- spss.get('19951996_nuevaBIEN0,1.sav')
datos05 <- spss.get('20052006_nueva0,1.sav')

datos95$DEPR <- floor(datos95$DEPRESSED/3)
datos05$SATISF <- floor(datos05$SATISFACTION/3)

datos05$DEPR <- datos05$DEPRESSED
levels(datos05$DEPR)[4:5] <- c(0,0)
levels(datos05$DEPR)[1:3] <- c(1,1,1)
levels(datos05$DEPR) <- c("Yes", "No")


# 1995 ----------------------------------------------------------------------------------------------------------------
# Explicar sexo.
# GC: H=Very, F=12+, B=Never, D=Yes
#logitgender95 <- glm(GENDER ~ HEALTH + FIGHT + BULLSCHOOL + DEPRESSED, #family=binomial('logit'), data = datos95)
#betagender95 <- as.numeric(coefficients(logitgender))

# Explicar bulleados
# GC: G=Male, H=Very, E=None, L=A lot, P=None
logitbullied95 <- glm(BULLIED01 ~ GENDER + HEALTH + EXERCISE + LIKESCHOOL + PRESSURE, family=binomial('logit'), data=datos95)
betabullied95 <- as.numeric(coefficients(logitbullied95))
summary(logitbullied95)

# Explicar bullies.
# GC: G=Male, S=Never, A=Never, M=Never, L=A lot, P=Not at all, W=None
logitbully95 <- glm(BULLYS01 ~ GENDER + SMOKE + ALCOHOL + MARIJUANA + LIKESCHOOL + PRESSURE + WEAPON, family=binomial('logit'), data=datos95)
betabully95 <- as.numeric(coefficients(logitbully95))
summary(logitbully95)

# 2005 ----------------------------------------------------------------------------------------------------------------
# Explicar sexo.
# GC: H=Very, F=Never, B=Never, D=Yes
#logitgender05 <- glm(GENDER ~ HEALTH + FIGHT + BULLSCHOOL + DEPR, #family=binomial('logit'), data = datos05)
#betagender05 <- as.numeric(coefficients(logitgender))

# Explicar bulleados
# GC: G=Male, H=Very, E=None, L=A lot, P=None
logitbullied05 <- glm(BULLIED01 ~ GENDER + HEALTH + EXERCISE + LIKESCHOOL + PRESSURE, family=binomial('logit'), data=datos05)
betabullied05 <- as.numeric(coefficients(logitbullied05))
summary(logitbullied05)

# Explicar bullies.
# GC: G=Male, S=Never, A=Never, M=Never, L=A lot, P=Not at all, W=None
logitbully05 <- glm(BULLYS01 ~ GENDER + SMOKE + ALCOHOL + MARIJUANA + LIKESCHOOL + PRESSURE + WEAPON, family=binomial('logit'), data=datos05)
betabully05 <- as.numeric(coefficients(logitbully05))
summary(logitbully05)

a<-coefficients(logitbully95)
b<-coefficients(logitbully05)
c<-coefficients(logitbullied95)
d<-coefficients(logitbullied05)

write.table(a,'betalogitbully95.csv',sep=',')
write.table(b,'betalogitbully05.csv',sep=',')
write.table(c,'betalogitbullied95.csv',sep=',')
write.table(d,'betalogitbullied05.csv',sep=',')