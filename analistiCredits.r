# Carrega de les llibreries emprades=========================================
library(ggplot2)

# Carrega del dataset========================================================
credit_ds <- read.table(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data") # nolint
attach(credit_ds)

colnames(credit_ds) <- c("estatCompteCorrent", "mesosCredit",
                         "historiaCreditsAnteriors",
                         "motiu", "quantitat", "estatCompteEstalvi",
                         "tempsTreballActual", "percentRentaDedicat",
                         "sexeEstatCivil", "altresDeutors",
                         "tempsResidenciaActual", "propietats",
                         "edat", "altresPlans",
                         "propietatVivendaActual", "numCredits",
                         "tipusFeina",
                         "numPersonesManteniment", "telefon",
                         "estranger", "bonPagador")

files <- dim(credit_ds)
files

# ANALISI PRELIMINAR =============================================================
summary(credit_ds)

# Analitzant la informacio obtinguda de l'aplicacio de la funcio summary, es poden comprovar els valors mitjans, les medianes i quartils.
# de les diferents variables del dataset, d'aquesta analisi preliminar es poden extreure les seguents conclusions:
#   * El valor maxim de la variable quantitat es troba a una distancia relativament superior de la mediana i la mitjana que el valor manim, aixo pot indicar l'existencia d'outliers
#   * La distribucio de valors de les variables edat i mesosCredit també pot fer pensar que hi puguin haver outliers.
#   * El camp 'bonPagador' esta definit com a numeric (1-bon pagador i 2-mal pagador), per tant, es podria posar en termes de 0 i 1, per ser convertit a factor.

# Converteix els valors de bon pagador a 0(FALS) i 1(VERTADER: bon pagador)===
credit_ds$bonPagador[credit_ds$bonPagador==2] <-0

# Converteix els valors numerics a factors====================================
credit_ds$bonPagador <- as.factor(credit_ds$bonPagador)
summary(credit_ds$bonPagador)

#Converteix les codificacions a valors========================================
# Estat compte corrent
credit_ds$estatCompteCorrent[credit_ds$estatCompteCorrent=='A11']<-'< 0DM'
credit_ds$estatCompteCorrent[credit_ds$estatCompteCorrent=='A12']<-'[0DM,200DM]'
credit_ds$estatCompteCorrent[credit_ds$estatCompteCorrent=='A13']<-'>= 200DM'
credit_ds$estatCompteCorrent[credit_ds$estatCompteCorrent=='A14']<-'no checking account'
credit_ds$estatCompteCorrent <- as.factor(credit_ds$estatCompteCorrent)

#Historia de credits anterior
credit_ds$historiaCreditsAnteriors[credit_ds$historiaCreditsAnteriors=='A30']<-'no credits taken/all credits paid back duly'
credit_ds$historiaCreditsAnteriors[credit_ds$historiaCreditsAnteriors=='A31']<-'all credits at this bank paid back duly'
credit_ds$historiaCreditsAnteriors[credit_ds$historiaCreditsAnteriors=='A32']<-'existing credits paid back duly till now'
credit_ds$historiaCreditsAnteriors[credit_ds$historiaCreditsAnteriors=='A33']<-'delay in paying off in the past'
credit_ds$historiaCreditsAnteriors[credit_ds$historiaCreditsAnteriors=='A34']<-'critical account/other credits existing (not at this bank)'
credit_ds$historiaCreditsAnteriors <- as.factor(credit_ds$historiaCreditsAnteriors)

#Motiu del credit
credit_ds$motiu[credit_ds$motiu=='A40']<-'car (new)'
credit_ds$motiu[credit_ds$motiu=='A41']<-'car (used)'
credit_ds$motiu[credit_ds$motiu=='A42']<-'furniture/equipment'
credit_ds$motiu[credit_ds$motiu=='A43']<-'radio/television'
credit_ds$motiu[credit_ds$motiu=='A44']<-'domestic appliances'
credit_ds$motiu[credit_ds$motiu=='A45']<-'repairs'
credit_ds$motiu[credit_ds$motiu=='A46']<-'education'
credit_ds$motiu[credit_ds$motiu=='A47']<-'(vacation - does not exist?)'
credit_ds$motiu[credit_ds$motiu=='A48']<-'retraining'
credit_ds$motiu[credit_ds$motiu=='A49']<-'business'
credit_ds$motiu[credit_ds$motiu=='A410']<-'others'
credit_ds$motiu <- as.factor(credit_ds$motiu)

#Estat compte estalvi
credit_ds$estatCompteEstalvi[credit_ds$estatCompteEstalvi=='A61']<-'< 100 DM'
credit_ds$estatCompteEstalvi[credit_ds$estatCompteEstalvi=='A62']<-'[100DM,500DM)'
credit_ds$estatCompteEstalvi[credit_ds$estatCompteEstalvi=='A63']<-'[500DM,1000DM)'
credit_ds$estatCompteEstalvi[credit_ds$estatCompteEstalvi=='A64']<-'>= 1000 DM'
credit_ds$estatCompteEstalvi[credit_ds$estatCompteEstalvi=='A65']<-'unknown/ no savings account'
credit_ds$estatCompteEstalvi <- as.factor(credit_ds$estatCompteEstalvi)

#Temps en el lloc de feina actual
credit_ds$tempsTreballActual[credit_ds$tempsTreballActual=='A71']<-'unemployed'
credit_ds$tempsTreballActual[credit_ds$tempsTreballActual=='A72']<-'< 1 year'
credit_ds$tempsTreballActual[credit_ds$tempsTreballActual=='A73']<-'[1 year,4 years)'
credit_ds$tempsTreballActual[credit_ds$tempsTreballActual=='A74']<-'[4 year,7 years)'
credit_ds$tempsTreballActual[credit_ds$tempsTreballActual=='A75']<-'>= 7 years'
credit_ds$tempsTreballActual <- as.factor(credit_ds$tempsTreballActual)

#Sexe i estat civil
credit_ds$sexeEstatCivil[credit_ds$sexeEstatCivil=='A91']<-'male : divorced/separated'
credit_ds$sexeEstatCivil[credit_ds$sexeEstatCivil=='A92']<-'female : divorced/separated/married'
credit_ds$sexeEstatCivil[credit_ds$sexeEstatCivil=='A93']<-'male : single'
credit_ds$sexeEstatCivil[credit_ds$sexeEstatCivil=='A94']<-'male : married/widowed'
credit_ds$sexeEstatCivil[credit_ds$sexeEstatCivil=='A95']<-'female : single'
credit_ds$sexeEstatCivil <- as.factor(credit_ds$sexeEstatCivil)

#Altres deutors
credit_ds$altresDeutors[credit_ds$altresDeutors=='A101']<-'none'
credit_ds$altresDeutors[credit_ds$altresDeutors=='A102']<-'co-applicant'
credit_ds$altresDeutors[credit_ds$altresDeutors=='A103']<-'guarantor'
credit_ds$altresDeutors <- as.factor(credit_ds$altresDeutors)

#Propietats
credit_ds$propietats[credit_ds$propietats=='A121']<-'real estate'
credit_ds$propietats[credit_ds$propietats=='A122']<-'building society savings agreement/ life insurance'
credit_ds$propietats[credit_ds$propietats=='A123']<-'car or other'
credit_ds$propietats[credit_ds$propietats=='A124']<-'unknown/no property'
credit_ds$propietats <- as.factor(credit_ds$propietats)

#Altres plans
credit_ds$altresPlans[credit_ds$altresPlans=='A141']<-'bank'
credit_ds$altresPlans[credit_ds$altresPlans=='A142']<-'stores'
credit_ds$altresPlans[credit_ds$altresPlans=='A143']<-'none'
credit_ds$altresPlans <- as.factor(credit_ds$altresPlans)

#Vivenda actual
credit_ds$propietatVivendaActual[credit_ds$propietatVivendaActual=='A151']<-'rent'
credit_ds$propietatVivendaActual[credit_ds$propietatVivendaActual=='A152']<-'own'
credit_ds$propietatVivendaActual[credit_ds$propietatVivendaActual=='A153']<-'for free'
credit_ds$propietatVivendaActual <- as.factor(credit_ds$propietatVivendaActual)

#Feina
credit_ds$tipusFeina[credit_ds$tipusFeina=='A171']<-'unemployed/ unskilled - non-resident'
credit_ds$tipusFeina[credit_ds$tipusFeina=='A172']<-'unskilled - resident'
credit_ds$tipusFeina[credit_ds$tipusFeina=='A173']<-'skilled employee / official'
credit_ds$tipusFeina[credit_ds$tipusFeina=='A174']<-'management/ self-employed/highly qualified employee/ officer'
credit_ds$tipusFeina <- as.factor(credit_ds$tipusFeina)

#Telefon
credit_ds$telefon[credit_ds$telefon=='A191']<-'none'
credit_ds$telefon[credit_ds$telefon=='A192']<-'yes, registered under the customers name'
credit_ds$telefon <- as.factor(credit_ds$telefon)

#Treballador estranger
credit_ds$estranger[credit_ds$estranger=='A201']<-'yes'
credit_ds$estranger[credit_ds$estranger=='A202']<-'no'
credit_ds$estranger <- as.factor(credit_ds$estranger)

# ANALISI D'OUTLIERS ===========================================================
# A continuacio s'analitzen els valors de la variable Quantitat per determinar si realment hi ha outliers.
# a) Grafics de caixes
credit_ds_quantitat_bp <-boxplot(credit_ds$quantitat, main="Quantitat")
credit_ds_quantitat_bp$out

# Tot i que la majoria de valors es concentren entre el rang 1.000 - 5.000, alguns valors estan per sobre dels 8.000, que poden ser considerats outliers.

# b) Criteri de les dues desviacions tipiques
quantitat_deteccio_outlier <- abs(scale(credit_ds$quantitat)) > 2 # Genera un array boolea per indicar si el valor de la posició x és outlier
quantitat_deteccio_outlier

# Guarda la llista de registres detectats com a outliers de quantitat
credit_ds$quantitat_deteccio_outlier<-quantitat_deteccio_outlier
credit_ds$quantitat_deteccio_outlier

# Selecciona els valors outliers, els ordena del primer (menor) al darrer (major) i els llista.
quantitat_deteccio_outlier <- data.frame(valor = credit_ds$quantitat, outlier = quantitat_deteccio_outlier)
quantitat_outliers <- quantitat_deteccio_outlier[quantitat_deteccio_outlier$outlier == "TRUE", ]
quantitat_outliers[order(quantitat_outliers$valor),]$valor

# Com es pot veure els valors a partir de 8947 podrien ser considerats outliers.
# Creacio d'un array sense outliers
quantitat_netejat <- quantitat_deteccio_outlier[quantitat_deteccio_outlier$outlier == "FALSE", ]
quantitat_netejat <- quantitat_netejat$valor
quantitat_netejat

# A continuacio s'analitzen els valors de la variable Edat per determinar si hi ha outliers

# a) Grafics de caixes
credit_ds_edat_bp <-boxplot(credit_ds$edat, main="Edat")
credit_ds_edat_bp$out

# Tot i que la majoria de valors es concentren entre el rang 25 - 45, alguns valors estan per sobre dels 60, que poden ser considerats outliers.

# b) Criteri de les dues desviacions tipiques
edat_deteccio_outlier <- abs(scale(credit_ds$edat)) > 2 # Genera un array boolea per indicar si el valor de la posició x és outlier
summary(credit_ds$edat)
edat_deteccio_outlier

# Guarda la llista de registres detectats com a outliers d'edat
credit_ds$edat_deteccio_outlier<-edat_deteccio_outlier


# Selecciona els valors outliers, els ordena del primer (menor) al darrer (major) i els llista.
edat_deteccio_outlier = data.frame(valor = credit_ds$edat, outlier = edat_deteccio_outlier)
edat_deteccio_outlier
edat_outliers = edat_deteccio_outlier[edat_deteccio_outlier$outlier == "TRUE", ]
edat_outliers
edat_outliers[order(edat_outliers$valor),]$valor
# Com es pot veure els valors a partir de 59 podrien ser considerats outliers.
# Creacio d'un array sense outliers
edat_netejat <- edat_deteccio_outlier[edat_deteccio_outlier$outlier == "FALSE", ]
edat_netejat <- edat_netejat$valor
edat_netejat



# Finalment s'analitzen els valors de la variable mesosCredit per determinar si hi ha outliers

# a) Grafics de caixes
credit_ds_mesosCredit_bp <-boxplot(credit_ds$mesosCredit, main="mesosCredit - durada")
credit_ds_mesosCredit_bp$out

# Tot i que la majoria de valors es concentren entre el rang 10 - 30, alguns valors estan per sobre dels 40, que poden ser considerats outliers.

# b) Criteri de les dues desviacions t??piques
mesosCredit_deteccio_outlier <- abs(scale(credit_ds$mesosCredit)) > 2 # Genera un array boolea per indicar si el valor de la posició x és outlier
summary(credit_ds$mesosCredit)
mesosCredit_deteccio_outlier

# Guarda la llista de registres detectats com a outliers d'edat
credit_ds$mesosCredit_deteccio_outlier<-mesosCredit_deteccio_outlier


# Selecciona els valors outliers, els ordena del primer (menor) al darrer (major) i els llista.
mesosCredit_deteccio_outlier <- data.frame(valor = credit_ds$mesosCredit, outlier = mesosCredit_deteccio_outlier)
mesosCredit_outliers <- mesosCredit_deteccio_outlier[mesosCredit_deteccio_outlier$outlier == "TRUE", ]
mesosCredit_outliers[order(mesosCredit_outliers$valor),]$valor
# Com es pot veure els valors a partir de 47 podrien ser considerats outliers.

# Creacio d'un array sense outliers
mesosCredit_netejat <- mesosCredit_deteccio_outlier[mesosCredit_deteccio_outlier$outlier == "FALSE", ]
mesosCredit_netejat <- mesosCredit_netejat$valor
mesosCredit_netejat

#Ens quedem amb el conjunt de dades net
credit_ds_net<-credit_ds[credit_ds$quantitat_deteccio_outlier=='FALSE',]
credit_ds_net<-credit_ds_net[credit_ds_net$edat_deteccio_outlier=='FALSE',]
credit_ds_net<-credit_ds_net[credit_ds_net$mesosCredit_deteccio_outlier=='FALSE',]


# COMPROVACIO DE LA NORMALITAT==============================================

# 1. QUANTITAT
# a) Test de Kolmogorov-Smirnov
ks.test(credit_ds_net$quantitat, pnorm, mean(credit_ds_net$quantitat), sd(credit_ds_net$quantitat))
# NOTA: Retorna un warning degut a que hi ha valors repetits a la serie

# b) Test de Shapiro-Wilk
shapiro.test(credit_ds_net$quantitat)

# En ambdos casos el p-valor es menor que el nivell de significacio alfa=0,05, per tant no passa el test de normalitat

# c) Valoració gràfica de la normalitat emprant Q-Q Plot:
qqnorm(credit_ds_net$quantitat);qqline(credit_ds_net$quantitat, col = 3)

# A la gràfica es pot veure clarament que els punts no segueixen la forma de la línia recta, per tant, no es tracta d'una distribució normal.


# 2.EDAT
# a) Test de Kolmogorov-Smirnov
ks.test(credit_ds_net$edat, pnorm, mean(credit_ds_net$edat), sd(credit_ds_net$edat))

# b) Test de Shapiro-Wilk
shapiro.test(credit_ds_net$edat)

# En ambdos casos el p-valor es menor que el nivell de significacio alfa=0,05, per tant no passa el test de normalitat

# c) Valoració gràfica de la normalitat emprant Q-Q Plot:
qqnorm(credit_ds_net$edat);qqline(credit_ds_net$edat, col = 3)

# Si es realitza la valoració gràfica del conjunt original de valors, sense l'eliminació d'outliers, la distribució encara s'allunya més de la normal
qqnorm(credit_ds$edat);qqline(credit_ds$edat, col = 3)



# 3.MESOSCREDIT
# a) Test de Kolmogorov-Smirnov
ks.test(credit_ds_net$mesosCredit, pnorm, mean(credit_ds_net$mesosCredit), sd(credit_ds_net$mesosCredit))

# b) Test de Shapiro-Wilk
shapiro.test(credit_ds_net$mesosCredit)

# En ambdos casos el p-valor es menor que el nivell de significacio alfa=0,05, per tant no passa el test de normalitat

# c) Valoració gràfica de la normalitat emprant Q-Q Plot:
qqnorm(credit_ds_net$mesosCredit);qqline(credit_ds_net$mesosCredit, col = 3)

# En aquest cas, a la gràfica també es pot observar que hi ha menys valors diferents en el conjunt de mesos (en comparació amb altres variables com quantitat), per això l'aspecte escalonat de la gràfica.



# 4.percentRentaDedicat
# a) Test de Kolmogorov-Smirnov
ks.test(credit_ds_net$percentRentaDedicat, pnorm, mean(credit_ds_net$percentRentaDedicat), sd(credit_ds_net$percentRentaDedicat))

# b) Test de Shapiro-Wilk
shapiro.test(credit_ds_net$percentRentaDedicat)

# En ambdos casos el p-valor es menor que el nivell de significacio alfa=0,05, per tant no passa el test de normalitat

# 5.tempsResidenciaActual
# a) Test de Kolmogorov-Smirnov
ks.test(credit_ds_net$tempsResidenciaActual, pnorm, mean(credit_ds_net$tempsResidenciaActual), sd(credit_ds_net$tempsResidenciaActual))

# b) Test de Shapiro-Wilk
shapiro.test(credit_ds_net$tempsResidenciaActual)

# En ambdos casos el p-valor es menor que el nivell de significacio alfa=0,05, per tant no passa el test de normalitat

# 6.numCredits
# a) Test de Kolmogorov-Smirnov
ks.test(credit_ds_net$numCredits, pnorm, mean(credit_ds_net$numCredits), sd(credit_ds_net$numCredits))

# b) Test de Shapiro-Wilk
shapiro.test(credit_ds_net$numCredits)

# En ambdos casos el p-valor es menor que el nivell de significacio alfa=0,05, per tant no passa el test de normalitat


# COMPROVACIO DE LA HOMOSCEDASTICITAT==============================================
# Es comprova si les vari?ncies de les variables canvien en passar de bon a mal pagador

# 1. QUANTITAT
var.test(credit_ds_net[credit_ds_net$bonPagador==0,]$quantitat,credit_ds_net[credit_ds_net$bonPagador==1,]$quantitat)

# Es rebutja la hip?tesi nul?la: les dues mostres tenen variancies diferents.

# 2. EDAT
var.test(credit_ds_net[credit_ds_net$bonPagador==0,]$edat,credit_ds_net[credit_ds_net$bonPagador==1,]$edat)

# No es pot rebutjar la hipotesi nula.

# 3. MESOSCREDIT
var.test(credit_ds_net[credit_ds_net$bonPagador==0,]$mesosCredit,credit_ds_net[credit_ds_net$bonPagador==1,]$mesosCredit)

# No es pot rebutjar la hipotesi nula.

# 4.percentRentaDedicat
var.test(credit_ds_net[credit_ds_net$bonPagador==0,]$percentRentaDedicat,credit_ds_net[credit_ds_net$bonPagador==1,]$percentRentaDedicat)

# No es pot rebutjar la hipotesi nula.

# 5.tempsResidenciaActual
var.test(credit_ds_net[credit_ds_net$bonPagador==0,]$tempsResidenciaActual,credit_ds_net[credit_ds_net$bonPagador==1,]$tempsResidenciaActual)

# No es pot rebutjar la hipotesi nula.

# 6.numCredits
var.test(credit_ds_net[credit_ds_net$bonPagador==0,]$tempsResidenciaActual,credit_ds_net[credit_ds_net$bonPagador==1,]$tempsResidenciaActual)

# No es pot rebutjar la hipotesi nula.


# COMPROVACIO DE VALORS BUITS, NULS I INDEFINITS
# Valors nuls
is.na(credit_ds_net)
# Valors buits
credit_ds_net==""

# Del resultats obtinguts, es pot veure que no hi ha cap atribut que contingui instacies amb valors nuls.

# COMPROVACIO DE VALORS DIFERENTS QUE PREN CADASCUNA DE LES VARIABLES
apply(credit_ds,2, function(x) length(unique(x)))

# Es pot veure que els atributs que admeten una quantitat major de valors diferents són numèrics: quantitat (sol?licitada), edat, mesosCredit (durada en mesos). Per? alguns atributs cosiderats numerics a la font de dades original, podrien ser considerats com factors, degut al seu reduit nombre de valors possibles. Son concretament els seguents:
# Percentatge de renta dedicat (percentRentaDedicat)
# Temps visquent a la residencia habitual (tempsResidenciaActual)
# El numero de credits que te atorgats cadascuna de les persones (numCredits)
# El numero de persones que es poden fer carrec del credit (numPersonesManteniment)

# En aquest darrer cas, numPersonesManteniment, el nombre de valors diferents es redueix a 2 (1 o 2), per tant es podria transformar en un factor (nova variable creditCompartit)
credit_ds_net$creditCompartit[credit_ds_net$numPersonesManteniment==1] <- 0
credit_ds_net$creditCompartit[credit_ds_net$numPersonesManteniment==2] <- 1
credit_ds_net$creditCompartit <- as.factor(credit_ds_net$creditCompartit)
summary(credit_ds_net$creditCompartit)


# NORMALITZACIO ==============================================
# Depenent de la diferència entre magnituds dels diferents camps pot ser convenient escalar-los per homogenaïtzar-los
# Per tant, a continuació es crearan camps normalitzats per cadascun dels camps numèrics
credit_ds_numeric <- credit_ds_net[,c("edat","quantitat","mesosCredit","percentRentaDedicat","tempsResidenciaActual","numCredits", "numPersonesManteniment")]
credit_ds_norm <- as.data.frame(scale(credit_ds_numeric))
summary(credit_ds_norm)


credit_ds_net$edatNorm <- credit_ds_norm$edat
credit_ds_net$quantitatNorm <- credit_ds_norm$quantitat
credit_ds_net$mesosCreditNorm <- credit_ds_norm$mesosCredit
credit_ds_net$percentRentaDedicat <- credit_ds_norm$percentRentaDedicat
credit_ds_net$tempsResidenciaActual <- credit_ds_norm$tempsResidenciaActual
credit_ds_net$numCredits <- credit_ds_norm$numCredits
credit_ds_net$numPersonesManteniment <- credit_ds_norm$numPersonesManteniment

nrow(credit_ds_net)

# ESTUDI DE LES VARIABLES==================================================

files <- dim(credit_ds_net)

# Relacio amb l'edat -------
ggplot(data=credit_ds_net[1:files,],aes(x=as.numeric(edat), group=bonPagador, fill=bonPagador)) + geom_histogram(binwidth=1, color='black')

# Relacio relativa entre edat i ingressos ------
ggplot(data=credit_ds_net[1:files,],aes(x=as.numeric(edat), group=bonPagador, fill=bonPagador)) + geom_histogram(binwidth=1, position="fill", color='grey')

# D'aquestes grafiques, es visualitza clarament que la majoria de credits es sol?liciten entorn als 30 anys (entre els 25 i 35 anys).
# A més, el percentatge d'impagaments (mal pagadors) en general és més baix com més edat tenen els clients.


# Relacio amb la quantitat de credit sol?licitat -----
# Relacio en numeros absoluts amb la quantitat de credit sol?licitat.
ggplot(data=credit_ds_net[1:files,],aes(x=as.numeric(quantitat), group=bonPagador, fill=bonPagador)) + geom_density(alpha=0.8)

# Relacio relativa amb la quantitat de credit sol?licitat 
ggplot(data=credit_ds_net[1:files,],aes(x=as.numeric(quantitat), group=bonPagador, fill=bonPagador)) + geom_density(alpha=0.8, position="fill")

# Les gràfiques indiquen que no hi ha una relació directa o concloent entre la quantitat sol·licitada i la taxa d'impagaments (per a crèdits molt petits o molt alts  la taxa d'impagaments sembla ser lleugerament superior als crèdits d'entre 1.500 i 4.000 €)

ggplot(data=credit_ds_net[1:files,],aes(x=as.numeric(propietats), group=bonPagador, fill=bonPagador)) + geom_histogram(binwidth=1, color='black')

# Relació amb les propietats de les que disposa
ggplot(data=credit_ds_net[1:files,],aes(x=propietats, fill=bonPagador)) + geom_bar()
ggplot(data=credit_ds_net[1:files,],aes(x=propietats, fill=bonPagador)) + geom_histogram(stat="count", position="fill")+ylab("Freqüència")

# Les gràfiques mostren una lleugera proporció de més bon pagadors entre les persones que disposa algun tipus de propietat confirmada


# Test de difer?ncia de proporcions propietats================================

# El que es busca amb aquest test es veure si hi ha diferencia entre bons i mals
# pagadors respecte a ser propietari d'un immoble

#Nombre d'observacions
nbo<-nrow(credit_ds_net[credit_ds_net$bonPagador==1,])
nmal<-nrow(credit_ds_net[credit_ds_net$bonPagador==0,])

#Proporcions observades
pobbo <- nrow( credit_ds_net[credit_ds_net$bonPagador==1&credit_ds_net$propietats=='real estate',])/nbo
pobmal <- nrow( credit_ds_net[credit_ds_net$bonPagador==0&credit_ds_net$propietats=='real estate',])/nmal

#Imposem valor de alfa
alpha<-0.05
#Aproximacio del parametre p
p<-(nbo*pobbo + nmal*pobmal) / (nbo+nmal)
# Computem el valor observat, el valor cr?titc i el p-valor
zobs <- (pobbo-pobmal)/( sqrt(p*(1-p)*(1/nbo+1/nmal)) )
zcrit <- qnorm(1-alpha, lower.tail=TRUE)
pvalue<- pnorm(abs(zobs), lower.tail=FALSE)
#Imprimim els resultats
print(sprintf("Valor observat: %1.5f",zobs))
print(sprintf("Valor cr?tic superior: %1.5f",zcrit))
print(sprintf("p-valor: %E",pvalue))

# Es conclou que els bons pagadors tenen una proporcio superior de clients propietaris
# d'un inmoble.

# Calcul de l'interval de confian?a de quantitat================================

# L'objectiu d'aquest metode es calcular els intervals de confian?a amb una
# confian?a del 95% de la mitjana de la variable quantitat. Fem el calcul per
# a bons i mals pagadors

# 1. Bons pagadors
t.test(credit_ds_net[credit_ds_net$bonPagador==1,]$quantitat, conf.level = 0.95)

# 2. Mals pagadors
t.test(credit_ds_net[credit_ds_net$bonPagador==0,]$quantitat, conf.level = 0.95)

# Regressio logistica per predir mals pagadors==================================

# L'objectiu d'aquest metode es crear un model que ens permeti predir quins
# clients seran mals pagadors a partir de la resta de variables

model_logreg=glm(formula=bonPagador~edatNorm+quantitatNorm+mesosCreditNorm+creditCompartit+estranger+telefon+tipusFeina+numCredits+propietatVivendaActual+altresPlans+propietats+tempsResidenciaActual+altresDeutors+sexeEstatCivil+percentRentaDedicat+tempsTreballActual+estatCompteEstalvi+estatCompteCorrent+motiu+historiaCreditsAnteriors,
                 family=binomial(link=logit),data = credit_ds_net)
summary(model_logreg)

# Intervals dels odds ratio de les variables explicatives
exp(confint(model_logreg))

# Matriu de confusio
table(credit_ds_net$bonPagador,model_logreg$fitted.values>0.5)

#Imprimim els resultats de la regressio
print(sprintf("Sensibilitat: %1.5f%%",568/(568+51)*100))
print(sprintf("Especificitat: %1.5f%%",117/(117+117)*100))



