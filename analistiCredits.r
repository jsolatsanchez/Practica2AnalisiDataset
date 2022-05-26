# Carrega de les llibreries emprades
library(ggplot2)

# Carrega del dataset
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

# Analitzant la informació obtinguda de l'aplicació de la funció summary, es poden comprovar els valors mitjans, les medianes i quartils.
# de les diferents variables del dataset, d'aquesta analisi preliminar es poden extreure les següents conclusions:
#   * El valor maxim de la variable quantitat es troba a una distància relativament superior de la mediana i la mitjana que el valor mínim, això pot indicar l'existència d'outliers
#   * La distribució de valors de les variables edat i mesosCredit també pot fer pensar que hi puguin haver outliers.
#   * El camp 'bonPagador' està definit com a numèric (1-bon pagador i 2-mal pagador), per tant, es podria posar en termes de 0 i 1, per ser convertit a factor.

# Converteix els valors de bon pagador a 0(FALS) i 1(VERTADER: bon pagador)
credit_ds$bonPagador[credit_ds$bonPagador==2] <-0
# Converteix els valors numèrics a factors
credit_ds$bonPagador <- as.factor(credit_ds$bonPagador)
summary(credit_ds$bonPagador)
#Converteix les codificacions a valors
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

# ANÀLISI D'OUTLIERS ===========================================================
# A continuació s'analitzen els valors de la variable Quantitat per determinar si realment hi ha outliers.
# a) Gràfics de caixes
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
# Creació d'un array sense outliers
quantitat_netejat <- quantitat_deteccio_outlier[quantitat_deteccio_outlier$outlier == "FALSE", ]
quantitat_netejat <- quantitat_netejat$valor
quantitat_netejat

# A continuació s'analitzen els valors de la variable Edat per determinar si hi ha outliers

# a) Gràfics de caixes
credit_ds_edat_bp <-boxplot(credit_ds$edat, main="Edat")
credit_ds_edat_bp$out

# Tot i que la majoria de valors es concentren entre el rang 25 - 45, alguns valors estan per sobre dels 60, que poden ser considerats outliers.

# b) Criteri de les dues desviacions típiques
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
# Creació d'un array sense outliers
edat_netejat <- edat_deteccio_outlier[edat_deteccio_outlier$outlier == "FALSE", ]
edat_netejat <- edat_netejat$valor
edat_netejat



# Finalment s'analitzen els valors de la variable mesosCredit per determinar si hi ha outliers

# a) Gràfics de caixes
credit_ds_mesosCredit_bp <-boxplot(credit_ds$mesosCredit, main="mesosCredit - durada")
credit_ds_mesosCredit_bp$out

# Tot i que la majoria de valors es concentren entre el rang 10 - 30, alguns valors estan per sobre dels 40, que poden ser considerats outliers.

# b) Criteri de les dues desviacions típiques
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
# Creació d'un array sense outliers
mesosCredit_netejat <- mesosCredit_deteccio_outlier[mesosCredit_deteccio_outlier$outlier == "FALSE", ]
mesosCredit_netejat <- mesosCredit_netejat$valor
mesosCredit_netejat

#Ens quedem amb el conjunt de dades net
credit_ds_net<-credit_ds[credit_ds$quantitat_deteccio_outlier=='FALSE',]
credit_ds_net<-credit_ds_net[credit_ds_net$edat_deteccio_outlier=='FALSE',]
credit_ds_net<-credit_ds_net[credit_ds_net$mesosCredit_deteccio_outlier=='FALSE',]


# COMPROVACIÓ DE LA NORMALITAT DE QUANTITAT ================================
# a) Test de Kolmogorov-Smirnov
ks.test(credit_ds_net$quantitat, pnorm, mean(credit_ds_net$quantitat), sd(credit_ds_net$quantitat))
# NOTA: Retorna un warning degut a què hi ha valors repetits a la sèrie
ks.test(unique(credit_ds$quantitat), pnorm, mean(credit_ds$quantitat), sd(credit_ds$quantitat))

# b) Test de Shapiro-Wilk
shapiro.test(credit_ds$quantitat)

# En ambdós casos el p-valor és menor que el nivell de significació alfa=0,05, per tant no passa el test de normalitat


# COMPROVACIÓ DE LA NORMALITAT DE QUANTITAT NETEJADA (sense outliers) ===============
# a) Test de Kolmogorov-Smirnov
ks.test(quantitat_netejat, pnorm, mean(quantitat_netejat), sd(quantitat_netejat))

# b) Test de Shapiro-Wilk
shapiro.test(quantitat_netejat)

# En ambdós casos el p-valor és menor que el nivell de significació alfa=0,05, per tant no passa el test de normalitat



# COMPROVACIÓ DE LA NORMALITAT D'EDAT =====================================
# a) Test de Kolmogorov-Smirnov
ks.test(credit_ds$edat, pnorm, mean(credit_ds$edat), sd(credit_ds$edat))
ks.test(edat_netejat, pnorm, mean(edat_netejat), sd(edat_netejat))


# b) Test de Shapiro-Wilk
shapiro.test(edat_netejat)

# En ambdós casos el p-valor és menor que el nivell de significació alfa=0,05, per tant no passa el test de normalitat


# COMPROVACIÓ DE LA NORMALITAT DE MESOSCREDIT =====================================
# a) Test de Kolmogorov-Smirnov
ks.test(credit_ds$mesosCredit, pnorm, mean(credit_ds$mesosCredit), sd(credit_ds$mesosCredit))
ks.test(mesosCredit_netejat, pnorm, mean(mesosCredit_netejat), sd(mesosCredit_netejat))

# b) Test de Shapiro-Wilk
shapiro.test(mesosCredit_netejat)

# En ambdós casos el p-valor és menor que el nivell de significació alfa=0,05, per tant no passa el test de normalitat

# Test homoscedasticitat


# COMPROVACIÓ DE VALORS BUITS, NULS I INDEFINITS
# Valors nuls
is.na(credit_ds_net)
# Valors buits
credit_ds_net==""

# Del resultats obtinguts, es pot veure que no hi ha cap atribut que contingui instàncies amb valors nuls.

# COMPROVACIÓ DE VALORS DIFERENTS QUE PREN CADASCUNA DE LES VARIABLES
apply(credit_ds,2, function(x) length(unique(x)))

# Es pot veure que els atributs que admeten una quantitat major de valors diferents són numèrics: quantitat (sol·licitada), edat, mesosCredit (durada en mesos). Però alguns atributs cosiderats numèrics a la font de dades original, podríen ser considerats com factors, degut al seu reduït nombre de valors possibles. Són concretament els següents:
# Però alguns atributs cosiderats numèrics a la font de dades original, podríen ser considerats com factors, degut al seu reduït nombre de valors possibles. Són concretament els següents:
  # Percentatge de renta dedicat (percentRentaDedicat)
  # Temps visquent a la residència habitual (tempsResidenciaActual)
  # El número de crèdits que té atorgats cadascuna de les persones (numCredits)
  # El número de persones que es poden fer càrrec del crèdit (numPersonesManteniment)

# En aquest darrer cas, numPersonesManteniment, el nombre de valors diferents es redueix a 2 (1 o 2), per tant es podria transformar en un factor (nova variable creditCompartit)
credit_ds$creditCompartit[credit_ds$numPersonesManteniment==1] <- 0
credit_ds$creditCompartit[credit_ds$numPersonesManteniment==2] <- 1
credit_ds$creditCompartit <- as.factor(credit_ds$creditCompartit)
summary(credit_ds$creditCompartit)


# NORMALITZACIÓ ==============================================
# Depenent de la diferència entre magnituds dels diferents camps pot ser convenient escalar-los per homogenaïtzar-los
# Per tant, a continuació es crearan camps normalitzats per cadascun dels camps numèrics
credit_ds_numeric <- credit_ds[,c("edat","quantitat","mesosCredit","percentRentaDedicat","tempsResidenciaActual","numCredits", "numPersonesManteniment")]
credit_ds_norm <- as.data.frame(scale(credit_ds_numeric))
summary(credit_ds_norm)


credit_ds$edatNorm <- credit_ds_norm$edat
credit_ds$quantitatNorm <- credit_ds_norm$quantitat
credit_ds$mesosCreditNorm <- credit_ds_norm$mesosCredit
credit_ds$percentRentaDedicat <- credit_ds_norm$percentRentaDedicat
credit_ds$tempsResidenciaActual <- credit_ds_norm$tempsResidenciaActual
credit_ds$numCredits <- credit_ds_norm$numCredits
credit_ds$numPersonesManteniment <- credit_ds_norm$numPersonesManteniment


# Observant els valors de quantitat ja normalitzats, es pot comprovar, de nou, l'existència d'outliers (que el seu valor màxim sigui 5,3681 ja pot ser considerat allunyat, per tant, pot ser un bon indicador per determinar si s’han d’eliminar algunes ocurrències (detecció de outliers). 
# Per tant, es genera un nou conjunt eliminant els outliers ja detectats

credit_ds <- credit_ds[(credit_ds$quantitat_deteccio_outlier == FALSE),]
credit_ds <- credit_ds[(credit_ds$edat_deteccio_outlier == FALSE),]
credit_ds <- credit_ds[(credit_ds$mesosCredit_deteccio_outlier == FALSE),]

nrow(credit_ds)

# ESTUDI DE LES VARIABLES

# Relació amb l'edat -------
ggplot(data=credit_ds[1:files,],aes(x=as.numeric(edat), group=bonPagador, fill=bonPagador)) + geom_histogram(binwidth=1, color='black')

# Relació relativa entre edat i ingressos ------
ggplot(data=credit_ds[1:files,],aes(x=as.numeric(edat), group=bonPagador, fill=bonPagador)) + geom_histogram(binwidth=1, position="fill", color='grey')

# D’aquestes gràfiques, es visualitza clarament que la majoria de crèdits es sol·liciten entorn als 30 anys (entre els 25 i 35 anys). Es tracta d’un factor important a considerar quan posteriorment s’hagi de decidir el criteri pel qual es discretitza l’edat.


# Relació amb la quantitat de crèdit sol·licitat -----
# Relació en números absoluts amb la quantitat de crèdit sol·licitat.
ggplot(data=credit_ds[1:files,],aes(x=as.numeric(quantitat), group=bonPagador, fill=bonPagador)) + geom_density(alpha=0.8)

# Relacio relativa amb la quantitat de crèdit sol·licitat 
ggplot(data=credit_ds[1:files,],aes(x=as.numeric(quantitat), group=bonPagador, fill=bonPagador)) + geom_density(alpha=0.8, position="fill")

# Com es pot veure a partir de la primera gràfica (valors absoluts) la majoria de crèdits són de fins a 5000DM, i a més, per aquestes quantitats, el número de mals pagadors és superior als de bons pagadors. A partir dels 4500DM la tendència s’inverteix i, tot i què la base de dades conté menys casos, per grans quantitats sol·licitades es podria afirmar que es tindran més bons pagadors.

