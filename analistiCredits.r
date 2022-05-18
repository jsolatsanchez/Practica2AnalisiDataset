# Càrrega de les llibreries emprades
library(ggplot2)

# Càrrega del dataset
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
                    "numPersonesManteniment", "teTelèfon",
                    "estranger", "bonPagador")

linies <- dim(credit_ds)
linies

summary(credit_ds)

# Analitzant la informació obtinguda de l'aplicació de la funció summary, es poden comprovar els valors mitjans i les medianes (i quartils)
# de les diferents variables del dataset, d'aquesta anàlisi preliminar es poden extreure les següents conclusions:
#   * El valor màxim de la variable quantitat es troba a una distància relativament superior de la mediana i la mitjana que el valor mínim, això pot indicar l'existència d'outliers
#   * La resta de variables tenen una distribució de valors més propera la d'una normal.

# A continuació s'analitzen els valors de la variable Quantitat per determinar si realment hi ha outliers.
# a) Gràfics de caixes
credit_ds_quantitat_bp <-boxplot(credit_ds$quantitat, main="Quantitat")
credit_ds_quantitat_bp$out

# b) Criteri de les dues desviacions típiques
quantitat.outlier <- abs(scale(credit_ds$quantitat))
quantitat.outlier


# Comprovació de la normalitat
ks.test(credit_ds$quantitat, pnorm, mean(credit_ds$quantitat), sd(credit_ds$quantitat))

unique(unique(credit_ds$quantitat))