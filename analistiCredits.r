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
