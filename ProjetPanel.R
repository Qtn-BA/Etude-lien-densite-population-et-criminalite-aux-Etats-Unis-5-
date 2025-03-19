library(AER)
data = data("Guns")

# Charger les packages
library(plm)
library(AER)
library(stargazer)

# Charger la base de données "Guns"
data("Guns", package = "AER")

# Aperçu des données
head(Guns)

# Statistiques descriptives des variables principales
summary(Guns)

# Modèle OLS (MCO)
ols_model <- lm(violent ~ density + income + law + afam + prisoners, data = Guns)
summary(ols_model)

# Modèle à effets fixes individuels (Within)
fe_ind_model <- plm(violent ~ density + income + law + afam + prisoners, 
                    data = Guns, index = c("state", "year"), model = "within")
summary(fe_ind_model)

# Modèle à effets fixes temporels
fe_temp_model <- plm(violent ~ density + income + law + afam + prisoners + factor(year), 
                     data = Guns, index = c("state", "year"), model = "within")
summary(fe_temp_model)

# Modèle à effets fixes individuels et temporels
fe_both_model <- plm(violent ~ density + income + law + afam + prisoners + factor(year), 
                     data = Guns, index = c("state", "year"), model = "within", effect = "twoways")
summary(fe_both_model)

# Modèle Between (comparaison entre États ou entre années)
be_ind_model <- plm(violent ~ density + income + law + afam + prisoners, 
                    data = Guns, index = c("state", "year"), model = "between", effect = "individual")
summary(be_ind_model)

be_temp_model <- plm(violent ~ density + income + law + afam + prisoners, 
                     data = Guns, index = c("state", "year"), model = "between", effect = "time")
summary(be_temp_model)

# Modèle à effets aléatoires (RE)
re_model <- plm(violent ~ density + income + law + afam + prisoners, 
                data = Guns, index = c("state", "year"), model = "random")
summary(re_model)


# Test de Hausman (choix entre effets fixes et effets aléatoires)
hausman_test <- phtest(fe_both_model, re_model)
print(hausman_test)

# Test de Breusch-Pagan pour l'homoscédasticité
bptest(ols_model)

# Test de Wooldridge pour l'autocorrélation
pbgtest(fe_both_model)

