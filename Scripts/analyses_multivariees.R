##########################################
#          Librairies - options          #
##########################################


library(FactoMineR) 
library(Factoshiny)
library(factoextra)



###################################################################
#                      Analyse outils (ACP)                      #
###################################################################

# Rappel variables
glimpse(data)

# Sélection des variables : utilisation des outils
data_acp_outils <- data %>% select(id, starts_with("num_B2"), A3, nb_outils_utilises, outil_programmation, outil_dataviz) %>% 
  column_to_rownames("id") %>% # ID mis en intitulé de ligne
  rename_with(~str_sub(.x,start = 8), .cols = starts_with("num_B2")) %>%  # Simplification des noms de variables pour l'ACP
  select(-Matlab) # Suppression matlab car personne l'utilise
  
# Intitulé de ligne
glimpse(data_acp_outils)

# Matrice de corrélations
correlations_outils <- round(cor(data_acp_outils[,1:12], use = "complete.obs"),2)

# Visualisation des corrélations
ggcorr(correlations_outils, label = TRUE)

    # Bilan corrélations : assez peu, corrélatiosn négative entre SAS et Python, et SQL et R

# Lancement de l'ACP
acp_outil <- PCA(data_acp_outils, quali.sup = c(13,15,16), quanti.sup = 14, scale.unit = FALSE)

# Valeurs propres
acp_outil$eig

# Visualisation des valeurs propres
fviz_eig(acp_outil, choice = "variance", addlabels = TRUE)
  # 2 dimensions à prendre en compte, totalisant 41% de la variance

# Variables significatives dans les axes
dimdesc(acp_outil)$Dim.1$quanti
dimdesc(acp_outil)$Dim.2$quanti

# Visualisation des variables 
fviz_pca_var(acp_outil)
   # Dim 1 : Excel / SQL / SAS vs Python --> Programmateur vs non-programmeur 
   # Dim 2 : SQL / Python vs SAS / R --> Profil plus général vs spécifique (stat ?)
    # Pas de grosse tendance pour dataviz

# Visualisation des individus 
fviz_pca_ind(acp_outil, habillage = data_acp$A3)
plot.PCA(acp_outil,invisible=c('ind.sup'),habillage=13,title="Graphe des individus de l'ACP",label =c('quali'))

# Affinage avec factoshiny
Factoshiny(data_acp_outils)




###################################################################
#                      Analyse missions  (ACP)                      #
###################################################################



# Rappel variables
glimpse(data)

# Sélection des variables : missions effectives
data_acp_missions <- data %>% select(id, starts_with("num_C1"), A3, Y1a, Y4, Y1b_tr_age) %>% 
  column_to_rownames("id") %>% # ID mis en intitulé de ligne
  rename_with(~str_sub(.x,start = 8), .cols = starts_with("num_C1"))   # Simplification des noms de variables pour l'ACP
  

# Intitulé de ligne
glimpse(data_acp_missions)

# Matrice de corrélations
correlations_missions <- round(cor(data_acp_missions[,1:13], use = "complete.obs"),2)

# Visualisation des corrélations
ggcorr(correlations_missions, label = TRUE)

# Bilan corrélations : beaucoup de corrélations positives / négatives, sautent aux yeux celles entre outils stats

# Lancement de l'ACP
acp_missions <- PCA(data_acp_missions, quali.sup = c(14,15,16, 17), scale.unit = FALSE)
  # Warning pas grave : juste l'individu avec l'age manquant...

# Valeurs propres
acp_missions$eig

# Visualisation des valeurs propres
fviz_eig(acp_missions, choice = "variance", addlabels = TRUE)
# 2 ou 3 dimensions à prendre en compte, totalisant 47% / 58% de la variance

# Variables significatives dans les axes
dimdesc(acp_missions)$Dim.1$quanti # Tout ce qui est stats / machine learning + Presentation
dimdesc(acp_missions)$Dim.2$quanti # BDD + Tableau de bord + viz + Presentation

# Visualisation des variables 
fviz_pca_var(acp_missions)
# Dim 1 : Statistiques / machine learning 
# Dim 2 : SQL / ANalyse / Tableaux de bord


# Visualisation des individus 
plot.PCA(acp_missions,invisible=c('ind.sup'),habillage=14,label =c('quali'))

# Affinage avec factoshiny
Factoshiny(data_acp_missions)


###################################################################
#                  Analyse missions préférées (ACP)               #
###################################################################



# Rappel variables
glimpse(data)

# Sélection des variables : missions effectives
data_acp_preferences <- data %>% select(id, starts_with("num_C2"), A3, Y1a, Y4, Y1b_tr_age) %>% 
  column_to_rownames("id") %>% # ID mis en intitulé de ligne
  rename_with(~str_sub(.x,start = 8), .cols = starts_with("num_C2"))   # Simplification des noms de variables pour l'ACP


# Intitulé de ligne
glimpse(data_acp_preferences)

# Matrice de corrélations
correlations_preferences <- round(cor(data_acp_preferences[,1:13], use = "complete.obs"),2)

# Visualisation des corrélations
ggcorr(correlations_preferences, label = TRUE)

# Bilan corrélations : forte corrélation tableaux de bord / visualisation

# Lancement de l'ACP
acp_preferences <- PCA(data_acp_preferences, quali.sup = c(14,15,16, 17), scale.unit = FALSE)
# Warning : quelques NA qui trainent car question optionnelle par erreur

# Valeurs propres
acp_preferences$eig

# Visualisation des valeurs propres
fviz_eig(acp_preferences, choice = "variance", addlabels = TRUE)
# 2 ou 3 dimensions à prendre en compte, totalisant 40% / 52% de la variance

# Variables significatives dans les axes
dimdesc(acp_preferences)$Dim.1$quanti 
dimdesc(acp_preferences)$Dim.2$quanti 

# Visualisation des variables 
fviz_pca_var(acp_preferences)
# Dim 1 : Traitements + viz vs Machine Learning
# Dim 2 : Traitements vs viz
fviz_pca_var(acp_preferences,axes = c(1,3))
# Dim 3 : régression vs machine learning

# Visualisation des individus 
plot.PCA(acp_preferences,invisible=c('ind.sup'),habillage=14,label =c('quali'))
plot.PCA(acp_preferences,axes = c(1,3))

# Affinage avec factoshiny
Factoshiny(data_acp_preferences)



###################################################################
#               Analyse croisée secteur / poste (AFC)             #
###################################################################



# Rappel variables
glimpse(data)

# Sélection des variables : secteur et poste
data_afc <- data %>% select(A4, A3) %>%
  mutate(A4 = fct_drop(A4), A3 = fct_drop(A3)) # Suppression levels inutiles

# Tableau de contingences
tab_contingences <- table(data_afc$A4, data_afc$A3, useNA = "no")

# Test du khi2
chisq.test(tab_contingences)
  # Pvalue < 0 : il y a un lien significatif entre le secteur et le poste

# Lancement de l'AFC
afc <- CA(tab_contingences)

# Statisticiens proches de la santé
# Chargés d'études surreprésentés dans les banques
# Data scientists plus nombreux en informatique





###################################################################
#                Analyse profil démographique (ACM)               #
###################################################################



# Rappel variables
glimpse(data)

# Sélection des variables : profil socio-démographique
data_acm <- data %>% select(id, Y1a, Y4, Y1b_tr_age, A3) %>% 
  column_to_rownames("id") # ID mis en intitulé de ligne

# Variables dans l'ACM
glimpse(data_acm)

# Lancement de l'ACM
acm <- MCA(data_acm, quali.sup = 4)

# Visualisation des valeurs propres
fviz_eig(acm, choice = "variance", addlabels = TRUE)
acm$eig
  # Les 3 premières dimensions rasssemblent 57% de la variance


# Visualisation des individus 
fviz_mca_ind(acm)

# Visualisation des modalités 
fviz_mca_var(acm)

# Visualisation des variables 
fviz_mca_var(acm, choice = "var")



###################################################################
#             Classification sur "types de datanalyst"            #
###################################################################

# Objectif : essayer de trouver 2 ou 3 "sous_classes" de data analyst / chargés d'étude 
  # Classer en fonction de leurs missions

# Même problématique que "analyse missions" mais sur data analyst uniquement


# Sélection des variables et des individus (data analyst et chargés d'études)
data_acp_missions_analystes <- filter(data, A3 %in% c("Chargé d'études", "Data Analyst")) %>%  
  select(id, starts_with("num_C1"), A3, starts_with("B2"), Y1a, Y1b, Y4)  %>% 
  column_to_rownames("id") %>% # ID mis en intitulé de ligne
  rename_with(~str_sub(.x,start = 8), .cols = starts_with("num_C1"))   # Simplification des noms de variables pour l'ACP

# Intitulé de ligne
glimpse(data_acp_missions_analystes)

# Matrice de corrélations
correlations_missions_analystes <- round(cor(data_acp_missions_analystes[,1:13], use = "complete.obs"),2)

# Visualisation des corrélations
ggcorr(correlations_missions_analystes, label = TRUE)


# Lancement de l'ACP
acp_missions_analystes <- PCA(data_acp_missions_analystes, quali.sup = 14:32, scale.unit = FALSE)


# Valeurs propres
acp_missions_analystes$eig

# Visualisation des valeurs propres
fviz_eig(acp_missions_analystes, choice = "variance", addlabels = TRUE)

# Visualisation des variables 
fviz_pca_var(acp_missions_analystes)
# Dim 1 : Travail technique vs présentation
# Dim 2 : Tests statistique


# Visualisation des individus 
plot.PCA(acp_missions_analystes, label = "none")

# Affinage et classification avec factoshiny
Factoshiny(data_acp_missions_analystes)
  # Bilan : 2 classes équilibrées, une technique l'autre moins technique mais plus de présentation


###################################################################

#                     Synthèse générale (AFM)                     #
###################################################################




# Rappel variables
glimpse(data)

# Sélection des variables : missions effectives
data_afm <- data %>% select(id, 
                                        starts_with("num_B2"), # Outils
                                        starts_with("num_C1"), # Missions réalisées
                                        #starts_with("num_C2"), # Missions préférées
                                        Y1a, Y4, Y1b_tr_age, # Socio démo (quali))
                                        A3, A4 # Poste & secteur (supplémentaires)
                                        ) %>% 
  column_to_rownames("id") %>% # ID mis en intitulé de ligne
  rename_with(~str_sub(.x,start = 8), .cols = starts_with("num_C1")) %>% 
  rename_with(~str_sub(.x,start = 8), .cols = starts_with("num_B2"))  
  #rename_with(~paste0("pref_", str_sub(.x,start = 8)), .cols = starts_with("num_C2")) 
  

# Variables pour l'Afm
names(data_afm)
?MFA
afm <- MFA(data_afm, 
                  group = c(13, 13 ,3, 1, 1),
                  type = c("c", "c", "n","n", "n"), # c pour var quantitatives non réduites, n pour var qualitatives
                  name.group = c("outils", "missions", "demographie","poste", "secteur"), # Noms des groupes
                  num.group.sup =  c(3, 4, 5), # Groupes supplémentaires
                  ncp = 4)  # 4 axes de conservés (pour la CAH)

# Valeurs propres
fviz_eig(afm, choice = "variance", addlabels = TRUE)
afm$eig
  # Les 3 premiers axes expliquent 44% de l'information (52% avec 4 axes)

# Aide pour les visualisations de l'AFM
?plot.MFA

# Graphe des individus
plot.MFA(afm, lab.ind = FALSE, habillage = "A3", axes = c(1,2))
plot.MFA(afm, lab.ind = FALSE, habillage = "A3", axes = c(3,4))

  # Graphe des variables
plot.MFA(afm, choix = "var", axes = c(1,2), select = "cos2 0.3")
plot.MFA(afm, choix = "var", axes = c(3,4), select = "cos2 0.2")
  # Axe 1 : missions techniques et programmation
  # Axe 2 : bases de données et présentations
  # Axe 3 : R / SAS ( ? )
  # Axe 4 : tableaux de ord / viz / tableau / excel

# Graphe des axes partiels
plot.MFA(afm, choix = "axes")

# Graphe des groupes
plot.MFA(afm, choix  = "group")
plot.MFA(afm, choix  = "group", axes = c(3,4))

# Avec factoshiny
Factoshiny(afm)

Factoshiny(data_afm)





###################################################################
#             Classification finale (CAH sur AFM)                 #
###################################################################


?HCPC

# Classification à partir de l'AFM en 3 classes : retrouvera t'on le poste à partir des missions / outils ? 
CAH <- HCPC(afm, nb.clust = 3)

# Dendogramme
plot.HCPC(CAH,choice='tree')

# Représentation sur l'axe factorielle
plot.HCPC(CAH,choice='map', draw.tree = FALSE )

# Effectifs de chaque classe
count(CAH$data.clust,clust)

# Proportion de chaque poste dans chaque classe
tab_classe_poste <- table(CAH$data.clust$A3, CAH$data.clust$clust )
tab_classe_poste # Valeurs brutes
round(prop.table(tab_classe_poste, margin = 1),2) # Pourcentages lignes
round(prop.table(tab_classe_poste, margin = 2),2) # Pourcentages colonnes


      