##########################################
#          Librairies - options          #
##########################################


# Visualisation
library(ggplot2)
library(GGally) # Pour des visualisations de corrélations sous ggplot (ggcorr)
library(corrplot) # Idem, fonctions corrplot
library(ggthemes) # Thèmes additionnels ggplot (dont economist)
#library(ggrepel) # Pour placer les points de ggplot de façon optimal (utiliser geom_repel() par ex)
#library(RColorBrewer) # Palettes de couleurs (display.brewer.all() pour les afficher)
#library(wordcloud2) # Nuages de mots (text mining)
#library(rAmCharts) # Visualisation intéractive (package français)
#library(dygraphs) # Visualisations intéractives de séries temporelles
library(plotly) # Graphiques avec intéractions
#library(maps) # Cartographie avec ggplot
#library(shiny) # Shiny
#library(shinydashboard) # Shiny template dashboard
library(kableExtra) # Tableaux améliorés avec markdown

#library(ggfortify) # Pour faire des "diagnostic plot" de modèles en mode ggplot via autoplot() (s'utilise aussi sur xts et kmeans)



#library(Factoshiny) # Interface shiny pour analyses factorielles

# Analyse statistique

library(psych) # Méthodes statistiques utilisées en psycho
library(psy) # Méthodes statistiques utilisées en psycho
library(FactoMineR) # Analyses factorielles
library(factoextra) # Visualisations en ggplot pour analyses factorielles
library(broom) # Utilisation de fonctions statistiques en mode "tidy data"
library(rstatix) # Fonctions de statistiques en mode tidy data (chisq_test)
library(gtools) # Fonctions diverses, notamment stars.pval permettant d'avoir la significativité
#library(sjPlot) # Fonction tab_model pour des sorties html des tables de sortie des models

library(xtable)
library(Hmisc) # Fonctions diverses de statistiques, notamment rcorr pour les significativité de corrélations (attention, problème de conflits avec dplyr et plotly)


# Traitement de données tidyverse

library(readxl) # Importer des fichiers excel
library(readr) # Importer des fichiers plats
library(dplyr) # Traitement de données : la référence
library(tidyr) # Pour passer de format long à format court
library(stringr) # Traitement de chaines de caractères : str_sub, str_detect, str_locate...
library(lubridate) # Traitement de dates
library(purrr) # Remplace les apply en tidyverse
library(forcats) # Traitement de facteurs
library(skimr) # Summary plus efficace



# Traitements de données autres
#library(tm) # Text mining
#library(stringi) # Conversion avec accents
#library(xts) # Séries temporelles (nécessaire pour dygraph)
library(openxlsx) # Importation et écrire dans fichiers Excel




###################################################################
#               Récupération fichiers de données                  #
###################################################################


#####  Fichier des résultats bruts avec codes réponses ##### 
data <- read.csv("./data/resultats.csv", encoding = "UTF-8")
  
  # Aperçu
  dim(data)
  str(data)
  data[1:6, 5:7]
  

  #####  Fichier des résultats bruts avec réponses en texte ##### 
data2 <- read.csv("./data/resultats_codes.csv", encoding = "UTF-8")

  # Aperçu  
  dim(data2)
  data2[1:6, 5:7]
 
   
  #####  Libellés des questions de l'enquête ##### 
libelles_questions <- read.csv("./data/libelles_questions.csv",  encoding = "UTF-8", header = FALSE, nrows = 1, stringsAsFactors = FALSE)

  # Affecter les mêmes noms de questions
  names(libelles_questions) <- names(data)

  
  
###################################################################
#               Pré-traitements fichiers de données               #
###################################################################


  #####  Supprimer les questions de "ranks" inutiles, et "startlanguage / lastpage / submitdate ##### 
  var_inutiles <- c("startlanguage", "lastpage", "submitdate", "C2.4.", "C2.5.", "C2.6.", "C2.7.", "C2.8.", "C2.9.", "C2.10.", "C2.11.", "C2.12.", "C2.13.")
  
  data <- data %>% select(-var_inutiles)
  libelles_questions <- libelles_questions %>% select(-var_inutiles)
  glimpse(data)
  
  
##### Convertir en facteurs les résultats de valeurs non numériques / hors questions ouvertes ##### 

  # Vecteur de booléens sur questions numériques, dates et pures Q0
  codes_hors_num_QO <- names((data[,!names(data) %in% c("submitdate", "Z2", "Z1", "A3.other.", "A2.other.", "A4.other." ) & !map_lgl(data, is.numeric)]))
  
  # Conversion des facteurs
  data[,codes_hors_num_QO] <- map2_df(data[,codes_hors_num_QO], data2[,codes_hors_num_QO], ~factor(paste0(str_replace(.y, "-oth-", "A99"),"_",.x))) %>% 
    map_df(~fct_relevel(.x), levels = sort(unique(.x))) %>%  # Trier les facteurs selon l'ordre des A1/A2/etc...
    map_df(~fct_recode(.x, NULL = "_")) %>%  # Remplacement des "_" en valeurs nulles
    map_df(~fct_relabel(.x, ~str_replace(str_extract(.x, "_.+"), "_", ""))) # Transformation des valeurs type "A1_Data" en "Data"

        # data %>%  mutate(A3 = fct_relabel(A3, ~str_replace(str_extract(.x, "_.+"), "_", "")))
  
  #####  Reconvertir en caractères les pures questions ouvertes ##### 
  codes_questions_ouvertes <- c("Z2", "Z1", "A3.other.", "A2.other.", "A4.other." )
  data[,codes_questions_ouvertes] <- data[,codes_questions_ouvertes] %>% map_df(~as.character(.x))
  


#####  Récap  ##### 
glimpse(data)
dim(data)
dim(libelles_questions)

# Vérifications des niveaux des facteurs 
map(data[,codes_hors_num_QO], levels)

# sauvegarde
raw_data <- data


###########################################################################
#                           Traitements généraux                          #
###########################################################################


##### Rechargement données de départ #####
data <- raw_data


# Nombre de réponses brutes à l'enquête
nb_reponses_brut <- nrow(data)



##### Recodage A2 avec les QO associés #####

  # Contenus de la question et des QO 
  table(data$A2.other.)
  table(data$A2)

  
  # Recodage A2
  data <- data %>% mutate(
    A2 = factor(ifelse(str_trim(A2.other.)=="en reconversion professionnelle", "Etudiant (hors alternance / stage)",
    ifelse(str_trim(A2.other.)=="CDI de chantier", "En CDI", 
    ifelse(str_trim(A2.other.)=="Congé maternité","En CDI",as.character(A2)))))) 

  # Vérification
  table(data$A2)
  
  
### Recodage A3 avec les QO associés #####
  
  # Contenus de la question et des QO 
  count(data,A3.other.)
  count(data, A3)  
  
  
  # Recodage A3
  equivalent_data_analyst <- c("Consultante BI", "Demand planner/ data analyst", "Data Visualizer")
  equivalent_datascientist <- c("Developpeur R&D en intelligence artificielle", "Ingénieur IA", "Ingénieur de recherches en Data Science")
  equivalent_statisticien <- c("Biostatisticien", "Maïtre de Conférences" )
  equivalent_autre <- c("Ingénieur en développement", "Business analyst", "Consultant informatique", "Ingénieur de recherche", "Développeur Python" ,"Ingénieur en développement","Data Lead", "Expert en ingénierie logicielle","Data Gouvernance" )
  
  data <- data %>% mutate(
    A3 = factor(ifelse(str_trim(A3.other.) %in%  equivalent_data_analyst , "Data Analyst / analyste" ,
                       ifelse(str_trim(A3.other.) %in% equivalent_datascientist, "Data Scientist", 
                              ifelse(str_trim(A3.other.) %in% equivalent_statisticien, "Statisticien",
                                     ifelse(str_trim(A3.other.) %in% equivalent_autre,"Autre", as.character(A3)))))))
  
  # Vérification
  count(data, A3)  
  
    # Les NA sont bien les étudiants / en recherche d'emploi ?
  data %>% filter(is.na(A3)) %>% count(A2)
  
  
  
### Recodage A4 avec les QO associés #####    
  
  # Contenus de la question et des QO 
  count(data,A4.other.)
  count(data, A4) 
  
  # Recodage A4
  equivalent_transport <- c("Aérien","Aeronautique","Compagnie maritime","Transport", "Transports") 
  equivalent_industrie <- c("AGRO ALIMENTAIRE","Agroalimentaire","Energie", "Énergie"," Industrie", "Automobile") 
  equivalent_numerique <- c("E-commerce", "E-commerce")
  equivalent_autre <- c("Un peut tout car en esn", "Sport", "Urbanisme commercial", "Retail", "Marketing", "Marketing (opticien)", "Markzting6", "Gambling","Audit","Caritatifs","Commerce de gros","Conseil","Conseil en Stratégie et Management","Culture")

  data <- data %>% mutate(
    A4 = factor(ifelse(str_trim(A4.other.) %in%  equivalent_transport , "Transport" ,
                              ifelse(str_trim(A4.other.) %in% equivalent_industrie, "Industrie",
                                     ifelse(str_trim(A4.other.) %in% equivalent_numerique, "Numérique / Web",
                                     ifelse(str_trim(A4.other.) %in% equivalent_autre,"Autre", as.character(A4))))))) %>% 
      mutate(A4 = fct_recode(A4, Autre = "Administration", Autre = "Télécommunications", Autre = "Transport", Autre = "Industrie", Autre = "Numérique / Web", Autre = "Enseignement / recherche"), # Pas assez finalement pour en faire des catégories à part
             A4 = fct_relevel(A4, "Autre", after = Inf )) # "Autre" placé à la fin
  
    
  
  # Vérification
  count(data, A4)  
  

  
### Recodage B2 : inverser les facteurs puis remplacer les NA par jamais ###
  
  # Inversion de facteurs
  data <- data %>% 
    mutate(across(starts_with("B2"), ~fct_rev(.x))) 
  
  # Remplacer les NA par "Jamais" 
  data <- data %>% mutate(across(starts_with("B2"), ~fct_explicit_na(.x, "Jamais")))

  #Vérification
  table(data$B2.SQ012.)

  
### Recodage age Y1B : correctif valeurs aberrantes  ###  
  
  
  # Recodage valeur aberrante (99)
  data <- data %>%  mutate(Y1b = ifelse(Y1b > 90, NA, Y1b))


  
### Recodage C1 : correctif facteurs pour SQ001  ###  
  data <- data %>% mutate(C1.SQ001. = fct_expand(C1.SQ001., "Jamais"),
                          C1.SQ001. = fct_relevel(C1.SQ001., "Jamais"))

  
  
    
### Réponses à supprimer : étudiants /  en recherche d'emploi et data ingénieurs / autres profils ###
  
 data <- filter(data, !A2 %in% c("Etudiant (hors alternance / stage)", "En recherche d'emploi")) %>% 
        filter(!A3 %in% c("Autre", "Data Engineer"))

 
 
### Colonnes à supprimer : détails déjà exploités ###
 
 data <- data %>%  select(- A2.other., - A3.other.,-  A4.other., - Z1, -Z2 ) 

 
 # Nombre de réponses exploitables à l'enquête
 nb_reponses <- nrow(data)

  # Vérification périmètre de base
  count(data, A2)
  count(data, A3)
  count(data, A4)
  

  
  
  
# Pas de traitement de dates, ni calcul de temps de réponse
#data$date <- as_date(ymd_hms(data$submitdate))



# Recodage noms des variables (pour Tableau)
names(data)


###########################################################################
#                       Variables supplémentaires                         #
###########################################################################



### Nouvelle variable : tranches d'âge (Y1b)

  # Distribution de l'age ? 
  quantile(data$Y1b, na.rm = TRUE)
  hist(data$Y1b)

  # Création des tranches d'age
  data <- data %>%  mutate(Y1b_tr_age = ifelse(Y1b <= 25, "25 ans et moins",
                                               ifelse(Y1b < 31, "Entre 26 et 30 ans",
                                                      ifelse(Y1b <= 35, "Entre 31 et 35 ans",
                                                             "Plus de 35 ans"))))
  # Vérification
  count(data, Y1b_tr_age)

  
    
### Nouvelles variables : B2 en numérique
  
  # Création variables
  data <- data %>% 
    mutate(across(starts_with("B2"), ~as.integer(fct_rev(.x)), .names = "num_{.col}"))

  # Vérification cohérence
  data %>% select(contains("B2.SQ001.")) %>%  head(10)

    
### Nouvelles variables :  C1 en numérique
  
  # Création variables
  data <- data %>% 
    mutate(across(starts_with("C1"), ~as.integer(.x), .names = "num_{.col}"))
  
  # Vérification cohérence
  data %>% select(contains(c("C1.SQ001.", "C1.SQ003."))) %>%  head(10)
 
  
### Nouvelles variables :  classement des tâches préférées (C2) recodées (autant de variables qu'il y a de tâches)
  
  head(data$C1.SQ005.)
  head(data$C2.1.)
  
  # Affichage des modalités de réponse possibles
  levels(data$C2.1.)
  
  tache1 <- "Traitement / manipulation de données"
  tache2 <- "Visualisation de données / graphiques"
  tache3 <- "Construction de tableaux de bord"
  tache4 <- "Régressions et modèles statistiques"
  tache5 <- "Algorithmes prédictifs / Machine Learning"
  tache6 <- "Analyses multi-dimensionnelles, type ACP / Clustering"
  tache7 <- "Tests statistiques / statistiques inférentielles"
  tache8 <- "Présentation orale d’études"
  tache9 <- "Gestion / construction de bases de données"
  tache10 <- "Analyse « métier » de données"
  tache11 <- "Requêtes sur bases de données"
  tache12 <- "Traitement d’images"
  tache13 <- "Analyse textuelle"
  
  
    # Création de variables sur la préférence de chacune des tâches
  data <- data %>% 
    mutate(across(starts_with("C2"), ~as.character(.x))) %>% 
    mutate(
      C2_SQ001. = if_else(C2.1. == tache1, "1_Tâche préférée", 
                           ifelse(C2.2. == tache1, "2_Deuxième tâche préférée",
                                  ifelse(C2.3. == tache1, "3_Troisième tâche préférée",
                                       "Tâche non-citée"))),
      C2_SQ002. = if_else(C2.1. == tache2, "1_Tâche préférée", 
                          ifelse(C2.2. == tache2, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache2, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ003. = if_else(C2.1. == tache3, "1_Tâche préférée", 
                          ifelse(C2.2. == tache3, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache3, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ004. = if_else(C2.1. == tache4, "1_Tâche préférée", 
                          ifelse(C2.2. == tache4, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache4, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ005. = if_else(C2.1. == tache5, "1_Tâche préférée", 
                          ifelse(C2.2. == tache5, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache5, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ006. = if_else(C2.1. == tache6, "1_Tâche préférée", 
                          ifelse(C2.2. == tache6, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache6, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ007. = if_else(C2.1. == tache7, "1_Tâche préférée", 
                          ifelse(C2.2. == tache7, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache7, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ008. = if_else(C2.1. == tache8, "1_Tâche préférée", 
                          ifelse(C2.2. == tache8, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache8, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ009. = if_else(C2.1. == tache9, "1_Tâche préférée", 
                          ifelse(C2.2. == tache9, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache9, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ010. = if_else(C2.1. == tache10, "1_Tâche préférée", 
                          ifelse(C2.2. == tache10, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache10, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ011. = if_else(C2.1. == tache11, "1_Tâche préférée", 
                          ifelse(C2.2. == tache11, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache11, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ012. = if_else(C2.1. == tache12, "1_Tâche préférée", 
                          ifelse(C2.2. == tache12, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache12, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))),
      C2_SQ013. = if_else(C2.1. == tache13, "1_Tâche préférée", 
                          ifelse(C2.2. == tache13, "2_Deuxième tâche préférée",
                                 ifelse(C2.3. == tache13, "3_Troisième tâche préférée",
                                        "Tâche non-citée"))))  

    #   mutate( # Recoder en facteur, en enlevant les numéros devant... (un peu lourd oui !)
    #   Q4_tot_tel = fct_relabel(Q4_tot_tel, str_remove, "[0-9]_"),
    #   Q4_tot_mail = fct_relabel(Q4_tot_mail, str_remove, "[0-9]_"),
    #   Q4_tot_visio = fct_relabel(Q4_tot_visio, str_remove, "[0-9]_"),
    #   Q4_tot_chat = fct_relabel(Q4_tot_chat, str_remove, "[0-9]_"),
    #   Q4_tot_rs = fct_relabel(Q4_tot_rs, str_remove, "[0-9]_"),
    #   Q4_tot_autre = fct_relabel(Q4_tot_autre, str_remove, "[0-9]_")
    # )
    
  
  # Vérifications / cohérence des nouvelles variables
  data %>% count(C2_SQ001.)
  data %>% count(C2_SQ010.)
  data %>% count(C2_SQ013.)
    data %>% count(C2.1.)
    data %>% count(C2.2.)
    data %>% count(C2.3.)
    
    
  # Création de versions numériques de ces variables
  data <- data %>% mutate(across(starts_with("C2_"), ~as.numeric(as.character(fct_recode(.x, "3" = "1_Tâche préférée",
                                                 "2" = "2_Deuxième tâche préférée",
                                                 "1" = "3_Troisième tâche préférée",
                                                 "0" = "Tâche non-citée"))), 
                         .names = "num_{.col}")) 
                
  # Vérifications cohérence valeurs numériques  
  data %>% select(starts_with("num_C2")) %>%   colMeans(na.rm = TRUE)
    

###########################################################################
#                            Traitements autres                          #
###########################################################################


  map(data[,codes_hors_num_QO], table)





###########################################################################
#                      Format pour export Tableau                         #
###########################################################################

str(data)

# Préparation data Tableaux pour tris croisés
data_tris_croises <- data %>% 
  select(id, A2, A3, A4, Y1a, Y1b, Y1b_tr_age) %>% 
  rename(Statut = A2, Poste = A3, Secteur = A4, genre = Y1a, age = Y1b, tr_age = Y1b_tr_age)
  
# Préparation data Tableau pour données de type texte 
data_text <- data %>% 
  select(!contains("B1"), -Y1b) %>% select(!starts_with("num")) %>%  # Suppression colonnes numériques
  pivot_longer(-id, values_to = "reponse", names_to = "code_question") 


# Préparation data Tableau pour données de type numérique
data_num <- data %>% 
  select(id, contains("B1"), Y1b, starts_with(c("num_B2", "num_C1", "num_C2"))) %>%  # On garde que les colonnes numériques
  pivot_longer(-id, values_to = "reponse_num", names_to = "code_question") %>% 
  mutate(code_question = str_replace(code_question, "num_","")) 

# Jointure data numériques et texte
data_tableau <- data_text %>% full_join(data_num, by = c("code_question", "id"))

# Export vers fichier Excel
onglets_data <- list('data_tris_croises' = data_tris_croises, 'data_text' = data_text, 'data_num' = data_num, "data_tableau" = data_tableau)
write.xlsx(onglets_data, file = './Visualisation/Data/data_tableau.xlsx')


View(data_tableau)
View(count(data_tableau, code_question))

