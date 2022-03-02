
###########################################################################
#                             Analyses adhoc                              #
###########################################################################   

str(data)


A2 <- traitement_qst(data, type_question = "barplot", code_question = "A2",
                     titre_question = "Situation professionnelle", cible = "tous les répondants", color = couleur2)
A2$tab
A2$vis



# Poste
A3 <- traitement_qst(data, type_question = "barplot", code_question = "A3",
                     titre_question = "Poste", cible = "tous les répondants", color = couleur2)
A3$tab
A3$vis


# Secteur
A4 <- traitement_qst(data, type_question = "barplot", code_question = "A4",
                     titre_question = "Poste", cible = "tous les répondants", color = couleur2)
A4$tab
A4$vis


# Niveau d'étude



# Outils

B2<- traitement_qst(data, type_question = "barplot multiple", code_question = "B2",
                    titre_question = "Outils", cible = "tous les répondants", sous_total = TRUE)
walk(B2, print)

B2$vis


