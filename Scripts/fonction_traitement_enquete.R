


nb_bins = 19
group = NA
color = NA
tosort = FALSE 
ignore_empty = TRUE
coord_flip = FALSE
nb_mod = NA
cible = ""
question = ""


subplot = plotly::subplot # subplot fait référence à la fonction de plotly, pas de hsmic

traitement_qst = function(data, 
                          type_question = c("barplot", "choix multiple", "barplot multiple", "boxplot", "barplot circulaire",  "pie", "donut", "histogram", "correlations", "ACP"), 
                          code_question, 
                          titre_question, 
                          question = "", # Texte à passer pour remplacer le libellé de la question automatique 
                          group = NA,    # Variable à préciser si on veut les résultats par groupes (facet)
                          color = couleur1, # Couleur / vecteur de couleurs à passer manuellement au graphique
                          tosort = FALSE, # mettre TRUE si on veut
                          ignore_empty = TRUE, # Laisser à true si on veut ignorer les réponses vides (les supprimer)
                          coord_flip = FALSE, # Inverser x et y (barres horizontales)
                          stack = FALSE, # mettre TRUE si on veut des barres verticales empilées
                          nb_mod = NA, # Nombre de modalités à garder pour les barplot multiple (par défaut on garde tout)
                          compare = FALSE, # Comparer avec les résultats de l'enquête précédente si TRUE
                          sous_total = FALSE, # Ajouter ou non des sous_totaux type top / Top2 aux tableaux
                          labels_items = NA, # Labels raccourcis à utiliser
                          cible = "") {
  

  
  type_question <- match.arg(type_question) # S'assurer que le type de question renseignée est bien valide

  data_qst <- select(data, matches(code_question)) %>% rename(reponse = 1)


  if (!is.na(group)) { data_qst <- select(data, contains(code_question), group ) %>%  # -contains("other")
                        rename(reponse = 1, group = tail(names(.),1))} # group = tail... : dernière position
  
  #str(data_qst)
  #head(data_qst)
  
  
  #------------------------------------------------------# 
  #------------ nombre de répondants et NA---------------#
  #------------------------------------------------------# 
  
  # Objectif de cette sous-partie :
    # Récupérer dans nb_na le nombre de non-répondants à une question
    #  .. et dans nb_reponses_qst le nombre de répondants
  
  
  if (type_question %in% c("choix multiple")) { # Nb na sur choix multiple : plus compliqué...
    
    # Rajouter un truc ici pour ne faire cette opération que sur l'enquête la plus récente
    
    nb_na <- 0
    na_ligne <- TRUE
    for (i in (1:nrow((data_qst)))) {
      for (j in (1:ncol(data_qst))) {
        na_ligne <- ifelse(!(data_qst[i,j] %in% c("","N/A")), FALSE,na_ligne) # Dès qu'une ligne a autre chose que NA ou vide, on la compte plus en NA
      }
      if (na_ligne) {nb_na <- nb_na + 1 }
      na_ligne <- TRUE
    }  
  }  else if (type_question %in% c("barplot multiple","boxplot", "barplot circulaire multiple")) { # Sous-questions : on regarde juste la 1ere colonne pour le nombre de na
    nb_na <- select(data_qst,1) %>% filter(is.na(reponse)) %>% pull %>% length
  } else {
    nb_na <- data_qst %>% filter(is.na(reponse)) %>% pull %>% length
  }  
  #nb_na <- unlist(lapply(select(data_qst,contains(code_question)), function(col) {sum(col %in% c("","N/A"))}))

 
  #Nombre de répondants (hors NA) dans la question
  nb_reponses <- data %>% nrow() 
    
  nb_reponses_qst <- nb_reponses - nb_na  
  

  
  #---------------------------------------------------# 
  #-------------- La question posée-------------------#
  #---------------------------------------------------# 
  
  # Objectif de cette sous-partie :
  #   Récupérer dans question le nom de la question posée

  if (question == "") { # Si la question n'est pas en paramètre de fonction, aller le chercher dans la fichier des questions
    
    if (type_question %in% c("choix multiple", "barplot multiple", "boxplot", "correlations", "ACP", "barplot circulaire multiple")) { # Sous-questions : aller chercher le libellé de la première sous question
      libelle_sousquestion1 <- select(libelles_questions, matches(code_question)) %>% select(1) %>% pull 
      question <-  str_sub(libelle_sousquestion1, str_locate(libelle_sousquestion1,".+\\[")[1], str_locate(libelle_sousquestion1,".+\\[")[2]-2) # Récupération de tout ce qu'il y a entre le 1er et second crochet
      
    }
    else {
      question <- pull(libelles_questions, code_question)[1]
      #question <- select(libelles_questions, contains(code_question))
    }
  }
  

  #------------------------------------------------------------# 
  #------------Retravail des données de la question------------#
  #------------------------------------------------------------# 
  
  # Objectif de cette sous-partie :
  # Récupérer dans data_qst les données retravaillées de la / les question 

  if (type_question %in% c("choix multiple", "barplot multiple", "boxplot", "correlations", "ACP", "barplot circulaire multiple")) { # Sous-questions : prétraitements spécifiques

    raw_data_qst <- data_qst
   
    
    # Récupération d'un vecteur du nom des différentes réponses
     nom_sous_question <- function(i) {
      libelle_sousquestion <- select(libelles_questions, matches(code_question)) %>% select(i) %>% pull 
      sous_question <-  str_sub(libelle_sousquestion, str_locate(libelle_sousquestion,"\\[.+\\]")[1]+1, str_locate(libelle_sousquestion,"\\[.+\\]")[2]-1)
    }
    sous_questions <- NULL 
    max <- length(names(raw_data_qst))
    
    if (!is.na(group)) { max <- length(names(raw_data_qst)) - 1}

    nom_sous_question(1)
    for(i in 1:max) {
      c <- nom_sous_question(i)
      sous_questions <- c(sous_questions, c)
    }

    names(raw_data_qst) <- sous_questions
    if (!is.na(group)) { names(raw_data_qst) <- c(sous_questions, "group")}
    

  
    if (type_question =="choix multiple") {
      #data_qst <- gather(raw_data_qst, reponse, n, - group) %>% filter(n == "Oui") %>% mutate(reponse = as_factor(reponse)) %>% select(-n)
      
      # Mise au format ligne
      if (is.na(group)) {
        data_qst <- gather(raw_data_qst, reponse, n) 
      } else {
        data_qst <- gather(raw_data_qst, reponse, - group)
      }
      
      data_qst <- data_qst %>% filter(!(n %in% c("Non","N/A", ""))) %>%
        mutate(reponse = as_factor(reponse)) %>% select(-n)
   
    } else { 
      
      # Mise au format ligne (sauf pour corrélations)
      if (is.na(group)) {
        data_qst <- gather(raw_data_qst, question, reponse)
      } else {
        data_qst <- gather(raw_data_qst, question, reponse, - group) 
      }
     
      data_qst<-  data_qst %>% mutate(question = as_factor(question)) 
      
      if (type_question != "boxplot" & type_question != "correlations" & type_question != "ACP") {
      data_qst$reponse <- fct_relevel(data_qst$reponse,levels(raw_data_qst[,1] )) }
    
      if (type_question == "correlations") { # Corrélations / : on ne prend pas en compte le format ligne
      data_qst <- raw_data_qst }
     
      if (type_question == "ACP") { # ACP : comme corrélations mais suppression au préalable des lignes ayant au moins une valeur manquante
        data_qst <- raw_data_qst %>% drop_na()
        
        # Recalcul du nombre de répondants / NA pour la méthode
        nb_reponses_qst <- data_qst %>% nrow()  
        nb_na <- nb_reponses - nb_reponses_qst

        }
       
    }

}

  #------------------------------------------------# 
  #------------Résultats de la question------------#
  #------------------------------------------------# 
  
  # Objectif de cette sous-partie :
  #   Récupérer dans resultats_qst les résultats bruts de la question 
  #   ... fréquences / pourcentages ou moyennes / médianes, éventuellement croisés

  
  # Résultats sur tous les types de traitements liés à des calculs de fréquence
  if (type_question %in% c("barplot", "barplot multiple", "barplot circulaire multiple", "pie", "donut", "choix multiple")) {
  
  
  if(type_question %in% c("barplot multiple", "barplot circulaire multiple")) {
    resultats_qst <- count(data_qst, question, reponse) # barplot muliples sans groupe : résultats par réponses ET questions 
  } else {  resultats_qst <- count(data_qst, reponse) } # Sinon sans groupe: résultats par réponses sur la question unique
  
  
  # Si variable de group renseignée
  if (!is.na(group)) { 

    if(type_question %in% c("barplot multiple", "barplot circulaire multiple")) {
      resultats_qst <- group_by(data_qst, group) %>% count(question, reponse)   # barplot muliples avec groupe : résultats par réponses, questions & groupes
    } else {  resultats_qst <- count(data_qst, reponse, group) }  # sinon avec groupe : résultats par réponses sur la question et groupe
    
  }
  
  # Si on doit ignorer les valeurs manquantes :
  if (ignore_empty) {resultats_qst <- filter(resultats_qst, reponse !="")}
  
  # Résultats en pourcentages :
  
  if (is.na(group)){ 
    resultats_qst$percent = round(resultats_qst$n / nb_reponses_qst * 100, 1)
    if (type_question %in% c("barplot multiple", "barplot circulaire multiple")) {
        resultats_qst  = mutate(group_by(resultats_qst, question) %>% mutate(percent = round(n / sum(n) * 100, 1))) }
  } else {
    if(type_question %in% c("barplot multiple", "barplot circulaire multiple")){
      resultats_qst  = mutate(group_by(resultats_qst, question, group) %>% mutate(percent = round(n / sum(n) * 100, 1)))
    }
    
    else if (type_question %in% c("choix multiple")) {
      
    # Formule MAGIQUE !!! Pour réussir à gérer tous les cas liés aux N/A des "autres" et des "non exclusif".. Galère !
    raw_data_qst <- select(raw_data_qst, one_of('Autre'), everything()) # On met la colonne autre en premier si elle existe...
    colonne_cle <- raw_data_qst[, tail(colnames(raw_data_qst)[-1], 2)[1]] # ... on sélectionne la dernière colonne qui est le non exclusif s'il y en a
    base_group <- filter(raw_data_qst, colonne_cle != "N/A") %>% count(group) %>% rename(base_group = n) # On se base sur cette colonne pour calculer la base du groupe
      
    # La version ancienne, plus simple mais marche pas dans certains cas
    #base_group <- count(raw_data_qst, group) %>% rename(base_group = n)
    
    resultats_qst <- resultats_qst %>% left_join(base_group, by = "group")
    resultats_qst$percent = round(resultats_qst$n / resultats_qst$base_group * 100, 1)
    }
    
      else resultats_qst  = mutate(group_by(resultats_qst, group) %>% mutate(percent = round(n / sum(n) * 100, 1)))
     }
  
}
    
  if (type_question == "histogram") { # Histogramme : on remplace les valeurs par défauts de resultats_qst
    resultats_qst = summary(data_qst$reponse)
    
    resultats_qst <- summarise(data_qst, 
                        nb = n(),
                        moyenne = round(mean(reponse),1),
                        mediane = round(median(reponse), 1), 
                        minimum = min(reponse),
                        maximum = max(reponse))
    
    if (!is.na(group)) {resultats_qst = group_by(data_qst, group) %>%
                       summarise( 
                          nb = n(),
                          moyenne = round(mean(reponse, na.rm = TRUE),1),
                          mediane = round(median(reponse, na.rm = TRUE), 1), 
                          minimum = min(reponse, na.rm = TRUE),
                          maximum = max(reponse, na.rm = TRUE))}
  }
  

    
  if (type_question == "boxplot") { # Boxplot : calcul de moyennes, écarts types... par sous-question
   
    if (is.na(group)) { 
      resultats_qst <- data_qst %>% group_by(question) 
    } else resultats_qst <- data_qst %>% group_by(question, group) 
    
     resultats_qst <- resultats_qst %>% 
       filter(!is.na(reponse)) %>%  # On ne compte pas les NA
       summarise(
        n = n(),
        moyenne = round(mean(reponse, na.rm = TRUE),2),
        sd = round(sd(reponse, na.rm = TRUE),2),
        min = min(reponse, na.rm = TRUE),
        max = max(reponse, na.rm = TRUE),
        mediane = median(reponse, na.rm = TRUE)
      )
    
  }


  if (type_question == "correlations") { # Corrélations : calcul de la matrice de corrélations entre items

    resultats_qst <- round(cor(data_qst, use = "pairwise.complete.obs"), 2)
      # toutes les corrélations deux à deux sont calculées 
 

    
  }
  
  if (type_question == "ACP") { # ACP : exécution de la méthode
    
    if (!is.na(labels_items)) { # Si des labels raccourcis ont été fournis, les utiliser...
      colnames(data_qst) <- labels_items
    }
  
    
    resultats_qst <- PCA(data_qst, graph = FALSE, scale.unit = TRUE)
      # Par défaut : données centrées réduites
  }
  
  # Si résultats triés :
  if (tosort) { resultats_qst <- arrange(resultats_qst, percent) }

  
  #---------------------------------------------------# 
  #------------Graphiques & visualisations------------#
  #---------------------------------------------------# 

  # Objectif de cette sous-partie :
  #   Récupérer dans graph (ggplot) et/ou vis (plotly) une visualisation correspondant à la / les question(s)
  
  
  # Au préalable : définition du sous-titre
  caption <- paste0("<i>", nb_reponses_qst," répondants </i>")
  caption_graph <- paste0(nb_reponses_qst," répondants") # Caption utilisé pour les graphes ggplot
  
  if (cible != "") { 
    caption <- paste0(caption, " (", cible,")","</i>")
    caption_graph <- paste0(caption_graph, " (", cible,")")
  }

  # Au préalable : échelle de couleur utilisée si aucune couleur spécifique mentionnée et barplot multiple / barplot stacked
  if (length(color) <= 1 & (type_question %in% c("barplot multiple", "barplot circulaire multiple") | stack == TRUE))  {
    color <- switch(as.character(nlevels(resultats_qst$reponse)),
                    "6" = couleurs_6_points,
                    "5" = couleurs_5_points,
                    "4" = couleurs_4_points,
                    "3" = couleurs_3_points,
                    "2" = couleurs_2_points)}
  
 
    if (type_question %in% c("barplot", "pie", "donut", "lollipop", "choix multiple")) {     # Cas les plus classiques avec 1 seule variable qualitative


    resultats_qst$reponse <- factor(resultats_qst$reponse, unique(resultats_qst$reponse))
    #resultats_qst <- mutate(resultats_qst,  reponse =  as_factor(reponse) %>% fct_drop()) # Même chose qu'au dessus en principe mais en enlevant les facteurs vides

    if (stack) { # Affichage d'une seule barre avec la couleur pour différencier les modalités
      
      graph <- ggplot(resultats_qst, aes(1, percent)) + 
        geom_bar(stat = "identity", aes(fill = reponse, group = 1), size = 0.8) +
        geom_text(aes(label = paste0(round(percent,0), "%"), vjust = 0.5), position = position_stack(vjust=0.5), color = "black", fontface ="bold", size = size_label)
      
      
      # Retraitement spécifique pour les labels dans plotly
      #resultats_qst <- resultats_qst %>% mutate(ylabels = cumsum(percent))
      resultats_qst  <- resultats_qst %>% 
        mutate(ylabels = if_else(is.na(lag(percent)), cumsum(percent)/2, cumsum(percent) - ((cumsum(percent) - lag(cumsum(percent)))/2)))
      
      if (is.na(group))  {
        resultats_qst <-  resultats_qst  %>% mutate(group = factor("")) }
      else { resultats_qst <- resultats_qst %>%  ungroup() %>% mutate(group = fct_drop(group)) }
      
      vis <- resultats_qst %>% 
        group_by(group) %>% 
        do(vis = plot_ly(
                    data = ., 
                     x= 1, 
                     y = ~percent, 
                     hoverinfo = "text",
                     color = ~reponse, opacity= 0.6,
                     text = ~paste0(reponse," :<b> ", percent, "%</b><br>", group, "<br> <i>", n, " répondants</i>" )) %>% 
        add_bars(width = 0.25, colors = color) %>% 
        #add_text(text = ~paste0(round(percent,0), " %"), textposition = 'right',  textfont = list(color = "black", size = size_label, face="bold")) %>% 
        add_annotations(text = ~paste0(percent,0, " %"), x = 1, y = ~ylabels, xref = "x", yref = "y", showarrow = FALSE, xanchor = 'center', font = list(color = "black", size = 16)) %>%
        add_annotations(x = 1, y = 1, text = ~group, showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=14, color="purple")) %>% 
        layout(
              barmode = 'stack',
              title = list(text = paste0('<b>',titre_question, "</b><br><i>", question,"</i>"), font = list(size = 16, color = toRGB("dark blue"))),
               hoverlabel = "left",  showlegend = FALSE,
               margin = list( t = 100, b = 90),
               xaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
               yaxis = list(title ="", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE))) %>% 
        subplot(nrows =  length(levels(resultats_qst$group)), shareX = TRUE) %>% 
        add_annotations(x = 1, y = -0.1, text = ~caption, showarrow = F, xref='paper', yref='paper', xanchor='right', font=list(size=12, color="black")) %>% 
        hide_legend()

       
    } else { # Affichage d'une barre à chaque modalité
      
      graph <- ggplot(resultats_qst, aes(reponse, percent)) + 
        geom_bar(stat = "identity", fill = color, show.legend = FALSE) +
        geom_text(aes(label = paste0(round(percent,0), "%"), vjust = 0.5), position = position_stack(vjust=0.5), color = "black", fontface ="bold", size = size_label)
      



      if (is.na(group))  {
        resultats_qst <-  resultats_qst  %>% mutate(group = factor("")) }
      else { resultats_qst <- resultats_qst %>%  ungroup() %>% mutate(group = fct_drop(group)) }

      
      vis <- resultats_qst %>% 
        group_by(group) %>% 
        do( vis = plot_ly(
            data = .,
            x = ~percent, 
            y = ~reponse, 
            hoverinfo = "text",
            color = I(color), opacity= 0.6,
            text = ~paste0(reponse," :<b> ", percent, "%</b><br>", group, "<br> <i>", n, " répondants</i>" )) %>% 
          add_bars( orientation = 'h') %>%
          add_annotations(text = ~paste0(round(percent,0), " %"), x = ~percent, y = ~reponse, xref = "x", yref = "y", showarrow = FALSE, xanchor = 'left', font = list(color = "black", size = 14)) %>% 
          add_annotations(x = 1, y = 1, text = ~group, showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=14, color="purple")) %>% 
          layout(
                margin = list( t = 100, b = 90),
                hoverlabel = "left",
                xaxis = list(title = ""),
                yaxis = list(title = "% répondants"),
                title = list(text = paste0('<b>',titre_question, "</b><br><i>", question,"</i>"), font = list(size = 16, color = toRGB("dark blue"))))
          ) %>%
        #add_segments(x = 0 , xend = 100, y = 1, yend = 1) %>% # Ne marche pas (segment de séparation entre facets)
        subplot(nrows =  length(levels(resultats_qst$group)), shareX = TRUE) %>% 
        add_annotations(x = 1, y = -0.1, text = ~caption, showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=10, color="black")) %>%  
        hide_legend()

    }

    
    

    if(type_question %in% c("pie","donut")) {
      
    
      
    resultats_qst <- resultats_qst %>%  arrange(percent) %>% group_by(reponse)
      
    graph <- ggplot(resultats_qst, aes(2, percent, fill = reponse)) + 
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(round(percent,0), "%")), position = position_stack(vjust=0.5), color = "black", fontface ="bold", size = size_label) +
        coord_polar(theta="y")
    
    
    
    if (is.na(group))  {
      resultats_qst <-  resultats_qst  %>% mutate(group = factor("")) 
    } else { resultats_qst <- resultats_qst %>%  ungroup() %>% mutate(group = fct_drop(group)) }
    
    
    vis <- resultats_qst %>% 
      group_by(group) %>% 
      do(
         vis = plot_ly(
                   data = . ,
                   labels = ~reponse, 
                    values = ~percent,
                    marker = list(colors = I(rev(color))),
                    hoverinfo = "text",
                    text = ~paste0(reponse," :<b> ", percent, "%</b><br>", group, "<br> <i>", n, " répondants</i>" ),
                    textposition = 'inside',
                    textinfo = 'percent') %>% 
            add_pie(opacity= 0.8) %>% 
           add_annotations(x = 1, y = 1, text = ~group, showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=14, color="purple")) %>% 
            layout(title = list(text = paste0('<b>',titre_question, "</b><br><i>", question,"</i>"), font = list(size = 16, color = toRGB("dark blue"))),
                margin = list( t = 150 , b = 90),
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
            )
         ) %>% 
           subplot(nrows =  length(levels(resultats_qst$group)), shareX = TRUE) %>% 
        add_annotations(x = 1, y = -0.1, text = ~caption, showarrow = F, xref='paper', yref='paper', xanchor='right', font=list(size=12, color="black")) %>% 
        hide_legend()
    # MARCHE PAS avec des subplot 
 
    }
    
    if(type_question %in% c("lollipop")) {
    
      graph <- ggplot(resultats_qst, aes(reponse, percent)) + 
        geom_segment(aes(x = reponse, y = 0 , xend = reponse, yend = percent), color= "skyblue", size = 2) +
        geom_point(color = color, size = 4) + 
        geom_text(aes(label = paste0(round(percent,0), "%"), vjust = 0.5, hjust = -1), color = "black", fontface ="bold", size = size_label) 
    
    }
    
    if (compare) {
      graph <- graph + geom_text(aes(label = paste0("\n\n(", factor(evol), ")"),  color = color_evol, vjust = 0.5), show.legend = FALSE, position = position_stack(vjust=0.5), fontface ="bold", size = size_label) +
        geom_point(aes(color = color_evol), size = size_label, show.legend = FALSE)
    }

    
  }  else if(type_question %in% c("barplot multiple", "barplot circulaire multiple")) {     # Cas avec plusieurs variables qualitatives


    
    if (!is.na(nb_mod)) { # Conserver un certain nombre de modalités de réponse
      resultats_qst <- filter(resultats_qst, reponse %in% levels(reponse)[1:nb_mod])
    }
   
     if (tosort) { # Tri sur les deux premières modalités
            
         ordre_question <- filter(resultats_qst, reponse %in% c(levels(resultats_qst$reponse)[1], levels(resultats_qst$reponse)[2])) %>%
           group_by(question) %>% summarise(n = sum(n), percent = sum(percent)) %>% 
           arrange(desc(percent))  %>%  pull(question)  
        
         # Tri sur 1ere modalité uniquement        
         #ordre_question <- filter(resultats_qst, reponse == levels(resultats_qst$reponse)[1]) %>%  arrange(desc(percent)) %>%  pull(question)
        
          resultats_qst$question <- factor(resultats_qst$question, ordre_question)
          #resultats_qst <- mutate(resultats_qst,  question =  as_factor(question,  ordre_question) %>% fct_drop()) # Même chose qu'au dessus en principe mais en enlevant les facteurs vides
          
     
     } 

  
    
    graph <- ggplot(resultats_qst, aes(question, percent, group = reponse)) +
      geom_bar(stat = "identity", aes( fill = reponse), position = position_stack(reverse = TRUE)) +
      geom_text(aes(label = paste0(round(percent,0), "%"), vjust = 0.5), position = position_stack(vjust=0.5, reverse = TRUE), color = "black", fontface ="bold", size = size_label)
    
    
    
    # Spécifique Plotly 
    if (is.na(group))  {
      resultats_qst <-  resultats_qst  %>% mutate(group = factor("")) 
    } else { resultats_qst <- resultats_qst %>%  ungroup() %>% mutate(group = fct_drop(group)) }
    
    
    
    # Spécifique plotly : rajouter un indicateur pour placer les libellés au milieu des barres cumulées (compliqué... !)
    resultats_qst  <- resultats_qst %>% arrange(question, reponse) %>% group_by(question, group) %>% 
      mutate(x = if_else(is.na(lag(percent)), cumsum(percent)/2, cumsum(percent) - ((cumsum(percent) - lag(cumsum(percent)))/2)))
    
    # vis <- plot_ly(resultats_qst, 
    #                x= ~percent, 
    #                y = ~fct_rev(question), 
    #                hoverinfo = "text",
    #                color = ~reponse, opacity= 0.6,
    #                text = ~paste0(reponse," :<b> ", percent, "</b>%<br> <i>", n, " répondants</i>")) %>% 
    #   add_bars(orientation = 'h', colors = color) %>% 
    #   add_annotations(text = ~if_else(resultats_qst$percent<5, "", paste0(round(resultats_qst$percent,0), " %")), x = ~resultats_qst$x, y = ~resultats_qst$question, xref = "x", yref = "y", showarrow = FALSE) %>% 
    #   layout(title = list(text = paste0('<b>',titre_question, "</b><br><i>", question,"</i>"), font = list(size = 16)),
    #          hoverlabel = "left",  #showlegend = FALSE,
    #          barmode = 'stack',
    #          margin = list( t = 100, b = 90),
    #          xaxis = list(title = ""),
    #          yaxis = list(title ="")) %>% 
    #   add_annotations(x = 1, y = -0.1, text = ~caption, showarrow = F, xref='paper', yref='paper', xanchor='right', font=list(size=12, color="black"))

    vis <- resultats_qst %>% 
      group_by(group) %>% 
      do( vis = plot_ly(
        data = .,
        x= ~percent, 
        y = ~fct_rev(question), 
        hoverinfo = "text",
        color = ~reponse, opacity= 0.6,
        text = ~paste0(reponse," :<b> ", percent, "</b>%<br> <i>", n, " répondants</i>")) %>% 
          add_bars(orientation = 'h', colors = color) %>% 
          #add_annotations(text = ~if_else(resultats_qst$percent<5, "", paste0(round(resultats_qst$percent,0), " %")), x = ~resultats_qst$x, y = ~resultats_qst$question, xref = "x", yref = "y", showarrow = FALSE) %>% 
          add_annotations(text = ~if_else(percent<5, "", paste0(round(percent,0), " %")), x = ~x, y = ~question, xref = "x", yref = "y", showarrow = FALSE) %>% 
          #add_annotations(text = ~paste0(percent,0, " %"), x = 1, y = ~ylabels, xref = "x", yref = "y", showarrow = FALSE, xanchor = 'center', font = list(color = "black", size = 16)) %>%
          add_annotations(x = 1, y = 1, text = ~group, showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=14, color="purple")) %>% 
          layout(title = list(text = paste0('<b>',titre_question, "</b><br><i>", question,"</i>"), font = list(size = 16, color = toRGB("dark blue"))),
                 hoverlabel = "left",  #showlegend = FALSE,
                 barmode = 'stack',
                 margin = list( t = 100, b = 90),
                 xaxis = list(title = ""),
                 yaxis = list(title ="", tickfont = list(size = 10)))
      )%>% 
      subplot(nrows =  length(levels(resultats_qst$group)), shareX = TRUE) %>% 
      add_annotations(x = 1, y = -0.1, text = ~paste0(caption), showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=10, color="black"))  
     # %>% hide_legend()
    
    if (type_question == "barplot circulaire multiple") { 

    
      # Création d'une table des coordonnées des sous-question
      label_data <- filter(resultats_qst, reponse %in% c(levels(resultats_qst$reponse)[1], levels(resultats_qst$reponse)[2])) %>%
        group_by(question) %>% summarise(n = sum(n), percent = sum(percent)) %>% 
        arrange(desc(percent))  
      
      # Lignes vides à rajouter pour le diagramme circulaire
      for (i in 1:1) { # Ca ne marche pas si on insère plusieurs cases vides...
      resultats_qst <- rbind(resultats_qst, data.frame(question = "",reponse = "", n = NA,percent = NA))
      label_data <- rbind(label_data, data.frame(question = "", n = NA,percent = NA))}
      
      
      # Calcul des angles pour les libellés des sous-question
      number_of_bar <- nrow((label_data))
      label_data$id <- seq(1,nrow(label_data))
      label_data$angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
      label_data$angle<-ifelse(label_data$angle < -90, label_data$angle+180, label_data$angle)
      
      graph <- ggplot(resultats_qst, aes(question, percent, group = reponse)) +
        geom_bar(stat = "identity", aes( fill = reponse), position = position_stack(reverse = TRUE)) +
        geom_text(aes(y = percent , label = paste0(round(percent,0), "%"), vjust = 0.5), position = position_stack(vjust=0.5, reverse = TRUE), color = "black", fontface ="bold", size = size_label) +
        geom_text(data = label_data, aes(x = question, y = percent + 45 , label = question, angle = angle), color = "black", fontface ="bold", size = size_label,  inherit.aes = FALSE) + 
        coord_polar(start = 0) +  ylim(-100,160)
      
    } 


    
  } else if(type_question == "histogram") {
    
    
    graph <- ggplot(data_qst, aes(reponse)) + geom_histogram(fill = color, color = "white", bins = nb_bins) +
      geom_text(data = resultats_qst, y = 100, aes(x = moyenne, label = paste0("moyenne : ",moyenne, "\n")), fontface = "bold") +
      geom_vline(data=resultats_qst, aes(xintercept = moyenne), color = "salmon", size = 1.25, linetype = "dashed")
           
    
    
    vis <- plot_ly(data_qst, x = ~reponse, nbinsx = 6)  %>% 
           add_histogram() %>% 
           layout(title = list(text = paste0('<b>',titre_question, "</b><br><i>", question,"</i>"), font = list(size = 16, color = toRGB("dark blue"))),
                  margin = list( t = 150 , b = 90),
                  xaxis = list(title = ""),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
           add_annotations(x = 1, y = -0.1, text = ~caption, showarrow = F, xref='paper', yref='paper', xanchor='right', font=list(size=12, color="black"))
   
  
    
  }
  
  
  if(type_question == "boxplot") {
    
    vis <- plot_ly(data_qst, x = ~reponse, color = ~question, type = "box") %>% 
      layout(title = list(text = paste0('<b>',titre_question, "</b><br><i>", question,"</i>"), font = list(size = 16, color = toRGB("dark blue"))),
             margin = list( t = 150 , b = 90),
             xaxis = list(title = ""),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      add_annotations(x = 1, y = -0.1, text = ~caption, showarrow = F, xref='paper', yref='paper', xanchor='right', font=list(size=12, color="black"))
    
    graph <- ggplot() # Pas de graph mode ggplot pour les boxplot
    
  }
  
  
  if (type_question == "correlations") { # Corrélogramme
    
    if (!is.na(labels_items)) { # Si des labels raccourcis ont été fournis, les utiliser...
      rownames(resultats_qst) <- labels_items
      colnames(resultats_qst) <- labels_items
    }
    
    graph <- ggcorr(resultats_qst, label = TRUE, label_size = 5, label_color = "white",label_round = 2)
    
    # Matrice de significativité des coefficients de corrélations
    pvalue_corr <- rcorr(as.matrix(data_qst))$P
    
    vis <- corrplot(resultats_qst, method = "number", type = "upper", tl.col="black", p.mat = pvalue_corr, sig.level = 0.05)
    # corrplot me semble plus propre visuellement que ggcorr, et permet de rayer les significativités faibles

  }
  
  if (type_question == "ACP") { # Cercle des corrélations
    
    graph <- fviz_pca_var(resultats_qst, title = "Cercle des corrélations sur les 2 premiers axes", repel = TRUE)
    
    vis <- graph # Pas de visualisation intéractive pour l'ACP
 
  }
  
  
  # Ajout du thème et des titres / sous-titres
  graph <- graph + 
    labs(title = titre_question, subtitle = question, caption = caption_graph) + # Rajouter x="", y = "" éventuellement
    theme_GS() 
  
  if (type_question == "barplot circulaire multiple") {
    graph <- graph  +   theme(axis.text.x = element_blank())
  }

  
  
  if(type_question %in% c("pie","donut")) { graph <- graph + theme(axis.ticks=element_blank(),
                                                     axis.text=element_blank(),
                                                     panel.grid=element_blank()) }

  if(type_question =="donut") { 
    graph <- graph + xlim(c(0.5, 2.5))
    vis <- vis %>% add_pie(hole = 0.6)
    }
    
  if(coord_flip == TRUE) { graph <- graph + coord_flip() + scale_x_discrete(limits = rev(levels(resultats_qst$question))) +
    theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 12, face = "bold"),
          panel.grid.major.x =  element_line(size = 0.5, linetype = "dotdash"),
          panel.grid.minor.x = element_line(size = 0.25, linetype = "dotdash"))  }        
  

  # Ajout de l'éventuel facet si variable de groupe et pas de comparaison directe
  if (!is.na(group) & !compare)  {
    if (coord_flip) {
      graph <- graph + facet_wrap(group~.,ncol = 1)
    }
    else {
      graph <- graph + facet_wrap(group~.) }}
  
  
  if(stack) { graph <- graph + scale_x_discrete(NULL)} 
  

  # Ajout de la couleur pour les graphiques ggplot
  if(length(color) > 1) {graph <- graph + scale_fill_manual(values = color)}


  
  #---------------------------------------------------------# 
  #------------ Tests statistiques (éventuels) -------------#
  #---------------------------------------------------------# 
  
  # Objectif de cette sous-partie : 
  #   récupérer dans test_qst les résultats d'un /de test(s) statistique(s) sur variables croisées
  
  
  # Tests statistiques uniquement si résultats croisés
  if(!is.na(group)) {
    
    if (type_question == "barplot multiple") { # Barplot multiple : tests du khi2 pour chaque item
      
  
      test_qst <- resultats_qst %>%
        select(group, question, reponse, n) %>% 
        ungroup() %>% 
        pivot_wider(names_from = "group", values_from = "n", values_fill = 0) %>% 
        group_split(question)  %>% 
        map_df(~select(.x, -question, -reponse) %>% chisq_test()) %>% 
        bind_cols(question = sous_questions) # Note : réutilisation ici du vecteur sous-question calculé au début
  
    }
    
    
  if(type_question == "barplot") {  # Barplot simple : 1 test du khi 2
      
        test_qst <- resultats_qst %>%
          select(group,  reponse, n) %>% 
          ungroup() %>% 
          pivot_wider(names_from = "group", values_from = "n", values_fill = 0) %>% 
          select(-reponse) %>% 
          chisq_test() 
   
      }
   
      
    
    if (type_question == "boxplot") { # Boxplot : Anova pour chaque item
      # A faire
      
      test_qst <- data_qst %>% 
        group_split(question) %>% 
        map_df( ~tidy(aov(reponse ~ group, data = .)) %>% filter(term=="group")) %>% 
        bind_cols(question = sous_questions) %>% # Note : réutilisation ici du vecteur sous-question calculé au début
        #mutate(signif = stars.pval(p.value)) %>%   # Ajout de la significativité à partir de la pvalue (package gtools)
        #mutate(signif = ifelse(signif == " ", "ns", signif))
        mutate(signif =  signif_from_pvalue(p.value))  # Ajout de la significativité avec fonction personnalisée
    }
    
  }

  #---------------------------------------------------------# 
  #------------ Construction du / des tableaux -------------#
  #---------------------------------------------------------# 
  
  # Objectif de cette sous-partie :
  #   Récupérer dans tab les données brutes sous formes de tableaux markdown, éventuellement enrichis de tests
  
  
  data_tab <- resultats_qst
  
  
  if (!type_question %in% c("histogram","boxplot", "correlations", "ACP" )) { # Tableaux "classiques"
    
    data_tab <- resultats_qst %>% ungroup()
    
    if (!is.na(group)) { 
      data_tab <- data_tab %>% group_by(group)   # Group by par la variable de groupe
    }
    
    # Petit retraitement pour les barplot multiple (suppression variable x, et group par question)
    if (type_question== "barplot multiple") { 
      data_tab <- data_tab %>% 
        group_by(question) %>% 
        select(-x)
      
      if (!is.na(group)) { 
        data_tab <- data_tab %>% group_by(group, question)   # Group by par sous-question ET var de groupe
      }
      
    }

    # Calcul du / des top
    top <- data_tab %>% 
      filter(reponse %in% levels(reponse)[1])  %>%  # slice(1) marche pas si niveau sans donnée
      summarise(
        n = sum(n),
        percent = sum(percent)) %>% 
      mutate(reponse = "Top")
    
    # Calcul du / des top2
    top2 <- data_tab %>% 
      filter(reponse %in% levels(reponse)[1:2])  %>%  # slice(1:2) marche pas si niveau sans donnée
      summarise(
        n = sum(n),
        percent = sum(percent)) %>% 
      mutate(reponse = "Top2")
    
    # Calcul des totaux  
    total <- data_tab %>% 
      summarise(
        n = sum(n),
        percent = round(sum(percent))) %>% # Totaux arrondis à l'unité pour éviter les 99.9
      mutate(reponse = "Total")
    
    # Ajout des sous-totaux, si nécessaire
    if (sous_total == TRUE) {
      data_tab <- data_tab %>% bind_rows(top, top2)
    }
    
    # Ajout des totaux 
    data_tab <- data_tab %>% bind_rows(total)
    
    
    # Affichage des pourcentages format %
    #tab <- mutate(tab, percent = percent / 100)
    



    
    # Si pas de tri croisé : supprimer la variable group
    if (is.na(group)) {
      data_tab <- data_tab %>% ungroup() %>% select(-group)
      nb_modalites_group <- 1
    }
    
    # Si tri croisé : Repasser la variable groupe en colonne et conserver les modalités / le nb de modalités
    if (!is.na(group)) {
      
      data_tab <- data_tab %>%
        pivot_longer(cols = c("n", "percent")) %>%  # On met tout en longueur d'abord, pour paramétrer l'ordre "n puis %"
        pivot_wider(names_from = c("group", "name"), values_from = c("value"), names_sort = TRUE)
     
      modalites_group <- levels(fct_drop(data_qst$group)) # modalités dans la variable de groupe (en supprimant les niveaux sans données)
      nb_modalites_group <- nlevels(fct_drop(data_qst$group))   # Nombre de modalités
      

    }
    
    # Transformer les NA en 0
    data_tab[is.na(data_tab)] <- 0
      #data_tab <- data_tab %>% map_df(~replace_na(.x, 0)) # Alternative tidyR génère warning
    
    # Barplot multiple non croisé : on enlève le nombre de répondants
    # if(type_question %in% c("barplot multiple", "barplot circulaire multiple") & is.na(group)) {
    #  tab  <- select(tab, -Nb_reponses) %>% arrange(question)}
    #}
    
    col_names <- c("Réponse", rep(c("n", "%"), nb_modalites_group)) # Vecteur des noms de colonnes
    
    num_colonnes_n <- seq(2,length(col_names), 2) # Numéros de colonnes avec n
    num_colonnes_percent <- seq(3,length(col_names), 2) # Numéros de colonnes avec pourcentages
    
    # Vecteur pour rajouter les libellés de groupe
    if (!is.na(group)) {
      header_group <- c(1, rep(2, nb_modalites_group)) 
      names(header_group) <- c(" ", modalites_group) 
    }
    
    
    # Construction d'un tableau simple
    if (type_question %in% c("barplot","pie","donut")) {
      
      tab <-  data_tab %>% 
        kbl(col.names = col_names,  caption = question) %>%   
        kable_minimal(full_width = F, position = "left") %>%
        row_spec(nrow(data_tab), color = "white", background = "black") %>%  # Formatage total  
        column_spec(num_colonnes_n, italic = TRUE) %>% # Formatage Nombre de répondants
        column_spec(num_colonnes_percent, bold = TRUE)   # Formatage pourcentages 
      
      # Ajout des intitulés de groupe si nécessaire
      if (!is.na(group)) {
        tab <-  tab %>%
          add_header_above(header_group) 
      }
      
      
      if (sous_total == TRUE) {
        tab <- tab %>% row_spec((nrow(data_tab)-2):(nrow(data_tab)-1), color = "white", background = "seagreen")   # Formatage sous-totaux
      }
      
      # Ajout de la significativité du khi2 en note de bas de page, si tests statistiques
      if (!is.na(group)) {
          tab <- footnote(tab, paste0("Significativité du test du khi2 : ", test_qst[1, "p.signif"], " (p.value = ", test_qst[1, "p"], ")"))
      }
      
      
    }
    
    # Construction d'un tableau multiple
    if (type_question == "barplot multiple") {
      
      tab <- data_tab %>% group_by(question) %>% 
        group_split() %>% 
        map(~kbl(.x[,-1], col.names = col_names, caption = unique(.x$question)) %>% 
              kable_minimal(full_width = F, position = "left") %>% 
              column_spec(num_colonnes_n, italic = TRUE) %>%
              column_spec( num_colonnes_percent, bold = TRUE)  %>%  
              row_spec(nrow(.x), color = "white", background = "black") %>% 
              row_spec((nrow(.x)-2):(nrow(.x)-1), color = "white", background = "seagreen"))
      
      # Ajout des significativités en notes de bas de page, si tests statistiques
      if (!is.na(group)) {
        for(i in 1:length(tab)) { # La boucle ici me parait plus simple que le map !
          tab[[i]] <- footnote(tab[[i]], paste0("Significativité du test du khi2 : ", test_qst[i, "p.signif"], " (p.value = ", test_qst[i, "p"], ")"))
        }
      }
      
      # Ajout des intitulés de groupe si tris croisés
      if (!is.na(group)) {
        tab <-  tab %>% map(~add_header_above(.x, header_group)) 
      }
      
      # ---------Tableau récapitulatif sur les top2
      if (sous_total == TRUE) {
        
        col_names_recap <- col_names
        if(!is.na(group)) {
          col_names_recap <- c(col_names, "test") # Ajout du résultat du test statistique en cas de tri croisé
          header_group_recap <- c(header_group, 1) # idem pour intitulé de groupe
          names(header_group_recap) <- c(names(header_group), " ")
        }
        
        data_tab_recap <- filter(data_tab, reponse =="Top2") %>% select(-reponse) 
        
        if(!is.na(group)) { # Ajout des infos du test statistique en cas de tri croisés
          data_tab_recap <- data_tab_recap %>% 
            left_join(test_qst, by = "question") %>% 
            mutate(test = paste0(p.signif, " (p = ", round(p,3),")")) %>% 
            select(- statistic, -p, -df, -method, -p.signif, -n) %>% 
            mutate(question = paste0(question, " [top2]")) 
        }
        
        tab_recap <-  data_tab_recap %>% 
          kbl(caption = paste("[Recap top2]",question), col.names = col_names_recap ) %>% 
          kable_minimal(full_width = F, position = "left") %>%
          column_spec(num_colonnes_n, italic = TRUE) %>% # Formatage Nombre de répondants
          column_spec(num_colonnes_percent, bold = TRUE)  %>%  # Formatage pourcentages 
          row_spec(0, color = "white", background = "seagreen") 
        

        
        # Ajout des intitulés de groupe du recap si tris croisés
        if (!is.na(group)) {
          tab_recap <-  tab_recap %>% add_header_above(header_group_recap)
        }
        tab[[length(tab) + 1]] <- tab_recap # Ajout du tableau recap à la liste de tableaux
      }
      
      
      
      
    }
    

    
  }
  
  if (type_question== "histogram") {
    tab <- data_tab %>% kbl(caption = question) %>% 
      kable_minimal(full_width = F, position = "left")   %>% 
      column_spec(1, italic = TRUE) %>%
      column_spec(2:3, bold = TRUE) 
  }
  
  if (type_question== "boxplot") {
    
    # Si pas de tri croisé
    if (is.na(group)) {
    tab <- data_tab %>% 
      kbl(caption = question)    %>% 
      kable_minimal(full_width = F, position = "left")  %>% 
      column_spec(2, italic = TRUE) %>%
      column_spec(3, bold = TRUE) 
    }
    
    # Si tri croisé : Repasser la variable groupe en colonne, garder que moyenne et n, et conserver les modalités / le nb de modalités
    if (!is.na(group)) {
      
      data_tab <- data_tab %>%
              select(-min, -max, -mediane, -sd)%>%
              pivot_longer(cols = c("n", "moyenne")) %>%  # On met tout en longueur d'abord, pour paramétrer l'ordre "n puis %"
               pivot_wider(names_from = c("name", "group"), values_from = c("value"))

      modalites_group <- levels(fct_drop(data_qst$group)) # modalités dans la variable de groupe (en supprimant les niveaux sans données)
      nb_modalites_group <- nlevels(fct_drop(data_qst$group))   # Nombre de modalités
      
      col_names <- c("Réponse", rep(c("n", "moyenne"), nb_modalites_group), "test") # Vecteur des noms de colonnes
      num_colonnes_n <- seq(2,length(col_names), 2) # Numéros de colonnes avec n
      num_colonnes_moyenne <- seq(3,length(col_names), 2) # Numéros de colonnes avec moyennes
      
      # Intitulés de groupe 
      header_group <- c(1, rep(2, nb_modalites_group), 1) 
      names(header_group) <- c(" ", modalites_group, " ") 
      
      # Tableau avec variables de groupe
      tab <- data_tab %>% 
        left_join(test_qst, by = "question") %>% 
        mutate(test = paste0(signif, " (p = ", round(p.value,3),")")) %>% 
        select(-df, -term, -sumsq, -meansq, -statistic, -p.value, - signif) %>% 
        kbl(caption = question, col.names = col_names)    %>% 
        kable_minimal(full_width = F, position = "left")  %>% 
        column_spec(num_colonnes_n, italic = TRUE) %>%
        column_spec(num_colonnes_moyenne, bold = TRUE) %>%
        column_spec(max(num_colonnes_moyenne) +1, italic = TRUE) %>%
        add_header_above(header_group)

      #tab <- data_tab %>% kbl(caption = question, col.names = col_names)    %>% 
      #  kable_minimal(full_width = F, position = "left")  %>% 
      #  column_spec(num_colonnes_n, italic = TRUE) %>%
      #  column_spec(num_colonnes_moyenne, bold = TRUE) %>%
      # add_header_above(header_group)
      
    }

  }

  if (type_question== "correlations") { # Rien de spécifique en "tableau" pour la corrélations, la visualisation remplit déjà ce rôle
    tab <- kbl(data_tab, caption = "Corrélations") %>% 
      kable_minimal(full_width = F, position = "left") 
  } 
  
  if (type_question== "ACP") { # ACP : afficher les valeurs propres et contributions des variables
    
    tab <- list()
    
    tab[[1]] <- data_tab$eig %>% round(2) %>% 
      kbl(caption = "Valeurs propres", col.names = c("Valeur propre", "% de variance", "% cumulé de variance")) %>% 
      kable_minimal(full_width = F, position = "left") 

    tab[[2]] <- data_tab$var$coord %>% round(2) %>% 
      kbl(caption = "Coordonnées des variables par axe") %>% 
      kable_minimal(full_width = F, position = "left") 
        
    tab[[3]] <- data_tab$var$contrib %>% round(1) %>% 
      kbl(caption = "Contribution des variables par axe") %>% 
      kable_minimal(full_width = F, position = "left") 
  } 
  

  

  return(list(data_qst = data_qst, resultats_qst = resultats_qst, nb_na = nb_na, question = question, nb_reponses_qst = nb_reponses_qst, cible = cible, graph = graph, vis = vis, tab = tab))
}


# Petite fonction renvoyant le degré de significativté d'un test à partir d'un vecteur de pvalue
signif_from_pvalue <- function(pvalue) {
  if_else(pvalue <= 0.001, "***",
          if_else(pvalue <= 0.01, "***",
                  if_else (pvalue <= 0.05,"*","ns")))
}


#signif_from_pvalue <- function(pvalue) {
#  if (pvalue <= 0.001) return("***")
#  if (pvalue <= 0.01) return("***")
#  if (pvalue <= 0.05) return("*")
#  else ("ns")
#}




