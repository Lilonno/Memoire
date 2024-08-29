library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(shinythemes)
library(tibble)
library(kableExtra)
library(car)
library(lmtest)
library(sandwich)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(shiny)


# Define UI for application
ui <- fluidPage(
  theme = shinytheme("lumen"),
  tags$head(
    tags$style(HTML("
      /* Ajout de bordures aux onglets */
      .tabbable > .nav-tabs > li > a {
        border: 2px solid #ccc;
        border-radius: 5px;
      }

      /* Changement de couleur au survol des onglets */
      .tabbable > .nav-tabs > li > a:hover {
        background-color: #f0f0f0;
      }

      /* Ajout de marges autour des onglets */
      .tabbable > .nav-tabs {
        margin-bottom: 20px;
      }

      /* Augmentation de la taille du texte des onglets */
      .tabbable > .nav-tabs > li > a {
        font-size: 16px;
      }
    "))
  ),
  titlePanel(
    HTML("<h1 style='font-weight: bold; font-size: 36px;'>Déterminant de la survie du bagne de Cayenne</h1>
       <p style='font-size: 18px;'>Onno Lilou 2024-2025</p>")
  ),
  sidebarLayout(
    sidebarPanel(
      h3("Navigation"),
      tags$hr(),
      tabsetPanel(
        id = "onglet",
        tabPanel("Environnement économique", 
                 h4("Environnement économique"),
                 p(""),
                 HTML("<ol style='padding-left: 20px; font-size: 18px; margin-bottom: 20px;'>
                <li style='font-size: 22px; font-weight: bold;'>Création du bagne
                  <ol style='padding-left: 50px; font-size: 18px; font-weight: normal;'>
                    <li>Dangereux pour la population civile</li>
                    <li>Dimension économique des bagnes coloniaux</li>
                   
                  </ol>
                </li>
                <li style='font-size: 22px; font-weight: bold; margin-top: 20px;'>Les théories qui ont émergé
                  <ol style='padding-left: 50px; font-size: 18px; font-weight: normal;'>
                    <li>César Lombroso - Théorie du criminel né</li>
                    <li>Premier congrés d'anthropologie du crime</li>
                    <li>Distinction entre crime d’habitude et crime d’accident</li>
                    <li>Alphonse Bertillon - technique d’identification</li>
                    <li>Les conséquences de toutes ces théories</li>
                  </ol>
                </li>
                <li style='font-size: 22px; font-weight: bold; margin-top: 20px;'>La réalité sur les bagnes
                  <ol style='padding-left: 50px; font-size: 18px; font-weight: normal;'>
                    <!-- Ajoutez ici les sous-parties de la réalité sur les bagnes -->
                  </ol>
                </li>
              </ol>")),
        
        tabPanel("Base de données", 
                 p(""),
                 HTML("<ol style='padding-left: 20px; font-size: 18px; margin-bottom: 20px;'>
                <li style='font-size: 22px; font-weight: bold;'>Source : le site des Archives nationales d’outre-mer (ANOM)</li>
                <li style='font-size: 22px; font-weight: bold; margin-top: 20px;'>Technique d'échantillonnage aléatoire
                  <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                    <li>Chaque individu ait une chance égale d'être choisi</li>
                    <li>150 individus</li>
                    <li>Année : 1884-1886</li>
                  </ol>
                </li>
                <li style='font-size: 22px; font-weight: bold; margin-top: 20px;'>Seconde sélection
                  <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                    <li>59 à 22 variables</li>
                  </ol>
                </li>
              </ol>")),
        
        tabPanel("Statistique descriptive", 
                 p(""),
                 HTML("<ol style='padding-left: 20px; font-size: 18px; margin-bottom: 20px;'>
                <li style='font-size: 22px; font-weight: bold;'>Codage des variables</li>
                <li style='font-size: 22px; font-weight: bold; margin-top: 20px;'>Statistique descriptive
                  <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                    <li>Des variables Qualitatives</li>
                    <li>Des variables Quantitatives
                      <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                        <li>Détection des Outliers avec les Boxplots</li>
                        <li>Test ESD et de Ramsey</li>
                        <li>La technique du 1 et 99ème percentile</li>
                      </ol>
                    </li>
                  </ol>
                </li>
              </ol>")),
        
        tabPanel("Méthodologie économétrique", ""),
        tabPanel("Application théorique", 
                 p(""),
                 HTML("<ol style='padding-left: 20px; font-size: 18px; margin-bottom: 20px;'>
                <li style='font-size: 22px; font-weight: bold;'>Régression linéaire multiple sur la variable dépendante : Mort au bagne
                  <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                    <li>Estimation robustes des erreurs standards</li>
                    <li>Analyse des résultats</li>
                  </ol>
                </li>
                <li style='font-size: 22px; font-weight: bold; margin-top: 20px;'>Vérification des VIF
                  <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                    <li>Suppression de la variable Célibataire</li>
                  </ol>
                </li>
                <li style='font-size: 22px; font-weight: bold; margin-top: 20px;'>Création d'un nouveau modèle sans la variable célibataire
                  <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                    <li>Vérification des coefficients pour l'influence de la variable célibataire</li>
                    <li>Estimation robustes des erreurs standards</li>
                    <li>Analyse des résultats</li>
                  </ol>
                </li>
              </ol>")),
        tabPanel("Conclusion et discussion", 
                 p(""),
                 HTML("<ol style='padding-left: 20px; font-size: 18px; margin-bottom: 20px;'>
              <li style='font-size: 22px; font-weight: bold;'>Conclusion
                <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                  <li>Résumé des variables significatives
                    <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                      <li>Mort au bagne</li>
                      <li>Mauvaise conduite</li>
                      <li>Nombre de condamnations antérieures</li>
                    </ol>
                  </li>
                </ol>
              </li>
              <li style='font-size: 22px; font-weight: bold; margin-top: 20px;'>Discussion
                <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                  <li>Création de la base de données - Informations manquantes</li>
                  <li>Technique d'échantillonnage - Convergence de la moyenne</li>
                  <li>Littérature économique - Divergence d'idées</li>
                </ol>
              </li>
              <li style='font-size: 22px; font-weight: bold; margin-top: 20px;'>Ouverture
                <ol style='padding-left: 20px; font-size: 18px; font-weight: normal;'>
                  
                </ol>
              </li>
            </ol>")
        
      ))),
        
    mainPanel(
      uiOutput("selected_tab_content"),
    )
  )
)





server <- function(input, output, session) {
  
  base <- read_excel("nouvelle_base.xlsx", 
                     col_types = c("text", "text", "numeric", 
                                   "text", "text", "text", "text", "numeric", 
                                   "text", "text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "text", "text", "numeric", 
                                   "text", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric"))
  
  # Statistique descriptive
  binary_vars <- c("Signe_handicapant", "Lire_ecrire", "Mort_bagne", "Mauvaise_conduite", 
                   "Nmbr_mois_prison_sup_96", "Oui_condamn_ant", "profession_elevee", 
                   "profession_moderee", "profession_faible", "yeux_fonce", "yeux_clair", 
                   "nationalite_france", "Divorcé", "Marier", "Veuve", 
                   "Célibataire")
  
  percentages <- base %>%
    select(all_of(binary_vars)) %>%
    summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "percentage")
  
  quantitative_vars <- c("Age", "Taille", "Nmbr_enfant", "Nmbr_conda_bagne")
  
  averages <- base %>%
    select(all_of(quantitative_vars)) %>%
    summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "average")
  
  histograms <- lapply(quantitative_vars, function(var) {
    ggplot(base, aes_string(x = var)) +
      geom_histogram(binwidth = 1, fill = "#7199B2", color = "black") +
      labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
      theme_minimal()
  })
  
  
  
  boxplot_list <- lapply(quantitative_vars, function(var) {
    ggplot(base, aes_string(y = var)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", var), y = var) +
      theme_minimal()
  })
  
  # Application théorique
  modelD1 <- lm(Mort_bagne ~ Age + Lire_ecrire + Signe_handicapant + yeux_clair + Célibataire + Marier + Taille + Divorcé + Nmbr_enfant + profession_elevee + profession_faible + nationalite_france + Oui_condamn_ant + Nmbr_mois_prison_sup_96, data = base)
  
  modelD2 <- lm(Mort_bagne ~ Age + Signe_handicapant + Taille + Lire_ecrire + yeux_clair + Célibataire + Marier + Divorcé + Nmbr_enfant + nationalite_france + Oui_condamn_ant + Nmbr_mois_prison_sup_96, data = base)
  
  model_summary_table <- function(model) {
    coef_table <- summary(model)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "Variable")
    return(coef_table)
  }
  
  formatSignifColumn <- function(data, p_value_column_name) {
    significance <- ifelse(data[, p_value_column_name] < 0.1 & data[, p_value_column_name] >= 0.05, "*", 
                           ifelse(data[, p_value_column_name] < 0.05 & data[, p_value_column_name] >= 0.01, "**", 
                                  ifelse(data[, p_value_column_name] < 0.01, "***", "")))
    data$Significance <- significance
    return(data)
  }
  
  
  output$regression_table1 <- renderDT({
    coef_table <- model_summary_table(modelD1) %>%
      mutate(across(where(is.numeric), ~ round(., 6))) %>%
      formatSignifColumn("Pr(>|t|)")
    datatable(coef_table, options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"))
  })
  
  output$regression_table2 <- renderDT({
    coef_table <- model_summary_table(modelD2) %>%
      mutate(across(where(is.numeric), ~ round(., 6))) %>%
      formatSignifColumn("Pr(>|t|)")
    datatable(coef_table, options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"))
  })
  
  output$vif_output1 <- renderPrint({
    vif(modelD1)
  })
  
  output$vif_output2 <- renderPrint({
    vif(modelD2)
  })
  
  modelD3 <- lm(Mort_bagne ~ Age + Lire_ecrire + Signe_handicapant + Taille + yeux_clair + Marier + Divorcé + Nmbr_enfant + profession_elevee + profession_faible + nationalite_france + Oui_condamn_ant + Nmbr_mois_prison_sup_96, data = base)
  
  modelD4 <- lm(Mort_bagne ~ Age + Signe_handicapant + Lire_ecrire + Taille + yeux_clair + Marier + Divorcé + Nmbr_enfant + nationalite_france + Oui_condamn_ant + Nmbr_mois_prison_sup_96, data = base)
  
  output$regression_table3 <- renderDT({
    coef_table <- model_summary_table(modelD3) %>%
      mutate(across(where(is.numeric), ~ round(., 6))) %>%
      formatSignifColumn("Pr(>|t|)")
    datatable(coef_table, options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"))
  })
  
  output$regression_table4 <- renderDT({
    coef_table <- model_summary_table(modelD4) %>%
      mutate(across(where(is.numeric), ~ round(., 6))) %>%
      formatSignifColumn("Pr(>|t|)")
    datatable(coef_table, options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"))
  })
  
  output$robuste_table3 <- renderDT({
    robust_modelD3 <- coeftest(modelD3, vcov = vcovHC(modelD3, type = "HC1"))
    coef_table <- data.frame(Variable = rownames(robust_modelD3), 
                             Estimate = round(robust_modelD3[, 1], 6),
                             `Robust Std. Error` = round(robust_modelD3[, 2], 6),
                             `t-value` = round(robust_modelD3[, 3], 6),
                             `Pr(>|t|)` = round(robust_modelD3[, 4], 6))
    datatable(coef_table[, -1], options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"))
  })
  
  output$robuste_table4 <- renderDT({
    robust_modelD4 <- coeftest(modelD4, vcov = vcovHC(modelD4, type = "HC1"))
    coef_table <- data.frame(Variable = rownames(robust_modelD4), 
                             Estimate = round(robust_modelD4[, 1], 6),
                             `Robust Std. Error` = round(robust_modelD4[, 2], 6),
                             `t-value` = round(robust_modelD4[, 3], 6),
                             `Pr(>|t|)` = round(robust_modelD4[, 4], 6))
    datatable(coef_table[, -1], options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"))
  })

  
  #RESUME - TABLEAU 
  
  # Rendu des tableaux de résumé
  output$summary_table1 <- renderDataTable({
    data <- data.frame(
      "Variables significatives" = c("Lire et écrire", "Profession faible", "Nmbrs de mois de prison supérieur à 8 ans"),
      "Taux de significativité" = c("5%", "10%", "10%")
    )
    datatable(data, 
              options = list(paging = FALSE, searching = FALSE),
              caption = "Tableau 1 : Modèle n°1")
  })
  
  output$summary_table2 <- renderDataTable({
    data <- data.frame(
      "Variables significatives" = c("Lire et écrire", "Marier","Profession faible","Nmbrs de mois de prison supérieur à 8 ans"),
      "Taux de significativité" = c("5%", "5%", "10%","10%")
    )
    datatable(data, 
              options = list(paging = FALSE, searching = FALSE),
              caption = "Tableau 2 : Modèle n°1 bis sans 'Célibataire'")
  })
  
  output$summary_table3 <- renderDataTable({
    data <- data.frame(
      "Variables significatives" = c("Age", "Lire et écrire", "Marier", "Nombre d'enfants", "Profession faible","Nmbrs de mois de prison supérieur à 8 ans"),
      "Taux de significativité" = c("10%", "5%", "5%", "10%", "10%", "5%")
    )
    datatable(data, 
              options = list(paging = FALSE, searching = FALSE),
              caption = "Tableau 3 : Modèle n°1 bis avec estimation robuste des erreurs standards")
  })
  
  #RESUME TABLEAU CONCLUSION 
  output$MBtable1 <- renderDataTable({
    data <- data.frame(
      "Variables significatives" = c("Age", "Lire et écrire", "Marier", "Nombre d'enfants", "Profession faible","Nmbrs de mois de prison supérieur à 8 ans"),
      "Taux de significativité" = c("10%", "5%", "5%", "10%", "10%", "5%")
    )
    datatable(data, 
              options = list(paging = FALSE, searching = FALSE),
              caption = "Modèle n°1 bis : Mort au bagne")
  })
  
  output$MBtable2 <- renderDataTable({
    data <- data.frame(
      "Variables significatives" = c("Lire et écrire", "Marier", "Nombre d'enfants", "Nationalité France métropolitaine","Oui a eu une condamnation antérieur","Nmbrs de mois de prison supérieur à 8 ans"),
      "Taux de significativité" = c("5%", "10%", "5%", "10%", "10%", "5%")
    )
    datatable(data, 
              options = list(paging = FALSE, searching = FALSE),
              caption = "Modèle n°2 bis : Mort au bagne")
  })
  
  output$MCtable1 <- renderDataTable({
    data <- data.frame(
      "Variables significatives" = c("Taille", "Signe handicapant", "Age","Profession faible"),
      "Taux de significativité" = c("5%", "10%", "10%", "10%")
    )
    datatable(data, 
              options = list(paging = FALSE, searching = FALSE),
              caption = "Modèle n°3 : Mauvaie conduite")
  })
  
  output$NCBtable1 <- renderDataTable({
    data <- data.frame(
      "Variables significatives" = c("Profession physiquement exigeante"),
      "Taux de significativité" = c("5%")
    )
    datatable(data, 
              options = list(paging = FALSE, searching = FALSE),
              caption = "Modèle n°5 : Nombre de condamnation durant le bagne")
  })
  
  output$selected_tab_content <- renderUI({
    switch(input$onglet,
           "Environnement économique" = 
             tabsetPanel(
               id = "histoire_subtab",
               div(
                 style = "text-align: center;",
                 p(style = "font-weight: bold; font-size: 24px;", style = "font-weight: bold;", "Quels sont les déterminants personnels des détenus du bagne de Cayenne susceptibles d’influencer leur survie ? "),
                 img(src = "https://journals.openedition.org/criminocorpus/docannexe/file/3495/le_temps_des_bagnes.jpg", width = "50%")
                
               )),
           
           "Base de données" = tabsetPanel(
             id = "bdd_tab",
             div(
               style = "text-align: center;",
               p(style = ""),
               img(src = "https://scribavita.fr/user/pages/01.blog/20141218-bagnard.guyane/bagnard.guyane.2.JPG", width = "80%")
             )
           ),
           
           "Statistique descriptive" = tabsetPanel(
             id = "stat_desc_tab",
             tabPanel("Variables qualitative",
                      p(style = "font-size: 20px; font-weight: bold;", "Statistique des variables Qualitatives"),
                      DT::dataTableOutput("percentage_table_qualitative")),
             tabPanel("Variables quantitatives",
                      p(style = "font-size: 20px; font-weight: bold;", "Statistique des variables Quantitatives"),
                      DT::dataTableOutput("average_table_quantitative"),
                      tabsetPanel(
                        
                        tabPanel("Age", plotOutput("histogram_age")),
                        tabPanel("Taille", plotOutput("histogram_taille")),
                        tabPanel("Nombre d'enfant", plotOutput("histogram_nmbr_enfant")),
                        tabPanel("Nombre de condamnation durant le bagne", plotOutput("histogram_nmbr_conda_bagne"))
                      ),
                      
             
             ),
             tabPanel("Valeur aberrante", 
                      p(style = "font-size: 20px; font-weight: bold;", "Détection des valeurs atypiques"),
                      uiOutput("boxplots_ui"))
           )
    ,
           tabPanel("Méthodologie économétrique", 
                    tabsetPanel(
                      id = "metho_table",
                      tabPanel("Hypothèse Fondamentale",
                               p(style = "font-size: 20px; font-weight: bold;", "Hypothèses Fondamentales de Gauss-Markov comprennent :"),
                               HTML("<ol style='padding-left: 100px; font-size: 16px;'>
                            <li><b>H1 :</b> Linéarité : Les relations entre les variables sont linéaires.</li> <br>
                            <li><b>H2 :</b> Aléatoire : Les erreurs de modèle sont aléatoires et non systématiques.</li> <br>
                            <li><b>H3 :</b> Indépendance : Les erreurs sont indépendantes les unes des autres.</li> <br>
                            <li><b>H4 :</b> Homoscédasticité : La variance des erreurs est constante.</li> <br>
                            <li><b>H5 :</b> Non-colinéarité : Les variables explicatives ne sont pas fortement corrélées entre elles. </li> <br>
                            <li><b>H6 :</b> Espérance nulle : La moyenne des erreurs est nulle. </li>
                          </ol>")
                      ),
           
                    
           
           
             tabPanel("Modèle",
                      fluidRow(
                        column(6, 
                               h4(style = "font-size: 20px; font-weight: bold;","Modèle n°1 : incluant les variables « professions »"),
                               p("Yi = α + β2X2i + β3X3i + β4X4i + β5X5i + β6X6i + β7X7i + β8X8i + β9X9i + β10X10i + β11X11i + β12X12i + β13X13i + β14X14i + β15X15i εi"),
                               p("avec :"),
                               HTML("<ul>
             <li>α = estimateur de a</li>
             <li>βi = estimateur de bi</li>
             <li>Xi = variables indépendantes du modèle</li>
             <li>εi = résidu, terme d’erreur, avec i ∈ [1 ;150]</li> <br>
             <li>Yi = Mort au bagne / Mauvaise conduite / Nombre de condamnation durant le bagne</li>
             <li>X2 = Âge</li>
             <li>X3 = Avoir des signes handicapants</li>
             <li>X4 = Savoir lire et écrire</li>
             <li>X5 = Avoir des yeux de couleur clairs</li>
             <li>X6 = Taille</li>
             <li>X7 = Célibataire</li>
             <li>X8 = Marié</li>
             <li>X9 = Divorcé</li>
             <li>X10 = Nombre d’enfant</li>
             <li>X11 = Profession à un niveau faible physiquement durant le bagne</li>
             <li>X12 = Profession à un niveau élevé physiquement durant le bagne</li>
             <li>X13 = Individu de nationalité de France métropolitaine</li>
             <li>X14 = Ayant été condamné antérieurement</li>
             <li>X15 = Ayant été condamné à plus de 96 mois de prison</li>
           </ul>")
                        ),
                        column(6, 
                               h4(style = "font-size: 20px; font-weight: bold;","Modèle n°2 : n’incluant pas les variables « professions »"),
                               p("Yi = α + β2X2i + β3X3i + β4X4i + β5X5i + β6X6i + β7X7i + β8X8i + β9X9i + β10X10i + β11X11i + β12X12i + β13X13i εi"),
                               p("avec :"),
                               HTML("<ul>
             <li>α = estimateur de a</li>
             <li>βi = estimateur de bi</li>
             <li>Xi = variables indépendantes du modèle</li>
             <li>εi = résidu, terme d’erreur, avec i ∈ [1 ;150]</li> <br>
             <li>Yi = Mort au bagne / Mauvaise conduite / Nombre de condamnation durant le bagne</li>
             <li>X2 = Âge</li>
             <li>X3 = Avoir des signes handicapants</li>
             <li>X4 = Savoir lire et écrire</li>
             <li>X5 = Avoir des yeux de couleur clairs</li>
             <li>X6 = Taille</li>
             <li>X7 = Célibataire</li>
             <li>X8 = Marié</li>
             <li>X9 = Divorcé</li>
             <li>X10 = Nombre d’enfant</li>
             <li>X11 = Individu de nationalité de France métropolitaine</li>
             <li>X12 = Ayant été condamné antérieurement</li>
             <li>X13 = Ayant été condamné à plus de 96 mois de prison</li>
           </ul>")
                        )
                      )
             ))),
             
           "Application théorique" = tabsetPanel(
             id = "appt_table",
             tabPanel("Régression linéaire multiple", 
                      p(style = "font-size: 20px; font-weight: bold;", "Modèle n°1 RLM : Mort au bagne"),
                      DTOutput("regression_table1"), 
                      p(style = "font-size: 20px; font-weight: bold;", "Modèle n°2 RLM : Mort au bagne (sans les variables 'professions')"),
                      DTOutput("regression_table2")),
             tabPanel("VIF",
                      p(style = "font-size: 20px; font-weight: bold;", "Modèle n°1 VIF"),
                      verbatimTextOutput("vif_output1"),
                      p(style = "font-size: 20px; font-weight: bold;", "Modèle n°2 VIF"),
                      verbatimTextOutput("vif_output2")),
             tabPanel("Régression linéaire multiple bis",
                      p(style = "font-size: 20px; font-weight: bold;", "Modèle n°1 bis RLM : Mort au bagne sans 'Célibataire'"),
                      DTOutput("regression_table3"),
                      p(style = "font-size: 20px; font-weight: bold;", "Modèle n°2 bis RLM : Mort au bagne sans 'Célibataire (sans les variables 'professions')"),
                      DTOutput("regression_table4")),
             tabPanel("Estimation robuste des erreurs standards", 
                      p(style = "font-size: 20px; font-weight: bold;", "Modèle n°1 bis RLM : Mort au bagne estimateur robuste des s.e"),
                      DTOutput("robuste_table3"),
                      p(style = "font-size: 20px; font-weight: bold;", "Modèle n°2 bis RLM : Mort au bagne estimateur robuste des s.e (sans les variables 'professions')"),
                      DTOutput("robuste_table4")),
             tabPanel("Résumé",
                      fluidRow(
                        column(width = 4, dataTableOutput("summary_table1")),
                        column(width = 4, align = "center", offset = 4, dataTableOutput("summary_table2")),
                        column(width = 4, align = "center", offset = 4, dataTableOutput("summary_table3"))
                      ),
                      fluidRow(
                        column(width = 2),
                        column(width = 2, align = "center", offset = 4, htmlOutput("arrow1")),
                        column(width = 2, align = "center", offset = 4, htmlOutput("arrow2"))
                      ))
           ),
    "Conclusion et discussion" = tabsetPanel(
      id = "appt_table",
      tabPanel("Mort au bagne",
               fluidRow(
                 column(width = 4, dataTableOutput("MBtable1")),
                 column(width = 4, align = "center", offset = 4, dataTableOutput("MBtable2"))
                 
               ),
               fluidRow(
                 column(width = 2),
                 column(width = 2, align = "center", offset = 4, htmlOutput("arrow1")),
                 column(width = 2, align = "center", offset = 4, htmlOutput("arrow2"))
               )),
      tabPanel("Mauvaise conduite et Nombre de condamnation",
               fluidRow(
                 column(width = 4, dataTableOutput("MCtable1")),
                 column(width = 4, align = "center", offset = 4, dataTableOutput("NCBtable1"))
                 
               ),
               fluidRow(
                 column(width = 2),
                 column(width = 2, align = "center", offset = 4, htmlOutput("arrow1")),
                 column(width = 2, align = "center", offset = 4, htmlOutput("arrow2"))
               )),
      
    )
    
    )
  })
  
 
  
  
  

  output$percentage_table_qualitative <- renderDT({
    # Modifier les valeurs directement dans la dataframe
    percentages$variable[percentages$variable == "Signe_handicapant"] <- "<strong>Signe particulier</strong> <br> Signe handicapant"
    percentages$variable[percentages$variable == "Lire_ecrire"] <- "<strong>Instruction</strong> <br> Lire et écrire"
    percentages$variable[percentages$variable == "Mauvaise_conduite"] <- "<strong>Conduite</strong> <br> Mauvaise conduite"
    percentages$variable[percentages$variable == "yeux_fonce"] <- "<strong>Yeux</strong> <br> Yeux fonce"
    percentages$variable[percentages$variable == "yeux_clair"] <- "<strong>Yeux</strong> <br> Yeux clair"
    percentages$variable[percentages$variable == "Nmbr_mois_prison_sup_96"] <- "<strong>Nombre de mois de prison</strong> <br> Nmbr_mois_prison_sup_96"
    percentages$variable[percentages$variable == "Oui_condamn_ant"] <- "<strong>Durée de condamnation antérieur</strong> <br>Oui_condamn_ant"
    percentages$variable[percentages$variable == "profession_elevee"] <- "<strong>Profession durant le bagne</strong> <br>profession_elevee"
    percentages$variable[percentages$variable == "profession_moderee"] <- "<strong>Profession durant le bagne</strong> <br>profession_moderee"
    percentages$variable[percentages$variable == "profession_faible"] <- "<strong>Profession durant le bagne</strong> <br>profession_faible"
    percentages$variable[percentages$variable == "nationalite_france"] <- "<strong>Lieu de naissance</strong> <br> Nationalité france métropolitaine"
    percentages$variable[percentages$variable == "Divorcé"] <- "<strong>Situation matrimoniale</strong> <br>Divorcé"
    percentages$variable[percentages$variable == "Marier"] <- "<strong>Situation matrimoniale</strong> <br>Marier"
    percentages$variable[percentages$variable == "Veuve"] <- "<strong>Situation matrimoniale</strong> <br>Veuve"
    percentages$variable[percentages$variable == "Célibataire"] <- "<strong>Situation matrimoniale</strong> <br>Célibataire"
    
    datatable(percentages, options = list(pageLength = 10, autoWidth = TRUE), escape = FALSE)
  })
  
  
  output$average_table_quantitative <- DT::renderDataTable({
    datatable(averages)
  })
  
  output$histogram_age <- renderPlot({ histograms[[1]] })
  output$histogram_taille <- renderPlot({ histograms[[2]] })
  output$histogram_nmbr_enfant <- renderPlot({ histograms[[3]] })
  output$histogram_nmbr_conda_bagne <- renderPlot({ histograms[[4]] })

  
  
  
  
  
  
  output$average_table_quantitative <- renderDT({
    datatable(averages, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$boxplots_ui <- renderUI({
    plot_output_list <- lapply(1:length(boxplot_list), function(i) {
      plotname <- paste("boxplot", i, sep = "_")
      column(width = 6, plotOutput(plotname, height = "300px", width = "100%"))
    })
    do.call(fluidRow, plot_output_list)
  })
  
  lapply(1:length(boxplot_list), function(i) {
    output[[paste("boxplot", i, sep = "_")]] <- renderPlot({
      boxplot_list[[i]]
    })
  })
}



shinyApp(ui = ui, server = server)
