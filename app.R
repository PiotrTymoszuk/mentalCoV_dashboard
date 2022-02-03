# This script creates a Shiny application viewer for presenting the results 
# of the Mental Health after COVID-19 study. It consists of four tabs:
# 1) Intro with the terms of use
# 2) Answers to the mental health features
# 3) Scoring of OMH, QoL, Anxiety, Depression and Stress
# 4) Prevalence of stress, anxiety and depression defined by the the PHQ cutoffs

# tools -----

    library(shiny)
    library(plyr)
    library(tidyverse)
    library(stringi)
    library(shinyWidgets)
    library(readxl)
    
    source('./tools/counting_tools.R')
    source('./tools/app_tools.R')

# Define UI for application -----
    
    ui <- navbarPage(
        
        selected = 'Welcome', 
        
        tags$head(
            tags$style(
                "
            .title {
                background: url('banner.png');
                background-repeat: no-repeat; 
                background-size: cover;
                font-size: 40px;
                color: #cc3300;
                font-family: Bahnschrift, Verdana, Helvetica;
                text-shadow: 1px 1px #ffece6; 
                padding-left: 3%;
                padding-top: 1%; 
                padding-bottom: 0.05%
            }
            
             .footer {
                background: url('footer.png');
                background-repeat: no-repeat; 
                background-size: cover;
                font-size: 36px;
                color: #cc3300;
                font-family: Bahnschrift, Verdana, Helvetica;
                text-shadow: 1px 1px #ffece6; 
                padding-left: 3%;
                padding-top: 0.5%; 
                padding-bottom: 0.07%
             }
            
            h1 {
               font-size: 34;
               font-weight: bold; 
               font-family: Bahnschrift, Verdana, Helvetica
               }

            h2 {
               font-size: 30;
               font-weight: bold; 
               font-family: Bahnschrift, Verdana, Helvetica
               }
            
             h3 {
               font-size: 24;
               font-weight: bold; 
               font-family: Bahnschrift, Verdana, Helvetica
               }
            
            h4 {
               font-size: 18; 
               font-family: Bahnschrift, Verdana, Helvetica
            }
            
            .intro {
                font-size: 24;
                font-family: Helvetica
            }
            
            .shiny-text-output {
              font_size: 18, 
              font-family: Bahnschrift, Verdana, Helvetica
            }
            
            .shiny-html-output {
              font_size: 18, 
              font-family: Bahnschrift, Verdana, Helvetica
            }
            
            "
            )
        ), 
        
        title = 'Mental Health after COVID-19 in Tyrol', 
        tabPanel(title = 'Welcome', 
                 fluidPage(titlePanel(HTML("<div class = 'title'>
                                            Mental Health after COVID-19 in Tyrol
                                            <img src = '' width = 50%>
                                            <img src = '' width = 6%><br/>
                                            <div/>
                                            <hr style = 'height: 5px'>")), 
                           fluidRow(column(width = 8, 
                                           offset = 2, 
                                           h2('Welcome to the interactive result dashdoard'), 
                                           div(p("The study 'Health after COVID-19 in Tyrol' (ClinicalTrials.gov: ", 
                                               a('NCT04661462', href = 'https://clinicaltrials.gov/ct2/show/NCT04661462?term=Health+after+COVID&draw=2&rank=1'), 
                                               "is a collaborative project between two neighbor central European regions: Tyrol in Austria (AT) and South Tyrol 
                                               in Italy (IT). The aim of the study running from 30.09.2020 is to investigate the course of acute infection and recovery
                                               in non-hospitalized SARS-CoV-2-infected persons. The 'Mental Health' subproject monitors the mental health readout 
                                               in respect to the severity of SARS-CoV-2 infection, charcater and duration of symptoms, completness of convalescence 
                                               and psychosocial stress levels imposed by the individual disease burden and the COVID-19 pandemic."), 
                                               style = 'font-size: 16px'), 
                                           h2('Methods'), 
                                           div(p("Detailed methods and the analysis of the mental health data are available from the preprint by Hüfner et al 
                                                [1]. The study was conducted as a retrospective online-survey with a protocol approved by the respective 
                                                institional review boards. Every participant gave electronically signed informed consent. 
                                                Two independently recruited cohorts of adult, non-hospitalized region inhabitants 
                                                recovering from laboratory test-confirmed SARS-CoV-2 infection were analyzed (AT: n = 1157, IT: 
                                                n = 893). The minimal observation time between the diagnosis and survey completion was 28 days. 
                                                 The median observation time was 79 [IQR: 40 - 175] days in the AT cohort and 96 [IQR: 60 - 138] days
                                                 in the IT collective"), 
                                               p("Clinical signs of depression and anxiety after SARS-CoV-2 infection were 
                                                monitored by the modified versions of the PHQ Questionnaire [2]. Self-perceived impairment of overall mental 
                                                 health, quality of life were assessed with 4-point Likert scales. 
                                                 Physical performance impairment was surveyed as a percent loss as compared with the time before the infection. 
                                                 For details, see", 
                                                 a('Table 1', href = 'study_questions.pdf'), 
                                                 ". The cutoffs for substantial signs of anxiety, depression are based on literature [2, 3], 
                                                 for substantial psychosocial the cutoff of 7 points or more corresponding to the 75th percentile of the 
                                                 stress score was applied (", 
                                                 a('Table 2', href = 'phq_calculation.pdf'), 
                                                 ').'), 
                                               p("Acute symptoms of COVID-19 are defined as manifestations present in the first 2 weeks after clinical
                                                 onset. Persistent symptoms are defined as manifestations present for 4 weeks or longer. Confusion, 
                                                 impaired concentration and forgetfullness were classified as neurocognitive symptoms."),
                                               p("Mental Dirorder Risk Clusters (LR: low risk, IR: intermediate risk, HR: high risk) were defined by a 
                                                 two-step association analysis procedure involving self-organized map and hierarchical clustering of 
                                                 the respondents by the most influential co-variates affecting the combined mental health scoring. 
                                                 For details, see [1]."), 
                                               p("Statistical significance of differences in mental health readouts between strata of the study cohort
                                                 defined by 147 demographic, biometric, clinical and psychosocial variables is assessed by \u03c7\u00B2, 
                                                 Mann-Whitney U and Kruskal-Wallis test, as appropriate. Caution: the p values displayed in the dashboard 
                                                 were not corrected for multiple testing."), 
                                               style = 'font-size: 16px'), 
                                           h2('Terms of use'), 
                                           div(p("The result dashboard is an Open Science tool that accompanies the preprint of Hüfner et al. providing 
                                                the insightful interpretation of the study results. The results may not be used for medical or diagnostic purposes. 
                                                By using the dashboard you accept the", 
                                                 a('Terms of use and licensing', href = 'readme.txt'), 
                                                 '.', 
                                                 style = 'font-size: 16px')),
                                           h2('References'), 
                                           div(p('1. Hüfner K, et.al. Who is at risk of poor mental health following COVID-19 outpatient management?
                                                 MedRxiv. DOI: 10.1101/2021.09.22.21263949'), 
                                               p('2. Löwe B, Wahl I, Rose M, et al. 
                                                 A 4-item measure of depression and anxiety: 
                                                 Validation and standardization of the Patient Health Questionnaire-4 (PHQ-4) in the general population. 
                                                 J Affect Disord 2010; 122:86–95.'), 
                                               p('3. Löwe B, Spitzer RL, Zipfel S, Herzog W. Auflage Manual 17.07. 2002.'), 
                                               style = 'font-size: 16px'), 
                                           br())), 
                           fluidRow(HTML("<div class =  'footer'>
                                        <img src = '', width = 10%>
                                        <img src = 'mui_logo.png' width = 80>
                                        <img src = '' width = 5%>
                                        <img src = 'claudiana.png' width = 110>
                                        <img src = '', width = 5%>
                                        <img src = 'tilak_logo.png' width = 120>
                                        <img src = '' width = 5%>
                                        <img src = 'aerzte_kammer.png' width = 110>
                                        <img src = '', width = 5%>
                                        <img src = 'logo_landtirol.png' width = 80>
                                        <img src = '' width = 5%>
                                        <img src = 'south_tirol.png' width = 210>
                                        <img src = '' width = 5%>
                                        <img src = 'logo_large.png' width = 70>
                                        <img src = '' width = 10%>
                                        <hr style = 'height: 5px'>
                                        </div>")))),
        tabPanel(title = 'Answers', 
                 fluidPage(titlePanel(HTML("<div class = 'title'>
                                 Mental Health after COVID-19 in Tyrol
                                 <img src = '' width = 50%>
                                 <img src = '' width = 6%><br/>
                                 <div/>
                                 <hr style = 'height: 5px'>")), 
                           sidebarLayout(sidebarPanel(width = 3, 
                                                      h4('Select a survey question'), 
                                                      selectInput(inputId = 'feature_answer', 
                                                                  label = '', 
                                                                  choices = app_data$mental_item_choices), 
                                                      br(), 
                                                      hr(), 
                                                      h4('Select a stratification factor'), 
                                                      selectInput(inputId = 'split_var_answer', 
                                                                  label = '', 
                                                                  choices = app_data$split_choices), 
                                                      br(), 
                                                      hr(), 
                                                      h4('Study cohort'), 
                                                      radioButtons(inputId = 'cohort_answer', 
                                                                   label = '', 
                                                                   choices = c('Tyrol/AT' = 'north', 
                                                                               'South Tyrol/IT' = 'south'))), 
                                         mainPanel(width = 9,
                                                   align = 'center', 
                                                   br(), 
                                                   uiOutput('item_ui'), 
                                                   br(), 
                                                   hr(), 
                                                   p('Significance for the difference between the strata is provided
                                                     in the plot caption. Caution: p values were not corrected for multiple
                                                     comparisons. N number of complete answers is shown below the plot.'), 
                                                   br())))),
        tabPanel('Mental feature scoring', 
                 fluidPage(titlePanel(HTML("<div class = 'title'>
                                 Mental Health after COVID-19 in Tyrol
                                 <img src = '' width = 50%>
                                 <img src = '' width = 6%><br/>
                                 <div/>
                                 <hr style = 'height: 5px'>")), 
                           sidebarLayout(sidebarPanel(width = 3, 
                                                      h4('Select a mental health scale'), 
                                                      selectInput(inputId = 'feature_scale', 
                                                                  label = '', 
                                                                  choices = app_data$score_choices), 
                                                      br(), 
                                                      hr(), 
                                                      h4('Select a stratification factor'), 
                                                      selectInput(inputId = 'split_var_scale', 
                                                                  label = '', 
                                                                  choices = app_data$split_choices), 
                                                      br(), 
                                                      hr(), 
                                                      h4('Study cohort'), 
                                                      radioButtons(inputId = 'cohort_scale', 
                                                                   label = '', 
                                                                   choices = c('Tyrol/AT' = 'north', 
                                                                               'South Tyrol/IT' = 'south'))), 
                                         mainPanel(width = 9, 
                                                   align = 'center', 
                                                   br(), 
                                                   uiOutput('scale_ui'), 
                                                   br(), 
                                                   hr(), 
                                                   p('Diamonds represent medians, whiskers denote the IQR.
                                                   Significance for the difference between the strata is provided
                                                     in the plot caption. Caution: p values were not corrected for multiple
                                                     comparisons. N number of complete answers is shown below the plot.'), 
                                                   br())))), 
        tabPanel('Mental disorder prevalence', 
                 fluidPage(titlePanel(HTML("<div class = 'title'>
                                 Mental Health after COVID-19 in Tyrol
                                 <img src = '' width = 50%>
                                 <img src = '' width = 6%><br/>
                                 <div/>
                                 <hr style = 'height: 5px'>")), 
                           sidebarLayout(sidebarPanel(width = 3, 
                                                      h4('Select a mental health disorder'), 
                                                      selectInput(inputId = 'feature_prevalence', 
                                                                  label = '', 
                                                                  choices = app_data$prev_choices), 
                                                      br(), 
                                                      hr(), 
                                                      h4('Select a stratification factor'), 
                                                      selectInput(inputId = 'split_var_prevalence', 
                                                                  label = '', 
                                                                  choices = app_data$split_choices), 
                                                      br(), 
                                                      hr(), 
                                                      h4('Study cohort'), 
                                                      radioButtons(inputId = 'cohort_prevalence', 
                                                                   label = '', 
                                                                   choices = c('Tyrol/AT' = 'north', 
                                                                               'South Tyrol/IT' = 'south'))), 
                                         mainPanel(width = 9, 
                                                   align = 'center', 
                                                   br(), 
                                                   uiOutput('prev_ui'), 
                                                   br(), 
                                                   hr(), 
                                                   p('Significance for the difference between the strata is provided
                                                     in the plot caption. Caution: p values were not corrected for multiple
                                                     comparisons. N number of complete answers is shown below the plot.'), 
                                                   br()))))
            
    )


# Define server logic ----
    
    server <- function(input, output) {
        
        ## study questions, analysis output
        
        item_analysis <- reactive({
            
            display_analysis(feature = input$feature_answer, 
                             split_var = input$split_var_answer, 
                             fill_colors = translate_var(input$feature_answer, 
                                                         output = 'level_colors'), 
                             cohort = input$cohort_answer, 
                             label_txt_size = 4, 
                             x_lab = input$split_var_answer)
            
        })
        
        output$item_plot <- renderPlot({
            
            item_analysis()$plot
            
        })
        
        item_plot_h <- reactive({
            
            paste(ceiling(item_analysis()$level_no * 0.5) * 350 + 50, 
                  'px', 
                  sep = '')
            
        })
        
        output$item_ui <- renderUI({
  
            plotOutput('item_plot', 
                       height = item_plot_h())

        })
        
        ## scales, analysis output
        
        scale_analysis <- reactive({
            
            display_analysis(feature = input$feature_scale, 
                             split_var = input$split_var_scale,
                             fill_colors = translate_var(input$split_var_scale, 
                                                         output = 'level_colors'), 
                             y_lab = translate_var(input$feature_scale, 
                                                   output = 'label_short'), 
                             cohort = input$cohort_scale, 
                             violin = T)
            
        })
        
        output$scale_plot <- renderPlot({
            
            scale_analysis()$plot + 
                guides(fill = F)
            
        })
        
        scale_plot_w <- reactive({
            
            plot_w <- scale_analysis()$level_no * 15 + 3
            
            plot_w <- if(plot_w > 100) 100 else plot_w
            
            return(paste(plot_w, 
                         '%', sep = ''))
         
        })
        
        output$scale_ui <- renderUI({
            
            plotOutput('scale_plot', 
                       width = scale_plot_w(), 
                       height = '470px')
            
        })
        
        ## mental disorders, analysis output
        
        prev_analysis <- reactive({
            
            display_analysis(feature = input$feature_prevalence, 
                             split_var = input$split_var_prevalence, 
                             fill_colors = translate_var(input$feature_prevalence, 
                                                         output = 'level_colors'), 
                             cohort = input$cohort_prevalence, 
                             label_txt_size = 4)
            
        })
        
        output$prev_plot <- renderPlot({
            
            prev_analysis()$plot
            
        })
        
        prev_plot_h <- reactive({
            
            paste(ceiling(prev_analysis()$level_no * 0.5) * 350 + 50, 
                  'px', 
                  sep = '')
            
        })
        
        output$prev_ui <- renderUI({
            
            plotOutput('prev_plot', 
                       height = prev_plot_h())
            
        })

    }

# Run the application -----
    
    shinyApp(ui = ui, server = server)
