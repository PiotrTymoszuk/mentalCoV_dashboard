# this script provides function tools for data analysis and plotting 
# within the dashboard

# containers -----

  app_globals <- list()
  app_data <- list()
  
# functions ------
  
  translate_var <- function(var_vector, dictionary = app_data$var_lexicon, output = 'label_short') {
    
    ## translates the variable vector to the labels or other items provided by the dictionary
    
    label_vec <- dictionary[[output]] %>% 
      set_names(dictionary$variable)

    return(label_vec[var_vector])
    
  }
  
  flip_vector <- function(vector) {
    
    ## flips the names and items of the vector
    
    return(names(vector) %>% 
             set_names(unname(vector)))
    
  }
  
  display_analysis <- function(feature, split_var, data = cov_data, cohort = 'north', ...) {
    
    ## displays the analysis results in a form dependent on the data class
    
    if(split_var == 'cohort') {
      
      analysis_data <- rbind(cov_data$north, 
                             cov_data$south)
      
      var_labeller <- c(north = 'AT', 
                        south = 'IT')
      
    } else {
      
      analysis_data <- cov_data[[cohort]]
      
      var_labeller <- NULL
      
    }
    
    if(split_var == feature) {
      
      plot <- ggplot() + 
               annotate('text', 
                        label = 'Oops!\nThe requested response and splitting factor are the same.\nCannot compute significance.', 
                        x = 0, 
                        y = 0, 
                        size = 8, 
                        color = 'coral3') + 
               theme_void()
      
      return(list(plot = plot, 
                  level_no = 2))
      
    }
    
    analysis_data <- analysis_data %>% 
      filter(!is.na(.data[[feature]]), 
             !is.na(.data[[split_var]]))
    
    ## analysis object
    
    analysis_obj <- analyze_feature(inp_tbl = analysis_data, 
                                    variable = feature, 
                                    split_var = split_var)
    
    ## plotting
    
    plot <- plot_analysis(analysis_object = analysis_obj, 
                          signif_digits = 3, 
                          label = translate_var(feature, 
                                                output = 'description'), 
                          cust_theme = app_globals$common_theme, 
                          labeller = var_labeller, 
                          ...)
    
    ## appending the plot with the proper p value and test information

    if(is.numeric(analysis_data[[feature]])) {
      
      p_val <- try(extract_p(analysis_object = analysis_obj, 
                             test_type = 'u', 
                             signif_digits = 2), 
                   silent = T)
      
      test_type <- 'Mann-Whitney U test'
      
      if(class(p_val) == 'try-error') {
        
        p_val <- extract_p(analysis_object = analysis_obj, 
                           test_type = 'kruskal', 
                           signif_digits = 2)
        
        test_type <- 'Kruskal-Wallis test'
        
      }
      
    } else {
      
      
      p_val <- extract_p(analysis_object = analysis_obj, 
                         test_type = 'chi_sq', 
                         signif_digits = 2)
      
      test_type <- '\u03c7\u00B2 test'
      
    }
    
    plot_subtitle <- paste('By:', translate_var(split_var, output = 'description'))
    
    p_lab <- paste(test_type, 
                   p_val, 
                   sep = ': ')
    
    plot_subtitle <- paste(plot_subtitle, p_lab, sep = '\n')
      
    
    plot <- plot + 
      labs(subtitle = plot_subtitle %>% 
             paste('\n', ., sep = ''), 
           fill = translate_var(feature, output = 'description'), 
           x = translate_var(split_var, output = 'description'))
    
    ## specifying the hooks for the plot size
    
    fct_num <- length(levels(analysis_data[[split_var]]))
    
    return(list(plot = plot, 
                level_no = fct_num))

  }
  
# app globals ------

  # graphics theme
  
  app_globals$common_txt <- element_text(size = 14, 
                                         face = 'plain')
  
  app_globals$common_margin <- ggplot2::margin(t = 5, 
                                               r = 3, 
                                               l = 3, 
                                               b = 3, 
                                               unit = 'mm')
  
  app_globals$common_theme <- theme_classic() + 
    theme(plot.title = element_text(size = 18, 
                                    face = 'bold'), 
          plot.subtitle = element_text(size = 16, 
                                       face = 'plain'), 
          plot.tag = element_text(size = 14, 
                                  face = 'plain', 
                                  hjust = 0), 
          legend.text = app_globals$common_txt, 
          legend.title = app_globals$common_txt,
          axis.text = app_globals$common_txt, 
          axis.title = app_globals$common_txt, 
          strip.background = element_blank(), 
          strip.text = app_globals$common_txt)
  
# app data ------
  
  ## survey answers stored in the cov_data list, appending it with the clustering information
  
  tryCatch(load('./data/survey_data.RData'), 
           error = function(e) load('./mental_health/data/survey_data.RData'))
  
  tryCatch(load('./data/clust_assign.RData'), 
           error = function(e) load('./mental_health/data/clust_assign.RData'))
  
  cov_data <- map2(cov_data, 
                   clust_assignment, 
                   left_join, 
                   by = 'ID')
  
  ## variable lexicons and calulation tables
  
  app_data[c('var_lexicon', 
             'study_items', 
             'phq_calculation')] <- c('variable_lexicon.xlsx', 
                                      'study_questions.xlsx', 
                                      'phq_calculation.xlsx') %>% 
    map(function(x) tryCatch(read_excel(paste('./data', x, sep = '/')), 
                             error = function(e) read_excel(paste('./mental_health/data', x, sep = '/'))))
  
  ## vector with stratification factors
  
  app_data$split_vars <- app_data$var_lexicon %>% 
    filter(modeling_variable != 'no') %>%
    arrange(modeling_variable) %>% 
    .$variable
  
  app_data$split_choices <- translate_var(app_data$split_vars, 
                                          output = 'description') %>% 
    flip_vector
  
  ## vector of the mental health questions
  
  app_data$mental_item_vars <- app_data$var_lexicon %>% 
    filter(mental_health_item != 'no') %>% 
    arrange(as.numeric(mental_health_item)) %>% 
    .$variable
  
  app_data$mental_item_choices <- translate_var(app_data$mental_item_vars, 
                                                output = 'description') %>% 
    flip_vector
  
  ## vector with the score responses
  
  app_data$score_responses <- app_data$var_lexicon %>% 
    filter(score_response != 'no') %>% 
    arrange(as.numeric(score_response)) %>% 
    .$variable
  
  app_data$score_choices <- translate_var(app_data$score_responses, 
                                          output = 'description') %>% 
    flip_vector
  
  ## vector with the prevalence variables
  
  app_data$prev_responses <- app_data$var_lexicon %>% 
    filter(prev_response != 'no') %>% 
    arrange(as.numeric(prev_response)) %>% 
    .$variable
  
  app_data$prev_choices <- translate_var(app_data$prev_responses, 
                                         output = 'description') %>% 
    flip_vector
  
# END -----