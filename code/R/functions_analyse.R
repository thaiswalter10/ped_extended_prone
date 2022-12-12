extract_named_vector_binq <- function(df, type_of_value, table_name){
  unnamed_list <- df %>%
    filter(bin_q == type_of_value & table == table_name) %>%
    select(var_name) %>% 
    pull
  names <- df %>% 
    filter(bin_q == type_of_value & table == table_name) %>% 
    select(print_out_name) %>% 
    pull
  named_list <- unnamed_list %>% setNames(names)
}

# Descriptives statistics
descB <- function(x){
  if(sum(is.na(x))>0) print(paste("Attention, ",sum(is.na(x))," données manquantes, pour la variable",sep=""))
  a0 = paste(sum(x,na.rm=T)," (",sprintf("%.0f",100*mean(x,na.rm=T)),")",sep="")
}

descQ = function(x,n=0){
  if (sum(is.na(x))>0) print(paste("Attention, ",sum(is.na(x))," données manquantes",sep=""))
  xx = sprintf(paste("%.",n,"f",sep=""),quantile(x,probs=c(0.5,0.25,0.75),na.rm=T))
  a0 = paste(xx[1]," [",xx[2],"-",xx[3],"]",sep="")
  a0
}

summarise_demog <- function(df, named_list_var, desc_fun, col1_name, col2_name){
  tibble_var = as_tibble(named_list_var) %>% pull
  df %>%
    summarise(across(all_of(tibble_var), ~desc_fun(.x))) %>%
    rename(all_of(named_list_var)) %>%
    pivot_longer(cols = everything(), names_to = col1_name, values_to = rlang::as_string(col2_name))
}

do_descriptive_table <- function(df_to_summarise, df_of_vars, table_name, col1_name, col2_name){
  # Statistiques descriptives pour les variables binaires
  bin_var <- extract_named_vector_binq(df = df_of_vars, type_of_value = "bin", table_name= table_name)
  bin_demog <- summarise_demog(df = df_to_summarise, 
                               named_list_var = bin_var, 
                               desc_fun = descB, 
                               col1_name = col1_name, 
                               col2_name = col2_name)
  # Statistiques descriptives pour les variables en quartiles
  q_var <- extract_named_vector_binq(df = df_of_vars, type_of_value = "q", table_name = table_name)
  q_demog <- summarise_demog(df = df_to_summarise, 
                             named_list_var = q_var, 
                             desc_fun = descQ, 
                             col1_name = col1_name, 
                             col2_name = col2_name)
  table1 <- q_demog %>% add_row(bin_demog)
}

# Odd Ratio risque escarres
compute_odd_ratio_pval1 <- function(var, outcome_to_study, df){
    formula = as.formula(paste(outcome_to_study , var, sep="~"))
    mod = glm(formula,family="binomial",df)
    # Odd Ratio
    or = sprintf("%.1f",exp(coefficients(mod)[-1]))
    # Confidence interval
    ci = sprintf("%.1f",exp(confint(mod)[-1,]))
    ci = paste(ci,collapse="-")
    # p value
    pval = format(anova(mod,test="Chisq")$"Pr(>Chi)"[-1],digits=2)
    c( paste(or," (",ci,")",sep=""),pval)
}

compute_odd_ratio_pval3 <- function(var, outcome_to_study, df){
  formula = as.formula(paste(outcome_to_study , var, sep="~"))
  mod = glm(formula,family="binomial",df)
  # Odd Ratio
  or = sprintf("%.3f",exp(coefficients(mod)[-1]))
  # Confidence interval
  ci = sprintf("%.3f",exp(confint(mod)[-1,]))
  ci = paste(ci,collapse="-")
  # p value
  pval = format(anova(mod,test="Chisq")$"Pr(>Chi)"[-1],digits=2)
  c( paste(or," (",ci,")",sep=""),pval)
}

do_fdr_table <- function(stat_fun, list_var_df, col_name, what_these_var_are, outcome_to_study, df){
  list_var_df <- list_var_df %>% filter(table2 == "fdr")
  list_var <- list_var_df %>% pull(var_name) %>% setNames(list_var_df$print_out_name)
  
  # Calcule l'odd ratio + la p value pour chacune de ces variables sauf duree_tot_vent
  res <- sapply(list_var, stat_fun, outcome_to_study=outcome_to_study, df = df)
  # Met en page le résultat
  table <- res %>%
    t() %>%
    as_tibble %>%
    mutate(Variables = list_var_df$print_out_name) %>%
    rename("OR (95% CI)" = V1,
           "p value" = V2) %>%
    relocate(Variables)
}

do_fdr_table_mep <- function(info_table1, demog){
  table1 <- do_fdr_table(stat_fun = compute_odd_ratio_pval1,
               list_var_df = info_table1,
               col_name = table2,
               what_these_var_are = "fdr",
               outcome_to_study = "escarre", 
               df = demog)
 table3 <-  do_fdr_table(stat_fun = compute_odd_ratio_pval3,
                         list_var_df = info_table1,
                         col_name = table2,
                         what_these_var_are = "fdr",
                         outcome_to_study = "escarre", 
                         df = demog)
 table <- table1 %>% 
   filter(!Variables %in% c("Added duration of all proning sessions", "ICU length of stay")) %>% 
   add_row(table3 %>% filter(Variables %in% c("Added duration of all proning sessions", "ICU length of stay")))

 }

vect_lme <- function (df_time_point_as_col, var){
  # give var as a character
  df_time_point_as_col %>% 
    group_by(name_var) %>%
    group_modify(
      ~broom::tidy(
        anova(lme(eval(substitute(j ~ 1, list(j = as.name({{var}})))), random = ~1 |
                    gupi, na.action = na.omit, data = .))
      )
    ) %>%
    ungroup() %>%
    filter(column == "p-value") %>% 
    select(name_var, column, mean) %>%
    pivot_wider(names_from = column, values_from = mean)
}

do_df_1col_time_point <- function (df = dv, key_names_df = info_table1){
  # Création d'une liste contenant le nom des variables à étudier
  list_var <- key_names_df %>% 
    filter(table == "vent_row" & !var_name %in% c("vtpbw", "ph")) %>% 
    pull(var_name)
  list_time_point <- key_names_df %>% filter(table == "vent_col") %>% pull(var_name)
  
  # On reformate le tibble dv 
  df_1col_time_point <- map_dfr(
    c(list_var, "ph", "vtpbw"), 
    make_single_var_df, df, 
    list_time_point) 
}

