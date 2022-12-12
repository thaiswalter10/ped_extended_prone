upload_pi <- function(file_path){
  pi_df <- read_xlsx(file_path)
}

upload_demog <- function(file_path){
  demog <- read_excel(file_path, na = c("", "NA")) 
  demog <- demog  %>%
    replace_na(list(raison_non_inclu = "inclu_si_retrouve")) %>%
    mutate(
      across (
        c(height, 
          weight, 
          sapsII, 
          cirrhosis,
          t2_diabetes,
          chronic_kidney_disease),
        as.numeric)
    ) %>%
    mutate(across(c(hospital_mortality, brachial_plexus), ~case_when(
      .x == "1"~TRUE, 
      .x == "0"~FALSE, 
      TRUE ~ NA))) %>%
    mutate(hospital_mortality = case_when(
      icu_mortality == 1 ~ TRUE,
      TRUE ~ hospital_mortality)
    ) %>%
    mutate(
      bmi = weight / height^2*10000,
      pbw = case_when(
        sex == 1 ~ 50 + (0.91 * (height-152.4)),
        sex == 0 ~ 45.5 + (0.91 * (height-152.4))),
      activ_tbc = ifelse(tbc=="1", 1, 0)
    )
  return(demog)
}

upload_pp <- function(file_path, raw_demog = raw_demog){
  raw_pp <- read_xlsx(file_path)
  raw_pp <- raw_pp %>%
    filter(!is.na(gupi))
  raw_pp <- type_convert(raw_pp)
  raw_pp <- raw_pp %>%  
    mutate(pao2_fio2_bfr_pp = pao2_bfr_pp/fio2_bfr_pp *100,
           pao2_fio2_drg_pp = pao2_drg_pp/fio2_drg_pp *100,
           pao2_fio2_bfr_sp = pao2_bfr_sp/fio2_bfr_sp *100,
           pao2_fio2_aft_sp = pao2_aft_sp/fio2_aft_sp *100,
# je garde la variable pao2_fio2 pour jimmy, j'en crée une autre pf pour 
# qu'elle soit simple à parser
          pf_bfr_pp = pao2_bfr_pp/fio2_bfr_pp *100,
          pf_drg_pp = pao2_drg_pp/fio2_drg_pp *100,
          pf_bfr_sp = pao2_bfr_sp/fio2_bfr_sp *100,
          pf_aft_sp = pao2_aft_sp/fio2_aft_sp *100,
          drvp_bfr_pp = pltp_bfr_pp-peep_bfr_pp,
          drvp_drg_pp = pltp_drg_pp-peep_drg_pp,
          drvp_bfr_sp = pltp_bfr_sp-peep_bfr_sp,
          drvp_aft_sp = pltp_aft_sp-peep_aft_sp) %>%
    mutate(cpl_bfr_pp = tv_bfr_pp / drvp_bfr_pp,
           cpl_drg_pp = tv_drg_pp / drvp_drg_pp,
           cpl_bfr_sp = tv_bfr_sp / drvp_bfr_sp,
           cpl_aft_sp = tv_aft_sp / drvp_aft_sp) %>%
    left_join(raw_demog %>% select(gupi, pbw), by = "gupi") %>%
    mutate(tvpbw_bfr_pp = tv_bfr_pp / pbw,
           tvpbw_drg_pp = tv_drg_pp / pbw,
           tvpbw_bfr_sp = tv_bfr_sp / pbw,
           tvpbw_aft_sp = tv_aft_sp / pbw)
}

do_final_demog <- function(raw_demog, raw_pi_df, raw_pp) {
  # Selection of patients included
  demog <- raw_demog %>% 
    filter(included == 1)
  
  # Preprocessing of pressure injuries related information
  # pi stands for pressure injuries and is equal to 1 if the patient has had at 
  # least one pressure injury during his / here stay, 0 otherwise
  pi <- raw_pi_df %>%
    group_by(gupi)%>%
    distinct(pressure_injuries) %>%
    filter(gupi %in% demog$gupi) 
  
  # Preprocessing of prone positionning related information. 
  sum_up_pp <- raw_pp %>% 
    group_by(gupi) %>%
    summarise(nbpp=n())
  gupi_with_cathecolamines <- raw_pp %>% 
    group_by(gupi) %>% 
    filter(cathecolamines == 1) %>% 
    distinct(gupi) %>% pull
  pp1_params <- raw_pp %>% 
    filter(pp_session_number == 1) %>% 
    select(gupi, 
           ends_with("bfr_pp") & 
           starts_with(c("pao2_fio", "peep", "drvp", "cpl")))
  
  # Demog Final
  demog <- demog %>% 
    left_join(pi, by = "gupi") %>% 
    left_join(sum_up_pp, by = "gupi") %>%
    left_join(pp1_params, by = "gupi") %>%
    mutate(cathecolaminesPP = ifelse(gupi %in% gupi_with_cathecolamines, 1, 0),
           curarized = 1)
}

do_final_pp <- function(demog, raw_pp){
  demog_death <- demog %>% select(gupi, icu_mortality)
  final_pp = raw_pp %>% filter(gupi %in% demog$gupi) %>%
    left_join(demog_death, by = "gupi") %>%
    mutate(o2_responder = (pf_drg_pp - pf_bfr_pp) > 20)
}

check_miss_dat <- function(demog){
  demog %>% summarise(across(
    c(gupi,
    age,
    sex,
    weight,
    height,
    sapsII), ~sum(is.na(.))))
}