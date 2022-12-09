library(here)
source(here("code", "R", "packages.R"))
source(here("code", "R", "functions_data.R"))
source(here("code", "R", "functions_analyse.R"))
source(here("code", "R", "functions_visualization.R"))

total_nb_of_patients <- 121
nb_included_patients <- 81
set.seed(194842)

# The data generated mimick the one from the underlying article: 
# "Extended prone positioning duration for COVID‑19‑related ARDS: benefitsand detriments" from Walter et al 
# https://doi.org/10.1186/s13054-022-04081-2

# Inclusions
gupi <- 1:total_nb_of_patients
vect1 <- rep(1, nb_included_patients)
vect0 <- rep(0, total_nb_of_patients-nb_included_patients)
vect1_0 <- c(vect1, vect0)
shuffle <- sample(gupi, total_nb_of_patients, replace = FALSE)
included <- vect1_0[shuffle]
# Data
age <- round(rnorm(total_nb_of_patients, 60, 5),0) 
sex <- rbinom(total_nb_of_patients, 1, 0.72)
tbc <- rbinom(total_nb_of_patients, 1, 0.16)
t2_diabetes <- rbinom(total_nb_of_patients, 1, 0.27)
chronic_kidney_disease <- rbinom(total_nb_of_patients, 1, 0.06)
cirrhosis <- rbinom(total_nb_of_patients, 1, 0.06)
weight <- round(rnorm(total_nb_of_patients, 93, 10),0) 
height <- round(rnorm(total_nb_of_patients, 170, 10),0) 
ecmo <- rbinom(total_nb_of_patients, 1, 0.21)
no <- rbinom(total_nb_of_patients, 1, 0.42)
icu_mortality <- rbinom(total_nb_of_patients, 1, 0.30)
sapsII <- round(rnorm(total_nb_of_patients, 38, 8),0) 
hospital_mortality <- rbinom(total_nb_of_patients, 1, 0.40)
brachial_plexus <- rbinom(total_nb_of_patients, 1, 0.05)

raw_demog <- tibble(gupi, included, age, sex, tbc, t2_diabetes, chronic_kidney_disease, 
       cirrhosis, weight, height, ecmo, no, icu_mortality, sapsII, hospital_mortality, 
       brachial_plexus)

# Generating the pressure injury file
pressure_injuries <- rbinom(total_nb_of_patients, 1, 0.26)
raw_pi <- tibble(gupi, pressure_injuries)

# Generating the prone position file
total_number_of_pp_sessions <- 235
included_patients <- raw_demog %>% filter(included == 1) %>% pull(gupi)
first_pp <- included_patients # insure that all included patients have at least one prone positioning session. 
additional_pp <- sample(included_patients, (total_number_of_pp_sessions-nb_included_patients), replace = TRUE) # Add other pp sessions to sum up to 235 in total
all_pp_sessions <- c(first_pp, additional_pp) %>% as_tibble() %>% arrange(value) %>% rename(gupi = value)

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
fio2_bfr_pp <- rnorm(total_number_of_pp_sessions, 86.8, 20) %>%
  as_tibble() %>% 
  mutate(value = if_else(value > 100, 100, value)) %>% 
  round_any(5) %>% 
  pull()
tv_bfr_pp <- rnorm(total_number_of_pp_sessions, 419, 81.5) %>% round(0)
peep_bfr_pp <- rnorm(total_number_of_pp_sessions, 11.5, 2.83) %>% round_any(5)
pltp_bfr_pp  <- rnorm(total_number_of_pp_sessions, 25.5, 4.46) %>% round(0)
ph_bfr_pp <- rnorm(total_number_of_pp_sessions, 7.36, 0.0856) %>% round(2)
paco2_bfr_pp <- rnorm(total_number_of_pp_sessions, 51.3, 11.9) %>% round(0)
pao2_bfr_pp  <- rnorm(total_number_of_pp_sessions, 79.4, 10.8) %>% round(0)
fio2_drg_pp <- rnorm(total_number_of_pp_sessions, 70.9, 20) %>%
  as_tibble() %>% 
  mutate(value = if_else(value > 100, 100, value)) %>% 
  round_any(5) %>% pull()
tv_drg_pp <- rnorm(total_number_of_pp_sessions, 421, 72.9) %>% round(0)
peep_drg_pp <- rnorm(total_number_of_pp_sessions, 12, 2.89) %>% round_any(5)
pltp_drg_pp <- rnorm(total_number_of_pp_sessions, 25.0, 4.02) %>% round(0)
ph_drg_pp <- rnorm(total_number_of_pp_sessions, 7.37, 0.110) %>% round(2)
paco2_drg_pp <- rnorm(total_number_of_pp_sessions, 51.4, 12.2) %>% round(0)
pao2_drg_pp <- rnorm(total_number_of_pp_sessions, 108, 30.7) %>% round(0)
fio2_bfr_sp <- rnorm(total_number_of_pp_sessions, 65.7, 20) %>%
  as_tibble() %>% 
  mutate(value = if_else(value > 100, 100, value)) %>% 
  round_any(5) %>% 
  pull()
tv_bfr_sp <- rnorm(total_number_of_pp_sessions, 425, 72.2) %>% round(0)
peep_bfr_sp <- rnorm(total_number_of_pp_sessions, 11.9, 2.87) %>% round_any(5)
pltp_bfr_sp <- rnorm(total_number_of_pp_sessions, 25.1, 4.20) %>% round(0)
ph_bfr_sp <- rnorm(total_number_of_pp_sessions, 7.38, 0.0819) %>% round(2)
paco2_bfr_sp <- rnorm(total_number_of_pp_sessions, 50, 10.8) %>% round(0)
pao2_bfr_sp <- rnorm(total_number_of_pp_sessions, 103, 30.3) %>% round(0)
fio2_aft_sp <- rnorm(total_number_of_pp_sessions, 71.8, 15) %>% 
  as_tibble() %>% 
  mutate(value = if_else(value > 100, 100, value)) %>% 
  round_any(5) %>% 
  pull()
tv_aft_sp <- rnorm(total_number_of_pp_sessions, 427, 71.7) %>% round(0)
peep_aft_sp <- rnorm(total_number_of_pp_sessions, 12, 2.87) %>% round_any(5)
pltp_aft_sp <- rnorm(total_number_of_pp_sessions, 25.4, 4.20) %>% round(0)
ph_aft_sp <- rnorm(total_number_of_pp_sessions, 7.39, 0.0874) %>% round(2)
paco2_aft_sp  <- rnorm(total_number_of_pp_sessions, 49.4, 10.8) %>% round(0)
pao2_aft_sp <- rnorm(total_number_of_pp_sessions, 100, 51.2) %>% round(0)

curares <- rbinom(total_number_of_pp_sessions, 1, 0.99)
ecmo <- rbinom(total_number_of_pp_sessions, 1, 0.06)
no <- rbinom(total_number_of_pp_sessions, 1, 0.174)
cathecolamines <- rbinom(total_number_of_pp_sessions, 1, 0.311)

raw_pp <- tibble(all_pp_sessions, fio2_bfr_pp, tv_bfr_pp, peep_bfr_pp, pltp_bfr_pp, ph_bfr_pp, paco2_bfr_pp,
                 pao2_bfr_pp, fio2_drg_pp, tv_drg_pp, peep_drg_pp, pltp_drg_pp, ph_drg_pp, 
                 paco2_drg_pp, pao2_drg_pp, fio2_bfr_sp, tv_bfr_sp, peep_bfr_sp, pltp_bfr_sp, ph_bfr_sp, 
                 paco2_bfr_sp, pao2_bfr_sp, fio2_aft_sp, tv_aft_sp, peep_aft_sp, pltp_aft_sp,
                 ph_aft_sp, paco2_aft_sp, pao2_aft_sp, curares, ecmo, no, cathecolamines) %>%
  group_by(gupi) %>% 
  mutate(pp_session_number =  row_number()) %>%
  ungroup()

write_xlsx(raw_demog, here("random_data", "random_demog.xlsx"))
write_xlsx(raw_pi, here("random_data", "random_pressure_injuries.xlsx"))
write_xlsx(raw_pp, here("random_data", "random_prone_positioning.xlsx"))




  