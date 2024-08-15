library(tidyverse)
library(readxl)
library(ggplot2)
library(writexl)
library(labelled)
library(robotoolbox)
library(Hmisc)
library(modeest)

#############################
##### Step 1. Load data #####
#############################

kobo_setup(url = "https://kobo.unhcr.org",token = ****)

x <- kobo_submissions("******")

HH <- x$main
Individual <- x$rpt_hhmnames

#############################
## Step 2. Reorder columns ##
#############################
# rename variables that where wrongly named

Individual <- Individual |>
  rename(NUT_D1_Q2 = NUT_D2_Q1,
         NUT_D1_Q2_1 = NUT_D2_Q1_1,
         NUT_D1_Q2_2 = NUT_D2_Q1_2,
         NUT_D1_Q2_3 = NUT_D2_Q1_3,
         NUT_D1_Q2_4 = NUT_D2_Q1_4,
         NUT_D1_Q2_5 = NUT_D2_Q1_5,
         NUT_D1_Q2_6 = NUT_D2_Q1_6,
         NUT_D1_Q2_98 = NUT_D2_Q1_98,
         NUT_D1_Q2_99 = NUT_D2_Q1_99)

HH <- HH |>
  rename(GBV_D3_Q1 = GBV_D2_Q1)

# reorder stuff to better EDA

Individual <- Individual |>
  select("hhmnames_pos", "HHH01_2_aux", "HH01_aux", "nt_rostermember", "HH03_aux", "HH01_2_aux", "HH03_2_aux", "nt_names", 
         "personId", "hhroster_pos_aux", "hhmnames_pos_match", "nt_endnames", "HH01", "HH03", "HH04", "HH05", 
         "HH06", "calcul1", "age", "AgeMonths", "ageMD","agecalculated", "age_est", "months_est", 
         "HH07", "HH07_months", "MH_3", "MH_3_1", "MH_3_2", "MH_3_3", "MH_3_4", "MH_3_5", 
         "MH_3_6", "MH_3_7", "MH_3_8", "MH_3_9", "MH_3_10", "MH_3_99", "PRO_D4_Q1", "PRO_D4_Q1_1", 
         "PRO_D4_Q1_2", "PRO_D4_Q1_3", "PRO_D4_Q1_4", "PRO_D4_Q1_5", "PRO_D4_Q1_6", "PRO_D4_Q1_7", "PRO_D4_Q1_10", "PRO_D4_Q1_96",
         "PRO_D4_Q1_98", "PRO_D4_Q1_99", "PRO_D4_Q1_O", "NUT_D1_Q1", "NUT_D1_Q1_1", "NUT_D1_Q1_2", "NUT_D1_Q1_3", "NUT_D1_Q1_98",
         "NUT_D1_Q1_99", "NUT_D1_Q2", "NUT_D1_Q2_1", "NUT_D1_Q2_2", "NUT_D1_Q2_3", "NUT_D1_Q2_4", "NUT_D1_Q2_5", "NUT_D1_Q2_6",
         "NUT_D1_Q2_98", "NUT_D1_Q2_99", "NUT_D4_Q1", "NUT_D4_Q1_1", "NUT_D4_Q1_2", "NUT_D4_Q1_3", "NUT_D4_Q1_4", "NUT_D4_Q1_98",
         "NUT_D4_Q1_99", "NUT_D5_Q1", "NUT_D5_Q2", "NUT_D5_Q2_1", "NUT_D5_Q2_2", "NUT_D5_Q2_3", "NUT_D5_Q2_4", "NUT_D5_Q2_5", 
         "NUT_D5_Q2_6", "NUT_D5_Q2_7", "NUT_D5_Q2_8", "NUT_D5_Q2_9", "NUT_D5_Q2_10", "NUT_D5_Q2_98", "NUT_D5_Q2_99", "NUT_D8_Q1",
         "NUT_D8_Q1_1", "NUT_D8_Q1_2", "NUT_D8_Q1_3", "NUT_D8_Q1_4", "NUT_D8_Q1_5", "NUT_D8_Q1_6", "NUT_D8_Q1_7","NUT_D8_Q1_8",
         "NUT_D8_Q1_9", "NUT_D8_Q1_98", "NUT_D8_Q1_99", "NUT_D10_Q1", "NUT_D10_Q1_1", "NUT_D10_Q1_2", "NUT_D10_Q1_3", "NUT_D10_Q1_4",
         "NUT_D10_Q1_5", "NUT_D10_Q1_6", "NUT_D10_Q1_7", "NUT_D10_Q1_8", "NUT_D10_Q1_9", "NUT_D10_Q1_10", "NUT_D10_Q1_11", "NUT_D10_Q1_98",
         "NUT_D10_Q1_99", "EDU_D1_Q1", "EDU_D2_Q1", "EDU_D2_Q2", "EDU_D3_Q1", "EDU_D6_Q1", "EDU_D6_Q1_1", "EDU_D6_Q1_2", 
         "EDU_D6_Q1_3", "EDU_D6_Q1_4", "EDU_D6_Q1_5", "EDU_D6_Q1_6", "EDU_D6_Q1_96", "EDU_D6_Q1_98", "EDU_D6_Q1_99", "EDU_D6_Q1_O", 
         "EDU_D7_Q1", "EDU_D7_Q1_1", "EDU_D7_Q1_2", "EDU_D7_Q1_3", "EDU_D7_Q1_4", "EDU_D7_Q1_5", "EDU_D7_Q1_6", "EDU_D7_Q1_96",
         "EDU_D7_Q1_98", "EDU_D7_Q1_99", "EDU_D7_Q1_O", "HE_D1_Q1", "HE_D1_Q2", "HE_D3_Q1", "HE_D3_Q1_1", "HE_D3_Q1_2", 
         "HE_D3_Q1_3", "HE_D3_Q1_4", "HE_D3_Q1_5", "HE_D3_Q1_6", "HE_D3_Q1_7", "HE_D3_Q1_8", "HE_D3_Q1_9", "HE_D3_Q1_10", 
         "HE_D3_Q1_11", "HE_D3_Q1_96", "HE_D3_Q1_98", "HE_D3_Q1_99", "HE_D3_Q1_O", "INT_D1_Q1", "INT_D2_Q1", "INT_D2_Q2", 
         "INT_D2_Q3", "start_time_2_001", "position", "Relation_R", "adult18", "women_b", "father_b", "childLess2", 
         "childLess2name", "women", "father", "adult", "women_b_count", "hh_size", "hhhead_age_ab18", "adult_sum", 
         "hhhead_age", "position18", "adult01", "age18above", "below5", "below18", "child_edu_calcul", "below5_r", 
         "below18_r", "child_edu_calcul_r", "positionbelow5", "positionbelow18", "positionchild_edu_calcul", "below5_nc", 
         "below18_nc", "child_edu_calcul_nc", "adult_sum_001", "hh_size_001", "ven01", "_index", "_parent_table_name",
         "_parent_index", "_validation_status")

HH <- HH |>
  select("start", "end", "start_time_1", "logo", "note1", "interviewdate", "implementador", "testreal", 
         "name_enumerator", "Intro01", "Intro01_A", "number", "PoC_name", "call_attempt", "attempt1", "next_attempt",
         "note_attempt_implementador1", "note_attempt_implementador2", "attempt2", "attempt3", "consent_form", "Intro04", 
         "Intro04.1", "DEMO_1", "DEMO_2", "DEMO_00.6", "attempt3.1", "DEMO_5", "DEMO_5.1", "DEMO_6", 
         "DEMO_7", "DEMO_7.1", "country", "country.1", "admin1", "admin2", "DEMO_18", "note_HH01", 
         "HHH01_aux", "MH_1", "namechild2less", "nochild2less", "women_name_b_total", "women_name_b", 
         "father_name_b", "women_name", "father_name", "adult_name", "sumbelow5", "sumbelow18", "sumchild_edu_calcul",
         "nobelow5", "nobelow18", "nochild_edu_calcul", "ven_sum", "respondent_1", "note_integration", 
         "INT_D3_Q1", "INT_D3_Q1B", "INT_D3_Q1B_1", "INT_D3_Q1B_2", "INT_D3_Q1B_3", "INT_D3_Q1B_4", "INT_D3_Q1B_5", 
         "INT_D3_Q1B_6", "INT_D3_Q1B_7", "INT_D3_Q1B_8", "INT_D3_Q1B_9", "INT_D3_Q1B_98", "INT_D3_Q1B_99", "INT_D3_Q2", 
         "INT_D3_Q2_1", "INT_D3_Q2_2", "INT_D3_Q2_3", "INT_D3_Q2_4", "INT_D3_Q2_5", "INT_D3_Q2_6","INT_D3_Q2_7", 
         "INT_D3_Q2_8", "INT_D3_Q2_9", "INT_D3_Q2_10", "INT_D3_Q2_98", "INT_D3_Q2_99", "INT_D3_Q2_O", "INT_D4_Q1", 
         "INT_D4_Q1_1", "INT_D4_Q1_2", "INT_D4_Q1_3", "INT_D4_Q1_4", "INT_D4_Q1_5", "INT_D4_Q1_6", "INT_D4_Q1_10", 
         "INT_D4_Q1_96", "INT_D4_Q1_98", "INT_D4_Q1_99", "INT_D4_Q1_O", "note_food_security", "FS_D1_label", "FS_D1_Q1", 
         "FS_D1_Q2", "FS_D1_Q3", "FS_D1_Q4", "FS_D1_Q5", "FS_D1_Q6", "FS_D1_Q7", "FS_D1_Q8", "note_food_security2", 
         "FS_D2_label", "FS_D2_Q1","FS_D2_Q2", "FS_D2_Q3", "FS_D2_Q4", "FS_D2_Q5", "note_food_security3", "FS_D3_Q1", 
         "FS_D3_Q2", "note_food_security4", "FS_D4_label", "FS_D4_Q1", "FS_D4_Q2", "FS_D4_Q3", "FS_D4_Q4", "FS_D4_Q5", 
         "FS_D4_Q6", "FS_D4_Q7", "FS_D4_Q8", "FS_D4_Q9", "FS_D4_Q10", "note_transportation", "HT_D1_Q1_label", "HT_D1_Q1_first",
         "HT_D1_Q1_first_o", "HT_D1_Q1_second", "HT_D1_Q1_second_o","HT_D1_Q2", "HT_D2_Q1", "HT_D2_Q1_O", "note_shelter",
         "SHE_D1_Q1", "SHE_D1_Q1_O", "SHE_D1_Q2", "SHE_D1_Q2_1", "SHE_D1_Q2_2", "SHE_D1_Q2_3", "SHE_D1_Q2_4", "SHE_D1_Q2_5", 
         "SHE_D1_Q2_6", "SHE_D1_Q2_98", "SHE_D1_Q2_99", "SHE_D1_Q3", "SHE_D1_Q3_1", "SHE_D1_Q3_2", "SHE_D1_Q3_3", 
         "SHE_D1_Q3_4", "SHE_D1_Q3_5", "SHE_D1_Q3_98", "SHE_D1_Q3_99", "SHE_D2_Q1", "SHE_D3_Q1", "SHE_D3_Q1_1", "SHE_D3_Q1_2", 
         "SHE_D3_Q1_3", "SHE_D3_Q1_4", "SHE_D4_Q1", "note_wash", "WA_D1_Q1", "WA_D1_Q2", "WA_D2_Q1", "WA_D2_Q2", 
         "WA_D4_Q1", "WA_D4_Q2", "WA_D6_Q1", "WA_D8_Q1", "WA_D11_Q1", "note_protection", "PRO_D1_Q1", "PRO_D1_Q1_1", 
         "PRO_D1_Q1_2", "PRO_D1_Q1_3", "PRO_D1_Q1_4", "PRO_D1_Q1_5", "PRO_D1_Q1_6", "PRO_D1_Q1_7", "PRO_D1_Q1_9", 
         "PRO_D1_Q1_96", "PRO_D1_Q1_98", "PRO_D1_Q1_99", "PRO_D1_Q1_O", "PRO_D2_Q1", "PRO_D2_Q1B", "PRO_D2_Q1B_1", 
         "PRO_D2_Q1B_2", "PRO_D2_Q1B_3", "PRO_D2_Q1B_4", "PRO_D2_Q1B_5", "PRO_D2_Q1B_6", "PRO_D2_Q1B_7", "PRO_D2_Q1B_8", 
         "PRO_D2_Q1B_9", "PRO_D2_Q1B_10", "PRO_D2_Q1B_96", "PRO_D2_Q1B_98", "PRO_D2_Q1B_99", "PRO_D2_Q1B_O", "PRO_D3_Q1", 
         "PRO_D3_Q1B", "PRO_D3_Q1B_1", "PRO_D3_Q1B_2", "PRO_D3_Q1B_3", "PRO_D3_Q1B_4", "PRO_D3_Q1B_5", "PRO_D3_Q1B_6", 
         "PRO_D3_Q1B_7", "PRO_D3_Q1B_96", "PRO_D3_Q1B_98", "PRO_D3_Q1B_99", "PRO_D3_Q1B_O", "PRO_D3_Q2", 
         "PRO_D3_Q2B", "PRO_D3_Q2B_1", "PRO_D3_Q2B_2", "PRO_D3_Q2B_3", "PRO_D3_Q2B_4", "PRO_D3_Q2B_5", "PRO_D3_Q2B_6", 
         "PRO_D3_Q2B_7", "PRO_D3_Q2B_8", "PRO_D3_Q2B_9", "PRO_D3_Q2B_98", "PRO_D3_Q2B_99", "PRO_D5_Q1", "PRO_D5_Q2", 
         "PRO_D5_Q2_1", "PRO_D5_Q2_2", "PRO_D5_Q2_3", "PRO_D5_Q2_4", "PRO_D5_Q2_5", "PRO_D5_Q2_6", "PRO_D5_Q2_7", 
         "PRO_D5_Q2_98", "PRO_D5_Q2_99", "note_gbv", "GBV_D1_Q1", "GBV_D1_Q1B", "GBV_D1_Q1B_1", "GBV_D1_Q1B_2", 
         "GBV_D1_Q1B_3", "GBV_D1_Q1B_4", "GBV_D1_Q1B_5", "GBV_D1_Q1B_6", "GBV_D1_Q1B_8", "GBV_D1_Q1B_96", "GBV_D1_Q1B_98",
         "GBV_D1_Q1B_99", "GBV_D1_Q1B_O", "GBV_D3_Q1", "HTS_D1_Q1", "HTS_D1_Q2", "HTS_D2_Q1", "HTS_D2_Q1_1", 
         "HTS_D2_Q1_2", "HTS_D2_Q1_3", "HTS_D2_Q1_4", "HTS_D2_Q1_5", "HTS_D2_Q1_6", "CP_D1_Q1", "CP_D1_Q1_1", 
         "CP_D1_Q1_2", "CP_D1_Q1_3", "CP_D1_Q1_4", "CP_D1_Q1_5", "CP_D1_Q1_6", "CP_D1_Q1_7", "CP_D1_Q1_98", 
         "CP_D1_Q1_99", "CP_D1_Q2", "psea_org", "psea3", "end_survey", "end_result", "name_respondent", "final_notes", 
         "final_notes_entry", "end_time_1", "_id", "uuid", "_submission_time", "_validation_status", "_status", "_submitted_by",
         "__version__", "_uuid", "_index", "rpt_hhmnames_count", "S2_respondent_count", "instanceID", "_xform_id_string")

#############################
##### Step 3. Functions #####
#############################
# Some functions need to be created to be used during the data-processing of the individual dataset since there where changes applied to the regional form while we had already entered the data collection process.

# this function will be used for rounding in de CARI_FES
round_half_up <- function(x) {
  floor(x + 0.5)
}

# define processes to clear inconsistencies in bulk
replace_values_nut_under_6_months <- function(data, uuid_condition, aux_condition, columns) {
  for (column in columns) {
    data <- data %>%
      mutate({{ column }}  := 
               ifelse(
                 `_parent_index` == uuid_condition & 
                   HH01_aux == aux_condition,
                 NA_character_,
                 !!rlang::sym(column)
               )
      )
  }
  return(data)
}

replace_values_nut_over_5_years <- function(data, age_condition, columns) {
  for (column in columns) {
    data <- data %>%
      mutate({{ column }}  := 
               ifelse(
                 age > age_condition & HH05 == "1" |
                   age_est > age_condition & HH05 == "0", 
                 NA_character_,
                 !!rlang::sym(column)
               )
      )
  }
  return(data)
}

############################################        
##### Step 4. Household Pre-processing #####
############################################
# The aim was to lose as less information as possible. We had two different data services providers working in this project. PULSO was hired by UNHCR and Equilibrium hired by IOM. 
# PULSO identified 6 records where the data quality check was not passed, then they where dismissed from the dataset. Other minor adjustments had to do with phone numbers and internal coding. 
# On the other hand, Equilibrium corrected information on food security via a second call.

# Other pre-processing:
# Clearing some variables since there where changes in the skip logic in the regional form while we were already gathering data.
# Creation of SHE_D1_Q3_ALL that will be used in step 9 Household level indicators


# fixing minor bugs to do not lose records 
HH <- HH |>
  mutate(implementador = 
           ifelse(`_uuid` == "c6de4918-d3af-4d56-8465-fdf15051647a", "2", implementador),
         implementador = 
           ifelse(`_uuid` == "298b6023-4c6d-420f-8749-87bdc8710d7b", "2", implementador),
         implementador = 
           ifelse(`_uuid` == "53d14903-76e2-4acc-8faa-3532e44660c1", "2", implementador),
         implementador = 
           ifelse(`_uuid` == "93f82e5d-1657-4df9-8936-f97edb4b9f26", "2", implementador),
         implementador = 
           ifelse(`_uuid` == "faab88b8-9191-4a2a-bd11-def8ef9c3421", "1", implementador), 
         testreal = 
           ifelse(`_uuid` == "aa3cd4df-9062-4af8-a9c0-141aa08c1789", "real", testreal),
         Intro01_A = 
           if_else(Intro01_A == "ACNUR _05703", "ACNUR_05703", Intro01_A)
  )

# Delete 6 invalid registries that did not pass the supervision re-call quality check 
HH <- HH |>
  subset(!`_uuid` %in% c('7a54aa3e-aac9-4cb9-97e1-2983138d7f3e',
                         'c3da093c-80f7-4e17-acd7-b808024bbeeb',
                         '686d4e26-573a-4282-88c5-f1452d970df5',
                         '4c39a23c-dd95-4c07-9664-5521503a795a',
                         'b702dc78-3553-4c18-9ea9-b3ab5a4b27ce',
                         '9d7f23c3-6c0b-4658-b6d3-99a0060200b9'))
# Data imputation
# PULSO
HH <- HH |>
  mutate(
    # Código Pulso
    Intro01 = case_when(
      `_id` == 49603065 ~ 'ACNUR_03946',
      `_id` == 49391490 ~ 'ACNUR_03290',
      `_id` == 49490163 ~ 'ACNUR_04655',
      TRUE ~ Intro01),
    # Número de celular
    number = case_when(
      `_id` == 49868767 ~ 51951531796,
      `_id` == 49490268 ~ 51917529462,
      TRUE ~ number))

# Equilibrium
HH <- HH |>
  mutate(
    # FS_D3_Q1
    FS_D3_Q1 = case_when(
      `_id` == 48952959 ~ 1000,
      `_id` == 49044796 ~ 1500,
      `_id` == 49294309 ~ 200,
      `_id` == 49363836 ~ 800,
      `_id` == 49363877 ~ 400,
      `_id` == 49423606 ~ 1500,
      TRUE ~ FS_D3_Q1),
    # FS_D3_Q2
    FS_D3_Q2 = case_when(
      `_id` == 49477621 ~ 300,
      `_id` == 49122384 ~ 2000,
      `_id` == 49294338 ~ 1300,
      TRUE ~ FS_D3_Q2))

# Clearing the values of WA_D1_Q2 since there where changes in the skip logic of the regional form
HH <- HH |>
  mutate(WA_D1_Q2 = 
           ifelse(WA_D1_Q1 == "2", NA, WA_D1_Q2))

# Creating new variables
# Zones Lima, Norte and Sur
HH <- HH |>
  mutate(SECTOR = ifelse(admin1 == "PE15" | admin1 == "PE07", "LIMA METROPOLITANA",
                         ifelse(admin1 == "PE01" | 
                                  admin1 == "PE02" | 
                                  admin1 == "PE06" | 
                                  admin1 == "PE13" |
                                  admin1 == "PE14" |
                                  admin1 == "PE16" | 
                                  admin1 == "PE20" |
                                  admin1 == "PE22" |
                                  admin1 == "PE24", "NORTE",
                                "SUR")))

# All facilities inside household

HH <- HH |>
  mutate(SHE_D1_Q3_ALL = if_else((SHE_D1_Q3_1 == "1" & 
                                    SHE_D1_Q3_2 == "1" &
                                    SHE_D1_Q3_3 == "1" &
                                    SHE_D1_Q3_4	== "1"), 1, 0))

############################################# 
##### Step 5. Individual Pre-processing #####
#############################################

# In this step data is edited in the variables related with age (AgeMonths, ageMD, age, HH07 and HH07_months). 
# Since we used an old RMS form as the canvas for the JNA some calculations and other variables where inherited. Age was calculated substracting the Today() variable minus date of birth (HH06). 
# Errors emerged when enumerators tried to correct the date of birth due to an approximate calculation of age (ie. phone survey was held in april, and someone said their DOB was 04-2000, 
# the calculated age would be 24, however he/she might have been born in the final days of April so could still be a 23 y/o)

# The nutrition quality checks functions created in step 3 where used to fix errors due to bad programming of the xls form. 
# We spotted the error 3 days deep in the data collection process. Some values were imputed in the nutrition questions since 23 minors where not assessed in the NUT_D8 and NUT_D10 indicators. 
# We used the mode since it was a categorical question

Individual <- Individual |>
  mutate(
    AgeMonths = case_when(
      `_parent_index` == "213" & `_index` == "385" ~ 180,
      `_parent_index` == "2535" & `_index` == "4823" ~ 419,
      TRUE ~ AgeMonths),
    ageMD = case_when(
      `_parent_index` == "213" & `_index` == "385" ~ 0,
      `_parent_index` == "3318" & `_index` == "5660" ~ 11,
      `_parent_index` == "3610" & `_index` == "5913" ~ 11,
      `_parent_index` == "1738" & `_index` == "3525" ~ 1,
      `_parent_index` == "1198" & `_index` == "2498" ~ 0,
      `_parent_index` == "2535" & `_index` == "4823" ~ 11,
      TRUE ~ as.numeric(ageMD)),
    age = case_when(
      `_parent_index` == "1738" & `_index` == "3525" ~ 5,
      `_parent_index` == "1198" & `_index` == "2498" ~ 8,
      TRUE ~ age), 
    HH07 = case_when(
      `_parent_index` == "1738" & `_index` == "3525" ~ 5,
      `_parent_index` == "1198" & `_index` == "2498" ~ 8,
      TRUE ~ HH07),
    HH07_months = case_when(
      `_parent_index` == "213" & `_index` == "385" ~ 0,
      `_parent_index` == "3318" & `_index` == "5660" ~ 11,
      `_parent_index` == "3610" & `_index` == "5913" ~ 11,
      `_parent_index` == "1738" & `_index` == "3525" ~ 1,
      `_parent_index` == "1198" & `_index` == "2498" ~ 0,
      `_parent_index` == "2535" & `_index` == "4823" ~ 11,
      TRUE ~ as.numeric(HH07_months)))

Individual <- Individual |>
  mutate(
    HH06 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ as.Date(32599, origin = "1899-12-30"),
      TRUE ~ HH06),
    age = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 35,
      TRUE ~ age),
    AgeMonths = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 420,
      TRUE ~ AgeMonths),
    ageMD = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 0,
      TRUE ~ ageMD),
    HH07 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 35,
      TRUE ~ HH07),
    HH07_months = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 0,
      TRUE ~ HH07_months),
    NUT_D1_Q1 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ "3",
      TRUE ~ NUT_D1_Q1),
    NUT_D1_Q1_1 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 0,
      TRUE ~ NUT_D1_Q1_1),
    NUT_D1_Q1_2 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 0,
      TRUE ~ NUT_D1_Q1_2),
    NUT_D1_Q1_3 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 1,
      TRUE ~ NUT_D1_Q1_3),
    NUT_D1_Q1_98 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 0,
      TRUE ~ NUT_D1_Q1_98),
    NUT_D1_Q1_99 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ 0,
      TRUE ~ NUT_D1_Q1_99),
    INT_D1_Q1 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ "1",
      TRUE ~ INT_D1_Q1),   
    INT_D2_Q1 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ "2",
      TRUE ~ INT_D2_Q1), 
    INT_D2_Q2 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ "1",
      TRUE ~ INT_D2_Q2), 
    INT_D2_Q3 = case_when(
      `_parent_index` == "435" & `_index` == "813" ~ "1",
      TRUE ~ INT_D2_Q3),
    HH06 = case_when(
      `_parent_index` == "800" & `_index` == "1631" ~ as.Date(24624, origin = "1899-12-30"),
      TRUE ~ HH06),
    age = case_when(
      `_parent_index` == "800" & `_index` == "1631" ~ 56,
      TRUE ~ age),
    AgeMonths = case_when(
      `_parent_index` == "800" & `_index` == "1631" ~ 682,
      TRUE ~ AgeMonths),
    ageMD = case_when(
      `_parent_index` == "800" & `_index` == "1631" ~ 10,
      TRUE ~ ageMD),
    HH07 = case_when(
      `_parent_index` == "800" & `_index` == "1631" ~ 56,
      TRUE ~ HH07),
    HH07_months = case_when(
      `_parent_index` == "800" & `_index` == "1631" ~ 10,
      TRUE ~ HH07_months),
    INT_D1_Q1 = case_when(
      `_parent_index` == "800" & `_index` == "1631" ~ "4",
      TRUE ~ INT_D1_Q1),
    AgeMonths = case_when(
      `_parent_index` == "2485" & `_index` == "4737" ~ 419,
      TRUE ~ AgeMonths),
    ageMD = case_when(
      `_parent_index` == "2485" & `_index` == "4737" ~ 11,
      TRUE ~ ageMD),
    HH07_months = case_when(
      `_parent_index` == "2485" & `_index` == "4737" ~ 11,
      TRUE ~ HH07_months),
    AgeMonths = case_when(
      `_parent_index` == "86" & `_index` == "142" ~ 180,
      TRUE ~ AgeMonths),
    ageMD = case_when(
      `_parent_index` == "86" & `_index` == "142" ~ 0,
      TRUE ~ ageMD),
    HH07_months = case_when(
      `_parent_index` == "86" & `_index` == "142" ~ 0,
      TRUE ~ HH07_months),
    ageMD = case_when(
      `_parent_index` == "3305" & `_index` == "5638" ~ 11,
      TRUE ~ ageMD),
    HH07_months = case_when(
      `_parent_index` == "3305" & `_index` == "5638" ~ 11,
      TRUE ~ HH07_months),
    age = case_when(
      `_parent_index` == "3602" & `_index` == "5905" ~ 51,
      TRUE ~ age),
    ageMD = case_when(
      `_parent_index` == "3602" & `_index` == "5905" ~ 11,
      TRUE ~ ageMD),
    HH07 = case_when(
      `_parent_index` == "3602" & `_index` == "5905" ~ 51,
      TRUE ~ HH07),
    HH07_months = case_when(
      `_parent_index` == "3602" & `_index` == "5905" ~ 11,
      TRUE ~ HH07_months),
    HH05 = case_when(
      `_parent_index` == "3121" & `_index` == "5407" ~ "0",
      TRUE ~ HH05),
    HH07 = case_when(
      `_parent_index` == "3121" & `_index` == "5407" ~ 8,
      TRUE ~ HH07),
    EDU_D1_Q1 = case_when(
      `_parent_index` == "3121" & `_index` == "5407" ~ "1",
      TRUE ~ EDU_D1_Q1), 
    EDU_D3_Q1 = case_when(
      `_parent_index` == "3121" & `_index` == "5407" ~ "5",
      TRUE ~ EDU_D3_Q1)) 


# fixing nutrition errors due to bad skip logic

nut_under_6_months <- c("NUT_D4_Q1", 
                        "NUT_D4_Q1_1",
                        "NUT_D4_Q1_2",
                        "NUT_D4_Q1_3",
                        "NUT_D4_Q1_4",
                        "NUT_D4_Q1_98",
                        "NUT_D4_Q1_99",
                        "NUT_D5_Q1",
                        "NUT_D5_Q2",
                        "NUT_D5_Q2_1",
                        "NUT_D5_Q2_2",
                        "NUT_D5_Q2_3",
                        "NUT_D5_Q2_4",
                        "NUT_D5_Q2_5",
                        "NUT_D5_Q2_6",
                        "NUT_D5_Q2_7",
                        "NUT_D5_Q2_8",
                        "NUT_D5_Q2_9",
                        "NUT_D5_Q2_10",
                        "NUT_D5_Q2_98",
                        "NUT_D5_Q2_99")


nut_over_5_years <- c("NUT_D8_Q1",
                      "NUT_D8_Q1_1", 
                      "NUT_D8_Q1_2",
                      "NUT_D8_Q1_3",
                      "NUT_D8_Q1_4",
                      "NUT_D8_Q1_5",
                      "NUT_D8_Q1_6",
                      "NUT_D8_Q1_7",
                      "NUT_D8_Q1_8",
                      "NUT_D8_Q1_9",
                      "NUT_D8_Q1_98",
                      "NUT_D8_Q1_99",
                      "NUT_D10_Q1",
                      "NUT_D10_Q1_1",
                      "NUT_D10_Q1_2",
                      "NUT_D10_Q1_3",
                      "NUT_D10_Q1_4",
                      "NUT_D10_Q1_5",
                      "NUT_D10_Q1_6",
                      "NUT_D10_Q1_7",
                      "NUT_D10_Q1_8",
                      "NUT_D10_Q1_9",
                      "NUT_D10_Q1_10",
                      "NUT_D10_Q1_11",
                      "NUT_D10_Q1_98",
                      "NUT_D10_Q1_99")


## clearing the columns that should have not been applied due to age group using the adhoc functions created

Individual <- replace_values_nut_under_6_months(Individual, "493", "ATALI VICTORIA TALLIDO GOITIA", nut_under_6_months)

Individual <- replace_values_nut_under_6_months(Individual, "732", "THIAGO", nut_under_6_months)

Individual <- replace_values_nut_under_6_months(Individual, "781", "MÍA ISABELLA MONTIER", nut_under_6_months)

## clearing the columns that should have not been applied due to age group 

Individual <- replace_values_nut_over_5_years(Individual, 4, nut_over_5_years)


# creating new variables and recoding them to fix changes in nutrition sector from the regional form for question NUT_D8

Individual <- Individual |>
  mutate(NUT_D8_Q1_2_rev = case_when(
    NUT_D8_Q1_2 %in% c("1") | NUT_D8_Q1_3 %in% c("1") ~ "1",
    NUT_D8_Q1_2 %in% c("0") & NUT_D8_Q1_3 %in% c("0") ~ "0")) |>
  select(-NUT_D8_Q1_2, -NUT_D8_Q1_3)

Individual <- Individual |>
  mutate(
    NUT_D8_Q1_1 = as.character(NUT_D8_Q1_1),
    NUT_D8_Q1_2	= as.character(NUT_D8_Q1_2_rev),
    NUT_D8_Q1_3	= as.character(NUT_D8_Q1_4),
    NUT_D8_Q1_4	= as.character(NUT_D8_Q1_5),
    NUT_D8_Q1_5	= as.character(NUT_D8_Q1_6),
    NUT_D8_Q1_6	= as.character(NUT_D8_Q1_7),
    NUT_D8_Q1_7	= as.character(NUT_D8_Q1_8),
    NUT_D8_Q1_8	= as.character(NUT_D8_Q1_9),
    NUT_D8_Q1_98 = as.character(NUT_D8_Q1_98),
    NUT_D8_Q1_99 = as.character(NUT_D8_Q1_99)) |>
  select(-NUT_D8_Q1_9, -NUT_D8_Q1_2_rev)

# creating new variables and recoding them to fix changes in nutrition sector from the regional form for question NUT_D10
Individual <- Individual |>
  mutate(NUT_D10_Q1_3_rev = case_when(
    NUT_D10_Q1_3 %in% c("1") | NUT_D10_Q1_4 %in% c("1") ~ "1",
    NUT_D10_Q1_3 %in% c("0") & NUT_D10_Q1_4 %in% c("0") ~ "0"),
    NUT_D10_Q1_7_rev = case_when(
      NUT_D10_Q1_8 %in% c("1") | NUT_D10_Q1_9 %in% c("1") ~ "1",
      NUT_D10_Q1_8 %in% c("0") & NUT_D10_Q1_9 %in% c("0") ~ "0"),
    NUT_D10_Q1_8_rev = case_when(
      NUT_D10_Q1_10 %in% c("1") | NUT_D10_Q1_11 %in% c("1") ~ "1",
      NUT_D10_Q1_10 %in% c("0") & NUT_D10_Q1_11 %in% c("0") ~ "0")) |>
  select(-NUT_D10_Q1_3, -NUT_D10_Q1_4, -NUT_D10_Q1_8, -NUT_D10_Q1_9, -NUT_D10_Q1_10, -NUT_D10_Q1_11)

Individual <- Individual |>
  mutate(
    NUT_D10_Q1_1 = as.character(NUT_D10_Q1_1),
    NUT_D10_Q1_2 = as.character(NUT_D10_Q1_2),
    NUT_D10_Q1_3 = as.character(NUT_D10_Q1_3_rev),
    NUT_D10_Q1_4 = as.character(NUT_D10_Q1_5),
    NUT_D10_Q1_5 = as.character(NUT_D10_Q1_6),
    NUT_D10_Q1_6 = as.character(NUT_D10_Q1_7),
    NUT_D10_Q1_7 = as.character(NUT_D10_Q1_7_rev),
    NUT_D10_Q1_8 = as.character(NUT_D10_Q1_8_rev),
    NUT_D10_Q1_98 = as.character(NUT_D10_Q1_98),
    NUT_D10_Q1_99 = as.character(NUT_D10_Q1_99)) |>
  select(-NUT_D10_Q1_3_rev, -NUT_D10_Q1_7_rev, -NUT_D10_Q1_8_rev)

# Imputation based in the mode

mode_NUT_D8_Q1_1 <-mlv(Individual$NUT_D8_Q1_1, na.rm = TRUE)
mode_NUT_D8_Q1_2 <-mlv(Individual$NUT_D8_Q1_2, na.rm = TRUE)
mode_NUT_D8_Q1_3 <-mlv(Individual$NUT_D8_Q1_3, na.rm = TRUE)
mode_NUT_D8_Q1_4 <-mlv(Individual$NUT_D8_Q1_4, na.rm = TRUE)
mode_NUT_D8_Q1_5 <-mlv(Individual$NUT_D8_Q1_5, na.rm = TRUE)
mode_NUT_D8_Q1_6 <-mlv(Individual$NUT_D8_Q1_6, na.rm = TRUE)
mode_NUT_D8_Q1_7 <-mlv(Individual$NUT_D8_Q1_7, na.rm = TRUE)
mode_NUT_D8_Q1_8 <-mlv(Individual$NUT_D8_Q1_8, na.rm = TRUE)
mode_NUT_D8_Q1_98 <-mlv(Individual$NUT_D8_Q1_98, na.rm = TRUE)
mode_NUT_D8_Q1_99 <-mlv(Individual$NUT_D8_Q1_99, na.rm = TRUE)

mode_NUT_D10_Q1_1 <-mlv(Individual$NUT_D10_Q1_1, na.rm = TRUE)
mode_NUT_D10_Q1_2 <-mlv(Individual$NUT_D10_Q1_2, na.rm = TRUE)
mode_NUT_D10_Q1_3 <-mlv(Individual$NUT_D10_Q1_3, na.rm = TRUE)
mode_NUT_D10_Q1_4 <-mlv(Individual$NUT_D10_Q1_4, na.rm = TRUE)
mode_NUT_D10_Q1_5 <-mlv(Individual$NUT_D10_Q1_5, na.rm = TRUE)
mode_NUT_D10_Q1_6 <-mlv(Individual$NUT_D10_Q1_6, na.rm = TRUE)
mode_NUT_D10_Q1_7 <-mlv(Individual$NUT_D10_Q1_7, na.rm = TRUE)
mode_NUT_D10_Q1_8 <-mlv(Individual$NUT_D10_Q1_8, na.rm = TRUE)
mode_NUT_D10_Q1_98 <-mlv(Individual$NUT_D10_Q1_98, na.rm = TRUE)
mode_NUT_D10_Q1_99 <-mlv(Individual$NUT_D10_Q1_99, na.rm = TRUE)


# Data imputation for multiple option question NUT_D8_Q1 

Individual <- Individual |>
  mutate(
    NUT_D8_Q1_1 = case_when(
      is.na(NUT_D8_Q1_1) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_1,
      TRUE ~ NUT_D8_Q1_1),
    NUT_D8_Q1_2 = case_when(
      is.na(NUT_D8_Q1_2) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_2,
      TRUE ~ NUT_D8_Q1_2),
    NUT_D8_Q1_3 = case_when(
      is.na(NUT_D8_Q1_3) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_3,
      TRUE ~ NUT_D8_Q1_3),
    NUT_D8_Q1_4 = case_when(
      is.na(NUT_D8_Q1_4) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_4,
      TRUE ~ NUT_D8_Q1_4),
    NUT_D8_Q1_5 = case_when(
      is.na(NUT_D8_Q1_5) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_5,
      TRUE ~ NUT_D8_Q1_5),
    NUT_D8_Q1_6 = case_when(
      is.na(NUT_D8_Q1_6) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_6,
      TRUE ~ NUT_D8_Q1_6),
    NUT_D8_Q1_7 = case_when(
      is.na(NUT_D8_Q1_7) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_7,
      TRUE ~ NUT_D8_Q1_7),
    NUT_D8_Q1_8 = case_when(
      is.na(NUT_D8_Q1_8) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_8,
      TRUE ~ NUT_D8_Q1_8),
    NUT_D8_Q1_98 = case_when(
      is.na(NUT_D8_Q1_98) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_98,
      TRUE ~ NUT_D8_Q1_98),    
    NUT_D8_Q1_99 = case_when(
      is.na(NUT_D8_Q1_99) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_99,
      TRUE ~ NUT_D8_Q1_99),
    NUT_D8_Q1 = case_when(
      is.na(NUT_D8_Q1) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D8_Q1_1,
      TRUE ~ NUT_D8_Q1)
  )

# Data imputation for multiple option question NUT_D10_Q1 
Individual <- Individual |>
  mutate(
    NUT_D10_Q1_1 = case_when(
      is.na(NUT_D10_Q1_1) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_1,
      TRUE ~ NUT_D10_Q1_1),
    NUT_D10_Q1_2 = case_when(
      is.na(NUT_D10_Q1_2) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_2,
      TRUE ~ NUT_D10_Q1_2),
    NUT_D10_Q1_3 = case_when(
      is.na(NUT_D10_Q1_3) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_3,
      TRUE ~ NUT_D10_Q1_3),
    NUT_D10_Q1_4 = case_when(
      is.na(NUT_D10_Q1_4) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_4,
      TRUE ~ NUT_D10_Q1_4),
    NUT_D10_Q1_5 = case_when(
      is.na(NUT_D10_Q1_5) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_5,
      TRUE ~ NUT_D10_Q1_5),
    NUT_D10_Q1_6 = case_when(
      is.na(NUT_D10_Q1_6) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_6,
      TRUE ~ NUT_D10_Q1_6),
    NUT_D10_Q1_7 = case_when(
      is.na(NUT_D10_Q1_7) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_7,
      TRUE ~ NUT_D10_Q1_7),
    NUT_D10_Q1_8 = case_when(
      is.na(NUT_D10_Q1_8) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_8,
      TRUE ~ NUT_D10_Q1_8),
    NUT_D10_Q1_98 = case_when(
      is.na(NUT_D10_Q1_98) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_98,
      TRUE ~ NUT_D10_Q1_98),    
    NUT_D10_Q1_99 = case_when(
      is.na(NUT_D10_Q1_99) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ mode_NUT_D10_Q1_99,
      TRUE ~ NUT_D10_Q1_99),
    NUT_D10_Q1 = case_when(
      is.na(NUT_D10_Q1) & 
        (HH07 %in% c("1", "2", "3", "4") | 
           (HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11"))) ~ "2 4 5",
      TRUE ~ NUT_D10_Q1)
  )

# fixing changes in integration sector from the regional form for question INT_D2_Q2 and INT_D2_Q3
Individual <- Individual |>
  mutate(
    INT_D2_Q2 = case_when(
      INT_D2_Q1 %in% c("0", "98") & is.na(INT_D2_Q2) ~ "0",
      TRUE ~ INT_D2_Q2),
    INT_D2_Q3 = case_when(
      INT_D2_Q1 %in% c("0", "98") & is.na(INT_D2_Q3) ~ "0",
      TRUE ~ INT_D2_Q3)
  )

# creating a variable that will help build the indicator as a sum of different food groups for children among 6 and 59 months of age.

Individual <- Individual |>
  mutate(NUT_D10_Q1_ALL = 
           as.numeric(NUT_D10_Q1_1) +
           as.numeric(NUT_D10_Q1_2) +
           as.numeric(NUT_D10_Q1_3) +
           as.numeric(NUT_D10_Q1_4) +
           as.numeric(NUT_D10_Q1_5) +
           as.numeric(NUT_D10_Q1_6) +
           as.numeric(NUT_D10_Q1_7) + 
           as.numeric(NUT_D10_Q1_8)) 

Individual <- Individual |>
  mutate(
    Age_Group = case_when(
      HH07>=0 & HH07 <=4 ~ "0-4",
      HH07>4 & HH07 <=11 ~ "5-11",
      HH07>11 & HH07 <=17 ~ "12-17",
      HH07>17 & HH07 <=29 ~ "18-29",
      HH07>29 & HH07 <=39 ~ "30-39",
      HH07>39 & HH07 <=49 ~ "40-49",
      HH07>49 & HH07 <=59 ~ "50-59",
      HH07>59 ~ "60+"))


Individual <- Individual |>
  mutate(
    HH04 = case_when(
      `_parent_index` == "2340" & `_index` == "4524" ~ "1",
      TRUE ~ HH04),
    HH04 = case_when(
      `_parent_index` == "1212" & `_index` == "2533" ~ "1",
      TRUE ~ HH04))


#############################
##### Step 6. Filtering #####
#############################

# Filter and subset the Individual dataset according to the Parent ID's of the Households that where selected with the filtering


HH <- HH |>
  filter(testreal == "real") |>
  filter(attempt1 == "1") |>
  filter(attempt2 == "1") |>
  filter(Intro04 == "1") |>
  filter(DEMO_1 >= 18) |>
  filter(DEMO_00.6 == "1") |>
  filter(DEMO_5 == "1") |>
  filter(DEMO_7 %in% c(1, 4, 5, 6)) |>
  filter(country == "Peru")

HH_final_uuid <-HH$`_index`

Individual <- Individual |>
  filter(`_parent_index` %in% HH_final_uuid)

###############################################
###### Step 7. Qualitative pre-processing #####
###############################################

# Creation of new categories in the "Other (specify)" options. Some answers needed to be edited since it already existed an option and there was not need to use the "Other" text-option

# Re categorization of "other" in Individual dataset

# PRO_D4_Q1_O --> 9 records 
Individual <- Individual |>
  mutate(
    PRO_D4_Q1_96 = case_when(
      PRO_D4_Q1_O == "Licencia para conducir" ~ 0,
      PRO_D4_Q1_O == "Licencia de conducir" ~ 0,
      PRO_D4_Q1_O == "PARTIDA DE NACIMIENTO" ~ 0,
      PRO_D4_Q1_O == "Carnet de refugiada" ~ 0,
      PRO_D4_Q1_O == "Carnet de refugio" ~ 0,
      TRUE ~ PRO_D4_Q1_96), 
    PRO_D4_Q1_5 = case_when(
      PRO_D4_Q1_O == "Carnet de refugiada" ~ 1,
      PRO_D4_Q1_O == "Carnet de refugio" ~ 1,
      TRUE ~ PRO_D4_Q1_5),
    PRO_D4_Q1_6 = case_when(
      PRO_D4_Q1_O == "PARTIDA DE NACIMIENTO" ~ 1,
      TRUE ~ PRO_D4_Q1_6))


# EDU_D6_Q1_O --> 112 records
Individual <- Individual |>
  mutate(
    EDU_D1_Q1 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D1_Q1),
    EDU_D6_Q1 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1),
    EDU_D6_Q1_1 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1_1),
    EDU_D6_Q1_2 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1_2),
    EDU_D6_Q1_3 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1_3),
    EDU_D6_Q1_4 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1_4),
    EDU_D6_Q1_5 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1_5),
    EDU_D6_Q1_6 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1_6),
    EDU_D6_Q1_96 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1_96),
    EDU_D6_Q1_98 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1_98),
    EDU_D6_Q1_99 = case_when(
      EDU_D6_Q1_O == "YA VA PARA LA UNIVERSIDAD NO CONSIGUE CUPO Y NO CUENTA CON RECURSOS PARA UNIVERSIDADES PRIVADAS" ~ NA, 
      EDU_D6_Q1_O == "YA ACABO EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "ya acabo el colegio" ~ NA, 
      EDU_D6_Q1_O == "Ya culmino la secundaria" ~ NA, 
      EDU_D6_Q1_O == "YA SE GRADUO" ~ NA, 
      EDU_D6_Q1_O == "EL AÑO PASADO ACABO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA TERMINÓ EL COLEGIO" ~ NA, 
      EDU_D6_Q1_O == "en espera para ingresar a la universidad" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ SUS ESTUDIOS" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINO SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "POSTULO A LA UNIVERSIDAD NACIONAL Y NO QUEDÓ SELECCIONADA Y NO CUENTA CON LOS RECURSOS PARA INGRESAR A UNA UNIVERSIDAD PRIVADA" ~ NA, 
      EDU_D6_Q1_O == "TERMINO LA SECUNDARIA" ~ NA, 
      EDU_D6_Q1_O == "YA CULMINÓ LA SECUNDARIA Y NO TIENE RECURSOS PARA CURSAR LA UNIVERSIDAD" ~ NA,
      TRUE ~ EDU_D6_Q1_99),
    EDU_D6_Q1_2 = case_when(
      EDU_D6_Q1_O == "NO CONSIGUIÓ CUPO EN UN ESTADAL Y ESTÁ EMBARAZADA Y AL SER MADRE SOLTERA SE LE COMPLICA INGRESAR A UN PRIVADO" ~ 1,
      EDU_D6_Q1_O == "POR CAMBIO DE DOMICILIO QUE NO CONSIGUIÓ EN ESTADAL" ~ 1,
      TRUE ~ EDU_D6_Q1_2),
    EDU_D6_Q1_7 = case_when(
      EDU_D6_Q1_O == "LA NIÑA TIENE AUTISMO" ~ 1,
      EDU_D6_Q1_O == "INCAPACIDAD" ~ 1,
      EDU_D6_Q1_O == "Dificultad en el desarrollo del lenguaje" ~ 1,
      EDU_D6_Q1_O == "DISCAPACIDAD, TIENE DIFICULTADES PARA COMER" ~ 1,
      EDU_D6_Q1_O == "Presenta discapacidad visual de nacimiento" ~ 1,
      EDU_D6_Q1_O == "TIENE UNA CONDICION ESPECIAL POR SALUD POR ESO NO ASISTE A LA ESCUELA" ~ 1,
      EDU_D6_Q1_O == "TIENE UNA DISCAPACIDAD AUTISMO." ~ 1,
      EDU_D6_Q1_O == "TIENE PROBLEMA DE APRENDIZAJE NO ENTIENDE MUCHO" ~ 1,
      EDU_D6_Q1_O == "ES AUTISTA, NO HA ENCONTRADO UN CENTRO EDUCATIVO QUE PUEDA PAGAR, Y NO TINE SIS" ~ 1,
      EDU_D6_Q1_O == "NIÑO CON DISCAPACIDAD" ~ 1,
      EDU_D6_Q1_O == "TIENE RETRASO PSICOMOTRIZ. NO HABLA NI CAMINA." ~ 1,
      EDU_D6_Q1_O == "CONDICION DE AUTISMO, ATENCION EN CASA" ~ 1,
      EDU_D6_Q1_O == "TIENE AUTISMO Y NO HA CONSEGUIDO UNA ESCUELA DONDE PUEDAN ATENDER A NIÑOS CON SU CONDICION" ~ 1,
      EDU_D6_Q1_O == "SUFRE DE DISCAPACIDAD INTELECTUAL" ~ 1,
      EDU_D6_Q1_O == "TERAPIA DE LENGUAJE EN HOGAR, NO VA POR FALTA DE PASAJES" ~ 1,
      EDU_D6_Q1_O == "salud" ~ 1,
      EDU_D6_Q1_O == "SALUD" ~ 1,
      EDU_D6_Q1_O == "TIENE MICROCEFALIA" ~ 1,
      EDU_D6_Q1_O == "EL NIÑO TIENE UNA CONDICION D SALUD Y SU MADRE TEME MANDARLO AL COLEGIO" ~ 1,
      EDU_D6_Q1_O == "EL NIÑO TIENE HEMORROIDES Y NECESITA OPERACION , ELIMINA SANGRE" ~ 1,
      EDU_D6_Q1_O == "LA NIÑA CUENTA CON HIDROCEFALIA" ~ 1,
      EDU_D6_Q1_O == "La familia estuvo delicada de salud y no pudieron inscribirla." ~ 1,
      EDU_D6_Q1_O == "PORQUE LO OPERARON HACE QUINCE DIAS Y NO LO HA PODIDO MATRICULAR" ~ 1,
      EDU_D6_Q1_O == "Todavía no pueden inscribirlo por su salud." ~ 1,
      EDU_D1_Q1 == "0" ~ 0),
    EDU_D6_Q1_96 = case_when(
      EDU_D6_Q1_O == "LA NIÑA TIENE AUTISMO" ~ 0,
      EDU_D6_Q1_O == "INCAPACIDAD" ~ 0,
      EDU_D6_Q1_O == "Dificultad en el desarrollo del lenguaje" ~ 0,
      EDU_D6_Q1_O == "DISCAPACIDAD, TIENE DIFICULTADES PARA COMER" ~ 0,
      EDU_D6_Q1_O == "Presenta discapacidad visual de nacimiento" ~ 0,
      EDU_D6_Q1_O == "TIENE UNA CONDICION ESPECIAL POR SALUD POR ESO NO ASISTE A LA ESCUELA" ~ 0,
      EDU_D6_Q1_O == "TIENE UNA DISCAPACIDAD AUTISMO." ~ 0,
      EDU_D6_Q1_O == "TIENE PROBLEMA DE APRENDIZAJE NO ENTIENDE MUCHO" ~ 0,
      EDU_D6_Q1_O == "ES AUTISTA, NO HA ENCONTRADO UN CENTRO EDUCATIVO QUE PUEDA PAGAR, Y NO TINE SIS" ~ 0,
      EDU_D6_Q1_O == "NIÑO CON DISCAPACIDAD" ~ 0,
      EDU_D6_Q1_O == "TIENE RETRASO PSICOMOTRIZ. NO HABLA NI CAMINA." ~ 0,
      EDU_D6_Q1_O == "CONDICION DE AUTISMO, ATENCION EN CASA" ~ 0,
      EDU_D6_Q1_O == "TIENE AUTISMO Y NO HA CONSEGUIDO UNA ESCUELA DONDE PUEDAN ATENDER A NIÑOS CON SU CONDICION" ~ 0,
      EDU_D6_Q1_O == "SUFRE DE DISCAPACIDAD INTELECTUAL" ~ 0,
      EDU_D6_Q1_O == "TERAPIA DE LENGUAJE EN HOGAR, NO VA POR FALTA DE PASAJES" ~ 0,
      EDU_D6_Q1_O == "salud" ~ 0,
      EDU_D6_Q1_O == "SALUD" ~ 0,
      EDU_D6_Q1_O == "TIENE MICROCEFALIA" ~ 0,
      EDU_D6_Q1_O == "EL NIÑO TIENE UNA CONDICION D SALUD Y SU MADRE TEME MANDARLO AL COLEGIO" ~ 0,
      EDU_D6_Q1_O == "EL NIÑO TIENE HEMORROIDES Y NECESITA OPERACION , ELIMINA SANGRE" ~ 0,
      EDU_D6_Q1_O == "LA NIÑA CUENTA CON HIDROCEFALIA" ~ 0,
      EDU_D6_Q1_O == "La familia estuvo delicada de salud y no pudieron inscribirla." ~ 0,
      EDU_D6_Q1_O == "PORQUE LO OPERARON HACE QUINCE DIAS Y NO LO HA PODIDO MATRICULAR" ~ 0,
      EDU_D6_Q1_O == "Todavía no pueden inscribirlo por su salud." ~ 0,
      TRUE ~ EDU_D6_Q1_96),
    EDU_D6_Q1_8 = case_when(
      EDU_D6_Q1_O == "CONSIDERA LA MADRE QUE ESTA MUY PEQUEÑO" ~ 1,
      EDU_D6_Q1_O == "NO TIENE LA EDAD AUN NO LO RECIBEN EN CENTRO EDUCATIVOS" ~ 1,
      EDU_D6_Q1_O == "TODAVIA NO QUIERE MATRICULARLO" ~ 1,
      EDU_D6_Q1_O == "NO TIENE EDAD PARA INICIAL" ~ 1,
      EDU_D6_Q1_O == "NO CUMPLE CON LA EDAD REQUERIDA PARA MATRICULARSE" ~ 1,
      EDU_D6_Q1_O == "NO CUMPLIR CON LA EDAD CORRESPONDIENTE EN EL MES DE INICIO DE LA ESCOLARIDAD" ~ 1,
      EDU_D6_Q1_O == "No, porque no tiene la edad necesaria (3 años cumplidos)" ~ 1,
      EDU_D6_Q1_O == "NO CUMPLE CON LA EDAD REQUERIDA PARA PODER MATRICULARSE" ~ 1,
      EDU_D6_Q1_O == "CONSIDERA QUE AÚN ES MUY PEQUEÑO PARA ASISTIR AL COLEGIO" ~ 1,
      EDU_D6_Q1_O == "QUIERE ESPERAR A QUE CREZCA UN POCO MÁS" ~ 1,
      EDU_D6_Q1_O == "RECIEN CUMPLIO Y LAS NORMAS NO LO ACEPTAN" ~ 1,
      EDU_D6_Q1_O == "AUN NO HABLA Y SU MAMA NO LO HA INSCRITO POR ESA RAZON" ~ 1,
      EDU_D6_Q1_O == "ES MUY CHIQUITA" ~ 1,
      EDU_D6_Q1_O == "PORQUE LA VE MUY PEQUEÑA EL SIGUIENTE AÑO LA PONDRA" ~ 1,
      EDU_D6_Q1_O == "POR LA EDAD NO LE PERMITIERON LA INSCRIPCIÓN EN 1ER GRADO" ~ 1,
      EDU_D6_Q1_O == "POR QUE ES PEQUEÑ0" ~ 1,
      EDU_D6_Q1_O == "SU MAMÁ AÚN NO DESEA ENVIARLO" ~ 1,
      EDU_D6_Q1_O == "TODAVIA ESTÁ ESPERANDO QUE CUMPLA LOS TRES AÑOS" ~ 1,
      EDU_D6_Q1_O == "NO LO ACEPTARON POR LA EDAD Y EL OTRO CENTRO POR DISCRIMINACIÓN." ~ 1,
      EDU_D1_Q1 == "0" ~ 0),
    EDU_D6_Q1_96 = case_when(
      EDU_D6_Q1_O == "CONSIDERA LA MADRE QUE ESTA MUY PEQUEÑO" ~ 0,
      EDU_D6_Q1_O == "NO TIENE LA EDAD AUN NO LO RECIBEN EN CENTRO EDUCATIVOS" ~ 0,
      EDU_D6_Q1_O == "TODAVIA NO QUIERE MATRICULARLO" ~ 0,
      EDU_D6_Q1_O == "NO TIENE EDAD PARA INICIAL" ~ 0,
      EDU_D6_Q1_O == "NO CUMPLE CON LA EDAD REQUERIDA PARA MATRICULARSE" ~ 0,
      EDU_D6_Q1_O == "NO CUMPLIR CON LA EDAD CORRESPONDIENTE EN EL MES DE INICIO DE LA ESCOLARIDAD" ~ 0,
      EDU_D6_Q1_O == "No, porque no tiene la edad necesaria (3 años cumplidos)" ~ 0,
      EDU_D6_Q1_O == "NO CUMPLE CON LA EDAD REQUERIDA PARA PODER MATRICULARSE" ~ 0,
      EDU_D6_Q1_O == "CONSIDERA QUE AÚN ES MUY PEQUEÑO PARA ASISTIR AL COLEGIO" ~ 0,
      EDU_D6_Q1_O == "QUIERE ESPERAR A QUE CREZCA UN POCO MÁS" ~ 0,
      EDU_D6_Q1_O == "RECIEN CUMPLIO Y LAS NORMAS NO LO ACEPTAN" ~ 0,
      EDU_D6_Q1_O == "AUN NO HABLA Y SU MAMA NO LO HA INSCRITO POR ESA RAZON" ~ 0,
      EDU_D6_Q1_O == "ES MUY CHIQUITA" ~ 0,
      EDU_D6_Q1_O == "PORQUE LA VE MUY PEQUEÑA EL SIGUIENTE AÑO LA PONDRA" ~ 0,
      EDU_D6_Q1_O == "POR LA EDAD NO LE PERMITIERON LA INSCRIPCIÓN EN 1ER GRADO" ~ 0,
      EDU_D6_Q1_O == "POR QUE ES PEQUEÑ0" ~ 0,
      EDU_D6_Q1_O == "SU MAMÁ AÚN NO DESEA ENVIARLO" ~ 0,
      EDU_D6_Q1_O == "TODAVIA ESTÁ ESPERANDO QUE CUMPLA LOS TRES AÑOS" ~ 0,
      EDU_D6_Q1_O == "NO LO ACEPTARON POR LA EDAD Y EL OTRO CENTRO POR DISCRIMINACIÓN." ~ 0,
      EDU_D6_Q1_O == "POR CAMBIO DE DOMICILIO QUE NO CONSIGUIÓ EN ESTADAL" ~ 0,
      TRUE ~ EDU_D6_Q1_96),
    EDU_D6_Q1_3 = case_when(
      EDU_D6_Q1_O == "TRAMITES MIGRATORIOS" ~ 1,
      TRUE ~ EDU_D6_Q1_3),
    EDU_D6_Q1_96 = case_when(
      EDU_D6_Q1_O == "TRAMITES MIGRATORIOS" ~ 0,
      TRUE ~ EDU_D6_Q1_96),
    EDU_D6_Q1_9 = case_when(
      EDU_D6_Q1_O == "ESTE AÑO QUIERE REGRESAR A VENEZUELA Y TOMARON LA DECISION DE NO INSCRIBIR A LOS NIÑOS" ~ 1,
      EDU_D6_Q1_O == "NO SABE SI REGRESA MAS ADELANTE A VENEZUELA" ~ 1,
      EDU_D6_Q1_O == "VA A VIAJAR AL EXTRANJERO." ~ 1,
      EDU_D6_Q1_O == "INDECISIÓN DE QUEDARSE EN EL PAÍS HA GENERADO QUE NO LO MATRICULE, LO TIENE EN TAREAS DIRIGIDAS (CLASES PARTICULARES)" ~ 1,
      EDU_D6_Q1_O == "Va a viajar a venezuela y regresa" ~ 1,
      EDU_D6_Q1_O == "PIENA RETORNAR A VENEZUELA" ~ 1,
      EDU_D6_Q1_O == "PENSABA HACER UN VIAJE PERO DECIDIO NO YA NO HACERLO" ~ 1,
      EDU_D1_Q1 == "0" ~ 0),
    EDU_D6_Q1_10 = case_when(    
      EDU_D6_Q1_O == "NO CONSIGUIÓ CUPO EN UN ESTADAL Y ESTÁ EMBARAZADA Y AL SER MADRE SOLTERA SE LE COMPLICA INGRESAR A UN PRIVADO" ~ 1,    
      EDU_D6_Q1_O == "Porque se caso y no quiso seguir estudiando." ~ 1,   
      EDU_D6_Q1_O == "No estudia porque está embarazada" ~ 1,   
      EDU_D6_Q1_O == "PORQUE LO VA A MATRICULAR LUEGO DE DAR A LUZ A SU SEGUNDO HIJO" ~ 1,   
      EDU_D6_Q1_O == "ES MADRE Y DEBE CUIDAR DE SU BB" ~ 1,   
      EDU_D6_Q1_O == "TIENE UNA BEBE Y DEJO DE ESTUDIAR" ~ 1,   
      EDU_D6_Q1_O == "La encuestada indicó que la menor no está llevando clases por el embarazo." ~ 1,   
      EDU_D6_Q1_O == "Estaba embarazada" ~ 1,   
      EDU_D1_Q1 == "0" ~ 0),
    EDU_D6_Q1_96 = case_when(
      EDU_D6_Q1_O == "ESTE AÑO QUIERE REGRESAR A VENEZUELA Y TOMARON LA DECISION DE NO INSCRIBIR A LOS NIÑOS" ~ 0,
      EDU_D6_Q1_O == "NO SABE SI REGRESA MAS ADELANTE A VENEZUELA" ~ 0,
      EDU_D6_Q1_O == "VA A VIAJAR AL EXTRANJERO." ~ 0,
      EDU_D6_Q1_O == "INDECISIÓN DE QUEDARSE EN EL PAÍS HA GENERADO QUE NO LO MATRICULE, LO TIENE EN TAREAS DIRIGIDAS (CLASES PARTICULARES)" ~ 0,
      EDU_D6_Q1_O == "Va a viajar a venezuela y regresa" ~ 0,
      EDU_D6_Q1_O == "PIENA RETORNAR A VENEZUELA" ~ 0,
      EDU_D6_Q1_O == "PENSABA HACER UN VIAJE PERO DECIDIO NO YA NO HACERLO" ~ 0,
      EDU_D6_Q1_O == "NO CONSIGUIÓ CUPO EN UN ESTADAL Y ESTÁ EMBARAZADA Y AL SER MADRE SOLTERA SE LE COMPLICA INGRESAR A UN PRIVADO" ~ 0,    
      EDU_D6_Q1_O == "Porque se caso y no quiso seguir estudiando." ~ 0,   
      EDU_D6_Q1_O == "No estudia porque está embarazada" ~ 0,   
      EDU_D6_Q1_O == "PORQUE LO VA A MATRICULAR LUEGO DE DAR A LUZ A SU SEGUNDO HIJO" ~ 1,   
      EDU_D6_Q1_O == "ES MADRE Y DEBE CUIDAR DE SU BB" ~ 0,   
      EDU_D6_Q1_O == "TIENE UNA BEBE Y DEJO DE ESTUDIAR" ~ 0,   
      EDU_D6_Q1_O == "La encuestada indicó que la menor no está llevando clases por el embarazo." ~ 0,   
      EDU_D6_Q1_O == "Estaba embarazada" ~ 0,  
      TRUE ~ EDU_D6_Q1_96))      


# EDU_D7_Q1_O --> 103 records
Individual <- Individual |>
  mutate(
    EDU_D3_Q1 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ "5",
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ "5",
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ "5",
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ "5",
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ "5",
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ "5",
      TRUE ~ EDU_D3_Q1),
    EDU_D7_Q1_7 = case_when(
      EDU_D7_Q1_O == "TIENE ASMA, HA ESTADO DELICADO DE SALUD." ~ 1,
      EDU_D7_Q1_O == "Tiene una patología: niproadenoma hipofiliaria" ~ 1,
      EDU_D7_Q1_O == "Salud" ~ 1,
      EDU_D7_Q1_O == "Salud, sufre de asma" ~ 1,
      EDU_D7_Q1_O == "SITUACIÓN DE SALUD Y NO TIENEN TODOS LOS ÚTILES." ~ 1,
      EDU_D7_Q1_O == "ENFERMEDAD" ~ 1,
      EDU_D7_Q1_O == "ESTA ENFERMA ESTA ULTIMA SEMANA" ~ 1,
      EDU_D7_Q1_O == "ESTA SEMANA FALTÓ PORQUE ESTUVO ENFERMA Y ESTABA YENDO AL HOSPITAL" ~ 1,
      EDU_D7_Q1_O == "ESTABA ENFERMO CON FIEBRE." ~ 1,
      EDU_D7_Q1_O == "ESTOY CON OTITIS" ~ 1,
      EDU_D7_Q1_O == "ESTUVO ENFERMOY PROBLEMAS ECONOMICOS" ~ 1,
      EDU_D7_Q1_O == "ESTUVO UNOS DIAS INDISPUESTO DE SALUD" ~ 1,
      EDU_D7_Q1_O == "POR MOTIVOS DE SALUD" ~ 1,
      EDU_D7_Q1_O == "POR SALUD" ~ 1,
      EDU_D7_Q1_O == "Por salud" ~ 1,
      EDU_D7_Q1_O == "POR SALUD SE ENFERMO DE DENGUE" ~ 1,
      EDU_D7_Q1_O == "HA ESTADO ENFERMA" ~ 1,
      EDU_D7_Q1_O == "SUFRIÓ UN ACCIDENTE EN EL COLEGIO Y TUVO LICENCIA MÉDICA POR 8 DÍAS." ~ 1,
      EDU_D7_Q1_O == "Recién operado porque tiene sindrome de extrolactili" ~ 1,
      EDU_D7_Q1_O == "PORQUE ESTÁ EN EL PROCESO DE ADAPTACIÓN Y ESTUVO ENFERMO LÁ ÚLTIMA SEMANA POR DIARREA" ~ 1,
      EDU_D7_Q1_O == "Por enfermedad de los oídos" ~ 1,
      EDU_D7_Q1_O == "El menor no asistió a la escuala para asistir a consultas de salud (solo tiene un riñon) y algunos días porque no hubo suficiente dinero para los alimentos y prefirieron que pueda descansar en casa." ~ 1,
      EDU_D7_Q1_O == "MOTIVOS DE SALUDO" ~ 1,
      EDU_D7_Q1_O == "NO TENIA UTILES, POR NO TENER DESAYUNO, ENFERMO DE TOS" ~ 1,
      EDU_D7_Q1_O == "No tiene dinero para enviarlo y esta enfermo." ~ 1,
      EDU_D7_Q1_O == "UTILES, DESAYUNO, TOS, ASMA" ~ 1,
      EDU_D3_Q1 == "1" | EDU_D3_Q1 == "2" | EDU_D3_Q1 == "3" | EDU_D3_Q1 == "4"  ~ 0,
      TRUE ~ NA),
    EDU_D7_Q1_96 = case_when(
      EDU_D7_Q1_O == "TIENE ASMA, HA ESTADO DELICADO DE SALUD." ~ 0,
      EDU_D7_Q1_O == "Tiene una patología: niproadenoma hipofiliaria" ~ 0,
      EDU_D7_Q1_O == "Salud" ~ 0,
      EDU_D7_Q1_O == "Salud, sufre de asma" ~ 0,
      EDU_D7_Q1_O == "SITUACIÓN DE SALUD Y NO TIENEN TODOS LOS ÚTILES." ~ 0,
      EDU_D7_Q1_O == "ENFERMEDAD" ~ 0,
      EDU_D7_Q1_O == "ESTA ENFERMA ESTA ULTIMA SEMANA" ~ 0,
      EDU_D7_Q1_O == "ESTA SEMANA FALTÓ PORQUE ESTUVO ENFERMA Y ESTABA YENDO AL HOSPITAL" ~ 0,
      EDU_D7_Q1_O == "ESTABA ENFERMO CON FIEBRE." ~ 0,
      EDU_D7_Q1_O == "ESTOY CON OTITIS" ~ 0,
      EDU_D7_Q1_O == "ESTUVO ENFERMOY PROBLEMAS ECONOMICOS" ~ 0,
      EDU_D7_Q1_O == "ESTUVO UNOS DIAS INDISPUESTO DE SALUD" ~ 0,
      EDU_D7_Q1_O == "POR MOTIVOS DE SALUD" ~ 0,
      EDU_D7_Q1_O == "POR SALUD" ~ 0,
      EDU_D7_Q1_O == "Por salud" ~ 0,
      EDU_D7_Q1_O == "POR SALUD SE ENFERMO DE DENGUE" ~ 0,
      EDU_D7_Q1_O == "HA ESTADO ENFERMA" ~ 0,
      EDU_D7_Q1_O == "SUFRIÓ UN ACCIDENTE EN EL COLEGIO Y TUVO LICENCIA MÉDICA POR 8 DÍAS." ~ 0,
      EDU_D7_Q1_O == "Recién operado porque tiene sindrome de extrolactili" ~ 0,
      EDU_D7_Q1_O == "PORQUE ESTÁ EN EL PROCESO DE ADAPTACIÓN Y ESTUVO ENFERMO LÁ ÚLTIMA SEMANA POR DIARREA" ~ 0,
      EDU_D7_Q1_O == "Por enfermedad de los oídos" ~ 0,
      EDU_D7_Q1_O == "El menor no asistió a la escuala para asistir a consultas de salud (solo tiene un riñon) y algunos días porque no hubo suficiente dinero para los alimentos y prefirieron que pueda descansar en casa." ~ 0,
      EDU_D7_Q1_O == "MOTIVOS DE SALUDO" ~ 0,
      EDU_D7_Q1_O == "NO TENIA UTILES, POR NO TENER DESAYUNO, ENFERMO DE TOS" ~ 0,
      EDU_D7_Q1_O == "No tiene dinero para enviarlo y esta enfermo." ~ 0,
      EDU_D7_Q1_O == "UTILES, DESAYUNO, TOS, ASMA" ~ 0,
      EDU_D7_Q1_O == "ES AUTISTA LO LLEVO 2 O 3 DIAS, SE ESTRESA" ~ 0,
      EDU_D7_Q1_O == "EL MENOR TIENE UNA CONDICION AUTISTA Y TDHA" ~ 0,
      TRUE ~ EDU_D7_Q1_96),
    EDU_D7_Q1_8 = case_when(
      EDU_D7_Q1_O == "El menor no asistió a la escuala para asistir a consultas de salud (solo tiene un riñon) y algunos días porque no hubo suficiente dinero para los alimentos y prefirieron que pueda descansar en casa." ~ 1,
      EDU_D7_Q1_O == "A VECES NO TIENEN ALIMENTOS SUFICIENTES PARA MANDARLA AL COLEGIO" ~ 1,
      EDU_D7_Q1_O == "Desayuno" ~ 1,
      EDU_D7_Q1_O == "FALTA DE MERIENDA" ~ 1,
      EDU_D7_Q1_O == "NO TENÍA DINERO PARA LA MERIENDA" ~ 1,
      EDU_D7_Q1_O == "No tenian del desayuno" ~ 1,
      EDU_D7_Q1_O == "POR FALTA DE ALIMENTOS" ~ 1,
      EDU_D7_Q1_O == "POR LA MERIENDA" ~ 1,
      EDU_D7_Q1_O == "PORQUE NO TENIA PARA DARLE ALIMENTOS" ~ 1,
      EDU_D7_Q1_O == "PORQUE NO TIENE COMIDA PARA DARLE AL MENOR Y NO LO ENVIA AL COLEGIO" ~ 1,
      EDU_D7_Q1_O == "PORQUE NO TIENE DINERO PARA BRINDARLE EL DESAYUNO" ~ 1,
      EDU_D7_Q1_O == "NO TUVO PARA LA MERIENDA ,LE EXIJEN EN EL COLEGIO" ~ 1,
      EDU_D7_Q1_O == "FALTA DEALIMENTOS" ~ 1,
      EDU_D7_Q1_O == "NO ASISTEN SIEMPRE POR LA ALIMENTACION" ~ 1,
      EDU_D7_Q1_O == "NO HA PODIDO HACERLE SU LONCHERA" ~ 1,
      EDU_D7_Q1_O == "NO LO MANDA AL CENTRO EDUCATIVO A VECES PORQUE NO TIENE PARA DARLE DESAYUNO" ~ 1,
      EDU_D7_Q1_O == "Tampoco cuenta con dinero para la lonchera" ~ 1,
      EDU_D7_Q1_O == "POR FALTA DE DINERO PARA LLEVARLO AL COLEGIO Y SUS MERIENDAS." ~ 1,
      EDU_D7_Q1_O == "no contaba con recursos para mandarle a la escuela, no habia para la comida ni los utiles" ~ 1,
      EDU_D7_Q1_O == "NO CUENTA CON DINERO PARA ENVIARLO AL COLEGIO, SUS MERIENDAS, GASTOS ESCOLARES" ~ 1,
      EDU_D7_Q1_O == "NO TENIA UTILES, POR NO TENER DESAYUNO, ENFERMO DE TOS" ~ 1,
      EDU_D7_Q1_O == "UTILES, DESAYUNO, TOS, ASMA" ~ 1,
      EDU_D7_Q1_O == "FALTA DE ROPA Y ALIMENTACIÓN PARA RENDIR EN EL COLEGIO." ~ 1,
      EDU_D7_Q1_O == "NO TIENE PARA SU LONCHERA Y LISTA DE UTILES" ~ 1,
      EDU_D3_Q1 == "1" | EDU_D3_Q1 == "2" | EDU_D3_Q1 == "3" | EDU_D3_Q1 == "4"  ~ 0,
      TRUE ~ NA),
    EDU_D7_Q1_96 = case_when(
      EDU_D7_Q1_O == "El menor no asistió a la escuala para asistir a consultas de salud (solo tiene un riñon) y algunos días porque no hubo suficiente dinero para los alimentos y prefirieron que pueda descansar en casa." ~ 0,
      EDU_D7_Q1_O == "A VECES NO TIENEN ALIMENTOS SUFICIENTES PARA MANDARLA AL COLEGIO" ~ 0,
      EDU_D7_Q1_O == "Desayuno" ~ 0,
      EDU_D7_Q1_O == "FALTA DE MERIENDA" ~ 0,
      EDU_D7_Q1_O == "NO TENÍA DINERO PARA LA MERIENDA" ~ 0,
      EDU_D7_Q1_O == "No tenian del desayuno" ~ 0,
      EDU_D7_Q1_O == "POR FALTA DE ALIMENTOS" ~ 0,
      EDU_D7_Q1_O == "POR LA MERIENDA" ~ 0,
      EDU_D7_Q1_O == "PORQUE NO TENIA PARA DARLE ALIMENTOS" ~ 0,
      EDU_D7_Q1_O == "PORQUE NO TIENE COMIDA PARA DARLE AL MENOR Y NO LO ENVIA AL COLEGIO" ~ 0,
      EDU_D7_Q1_O == "PORQUE NO TIENE DINERO PARA BRINDARLE EL DESAYUNO" ~ 0,
      EDU_D7_Q1_O == "NO TUVO PARA LA MERIENDA ,LE EXIJEN EN EL COLEGIO" ~ 0,
      EDU_D7_Q1_O == "FALTA DEALIMENTOS" ~ 0,
      EDU_D7_Q1_O == "NO ASISTEN SIEMPRE POR LA ALIMENTACION" ~ 0,
      EDU_D7_Q1_O == "NO HA PODIDO HACERLE SU LONCHERA" ~ 0,
      EDU_D7_Q1_O == "NO LO MANDA AL CENTRO EDUCATIVO A VECES PORQUE NO TIENE PARA DARLE DESAYUNO" ~ 0,
      EDU_D7_Q1_O == "Tampoco cuenta con dinero para la lonchera" ~ 0,
      EDU_D7_Q1_O == "POR FALTA DE DINERO PARA LLEVARLO AL COLEGIO Y SUS MERIENDAS." ~ 0,
      EDU_D7_Q1_O == "no contaba con recursos para mandarle a la escuela, no habia para la comida ni los utiles" ~ 0,
      EDU_D7_Q1_O == "NO CUENTA CON DINERO PARA ENVIARLO AL COLEGIO, SUS MERIENDAS, GASTOS ESCOLARES" ~ 0,
      EDU_D7_Q1_O == "NO TENIA UTILES, POR NO TENER DESAYUNO, ENFERMO DE TOS" ~ 0,
      EDU_D7_Q1_O == "UTILES, DESAYUNO, TOS, ASMA" ~ 0,
      EDU_D7_Q1_O == "FALTA DE ROPA Y ALIMENTACIÓN PARA RENDIR EN EL COLEGIO." ~ 0,
      EDU_D7_Q1_O == "NO TIENE PARA SU LONCHERA Y LISTA DE UTILES" ~ 0,
      TRUE ~ EDU_D7_Q1_96),
    EDU_D7_Q1_9 = case_when(
      EDU_D7_Q1_O == "No ha podido comprar los utiles escolares" ~ 1,
      EDU_D7_Q1_O == "NO HA PODIDO COMPRAR SUS UTILES" ~ 1,
      EDU_D7_Q1_O == "NO HA PODIDO COMPRARR LOS UTILES ESCOLARES" ~ 1,
      EDU_D7_Q1_O == "No contaba con recursos para los útiles y uniforme escolar" ~ 1,
      EDU_D7_Q1_O == "NO CUENTA CON DINERO PARA COMPRAR SUS ÚTILES" ~ 1,
      EDU_D7_Q1_O == "NO CUENTA CON DINERO PARA LOS UTILES DEL COLEGIO" ~ 1,
      EDU_D7_Q1_O == "NO CUENTAN CON RECURSOS ECONÓMICOS PARA LOS ÚTILES ESCOLARES" ~ 1,
      EDU_D7_Q1_O == "NO CUENTAN CON RECURSOS PARA COMPRAR LOS MATERIALES PARA LAS CLASES" ~ 1,
      EDU_D7_Q1_O == "NO PUDIERON COMPRAR UTILES ESCOLARES" ~ 1,
      EDU_D7_Q1_O == "NO CUENTA CON MATERIALES EDUCATIVOS CUANDO LE PIDEN EN LA ESCUELA, TBM NO TIENE QUIEN LA LLEVE" ~ 1,
      EDU_D7_Q1_O == "NO PUEDE COSTEAR LOS ÚTILES Y EL UNIFORME, Y SE LO ESTÁN PIDIENDO." ~ 1,
      EDU_D7_Q1_O == "Falta de útiles y uniforme, dinero" ~ 1,
      EDU_D7_Q1_O == "NO VA TODOS LOS DIAS POR FACTORES ECONOMICOS COMO LA HERMANA, NO TIENE PARA LOS MATERIALES EDUCATIVOS." ~ 1,
      EDU_D7_Q1_O == "no contaba con recursos para mandarle a la escuela, no habia para la comida ni los utiles" ~ 1,
      EDU_D7_Q1_O == "NO CUENTA CON DINERO PARA ENVIARLO AL COLEGIO, SUS MERIENDAS, GASTOS ESCOLARES" ~ 1,
      EDU_D7_Q1_O == "NO TENIA UTILES, POR NO TENER DESAYUNO, ENFERMO DE TOS" ~ 1,
      EDU_D7_Q1_O == "UTILES, DESAYUNO, TOS, ASMA" ~ 1,
      EDU_D7_Q1_O == "FALTA DE ROPA Y ALIMENTACIÓN PARA RENDIR EN EL COLEGIO." ~ 1,
      EDU_D7_Q1_O == "NO TIENE PARA SU LONCHERA Y LISTA DE UTILES" ~ 1,
      EDU_D7_Q1_O == "SITUACIÓN DE SALUD Y NO TIENEN TODOS LOS ÚTILES." ~ 1,
      EDU_D7_Q1_O == "NO CUENTAN CON LOS RECURSOS PARA LOS PAGOS SOLICITADOS EN LA ESCUELA Y LIMITAN EL ACCESO" ~ 1,
      EDU_D7_Q1_O == "NO CUENTAN CON RECURSOS PARA LOS PAGOS QUE PIDEN EN LAS ESCUELAS" ~ 1,
      EDU_D3_Q1 == "1" | EDU_D3_Q1 == "2" | EDU_D3_Q1 == "3" | EDU_D3_Q1 == "4"  ~ 0,
      TRUE ~ NA),
    EDU_D7_Q1_96 = case_when(
      EDU_D7_Q1_O == "No ha podido comprar los utiles escolares" ~ 0,
      EDU_D7_Q1_O == "NO HA PODIDO COMPRAR SUS UTILES" ~ 0,
      EDU_D7_Q1_O == "NO HA PODIDO COMPRARR LOS UTILES ESCOLARES" ~ 0,
      EDU_D7_Q1_O == "No contaba con recursos para los útiles y uniforme escolar" ~ 0,
      EDU_D7_Q1_O == "NO CUENTA CON DINERO PARA COMPRAR SUS ÚTILES" ~ 0,
      EDU_D7_Q1_O == "NO CUENTA CON DINERO PARA LOS UTILES DEL COLEGIO" ~ 0,
      EDU_D7_Q1_O == "NO CUENTAN CON RECURSOS ECONÓMICOS PARA LOS ÚTILES ESCOLARES" ~ 0,
      EDU_D7_Q1_O == "NO CUENTAN CON RECURSOS PARA COMPRAR LOS MATERIALES PARA LAS CLASES" ~ 0,
      EDU_D7_Q1_O == "NO PUDIERON COMPRAR UTILES ESCOLARES" ~ 0,
      EDU_D7_Q1_O == "NO CUENTA CON MATERIALES EDUCATIVOS CUANDO LE PIDEN EN LA ESCUELA, TBM NO TIENE QUIEN LA LLEVE" ~ 0,
      EDU_D7_Q1_O == "NO PUEDE COSTEAR LOS ÚTILES Y EL UNIFORME, Y SE LO ESTÁN PIDIENDO." ~ 0,
      EDU_D7_Q1_O == "Falta de útiles y uniforme, dinero" ~ 0,
      EDU_D7_Q1_O == "NO VA TODOS LOS DIAS POR FACTORES ECONOMICOS COMO LA HERMANA, NO TIENE PARA LOS MATERIALES EDUCATIVOS." ~ 0,
      EDU_D7_Q1_O == "no contaba con recursos para mandarle a la escuela, no habia para la comida ni los utiles" ~ 0,
      EDU_D7_Q1_O == "NO CUENTA CON DINERO PARA ENVIARLO AL COLEGIO, SUS MERIENDAS, GASTOS ESCOLARES" ~ 0,
      EDU_D7_Q1_O == "NO TENIA UTILES, POR NO TENER DESAYUNO, ENFERMO DE TOS" ~ 0,
      EDU_D7_Q1_O == "UTILES, DESAYUNO, TOS, ASMA" ~ 0,
      EDU_D7_Q1_O == "FALTA DE ROPA Y ALIMENTACIÓN PARA RENDIR EN EL COLEGIO." ~ 0,
      EDU_D7_Q1_O == "NO TIENE PARA SU LONCHERA Y LISTA DE UTILES" ~ 0,
      EDU_D7_Q1_O == "SITUACIÓN DE SALUD Y NO TIENEN TODOS LOS ÚTILES." ~ 0,
      EDU_D7_Q1_O == "NO CUENTAN CON LOS RECURSOS PARA LOS PAGOS SOLICITADOS EN LA ESCUELA Y LIMITAN EL ACCESO" ~ 0,
      EDU_D7_Q1_O == "NO CUENTAN CON RECURSOS PARA LOS PAGOS QUE PIDEN EN LAS ESCUELAS" ~ 0,
      TRUE ~ EDU_D7_Q1_96),
    EDU_D7_Q1_2 = case_when(
      EDU_D7_Q1_O == "A veces no cuentan con dinero para enviarla." ~ 1,
      EDU_D7_Q1_O == "SE QUEDA CON LA MAMA PARA IR AL TRABAJO, LA ACOMPANA, PORQUE NO TIENE DINERO PARA ENVIARLA AL COLEGIO, PASAJE" ~ 1,
      EDU_D7_Q1_O == "POR FALTA DE DINERO PARA LLEVARLO AL COLEGIO Y SUS MERIENDAS." ~ 1,
      EDU_D7_Q1_O == "no contaba con recursos para mandarle a la escuela, no habia para la comida ni los utiles" ~ 1,
      EDU_D7_Q1_O == "No tiene dinero para enviarlo y esta enfermo." ~ 1,
      TRUE ~ EDU_D7_Q1_2),
    EDU_D7_Q1_96 = case_when(
      EDU_D7_Q1_O == "A veces no cuentan con dinero para enviarla." ~ 0,
      EDU_D7_Q1_O == "NO CUENTA CON LOS RECURSOS ECONOMICOS PARA LLEVARLO TODOS LOS DIAS A LA ESCUELA" ~ 0,
      EDU_D7_Q1_O == "SE QUEDA CON LA MAMA PARA IR AL TRABAJO, LA ACOMPANA, PORQUE NO TIENE DINERO PARA ENVIARLA AL COLEGIO, PASAJE" ~ 0,
      EDU_D7_Q1_O == "POR FALTA DE DINERO PARA LLEVARLO AL COLEGIO Y SUS MERIENDAS." ~ 0,
      EDU_D7_Q1_O == "no contaba con recursos para mandarle a la escuela, no habia para la comida ni los utiles" ~ 0,
      EDU_D7_Q1_O == "NO CUENTA CON DINERO PARA ENVIARLO AL COLEGIO, SUS MERIENDAS, GASTOS ESCOLARES" ~ 0,
      EDU_D7_Q1_O == "No tiene dinero para enviarlo y esta enfermo." ~ 0,
      EDU_D7_Q1_O == "Falta de dinero en general." ~ 0,
      TRUE ~ EDU_D7_Q1_96),
    EDU_D7_Q1_6 = case_when(
      EDU_D7_Q1_O == "ES AUTISTA LO LLEVO 2 O 3 DIAS, SE ESTRESA" ~ 1,
      EDU_D7_Q1_O == "EL MENOR TIENE UNA CONDICION AUTISTA Y TDHA" ~ 1,
      TRUE ~ EDU_D7_Q1_6),
    EDU_D7_Q1 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA,
      TRUE ~ EDU_D7_Q1),
    EDU_D7_Q1_1 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA,
      EDU_D7_Q1_O == "Se mudaron esta semana y les queda lejos el colegio" ~ 1,
      TRUE ~ EDU_D7_Q1_1),
    EDU_D7_Q1_2 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA,
      TRUE ~ EDU_D7_Q1_2),
    EDU_D7_Q1_3 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA,
      TRUE ~ EDU_D7_Q1_3),
    EDU_D7_Q1_4 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA,
      TRUE ~ EDU_D7_Q1_4),
    EDU_D7_Q1_5 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA,
      TRUE ~ EDU_D7_Q1_5),
    EDU_D7_Q1_6 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA,
      TRUE ~ EDU_D7_Q1_6),
    EDU_D7_Q1_96 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA,
      EDU_D7_Q1_O == "Se mudaron esta semana y les queda lejos el colegio" ~ 0,
      TRUE ~ EDU_D7_Q1_96),
    EDU_D7_Q1_98 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA,
      TRUE ~ EDU_D7_Q1_98),
    EDU_D7_Q1_99 = case_when(
      EDU_D7_Q1_O == "porque un dia tienen libre y salen de paseo 1 dia de la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela dectestigo de Jehová , solo hay clase 3 veces a la semana" ~ NA,
      EDU_D7_Q1_O == "Escuela testigo de Jehová , solo asisten tres dias a la semana" ~ NA,
      EDU_D7_Q1_O == "ES EL HORARIO DE LA INSTITUCIÓN" ~ NA,
      EDU_D7_Q1_O == "ESTUDIA DE NOCHE" ~ NA,
      EDU_D7_Q1_O == "Es un colegio nocturno  CEBA" ~ NA))


# HE_D3_Q1_O  --> 56 records
Individual <- Individual |>
  mutate(
    HE_D3_Q1_12 = case_when(
      HE_D3_Q1_O == "no tiene sis" ~ 1,
      HE_D3_Q1_O == "No tiene sis" ~ 1,
      HE_D3_Q1_O == "NO TIENE SIS" ~ 1,
      HE_D3_Q1_O == "NO TENIA SIS" ~ 1,
      HE_D3_Q1_O == "NO CUENTA CON EL SIS" ~ 1,
      HE_D3_Q1_O == "PORQUE NO TIENE SIS" ~ 1,
      HE_D3_Q1_O == "NO HA PODIDO REGISTRARSE EN EL SIS" ~ 1,
      HE_D3_Q1_O == "NO CONTABA CON SIS" ~ 1,
      HE_D3_Q1_O == "no tenía seguro" ~ 1,
      HE_D3_Q1_O == "NO LO ATIENDEN PORQUE NO TIENE SIS" ~ 1,
      HE_D3_Q1_O == "NO ESTABA AFILIADO AL SISTEMA" ~ 1,
      HE_D3_Q1_O == "POR QUE NO TIENE SIS Y CARNET DE EXTRANJERIA" ~ 1,
      HE_D3_Q1_O == "NO PUEDE ACCEDER AL SIS PORQUE TIENEN CPP" ~ 1,
      HE_D3_Q1_O == "TIENE UN LIPOMA Y NO CUENTA CON SIS" ~ 1,
      HE_D3_Q1_O == "FUE PARA ATENDERSE CON EL MEDICO Y NO LE QUISIERON ATENDER PORQUE NO TIENE SIS" ~ 1,
      HE_D3_Q1_O == "Seguro de salud no asumió gastó" ~ 1,
      HE_D3_Q1_O == "PORQUE SE MUDARON Y NO CUENTA CON SEGURO SIS" ~ 1,
      HE_D3_Q1_O == "Indicaron que no tenía seguro del trabajo activado en el momento que debía atenderse." ~ 1,
      HE_D1_Q2 == "0" ~ 0),
    HE_D3_Q1_7 = case_when(
      HE_D3_Q1_O == "POR NO TENER CARNET DE EXTRANJERIA" ~ 1,
      HE_D3_Q1_O == "por falta de domentación" ~ 1,
      HE_D3_Q1_O == "NO CONTABA CON EL CARNET DE EXTRANJERIA" ~ 1,
      HE_D3_Q1_O == "NO PUEDE ACCEDER AL SIS PORQUE TIENEN CPP" ~ 1,
      HE_D3_Q1_O == "POR QUE NO TIENE SIS Y CARNET DE EXTRANJERIA" ~ 1,
      TRUE ~ HE_D3_Q1_7),
    HE_D3_Q1_96 = case_when(
      HE_D3_Q1_O == "POR NO TENER CARNET DE EXTRANJERIA" ~ 0,
      HE_D3_Q1_O == "por falta de domentación" ~ 0,
      HE_D3_Q1_O == "NO CONTABA CON EL CARNET DE EXTRANJERIA" ~ 0,
      HE_D3_Q1_O == "NO PUEDE ACCEDER AL SIS PORQUE TIENEN CPP" ~ 0,
      HE_D3_Q1_O == "POR QUE NO TIENE SIS Y CARNET DE EXTRANJERIA" ~ 0,
      HE_D3_Q1_O == "Falta de tiempo" ~ 0,
      HE_D3_Q1_O == "Por su trabajo" ~ 0,
      HE_D3_Q1_O == "NO CONSIGUE CITA PARA ATENCION EN EL HOSPITAL" ~ 0,
      HE_D3_Q1_O == "Seguro de salud no asumió gastó" ~ 0,
      TRUE ~ HE_D3_Q1_96),
    HE_D3_Q1_5 = case_when(
      HE_D3_Q1_O == "Falta de tiempo" ~ 1,
      HE_D3_Q1_O == "Por su trabajo" ~ 1,
      TRUE ~ HE_D3_Q1_5),
    HE_D3_Q1_8 = case_when(
      HE_D3_Q1_O == "NO CONSIGUE CITA PARA ATENCION EN EL HOSPITAL" ~ 1,
      TRUE ~ HE_D3_Q1_8),
    HE_D3_Q1_2 = case_when(
      HE_D3_Q1_O == "Seguro de salud no asumió gastó" ~ 1,
      TRUE ~ HE_D3_Q1_2))    


# Re categorization of "other" in Household dataset

# INT_D3_Q2_O --> 21 records ~ 0
HH <- HH |>
  mutate(
    INT_D3_Q2_2 = case_when(
      `_id` ==  49040578 ~ 1,
      TRUE ~ INT_D3_Q2_2),
    INT_D3_Q2_3 = case_when(
      `_id` ==  49113816 ~ 1,
      TRUE ~ INT_D3_Q2_3),
    INT_D3_Q2_4 = case_when(
      INT_D3_Q2_O == "ACCESO A UN TRAMITE LEGAL" ~ 1,
      INT_D3_Q2_O == "EN LIMA, CUANDO FUERON A MIGRACIONES" ~ 1,
      INT_D3_Q2_O == "En la comisaría" ~ 1,
      TRUE ~ INT_D3_Q2_4),
    INT_D3_Q2_6 = case_when(
      INT_D3_Q2_O == "AGENCIA MOVISTAR EN TRUJILLO" ~ 1,
      INT_D3_Q2_O == "CENTRO COMERCIAL" ~ 1,
      INT_D3_Q2_O == "En una barbería" ~ 1,
      INT_D3_Q2_O == "Centro comercial" ~ 1,
      INT_D3_Q2_O == "EN UN AUTOLAVADO" ~ 1,
      TRUE ~ INT_D3_Q2_6),
    INT_D3_Q2_8 = case_when(
      INT_D3_Q2_O == "sitio recreacional" ~ 1,
      INT_D3_Q2_O == "Parques públicos" ~ 1,
      INT_D3_Q2_O == "En su casa" ~ 1,
      `_id` ==  48950722 ~ 1,
      `_id` ==  49040578 ~ 1,
      TRUE ~ INT_D3_Q2_8),
    INT_D3_Q2_9 = case_when(
      INT_D3_Q2_O == "HOSPITAL" ~ 1,
      INT_D3_Q2_O == "HOSPITAL BELEN DE TRUJILLO" ~ 1,
      TRUE ~ INT_D3_Q2_9),
    INT_D3_Q2_10 = case_when(
      INT_D3_Q2_O == "En su casa" ~ 0,
      INT_D3_Q2_O == "EN EL COLEGIO LO ATACARON CON UN CUTER UN COMPAÑERO DEL COLEGIO" ~ 0,
      INT_D3_Q2_O == "ACCESO A UN TRAMITE LEGAL" ~ 0,
      INT_D3_Q2_O == "PARQUES" ~ 0,
      INT_D3_Q2_O == "sitio recreacional" ~ 0,
      INT_D3_Q2_O == "COLEGIO POR REUNIONES DE PADRES" ~ 0,
      INT_D3_Q2_O == "AGENCIA MOVISTAR EN TRUJILLO" ~ 0,
      INT_D3_Q2_O == "HOSPITAL" ~ 0,
      INT_D3_Q2_O == "CENTRO COMERCIAL" ~ 0,
      INT_D3_Q2_O == "HOSPITAL BELEN DE TRUJILLO" ~ 0,
      INT_D3_Q2_O == "En una barbería" ~ 0,
      INT_D3_Q2_O == "EN LIMA, CUANDO FUERON A MIGRACIONES" ~ 0,
      INT_D3_Q2_O == "Parques públicos" ~ 0,
      INT_D3_Q2_O == "Sitios públicos, parques" ~ 0,
      INT_D3_Q2_O == "Centro comercial" ~ 0,
      INT_D3_Q2_O == "EN UN AUTOLAVADO" ~ 0,
      INT_D3_Q2_O == "SU VECINA LA ODIA" ~ 0,
      INT_D3_Q2_O == "En la comisaría" ~ 0,
      `_id` ==  48950722 ~ 0,
      `_id` ==  49040578 ~ 0,
      `_id` ==  49113816 ~ 0,
      TRUE ~ INT_D3_Q2_10))

# INT_D4_Q1_O --> 1 record
HH <- HH |>
  mutate(
    INT_D4_Q1_10 = case_when(
      INT_D4_Q1_O == "YAPE A NOMBRE DE OTRA PERSONA" ~ 1,
      TRUE ~ INT_D4_Q1_10),
    INT_D4_Q1_96 = case_when(
      INT_D4_Q1_O == "YAPE A NOMBRE DE OTRA PERSONA" ~ 0,
      TRUE ~ INT_D4_Q1_96))


# HT_D1_Q1_first_o --> 9 records 
# HT_D1_Q1_second_o --> 6 records
HH <- HH |>
  mutate(
    HT_D1_Q1_first = case_when(
      HT_D1_Q1_first_o == "NO TRABAJA" ~ "2", #moda
      HT_D1_Q1_first_o == "NINGUNO, YA QUE TRABAJA DE TEXISTA" ~ "5",
      HT_D1_Q1_first_o == "Cuando los dueños de la chacra van aprovechan de salir" ~ "2", #moda
      HT_D1_Q1_first_o == "MOTO ALQUILADA" ~ "5",
      HT_D1_Q1_first_o == "CARRO PARTICULAR" ~ "5",
      HT_D1_Q1_first_o == "MOTOTAXI PERO ES LO QUE ALQUILA PARA TRABAJAR" ~ "5",
      HT_D1_Q1_first_o == "TRABAJA CON MOVILIDAD ALQUILADA" ~ "5",
      HT_D1_Q1_first_o == "MOTO ALQUILADA" ~ "5",
      HT_D1_Q1_first_o == "No utiliza transporte , porque trabaja desde su hogar" ~ "8",
      HT_D2_Q1_O == "ALQUILAN LA MOTO PARA TRABAJAR Y SE MOVILIZAN CON ESO" ~ "5",
      HT_D2_Q1_O == "MOTO QUE ES EL MEDIO DE TRABAJO Y LO UTILIZAN PARA SUS MOVILIDADES PERSONALES" ~ "5", 
      HT_D2_Q1_O == "TIENE UNA MOTOTAXI" ~ "5",
      TRUE ~ HT_D1_Q1_first),
    HT_D1_Q1_second = case_when(
      HT_D1_Q1_second_o == "TREN ELECTRICO" ~ "8", 
      HT_D1_Q1_second_o == "MOVILIDAD ESCOLAR" ~ "5",
      HT_D1_Q1_second_o == "TRANSPORTE DE LA EMPRESA" ~ "5",
      HT_D1_Q1_second_o == "MOTOTAXI" ~ "8",
      TRUE ~ HT_D1_Q1_second))


# HT_D2_Q1_O --> 66 records
HH <- HH |>
  mutate(
    HT_D1_Q1_first = case_when(
      HT_D2_Q1_O == "Tiene moto propia" ~ "5",
      TRUE ~ HT_D1_Q1_first),
    HT_D2_Q1 = case_when(
      HT_D2_Q1_O == "PORQUE POR DONDE VIVE ESTA MÁS CERCA LOS PARADEROS Y ES MÁS ECONOMICO" ~ "4",
      HT_D2_Q1_O == "CERCANÍA CON LUGAR DE TRABAJO" ~ "5",
      HT_D2_Q1_O == "DESTINO CERCANO" ~ "5",
      HT_D2_Q1_O == "DESTINOS CERCA" ~ "5",
      HT_D2_Q1_O == "DESTINOS CERCANOS" ~ "5",
      HT_D2_Q1_O == "ES CERCA A DONDE VIVE" ~ "5",
      HT_D2_Q1_O == "Le queda cerca" ~ "5",
      HT_D2_Q1_O == "ME QUEDA CERCA" ~ "5",
      HT_D2_Q1_O == "Pasa cerca de su domicilio" ~ "5",
      HT_D2_Q1_O == "Por cercania" ~ "5",
      HT_D2_Q1_O == "POR CERCANÍA" ~ "5",
      HT_D2_Q1_O == "Por cercanía" ~ "5",
      HT_D2_Q1_O == "Por la cercania del trabajo a su casa" ~ "5",
      HT_D2_Q1_O == "POR Q ES CERCA" ~ "5",
      HT_D2_Q1_O == "POR SER LO MAS CERCANO QUE TIENE Y LA LLEVA AL TRABAJO" ~ "5",
      HT_D2_Q1_O == "porque esta cerca y prefiere caminar" ~ "5",
      HT_D2_Q1_O == "PORQUE LE QUEDA CERCA" ~ "5",
      HT_D2_Q1_O == "PORQUE TODO ESTA CERCA Y LO PUEDO HACER CAMINANDO" ~ "5",
      HT_D2_Q1_O == "TIENE EL COLEGIO Y EL TRABAJO CERCA" ~ "5",
      HT_D2_Q1_O == "POR SALUD" ~ "9",
      TRUE ~ HT_D2_Q1))

# SHE_D1_Q1_O --> 107 records 
HH <- HH |>
  mutate(
    SHE_D1_Q1_O = case_when(
      `_id` == 48942853 ~ "Cuarto alquilado",
      `_id` == 49052964 ~ "Cuarto alquilado",
      `_id` == 49113171 ~ "Cuarto",
      `_id` == 49116448 ~ "Cuarto alquilado",
      `_id` == 49294167 ~ "Cuarto",
      `_id` == 49296432 ~ "Cuarto alquilado dentro de vivienda",
      `_id` == 49296909 ~ "Cuarto alquilado",
      `_id` == 49297582 ~ "Cuarto alquilado",
      TRUE ~ SHE_D1_Q1_O),
    SHE_D1_Q1 = case_when(
      SHE_D1_Q1_O == "CASA DE MADERA ALQUILADA" ~ "2",
      SHE_D1_Q1_O == "MINI DEPARTAMENTO" ~ "2",
      SHE_D1_Q1_O == "MINI DEPARTAMENTO ALQUILADO" ~ "2",
      SHE_D1_Q1_O == "MINI DEPARTAMENTO EN ALQUILER" ~ "2",
      SHE_D1_Q1_O == "MINIDEPARTAMENTO" ~ "2",
      SHE_D1_Q1_O == "VIVIENDA ALQUILADA POR FAMILIAR Y LO CUIDA, NO PAGA ALQUILER." ~ "2",
      SHE_D1_Q1_O == "A CUIDADO DE CASA" ~ "3",
      SHE_D1_Q1_O == "LA HAN ACOJIDO EN UNA CASA" ~ "3",
      SHE_D1_Q1_O == "LE DIERON UN ESPACIO PARA QUE VIVA CON SU FAMILIA SOLO PAGA LOS SERVICIOS" ~ "3",
      SHE_D1_Q1_O == "VIVE EN UNA CASA DADA POR LOS EMPLEADORES" ~ "3",
      SHE_D1_Q1_O == "VIVIENDA DEL PAPA DE LOS NIÑOS. EL PAPA NO VIVE EN CASA" ~ "3",
      SHE_D1_Q1_O == "CASA EN CONSTRUCCION" ~ "10",
      SHE_D1_Q1_O == "LE DIERON UN TERRENO E HIZO SU CASITA DE TRIPLAY" ~ "10",
      SHE_D1_Q1_O == "LUEGO DE UNA SITUACION FUERTE DE VBG SE QUEDO SIN HOGAR Y LE CEDEN UN LOCA L DONDE FUNCIONA UNA BARBERIA PARA QUE PUEDA QUEDARSE CON EL NIÑO POR LAS NOCHES, AHI NO LE COBRAN ARRIENDO, ACTUALMENTE REUNE EL DINERO PARA PAGAR EL DEPOSITO DE UN ALQUILER" ~ "10",
      SHE_D1_Q1_O == "UN ESTACIONAMIENTO" ~ "10",
      SHE_D1_Q1_O == "CIUARTO" ~ "13",
      SHE_D1_Q1_O == "CUARTO" ~ "13",
      SHE_D1_Q1_O == "CUARTO DEL TRABAJO DEL ESPOSO" ~ "13",
      SHE_D1_Q1_O == "CUARTO EN ALQUILER" ~ "13",
      SHE_D1_Q1_O == "CUARTO GRANDE" ~ "13",
      SHE_D1_Q1_O == "CURTO" ~ "13",
      SHE_D1_Q1_O == "Habitacion" ~ "13",
      SHE_D1_Q1_O == "HABITACION" ~ "13",
      SHE_D1_Q1_O == "HABITACIÓN" ~ "13",
      SHE_D1_Q1_O == "HABITACION ALQUILADA" ~ "13",
      SHE_D1_Q1_O == "HABITACIÓN ALQUILADA" ~ "13",
      SHE_D1_Q1_O == "HABITACION EN ALQUILER" ~ "13",
      SHE_D1_Q1_O == "HABITACION EN CASA EN LITIGIO" ~ "13",
      SHE_D1_Q1_O == "HABITACIÓN GRANDE" ~ "13",
      SHE_D1_Q1_O == "HABITACIONES" ~ "13",
      SHE_D1_Q1_O == "UNA HABITACIÓN ALQUILADA" ~ "13",
      SHE_D1_Q1_O == "Cuarto alquilado" ~ "13",
      SHE_D1_Q1_O == "Cuarto" ~ "13",
      SHE_D1_Q1_O == "Cuarto alquilado dentro de vivienda" ~ "13",
      SHE_D1_Q1_O == "1" ~ "98",
      TRUE ~ SHE_D1_Q1)
  )

# PRO_D1_Q1_O --> 15 records 
HH <- HH |>
  mutate(
    PRO_D1_Q1_3 = case_when(
      PRO_D1_Q1_O == "YOSKARLIS ANTONELLA LA HIJA DE 7 AÑOS HA RECIBIDO ACOSO, LA MADRE HA RECIBIO MENSAJES QUE LE HACEN PENSAR QUE LA HIJA ESTA EN RIESGO." ~ 1,
      PRO_D1_Q1_O == "ESCUCHO QUE HAY ALGUIEN QUE LO AMENAZAN, POR FACEBOOK TAMBIEN LE HAN ESCRITO QUE LO VAN A SECUESTRAR, TODO ESTA EN MESENGER" ~ 1,
      TRUE ~ PRO_D1_Q1_3),
    PRO_D1_Q1_4 = case_when(
      PRO_D1_Q1_O == "LA ENCUESTADA TUVO UNA PAREJA POLICIA QUE LE QUITO SU PASAPORTE, TUVO QUE DENUNCIARLO EN LA COMISARIA  DELA MUJER PARA QUE LE DEVUELVA EL PASAPORTE" ~ 1,
      TRUE ~ PRO_D1_Q1_4),
    PRO_D1_Q1_6 = case_when(
      PRO_D1_Q1_O == "Arresto y detención por trabajo informal" ~ 1,
      TRUE ~ PRO_D1_Q1_6),
    PRO_D1_Q1_7 = case_when(
      PRO_D1_Q1_O == "SU HIJO AULISES ALEXANDER ESTUVO VARIOS DIAS DESAPARECIDO Y REALIZO LA DENUNCIA A LA POLICIA" ~ 1,
      TRUE ~ PRO_D1_Q1_7),    
    PRO_D1_Q1_98 = case_when(
      PRO_D1_Q1_O == "NO CONTAR CON AYUDA ECONOMICA PARA EL TRATAMIENTO DE SU MENOR HIJA" ~ 1,
      PRO_D1_Q1_O == "Falta de regulación de su menor hija" ~ 1,
      PRO_D1_Q1_O == "ACCIDENTE" ~ 1,
      PRO_D1_Q1_O == "Al responder esta pregunta indicó que su menor hijo se ha visto afectado por la mudanza constante." ~ 1,
      TRUE ~ PRO_D1_Q1_98),
    PRO_D1_Q1_96 = case_when(
      PRO_D1_Q1_O == "NO CONTAR CON AYUDA ECONOMICA PARA EL TRATAMIENTO DE SU MENOR HIJA" ~ 0,
      PRO_D1_Q1_O == "Falta de regulación de su menor hija" ~ 0,
      PRO_D1_Q1_O == "ACCIDENTE" ~ 0,
      PRO_D1_Q1_O == "Al responder esta pregunta indicó que su menor hijo se ha visto afectado por la mudanza constante." ~ 0,
      PRO_D1_Q1_O == "SU ESPOSO ESTA EN GRUPOS DE TRABAJO EN WHATSAPP Y LOS COMPANEROS LO MOLESTAN Y LO CRITICAN CONSTANTEMENTE" ~ 0,
      PRO_D1_Q1_O == "LA ENCUESTADA TUVO UNA PAREJA POLICIA QUE LE QUITO SU PASAPORTE, TUVO QUE DENUNCIARLO EN LA COMISARIA  DELA MUJER PARA QUE LE DEVUELVA EL PASAPORTE" ~ 0,
      PRO_D1_Q1_O == "Arresto y detención por trabajo informal" ~ 0,
      PRO_D1_Q1_O == "SU HIJO AULISES ALEXANDER ESTUVO VARIOS DIAS DESAPARECIDO Y REALIZO LA DENUNCIA A LA POLICIA" ~ 0,
      PRO_D1_Q1_O == "YOSKARLIS ANTONELLA LA HIJA DE 7 AÑOS HA RECIBIDO ACOSO, LA MADRE HA RECIBIO MENSAJES QUE LE HACEN PENSAR QUE LA HIJA ESTA EN RIESGO." ~ 0,
      PRO_D1_Q1_O == "Estan utilizando su imagen para estafar" ~ 0,
      TRUE ~ PRO_D1_Q1_96))

# PRO_D2_Q1B_O --> 7 records
HH <- HH |>
  mutate(
    PRO_D2_Q1B_1 = case_when(
      PRO_D2_Q1B_O == "NO  TENIA EL CPP  CUANDO VIAJO A ECUADOR, Y NO QUERIAN DEJARLE REGRESAR A LA PAIS" ~ 1,
      TRUE ~ PRO_D2_Q1B_1), 
    PRO_D2_Q1B_6 = case_when(
      PRO_D2_Q1B_O == "Secuestro" ~ 1,
      TRUE ~ PRO_D2_Q1B_6), 
    PRO_D2_Q1B_10 = case_when(
      PRO_D2_Q1B_O == "FALTA DE PERMISO DEL PADRE" ~ 1,
      TRUE ~ PRO_D2_Q1B_10),      
    PRO_D2_Q1B_96 = case_when(
      PRO_D2_Q1B_O == "FALTA DE PERMISO DEL PADRE" ~ 0,
      PRO_D2_Q1B_O == "POR PASAPORTE LE COBRARON PARA PODER INGRESAR" ~ 0,
      PRO_D2_Q1B_O == "ESTAFARON" ~ 0,
      PRO_D2_Q1B_O == "NO  TENIA EL CPP  CUANDO VIAJO A ECUADOR, Y NO QUERIAN DEJARLE REGRESAR A LA PAIS" ~ 0,
      PRO_D2_Q1B_O == "Secuestro" ~ 0,
      TRUE ~ PRO_D2_Q1B_96))

# PRO_D3_Q1B_O --> 15 records 
HH <- HH |>
  mutate(
    PRO_D3_Q1B_1 = case_when(
      PRO_D3_Q1B_O == "SACAR UNA LICENCIA DE CONDUCIR" ~ 1,
      TRUE ~ PRO_D3_Q1B_1),
    PRO_D3_Q1B_3 = case_when(
      PRO_D3_Q1B_O == "Pasaporte" ~ 1,
      PRO_D3_Q1B_O == "CARNET DE EXTRAJERIA PARA SU NIÑO" ~ 1,
      PRO_D3_Q1B_O == "CPP" ~ 1,
      TRUE ~ PRO_D3_Q1B_3),
    PRO_D3_Q1B_4 = case_when(
      PRO_D3_Q1B_O == "DENUNCIA POR VBG" ~ 1,
      PRO_D3_Q1B_O == "Denuncia en contra de su ex pareja por abuso sexusl de su hija" ~ 1,
      TRUE ~ PRO_D3_Q1B_4),
    PRO_D3_Q1B_96 = case_when(
      PRO_D3_Q1B_O == "SACAR UNA LICENCIA DE CONDUCIR" ~ 0,
      PRO_D3_Q1B_O == "Pasaporte" ~ 0,
      PRO_D3_Q1B_O == "CARNET DE EXTRAJERIA PARA SU NIÑO" ~ 0,
      PRO_D3_Q1B_O == "CPP" ~ 0,
      PRO_D3_Q1B_O == "PARA TRAMITAR EL CARNET DEL PERMISO TEMPORAL DE PERMANENCIA NO FUNCIONA LA PAGINA DE MIGRACIONES Y TIENE A SUS HIJOS INDOCUMENTADOS" ~ 0,
      PRO_D3_Q1B_O == "DENUNCIA POR VBG" ~ 0,
      PRO_D3_Q1B_O == "Denuncia en contra de su ex pareja por abuso sexusl de su hija" ~ 0,
      TRUE ~ PRO_D3_Q1B_96))

# GBV_D1_Q1B_O --> 15 records
HH <- HH |>
  mutate(
    GBV_D1_Q1B_1 = case_when(
      GBV_D1_Q1B_O == "CENTRO DE LA CIUDAD" ~ 1,
      GBV_D1_Q1B_O == "Evita salir sola  despues de las 7pm" ~ 1,
      GBV_D1_Q1B_O == "Evitan estar de noche en la calle" ~ 1,
      GBV_D1_Q1B_O == "Prefiere no salir sola de noche" ~ 1,
      GBV_D1_Q1B_O == "LOS ESPACIOS PÚBLICOS DEL LUGAR DONDE RESIDEN" ~ 1,
      GBV_D1_Q1B_O == "EVITA SALIR DE NOCHE" ~ 1,
      GBV_D1_Q1B_O == "CALLES CERCANAS" ~ 1,
      GBV_D1_Q1B_O == "SU ESPOSA EVITA SALIR DE NOCHE" ~ 1,
      GBV_D1_Q1B_O == "NO SALEN DE NOCHE" ~ 1,
      TRUE ~ GBV_D1_Q1B_1),
    GBV_D1_Q1B_4 = case_when(
      GBV_D1_Q1B_O == "IR HACER COMPRAS A GAMARRA" ~ 1,
      TRUE ~ GBV_D1_Q1B_4),
    GBV_D1_Q1B_96 = case_when(
      GBV_D1_Q1B_O == "CENTRO DE LA CIUDAD" ~ 0,
      GBV_D1_Q1B_O == "Evita salir sola  despues de las 7pm" ~ 0,
      GBV_D1_Q1B_O == "Evitan estar de noche en la calle" ~ 0,
      GBV_D1_Q1B_O == "Prefiere no salir sola de noche" ~ 0,
      GBV_D1_Q1B_O == "LOS ESPACIOS PÚBLICOS DEL LUGAR DONDE RESIDEN" ~ 0,
      GBV_D1_Q1B_O == "EVITA SALIR DE NOCHE" ~ 0,
      GBV_D1_Q1B_O == "CALLES CERCANAS" ~ 0,
      GBV_D1_Q1B_O == "SU ESPOSA EVITA SALIR DE NOCHE" ~ 0,
      GBV_D1_Q1B_O == "NO SALEN DE NOCHE" ~ 0,
      GBV_D1_Q1B_O == "IR HACER COMPRAS A GAMARRA" ~ 0,
      TRUE ~ GBV_D1_Q1B_96))

#######################################
##### Step 8. Food security check #####
#######################################

# Extra quality data check on food security section. PULSO identified there where some HH with no children but had used FS_D2_Q4 option as well as FS_D4_Q4

# Summarize the data to count the occurrences of each age for each parent_index

summarized_individual <- Individual |>
  filter(HH07 < 18) |>
  select(`_parent_index`, HH07) |>
  group_by(`_parent_index`, HH07) |>
  summarise(count = n()) |>
  ungroup()

# Spread the data    
summarized_individual <- summarized_individual |>
  pivot_wider(names_from = HH07, values_from = count, values_fill = 0)     

# Order the columns from lower to higher value
age_columns <- as.numeric(names(summarized_individual)[-1])
sorted_columns <- c("_parent_index", sort(age_columns))
summarized_individual <- summarized_individual[, c(as.character(sorted_columns))]

colnames(summarized_individual)[-1] <- paste0("Menor_", colnames(summarized_individual)[-1], "_años")

HH <- merge(HH, summarized_individual, by.x = "_index", by.y = "_parent_index", all.x = TRUE)

HH <- HH |>
  mutate(
    Menores_0_3 = rowSums(cbind(Menor_0_años, Menor_1_años, Menor_2_años, Menor_3_años), na.rm = TRUE),
    Menores_0_5 = rowSums(cbind(Menores_0_3, Menor_4_años, Menor_5_años), na.rm = TRUE), 
    Menores_0_12 = rowSums(cbind(Menores_0_5, Menor_6_años, Menor_7_años, Menor_8_años, Menor_9_años, Menor_10_años, Menor_11_años, Menor_12_años), na.rm = TRUE),
    Menores_0_17 = rowSums(cbind(Menores_0_12, Menor_13_años, Menor_14_años, Menor_15_años, Menor_16_años, Menor_17_años), na.rm = TRUE),
    Count_9999s = rowSums(cbind(FS_D4_Q1 == "9999", FS_D4_Q2 == "9999", FS_D4_Q3 == "9999", FS_D4_Q4 == "9999", FS_D4_Q5 == "9999", 
                                FS_D4_Q6 == "9999", FS_D4_Q7 == "9999", FS_D4_Q8 == "9999", FS_D4_Q9 == "9999", FS_D4_Q10 == "9999")),
    # cuando no hay menores en casa se cambia a 0 el número de días que utilizaron la estrategia 2.4 Restringir el consumo de alimentos de los adultos para que coman los niños y niñas
    FS_D2_Q4 = case_when(
      Menores_0_17 == 0 ~ "0",
      TRUE ~ FS_D2_Q4),
    # cuando no hay menores en casa se cambia a 9999 la estrategia 4.4 Retirar a los niños y niñas de la escuela para que aporten a la economía del hogar
    FS_D4_Q4 = case_when(
      Menores_0_17 == 0 & !(`_uuid` %in% c("8c4f9077-94c0-4b0c-8110-3d343bbaa63e")) ~ "9999",
      TRUE ~ FS_D4_Q4),
    # cuando hay menores en casa y se había marcado "9999" se cambia a "10"
    FS_D4_Q4 = case_when(
      Menores_0_17 > 0 & FS_D4_Q4 =="9999" ~ "10",
      TRUE ~ FS_D4_Q4),
    # imputar 9999s por 10 en opción 6, 9 y 10 de la FS_D4
    FS_D4_Q6 = case_when(
      FS_D4_Q6 == "9999" ~ "10",
      TRUE ~ FS_D4_Q6),
    FS_D4_Q9 = case_when(
      FS_D4_Q9 == "9999" ~ "10",
      TRUE ~ FS_D4_Q9),
    FS_D4_Q10 = case_when(
      FS_D4_Q10 == "9999" ~ "10",
      TRUE ~ FS_D4_Q10),
    Count_9999s_V2 = rowSums(cbind(FS_D4_Q1 == "9999", FS_D4_Q2 == "9999", FS_D4_Q3 == "9999", FS_D4_Q4 == "9999", FS_D4_Q5 == "9999", 
                                   FS_D4_Q6 == "9999", FS_D4_Q7 == "9999", FS_D4_Q8 == "9999", FS_D4_Q9 == "9999", FS_D4_Q10 == "9999"))
  )

###############################################
##### Step 9. Final filtering and merging #####
###############################################

# Related with the data quality check on food security.

# Elimination of some duplicates and the records of an enumerator that incorrectly applied the Food Security questions. 

HH <- HH |>
  subset(!(`_uuid` %in% c("4e7064e4-f125-42e0-8ebd-fcf1958da913",
                          "ef8d43be-0f1e-49c9-98af-7dc853f97d75",
                          "50bc5c06-e132-4966-9ce9-d13f7d3f577c",
                          "3338f6f8-de9b-436e-aed2-36e5bde8de0e",
                          "03a17268-16ec-42d6-a5e7-b78b45bc6714"))) |>
  filter(!(Count_9999s_V2 == 7 & name_enumerator %in% c("Karla Gamez", "KARLA GAMEZ")))


HH_final_uuid <-HH$`_index`

Individual <- Individual |>
  filter(`_parent_index` %in% HH_final_uuid)

### Merge household data to individual data by parent index
main_merged <- left_join(Individual, HH, by = c("_parent_index"="_index")) |>
  rename(id_hogar = "_parent_index") |>
  rename(id_individual = "_index") 

write_xlsx(main_merged, "main_merged.xlsx")
