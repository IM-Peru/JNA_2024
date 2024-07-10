
###############################################
##### Step 10. Household level indicators ##### 
###############################################

# Indicator creation for the dimensions at the household level. 
# Be careful since I did some adhoc adjustmets. For example creating the round_half_up function in step 3 and the variable SHE_D1_Q3_ALL in step 4. 
# Indicator INT_D3 is built following the specific structure used under our XLS form. The structure of how the humanitarian transportation questions where asked is also specific to Peru.

## Integration
# INT_D3: Porcentaje de personas que se han sentido discriminadas por su nacionalidad
# INT_D4: Porcentaje de personas encuestadas que no tienen acceso a servicios financieros

main_merged <- main_merged |>
  mutate(INT_D3 = if_else(!is.na(INT_D3_Q1B_7) & INT_D3_Q1B_7 == "1", 1, 0),
         INT_D4 = if_else(INT_D4_Q1_10 == "1", 1, 0)) |>
  mutate(IND_INT_D2_max = INT_D3,
         IND_INT_D3_max = INT_D4)

## Shelter
# SHE_D1: Porcentaje de hogares que viven en viviendas con condiciones inadecuadas e insostenibles en el largo plazo (excluye hacinamiento)
# SHE_D2: Hacinamiento
# SHE_D3: Porcentaje de hogares sin acceso a articulos domésticos esenciales
# SHE_D4: Porcentaje de hogares que estan en riesgo de desalojo

main_merged <- main_merged |>
  mutate(SHE_D1 = if_else((SHE_D1_Q1 %in% c("3","7","8","9","10","11", "98") |
                             SHE_D1_Q2_6 != "1" |
                             SHE_D1_Q3_ALL == "0"), 1, 0),
         SHE_D2 = ifelse(MH_1/SHE_D2_Q1 > 3, 1, 0),
         SHE_D3 = ifelse(SHE_D3_Q1_1 == "0" | 
                           SHE_D3_Q1_2 == "0" |
                           SHE_D3_Q1_3 == "0" |
                           SHE_D3_Q1_4 == "1", 1, 0),
         SHE_D4 = ifelse(SHE_D4_Q1 %in% c("2", "3", "99"), 1, 0)) |>
  mutate(IND_SHE_D1_max = case_when(SHE_D1 == 1 | SHE_D2 == 1 ~ 1,
                                    TRUE ~ 0),
         IND_SHE_D2_max = SHE_D3,
         IND_SHE_D3_max = SHE_D4)

## WASH
# WA_D1: W1 Porcentaje de hogares de Refugiados y migrantes venezolanos o individuos que no acceden a una fuente primaria de agua mejorada para beber
# WA_D2: W2 Porcentaje de hogares de Refugiados y migrantes venezolanos o individuos que no acceden de forma continua a suficiente cantidad de agua 
# WA_D4: S1 Porcentaje de hogares de Refugiados y migrantes venezolanos o individuos sin acceso a instalaciones de saneamiento mejoradas y en funcionamiento
# WA_D6: S2 Porcentaje de hogares de Refugiados y migrantes que carecen de buenas practicas de gestion de residuos y salud ambiental en sus alrededores
# WA_D8: HI 1 Porcentaje de hogares de Refugiados y migrantes venezolanos o individuos sin acceso basico a instalaciones de lavado de manos 
# WA_D11: HI2 Porcentaje de hogares de refugiados y migrantes en donde las mujeres y niñas están sin acceso a artículos de higiene menstrual apropiados

main_merged <- main_merged |>
  mutate(WA_D1 = ifelse(WA_D1_Q1 %in% c("3", "4", "5", "6") & 
                          WA_D1_Q2 == "2", 1, 0),
         WA_D2 = ifelse(WA_D2_Q1 != "1" | 
                          (WA_D2_Q2 %in% c("2", "3", "4", "5", "98", "99")), 1, 0),
         WA_D4 = ifelse(WA_D4_Q1	%in% c("5", "6") |
                          WA_D4_Q2 == "1", 1, 0),
         WA_D6 = ifelse(WA_D6_Q1 %in% c("3","4","98","99"), 1, 0),
         WA_D8 = ifelse(WA_D8_Q1 != "1", 1, 0),
         WA_D11 = ifelse(WA_D11_Q1 %in% c("1", "3", "6", "7", "9"), 1, 0)) |>
  mutate(
    IND_WA_D1_max = ifelse(WA_D1 == 1 | WA_D2 == 1, 1, 0),
    IND_WA_D2_max = ifelse(WA_D4 == 1 | WA_D6 == 1, 1, 0),
    IND_WA_D3_max = ifelse(WA_D8 == 1 | WA_D11 == 1, 1,0))

# Humanitarian Transportation
# HT_D1: Porcentaje de personas encuestadas o jefes de hogar que tardan más de 30 minutos a su lugar de destino caminando o en bicicleta
main_merged <- main_merged |>
  mutate(HT_D1 = ifelse((HT_D1_Q1_first %in% c(1, 6) | 
                           HT_D1_Q1_second %in% c(1, 6)) & 
                          HT_D1_Q2 == "4", 1, 0)) |>
  mutate(IND_HT_max = HT_D1)

# Food Security
# FS_D1: porcentaje de personas en situación de inseguridad alimentaria (Componente 1. Puntaje del consumo de alimentos: Food Consumption Score FCS) - Versión resumida CORE
# FS_D2: porcentaje de personas en situación de inseguridad alimentaria (Componente 2. Estrategias de afrontamiento basadas en el consumo. Reduced coping strategies index (rCSI))
# FS_D3: porcentaje de personas en situación de inseguridad alimentaria (Componente 3.  Proporcion de gastos en alimentos: FES; Food Expenditure Share) - Versión resumida CORE
# FS_D4: porcentaje de personas en situación de inseguridad alimentaria (Componente 4. Estrategias de afrontamiento basadas en medios de vida. Livelihoods coping strategies Index (LCSI))

main_merged <- main_merged |>
  mutate_at(vars(starts_with("FS_D1_Q")), ~as.numeric(as.character(.)))|>   #linea extra para Peru
  mutate(FCS = (FS_D1_Q1 * 2 + FS_D1_Q5 * 1 + 
                  FS_D1_Q6 * 1 + FS_D1_Q4 * 4 +
                  FS_D1_Q3 * 4 + FS_D1_Q2 * 3 +
                  FS_D1_Q7 * .5 + FS_D1_Q8 * .5)) |>
  mutate(FCG = case_when(
    FCS <= 28.0 ~ 1,
    FCS >= 28.5 & FCS <= 42.0 ~ 2,
    FCS >= 42.5 ~ 3)) |>
  mutate(FCS_4pt = case_when(
    FCG == 1 ~ 4,
    FCG == 2 ~ 3,
    FCG == 3 ~ 1)) |>
  mutate(FS_D1 = FCS_4pt) |>
  mutate_at(vars(starts_with("FS_D2_Q")), ~as.numeric(as.character(.)))|>   #linea extra para Peru
  mutate(R_CSI = (FS_D2_Q1 * 1 + FS_D2_Q2 * 2 + 
                    FS_D2_Q3 * 1 + FS_D2_Q4 * 3 +
                    FS_D2_Q5 * 1)) |>
  mutate(r_CSI_categories = case_when(
    R_CSI <= 4 ~ 1,
    R_CSI > 4 & R_CSI <= 18 ~ 2,
    R_CSI >=19 ~ 3)) |>
  mutate(FCS_rCSI = ifelse(FCS_4pt %in% c(1,3,4), FCS_4pt, NA),
         FCS_rCSI = ifelse(FCS_4pt == 1 & R_CSI > 4, 2, FCS_rCSI)) |>
  mutate(FS_D2 = FCS_rCSI) |>
  mutate(FES = FS_D3_Q1 / (FS_D3_Q1 + FS_D3_Q2),
         Foodexp_4pt = case_when(
           FES <= .4999999 ~ 1,
           FES >= .5 & FES <= .64999999 ~ 2,
           FES >= .65 & FES <= .74999999 ~ 3,
           FES >= .75 ~ 4)) |>
  mutate(FS_D3 = Foodexp_4pt) |>
  mutate(stress_coping = ifelse(FS_D4_Q1 == "20" |
                                  FS_D4_Q1 == "30" |
                                  FS_D4_Q2 == "20" |
                                  FS_D4_Q2 == "30" |
                                  FS_D4_Q3 == "20" |
                                  FS_D4_Q3 == "30" |
                                  FS_D4_Q8 == "20" |
                                  FS_D4_Q8 == "30", 1, 0)) |>
  mutate(crisis_coping = ifelse(FS_D4_Q4 == "20" |
                                  FS_D4_Q4 == "30" |
                                  FS_D4_Q5 == "20" |
                                  FS_D4_Q5 == "30" |
                                  FS_D4_Q6 == "20" |
                                  FS_D4_Q6 == "30", 1, 0)) |>
  mutate(emergency_coping = ifelse(FS_D4_Q7 == "20" |
                                     FS_D4_Q7 == "30" |
                                     FS_D4_Q9 == "20" |
                                     FS_D4_Q9 == "30" |
                                     FS_D4_Q10 == "20" |
                                     FS_D4_Q10 == "30", 1, 0)) |>
  mutate(stress_coping = recode(stress_coping, "0" = 0, "1" = 2),
         crisis_coping = recode(crisis_coping, "0" = 0, "1" = 3),
         emergency_coping = recode(emergency_coping, "0" = 0, "1" = 4),
         Max_coping_behaviour = pmax(stress_coping, crisis_coping, emergency_coping),
         Max_coping_behaviour = recode(Max_coping_behaviour, "0" = 1)) |>
  mutate(FS_D4 = Max_coping_behaviour) |>
  mutate(Mean_coping_capacity_FES = rowMeans(across(c(Max_coping_behaviour, Foodexp_4pt)), na.rm = TRUE),
         CARI_unrounded_FES = rowMeans(across(c(FCS_rCSI, Mean_coping_capacity_FES)), na.rm = TRUE),
         CARI_FES = round_half_up(CARI_unrounded_FES))|> # do not use simple function "round"
  mutate(IND_FS_max = ifelse(CARI_FES > 2, 1, 0))


## Protection
# PRO_D1: Protección de hogares que reportan preocupaciones de seguridad, protección y violaciones a sus derechos en el marco del DIDH, DIH y DIR. 
# PRO_D2: Porcentaje de hogares que enfrentan dificultades para el acceso seguro al país de destino
# PRO_D3: Porcentaje de hogares que necesitan asistencia u orientación legal. 
# PRO_D5: Porcentaje de hogares que tienen necesidad de protección internacional.

main_merged <- main_merged |>
  mutate(PRO_D1 = ifelse(PRO_D1_Q1 != "9", 1,0),
         PRO_D2 = ifelse(PRO_D2_Q1 != "0", 1,0),
         PRO_D3 = ifelse(PRO_D3_Q1 == "1" & 
                           PRO_D3_Q2 == "0",1,0),
         PRO_D5 = ifelse(PRO_D5_Q1 %in% c("1", "98", "99"), 1, 0)) |>
  mutate(IND_PRO_D1_max = case_when(PRO_D1 == 1 | PRO_D2 == 1 ~ 1,
                                    TRUE ~ 0),
         IND_PRO_D2_max = case_when(PRO_D3 == 1 | PRO_D5 == 1 ~ 1,
                                    TRUE ~ 0))

## Gender Based Violence (GBV)
# GBV_D1: Porcentaje de hogares con mujeres y niñas que evitan lugares porque se sienten inseguras
# GBV_D3: Porcentaje de personas refugiadas y migrantes que se sienten o han sentido inseguras/os en su localidad/ comunidad frente al riesgo de VBG

main_merged <- main_merged |>
  mutate(GBV_D1 = ifelse(!is.na(GBV_D1_Q1) & GBV_D1_Q1 == "1", 1, 0),
         GBV_D3 = ifelse(GBV_D3_Q1 %in% c("4", "5"), 1, 0) ) |>
  mutate(IND_PRO_GBV_max = case_when(GBV_D1 == 1 | GBV_D3 == 1 ~ 1,
                                     TRUE ~ 0))

# Human Trafficking & Smuggling (HTS)
# HTS_D1: Porcentaje de hogares que han estado expuestos a situaciones de medios de trata de personas
# HTS_D2: Porcentaje de hogares que han sido expuestos a situaciones de explotación

main_merged <- main_merged |>
  mutate(HTS_D1 = ifelse(HTS_D1_Q1 == "1" | 
                           HTS_D1_Q2 == "1", 1, 0),
         HTS_D2 = ifelse(HTS_D2_Q1 != "6", 1, 0)) |>
  mutate(IND_PRO_HTS_max = case_when(HTS_D1 == 1 | HTS_D2 == 1 ~ 1,
                                     TRUE ~ 0))

## Child Protection (CP)
# CP_D1: Porcentaje de hogares que manifiestan haber conocido algún NNA que ha experimentado violencia, abuso, negligencia, y explotación y no han recibido asistencia
main_merged <- main_merged |>
  mutate(CP_D1 = case_when(
    CP_D1_Q1 == "7" ~ 0,
    CP_D1_Q1 == "98" ~ 0,
    CP_D1_Q1 == "99" ~ 0,
    CP_D1_Q2 != "0" ~ 0,
    CP_D1_Q2 == "0" ~ 1,
    TRUE ~ NA)
  ) |>
  mutate(IND_PRO_CP_max = CP_D1)


################################################
##### Step 11. Individual Level Indicators ##### 
################################################

#Indicator creation for the dimensions at the individual or member of household level. 
#Be careful since I did some adhoc adjustments. For example creating the variable NUT_D10_Q1_ALL in step 5.

## Integration
# INT_D1: Porcentaje de personas en desempleo
# INT_D2: Porcentaje de personas con trabajos informales
main_merged <- main_merged |>
  group_by(id_hogar) |>
  mutate(
    INT_D1 = ifelse(any(INT_D1_Q1 == "2", na.rm = TRUE), 1, 0),
    INT_D2 = case_when(any(INT_D2_Q1 %in% c("0", "98", "99") | 
                             INT_D2_Q2 %in% c("0", "98", "99") | 
                             INT_D2_Q3 %in% c("0", "98", "99")) ~ 1,
                       TRUE ~ 0)) |>
  ungroup() |>
  mutate(IND_INT_D1_max = ifelse(INT_D1 == 1 | INT_D2 == 1, 1, 0))

## Education
# EDU_D1: porcentaje de NNA refugiados y migrantes que no están inscritos en el sistema escolar formal 
# EDU_D2: porcentaje de NNA refugiados y migrantes entre los 0 y 3 años que no cuentan con un servicio de atención de desarrollo infantil temprano y/o cuidado adecuado
# EDU_D3: porcentaje de NNA refugiados y migrantes que no asisten al centro educativo (CE) o centro de atencion de primera (CAPI) infacia de manera regular
main_merged <- main_merged |>
  group_by(id_hogar) |>  
  mutate(
    EDU_D1 = ifelse(any(EDU_D1_Q1 == "0", na.rm = TRUE), 1, 0), 
    EDU_D2 = case_when(any(EDU_D2_Q1 == "0" & EDU_D2_Q2 != "1", na.rm = TRUE) ~ 1, 
                       TRUE ~ 0),
    EDU_D3 = ifelse(any(EDU_D1_Q1 == "1" & EDU_D3_Q1 != "5", na.rm = TRUE), 1, 0)) |> # vacío es = a PIN?
  mutate(IND_EDU_D2_max = EDU_D2,
         IND_EDU_D1_max = case_when(EDU_D1 == 1 | EDU_D3 == 1 ~ 1,
                                    TRUE ~ 0)) |>
  ungroup() 

## Health
# HE_D1: Número de personas refugiadas o migrantes que han requerido algún tipo de atención en salud en el país de destino, pero no han podido acceder al mismo
main_merged <- main_merged |>
  group_by(id_hogar) |> 
  mutate(
    HE_D1 = case_when(any(HE_D1_Q1 == "1" & 
                            HE_D1_Q2 == "0", na.rm = TRUE) ~ 1,
                      TRUE ~ 0),
    HE_D4 = case_when(any(INT_D2_Q2 == "0", na.rm = TRUE) ~ 1,
                      TRUE ~ 0)) |>
  mutate(
    IND_HE_max = case_when(HE_D1 == 1 | HE_D4 == 1 ~ 1,
                           TRUE ~ 0)) |>
  ungroup()

## Protection
# PRO_D4: porcentaje de personas que están en situación de estatus irregular en su pais de destino.
main_merged <- main_merged |>
  group_by(id_hogar) |> 
  mutate(PRO_D4 = ifelse(is.na(PRO_D4_Q1), 0, 
                         ifelse(PRO_D4_Q1_3 == "0" & PRO_D4_Q1_4 == "0" & PRO_D4_Q1_7 == "0", 1, 0))) |>
  mutate(IND_PRO_D3_max = PRO_D4) |>
  ungroup()

## Nutrition
# NUT_D1: # de mujeres gestantes y lactantes identificadas
# NUT_D1: % de mujeres embarazadas y lactantes que no han recibido el paquete mínimo de intervenciones nutricionales en los últimos 3 meses
# NUT_D4: % de niños y niñas menores de 6 meses que no recibieron el paquete mínimo de intervenciones de nutrición en los últimos 3 meses
# NUT_D5: % de bebés menores de 6 meses que no fueron alimentados con leche materna exclusivamente
# NUT_D8: % de niños/niñas de 6 a 59 meses que no han recibido el paquete mínimo de intervenciones nutricionales en los últimos 3 meses.
# NUT_D10: % de  niños y niñas de 6 a 59 meses con diversidad alimentaria mínima

main_merged <- main_merged |>
  group_by(id_hogar) |> 
  mutate(
    NUT_D1 = case_when(
      (NUT_D1_Q1_1 == "1" | NUT_D1_Q1_2 == "1") &
        (NUT_D1_Q2_1 == "0" | NUT_D1_Q2_2 == "0" | NUT_D1_Q2_3 == "0") ~ 1,
      TRUE ~ 0),
    NUT_D4 = case_when(
      NUT_D4_Q1_1 == "0" | (NUT_D4_Q1_2 == "0" & NUT_D4_Q1_3 == "0") ~ 1, 
      TRUE ~ 0),
    NUT_D5 = case_when(
      NUT_D5_Q1 == "0" | NUT_D5_Q2_10 == "0" ~ 1,
      TRUE ~ 0),
    NUT_D8 = case_when(
      (HH07 >= 2 & HH07 < 5) ~ case_when(
        (NUT_D8_Q1_1 == "0" | 
           NUT_D8_Q1_4 == "0" | 
           NUT_D8_Q1_5 =="0") ~ 1,
        TRUE ~ 0),
      HH07 == 1 ~ case_when(
        (NUT_D8_Q1_1 == "0" | 
           (NUT_D8_Q1_2 == "0" & NUT_D8_Q1_3 == "0") |
           NUT_D8_Q1_4 == "0" |
           NUT_D8_Q1_5 == "0") ~ 1,
        TRUE ~ 0),
      (HH07 == 0 & HH07_months %in% c("6", "7", "8", "9", "10", "11")) ~ case_when(
        (NUT_D8_Q1_1 == "0" | 
           (NUT_D8_Q1_2 == "0" & NUT_D8_Q1_3 == "0") |
           NUT_D8_Q1_4 == "0" |
           NUT_D8_Q1_5 == "0") ~ 1,
        TRUE ~ 0),
      HH07 >=5 ~ 0,
      TRUE ~ 0),
    NUT_D10 = case_when(
      HH07 >= 1 & HH07 < 5 ~ case_when(
        NUT_D10_Q1_ALL < 5 ~ 1,
        TRUE ~ 0),
      HH07 == "0" & HH07_months %in% c("6", "7", "8", "9", "10", "11") ~ case_when(
        NUT_D10_Q1_ALL < 5 ~ 1,
        TRUE ~ 0),
      HH07 >=5 ~ 0,
      TRUE ~ 0)
  ) |>
  mutate(
    IND_NUT_D1 = NUT_D1,
    IND_NUT_D2 = ifelse(NUT_D4 == 1 | NUT_D8 == 1, 1, 0),
    IND_NUT_D3 = ifelse(NUT_D10 == 1 | NUT_D5 == 1, 1, 0)) |>
  ungroup() 
