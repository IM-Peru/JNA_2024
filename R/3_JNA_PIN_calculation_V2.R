#####################################
##### Step 12. PINs Calculation #####
#####################################


pins_indicator <- main_merged |>
  #Select only the previously calculated indicators
  select(
    c(
      id_hogar, id_individual, SECTOR, DEMO_18, HH04, HH07, HH07_months, CARI_FES,CARI_NO_FES,
      #PSEA
      IND_PSEA_max,
      #Food Security
      IND_FS_max,
      #Health
      IND_HE_max,
      #Humanitarian Transport
      IND_HT_max,
      #Integration
      IND_INT_D1_max,
      IND_INT_D2_max,
      IND_INT_D3_max,
      #Nutrition
      IND_NUT_D1,
      IND_NUT_D2,
      IND_NUT_D3,
      #Protection
      IND_PRO_D1_max,
      IND_PRO_D2_max,
      IND_PRO_D3_max,
      IND_PRO_CP_max,
      IND_PRO_GBV_max,
      IND_PRO_HTS_max,
      #Shelter
      IND_SHE_D1_max,
      IND_SHE_D2_max,
      IND_SHE_D3_max,
      #Wash
      IND_WA_D1_max,
      IND_WA_D2_max,
      IND_WA_D3_max,
      #Education
      IND_EDU_D1_max,
      IND_EDU_D2_max
    )
  )

# read indicators sheet with weights

df_weights <- read_excel("df_weights.xlsx")

#Multiply each column of pins_indicator by the respective weight in df_weights

weighted_pins_indicator <- pins_indicator |>
  mutate(across(-c(1:10), ~ . * df_weights$ind_weight_per_sector[match(cur_column(), df_weights$ind_per_sector)]))

#Calculate sectorial deprivation score ds suming weighted indicators per sector

ds_per_sector <- weighted_pins_indicator |>
  mutate(
    Food_security_ds = IND_FS_max,
    Health_ds =  IND_HE_max,
    Humanitarian_transport_ds = IND_HT_max,
    Integration_ds = rowSums(weighted_pins_indicator[, c("IND_INT_D1_max", "IND_INT_D2_max",
                                                         "IND_INT_D3_max")], na.rm = TRUE),
    Nutrition_ds = rowSums(weighted_pins_indicator[, c("IND_NUT_D1","IND_NUT_D2", 
                                                       "IND_NUT_D3" )], na.rm = TRUE),
    Protection_ds = rowSums(weighted_pins_indicator[, c("IND_PRO_D1_max", "IND_PRO_D2_max",
                                                        "IND_PRO_D3_max", "IND_PRO_CP_max",
                                                        "IND_PRO_GBV_max",
                                                        "IND_PRO_HTS_max")], na.rm = TRUE),
    Child_protection_ds = IND_PRO_CP_max,
    Gender_based_violence_ds = IND_PRO_GBV_max,
    HT_S_ds = IND_PRO_HTS_max,
    Shelter_ds = rowSums(weighted_pins_indicator[, c("IND_SHE_D1_max", "IND_SHE_D2_max",
                                                     "IND_SHE_D3_max")], na.rm = TRUE),
    Wash_ds = rowSums(weighted_pins_indicator[, c("IND_WA_D1_max","IND_WA_D2_max",
                                                  "IND_WA_D3_max" )], na.rm = TRUE),
    Education_ds = rowSums(weighted_pins_indicator[, c("IND_EDU_D1_max", 
                                                       "IND_EDU_D2_max")], na.rm = TRUE)
  )

### Step 2: Calculate the intersectorial MPI 
# using the scores from each sector 
# and using the 33.3% cutoff to identify those to the right of the distribution.
# Exclude subsectors - CP, HT and GBV (only calculate intersectoral MPI with 9 dimensions)

df_pin <- ds_per_sector |>
  select(
    id_hogar, id_individual, SECTOR, DEMO_18, HH04, HH07, HH07_months, CARI_FES,CARI_NO_FES,
    Food_security_ds,
    Health_ds,
    Humanitarian_transport_ds,
    Integration_ds,
    Nutrition_ds,
    Protection_ds,
    Child_protection_ds,
    Gender_based_violence_ds,
    HT_S_ds,
    Shelter_ds,
    Wash_ds,
    Education_ds) |>
  mutate(intersector_ds = rowSums(across(c(
    "Food_security_ds",
    "Health_ds",
    "Humanitarian_transport_ds",
    "Integration_ds",
    "Nutrition_ds",
    "Protection_ds",
    "Shelter_ds",
    "Wash_ds",
    "Education_ds"
  )), na.rm = TRUE)) |>
  # Calculate if included in intersector_pin with the threshold of 33.3% of intersector ds
  mutate(intersector_pin = ifelse(intersector_ds > 0.333, 1, 0))

# ggplot of intersector_ds to see distribution

ggplot(df_pin, aes(x = intersector_ds)) +
  geom_density(fill = "blue", alpha = 0.9) +
  labs(title = "Density Plot of Intersector IPM Distribution",
       x = "Intersector ds",
       y = "Density") +
  theme_minimal()

pin_intersector <- sum(df_pin$"intersector_pin") / nrow(df_pin)


### Step 3: Adjustments for Nutrition, Education and Child Protection PINs
df_pin <- df_pin |>
  mutate(
    Under_18 = ifelse(HH07 < 18, 1, 0),
    Women_nut = ifelse(HH04 == "2" & (HH07 <= 50 & HH07 >= 14), 1, 0),
    Children_nut = ifelse(HH07 < 5, 1, 0),
    pobl_nut =ifelse(HH04 == "2" & (HH07 <= 50 & HH07 >= 14), 1, ifelse(HH07 < 5, 1, 0))
    ) |>
  select(id_hogar, id_individual, SECTOR, DEMO_18, HH04, HH07, HH07_months, CARI_FES, CARI_NO_FES, Under_18, Women_nut, Children_nut,pobl_nut,
         Food_security_ds, Health_ds, Humanitarian_transport_ds, Integration_ds, Nutrition_ds, Protection_ds,
         Child_protection_ds, Gender_based_violence_ds, HT_S_ds, Shelter_ds, Wash_ds, Education_ds, intersector_ds, intersector_pin)


### Step 4: Identify individuals with deprivations in each sector. 

#Now, having the figure of the Intersectoral MPI, we can identify who is part of the MPI of each sector. 
#This ensures that, although a person may have all the deprivations in a sector, they will
#only be part of the MPI if they have more than 33.3% of the total deprivations.

df_sectoral_pin <- df_pin |>
  select(-intersector_ds) |>
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Food_security_ds",
      "Health_ds",
      "Humanitarian_transport_ds",
      "Integration_ds",
      "Nutrition_ds",
      "Protection_ds",
      "Shelter_ds",
      "Wash_ds",
      "Education_ds"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  rename_with(~ str_replace(., "_ds$", "_pin"),
              c(
                "Food_security_ds",
                "Health_ds",
                "Humanitarian_transport_ds",
                "Integration_ds",
                "Nutrition_ds",
                "Protection_ds",
                "Shelter_ds",
                "Wash_ds",
                "Education_ds")) %>%
  mutate(across(
    c(
      "Child_protection_ds",
      "Gender_based_violence_ds",
      "HT_S_ds"
    ),
    ~ ifelse(Protection_pin == 1 & . > 0, 1, 0)
  ))|>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds$", "_pin")) 


#write_xlsx(df_sectoral_pin, "final_data_sin_ajustes.xlsx")
write_xlsx(df_sectoral_pin, "final_data_con_ajustes.xlsx")


#PIN table
df_sectoral_pin_filtered = df_sectoral_pin |>
  select(14,15,16,17,19,21,22,23,24,26)

pins_final <- data.frame(
  Sectors = c(
    "b_Food_security",
    "c_Health",
    "d_Humanitarian_transport",
    "e_Integration",
    "i_Protection",
    "h_Gender_based_violence",
    "j_Human_Traficking_&_Smuggling",
    "k_Shelter",
    "l_Wash",
    "m_Intersector"
  ),
  pins = round(colSums(df_sectoral_pin_filtered) / nrow(df_sectoral_pin_filtered) *
                 100, 1)
)

#Final calculation for Education sector
Education_pin_final <- data.frame(
  Sectors = "a_Education",
  pins = round(sum(df_sectoral_pin$Education_pin[df_sectoral_pin$Under_18 == 1])/sum(df_sectoral_pin$Under_18)*100,1))
#Final calculation for Nutrition sector
Nut_pin_final <- data.frame(
  Sectors = "f_Nutrition",
    pins = round(sum(df_sectoral_pin$Nutrition_pin[df_sectoral_pin$pobl_nut == 1])/sum(df_sectoral_pin$pobl_nut)*100,1))
#Final calculation for Child Protection sector
CP_pin_final <- data.frame(
  Sectors = "g_Child_protection",
  pins = round(sum(df_sectoral_pin$Child_protection_pin[df_sectoral_pin$Under_18 == 1])/sum(df_sectoral_pin$Under_18)*100,1))

#Final PIN table
pins_final <- pins_final|> bind_rows(Education_pin_final,Nut_pin_final,CP_pin_final)|>tibble() |> arrange(Sectors)

#write_xlsx(pins_final, "pins_final_sin_ajustes.xlsx")
write_xlsx(pins_final, "pins_final_con_ajustes.xlsx")


