viajes2019<-read.csv("ViajesEODH2019.csv")
etapas2019<-read.csv("EtapasEODH2019.csv")

et19_step1 <- etapas2019 %>%
  filter(!is.na(p18_id_medio_transporte),
         p18_id_medio_transporte != 99) %>%
  mutate(
    TripID = paste(id_hogar, id_persona, id_viaje, sep = "-"),
    modo_nombre = dplyr::recode(as.character(p18_id_medio_transporte), !!!mode_dict_2019)
  ) %>%
  arrange(TripID, id_etapa)

# Mapping name
mode_dict_2019 <- c(
  `1`  = "TransMilenio",
  `2`  = "Bus alimentador",
  `3`  = "Bus dual",
  `4`  = "SITP - Urbano (Azul)",
  `5`  = "SITP - Complementario (Naranja)",
  `6`  = "SITP - Especial (Vinotinto)",
  `7`  = "SITP - Provisional",
  `8`  = "Bus intermunicipal",
  `9`  = "Bus escalera/Chiva",
  `10` = "Campero/jeep",
  `11` = "Transporte escolar",
  `12` = "Bus privado/de empresa",
  `13` = "Bicicleta pública",
  `14` = "Vehículo solicitado por app (camioneta/automóvil)",
  `15` = "Automóvil informal/pirata",
  `16` = "Taxi",
  `17` = "Taxi solicitado por app",
  `18` = "Taxi colectivo",
  `19` = "Bus/buseta informal/pirata/Chana",
  `20` = "Mototaxi (2 ruedas)",
  `21` = "Motocarro de pasajeros/carga",
  `22` = "Vehículo de tracción animal",
  `23` = "Bicitaxi",
  `24` = "Cable",
  `25` = "Tren",
  `30` = "Bicicleta",
  `31` = "Bicicleta con motor",
  `32` = "Motocicleta como conductor",
  `33` = "Motocicleta como pasajero",
  `34` = "Vehículo privado como conductor",
  `35` = "Vehículo privado como pasajero",
  `36` = "Bicicleta como pasajero",
  `37` = "Patineta",
  `38` = "Vehículo de tracción humana",
  `39` = "Camión/Volqueta/Tractomula",
  `40` = "A pie"
)

et19_step1 <- et19_step1 %>%
  mutate(
    modo_nombre = dplyr::recode(as.character(p18_id_medio_transporte), !!!mode_dict_2019)
  )

# Step 2 combo
build_combo <- function(modes) {
  if (length(modes) == 0) return(NA_character_)
  keep <- c(TRUE, modes[-1] != head(modes, -1))
  v <- modes[keep]
  paste(v, collapse = "+")
}

et19_step2 <- et19_step1 %>%
  group_by(TripID) %>%
  summarise(
    combo    = build_combo(modo_nombre),
    n_stages = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(combo), combo != "")

# Step 3 Percentage
et19_step3 <- et19_step2 %>%
  count(combo, name = "n") %>%
  mutate(
    share = n / sum(n),
    share = round(share, 4)
  ) %>%
  arrange(desc(share))

#Step 4 Aggregate Mapping

collapse_adjacent <- function(v) {
  if (length(v) == 0) return(character())
  keep <- c(TRUE, v[-1] != head(v, -1))
  v[keep]
}


recode_bucket_2019 <- function(x) {
  x1 <- stringr::str_squish(x)

  has <- function(pat) stringr::str_detect(
    stringi::stri_trans_general(x1, "Latin-ASCII"),  # "móvil" -> "movil"
    stringr::regex(pat, ignore_case = TRUE)
  )

  dplyr::case_when(
    # Walk
    has("^a pie$") ~ "Walk",

    # Bike
    has("^bicicleta$") | has("^bicicleta como pasajero$") |
      has("^bicicleta con motor$") | has("^bicicleta publica$") ~ "Bike",

    # Car
    has("^vehiculo privado como conductor$") |
      has("^vehiculo privado como pasajero$") |
      has("^automovil informal/pirata$") |
      has("^campero/jeep$") |
      has("^camion/volqueta/tractomula$") ~ "Car",

    # Taxi
    has("^bicitaxi$") | has("^mototaxi \\(2 ruedas\\)$") |
      has("^taxi colectivo$") |
      has("app") & has("auto|camioneta") |   # 兼容 “Tr. individual en auto/camioneta por app móvil”
      has("^taxi$") | has("^taxi solicitado por app$") ~ "Taxi",

    # Motorcycle
    has("^motocicleta como conductor$") |
      has("^motocicleta como pasajero$") |
      has("^motocarro de pasajeros/carga$") ~ "Motorcycle",

    # BRT
    has("^transmilenio$") | has("^cable$") ~ "BRT",

    # Other transit mode (a.SITP)
    has("^sitp - provisional$") |
      has("^sitp - complementario \\(naranja\\)$") |
      has("^sitp - especial \\(vinotinto\\)$") |
      has("^sitp - urbano \\(azul\\)$") |
      has("^sitp zonal$") |
      has("^bus alimentador$") ~ "Other transit mode (a.SITP)",

    # Other transit mode (b.Not SITP) —— 包含 Bus dual（你要求放这里）
    has("^bus privado/de empresa$") |
      has("^bus/buseta informal/pirata/chana$") |
      has("^tren$") |
      has("^transporte escolar$") |
      has("^bus escalera/chiva$") |
      has("^bus intermunicipal$") |
      has("^bus dual$") ~ "Other transit mode (b.Not SITP)",

    # Other
    has("^otro$") |
      has("^vehiculo de traccion animal$") |
      has("^vehiculo de traccion humana$") |
      has("^patineta$") ~ "Other",

    TRUE ~ x1
  )
}

et19_step4 <- et19_step3 %>%
  transmute(
    combo, n, share,
    combo_bucket = sapply(strsplit(combo, "\\+"), function(parts) {
      buckets <- recode_bucket_2019(parts)
      buckets <- collapse_adjacent(buckets)
      paste(buckets, collapse = "+")
    })
  )

et19_step5 <- et19_step4 %>%
  group_by(combo_bucket) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(share = round(n / sum(n), 4)) %>%
  arrange(desc(share))

et_multi <- et19_step5 %>%
  filter(str_detect(combo_bucket, "\\+"))

# separate combo_bucket into first mode and second mode
et_multi <- et_multi %>%
  tidyr::separate(
    combo_bucket,
    into = c("first_mode", "second_mode", "third_mode", "fourth_mode", "fifth_mode", "sixth_mode"),
    sep = "\\s*\\+\\s*",    # handles "Metro + Bus" etc.
    fill = "right",
    extra = "merge"         # if there are 4+ parts, merge the rest into third_mode
  )

# turn all taxi, Car, Motorcycle into "Other"

et_multi <- et_multi %>%
  mutate(
    first_mode  = ifelse(first_mode  %in% c("Taxi", "Car", "Bike"), "Other", first_mode),
    second_mode = ifelse(second_mode %in% c("Taxi", "Car", "Bike"), "Other", second_mode),
    third_mode  = ifelse(third_mode  %in% c("Taxi", "Car", "Bike"), "Other", third_mode),
    fourth_mode = ifelse(fourth_mode %in% c("Taxi", "Car", "Bike"), "Other", fourth_mode),
    fifth_mode  = ifelse(fifth_mode  %in% c("Taxi", "Car", "Bike"), "Other", fifth_mode),
    sixth_mode  = ifelse(sixth_mode  %in% c("Taxi", "Car", "Bike"), "Other", sixth_mode)
  )

# turn all the walk into NA
et_multi <- et_multi %>%
  mutate(
    first_mode  = ifelse(first_mode  == "Walk", NA, first_mode),
    second_mode = ifelse(second_mode == "Walk", NA, second_mode),
    third_mode  = ifelse(third_mode  == "Walk", NA, third_mode),
    fourth_mode = ifelse(fourth_mode == "Walk", NA, fourth_mode),
    fifth_mode  = ifelse(fifth_mode  == "Walk", NA, fifth_mode)
  )

#motorcycle into other
et_multi <- et_multi %>%
  mutate(
    first_mode  = ifelse(first_mode  == "Motorcycle", "Other", first_mode),
    second_mode = ifelse(second_mode == "Motorcycle", "Other", second_mode),
    third_mode  = ifelse(third_mode  == "Motorcycle", "Other", third_mode),
    fourth_mode = ifelse(fourth_mode == "Motorcycle", "Other", fourth_mode),
    fifth_mode  = ifelse(fifth_mode  == "Motorcycle", "Other", fifth_mode)
  )

# avoid repeat
et_multi<- et_multi %>%
  rowwise() %>%
  mutate(
    first_mode  = ifelse(is.na(first_mode), second_mode, first_mode),
    second_mode = ifelse(second_mode == first_mode, NA, second_mode),
    third_mode  = ifelse(third_mode  %in% c(first_mode, second_mode), NA, third_mode),
    fourth_mode = ifelse(fourth_mode %in% c(first_mode, second_mode, third_mode), NA, fourth_mode),
    fifth_mode  = ifelse(fifth_mode  %in% c(first_mode, second_mode, third_mode, fourth_mode), NA, fifth_mode),
    sixth_mode  = ifelse(sixth_mode  %in% c(first_mode, second_mode, third_mode, fourth_mode, fifth_mode), NA, sixth_mode)
  ) %>%
  ungroup()

# clean up
et_multi <- et_multi %>%
  mutate(
    second_mode = ifelse(is.na(second_mode), third_mode, second_mode),
    third_mode  = ifelse(is.na(third_mode), fourth_mode, third_mode),
    fourth_mode = ifelse(is.na(fourth_mode), fifth_mode, fourth_mode),
    fifth_mode  = ifelse(is.na(fifth_mode), NA, fifth_mode)
  )
write.csv(et_multi, "et_multi_2019.csv", row.names = FALSE)
write.csv(et19_step5, "et19_step5.csv", row.names = FALSE)
write.csv(etapas2019, "etapas2019.csv", row.names = FALSE)
write.csv(viajes2019, "viajes2019.csv", row.names = FALSE)
