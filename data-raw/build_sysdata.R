# data-raw/build_sysdata.R
#
# Script para construir o dicionario interno dicionario_mestre (R/sysdata.rda)
# a partir de inst/extdata/dicionario_uls.csv.
#
# Deve ser executado sempre que o ficheiro CSV for alterado (p. ex., apos uma
# mudanca de fronteiras de ULS).
#
# Executar a partir da raiz do projecto:
#   source("data-raw/build_sysdata.R")
# ou via devtools:
#   devtools::source_file("data-raw/build_sysdata.R")

library(dplyr)
library(stringr)
library(readr)

# -------------------------------------------------------------------------
# 1. Ler o CSV fonte
# -------------------------------------------------------------------------
csv_path <- "inst/extdata/dicionario_uls.csv"

dicionario_raw <- readr::read_csv(
  csv_path,
  col_types = readr::cols(
    Freguesia = readr::col_character(),
    Concelho  = readr::col_character(),
    NOME_ULS  = readr::col_character()
  ),
  na = "NA"
)

message("CSV lido: ", nrow(dicionario_raw), " linhas.")

# -------------------------------------------------------------------------
# 2. Carregar a tabela de DICO por Concelho (da fonte DGT/CAOP)
#    O ficheiro shapefile_municipios contem a coluna CCA_2 (codigo de concelho
#    a 4 digitos = DICO) que usamos para fazer o join.
# -------------------------------------------------------------------------
shp_path <- "inst/extdata/shapefile_municipios"

concelhos_dico <- sf::st_read(shp_path, quiet = TRUE) |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    Concelho = stringr::str_trim(Dicofre),   # ajustar ao nome real da coluna se necessario
    DICO     = substr(as.character(CCA_2), 1, 4)
  ) |>
  dplyr::distinct()

# NOTA: Se as colunas do shapefile tiverem nomes diferentes, ajuste acima.
# Pode inspeccionar com: colnames(sf::st_read(shp_path, quiet = TRUE))

# -------------------------------------------------------------------------
# 3. Cruzar CSV com DICO
# -------------------------------------------------------------------------
dicionario_mestre <- dicionario_raw |>
  dplyr::left_join(concelhos_dico, by = "Concelho") |>
  dplyr::select(DICO, Freguesia, NOME_ULS)

# Validar que nao ficaram DICO a NA
n_sem_dico <- sum(is.na(dicionario_mestre$DICO))
if (n_sem_dico > 0) {
  warning(
    n_sem_dico, " linha(s) sem DICO apos o join. ",
    "Verifique os nomes de Concelho no CSV vs. shapefile."
  )
  print(dicionario_mestre[is.na(dicionario_mestre$DICO), ])
}

# Validar cobertura de ULS
n_uls <- dplyr::n_distinct(dicionario_mestre$NOME_ULS)
message("ULS distintas no dicionario: ", n_uls, " (esperado: 39)")

# -------------------------------------------------------------------------
# 4. Guardar como dados internos do pacote
# -------------------------------------------------------------------------
usethis::use_data(dicionario_mestre, internal = TRUE, overwrite = TRUE)

message("sysdata.rda actualizado com sucesso.")
