#' Mapa das Unidades Locais de Saúde (ULS)
#'
#' @export
ulsportugal <- function() {

  raw_mapa <- giscoR::gisco_get_lau(country = "Portugal", year = "2021")
  col_id <- intersect(c("LAU_ID", "GISCO_ID", "id", "LAU_CODE"), colnames(raw_mapa))[1]
  if (is.na(col_id)) stop("Não encontrei coluna de ID (LAU_ID/GISCO_ID/id/LAU_CODE).")

  mapa_base <- raw_mapa %>%
    dplyr::rename(ID_TEMP = dplyr::all_of(col_id)) %>%
    dplyr::mutate(
      ID_TEMP   = as.character(ID_TEMP),
      ID_DIGITS = stringr::str_extract(ID_TEMP, "\\d+"),
      DICO      = substr(ID_DIGITS, 1, 4),
      DIST      = as.numeric(substr(DICO, 1, 2)),
      LAU_NAME  = stringr::str_trim(stringr::str_replace_all(LAU_NAME, "freguesias", "Freguesias"))
    ) %>%
    dplyr::filter(!is.na(DIST) & DIST < 20)

  mapa_final <- mapa_base %>%
    dplyr::left_join(dicionario_mestre %>% dplyr::filter(!is.na(Freguesia)),
                     by = c("DICO" = "DICO", "LAU_NAME" = "Freguesia")) %>%
    dplyr::left_join(dicionario_mestre %>% dplyr::filter(is.na(Freguesia)),
                     by = "DICO", suffix = c("_esp", "_gen")) %>%
    dplyr::mutate(NOME_ULS = dplyr::coalesce(NOME_ULS_esp, NOME_ULS_gen)) %>%
    dplyr::filter(!is.na(NOME_ULS)) %>%
    dplyr::group_by(NOME_ULS) %>%
    dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop") %>%
    dplyr::mutate(
      NOME_CURTO = NOME_ULS %>%
        stringr::str_remove("^Unidade Local de Saúde d[eoa]'?s? ") %>%
        stringr::str_remove("^Unidade Local de Saúde ") %>%
        stringr::str_remove(", EPE$")
    )

  mapa_final
}
