#' Mapa das Unidades Locais de Saúde de Portugal Continental
#'
#' @description
#' Devolve um objeto \code{sf} com as geometrias agregadas das 39 Unidades
#' Locais de Saúde (ULS) de Portugal Continental, obtidas a partir das
#' geometrias LAU do Eurostat (via \pkg{giscoR}) e do dicionário territorial
#' interno (DICO/Freguesia → NOME_ULS).
#'
#' A agregação é feita em dois passos: primeiro por correspondência exacta
#' DICO + nome de freguesia; depois, para os concelhos com uma única ULS,
#' por DICO apenas.
#'
#' @param ano Ano das geometrias LAU a utilizar (passado a
#'   \code{\link[giscoR]{gisco_get_lau}}). Por omissão \code{"2021"}.
#' @param verbose Lógico. Se \code{TRUE}, emite mensagens de progresso.
#'   Por omissão \code{FALSE}.
#'
#' @return Um objeto \code{sf} com uma linha por ULS e as seguintes colunas:
#' \describe{
#'   \item{NOME_ULS}{Nome completo e oficial da Unidade Local de Saúde.}
#'   \item{NOME_CURTO}{Nome simplificado (ex.: \code{"S. João"}, \code{"Coimbra"})
#'     adequado para legendas de mapas.}
#'   \item{geometry}{Geometria \code{POLYGON}/\code{MULTIPOLYGON} da ULS,
#'     no sistema de referência EPSG:4326.}
#' }
#'
#' @examples
#' \dontrun{
#' library(ulsportugal)
#'
#' # Carregar o mapa das 39 ULS
#' mapa <- ulsportugal()
#'
#' # Visualização básica
#' plot(mapa["NOME_CURTO"], main = "ULS de Portugal Continental")
#'
#' # Usar um ano alternativo de geometrias LAU
#' mapa_2020 <- ulsportugal(ano = "2020")
#' }
#'
#' @seealso \code{\link[giscoR]{gisco_get_lau}}
#'
#' @export
ulsportugal <- function(ano = "2021", verbose = FALSE) {

  if (verbose) message("A obter geometrias LAU ", ano, " do giscoR...")

  raw_mapa <- giscoR::gisco_get_lau(country = "Portugal", year = ano)

  col_id <- intersect(c("LAU_ID", "GISCO_ID", "id", "LAU_CODE"), colnames(raw_mapa))[1]
  if (is.na(col_id)) {
    stop(
      "Nao encontrei coluna de ID nas geometrias giscoR. ",
      "Colunas disponiveis: ", paste(colnames(raw_mapa), collapse = ", ")
    )
  }

  if (verbose) message("A processar codigos DICO e filtrar Portugal Continental...")

  mapa_base <- raw_mapa |>
    dplyr::rename(ID_TEMP = dplyr::all_of(col_id)) |>
    dplyr::mutate(
      ID_TEMP   = as.character(ID_TEMP),
      ID_DIGITS = stringr::str_extract(ID_TEMP, "\\d+"),
      DICO      = substr(ID_DIGITS, 1, 4),
      DIST      = as.numeric(substr(DICO, 1, 2)),
      LAU_NAME  = stringr::str_trim(stringr::str_replace_all(LAU_NAME, "freguesias", "Freguesias"))
    ) |>
    dplyr::filter(!is.na(DIST) & DIST < 20)

  if (verbose) message("A cruzar com dicionario de ULS...")

  mapa_final <- mapa_base |>
    dplyr::left_join(
      dicionario_mestre |> dplyr::filter(!is.na(Freguesia)),
      by = c("DICO" = "DICO", "LAU_NAME" = "Freguesia")
    ) |>
    dplyr::left_join(
      dicionario_mestre |> dplyr::filter(is.na(Freguesia)),
      by = "DICO", suffix = c("_esp", "_gen")
    ) |>
    dplyr::mutate(NOME_ULS = dplyr::coalesce(NOME_ULS_esp, NOME_ULS_gen)) |>
    dplyr::filter(!is.na(NOME_ULS)) |>
    dplyr::group_by(NOME_ULS) |>
    dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop") |>
    dplyr::mutate(
      NOME_CURTO = NOME_ULS |>
        stringr::str_remove("^Unidade Local de Saude d[eoa]'?s? ") |>
        stringr::str_remove("^Unidade Local de Saude ") |>
        stringr::str_remove(", EPE$")
    )

  n_uls <- nrow(mapa_final)
  if (n_uls != 39) {
    warning(
      "Esperavam-se 39 ULS mas foram encontradas ", n_uls, ". ",
      "Verifique o dicionario_uls.csv ou se o giscoR alterou o formato dos identificadores LAU."
    )
  }

  if (verbose) message("Concluido: ", n_uls, " ULS encontradas.")

  mapa_final
}
