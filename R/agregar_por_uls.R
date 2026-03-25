#' Agregar dados de freguesia para o nível de ULS
#'
#' @description
#' Recebe um \code{data.frame} com dados ao nível de \strong{freguesia},
#' agrega todas as colunas numéricas para o nível de
#' \strong{Unidade Local de Saúde} usando uma função à escolha, e devolve
#' opcionalmente um objeto \code{sf} pronto a mapear.
#'
#' Internamente, chama \code{\link{juntar_uls}} para obter a coluna
#' \code{NOME_ULS} e depois faz o agrupamento. As linhas sem correspondência
#' ULS são descartadas com aviso.
#'
#' @param dados \code{data.frame} (ou \code{tibble}) com dados ao nível de
#'   freguesia e pelo menos uma coluna numérica a agregar.
#' @param col_freguesia Nome (string) da coluna em \code{dados} que contém o
#'   nome de freguesia. Por omissão \code{"Freguesia"}.
#' @param col_dico Nome (string) da coluna em \code{dados} que contém o código
#'   DICO de 4 dígitos. Se \code{NULL} (por omissão), a correspondência é feita
#'   apenas pelo nome de freguesia.
#' @param fn Função de agregação a aplicar às colunas numéricas.
#'   Por omissão \code{sum}. Outras opções comuns: \code{mean}, \code{median},
#'   \code{max}, etc. Deve aceitar o argumento \code{na.rm}.
#' @param geometry Lógico. Se \code{TRUE} (por omissão), junta as geometrias
#'   das ULS ao resultado, devolvendo um objeto \code{sf}. Se \code{FALSE},
#'   devolve um \code{tibble} simples.
#' @param ano Ano das geometrias LAU a utilizar (passado a
#'   \code{\link{ulsportugal}}). Ignorado se \code{geometry = FALSE}.
#'   Por omissão \code{"2021"}.
#' @param verbose Lógico. Se \code{TRUE}, emite mensagens de progresso.
#'   Por omissão \code{FALSE}.
#'
#' @return Se \code{geometry = TRUE}: um objeto \code{sf} com uma linha por
#'   ULS, as colunas numéricas agregadas e a geometria da ULS.\cr
#'   Se \code{geometry = FALSE}: um \code{tibble} com uma linha por ULS e
#'   as colunas numéricas agregadas.
#'
#'   Colunas sempre presentes no resultado:
#'   \describe{
#'     \item{NOME_ULS}{Nome completo da ULS.}
#'     \item{NOME_CURTO}{Nome abreviado, adequado para legendas de mapas.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(ulsportugal)
#' library(ggplot2)
#'
#' # Dados fictícios ao nível de freguesia
#' dados <- data.frame(
#'   Freguesia = c("Paranhos", "Campanhã", "Cedofeita", "Ramalde"),
#'   casos     = c(120, 95, 80, 60),
#'   pop       = c(50000, 30000, 20000, 40000)
#' )
#'
#' # Agregar por ULS (soma por omissão) e obter sf
#' mapa <- agregar_por_uls(dados)
#'
#' # Mapa temático com ggplot2
#' ggplot(mapa) +
#'   geom_sf(aes(fill = casos)) +
#'   scale_fill_viridis_c() +
#'   labs(title = "Casos por ULS", fill = "N.º casos")
#'
#' # Usar a média como função de agregação
#' mapa_media <- agregar_por_uls(dados, fn = mean)
#'
#' # Apenas tabela, sem geometria
#' tabela <- agregar_por_uls(dados, geometry = FALSE)
#' }
#'
#' @seealso \code{\link{juntar_uls}}, \code{\link{ulsportugal}}
#'
#' @export
agregar_por_uls <- function(
    dados,
    col_freguesia = "Freguesia",
    col_dico      = NULL,
    fn            = sum,
    geometry      = TRUE,
    ano           = "2021",
    verbose       = FALSE
) {

  # --- validações -----------------------------------------------------------
  if (!is.data.frame(dados)) {
    stop("`dados` tem de ser um data.frame ou tibble.")
  }

  cols_numericas <- names(dados)[sapply(dados, is.numeric)]
  # Excluir a coluna DICO se for numérica (é um código, não um valor)
  if (!is.null(col_dico)) {
    cols_numericas <- setdiff(cols_numericas, col_dico)
  }
  if (length(cols_numericas) == 0) {
    stop("`dados` nao tem colunas numericas para agregar.")
  }

  # --- juntar ULS -----------------------------------------------------------
  if (verbose) message("A associar cada freguesia a uma ULS...")

  dados_com_uls <- juntar_uls(
    dados,
    col_freguesia = col_freguesia,
    col_dico      = col_dico
  )

  # Descartar linhas sem ULS
  n_sem_uls <- sum(is.na(dados_com_uls$NOME_ULS))
  if (n_sem_uls > 0) {
    warning(n_sem_uls, " linha(s) sem ULS foram excluidas da agregacao.")
    dados_com_uls <- dados_com_uls |> dplyr::filter(!is.na(NOME_ULS))
  }

  # --- agregar por ULS ------------------------------------------------------
  if (verbose) message("A agregar ", length(cols_numericas), " coluna(s) numerica(s) por ULS...")

  resultado <- dados_com_uls |>
    dplyr::group_by(NOME_ULS, NOME_CURTO) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols_numericas), \(x) fn(x, na.rm = TRUE)),
      .groups = "drop"
    )

  # --- geometria ------------------------------------------------------------
  if (geometry) {
    if (verbose) message("A obter geometrias das ULS...")

    geo_uls <- ulsportugal(ano = ano, verbose = verbose) |>
      dplyr::select(NOME_ULS, geometry)

    resultado <- geo_uls |>
      dplyr::left_join(resultado, by = "NOME_ULS")

    n_sem_geo <- sum(is.na(resultado$NOME_CURTO))
    if (n_sem_geo > 0) {
      warning(n_sem_geo, " ULS na geometria nao tiveram dados correspondentes (ficam a NA).")
    }
  }

  if (verbose) message("Concluido.")

  resultado
}
