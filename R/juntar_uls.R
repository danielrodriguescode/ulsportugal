#' Juntar informação de ULS a um conjunto de dados ao nível de freguesia
#'
#' @description
#' Recebe um \code{data.frame} com dados ao nível de \strong{freguesia} e
#' devolve o mesmo \code{data.frame} enriquecido com as colunas
#' \code{NOME_ULS} e \code{NOME_CURTO}, indicando a Unidade Local de Saúde
#' a que cada freguesia pertence.
#'
#' A correspondência é feita em dois passos, por ordem de prioridade:
#' \enumerate{
#'   \item Pelo código DICO de 4 dígitos + nome de freguesia (quando
#'         \code{col_dico} é fornecido) — método mais robusto.
#'   \item Pelo nome de freguesia apenas — útil quando não há código DICO,
#'         mas mais sensível a discrepâncias de grafia entre fontes.
#' }
#'
#' As linhas sem correspondência ficam com \code{NOME_ULS = NA} e produzem
#' um aviso com o número de casos não encontrados.
#'
#' @param dados \code{data.frame} (ou \code{tibble}) com pelo menos uma coluna
#'   de nome de freguesia.
#' @param col_freguesia Nome (string) da coluna em \code{dados} que contém o
#'   nome da freguesia. Por omissão \code{"Freguesia"}.
#' @param col_dico Nome (string) da coluna em \code{dados} que contém o código
#'   DICO de 4 dígitos. Se \code{NULL} (por omissão), a correspondência é feita
#'   apenas pelo nome de freguesia.
#'
#' @return O \code{data.frame} original com as colunas \code{NOME_ULS} e
#'   \code{NOME_CURTO} adicionadas. A geometria \strong{não} é incluída —
#'   use \code{\link{agregar_por_uls}} para obter um objeto \code{sf}.
#'
#' @examples
#' \dontrun{
#' library(ulsportugal)
#'
#' # Dados fictícios ao nível de freguesia
#' dados <- data.frame(
#'   Freguesia = c("Paranhos", "Campanhã", "Cedofeita"),
#'   casos     = c(120, 95, 80)
#' )
#'
#' # Juntar a ULS correspondente
#' dados_com_uls <- juntar_uls(dados)
#'
#' # Com código DICO disponível
#' dados_dico <- data.frame(
#'   DICO      = c("1311", "1308", "1306"),
#'   Freguesia = c("Paranhos", "Campanhã", "Cedofeita"),
#'   casos     = c(120, 95, 80)
#' )
#' dados_com_uls <- juntar_uls(dados_dico, col_dico = "DICO")
#' }
#'
#' @seealso \code{\link{agregar_por_uls}}
#'
#' @export
juntar_uls <- function(dados, col_freguesia = "Freguesia", col_dico = NULL) {

  # --- validações -----------------------------------------------------------
  if (!is.data.frame(dados)) {
    stop("`dados` tem de ser um data.frame ou tibble.")
  }
  if (!col_freguesia %in% colnames(dados)) {
    stop("Coluna de freguesia '", col_freguesia, "' nao encontrada em `dados`.")
  }
  if (!is.null(col_dico) && !col_dico %in% colnames(dados)) {
    stop("Coluna DICO '", col_dico, "' nao encontrada em `dados`.")
  }

  # Dicionário interno partido em dois: entradas por freguesia e por concelho
  dict_freg   <- dicionario_mestre |> dplyr::filter(!is.na(Freguesia))
  dict_conc   <- dicionario_mestre |>
    dplyr::filter(is.na(Freguesia)) |>
    dplyr::select(DICO, NOME_ULS)

  # --- correspondência ------------------------------------------------------
  if (!is.null(col_dico)) {

    # Passo 1: DICO + Freguesia (correspondência exacta ao nível de freguesia)
    join_freg <- dados |>
      dplyr::left_join(
        dict_freg |> dplyr::rename(.NOME_ULS_esp = NOME_ULS),
        by = stats::setNames(c("DICO", "Freguesia"), c(col_dico, col_freguesia))
      )

    # Passo 2: DICO apenas (para concelhos com uma única ULS)
    resultado <- join_freg |>
      dplyr::left_join(
        dict_conc |> dplyr::rename(.NOME_ULS_gen = NOME_ULS),
        by = stats::setNames("DICO", col_dico)
      ) |>
      dplyr::mutate(NOME_ULS = dplyr::coalesce(.NOME_ULS_esp, .NOME_ULS_gen)) |>
      dplyr::select(-.NOME_ULS_esp, -.NOME_ULS_gen)

  } else {

    # Só pelo nome de freguesia (mais frágil — avisa o utilizador)
    message(
      "Nota: a correspondencia e feita apenas pelo nome de freguesia. ",
      "Para maior fiabilidade, forneca `col_dico` com o codigo DICO de 4 digitos."
    )

    resultado <- dados |>
      dplyr::left_join(
        dict_freg |> dplyr::select(Freguesia, NOME_ULS),
        by = stats::setNames("Freguesia", col_freguesia)
      )
  }

  # --- NOME_CURTO -----------------------------------------------------------
  resultado <- resultado |>
    dplyr::mutate(
      NOME_CURTO = dplyr::if_else(
        !is.na(NOME_ULS),
        NOME_ULS |>
          stringr::str_remove("^Unidade Local de Sa\u00fade d[eoa]'?s? ") |>
          stringr::str_remove("^Unidade Local de Sa\u00fade ") |>
          stringr::str_remove(", EPE$"),
        NA_character_
      )
    )

  # --- aviso de linhas sem ULS ----------------------------------------------
  n_sem_uls <- sum(is.na(resultado$NOME_ULS))
  if (n_sem_uls > 0) {
    warning(
      n_sem_uls, " linha(s) sem correspondencia com uma ULS. ",
      "Verifique a grafia dos nomes de freguesia ou forneca `col_dico`."
    )
  }

  resultado
}
