#' Juntar informação de ULS a um conjunto de dados ao nível de freguesia
#'
#' @description
#' Recebe um \code{data.frame} com dados ao nível de \strong{freguesia} e
#' devolve o mesmo \code{data.frame} enriquecido com as colunas
#' \code{NOME_ULS} e \code{NOME_CURTO}, indicando a Unidade Local de Saúde
#' a que cada freguesia pertence.
#'
#' A correspondência é feita em cascata, por ordem de prioridade:
#' \enumerate{
#'   \item \strong{DICO + nome exacto} — quando \code{col_dico} é fornecido;
#'         método mais robusto.
#'   \item \strong{DICO apenas} — para concelhos com uma única ULS.
#'   \item \strong{Nome exacto} — quando \code{col_dico = NULL}.
#'   \item \strong{Nome normalizado} — insensível a acentos e capitalização
#'         (ex.: \code{"sao joao"} encontra \code{"São João"}).
#'   \item \strong{Subcadeia} — o nome do utilizador está contido no nome do
#'         dicionário; apanha nomes pré-União de Freguesias
#'         (ex.: \code{"Cedofeita"} encontra
#'         \code{"União das Freguesias de Cedofeita, ..."}).
#'   \item \strong{Fuzzy} — distância de edição reduzida; apanha erros de
#'         digitação e variações menores.
#' }
#'
#' As linhas sem correspondência ficam com \code{NOME_ULS = NA} e produzem
#' um aviso. Use \code{\link{diagnosticar_freguesias}} para ver um relatório
#' detalhado da qualidade da correspondência antes de agregar.
#'
#' @param dados \code{data.frame} (ou \code{tibble}) com pelo menos uma coluna
#'   de nome de freguesia.
#' @param col_freguesia Nome (string) da coluna em \code{dados} que contém o
#'   nome da freguesia. Por omissão \code{"Freguesia"}.
#' @param col_dico Nome (string) da coluna em \code{dados} que contém o código
#'   DICO de 4 dígitos. Se \code{NULL} (por omissão), a correspondência é feita
#'   apenas pelo nome de freguesia.
#' @param max_dist Distância máxima de edição aceite na correspondência fuzzy.
#'   Por omissão \code{3}. Reduza para correspondências mais estritas ou aumente
#'   para tolerar mais variações.
#'
#' @return O \code{data.frame} original com as colunas \code{NOME_ULS} e
#'   \code{NOME_CURTO} adicionadas. A geometria \strong{não} é incluída —
#'   use \code{\link{agregar_por_uls}} para obter um objeto \code{sf}.
#'
#' @examples
#' \dontrun{
#' library(ulsportugal)
#'
#' # Correspondência por nome (multi-passo automático)
#' dados <- data.frame(
#'   Freguesia = c("Paranhos", "Cedofeita", "sao joao da madeira"),
#'   casos = c(120, 80, 55)
#' )
#' juntar_uls(dados)
#'
#' # Com código DICO (mais robusto)
#' dados_dico <- data.frame(
#'   DICO = c("1311", "1306", "0109"),
#'   Freguesia = c("Paranhos", "Cedofeita", "São João da Madeira"),
#'   casos = c(120, 80, 55)
#' )
#' juntar_uls(dados_dico, col_dico = "DICO")
#' }
#'
#' @seealso \code{\link{agregar_por_uls}}, \code{\link{diagnosticar_freguesias}}
#'
#' @export
juntar_uls <- function(dados,
                       col_freguesia = "Freguesia",
                       col_dico      = NULL,
                       max_dist      = 3) {

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

  dict_freg <- dicionario_mestre |> dplyr::filter(!is.na(Freguesia))
  dict_conc <- dicionario_mestre |>
    dplyr::filter(is.na(Freguesia)) |>
    dplyr::select(DICO, NOME_ULS)

  # --- correspondência ------------------------------------------------------

  if (!is.null(col_dico)) {

    # Caminho DICO: join por DICO+Freguesia exacto, depois DICO sozinho
    resultado <- dados |>
      dplyr::left_join(
        dict_freg |> dplyr::rename(.NOME_ULS_esp = NOME_ULS),
        by = stats::setNames(c("DICO", "Freguesia"), c(col_dico, col_freguesia))
      ) |>
      dplyr::left_join(
        dict_conc |> dplyr::rename(.NOME_ULS_gen = NOME_ULS),
        by = stats::setNames("DICO", col_dico)
      ) |>
      dplyr::mutate(NOME_ULS = dplyr::coalesce(.NOME_ULS_esp, .NOME_ULS_gen)) |>
      dplyr::select(-.NOME_ULS_esp, -.NOME_ULS_gen)

  } else {

    # Caminho nome: correspondência em cascata (exacto → normalizado →
    # subcadeia → fuzzy)
    nomes_usuario <- dados[[col_freguesia]]
    lookup <- .lookup_por_nome(nomes_usuario, dict_freg, max_dist = max_dist)

    # Contar e comunicar os tipos de correspondência usados
    tipos <- table(lookup$.match_tipo)
    tipos_usados <- setdiff(names(tipos), "exacto")
    if (length(tipos_usados) > 0) {
      msg <- paste(
        sapply(tipos_usados, function(t) paste0(tipos[t], " via '", t, "'")),
        collapse = ", "
      )
      message("Correspondencias adicionais (alem de exacto): ", msg,
              ".\nUse diagnosticar_freguesias() para ver o detalhe.")
    }

    resultado <- dados |>
      dplyr::left_join(
        lookup |> dplyr::select(nome_original, NOME_ULS),
        by = stats::setNames("nome_original", col_freguesia)
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
      "Use diagnosticar_freguesias() para ver sugestoes de correcao."
    )
  }

  resultado
}
