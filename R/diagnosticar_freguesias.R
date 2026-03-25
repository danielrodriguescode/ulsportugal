#' Diagnosticar a qualidade da correspondência de nomes de freguesia
#'
#' @description
#' Analisa os nomes de freguesia nos dados do utilizador e devolve um relatório
#' detalhado com o resultado da correspondência de cada nome único, incluindo
#' o método usado, o nome encontrado no dicionário e a ULS associada.
#'
#' É útil para identificar e corrigir problemas antes de usar
#' \code{\link{juntar_uls}} ou \code{\link{agregar_por_uls}}, especialmente
#' quando os dados provêm de fontes com nomes de freguesia diferentes do
#' padrão giscoR/LAU (acentuação divergente, nomes pré-reforma de 2013,
#' abreviaturas, etc.).
#'
#' @param dados \code{data.frame} (ou \code{tibble}) com pelo menos uma coluna
#'   de nome de freguesia.
#' @param col_freguesia Nome (string) da coluna em \code{dados} que contém o
#'   nome da freguesia. Por omissão \code{"Freguesia"}.
#' @param col_dico Nome (string) da coluna em \code{dados} que contém o código
#'   DICO de 4 dígitos. Se fornecido, a correspondência por DICO é testada
#'   primeiro. Por omissão \code{NULL}.
#' @param max_dist Distância máxima de edição aceite na correspondência fuzzy.
#'   Por omissão \code{3}.
#'
#' @return Um \code{tibble} com uma linha por nome único de freguesia e as
#'   seguintes colunas:
#'   \describe{
#'     \item{nome_original}{Nome de freguesia tal como aparece nos dados do
#'       utilizador.}
#'     \item{correspondencia}{Método de correspondência usado:
#'       \code{"exacto"}, \code{"normalizado"}, \code{"subcadeia"},
#'       \code{"fuzzy (dist=N)"}, \code{"dico+exacto"}, \code{"dico"} ou
#'       \code{"sem correspondencia"}.}
#'     \item{nome_dicionario}{Nome de freguesia no dicionário interno a que
#'       foi feita a correspondência (\code{NA} se sem correspondência).}
#'     \item{NOME_ULS}{Nome da ULS associada (\code{NA} se sem
#'       correspondência).}
#'     \item{n}{Número de linhas nos dados originais com este nome.}
#'   }
#'   As linhas estão ordenadas por correspondência (primeiro as
#'   problemáticas).
#'
#' @examples
#' \dontrun{
#' library(ulsportugal)
#'
#' dados <- data.frame(
#'   Freguesia = c("Paranhos", "Cedofeita", "sao joao da madeira",
#'                 "Paranhos", "FreguesiaInexistente"),
#'   casos = c(120, 80, 55, 30, 10)
#' )
#'
#' diagnosticar_freguesias(dados)
#'
#' # # A tibble: 4 × 5
#' #   nome_original       correspondencia   nome_dicionario   NOME_ULS         n
#' #   <chr>               <chr>             <chr>             <chr>        <int>
#' # 1 FreguesiaInexistente sem correspondência NA             NA               1
#' # 2 sao joao da madeira normalizado        São João da Mad… ULS Entre D…     1
#' # 3 Cedofeita           subcadeia          União das Freg…  ULS Santo A…     1
#' # 4 Paranhos            exacto             Paranhos         ULS São João     2
#' }
#'
#' @seealso \code{\link{juntar_uls}}, \code{\link{agregar_por_uls}}
#'
#' @export
diagnosticar_freguesias <- function(dados,
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

  # Contagem de ocorrências por nome (e DICO, se disponível)
  if (!is.null(col_dico)) {
    contagens <- dados |>
      dplyr::count(
        nome_original = .data[[col_freguesia]],
        DICO          = .data[[col_dico]]
      )
  } else {
    contagens <- dados |>
      dplyr::count(nome_original = .data[[col_freguesia]])
  }

  # --- correspondência por nome (todas as linhas únicas) --------------------
  lookup_nome <- .lookup_por_nome(
    contagens$nome_original,
    dict_freg,
    max_dist = max_dist
  ) |>
    dplyr::rename(correspondencia_nome = .match_tipo,
                  NOME_ULS_nome        = NOME_ULS)

  resultado <- contagens |>
    dplyr::left_join(lookup_nome, by = "nome_original")

  # --- sobrescrever com correspondência DICO quando disponível --------------
  if (!is.null(col_dico)) {

    # DICO + Freguesia exacto
    match_dico_freg <- dict_freg |>
      dplyr::rename(nome_original  = Freguesia,
                    NOME_ULS_dico  = NOME_ULS)

    match_dico_only <- dict_conc |>
      dplyr::rename(NOME_ULS_gen = NOME_ULS)

    resultado <- resultado |>
      dplyr::left_join(match_dico_freg,
                       by = c("DICO", "nome_original")) |>
      dplyr::left_join(match_dico_only, by = "DICO") |>
      dplyr::mutate(
        NOME_ULS = dplyr::case_when(
          !is.na(NOME_ULS_dico) ~ NOME_ULS_dico,
          !is.na(NOME_ULS_gen)  ~ NOME_ULS_gen,
          TRUE                  ~ NOME_ULS_nome
        ),
        correspondencia = dplyr::case_when(
          !is.na(NOME_ULS_dico) ~ "dico+exacto",
          !is.na(NOME_ULS_gen)  ~ "dico",
          TRUE                  ~ correspondencia_nome
        )
      ) |>
      dplyr::select(nome_original, DICO, correspondencia, NOME_ULS, n)

  } else {

    resultado <- resultado |>
      dplyr::rename(correspondencia = correspondencia_nome,
                    NOME_ULS        = NOME_ULS_nome) |>
      dplyr::select(nome_original, correspondencia, NOME_ULS, n)
  }

  # --- adicionar nome_dicionario para contexto ------------------------------
  dict_lookup <- dict_freg |>
    dplyr::select(Freguesia, NOME_ULS) |>
    dplyr::rename(nome_dicionario = Freguesia)

  resultado <- resultado |>
    dplyr::left_join(dict_lookup, by = "NOME_ULS") |>
    dplyr::select(
      nome_original, correspondencia, nome_dicionario, NOME_ULS, n,
      dplyr::everything()
    )

  # --- ordenar: problemáticos primeiro, depois por método ------------------
  ordem_corr <- c(
    "sem correspondencia",
    "fuzzy (dist=3)", "fuzzy (dist=2)", "fuzzy (dist=1)",
    "subcadeia", "normalizado", "dico", "dico+exacto", "exacto"
  )
  resultado <- resultado |>
    dplyr::mutate(
      .ordem = match(correspondencia, ordem_corr, nomatch = 0L)
    ) |>
    dplyr::arrange(.ordem) |>
    dplyr::select(-.ordem)

  # --- resumo no ecrã -------------------------------------------------------
  n_sem   <- sum(resultado$correspondencia == "sem correspondencia", na.rm = TRUE)
  n_fuzzy <- sum(stringr::str_starts(resultado$correspondencia %||% "", "fuzzy"),
                 na.rm = TRUE)
  n_sub   <- sum(resultado$correspondencia == "subcadeia", na.rm = TRUE)
  n_norm  <- sum(resultado$correspondencia == "normalizado", na.rm = TRUE)
  n_ok    <- nrow(resultado) - n_sem - n_fuzzy - n_sub - n_norm

  message(
    nrow(resultado), " nomes unicos analisados: ",
    n_ok,    " exacto(s), ",
    n_norm,  " normalizado(s), ",
    n_sub,   " subcadeia, ",
    n_fuzzy, " fuzzy, ",
    n_sem,   " sem correspondencia."
  )

  if (n_sem > 0) {
    message(
      "  -> ", n_sem, " nome(s) sem correspondencia. ",
      "Verifique a grafia ou forneca `col_dico`."
    )
  }
  if (n_fuzzy > 0 || n_sub > 0) {
    message(
      "  -> ", n_fuzzy + n_sub, " nome(s) encontrado(s) por metodo nao-exacto. ",
      "Confirme as correspondencias antes de usar os dados."
    )
  }

  tibble::as_tibble(resultado)
}

# Operador %||% (NULL coalesce) — evita dependência de rlang
`%||%` <- function(x, y) if (is.null(x)) y else x
