# Testes para as funções internas .normalizar() e .lookup_por_nome()
# e para a função exportada diagnosticar_freguesias()

# ---- .normalizar() --------------------------------------------------------

test_that(".normalizar() converte para minúsculas", {
  expect_equal(.normalizar("PARANHOS"), "paranhos")
})

test_that(".normalizar() remove acentos portugueses", {
  expect_equal(.normalizar("São João"),    "sao joao")
  expect_equal(.normalizar("Cedofeita"),   "cedofeita")
  expect_equal(.normalizar("Câmara"),      "camara")
  expect_equal(.normalizar("Évora"),       "evora")
  expect_equal(.normalizar("Ílhavo"),      "ilhavo")
  expect_equal(.normalizar("Óbidos"),      "obidos")
  expect_equal(.normalizar("Alcácer"),     "alcacer")
})

test_that(".normalizar() remove pontuação e colapsa espaços", {
  expect_equal(.normalizar("Vila   Nova"), "vila nova")
  expect_equal(.normalizar("São-João"),    "sao joao")
  expect_equal(.normalizar("S. João"),     "s  joao")  # ponto → espaço
})

test_that(".normalizar() devolve NA para NA", {
  expect_true(is.na(.normalizar(NA)))
})

# ---- .lookup_por_nome() ---------------------------------------------------

dict_teste <- data.frame(
  Freguesia = c(
    "Paranhos",
    "São João da Madeira",
    "União das Freguesias de Cedofeita, Santo Ildefonso, Sé, Miragaia, São Nicolau e Vitória"
  ),
  NOME_ULS = c(
    "Unidade Local de Saúde de São João, EPE",
    "Unidade Local de Saúde de Entre Douro e Vouga, EPE",
    "Unidade Local de Saúde de Santo António, EPE"
  ),
  stringsAsFactors = FALSE
)

test_that(".lookup_por_nome() encontra correspondência exacta", {
  res <- .lookup_por_nome("Paranhos", dict_teste)
  expect_equal(res$NOME_ULS, "Unidade Local de Saúde de São João, EPE")
  expect_equal(res$.match_tipo, "exacto")
})

test_that(".lookup_por_nome() encontra por nome normalizado (sem acento)", {
  res <- .lookup_por_nome("sao joao da madeira", dict_teste)
  expect_equal(res$NOME_ULS, "Unidade Local de Saúde de Entre Douro e Vouga, EPE")
  expect_equal(res$.match_tipo, "normalizado")
})

test_that(".lookup_por_nome() encontra nome pre-uniao por subcadeia", {
  res <- .lookup_por_nome("Cedofeita", dict_teste)
  expect_equal(res$NOME_ULS, "Unidade Local de Saúde de Santo António, EPE")
  expect_equal(res$.match_tipo, "subcadeia")
})

test_that(".lookup_por_nome() encontra por fuzzy para erro de digitação", {
  # "Parannhos" tem distância 1 de "Paranhos"
  res <- .lookup_por_nome("Parannhos", dict_teste)
  expect_equal(res$NOME_ULS, "Unidade Local de Saúde de São João, EPE")
  expect_true(stringr::str_starts(res$.match_tipo, "fuzzy"))
})

test_that(".lookup_por_nome() devolve NA para nome desconhecido", {
  res <- .lookup_por_nome("FreguesiaQueNaoExiste123", dict_teste)
  expect_true(is.na(res$NOME_ULS))
  expect_equal(res$.match_tipo, "sem correspondencia")
})

# ---- diagnosticar_freguesias() -------------------------------------------

dados_diag <- data.frame(
  Freguesia = c("Paranhos", "Paranhos", "sao joao da madeira",
                "Cedofeita", "FreguesiaInexistente"),
  casos = c(120, 30, 55, 80, 10),
  stringsAsFactors = FALSE
)

test_that("diagnosticar_freguesias() devolve tibble", {
  res <- suppressMessages(diagnosticar_freguesias(dados_diag))
  expect_s3_class(res, "tbl_df")
})

test_that("diagnosticar_freguesias() tem uma linha por nome único", {
  res <- suppressMessages(diagnosticar_freguesias(dados_diag))
  expect_equal(nrow(res), dplyr::n_distinct(dados_diag$Freguesia))
})

test_that("diagnosticar_freguesias() tem colunas esperadas", {
  res <- suppressMessages(diagnosticar_freguesias(dados_diag))
  expect_true(all(c("nome_original", "correspondencia", "nome_dicionario",
                     "NOME_ULS", "n") %in% colnames(res)))
})

test_that("diagnosticar_freguesias() ordena problematicos primeiro", {
  res <- suppressMessages(diagnosticar_freguesias(dados_diag))
  expect_equal(res$nome_original[1], "FreguesiaInexistente")
})

test_that("diagnosticar_freguesias() conta ocorrencias correctamente", {
  res <- suppressMessages(diagnosticar_freguesias(dados_diag))
  n_paranhos <- res$n[res$nome_original == "Paranhos"]
  expect_equal(n_paranhos, 2L)
})

test_that("diagnosticar_freguesias() da erro se col_freguesia nao existe", {
  expect_error(
    diagnosticar_freguesias(dados_diag, col_freguesia = "ColunaNaoExiste"),
    regexp = "nao encontrada"
  )
})
