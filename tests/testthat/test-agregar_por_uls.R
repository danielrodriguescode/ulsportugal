# Testes para agregar_por_uls()
# Os testes com geometry = TRUE requerem ligacao a internet (giscoR).
# Os testes com geometry = FALSE sao puramente locais.

dados_freg <- data.frame(
  DICO      = c("1311", "1308", "1310", "1311"),
  Freguesia = c("Paranhos", "Campanha", "Ramalde", "Paranhos"),
  casos     = c(120L, 95L, 60L, 30L),
  pop       = c(50000L, 30000L, 40000L, 10000L),
  stringsAsFactors = FALSE
)

dados_sem_numericos <- data.frame(
  Freguesia = c("Paranhos"),
  descricao = c("texto"),
  stringsAsFactors = FALSE
)

# --- sem geometria (testes puramente locais) --------------------------------

test_that("agregar_por_uls() com geometry=FALSE devolve data.frame", {
  resultado <- agregar_por_uls(dados_freg, col_dico = "DICO", geometry = FALSE)
  expect_s3_class(resultado, "data.frame")
})

test_that("agregar_por_uls() com geometry=FALSE tem colunas NOME_ULS e NOME_CURTO", {
  resultado <- agregar_por_uls(dados_freg, col_dico = "DICO", geometry = FALSE)
  expect_true("NOME_ULS"   %in% colnames(resultado))
  expect_true("NOME_CURTO" %in% colnames(resultado))
})

test_that("agregar_por_uls() agrega corretamente com sum (padrao)", {
  resultado <- agregar_por_uls(dados_freg, col_dico = "DICO", geometry = FALSE)
  # Paranhos aparece duas vezes (120 + 30 = 150 casos)
  # As tres freguesias devem colapsar para uma ou mais ULS
  expect_true(all(resultado$casos >= 0))
  expect_true(sum(resultado$casos) == sum(dados_freg$casos))
})

test_that("agregar_por_uls() agrega corretamente com mean", {
  resultado <- agregar_por_uls(dados_freg, col_dico = "DICO", fn = mean, geometry = FALSE)
  expect_true(all(!is.na(resultado$casos)))
})

test_that("agregar_por_uls() uma linha por ULS no output", {
  resultado <- agregar_por_uls(dados_freg, col_dico = "DICO", geometry = FALSE)
  expect_equal(nrow(resultado), dplyr::n_distinct(resultado$NOME_ULS))
})

test_that("agregar_por_uls() preserva as colunas numericas do input", {
  resultado <- agregar_por_uls(dados_freg, col_dico = "DICO", geometry = FALSE)
  expect_true("casos" %in% colnames(resultado))
  expect_true("pop"   %in% colnames(resultado))
})

# --- erros -----------------------------------------------------------------

test_that("agregar_por_uls() da erro se nao ha colunas numericas", {
  expect_error(
    suppressMessages(agregar_por_uls(dados_sem_numericos, geometry = FALSE)),
    regexp = "colunas numericas"
  )
})

test_that("agregar_por_uls() da erro se dados nao e data.frame", {
  expect_error(agregar_por_uls(list(a = 1)), regexp = "data.frame")
})

# --- com geometria (requer internet) ----------------------------------------

test_that("agregar_por_uls() com geometry=TRUE devolve sf", {
  skip_if_offline()
  resultado <- agregar_por_uls(dados_freg, col_dico = "DICO", geometry = TRUE)
  expect_s3_class(resultado, "sf")
})

test_that("agregar_por_uls() com geometry=TRUE tem coluna geometry valida", {
  skip_if_offline()
  resultado <- agregar_por_uls(dados_freg, col_dico = "DICO", geometry = TRUE)
  expect_true(all(sf::st_is_valid(resultado[!sf::st_is_empty(resultado), ])))
})

test_that("agregar_por_uls() com verbose=TRUE nao produz erro", {
  skip_if_offline()
  expect_no_error(
    agregar_por_uls(dados_freg, col_dico = "DICO", verbose = TRUE)
  )
})
