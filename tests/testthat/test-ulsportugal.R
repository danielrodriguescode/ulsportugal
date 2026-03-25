# Testes para a funcao principal ulsportugal()
# Estes testes requerem ligacao a internet (giscoR faz download das geometrias).
# Sao marcados com skip_if_offline() para nao falhar em ambientes sem rede.

test_that("ulsportugal() devolve um objeto sf", {
  skip_if_offline()
  resultado <- ulsportugal()
  expect_s3_class(resultado, "sf")
})

test_that("ulsportugal() devolve exactamente 39 ULS", {
  skip_if_offline()
  resultado <- ulsportugal()
  expect_equal(nrow(resultado), 39)
})

test_that("ulsportugal() tem as colunas esperadas", {
  skip_if_offline()
  resultado <- ulsportugal()
  expect_true(all(c("NOME_ULS", "NOME_CURTO", "geometry") %in% colnames(resultado)))
})

test_that("NOME_ULS nao tem valores NA", {
  skip_if_offline()
  resultado <- ulsportugal()
  expect_false(anyNA(resultado$NOME_ULS))
})

test_that("NOME_CURTO nao contem o prefixo 'Unidade Local de Saude'", {
  skip_if_offline()
  resultado <- ulsportugal()
  expect_false(any(grepl("^Unidade Local de Sa", resultado$NOME_CURTO)))
})

test_that("NOME_CURTO nao contem o sufixo ', EPE'", {
  skip_if_offline()
  resultado <- ulsportugal()
  expect_false(any(grepl(", EPE$", resultado$NOME_CURTO)))
})

test_that("NOME_CURTO nao contem o sufixo 'E.P.E.'", {
  skip_if_offline()
  resultado <- ulsportugal()
  expect_false(any(grepl("E\\.P\\.E\\.\\s*$", resultado$NOME_CURTO)))
})

test_that("as geometrias sao todas validas", {
  skip_if_offline()
  resultado <- ulsportugal()
  expect_true(all(sf::st_is_valid(resultado)))
})

test_that("nao existem geometrias vazias", {
  skip_if_offline()
  resultado <- ulsportugal()
  expect_false(any(sf::st_is_empty(resultado)))
})

test_that("o parametro verbose corre sem erros", {
  skip_if_offline()
  expect_no_error(ulsportugal(verbose = TRUE))
})

test_that("o parametro ano e passado corretamente ao giscoR", {
  skip_if_offline()
  # Apenas verifica que a funcao aceita o argumento sem erro
  resultado <- ulsportugal(ano = "2021")
  expect_s3_class(resultado, "sf")
})

test_that("NOME_ULS contem ULS esperadas", {
  skip_if_offline()
  resultado <- ulsportugal()
  uls_esperadas <- c(
    "Unidade Local de Saude do Algarve, EPE",
    "Unidade Local de Saude de Coimbra, EPE",
    "Unidade Local de Saude do Alto Minho, EPE",
    "Unidade Local de Saude de Braga, EPE"
  )
  # Normalizamos para comparacao sem accentos problematicos
  nomes <- resultado$NOME_ULS
  for (uls in uls_esperadas) {
    expect_true(
      any(stringr::str_detect(nomes, stringr::fixed(
        stringr::str_replace_all(uls, "u", "u") # identidade, so para explicitar
      ))),
      label = paste("ULS nao encontrada:", uls)
    )
  }
})
