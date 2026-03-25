# Testes para juntar_uls()
# Estes testes usam apenas dados internos do pacote (dicionario_mestre),
# sem ligacao a internet.

# Dados de teste com nomes de freguesia conhecidos no dicionario
dados_freg <- data.frame(
  Freguesia = c("Paranhos", "Campanha", "Ramalde"),
  casos     = c(120L, 95L, 60L),
  stringsAsFactors = FALSE
)

dados_dico <- data.frame(
  DICO      = c("1311", "1308", "1310"),
  Freguesia = c("Paranhos", "Campanha", "Ramalde"),
  casos     = c(120L, 95L, 60L),
  stringsAsFactors = FALSE
)

dados_mau <- data.frame(
  Freguesia = c("FreguesiaQueNaoExiste"),
  valor     = 1L,
  stringsAsFactors = FALSE
)

# --- estrutura do output ---------------------------------------------------

test_that("juntar_uls() devolve um data.frame", {
  resultado <- suppressMessages(juntar_uls(dados_freg))
  expect_s3_class(resultado, "data.frame")
})

test_that("juntar_uls() preserva o numero de linhas do input", {
  resultado <- suppressMessages(juntar_uls(dados_freg))
  expect_equal(nrow(resultado), nrow(dados_freg))
})

test_that("juntar_uls() adiciona as colunas NOME_ULS e NOME_CURTO", {
  resultado <- suppressMessages(juntar_uls(dados_freg))
  expect_true("NOME_ULS"   %in% colnames(resultado))
  expect_true("NOME_CURTO" %in% colnames(resultado))
})

test_that("juntar_uls() preserva as colunas originais", {
  resultado <- suppressMessages(juntar_uls(dados_freg))
  expect_true("Freguesia" %in% colnames(resultado))
  expect_true("casos"     %in% colnames(resultado))
})

# --- correspondencia -------------------------------------------------------

test_that("juntar_uls() com col_dico produz NOME_ULS sem NAs para dados validos", {
  resultado <- juntar_uls(dados_dico, col_dico = "DICO")
  expect_false(anyNA(resultado$NOME_ULS))
})

test_that("juntar_uls() devolve NOME_ULS = NA para freguesias desconhecidas", {
  resultado <- suppressWarnings(suppressMessages(juntar_uls(dados_mau)))
  expect_true(is.na(resultado$NOME_ULS[1]))
})

test_that("juntar_uls() emite aviso quando ha linhas sem correspondencia", {
  expect_warning(
    suppressMessages(juntar_uls(dados_mau)),
    regexp = "sem correspondencia"
  )
})

test_that("juntar_uls() emite mensagem quando nao e fornecido col_dico", {
  expect_message(
    suppressWarnings(juntar_uls(dados_freg)),
    regexp = "apenas pelo nome"
  )
})

# --- NOME_CURTO ------------------------------------------------------------

test_that("NOME_CURTO nao contem o prefixo 'Unidade Local de Saude'", {
  resultado <- juntar_uls(dados_dico, col_dico = "DICO")
  expect_false(any(grepl("^Unidade Local de Sa", resultado$NOME_CURTO, useBytes = TRUE)))
})

test_that("NOME_CURTO nao contem o sufixo ', EPE'", {
  resultado <- juntar_uls(dados_dico, col_dico = "DICO")
  expect_false(any(grepl(", EPE$", resultado$NOME_CURTO)))
})

# --- erros -----------------------------------------------------------------

test_that("juntar_uls() da erro se col_freguesia nao existe", {
  expect_error(
    juntar_uls(dados_freg, col_freguesia = "ColunaNaoExiste"),
    regexp = "nao encontrada"
  )
})

test_that("juntar_uls() da erro se col_dico nao existe", {
  expect_error(
    juntar_uls(dados_freg, col_dico = "ColunaNaoExiste"),
    regexp = "nao encontrada"
  )
})

test_that("juntar_uls() da erro se dados nao e data.frame", {
  expect_error(juntar_uls(list(a = 1)), regexp = "data.frame")
})
