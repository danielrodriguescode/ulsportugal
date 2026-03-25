# Funções internas de normalização e correspondência de nomes de freguesias
# Não exportadas — uso interno por juntar_uls() e diagnosticar_freguesias()

# -------------------------------------------------------------------------
# .normalizar()
# Remove acentos, converte para minúsculas, elimina pontuação e comprime
# espaços. Usado para tornar as comparações de nomes insensíveis a acentos,
# capitalização e pontuação.
# -------------------------------------------------------------------------
.normalizar <- function(x) {
  if (all(is.na(x))) return(x)
  x <- tolower(trimws(as.character(x)))

  # Substituições explícitas para caracteres portugueses com diacríticos
  x <- gsub("\u00e0|\u00e1|\u00e2|\u00e3|\u00e4", "a", x)  # à á â ã ä
  x <- gsub("\u00e8|\u00e9|\u00ea|\u00eb",         "e", x)  # è é ê ë
  x <- gsub("\u00ec|\u00ed|\u00ee|\u00ef",         "i", x)  # ì í î ï
  x <- gsub("\u00f2|\u00f3|\u00f4|\u00f5|\u00f6", "o", x)  # ò ó ô õ ö
  x <- gsub("\u00f9|\u00fa|\u00fb|\u00fc",         "u", x)  # ù ú û ü
  x <- gsub("\u00e7",                              "c", x)  # ç → c
  x <- gsub("\u00f1",                              "n", x)  # ñ → n

  # Remove tudo o que não seja alfanumérico ou espaço, colapsa espaços
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("\\s+", " ", trimws(x))
}

# -------------------------------------------------------------------------
# .lookup_por_nome()
#
# Para um vector de nomes de freguesia (do utilizador), devolve uma tabela
# de correspondência com o NOME_ULS do dicionário, usando quatro passos
# em cascata:
#
#   1. Exacto        — correspondência textual exacta
#   2. Normalizado   — correspondência após remoção de acentos/capitalização
#   3. Subcadeia     — o nome do utilizador está contido no nome do dicionário
#                      (apanha nomes pré-União de Freguesias)
#   4. Fuzzy         — distância de edição ≤ max_dist (apanha erros de digitação)
#   sem correspondência — nenhum dos passos funcionou
#
# Devolve um data.frame com colunas:
#   nome_original, NOME_ULS, .match_tipo
# -------------------------------------------------------------------------
.lookup_por_nome <- function(nomes, dict_freg, max_dist = 3) {

  candidatos     <- dict_freg$Freguesia
  candidatos_n   <- .normalizar(candidatos)
  uls_candidatos <- dict_freg$NOME_ULS

  resultado <- lapply(unique(nomes), function(nome) {

    if (is.na(nome)) {
      return(data.frame(nome_original = nome, NOME_ULS = NA_character_,
                        .match_tipo = "sem correspondencia", stringsAsFactors = FALSE))
    }

    nome_n <- .normalizar(nome)

    # Passo 1: correspondência exacta
    idx <- which(candidatos == nome)
    if (length(idx) > 0) {
      return(data.frame(nome_original = nome, NOME_ULS = uls_candidatos[idx[1]],
                        .match_tipo = "exacto", stringsAsFactors = FALSE))
    }

    # Passo 2: correspondência normalizada (sem acentos/capitalização)
    idx <- which(candidatos_n == nome_n)
    if (length(idx) > 0) {
      return(data.frame(nome_original = nome, NOME_ULS = uls_candidatos[idx[1]],
                        .match_tipo = "normalizado", stringsAsFactors = FALSE))
    }

    # Passo 3: subcadeia — o nome do utilizador está contido no nome do dicionário
    # Útil quando o utilizador usa o nome antigo de uma freguesia que foi
    # posteriormente integrada numa União de Freguesias
    idx <- which(stringr::str_detect(candidatos_n, stringr::fixed(nome_n)))
    if (length(idx) > 0) {
      return(data.frame(nome_original = nome, NOME_ULS = uls_candidatos[idx[1]],
                        .match_tipo = "subcadeia", stringsAsFactors = FALSE))
    }

    # Passo 4: correspondência fuzzy por distância de edição
    dists    <- utils::adist(nome_n, candidatos_n)
    min_dist <- min(dists)
    if (min_dist <= max_dist) {
      return(data.frame(nome_original = nome, NOME_ULS = uls_candidatos[which.min(dists)],
                        .match_tipo   = paste0("fuzzy (dist=", min_dist, ")"),
                        stringsAsFactors = FALSE))
    }

    # Sem correspondência
    data.frame(nome_original = nome, NOME_ULS = NA_character_,
               .match_tipo = "sem correspondencia", stringsAsFactors = FALSE)
  })

  do.call(rbind, resultado)
}
