# ulsportugal

O pacote **ulsportugal** simplifica a visualização e análise territorial das **39 Unidades Locais de Saúde (ULS)** de **Portugal Continental**.

Automatiza a agregação de geometrias ao nível das **freguesias** e **concelhos**, trata inconsistências de nomenclatura e codificação territorial (**DICO/LAU**), e permite ao utilizador trabalhar directamente com os seus próprios dados ao nível de freguesia — sem ter de lidar com shapefiles ou cruzamentos manuais.

## Instalação

```r
# install.packages("remotes")
remotes::install_github("danielrodriguescode/ulsportugal")
```

## Funções disponíveis

| Função | O que faz |
|---|---|
| `ulsportugal()` | Devolve o mapa `sf` das 39 ULS |
| `juntar_uls()` | Adiciona `NOME_ULS` aos seus dados por freguesia |
| `agregar_por_uls()` | Agrega os seus dados de freguesia para ULS e devolve um `sf` pronto a mapear |

---

## Caso de uso 1 — Obter o mapa das ULS

A forma mais simples de começar. `ulsportugal()` devolve um objeto `sf` com uma linha por ULS, pronto a usar com `ggplot2`, `tmap`, ou `plot()`.

```r
library(ulsportugal)
library(ggplot2)

mapa <- ulsportugal()

ggplot(mapa) +
  geom_sf(fill = "#d4e6f1", colour = "white") +
  geom_sf_label(aes(label = NOME_CURTO), size = 2) +
  theme_minimal() +
  labs(title = "Unidades Locais de Saúde — Portugal Continental")
```

Por omissão são usadas as geometrias LAU de 2021. Para outro ano:

```r
mapa_2020 <- ulsportugal(ano = "2020")
```

---

## Caso de uso 2 — Tenho dados por freguesia e quero saber a ULS de cada uma

Use `juntar_uls()` para enriquecer o seu `data.frame` com `NOME_ULS` e `NOME_CURTO`, sem alterar a estrutura dos dados nem adicionar geometria.

```r
dados <- data.frame(
  Freguesia = c("Paranhos", "Campanhã", "Ramalde", "Cedofeita"),
  internamentos = c(312, 198, 275, 143)
)

dados_com_uls <- juntar_uls(dados)

# # A tibble: 4 × 4
#   Freguesia  internamentos NOME_ULS                              NOME_CURTO
#   <chr>              <dbl> <chr>                                 <chr>
# 1 Paranhos             312 Unidade Local de Saúde de São João... São João
# 2 Campanhã             198 Unidade Local de Saúde de São João... São João
# ...
```

Se os seus dados tiverem um código **DICO** de 4 dígitos, use-o — a correspondência fica mais robusta:

```r
dados_com_uls <- juntar_uls(dados, col_dico = "DICO")
```

---

## Caso de uso 3 — Tenho dados por freguesia e quero um mapa por ULS

Use `agregar_por_uls()`. A função trata de tudo: cruza as freguesias com as ULS, agrega as colunas numéricas (por omissão com `sum`) e devolve um objeto `sf` pronto a mapear.

```r
library(ggplot2)

dados <- data.frame(
  Freguesia = c("Paranhos", "Campanhã", "Ramalde", "Cedofeita",
                "Arroios", "Marvila", "Areeiro"),
  casos  = c(312, 198, 275, 143, 421, 89, 204),
  pop    = c(49000, 28000, 42000, 19000, 31000, 37000, 22000)
)

mapa <- agregar_por_uls(dados)

ggplot(mapa) +
  geom_sf(aes(fill = casos)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme_minimal() +
  labs(title = "Casos por ULS", fill = "N.º casos")
```

Para agregar com outra função (p. ex., média):

```r
mapa_media <- agregar_por_uls(dados, fn = mean)
```

Para obter apenas a tabela sem geometria:

```r
tabela <- agregar_por_uls(dados, geometry = FALSE)
```

---

## Estrutura das colunas de output

| Coluna | Descrição |
|---|---|
| `NOME_ULS` | Nome completo e oficial da ULS (ex.: `"Unidade Local de Saúde de São João, EPE"`) |
| `NOME_CURTO` | Nome abreviado para legendas (ex.: `"São João"`) |
| `geometry` | Geometria `POLYGON`/`MULTIPOLYGON` em EPSG:4326 (apenas em `ulsportugal` e `agregar_por_uls`) |

---

## Licença

MIT
