# ulsportugal

O pacote **ulsportugal** simplifica a visualização e análise territorial das **39 Unidades Locais de Saúde (ULS)** de **Portugal Continental**.

O projeto automatiza a agregação de geometrias ao nível das **freguesias** e **concelhos**, tratando inconsistências de nomenclatura e codificação territorial (**DICO/LAU**) frequentemente encontradas em sistemas de informação de saúde.

## 🚀 Funcionalidades

- **Geometrias prontas a usar** para as 39 ULS do Continente (objeto `sf`).
- **Normalização de dados**: tratamento automático de nomes de freguesias (incluindo Uniões de Freguesias) e harmonização de códigos territoriais entre fontes (p. ex., **Eurostat via `giscoR`** e referências territoriais nacionais).
- **Simplicidade**: uma única função para obter o mapa completo e avançar para análise/visualização.

## 📦 Instalação

### Via GitHub

```r
# install.packages("remotes")
# remotes::install_github("danielrodriguescode/ulsportugal")
```

### Carregar o pacote

```r
library(ulsportugal)
```

## 🛠️ Utilização

A função principal, `ulsportugal()`, devolve um objeto `sf` com as geometrias agregadas por ULS.

```r
library(ulsportugal)
library(sf)
library(ggplot2)

# 1) Carregar o mapa das 39 ULS
mapa_uls <- ulsportugal()

# 2) Visualização básica
plot(mapa_uls["NOME_CURTO"], main = "ULS de Portugal Continental")

# 3) Exemplo de mapa temático com ggplot2
ggplot(mapa_uls) +
  geom_sf(aes(fill = NOME_CURTO)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Divisão Territorial por Unidade Local de Saúde")
```

## 📊 Estrutura dos dados

A função devolve um `sf` com as seguintes colunas principais:

| Coluna       | Descrição |
|-------------|-----------|
| `NOME_ULS`   | Nome completo e oficial da Unidade Local de Saúde. |
| `NOME_CURTO` | Nome simplificado (ex.: `"S. João"` ou `"Coimbra"`) para legendas. |
| `geometry`   | Geometria (`POLYGON`/`MULTIPOLYGON`) para mapeamento. |

## 📄 Licença

MIT
