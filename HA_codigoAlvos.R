# ============================================================
# PIPELINE DE EXTRAÇÃO E NORMALIZAÇÃO DE ALVOS DE CONSERVAÇÃO
# ============================================================
#
# Este script realiza a leitura, estruturação e normalização
# de listas de alvos de conservação extraídas de arquivos CSV
# derivados de PDFs institucionais.
#
# O objetivo é transformar um texto originalmente não estruturado
# (com múltiplas UPs e diferentes componentes misturados)
# em um dataframe estruturado em formato longo, contendo:
#
#   UP | Tipo | Conteudo
#
# Onde:
#   UP        → Unidade de Planejamento
#   Tipo      → componente (Fauna, Flora, Fitofisionomias, Ambientes)
#   Conteudo  → alvo individual
#
# O pipeline realiza:
#
# 1) leitura do arquivo bruto
# 2) reconstrução de blocos de texto por UP
# 3) separação dos componentes temáticos
# 4) transformação para formato longo
# 5) limpeza e normalização textual
# 6) aplicação de correções conhecidas
# 7) exportação do resultado final
#
# ============================================================


# ============================================================
# 0) Carregamento de bibliotecas
# ============================================================

# readr  → leitura e escrita eficiente de arquivos
# dplyr  → manipulação de dataframes
# stringr → manipulação de strings com regex
# tidyr  → transformação estrutural de tabelas
# purrr  → programação funcional (não usado diretamente aqui,
#          mas frequentemente necessário em pipelines similares)

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
#library(purrr)

# 1) Ler arquivo
alvos <- read_csv("HA_entradaAlvos.csv", col_names = FALSE, show_col_types = FALSE)

# 2) Garantir nome padrão
colnames(alvos) <- "coluna"

# Neste caso algumas UPs foram combinadas na mesma linha de texto e foi necessário um ajuste para separar

# 3) colapsa tudo em um único texto
texto_unico <- alvos$coluna %>%
  str_squish() %>%
  str_c(collapse = " ")

# 4) regex que captura:
# número seguido de espaço + texto
# até encontrar outro número isolado
padrao <- "(\\b\\d+\\b)\\s+(.*?)(?=\\s+\\d+\\s+(Fauna:|$)|$)"

matches <- str_match_all(texto_unico, padrao)[[1]]

resultado <- tibble(
  ID = matches[,2],
  texto = str_squish(matches[,3])
) %>%
  filter(str_detect(texto, "Fauna:")) %>%  # mantém só blocos válidos
  distinct()

# 5) Dividir em blocos

resultado_blocos <- resultado %>%
  
  # 1️⃣ Padronização preventiva
  mutate(
    texto = texto %>%
      str_replace_all(
        regex("Fitofisionomia:|Fitofisionomias(?!:)", ignore_case = TRUE),
        "Fitofisionomias:"
      ) %>%
      str_replace_all("\\s*-\\s*", "-") #\\s* = qualquer quantidade de espaço (inclusive zero)
  ) %>%
  
  mutate(
    texto = texto %>%
      str_replace_all(
        regex("\\bFlora\\b(?!:)", ignore_case = TRUE),
        "Flora:"
      ) %>%
      str_replace_all("\\s*-\\s*", "-")
  ) %>%
  
  # 2️⃣ Extração dos blocos
  mutate(
    Fauna = str_extract(texto, "Fauna:.*?(?=Flora:|Fitofisionomias:|Ambientes singulares:|$)"),
    
    Flora = str_extract(texto, "Flora:.*?(?=Fauna:|Fitofisionomias:|Ambientes singulares:|$)"),
    
    Fitofisionomias = str_extract(texto, "Fitofisionomias:.*?(?=Fauna:|Flora:|Ambientes singulares:|$)"),
    
    Ambientes = str_extract(texto, "Ambientes singulares:.*?(?=Fauna:|Flora:|Fitofisionomias:|$)")
  ) %>%
  
  # 3️⃣ Remover rótulos
  mutate(
    Fauna = str_remove(Fauna, "^Fauna:\\s*"),
    Flora = str_remove(Flora, "^Flora:\\s*"),
    Fitofisionomias = str_remove(Fitofisionomias, "^Fitofisionomias:\\s*"),
    Ambientes = str_remove(Ambientes, "^Ambientes singulares:\\s*")
  )

# 6) Transformar em formato longo

alvos_long <- resultado_blocos %>%
  select(ID, Fauna, Flora, Fitofisionomias, Ambientes) %>%
  pivot_longer(
    cols = -ID,
    names_to = "Tipo",
    values_to = "Conteudo"
  ) %>%
  filter(!is.na(Conteudo)) %>%
  separate_rows(Conteudo, sep = ",") %>%
  mutate(
    Conteudo = str_trim(Conteudo),
    Conteudo = str_remove(Conteudo, ";+$"),
    Conteudo = str_remove(Conteudo, "\\.+$")  # remove ponto(s) apenas no final
    #\\. → ponto literal
    #+ → um ou mais
    #$ → apenas no final
  ) %>%
  filter(Conteudo != "")

# Checagem

sort(unique(alvos_long$Tipo))
sort(unique(alvos_long$Conteudo))
str(alvos_long)
names(alvos_long)

# 7) Correções

correcoes <- tribble(
  ~Conteudo_antigo, ~Conteudo_novo,
  "Encontro de aguas branca-clara", "Encontro de águas branca-clara",
  "Encontro de aguas branca-preta", "Encontro de águas branca-preta",
  "Encontro de aguas brancapreta", "Encontro de águas branca-preta",
  "Encontro de aguas clara-preta", "Encontro de águas clara-preta",
  "Erythroxylum nelson rosae", "Erythroxylum nelson-rosae",
  "IBAS", "IBAs (Áreas Importantes para a Conservação das Aves e Biodiversidade)",
  "Protium giganteum var", "Protium giganteum var. crassifolium",
  "Rinorea villosiFlora", "Rinorea villosiflora",
  "Tabuleiros", "Tabuleiros (praias e tabuleiros potencialmente utilizados por quelônios)",
  "Picumnus Várzease", "Picumnus varzeae",
  "Pimelodus sterwartii", "Pimelodus stewartii",
  "Vanellus cayanus; ", "Vanellus cayanus",
  "Thryallis parviFlora", "Thryallis parviflora",
  "Pouteria multiFlora", "Pouteria multiflora",
  "Bosques de Várzeas del Cerrado transicionales a la Amazonia", "Bosques de varzea del Cerrado transicionales a la Amazonia",
  "Bosques estacionales deciduos del Cerrado en aFloramientos calcareos", "Bosques estacionales deciduos del Cerrado en afloramientos calcareos",
  "Bosque de los aFloramientos rocosos de la Amazonia Colombiana", "Bosque de los afloramientos rocosos de la Amazonia Colombiana"
)

alvos_long <- alvos_long %>%
  left_join(correcoes, by = c("Conteudo" = "Conteudo_antigo")) %>%
  mutate(Conteudo = coalesce(Conteudo_novo, Conteudo)) %>%
  select(-Conteudo_novo) %>%
  rename(UP = ID)

# 8) Exportar
write_csv(alvos_long, "HA_saidaAlvosProcessado.csv")

