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

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# ============================================================
# 1) Leitura do arquivo bruto
# ============================================================

# O arquivo de entrada possui apenas uma coluna contendo
# linhas de texto extraídas do PDF.
alvos <- read_csv("HA_entradaAlvos.csv", col_names = FALSE, show_col_types = FALSE)

# Padroniza o nome da coluna para facilitar manipulações posteriores
colnames(alvos) <- "coluna"

# ============================================================
# 2) Reconstrução do texto original
# ============================================================

# Em alguns casos, durante a extração do PDF,
# múltiplas Unidades de Planejamento (UPs) acabam
# sendo fragmentadas ou combinadas em linhas diferentes.
#
# Para permitir uma extração robusta baseada em padrões,
# todo o texto é primeiro colapsado em uma única string.

texto_unico <- alvos$coluna %>%
  str_squish() %>%        # remove espaços duplicados
  str_c(collapse = " ")   # concatena todas as linhas

# ============================================================
# 3) Identificação dos blocos de UP
# ============================================================

# A expressão regular abaixo identifica blocos que seguem o padrão:
#
#    número_da_UP + texto associado
#
# O bloco termina quando aparece:
#
#    outro número isolado indicando nova UP
#
# Estrutura do padrão:
#
# (\\b\\d+\\b)      → captura o número da UP
# \\s+              → espaço
# (.*?)             → captura o texto associado
# (?= ...)          → lookahead que detecta o início do próximo bloco

padrao <- "(\\b\\d+\\b)\\s+(.*?)(?=\\s+\\d+\\s+(Fauna:|$)|$)"

matches <- str_match_all(texto_unico, padrao)[[1]]

# Constrói um dataframe contendo:
#
# ID    → número da UP
# texto → bloco de texto correspondente

resultado <- tibble(
  ID = matches[,2],
  texto = str_squish(matches[,3])
) %>%
  # Mantém apenas blocos que realmente possuem seção Fauna
  # (heurística para evitar capturas inválidas)
  filter(str_detect(texto, "Fauna:")) %>%  # mantém só blocos válidos
  # Remove duplicatas eventualmente geradas pelo parsing
  distinct()

# ============================================================
# 4) Extração dos componentes temáticos
# ============================================================

# Cada bloco de texto contém subseções com rótulos como:
#
# Fauna:
# Flora:
# Fitofisionomias:
# Ambientes singulares:
#
# Esta etapa padroniza os rótulos e extrai o conteúdo de cada um.

resultado_blocos <- resultado %>%
  
# ----------------------------------------------------------
# 4.1 Padronização preventiva de rótulos
# ----------------------------------------------------------
  mutate(
    texto = texto %>%
      # Normaliza variações de "Fitofisionomias"
      str_replace_all(
        regex("Fitofisionomia:|Fitofisionomias(?!:)", ignore_case = TRUE),
        "Fitofisionomias:"
      ) %>%
      # Padroniza uso de hífens
      str_replace_all("\\s*-\\s*", "-") #\\s* = qualquer quantidade de espaço (inclusive zero)
  ) %>%
  
  mutate(
    texto = texto %>%
      # Garante que "Flora" sempre possua dois pontos, sempre que for uma palavra isolada
      str_replace_all(
        regex("\\bFlora\\b(?!:)", ignore_case = TRUE),
        "Flora:"
      ) %>%
      str_replace_all("\\s*-\\s*", "-")
  ) %>%
# ----------------------------------------------------------
# 4.2 Extração dos blocos por componente
# ----------------------------------------------------------
  mutate(
    Fauna = str_extract(texto, "Fauna:.*?(?=Flora:|Fitofisionomias:|Ambientes singulares:|$)"),
    
    Flora = str_extract(texto, "Flora:.*?(?=Fauna:|Fitofisionomias:|Ambientes singulares:|$)"),
    
    Fitofisionomias = str_extract(texto, "Fitofisionomias:.*?(?=Fauna:|Flora:|Ambientes singulares:|$)"),
    
    Ambientes = str_extract(texto, "Ambientes singulares:.*?(?=Fauna:|Flora:|Fitofisionomias:|$)")
  ) %>%
  
# ----------------------------------------------------------
# 4.3 Remoção dos rótulos
# ----------------------------------------------------------

  mutate(
    Fauna = str_remove(Fauna, "^Fauna:\\s*"),
    Flora = str_remove(Flora, "^Flora:\\s*"),
    Fitofisionomias = str_remove(Fitofisionomias, "^Fitofisionomias:\\s*"),
    Ambientes = str_remove(Ambientes, "^Ambientes singulares:\\s*")
  )

# ============================================================
# 5) Transformação para formato longo
# ============================================================

# Converte a estrutura de colunas em formato longo.
#
# Exemplo:
#
# ID | Fauna | Flora
#
# torna-se:
#
# ID | Tipo | Conteudo

alvos_long <- resultado_blocos %>%
  select(ID, Fauna, Flora, Fitofisionomias, Ambientes) %>%
  pivot_longer(
    cols = -ID,
    names_to = "Tipo",
    values_to = "Conteudo"
  ) %>%
  # Remove registros vazios
  filter(!is.na(Conteudo)) %>%
  
# ----------------------------------------------------------
# 5.1 Separação de múltiplos alvos
# ----------------------------------------------------------

# Muitos registros possuem múltiplos alvos separados por vírgula

  separate_rows(Conteudo, sep = ",") %>%
  
# ----------------------------------------------------------
# 5.2 Limpeza textual
# ----------------------------------------------------------

  mutate(
    Conteudo = str_trim(Conteudo),
    # remove ponto e vírgula no final
    Conteudo = str_remove(Conteudo, ";+$"),
    # remove pontos finais duplicados
    Conteudo = str_remove(Conteudo, "\\.+$")  # remove ponto(s) apenas no final
    #\\. → ponto literal
    #+ → um ou mais
    #$ → apenas no final
  ) %>%
  filter(Conteudo != "")

# ============================================================
# 6) Checagens exploratórias
# ============================================================

# Extração para checagem de alvos por Grupo
# Fauna
alvos_fauna <- alvos_long |>
  filter(Tipo == "Fauna") |>
  pull(Conteudo) |>
  unique() |>
  sort()
alvos_fauna
# Flora
alvos_flora <- alvos_long |>
  filter(Tipo == "Flora") |>
  pull(Conteudo) |>
  unique() |>
  sort()
alvos_flora
# Ambientes Singulares
alvos_ambientes <- alvos_long |>
  filter(Tipo == "Ambientes") |>
  pull(Conteudo) |>
  unique() |>
  sort()
alvos_ambientes
# Fitofisionomias
alvos_fitofisionomias <- alvos_long |>
  filter(Tipo == "Fitofisionomias") |>
  pull(Conteudo) |>
  unique() |>
  sort()
alvos_fitofisionomias

# Exibe todos os tipos identificados

sort(unique(alvos_long$Tipo))

# Exibe todos os alvos únicos

sort(unique(alvos_long$Conteudo))

# Estrutura do dataframe

str(alvos_long)

# Nome das colunas

names(alvos_long)

# ============================================================
# 7) Correções manuais conhecidas
# ============================================================

# Algumas inconsistências presentes no material original
# exigem correções específicas.
#
# Estas correções incluem:
#
# • erros de digitação
# • problemas de encoding
# • nomes científicos quebrados
# • rótulos institucionais incompletos

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

# Aplica as correções ao dataframe principal

alvos_long <- alvos_long %>%
  left_join(correcoes, by = c("Conteudo" = "Conteudo_antigo")) %>%
  mutate(Conteudo = coalesce(Conteudo_novo, Conteudo)) %>%
  select(-Conteudo_novo) %>%
  rename(UP = ID)

# ============================================================
# 8) Exportação do resultado final
# ============================================================

# O arquivo final contém:
#
# UP | Tipo | Conteudo
#
# Cada linha representa um alvo individual associado
# a uma Unidade de Planejamento e a um componente temático.

alvos_Integracao <- alvos_long %>%
  select(Tipo,
         Conteudo
  ) %>%
  distinct(Conteudo, .keep_all = TRUE) %>%
  rename(
    COMPONENTE = Tipo,
    ALVO = Conteudo
  )

write_csv(alvos_Integracao, "alvos_Processados_HA.csv")

write_csv(alvos_long, "HA_saidaAlvosProcessado.csv")

