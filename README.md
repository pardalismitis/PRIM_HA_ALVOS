---
editor_options: 
  markdown: 
    wrap: 120
---

# PRIM_HA_ALVOS

Estruturação automatizada dos **alvos de conservação** presentes no Plano de Redução de Impactos de Hidrelétricas sobre
a Biodiversidade na Amazônia - PRIM-HA.

Este repositório contém um pipeline em R desenvolvido para extrair, padronizar e organizar os alvos de conservação
(Fauna, Flora, Fitofisionomias e Ambientes Singulares) a partir dos arquivos brutos disponíveis no Material Suplementar
do PRIM HA, transformando o conteúdo textual em uma base de dados estruturada.

O objetivo é permitir análise, revisão técnica e integração com outras bases utilizadas nas análises da
COESP/CGCON/DIBIO/ICMBio e possíveis interessados no uso dos dados.

------------------------------------------------------------------------------------------------------------------------

## Contexto

Nos arquivos originais, os alvos de conservação aparecem como blocos de texto associados a cada **Unidade de
Planejamento (UP)**.

Esses blocos possuem características que podem dificultar análise direta:

-   texto contínuo
-   separadores inconsistentes
-   pequenas variações de grafia

Este projeto resolve esse problema criando um **pipeline reprodutível** que transforma os dados em uma base tabular
limpa.

------------------------------------------------------------------------------------------------------------------------

## Fluxo de Trabalho

O script executa as seguintes etapas principais:

-   Leitura do arquivo de entrada

-   Reconstrução dos blocos de texto por UP

-   Extração das categorias de alvos

-   Separação de múltiplos alvos em registros individuais

-   Limpeza e normalização textual

-   Aplicação de correções padronizadas

-   Exportação do dataset processado

Fluxo simplificado:
```         
CSV bruto
   ↓
Reconstrução de blocos por UP
   ↓
Extração de componentes
   ↓
Separação de alvos individuais
   ↓
Limpeza textual
   ↓
Correções padronizadas
   ↓
Dataset estruturado
```

------------------------------------------------------------------------------------------------------------------------

## Disponibilidade dos arquivos de dados

Os arquivos .csv utilizados como entrada no processamento não estão
incluídos neste repositório.

Isso ocorre principalmente por dois motivos:

Tamanho dos arquivos – os arquivos originais possuem grande volume de
dados, o que ultrapassa ou se aproxima dos limites recomendados para
versionamento em repositórios Git.

Boas práticas de versionamento – repositórios de código devem priorizar
scripts, funções e documentação, evitando incluir grandes bases de dados
que podem dificultar o clone, aumentar o histórico do repositório e
comprometer a performance do Git.

Assim, este repositório contém apenas o código necessário para processar
os dados, assumindo que os arquivos de entrada estejam disponíveis
localmente no diretório principal do repositório.

Pesquisadores ou equipes que necessitem acessar os arquivos de dados
utilizados neste PRIM podem solicitá-los diretamente à Coordenação
de Análises Geoespaciais para Conservação de Espécies (COESP/ICMBio).

Após obter os arquivos, basta colocá-los no respectivo diretório para
que o script possa ser executado normalmente.
