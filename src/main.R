# Carregar pacotes necessários
setwd("../assets")


pacotes <- c("ggplot2", "dplyr", "lubridate", "ggplot2", "plotly")
sapply(pacotes, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
})


# Carregar dados
dados <- read.csv("ralie-usina.csv")

# Inspecionar os dados
head(dados)
summary(dados)

# Limpar e preparar os dados
dados$MdaPotenciaOutorgadaKw <- as.numeric(gsub(",", ".", dados$MdaPotenciaOutorgadaKw))
dados$DatGeracaoConjuntoDados <- as.Date(dados$DatGeracaoConjuntoDados, format = "%Y-%m-%d")
dados$DatRalie <- as.Date(dados$DatRalie, format = "%Y-%m-%d")

mean_potencia <- mean(dados$MdaPotenciaOutorgadaKw, na.rm = TRUE)
median_potencia <- median(dados$MdaPotenciaOutorgadaKw, na.rm = TRUE)
sd_potencia <- sd(dados$MdaPotenciaOutorgadaKw, na.rm = TRUE)



resumo_potencia <- summary(dados$MdaPotenciaOutorgadaKw)
print(resumo_potencia)


# Imprimir as estatísticas
cat("Média da Potência Outorgada (kW):", mean_potencia, "\n")
cat("Mediana da Potência Outorgada (kW):", median_potencia, "\n")
cat("Desvio Padrão da Potência Outorgada (kW):", sd_potencia, "\n")



# Distribuição de tipos de geração
tabela_geracao <- table(dados$SigTipoGeracao)
print(tabela_geracao)


# Tema personalizado para melhorar a estética dos gráficos
tema_personalizado <- theme_minimal(base_size = 15) +
                      theme(plot.title = element_text(face = "bold", hjust = 0.5),
                            axis.title.x = element_text(face = "bold"),
                            axis.title.y = element_text(face = "bold"))

# Visualização: Gráfico de Barras para Tipo de Geração
ggplot(dados, aes(x = SigTipoGeracao, fill = SigTipoGeracao)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") + # Usando uma paleta de cores agradável
  labs(title = "Distribuição por Tipo de Geração",
       x = "Tipo de Geração",
       y = "Contagem",
       fill = "Tipo de Geração") +
  tema_personalizado

# Visualização: Histograma da Potência Outorgada
ggplot(dados, aes(x = MdaPotenciaOutorgadaKw)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribuição da Potência Outorgada",
       x = "Potência Outorgada (kW)",
       y = "Frequência") +
  tema_personalizado

# Visualização: Boxplot da Potência por Tipo de Geração
ggplot(dados, aes(x = SigTipoGeracao, y = MdaPotenciaOutorgadaKw, fill = SigTipoGeracao)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") + # Paleta de cores diferente para variedade
  labs(title = "Dispersão da Potência por Tipo de Geração",
       x = "Tipo de Geração",
       y = "Potência Outorgada (kW)",
       fill = "Tipo de Geração") +
  coord_flip() + # Inverter coordenadas para melhor leitura
  tema_personalizado



# Visualização da linha do tempo das obras (Gantt Chart)
dados_filtrados <- dados %>%
  filter(!is.na(DatInicioObraOutorgado) & !is.na(DatInicioObraRealizado)) %>%
  mutate(
    DatInicioObraOutorgado = as.Date(DatInicioObraOutorgado, format = "%Y-%m-%d"),
    DatInicioObraRealizado = as.Date(DatInicioObraRealizado, format = "%Y-%m-%d")
  )

ggplot(dados_filtrados, aes(x = DatInicioObraOutorgado, xend = DatInicioObraRealizado, y = NomEmpreendimento)) +
  geom_segment(aes(yend = NomEmpreendimento), color = "blue") +
  labs(title = "Linha do Tempo do Início das Obras", x = "Data", y = "Empreendimento") +
  theme_minimal()


# Distribuição geográfica das usinas
ggplot(dados, aes(x = SigUFPrincipal, fill = SigUFPrincipal)) +
  geom_bar() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Distribuição Geográfica das Usinas", x = "Estado", y = "Número de Usinas") +
  theme_minimal()


  # Viabilidade e situação das obras
tabela_viabilidade <- table(dados$DscViabilidade, dados$DscSituacaoObra)
print(tabela_viabilidade)

# Visualização da situação das obras
ggplot(dados, aes(x = DscSituacaoObra, fill = DscViabilidade)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Viabilidade vs. Situação das Obras", x = "Situação da Obra", y = "Proporção") +
  theme_minimal()


  # Análise de potência por tipo de conexão
ggplot(dados, aes(x = DscTipoConexao, y = MdaPotenciaOutorgadaKw, fill = DscTipoConexao)) +
  geom_boxplot() +
  labs(title = "Potência Outorgada por Tipo de Conexão", x = "Tipo de Conexão", y = "Potência (kW)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


  # Licenciamento ambiental vs. andamento das obras
licenciamento_vs_andamento <- table(dados$DscSituacaoLP, dados$DscSituacaoObra)
print(licenciamento_vs_andamento)

# Visualização do licenciamento ambiental
ggplot(dados, aes(x = DscSituacaoLP, fill = DscSituacaoObra)) +
  geom_bar(position = "fill") +
  labs(title = "Impacto do Licenciamento Ambiental", x = "Situação da LP", y = "Proporção") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()


  # Regime de participação e comercialização
tabela_regime_comercializacao <- table(dados$DscPropriRegimePariticipacao, dados$DscComercializacaoEnergia)
print(tabela_regime_comercializacao)

# Visualização do regime de participação
ggplot(dados, aes(x = DscComercializacaoEnergia, fill = DscPropriRegimePariticipacao)) +
  geom_bar(position = "fill") +
  labs(title = "Regime de Participação vs. Comercialização", x = "Comercialização de Energia", y = "Proporção") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()


  # Análise de justificativas de atraso
tabela_justificativa_atraso <- table(dados$DscJustificativaPrevisao)
print(tabela_justificativa_atraso)

# Visualização das justificativas de atraso
ggplot(dados, aes(x = DscJustificativaPrevisao)) +
  geom_bar(fill = "tomato") +
  labs(title = "Distribuição das Justificativas de Atraso", x = "Justificativa", y = "Contagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



  # Gráfico interativo de potência por tipo de geração
grafico_interativo <- ggplot(dados, aes(x = SigTipoGeracao, y = MdaPotenciaOutorgadaKw, color = SigTipoGeracao)) +
  geom_jitter(alpha = 0.6) +
  labs(title = "Potência por Tipo de Geração (Interativo)", x = "Tipo de Geração", y = "Potência (kW)") +
  theme_minimal()

# Tornar o gráfico interativo
plotly::ggplotly(grafico_interativo)