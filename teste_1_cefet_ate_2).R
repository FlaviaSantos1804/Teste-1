#carregando o pacote dplyr
library(dplyr)

#importando os dados para dentro do R
dados = read.csv(file = 'wine.data',header = FALSE)

#nomeando as colunas da nossa base de dados
colnames(dados) = c('type',"Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

# 1) a. calculando a media de todos os atributos
media =  colMeans(dados)
media

# 1) a. calculando o desvio padrao de todos os atributos
desvio = sapply(dados,sd)
desvio

# 1) b. calculando a media por tipo de vinho, para todos os atributos

df_mean = dados %>%
  group_by(type) %>%
  summarize_all(mean)
df_mean

# 1) b. calculando o desvio padrao por tipo de vinho, para todos os atributos

df_sd = dados %>%
  group_by(type) %>%
  summarize_all(sd)
df_sd


# 1) c. criando um grafico de distribuição de densidade para cada atributo, por tipo de vinho

library(ggplot2)
library(tidyr)
library(purrr)
library(tidyverse)



# Agrupando o dataframe pela coluna type

df_grouped <- dados %>%
  group_by(type)

# Transformando as colunas restantes em uma única coluna

df_long <- df_grouped %>%
  pivot_longer(cols = c("Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline"), names_to = "coluna", values_to = "valor")

# Fazendo gráficos de densidade para cada coluna agrupada pela coluna type

ggplot(df_long, aes(x = valor, fill = factor(type))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ coluna, scales = "free") +
  ggtitle("Gráficos de densidades por tipo de vinho")


# 1) d. criando um grafico de box-plot para cada atributo, por tipo de vinho


# Agrupando o dataframe pela coluna type

df_grouped <- dados %>%
  group_by(type)

# Transformando as colunas restantes em uma única coluna

df_long <- df_grouped %>%
  pivot_longer(cols = c("Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline"), names_to = "coluna", values_to = "valor")

# Gerando os boxplots

ggplot(df_long, aes(x = factor(type), y = valor, fill = factor(type))) +
  geom_boxplot() +
  facet_wrap(~ coluna, scales = "free") +
  ggtitle("Boxplots agrupados por tipo")


# 1) e. criando um grafico de dispersão para cada atributo, por tipo de vinho

# Agrupando o dataframe pela coluna type

df_grouped <- dados %>%
  group_by(type)

# Transformando as colunas restantes em uma única coluna

df_long <- df_grouped %>%
  pivot_longer(cols = c("Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline"), names_to = "coluna", values_to = "valor")

# Fazendo gráficos de dispersão para cada coluna agrupada pela coluna type

ggplot(df_long, aes(x = factor(type), y = valor, color = factor(type))) +
  geom_point() +
  facet_wrap(~ coluna, scales = "free") +
  labs(title = "Gráfico de Dispersão")




# 2) a.


# Identificando as colunas numéricas
colunas_numericas <- sapply(dados, is.numeric)

# Iterando sobre as colunas numéricas do conjunto de dados
for (coluna in names(dados)[colunas_numericas]) {
  
  # Calculando os valores mínimo e máximo da coluna
  valor_minimo <- min(dados[[coluna]])
  valor_maximo <- max(dados[[coluna]])
  
  # Definindo o número de faixas
  num_faixas <- 3
  
  # Calculando o intervalo de cada faixa
  intervalo <- (valor_maximo - valor_minimo) / num_faixas
  
  # Criando as faixas para cada valor do atributo
  dados[[coluna]] <- cut(dados[[coluna]], breaks = c(-Inf, valor_minimo + intervalo, valor_maximo - intervalo, Inf),
                         labels = c("baixo", "médio", "alto"), include.lowest = TRUE)
  
  # Convertendo o atributo para fator
  dados[[coluna]] <- as.factor(dados[[coluna]])
}

# Exibindo o conjunto de dados após a discretização
print(dados)




# 2) b.


# Convertendo o atributo "type" (tipo de vinho) para mapeamento categórico
type_categorico <- factor(dados$type)

# Exibindo os níveis do mapeamento categórico
levels(type_categorico)



