# Pressupostos da regressão linear

library(tidyverse)
library(openintro)
library(broom)
library(lmtest)
library(ISLR)
library(patchwork)
library(performance)
library(olsrr)
library(car)
library(palmerpenguins)
theme_set(theme_minimal())

## Dados

auto <- Auto |>
  as_tibble()

## Linearidade
### Banco de dados
ISLR::Auto

#### Modelo

p_auto <- auto |>
  ggplot(aes(horsepower, mpg)) +
  geom_point() +
  theme_minimal()

p_auto

p_auto_linha_de_regressao <- p_auto +
  geom_smooth(
    color = "red",
    method = "lm",
    se = F
  )

p_auto_linha_de_regressao

modelo_auto <- lm(mpg ~ horsepower, auto)
summary(modelo_auto)

##### Análise dos resíduos

resid_auto_linear <- modelo_auto |>
  augment() |>
  ggplot(aes(.fitted, .resid)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(color = "red") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "blue"
  ) +
  theme_minimal()

resid_auto_linear

##### Testes para avaliar a lienaridade

# H0: a relação é linear
# H1: a relação não é linear

harvtest(modelo_auto)
raintest(modelo_auto)
resettest(modelo_auto)


#### Modelo quadrático

modelo_auto <- lm(mpg ~ horsepower, auto)
summary(modelo_auto)

modelo_auto_quadratico <- lm(mpg ~ horsepower + I(horsepower^2), auto)
summary(modelo_auto_quadratico)

# R^2 aumentou
# Erro residual diminuiu

# O que muda ao incluir o Termo Quadrático (X^2)
# A regressão continua sendo linear (estimação dos beta). O R calcula o quadrado da variável original e o insere no modelo como se fosse uma nova variável independente, trabalhando com as duas forças simultaneamente.
# A linha de previsão "dobra" e ganha o formato de uma parábola (curva em "U" ou "U invertido"). Isso resolve o erro da reta rígida, permitindo que o modelo capture saturação, aceleração ou inversões de tendência.
# O impacto da variável deixa de ser constante. Em uma reta, +1 unidade de X sempre altera $Y$ na mesma proporção. No modelo quadrático, o efeito de +1 unidade depende de onde você já está no eixo

p_auto_linha_de_regressao

auto |>
  ggplot(aes(x = horsepower, y = mpg)) +
  geom_point(
    alpha = 0.1
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x^2),
    color = "red",
    linewidth = 1.2,
    se = FALSE
  ) +
  theme_minimal()

##### Resíduos

resid_auto_quad <- modelo_auto_quadratico |>
  augment() |>
  ggplot(aes(.fitted, .resid)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(color = "red") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "blue"
  ) +
  theme_classic()

##### Comparação dos gráficos de resíduos do modelo

resid_auto_linear + resid_auto_quad

#### Comparação entre os modelos

# `anova()`` testa se a redução na Soma dos Quadrados dos Resíduos (RSS) gerada pelo modelo mais complexo é grande o suficiente para justificar o gasto de mais um grau de liberdade (a inclusão de X^2)

# H1: o modelo mais complexo é melhor ajustado aos dados

anova(modelo_auto, modelo_auto_quadratico)


## Modelo com relação linear

### Dados
altura_peso <- read.table(
  "https://www.mv.helsinki.fi/home/mjxpirin/HDS_course/material/Davis_height_weight.txt",
  as.is = TRUE,
  header = TRUE
) |>
  as_tibble()

modelo_altura_peso <- lm(weight ~ height, altura_peso)

modelo_altura_peso |>
  augment() |>
  ggplot(aes(.fitted, .resid)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(color = "red") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "blue"
  ) +
  theme_classic()

harvtest(modelo_altura_peso)
raintest(modelo_altura_peso)
resettest(modelo_altura_peso)

## Ausência de multicolinearidade

# A multicolinearidade ocorre quando duas ou mais variáveis preditoras (X)
# são altamente correlacionadas entre si.
# O modelo linear assume que podemos interpretar
# o coeficiente de uma variável "mantendo as outras constantes".
# Quando as variáveis variam juntas na vida real, essa premissa matemática falha,
# gerando estimativas instáveis.

### Banco de Dados
ISLR::Credit

credit <- Credit |> 
  as_tibble()

m_limit <- lm(Balance ~ Limit, data = Credit)
summary(m_limit)
m_rating <- lm(Balance ~ Rating, data = Credit)
summary(m_rating)
m_multi <- lm(Balance ~ Limit + Rating, data = Credit)
summary(m_multi)

cor(credit$Limit, credit$Rating)
