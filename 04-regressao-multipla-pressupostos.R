# Pressupostos da regressão linear






### Banco de dados
ISLR::Auto

#### Modelo





##### Análise dos resíduos







#### Modelo quadrático



de 


##### Resíduos



##### Comparação dos gráficos de resíduos do modelo






## Modelo com relação linear

### Dados
altura_peso <- read.table(
  "https://www.mv.helsinki.fi/home/mjxpirin/HDS_course/material/Davis_height_weight.txt",
  as.is = TRUE,
  header = TRUE
) |>
  as_tibble()


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

performance::check_collinearity(m_multi)

y = a + bx1 + cx2

cor(credit$Limit, credit$Rating)
