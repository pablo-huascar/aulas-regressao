library(shiny)
library(tidyverse)
library(splines)
library(ISLR)

dados_ <- Auto %>% select(horsepower, mpg)

ui <- fluidPage(
  titlePanel("Simulador de Splines Naturais (ns)"),
  
  fluidRow(
    column(12,
      wellPanel(
        fluidRow(
          column(6,
            sliderInput("df_val", "Graus de Liberdade (df):",
                        min = 1, max = 10, value = 3,
                        width = "100%"),
            helpText("A função ns() posiciona os nós automaticamente nos quantis dos dados.")
          ),
          column(6,
            tableOutput("knotTable")
          )
        )
      )
    )
  ),
  
  fluidRow(
    column(12,
      plotOutput("splinePlot", height = "650px")
    )
  )
)

server <- function(input, output) {
  
  output$splinePlot <- renderPlot({
    if (input$df_val == 1) {
      modelo <- lm(mpg ~ horsepower, data = dados_)
      knots  <- NULL
    } else {
      modelo <- lm(mpg ~ ns(horsepower, df = input$df_val), data = dados_)
      spline_obj <- ns(dados_$horsepower, df = input$df_val)
      knots <- attr(spline_obj, "knots")
    }
    
    hp_range <- range(dados_$horsepower)
    x_seq <- tibble(horsepower = seq(hp_range[1], hp_range[2], length.out = 200))
    preds  <- predict(modelo, newdata = x_seq, interval = "confidence")
    x_seq  <- bind_cols(x_seq, as_tibble(preds))
    
    p <- ggplot(dados_, aes(x = horsepower, y = mpg)) +
      geom_point(alpha = 0.15, color = "gray40", size = 1.5) +
      geom_ribbon(data = x_seq,
                  aes(y = fit, ymin = lwr, ymax = upr),
                  fill = "steelblue", alpha = 0.25) +
      geom_line(data = x_seq, aes(y = fit),
                color = "steelblue", linewidth = 1.8) +
      theme_minimal(base_size = 15) +
      labs(
        title    = paste("Modelo: ns(horsepower, df =", input$df_val, ")"),
        subtitle = paste("Número de nós internos:", ifelse(is.null(knots), 0, length(knots))),
        x = "Potência (horsepower)",
        y = "Consumo (mpg)"
      )
    
    if (!is.null(knots)) {
      p <- p + geom_vline(xintercept = knots,
                          linetype = "dashed", color = "red", alpha = 0.7,
                          linewidth = 0.8)
    }
    
    p
  })
  
  output$knotTable <- renderTable({
    if (input$df_val > 1) {
      spline_obj <- ns(dados_$horsepower, df = input$df_val)
      knots <- attr(spline_obj, "knots")
      tibble(Nó = paste("Nó", seq_along(knots)),
             `Localização (HP)` = as.vector(knots))
    }
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
}

shinyApp(ui = ui, server = server)