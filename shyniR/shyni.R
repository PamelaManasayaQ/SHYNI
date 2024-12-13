library(shiny)
library(rsconnect)

rsconnect::setAccountInfo(name='pamelamanasaya',
                          token='396A79BCBE792815E45C997CBF396753',
                          secret='14A0Xha9HRMxW42+scqS7xd5kH9+b2D73g1LZJhm')


# UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel(
    div(
      tags$img(src = "unap.png", height = "60px", style = "margin-right: 15px;"),
      "Descenso de Gradiente - Shiny App"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Parámetros del Descenso de Gradiente"),
      numericInput("learning_rate", "Tasa de Aprendizaje (α):", value = 0.1, min = 0.001, max = 1, step = 0.01),
      numericInput("initial_x", "Valor inicial de x:", value = 0, min = -10, max = 10, step = 0.1),
      numericInput("iterations", "Número de Iteraciones:", value = 20, min = 1, max = 100, step = 1),
      actionButton("run", "Ejecutar"),
      br(),
      p("Modifica los valores para observar cómo afecta el descenso de gradiente."),
      hr(),
      p("Estudiante: Pamela Manasaya")
    ),
    
    mainPanel(
      h3("Resultados"),
      # Mostrar la ecuación f(x) = (x - 3)^2 + 2
      textOutput("equation"),
      plotOutput("gradientPlot", height = "400px"),
      h4("Tabla de Iteraciones"),
      tableOutput("gradientSteps"),
      br(),
      tags$img(src = "Cuadritos.jpg", height = "80px", style = "display: block; margin: auto;")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$run, {
    # Función objetivo: f(x) = (x - 3)^2 + 2
    f <- function(x) (x - 3)^2 + 2
    grad <- function(x) 2 * (x - 3)  # Derivada de f(x)
    
    # Inicializar valores
    x <- input$initial_x
    learning_rate <- input$learning_rate
    iterations <- input$iterations
    steps <- data.frame(Iteración = numeric(0), x = numeric(0), f_x = numeric(0))
    
    # Algoritmo de descenso de gradiente
    for (i in 1:iterations) {
      fx <- f(x)
      steps <- rbind(steps, data.frame(Iteración = i, x = x, f_x = fx))
      x <- x - learning_rate * grad(x)  # Actualizar x
    }
    
    # Tabla de resultados
    output$gradientSteps <- renderTable({
      steps
    })
    
    # Gráfico de la función y puntos del gradiente
    output$gradientPlot <- renderPlot({
      curve(f, from = -10, to = 10, col = "blue", lwd = 2, xlab = "x", ylab = "f(x)", main = "Descenso de Gradiente")
      points(steps$x, steps$f_x, col = "red", pch = 19, cex = 1.5)
      lines(steps$x, steps$f_x, col = "red", lwd = 2)
      legend("topright", legend = c("f(x)", "Puntos del gradiente"), col = c("blue", "red"), lty = c(1, 1), pch = c(NA, 19))
    })
    
    # Mostrar la ecuación en la interfaz
    output$equation <- renderText({
      "Ecuación trabajada: f(x) = (x - 3)^2 + 2"
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

