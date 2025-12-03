# app.R
# Shiny app demonstrating Maximum Likelihood Estimation for 5 univariate distributions
# Distributions: Normal (mu, sigma known), Exponential (rate), Poisson (lambda), Bernoulli (p), Uniform(0, theta)

if (!requireNamespace("shiny", quietly = TRUE)) stop("Please install 'shiny'")
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'")

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("MLE Playground: Univariate Distributions"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "dist", "Distribution",
        choices = c(
          "Normal (mu, sigma known)" = "normal",
          "Exponential (rate)" = "exp",
          "Poisson (lambda)" = "pois",
          "Bernoulli (p)" = "bern",
          "Uniform (0, theta)" = "unif"
        ),
        selected = "normal"
      ),
      numericInput("n", "Sample size (n)", value = 100, min = 5, step = 1),
      numericInput("seed", "Random seed", value = 123, min = 1, step = 1),
      hr(),
      conditionalPanel(
        condition = "input.dist == 'normal'",
        numericInput("mu_true", HTML("True <i>\u03BC</i>"), value = 0, step = 0.1),
        numericInput("sigma_known", HTML("Known <i>\u03C3</i> (sd)"), value = 1, min = 0.0001, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.dist == 'exp'",
        numericInput("rate_true", HTML("True rate <i>\u03BB</i>"), value = 1, min = 0.0001, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.dist == 'pois'",
        numericInput("lambda_true", HTML("True <i>\u03BB</i>"), value = 3, min = 0.0001, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.dist == 'bern'",
        sliderInput("p_true", HTML("True <i>p</i>"), min = 0.01, max = 0.99, value = 0.6, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.dist == 'unif'",
        numericInput("theta_true", HTML("True <i>\u03B8</i> (upper bound)"), value = 5, min = 0.01, step = 0.1)
      ),
      hr(),
      checkboxInput("show_grid", "Show parameter grid & numeric optimizer", value = TRUE),
      sliderInput("grid_points", "Grid resolution (for likelihood plot)", min = 50, max = 2000, value = 400, step = 50),
      helpText("Tip: Toggle distributions and parameters, regenerate with the seed for reproducibility."),
      actionButton("resample", "Resample Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Likelihood",
          br(),
          plotOutput("like_plot", height = "350px"),
          br(),
          fluidRow(
            column(6, tableOutput("estimates_table")),
            column(6, verbatimTextOutput("mle_derivation"))
          )
        ),
        tabPanel(
          "Data",
          br(),
          plotOutput("data_plot", height = "300px"),
          tableOutput("data_head")
        ),
        tabPanel(
          "Details",
          br(),
          htmlOutput("details_html")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # -- Reactive: generate sample data ----------------------------------------------------------
  dat <- reactiveVal(NULL)

  observeEvent(list(input$resample, input$dist, input$n, input$seed,
                    input$mu_true, input$sigma_known, input$rate_true,
                    input$lambda_true, input$p_true, input$theta_true), {
                      set.seed(input$seed)
                      n <- input$n

                      x <- switch(input$dist,
                                  normal = rnorm(n, mean = input$mu_true, sd = input$sigma_known),
                                  exp    = rexp(n, rate = input$rate_true),
                                  pois   = rpois(n, lambda = input$lambda_true),
                                  bern   = rbinom(n, size = 1, prob = input$p_true),
                                  unif   = runif(n, min = 0, max = input$theta_true)
                      )
                      dat(x)
                    }, ignoreInit = FALSE)

  # -- Log-likelihood functions and analytic MLEs ---------------------------------------------
  ll_fun <- reactive({
    x <- dat()
    if (is.null(x)) return(NULL)

    switch(input$dist,
           normal = list(
             loglik = function(mu) sum(dnorm(x, mean = mu, sd = input$sigma_known, log = TRUE)),
             mle = mean(x),
             param_name = expression(mu),
             param_label = "mu",
             domain = c(mean(x) - 5*input$sigma_known, mean(x) + 5*input$sigma_known)
           ),
           exp = list(
             loglik = function(lambda) ifelse(lambda > 0, length(x) * log(lambda) - lambda * sum(x), -Inf),
             mle = length(x) / sum(x),
             param_name = expression(lambda),
             param_label = "lambda",
             domain = c(max(1e-6, (length(x) / sum(x)) / 50), (length(x) / sum(x)) * 5 + 1e-6)
           ),
           pois = list(
             loglik = function(lambda) ifelse(lambda > 0, sum(dpois(x, lambda = lambda, log = TRUE)), -Inf),
             mle = mean(x),
             param_name = expression(lambda),
             param_label = "lambda",
             domain = c(max(1e-6, mean(x) / 50), max(5 * mean(x) + 1, mean(x) + 2))
           ),
           bern = list(
             loglik = function(p) ifelse(p > 0 & p < 1, sum(dbinom(x, size = 1, prob
