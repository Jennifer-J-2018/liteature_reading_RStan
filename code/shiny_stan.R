library(shiny)
library(shinystan)
library(rstan)
library(ggplot2)

# Define the Stan model
stan_model <- stan_model(
  model_code = '
        data {
          int<lower=0> Ny;
          int<lower=0> Nx;
          vector[Ny] y;
          vector[Nx] x;
          real shift_a;                    // Translation parameter
          real<lower=0> prior_sigma;       // Prior standard deviation for mu
          real prior_mu;                   // Prior mean for mu
        }
        
        parameters {
          real mu;
          real<lower=0> sigma;             // Standard deviation parameter
        }
        
        model {
          mu ~ normal(prior_mu + shift_a, prior_sigma/sqrt(Nx));  // Prior for mu
          y ~ normal(mu, sigma/sqrt(Ny));
        }
  '
)

ui <- fluidPage(
  titlePanel("Bayesian Normal Model"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu", "Mean (mu):", min = -10, max = 10, value = 0),
      sliderInput("sigma", "Standard Deviation (sigma):", min = 0.1, max = 5, value = 1),
      sliderInput("prior_mu", "Prior Mean for Mu:", min = -10, max = 10, value = 0),
      sliderInput("shift_a", "Shift parameter a:", min = -1, max = 1, value = 0),
      sliderInput("prior_sigma", "Prior Standard Deviation for Mu:", min = 0.1, max = 5, value = 1),
      numericInput("Ny", "Sample Size (Ny):", value = 100),
      numericInput("Nx", "Sample Size (Nx):", value = 100)
    ),
    mainPanel(
      plotOutput("prior_posterior_plot")
    )
  )
)


# Define the posterior function with input arguments
posterior <- function(data, Ny, Nx, shift_a, prior_mu, prior_sigma) {
  cat("Inside posterior function\n")  # Add this line for debugging
  
  stan_data <- list(
    Ny = Ny,
    Nx = Nx,
    y = data,
    shift_a = shift_a,
    prior_mu = prior_mu,
    prior_sigma = prior_sigma
  )
  
  cat("Before sampling\n")  # Add this line for debugging
  
  stan_fit <- sampling(stan_model, data = stan_data, chains = 1)
  
  cat("After sampling\n")  # Add this line for debugging
  
  posterior_samples <- as.data.frame(stan_fit)
  return(posterior_samples)
}



server <- function(input, output) {
  # Update the plot based on user input
  observe({
    mu <- input$mu
    sigma <- input$sigma
    Ny <- input$Ny  # Use input$Ny to get the sample size
    Nx <- input$Nx  # Use input$Nx to get the sample size
    
    posterior_data <- posterior(rnorm(Ny, mu, sigma), Ny, Nx, input$shift_a, input$prior_mu, input$prior_sigma)
    
    # Extract the prior density from the Stan fit
    prior_density <- as.vector(posterior_data$lp__)  # Assuming lp__ contains the log prior
    
    output$prior_posterior_plot <- renderPlot({
      ggplot(posterior_data, aes(x = mu)) +
        geom_density(color = "blue", size = 2) +
        geom_density(data = data.frame(mu = seq(-10, 10, by = 0.01)), aes(x = mu), 
                     color = "red", linetype = "dashed", size = 1) +
        labs(title = "Posterior and Prior Density for Mu", x = "Mu") +
        theme_minimal()
    })
  })
}


shinyApp(ui = ui, server = server)

