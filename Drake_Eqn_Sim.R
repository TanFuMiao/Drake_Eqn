#####
#Drake's equation simulation
#####

#####
#R the star formation rate.
#fp the fraction of stars with planetary systems.
#Ne the number of habitable planets in a planetary system.
#fl the fraction of the habitable planets that developed life.
#fi the fraction of planets with life form that developed intelligence.
#fc the fraction of civilizations that perform interstellar communications.
#L length of time for civilizations to transmit detectable signals in space.
#####

library(shiny)
library(dplyr)
library(ggplot2)

# Define the UI
ui = fluidPage(
  titlePanel("Interactive Drake Equation Histogram"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("R_mean", "R Mean (Stars Formation Rate):", min = 1, max = 10, value = 5, step = 0.1),
      sliderInput("R_sd", "R SD:", min = 0.1, max = 5, value = 1, step = 0.1),
      
      #sliderInput("fp_min", "fp Min:", min = 0, max = 1, value = 0),
      #sliderInput("fp_max", "fp Max:", min = 0, max = 1, value = 1),
      
      sliderInput("Ne_mean", "Ne Mean (Planets/System):", min = 1, max = 10, value = 5, step = 0.1),
      sliderInput("Ne_sd", "Ne SD:", min = 0.1, max = 5, value = 1, step = 0.1),
      
      #sliderInput("f1_min", "f1 Min:", min = 0, max = 1, value = 0),
      #sliderInput("f1_max", "f1 Max:", min = 0, max = 1, value = 1),
      
      #sliderInput("fi_min", "fi Min:", min = 0, max = 1, value = 0),
      #sliderInput("fi_max", "fi Max:", min = 0, max = 1, value = 1),
      
      #sliderInput("fc_min", "fc Min:", min = 0, max = 1, value = 0),
      #sliderInput("fc_max", "fc Max:", min = 0, max = 1, value = 1),
      
      sliderInput("L_mean", "L Mean (Years):", min = 100, max = 2000, value = 1000, step = 10),
      sliderInput("L_sd", "L SD:", min = 1, max = 100, value = 1, step = 1)
    ),
    
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

# Define the server logic
server = function(input, output) {
  
  output$histPlot = renderPlot({
    
    #create data frame and calculate N
    drake.df = data.frame(
      R = rnorm(10000, mean = input$R_mean, sd = input$R_sd),
      fp = runif(10000, min = 0, max = 1),
      Ne = rnorm(10000, mean = input$Ne_mean, sd = input$Ne_sd),
      f1 = runif(10000, min = 0, max = 1),
      fi = runif(10000, min = 0, max = 1),
      fc = runif(10000, min = 0, max = 1),
      L = rnorm(10000, mean = input$L_mean, sd = input$L_sd)
    )
    
    drake.df = drake.df %>%
      mutate(N = R * fp * Ne * f1 * fi * fc * L)
    
    # Plot the histogram
    ggplot(drake.df, aes(x = N)) +
      geom_histogram(bins = 50, fill = "skyblue", color = "black", alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Distribution of N",
        x = "N",
        y = "Frequency"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
