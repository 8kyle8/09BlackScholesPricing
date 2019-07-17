library(shiny)
library(plotly)

server <- function(input, output) {
  
  # Theoretical Price Calculations; Reactive to user input changes
  blackScholes <- reactive({
    
    Tm <- input$maturity
    K <- input$strike
    S0 <- input$stock0
    r <- input$interest / 100
    sigma <- input$stock.sd / 100
    
    d1 <- (log(S0/K) + (r + 0.5*sigma^2) * Tm) / (sigma*sqrt(Tm))
    d2 <- d1 - sigma*sqrt(Tm)
    call.tp <- round(S0*pnorm(d1) - K*exp(-r*Tm)*pnorm(d2), 2)
    put.tp <- round(K*exp(-r*Tm)*pnorm(-d2) - S0*pnorm(-d1), 2)
    
    return(list(call.tp, put.tp))
    
  })
  
  # Render Output for Theoretical Option Prices
  output$call.tp <- renderText({
    paste0("Euro Call: $", blackScholes()[[1]])
  })
  output$put.tp <- renderText({
    paste0("Euro Put: $", blackScholes()[[2]])
  })
  
  # Simulated Price Calculations; Not reactive; Run on button press
  observeEvent(input$button, {
    
    Tm <- input$maturity
    K <- input$strike
    S0 <- input$stock0
    r <- input$interest / 100
    sigma <- input$stock.sd / 100
    dt <- input$deltat
    trials <- input$trials
    
    set.seed(input$seed)
    
    sim.growth <- matrix(1 + rnorm((Tm/dt)*trials, mean = r*dt, sd = sigma*sqrt(dt)),
                         nrow = Tm/dt,
                         ncol = trials,
                         byrow = FALSE)
    
    sim.price <- S0 * apply(sim.growth, 2, cumprod) # Table of stock price growth
    sim.price <- rbind(rep(S0, ncol(sim.price)), sim.price) # Table of stock price
    
    # Option payoffs at maturity time T
    payoff.call <- pmax(0, tail(sim.price, 1) - K)
    payoff.put <- pmax(0, K - tail(sim.price, 1))
    
    # Discounting payoffs at T back to t=0
    call.sp <- round(mean(exp(-r*T) * payoff.call), 2)
    put.sp <- round(mean(exp(-r*T) * payoff.put), 2)
    
    # Ouput for debug tables
    output$sim.growth <- renderTable(sim.growth)
    output$sim.price <- renderTable(sim.price)
    output$call.sp <- renderText(paste0("Euro Call: $", call.sp))
    output$put.sp <- renderText(paste0("Euro Put: $", put.sp))
    
    # Dataframe containing the mean, median, 5th and 95th percentiles for the simulated
    # stock prices at each point in time of length dt
    St <- data.frame(t(rbind(seq(0, Tm, by = dt),
                             apply(sim.price, 1, quantile, probs = c(0.05, 0.5, 0.95)),
                             apply(sim.price, 1, mean))))
    colnames(St) <- c("t", "q0.05", "Median", "q0.95", "Mean")
    
    output$plot <- renderPlotly({
      plot_ly(St, x = ~t, y = ~Mean, name = "Mean", type = "scatter", mode = "lines") %>% 
        add_trace(y = ~q0.05, name = "5th Percentile") %>% 
        add_trace(y = ~Median, name = "Median") %>% 
        add_trace(y = ~q0.95, name = "95th Percentile") %>% 
        layout(title = "Simulated Stock Prices",
               xaxis = list(title = "Time t (Years)"),
               yaxis = list(title = "Simulated Price ($)"))
    })
    
  })
   
}