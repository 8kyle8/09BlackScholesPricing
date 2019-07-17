library(shiny)
library(shinythemes)
library(plotly)

ui <- navbarPage(theme = shinytheme("cosmo"),
                 selected = "Usage Guide",
                 "Monte Carlo European Call/Put Pricing",
                 tabPanel("Simulation",
                          sidebarPanel(
                            h3("Pricing Parameters"),
                            sliderInput("maturity", "T: Time to Maturity (years)", 
                                        min = 1, max = 5, value = 1),
                            numericInput("strike", "K: Strike Price ($)", 
                                         value = 33, min = 0.01, max = 100, step = 0.01),
                            numericInput("stock0", "S0: Initial Stock Price ($)", 
                                         value = 30, min = 0.01, max = 100, step = 0.01),
                            numericInput("interest", "r: Interest Rate (%)", 
                                         value = 10, min = 0.01, max = 100, step = 0.01),
                            numericInput("stock.sd", "sigma: Stock Volatility (%)", 
                                         value = 25, min = 0.01, max = 100, step = 0.01),
                            h3("Simulation Parameters"),
                            numericInput("seed", "Random Seed", 
                                         value = 123, step = 1),
                            numericInput("trials", "Number of Trials", 
                                         value = 1000, min = 100, max = 10000, step = 1),
                            sliderInput("deltat", "delta t", 
                                        value = 0.05, min = 0.05, max = 1, step = 0.05),
                            actionButton("button", "Run Simulation", 
                                         width = "100%")
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Option Pricing",
                                       wellPanel(
                                         h3("Theoretical Black Scholes Price"),
                                         textOutput("call.tp"),
                                         textOutput("put.tp")
                                       ),
                                       wellPanel(
                                         h3("Simulated Price"),
                                         textOutput("call.sp"),
                                         textOutput("put.sp")
                                       ),
                                       absolutePanel(
                                         plotlyOutput("plot")
                                       )),
                              tabPanel("Simulated Stock Growth",
                                       helpText("Displays table containing simulated 
                                                numbers. Use to debug. May take a while to
                                                load."),
                                       div(style = "overflow-x: scroll", 
                                           tableOutput("sim.growth"))),
                              tabPanel("Simulated Stock Prices",
                                       helpText("Displays table containing simulated 
                                                numbers. Use to debug. May take a while to
                                                load."),
                                       div(style = "overflow-x: scroll", 
                                           tableOutput("sim.price")))
                            )
                          )),
                 tabPanel("About",
                          withMathJax(), # Shiny function to load MathJax ro display math
                          h3("Black Scholes Option Pricing Model"),
                          p("The Black Scholes model is a mathematical model that can be 
                            used to model the price of European-style options. 
                            The value at time 0 for a European call \\(c\\) or put \\(p\\)
                            on a non-dividend-paying underlying stock is given as:"),
                          ("$$c=S_0\\mathrm{N}(d_1)-K\\mathrm{e}^{-rT}\\mathrm{N}(d_2)$$"),
                          p("$$p=K\\mathrm{e}^{-rT}\\mathrm{N}(-d_2)-S_0\\mathrm{N}(-d_1)$$"),
                          tags$style(type = "text/css", "#p{text-align:center}"),
                          p("$$where$$"),
                          p(align="center", "\\(S_0\\) is the spot price of the stock at time \\(t=0\\)"),
                          p(align="center", "\\(T\\) is the maturity time of the option"),
                          p(align="center", "\\(K\\) is the strike price of the option"),
                          p(align="center", "\\(r\\) is the continuously compounding risk free rate"),
                          p(align="center", "\\(\\sigma\\) is the volatility of the stock's returns"),
                          p(align="center", "\\(\\mathrm{N}(\\cdot)\\) is the standard Normal CDF"),
                          p("$$d_1=\\frac{\\mathrm{ln}(S_0/K)+(r+\\sigma^2/2)T}{\\sigma\\sqrt{T}}$$"),
                          p("$$d_2=d_1-\\sigma\\sqrt{T}$$"),
                          h3("Monte Carlo Simulation of European Option Prices"),
                          p("We can simulate full stock price movement paths and estimate 
                            the payoffs and prices of European options using the
                            discretisation method. Under the Black Scholes model, stock 
                            price follows a Geometric Brownian Motion. It can then be 
                            shown that \\(R_i\\), the return on a stock in period \\(i\\) 
                            to \\(i+1\\) is:"),
                          p("$$R_i=\\frac{S_{i+1}-S_i}{S_i}\\sim\\mathrm{N}(\\mu{}\\Delta{}t, \\sigma^2\\Delta{}t)$$"),
                          p("From the simulated returns on a stock, we can calculate the 
                             simulated stock prices. We can then price the value of an 
                             option at time \\(t\\),  \\(f\\) as the expected present 
                             value of the option's payoff at maturity, \\(f_T\\). That is,"),
                          p("$$f=\\mathrm{PV}(\\hat{\\mathbb{E}}(f_T))=\\mathrm{e}^{-rT}\\hat{\\mathbb{E}}(f_T)$$"),
                          p("Note that the payoff at maturity for a European call \\(c\\) 
                            is \\(\\mathrm{max}(0,S_T-K)\\), and that for a European put 
                            \\(p\\) is \\(\\mathrm{max}(K-S_T,0)\\).")),
                 tabPanel("Usage Guide",
                          p("This application calculates the price of a European call/put 
                            option using the Black Scholes formula. The application also 
                            uses Monte Carlo simulations to estimate a price using the 
                            simulation results."),
                          h4("About Tab"),
                          p("Click on the About tab located in the top Navigation Bar to 
                            read more about the model and methodologies used in this 
                            application."),
                          h4("Simulation Tab"),
                          p("Click on the Simulation tab located in the top Navigation Bar 
                            to access the main application. To use the application, first 
                            enter the option pricing parameters. You can also leave it to 
                            the given default values. The theoretical prices shown are", 
                            strong("reactive."),"Next, enter the simulation parameters. It 
                            is recommended to use the default values. Increasing the 
                            number of trials and decreasing dt may increase loading times. 
                            Note that the simulated prices are", strong("NOT reactive."),
                            "That is, to calculate the simulated prices, you have to press 
                            the", code("Run Simulation"), "button. A chart will also be 
                            shown after running the simulation. This chart shows the range
                            of simulated stock prices from t=0 to t=T.")),
                 # Footer
                     p(style = "position: fixed; bottom: 0; width:100%; text-align: center",
                       "Developing Data Products Course Project,", 
                       em("Kyle Kaicheng Bao,"), "17 July 2019")
                 )
