library(shiny)
library(ggplot2)
library(shinythemes)

shinyUI(fluidPage(
    theme = shinytheme("lumen"),
    titlePanel("Seatbelts_Dashboard App: UK Road Safety Seatbelts Dataset Analysis"),
    
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                condition = "input.tabs == 'Exploratory Regression'",
                h3("Exploratory Regression"),
                p("A classic scatter plot and regression summary for DriversKilled. 
                  Select a primary predictor variable from the list below. To see the
                  regression line in the plot, select the \"Show Regression Line\" option."),
            ),
            conditionalPanel(
                condition = "input.tabs == 'Front vs. Rear Comparison'",
                h3("Front vs. Rear Seats", br(), "Occupants Comparison"), 
                p("A side-by-side view, that includes both scatter plot and regression 
                summary, comparing how casualties changed for people in the front 
                (affected by the 1983 law) vs. people in the rear (not  affected, i.e. 
                 not mandated to wear seatbelts)"),
                 p("Select a primary predictor variable from the list below. To see the
                  regression line in both plots, select the \"Show Regression Line\" option"),
            ),
            conditionalPanel(
                condition = "input.tabs == 'Seasonality Analysis'",
                h3("Seasonality Analysis"),
                p("Anlyizing whether seasonality is a factor in the regression modeling 
                of the Seatbelts dataset. This tab provides an advanced statistical view 
                that uses color-coding by month to show seasonal clusters and trend lines."),
                p("Select a primary predictor variable from the list below. And, to see the
                monthly trend/regression lines in the plot, select the \"Show Regression 
                  Line\" option."),
                p("To demonstrate the effect of controling for seasonality (months) on 
                  the model and its accuracy, select the \"Control for Seasonality (Months)\" 
                  option below, and compare the results"),
            ),
            conditionalPanel(
                condition = "input.tabs == 'Predictor Tool'",
                h3("Predictor Tool"),
                p("Adjust the inputs, in the \"Simulation Inputs\" below, to see how economic,  
                    policy and seasonal factors change predicted number of Drivers Killed."),
                br(),
                h4(tags$b("Simulation Inputs:")),
                br(),
                numericInput("pred_petrol", tags$b("Petrol Price (£/liter):"), 
                             value = 0.10, step = 0.01),
                br(),
                sliderInput("pred_month_num", tags$b("Month of Year:"), 
                            min = 1, max = 12, value = 1, step = 1,
                            animate = TRUE), 
                br(),
                # style to have the checkbox after its label test
                tags$style(" .checkbox-reversed .checkbox label { display: flex; 
                              flex-direction: row-reverse; justify-content: flex-end;
                              align-items: center; }
                             .checkbox-reversed input[type='checkbox'] {
                              margin-left: 10px !important;  margin-top: 0px !important;
                                position: relative !important; }
                "),
                div(class = "checkbox-reversed", style = "position: absolute; left: 15px;",
                    checkboxInput("pred_law_bool", tags$b("Seatbelt Law Active?"), value = TRUE)
                ),
                br(),br(),
                br(),br(),
                numericInput("pred_kms", tags$b("Distance Driven (kms):"), 
                             value = 20000, step = 1000),
                br(),br(),
                h5(tags$b("Notes:")),
                p("By using the simulation tool above to predict future outcomes, 
                          you will see the following: "),
                tags$ul(
                    tags$li("Economic Impact: Lower the Petrol Price and watch how 
                            predicted fatalities likely rise, as people tend to drive 
                            more when gas is cheap. Same effect if you increase the 
                            number for the total Distance Driven (kms)"),
                    tags$li("Seasonal Impact: Switch from July (7) to December(12) to 
                            see the \"winter effect\" on the numbers."),
                    tags$li("Policy Impact: Toggle the checkbox labeled \"Seatbelt Law 
                            Active?\" from \"Unselected\" (not active) to \"Selected\"
                            (active), to visualize the immediate drop in predicted deaths.")
                )
            ),
            conditionalPanel(
                condition = "input.tabs == 'About Data'",
                h3("About Data"),
                p("Introducing the Seatbelts dataset in R, and its historic imprtantance. 
                  The dataset's variables are described." ),
                p("A time-series plot to show how fatalities changed before and after 
                  the 1983 Seatbelt Law is provided. The plot provides a visual proof 
                  of the interruption in the data caused by the 1983 law. "),
                p("Finally, a section discussing the complexity of road safety modeling 
                  is provided."),
                br(),br(),br(),br(),
            ),
            conditionalPanel(
                condition = "input.tabs == 'About App'",
                h3("About App"),
                p("A Navigationa Guide is provided to explain the different tabs, 
                  and what the type of analysis/information each provids. Additinally, 
                  the two main UI components of this app, the sidebar and the tabs, 
                  are explained with emphasis on where the controls are and what
                  information each provides."),
                p("The App is designed to be easy to use, with sSufficient help 
                embedded in the sidebar to help and guide the user. The tabs' 
                order, organization and visualizations are all designed for a 
                  data analyst user in mind. "),
                br(),
                h5(tags$b("The App, it's Presentation and its Code are available at:")),
                tags$ul(
                  tags$li(tags$a(href = "https://masapps.shinyapps.io/Seatbelts_Dashboard/", "The Seatbelts_Dashboard App")),
                  tags$li(tags$a(href = "https://rpubs.com/MAlShawa/1382929", "The App's Presentation")),
                  tags$li(tags$a(href = "https://github.com/MAlShawa/Seatbelts_Dashboard_App", "The App's Code"))
                ),
                br(),
            ),
            br(),br(),
            conditionalPanel(
                condition = "input.tabs == 'Exploratory Regression' || input.tabs == 'Front vs. Rear Comparison' || input.tabs == 'Seasonality Analysis'",
                selectInput("predictor", 
                        "Select Primary Predictor:",
                        choices = c("Kilometers Driven" = "kms", 
                                    "Petrol Price" = "PetrolPrice",
                                    "Front Seatbelt Law (1983)" = "law",
                                    "Front Seat Passengers (KSI)" = "front")),
            
                checkboxInput("show_lm", "Show Regression Line", value = TRUE),
            ),
            conditionalPanel(
                condition = "input.tabs == 'Exploratory Regression'",
                br(),br(),
                h5(tags$b("Notes:")),
                tags$ul(
                    tags$li("Residual Analysis, is provided to check the model sssumptions). 
                            If the model is a good fit, points should be randomly scattered 
                            around the Residuals horizontal 0 line."),
                    tags$li("Standard linear regression can sometimes be misleading if there 
                    is \"autocorrelation\" (where the error in one month is related to the error 
                    in the previous month). A residual plot is provided to see if the model is missing 
                            a pattern, or if the relationship is truly linear."),
                    tags$li("The \"Seasonality Analysis\" Tab tests if seasonality is
                            an important factor to consider. Check out the tab, to see how seasonality
                             (month) should be a categorical predictor(factor) in the regression model. This 
                            is understandable since road fatalities usually spike in the winter (shorter 
                            days, icy roads) and dip in the summer.")
                )
            ),
            conditionalPanel(
                condition = "input.tabs == 'Front vs. Rear Comparison'",
                br(),br(),
                h5(tags$b("Notes:")),
                tags$ul(
                    tags$li("To see the effect of the Front Seatbelts Law, select it as a predictor from 
                the list above You should see: 1) for the front occupants, a significant negative 
                coefficient (casualties dropped); and 2) for the rear occupants, the effect is often 
                  much smaller or non-significant, as they weren't legally required to wear belts yet."),
                    tags$li("In addition, historians and data scientists often look for \"risk compensation\" 
                in this data. The idea is that front-seat safety might have led to different 
                behaviors that affected rear passengers differently. One could see such effects
                by selecting other variables as predictors from the list above.")
                )
            ),
            conditionalPanel(
              condition = "input.tabs == 'Seasonality Analysis'",
              radioButtons("trend_type", "Trend Line Type:",
                           choices = c("One Global Trend" = "global", 
                                       "Individual Monthly Trends" = "monthly"),
                           selected = "global"),
              br(),
              checkboxInput("use_seasonality", "Control for Seasonality (Months)", value = TRUE),
              br(),br(),
              h5(tags$b("Notes:")),
              tags$ul(
                tags$li("If \"Controling for Seasonality\" is selected, the \"Month\" variable 
                is the included as a categorical predictor (factor), in addition to the \"Primary 
                        Predictor\" selcted from the list above,  in the regression model."),
                tags$li("When accounting for Seasonality (Months) is selected, the 
                D-W Statistic, a test to detect if regression errors have first-order
                autocorrelation (correlation with the immediately preceding error),  
                gets closer to 2 (the ideal no-correlation value), and the \"pattern\" 
                in the errors disappears or lessens, as shown in the Residulas plot provided."),
                tags$li("Seasonality, even though not a primary predictor, is important 
                to analyzing the Seatbelts dataset. This is understandable since road 
                fatalities usually spike in the winter (shorter days, icy roads) and 
                dip in the summer.")
              )
            ),
            br(),br(),
            hr(),
            conditionalPanel(
              condition = "input.tabs != 'About Data'",
              helpText(tags$b("\"About Data\" Tab"), ": provides an overview of the Seatbelts Dataset** used in this app, 
                     and the complexity of analyzing and modeling it.")
            ),
            conditionalPanel(
              condition = "input.tabs != 'About App'",
              helpText(tags$b("\"About App\" Tab"), ": provides a Navigation Guide of the app.")
            ),
            conditionalPanel(
              condition = "input.tabs != 'About Data'",
              helpText("** Data source: UK Department of Transport data (1969–1984).")
            ),
        ),
        
        mainPanel(
            tabsetPanel(id = "tabs",
                        
                        # Tab 1: Basic Regression
                        tabPanel("Exploratory Regression", 
                                 plotOutput("regPlot"),
                                 h4("Model Summary"),
                                 verbatimTextOutput("regSummary"),
                                hr(),
                                fluidRow(
                                    column(12, 
                                   h4("Residual Analysis"), 
                                   plotOutput("residualPlotMain")
                                    ),
                                )),
                        
                        
                        # Tab 2: Front vs Rear Comparison
                        tabPanel("Front vs. Rear Comparison",
                                 fluidRow(
                                     column(6, h4("Front Seat Occupants"), plotOutput("plotFront")),
                                     column(6, h4("Rear Seat Occupants"), plotOutput("plotRear"))
                                 ),
                                 fluidRow(
                                     column(6, verbatimTextOutput("sumFront")),
                                     column(6, verbatimTextOutput("sumRear"))
                                 )),
                        
                        # Tab 3: Seasonality & Residuals
                        tabPanel("Seasonality Analysis",
                                 h4("Seasonal Trends in Drivers Killed"),
                                 plotOutput("seasonalPlot"),
                                 hr(),
                                 h4("Durbin-Watson Autocorrelation Test"),
                                 verbatimTextOutput("dwTest"),
                                 br(),
                                 h4("Residual Plot"),
                                 plotOutput("residualPlot")),
                        
                        # Tab 4 : Prediction Tool 
                        tabPanel("Predictor Tool", 
                                  br(),br(),
                                  h3("Predicted Driver Fatalities"),
                                  div(style = "font-size: 40px; color: #2c3e50; font-weight: bold;",
                                                textOutput("predictionResult")),
                                  hr(),
                                  br(),br(),br(),
                                  fluidRow(
                                          column(6, 
                                                  wellPanel(
                                                  style = "background-color: #f8f9fa;",
                                                  h5("Model Confidence (R²)"),
                                                           textOutput("rSquaredText"),
                                                           tags$small("Higher is better (max 1.0)")
                                                   )
                                                 ),
                                           column(6, 
                                                   wellPanel(
                                                   style = "background-color: #f8f9fa;",
                                                   h5("Avg. Prediction Error (MAE)"),
                                                           textOutput("maeText"),
                                                           tags$small("Lower is better") 
                                                   ) 
                                                  )  
                                          ),
                                 br(),
                                 p(tags$b("Note:")),
                                 tags$ul(
                                   tags$li("The regression model used here to predict the future 
                                    number of DriversKilled is a multi-variable model thatincludes 
                                    all the main predictors: PetrolPrice, kms, law, and months 
                                    (seasonality factor). This will allow the user to test the effect
                                    of each predictors, individually or a group of them, on the 
                                           DriversKilled outcome."),
                                  tags$li("Accuracy metrics are calculated based on historical 
                                    model performance.")
                                  ),
                        ), 
                        
                        # Tab 5: About Data
                        tabPanel("About Data",
                                 h2("About the Seatbelts Dataset"),
                                 p("The 'Seatbelts' dataset is a built-in R dataset containing monthly totals of car 
                                   drivers in UK killed or seriously injured (KSI) between Jan 1969 and Dec 1984. 
                                   The ddta source is the UK Department of Transport."), 
                                 br(),
                                 h4("The 1983 Legislation"),
                                 p("On January 31, 1983, it became compulsory for drivers and front-seat passengers to wear seatbelts. This dataset is famously used in econometrics to study the effectiveness of safety legislation."),
                                 br(),
                                 h4("Variable Descriptions"),
                                 tableOutput("varTable"),
                                 br(),
                                 hr(),
                                 h4("Historical Trend: The 1983 Interruption"),
                                 plotOutput("timeSeriesPlot"),
                                 br(),
                                 h4("The Complexity of Road Safety Modeling"),
                                 p("Analyzing road fatalities requires more than a simple count. It necessitates a 
                                    multivariate analytical approach. While the UK's 1983 seatbelt mandate was a landmark legislative 
                                     intervention, it operated within a complex ecosystem of influences, as the plot above shows:"),
                                 tags$ul(
                                     tags$li(tags$b("Legislative Factors:"), "The 'law' variable marks a structural shift 
                                                    in the data, but its impact must be isolated from long-term improvements in 
                                                    vehicle engineering and emergency medical response (long-term trends over time 
                                                    are not captured specifically in the data, but assumed)."),br(),
                                     tags$li(tags$b("Economic Factors:"), "Petrol prices and total distance driven (kms) 
                                                    serve as proxies for economic activity. When fuel prices rise, discretionary 
                                                    driving often decreases, naturally lowering the risk of fatalities."),br(),
                                     tags$li(tags$b("Seasonal Factors:"), "Road conditions and daylight hours fluctuate 
                                                    predictably. Without controlling for seasonality, a summer dip in fatalities 
                                                    might be incorrectly attributed to a policy/economical change rather than the weather.")
                                 ),
                                 br(),br()
                        ),
                        
                        # Tab 6: About App
                        tabPanel("About App",
                                 h2("How to Use This App"),
                                 wellPanel(
                                     h4("Navigation Guide"), br(),
                                     tags$ol(
                                         tags$li(tags$b("Exploratory Regression:"), "Select a predictor in the sidebar to see its 
                                                 linear relationship with Driver Fatalities."), br(),
                                         tags$li(tags$b("Front vs. Rear Comparison:"), "Analyze if the 1983 Seatbelts Law affected 
                                                 front and rear passengers differently."), br(),
                                         tags$li(tags$b("Seasonality Analysis:"), "Validate the model by checking for monthly 
                                                 patterns and residual errors."), br(),
                                         tags$li(tags$b("Predictor Tool:"), "Interact with a live simulation to predict Driver 
                                                 Fatalities based on economic, legislative and seasonal changes."), br(),
                                         tags$li(tags$b("About Data:"), "Learn about the Seatbelts dataset importance, its variables, 
                                                 and see a time-series plot showing the effect of the 1983 Seatbelt Law on drivers 
                                                 killed. In addition, learn about the complexity of road safety modeling.")
                                     )
                                 ),
                                 h4("UI Components"),
                                 p("The App is designed to be easy to use, and all controls are easy to notice and interact with.
                                    Sufficient help is provided and embedded in the sidebar to help and guide the user. The tabs' 
                                    order, organization and visualizations are all designed for a data analyst user in mind. The 
                                   two major components of the app's User Interface (UI) are:"),
                                 tags$ol(
                                   tags$li(tags$b("The Sidebar:"), "Its content changes, as the user change from one tab to anoother, 
                                                affecting the visualizations for each tab. It provides:") ,
                                   tags$ol(
                                     tags$li("A general describtion of what the tab is about."),
                                     tags$li("Instructions on what the controls are, and how to use them."),
                                     tags$li("Provide the controls that control the visualizations appearing on the tab."),
                                     tags$li("Notes about things to observe, watch out for, notice, or should try out; In 
                                     addition to, some comments and important information about why certain items/visualizations 
                                             are added to the tab'content."),
                                     tags$li("Instructions on where to get help about the data or the app (including how to use
                                     it) are added at the bottom of the sidebar.")
                                   ), br(),
                                   tags$li(tags$b("The Tabs:"), "Each tab has its own content and visualizations related to the 
                                           type of analysis/information it intends to provide. Such content and visualization are
                                           controled by the specific controls provided in the \"sidebar\" for the tab. The 
                                           Navigation Guide above explains the different tabs in this app.")
                                 ),
                                 br(),br()
                        )
                        
            )
        )
    )
))