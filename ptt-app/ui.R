library(shiny)
library(markdown)

shinyUI(
    navbarPage("Perfect t-test",

    # first tab that opens is About tab
    tabPanel("Instructions & Data", 
        fluidPage(

            # row with explanations of the figure
            fluidRow(
                column(12, includeMarkdown("instructions_initial.md") )
            ),  # END of row

            br(),
            
            h1("Upload your data"),
            
            fluidRow(
                column(4, 
                    wellPanel(

                        fileInput('data_file', 'Choose CSV File',
                            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                        tags$hr(),
                        checkboxInput('header', 'Header', TRUE),
                        radioButtons('sep', 'Separator',
                            c(Comma=',', Semicolon=';', Tab='\t'), ','),
                        radioButtons('quote', 'Quote',
                            c(None='', 'Double Quote'='"', 'Single Quote'="'"),
                            '"'),
                        # submit button
                        submitButton("Upload ")
                    )
                ),

                # Show the output
                column(8,
                    tableOutput(outputId = "data_table")
                )
            )  # END of row
        )  # end of page
    ),

    # second tab are diagnostic graphs about outliers, normality assumptions...
    tabPanel("Diagnostics",
        fluidPage(

            # row with explanations of the figure
            fluidRow(
                column(10, includeMarkdown("instructions_diagnostics.md") ),
                column(2)
            ),  # END of row

            br(),

            # row with buttons and the figure
            fluidRow(
                column(4,
                    wellPanel(

                        br(),
                        numericInput("alpha", 
                            label = h5("Significance level, alpha:"), 
                            value = 0.05),
                        br(),  
                        sliderInput("conf_int", label = h5("Select confidence interval level:"), min = 0, max = 1, value = 0.95, step = 0.01),
                        
                        # submit button
                        submitButton("Update report!")
                    )
                ),

                # Show a plot of the generated distribution
                column(8,
                    plotOutput(outputId = "outlier_plot")
                )
            )  # END of row
        )
    ),  # END of tab

    # 3rd tab is utility and cost interactive figure
    tabPanel("Frequentist tests",

        fluidPage(
            # rendering equations anywhere in static text
            withMathJax(),

            fluidRow(
                column(10, includeMarkdown("instructions_freq.md") ),
                column(2)
            ),

            br(),

            # Sidebar with a slider input for the number of bins
            fluidRow(
                column(4,
                    wellPanel(

                        br(),
                        numericInput("alpha", 
                            label = h5("Significance level, alpha:"), 
                            value = 0.05),
                        br(),  
                        sliderInput("conf_int", label = h5("Select confidence interval level:"), min = 0, max = 1, value = 0.95, step = 0.01),
                        br(), 
                        radioButtons('alt_hyp', label = h5("Select alternative hypothesis:"),
                            c("Two sided"="two.sided", "Less"="less", "Greater"="greater"),
                            "two.sided"), 
                        br(),  
                        checkboxInput("InAHurry", label = h5("Are you in a hurry?"), FALSE),
                        br(),  
                        sliderInput("bootstraps", label = h5("Number of bootrstrap replications:"), min = 0, max = 100000, value = 2000, step = 1),
                        
                                                
                        # submit button
                        submitButton("Update report!")
                    )
                ),

                # Show a report
                column(8,
                    verbatimTextOutput(outputId = "ttestOut")
                )
            )  # END of row
        )
    ),  # END of tab

    # 4th tab with Inequality and unpredictability figures
    tabPanel("Bayesian tests",
        fluidPage(

            # row with explanations of the figure
            fluidRow(
                column(10, includeMarkdown("instructions_bayes.md") ),
                column(2)
            ),  # END of row

            br(),

            # user input
            fluidRow(
                column(4, 
                    wellPanel(
                        
                        br(),
                        numericInput("BFrscale", 
                            label = h5("Specify expected effect:"), 
                            value = 0.5),
                        br(),  
                        checkboxInput("InAHurry", label = h5("Are you in a hurry?"), FALSE),
                                                
                        # submit button
                        submitButton("Update report!")
                    )
                ),
                
                # Show a report
                column(8,
                    verbatimTextOutput(outputId = "bayesOut")
                )
            )  # END of row
        )
    ),  # END of tab

    # 5th tab, references used in the text
    tabPanel("References", includeMarkdown("references.md") 
            
    )  # END of tab

    
))  # END of user interface function
