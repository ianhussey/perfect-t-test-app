library(shiny)
library(markdown)

shinyUI(
    navbarPage("Perfect t-test",

    # first tab that opens is About tab
    tabPanel("About", 
        fluidPage(

            # styling the validation errors
            tags$head(
                tags$style(HTML("
                    .shiny-output-error-validation {
                        color: green;
                    }
                "))
            ),

            # row with explanations of the figure
            fluidRow(
                column(12, includeMarkdown("instructions_initial.md") )
            )  # END of row
        )
    ),        
    
    tabPanel("Your data", 
        fluidPage(  

            # row with explanations of the figure
            fluidRow(
                column(12, includeMarkdown("instructions_data.md"),br() )
            ),  # END of row

            fluidRow(
                column(5, 

                    wellPanel(
                        h4("Step 1: What type of test do you wish to do?"),
                        radioButtons('test_type', "",
                                     c(Independent = 'independent', 
                                       Dependent = 'dependent'),
                                     'independent')
                    ),

                    wellPanel(
                        h4('Step 2: Upload the file'),
                        fileInput('data_file', "",
                                  accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                        #tags$hr(),
                        radioButtons('sep', 'Separator',
                            c(Comma=',', Semicolon=';', Tab='\t'), '\t'),
                        radioButtons('dec', 'Decimal point',
                            c(Comma=',', Point='.'), '.'),
                        radioButtons('quote', 'Quote',
                            c(None='', 'Double Quote'='"', 'Single Quote'="'"),
                            '"')
                    ),
                    br(),
                    
                    # button for updating the Step 3 part
                    tags$p("After you have completed Step 1 and 2 please click the button before proceeding with the following step."), 
                    submitButton("Data is uploaded!"),
                    br(),
                    br(),
                    
                    # Step 3. part is updated according to the uploaded data and type of the test
                    wellPanel(
                        h4("Step 3: Set the variables"),
                        uiOutput("variables")
                    ),
                    br(),

                    # button for updating the Step 3 part
                    tags$p("After you have set the variables please click the button before proceeding with the following step."), 
                    submitButton("Variables are set!"),
                    br(),
                    br(),

                    # Step 4. part is updated according the variables set in step 3
                    wellPanel(
                        h4("Step 4: Set the groups"),
                        uiOutput("groups")
                    ),

                    wellPanel(
                        h4("Step 5: Set the labels for figures"),
                        uiOutput("labels")
                    ),
                    br(),

                    # button for updating the Step 5 part
                    tags$p("Please click the button before proceeding to the diagnostics and results tabs."), 
                    # submit button after all is done
                    submitButton("Groups and labels ready!"),
                    br(),
                    br()
                ),

                # example of how the data should look like
                # column(4,
                #     h4("Example of how your data should look like"),
                #     tableOutput(outputId = "data_example")
                # ),

                # Show the output
                column(7,
                    h4("First 60 rows of your data"),
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
                column(12,
                    # outlier plot
                    h2("Outliers"),
                    htmlOutput(outputId = "outlier_text"),
                    plotOutput(outputId = "outlier_plot"),

                    # Normality
                    h2("Normality assumption"),
                    htmlOutput(outputId = "normality_text"),
                    #tableOutput(outputId = "normality_test_results"),
                    plotOutput(outputId = "hist_plot"),
                    htmlOutput(outputId = "qqplot_text"),
                    plotOutput(outputId = "qq_plot"),
                    htmlOutput(outputId = "eqvar_text"),
                    br(),
                    br()
                )
            )  # END of row
        )
    ),  # END of tab

    # 3rd tab is the one with all the tests
    tabPanel("Tests",

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

                        h4("Standard frequentist test"),
                        
                        numericInput("alpha", 
                            label = h5("Set the significance level"), 
                            value = 0.05),
                        
                        numericInput("conf_int", 
                            label = h5("Set the confidence interval level"), 
                            value = 0.95),

                        radioButtons('alt_hyp', label = h5("Set the alternative hypothesis"),
                            c("Two sided"="two.sided", "Less"="less", "Greater"="greater"),
                            "two.sided"),
                        br(),

                        h4("Robust test"),

                        sliderInput("bootstraps", label = h5("Number of bootstrap replications"), min = 0, max = 100000, value = 2000, step = 1), 
                        br(),

                        h4("Bayesian test"),

                        numericInput("BFrscale", 
                            label = h5("Specify the scale for the prior distribution"), 
                            value = 0.5),                      
                        br(),
                        
                        tags$p("Certain calculations for robust and Bayesian tests could take some time, depending on the dataset size. If you have time to wait, uncheck the box."),
                        checkboxInput("InAHurry", label = h5("I'm in a hurry"), TRUE),
                        br(),

                        # submit button
                        submitButton("Update report!")
                    )
                ),

                # Show a report
                column(8,
                    tabsetPanel(type = "tabs", 
                        tabPanel("Classic", htmlOutput("ttestOut")), 
                        tabPanel("Robust", htmlOutput("robustOut")), 
                        tabPanel("Bayes",  htmlOutput("bayesOut"))
                    )
                )
            )  # END of row
        )
    ),  # END of tab

    # 4th tab are diagnostic graphs about outliers, normality assumptions...
    tabPanel("Results plot",
        fluidPage(

            # row with explanations of the figure
            fluidRow(
                column(10, includeMarkdown("instructions_figure.md") ),
                column(2)
            ),  # END of row

            br(),

            # row with buttons and the figure
            fluidRow(
                column(4,
                    wellPanel(

                        h4("Figure setup"),
                        br(),

                        radioButtons('plotType', 
                            label = h5("Select the type of the plot - means only, bar plot or a violin plot"),
                            c("Means plot"="meansPlot", 
                              "Bar plot"="barPlot", 
                              "Violin plot"="violinPlot")
                            ),

                        checkboxInput("individualData", 
                            label = h5("Show individual data in the bar plot?"), 
                            FALSE),

                        # submit button
                        submitButton("Update the figure!")
                    )
                ),

                column(8,
                    # Results plot
                    plotOutput(outputId = "results_plot")
                )
            )  # END of row
        )
    ),  # END of tab

    # 5th tab, references used in the text
    tabPanel("References", includeMarkdown("references.md") 
            
    )  # END of tab

    
))  # END of user interface function
