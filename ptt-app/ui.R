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
                column(12, includeMarkdown("instructions_data.md") )
            ),  # END of row

            fluidRow(
                column(4, 

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
                column(8,
                    h4("First 30 rows of your data"),
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

                        numericInput("alpha", 
                            label = h5("Significance level, alpha:"), 
                            value = 0.05),
                        
                        sliderInput("conf_int", label = h5("Select confidence interval level:"), min = 0, max = 1, value = 0.95, step = 0.01),

                        radioButtons('alt_hyp', label = h5("Select alternative hypothesis:"),
                            c("Two sided"="two.sided", "Less"="less", "Greater"="greater"),
                            "two.sided"),

                        checkboxInput("InAHurry", label = h5("Are you in a hurry?"), TRUE),

                        sliderInput("bootstraps", label = h5("Number of bootstrap replications:"), min = 0, max = 100000, value = 2000, step = 1), 

                        numericInput("BFrscale", 
                            label = h5("Specify expected effect:"), 
                            value = 0.5),                      
                                                
                        # submit button
                        submitButton("Update report!")
                    )
                ),

                # Show a report
                column(8,
                    tabsetPanel(type = "tabs", 
                        tabPanel("Instructions", includeMarkdown("instructions_freq.md")),
                        tabPanel("Classic", htmlOutput("ttestOut")), 
                        tabPanel("Robust", htmlOutput("robustOut")), 
                        tabPanel("Bayes",  htmlOutput("bayesOut"))
                    )
                )
            )  # END of row
        )
    ),  # END of tab


    # 4th tab, references used in the text
    tabPanel("References", includeMarkdown("references.md") 
            
    )  # END of tab

    
))  # END of user interface function
