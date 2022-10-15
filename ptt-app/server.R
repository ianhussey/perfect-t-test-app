# dependencies
library(shiny)
library(ggplot2)
library(gtable)
library(gridExtra)
library(markdown)
library(reshape2)

library(PoweR) 
library(car) 
library(MBESS) 
library(bootES) 
library(WRS2)
library(BEST)
library(BayesFactor) 
library(HLMdiag)

# globals
options(scipen=6) #disable scientific notation for numbers smaller than x (i.e., 10) digits (e.g., 4.312e+22)

# acquire data
example_independent       <- read.table("data/demo_independent.txt", stringsAsFactors=FALSE, header=TRUE, sep="\t")
example_dependent         <- read.table("data/demo_dependent.txt", stringsAsFactors=FALSE, header=TRUE, sep="\t")

# server logic 
shinyServer(function(input, output) {

    # ----
    # Validating inputs
    # ----
    
    validate_data_input   <- reactive({
       
        # input$data_file will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        
        # we validate if the data file is present, if not the text is displayed instead of ugly red error messages
        validate(
            need(!is.null(input$data_file), "Please upload a data set!")
        )
        
    })

    validate_indVariables_input <- reactive({
       
        # we validate if the variables are set, if not the text is displayed instead of ugly red error messages
        validate(
            need(!is.null(input$factorlabel), "Please set the variables!")
        )
        
    })

    validate_depVariables_input <- reactive({
       
        # we validate if the variables are set, if not the text is displayed instead of ugly red error messages
        validate(
            need(!is.null(input$subgrouplabel), "Please set the variables!")
        )
        
    })

    validate_test_input   <- reactive({
       
        # we validate if the alpha, confidence interval etc are correctly set, if not the text is displayed instead of ugly red error messages
        validate(
            need(input$alpha > 0 && input$alpha < 1, "Significance level should be between 0 and 1!"),
            need(input$conf_int > 0 && input$conf_int < 1, "Confidence interval should be between 0 and 1!"),
            need(input$BFrscale > 0, "Bayes factor prior scale should be positive")
        )
        
    })

    # ----
    # Loading and preprocessing the data
    # ----
    
    # original data file loaded by the user
    get_data              <- reactive({
       
        # we call validated data input 
        validate_data_input()
                
        # reading in the data
        data_original <- read.table(input$data_file$datapath, header=TRUE, sep=input$sep, quote=input$quote, dec=input$dec, stringsAsFactors=FALSE)
        return(data_original)
    })
    
    # data file after we have done some processing, this one will be actually used in computations
    get_proc_data         <- reactive( {      

        ### ORIGINAL DATA 
        data_original <- get_data()

        ### test_type dependent processing
        if (input$test_type == "dependent") {
            if (!is.null(input$factorlabel) &&
                !is.null(input$measurelabel) &&
                !is.null(input$xlabel) &&
                !is.null(input$ylabel)) {
                factorlabel <- input$factorlabel
                measurelabel <- input$measurelabel
                subgrouplabel <- input$subgrouplabel
                subgrouptokeep <- input$subgrouptokeep
                subject <- input$subject
                xlabel <- input$xlabel
                ylabel <- input$ylabel
                

                ### PROCESSING THE DATA   
                
                data_proc <- na.omit(data_original)
                if (subgrouplabel != " "){
                    data_proc <- subset(data_proc, data_proc[[subgrouplabel]]==subgrouptokeep)
                }

                # create two variables (x and y) that contain values of two datasets to be compared
                x <- data_proc[[xlabel]]
                y <- data_proc[[ylabel]]
                diff <- x-y #difference scores

                # Add difference to data_proc dataframe for plotting.
                data_proc[["diff"]] <- diff

                # Convert data_proc to long format
                data_proc_long <- melt(data_proc, id.vars = subject, measure.vars = c(xlabel,ylabel), variable.name = factorlabel,value.name = measurelabel)

                return(list(data_proc = data_proc,
                            data_proc_long = data_proc_long,
                            x = x,
                            y = y))
            }
        } else if (input$test_type == "independent") {
            if (!is.null(input$factorlabel) &&
                !is.null(input$measurelabel) &&
                !is.null(input$xlabel) &&
                !is.null(input$ylabel)) {
                factorlabel <- input$factorlabel
                measurelabel <- input$measurelabel
                xlabel <- input$xlabel
                ylabel <- input$ylabel
                

                ### PROCESSING THE DATA   
                data_proc <- na.omit(data_original)
                data_proc <- subset(data_proc, data_proc[[factorlabel]] == xlabel |
                                    data_proc[[factorlabel]] == ylabel)
                x_data_proc <- subset(data_proc, data_proc[[factorlabel]] == xlabel)
                y_data_proc <- subset(data_proc, data_proc[[factorlabel]] == ylabel)
                x <- x_data_proc[[measurelabel]]
                y <- y_data_proc[[measurelabel]]

                return(list(data_proc = data_proc,
                            x_data_proc = x_data_proc,
                            y_data_proc = y_data_proc,
                            x = x,
                            y = y))
            }
        }
    })

    # ------------------------------------------------------------------
    # OUTPUTS
    # ------------------------------------------------------------------
    
    # ----
    # Data tab
    # ----
    
    output$data_table     <- renderTable({
        
        # fetch the original data file and return it to the user
        data_user <- get_data()
        return(head(data_user, 60))
    })

    output$variables      <- renderUI({

        # get the variables available in the data set
        data_user <- get_data()
        variables <- colnames(data_user)
        
        if (input$test_type == "independent") {
            list(
                selectInput("factorlabel", 
                            label = h5("Select the variable that determines to which group the observation belongs"), 
                            choices = variables,
                            selected = variables[1]),

                selectInput("measurelabel", 
                            label = h5("Select the variable with the dependent measure"), 
                            choices = variables,
                            selected = variables[2])
            )
        } else if (input$test_type == "dependent") {
            list(
                selectInput("subject", 
                    h5("Select the variable that determines the subject identifier"), 
                    choices = variables, 
                    selected = variables[1]),

                selectInput("xlabel", 
                    h5("Select the variable that determines the first group"), 
                    choices = variables, 
                    selected = variables[2]),
                
                selectInput("ylabel", 
                    h5("Select the variable that determines the second group"),  
                    choices = variables, 
                    selected = variables[3]),
                
                selectInput("subgrouplabel", 
                    h5("Optionally, you can specify a variable that determines  subgroups of the data. To analyze all data, leave empty"), 
                    choices = c(" ", variables), 
                    selected = " ")
            )
        }
    })
    
    output$groups         <- renderUI({

        # reading in the data 
        data_user <- get_data()
        
        # return
        if (input$test_type == "independent") {

            # validating that variables are set
            validate_indVariables_input()

            # getting the groupings
            group_list <- unique(data_user[ ,input$factorlabel])

            list(
                selectInput("xlabel", 
                    h5("Select the label that determines the first group"), 
                    choices = group_list, 
                    selected = group_list[1]),
                
                selectInput("ylabel", 
                    h5("Select the label that determines the second group"),  
                    choices = group_list, 
                    selected = group_list[2])
                )

        } else if (input$test_type == "dependent") {  
            
            # validating that variables are set
            validate_depVariables_input()

            # check the input for the subgroup variable
            if (input$subgrouplabel == " ") {
                group_list <- " "
            } else {
                group_list <- unique(data_user[ ,input$subgrouplabel])
            }
            
            list(
                textInput("factorlabel", 
                    h5("Define the name of the factor that describes the difference between group 1 and group 2"), 
                    "Groups"),
                
                textInput("measurelabel", 
                    h5("Define the name of the measure that describes the values of group 1 and group 2"), 
                    "Measure"),

                selectInput("subgrouptokeep", 
                    h5("Optionally, if you have specified the subgroup variable in the previous step, you can specify the identifier of the group you want to analyze"), 
                    choices = group_list, 
                    selected = group_list[1])
                )
        }
    })

    output$labels         <- renderUI({

        # return
        if (input$test_type == "independent") {
            list(
                textInput("xlabelstring", 
                    h5("Set the label for the group variable that will be displayed on x axis"), 
                    value = "x axis label"),
                textInput("ylabelstring", 
                    h5("Set the label for the measure variable that will be displayed on y axis"),
                    value = "y axis label")
                )
            
        } else if (input$test_type == "dependent") {    
            list(
                
                textInput("xlabelstring", 
                    h5("Set the label for the group variable that will be displayed on x axis"), 
                    value = "x axis label"),
                textInput("ylabelstring", 
                    h5("Set the label for the measure variable that will be displayed on y axis"),
                    value = "y axis label")
                )
        }
    })

    # ----
    # Diagnostics tab
    # ----

    output$outlier_plot   <- renderPlot({
        
        if (input$test_type == "independent") {
            # loading the data
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring

            ### producing a figure with outliers
            figure <- 
                ggplot(data_proc, aes_string(factorlabel, measurelabel)) +
                geom_boxplot()+
                ylab(ylabelstring)  + 
                xlab(xlabelstring) + 
                theme_bw(base_size=14) + 
                theme(panel.grid.major.x = element_blank())
             return(figure)

        } else if (input$test_type == "dependent") {

            # data
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            diff <- data_proc[["diff"]]
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            alpha <- input$alpha
            H1 <- input$alt_hyp
            ConfInt <- input$conf_int
             
            # Main scatterplot
            p1 <- ggplot(data_proc, aes_string(x=xlabel, y=ylabel)) + 
                geom_point(alpha = 0.35) +
                scale_x_continuous(expand = c(0, 0)) + 
                scale_y_continuous(expand = c(0, 0)) + 
                expand_limits(y = c(min(data_proc[[ylabel]]) - 0.1 * diff(range(data_proc[[ylabel]])), 
                                  max(data_proc[[ylabel]]) + 0.1 * diff(range(data_proc[[ylabel]])))) + 
                expand_limits(x = c(min(data_proc[[xlabel]]) - 0.1 * diff(range(data_proc[[xlabel]])), 
                                  max(data_proc[[xlabel]]) + 0.1 * diff(range(data_proc[[xlabel]])))) +
                theme_bw(base_size=14) +
                ylab(ylabel)  + 
                xlab(xlabel) +
                theme(plot.margin= unit(c(0, 0, 0.5, 0.5), "lines"))
              
            # To remove all axis labelling and marks from the two marginal plots
            theme_remove_all <- theme(axis.text = element_blank(),
                                      axis.title = element_blank(),
                                      axis.ticks =  element_blank(),
                                      axis.ticks.margin = unit(0, "lines"),
                                      axis.ticks.length = unit(0, "cm"))

            # Horizontal marginal boxplot - to appear at the top of the chart
            p2 <- ggplot(data_proc, aes_string(x = factor(1), y = xlabel)) + 
                geom_boxplot() +
                # geom_jitter(position = position_jitter(width = 0.05)) +
                scale_y_continuous(expand = c(0, 0)) + 
                expand_limits(y = c(min(data_proc[[xlabel]]) - 0.1 * diff(range(data_proc[[xlabel]])), 
                                  max(data_proc[[xlabel]]) + 0.1 * diff(range(data_proc[[xlabel]])))) + 
                coord_flip() +
                theme_bw(base_size=14) +
                theme_remove_all +
                theme(plot.margin= unit(c(0.5, 0, 0, 0.5), "lines"))

            # Vertical marginal boxplot - to appear at the right of the chart
            p3 <- ggplot(data_proc, aes_string(x = factor(1), y = ylabel)) + 
                geom_boxplot() +
                #  geom_jitter(position = position_jitter(width = 0.05)) +
                scale_y_continuous(expand = c(0, 0)) + 
                expand_limits(y = c(min(data_proc[[ylabel]]) - 0.1 * diff(range(data_proc[[ylabel]])), 
                                  max(data_proc[[ylabel]]) + 0.1 * diff(range(data_proc[[ylabel]])))) + 
                theme_bw(base_size=14) +
                theme_remove_all +
                theme(plot.margin= unit(c(0, 0.5, 0.5, 0), "lines"))

            # Get the gtables
            gt1 <- ggplot_gtable(ggplot_build(p1))
            gt2 <- ggplot_gtable(ggplot_build(p2))
            gt3 <- ggplot_gtable(ggplot_build(p3))

            # Get maximum widths and heights for x-axis and y-axis title and text
            maxWidth = unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
            maxHeight = unit.pmax(gt1$heights[4:5], gt3$heights[4:5])

            # Set the maximums in the gtables for gt1, gt2 and gt3
            gt1$widths[2:3] <- as.list(maxWidth)
            gt2$widths[2:3] <- as.list(maxWidth)

            gt1$heights[4:5] <- as.list(maxHeight)
            gt3$heights[4:5] <- as.list(maxHeight)

            # Combine the scatterplot with the two marginal boxplots
            # Create a new gtable
            figure <- gtable(widths = unit(c(7, 1), "null"), height = unit(c(1, 7), "null"))

            # Instert gt1, gt2 and gt3 into the new gtable
            figure <- gtable_add_grob(figure, gt1, 2, 1)
            figure <- gtable_add_grob(figure, gt2, 1, 1)
            figure <- gtable_add_grob(figure, gt3, 2, 2)

            # And render the plot
            grid.newpage()
            grid.draw(figure)
        } 
    })

    output$outlier_text   <- renderText({
        
        if (input$test_type == "independent") {
            report <- "Boxplots can be used to identify outliers. Boxplots give the median (thick line), and 25% of the data above and below the median (box). End of whiskers are the maximum and minimum value when excluding outliers (whih are indicated by dots)."
        } else if (input$test_type == "dependent") {
            report <- "Boxplots can be used to identify outliers. Boxplots give the median (thick line), and 25% of the data above and below the median (box). End of whiskers are the maximum and minimum value when excluding outliers (which are indicated by dots). Code adapted from [Sandy Muspratt](https://github.com/SandyMuspratt/ScatterBoxPlot/blob/master/mtcars%20marginal%20boxplots.R)."
        }

        html_out <- renderMarkdown(text = report)
        return(html_out)
    })

    output$normality_text <- renderText({

        if (input$test_type == "independent") {

            ### loading the data
            data_user <- get_proc_data()
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring


            ### Test normality 
            normalityrejectionsx <- (statcompute(21, x, levels = c(0.05))$decision + statcompute(6, x, levels = c(0.05))$decision + statcompute(2, x, levels = c(0.05))$decision + statcompute(7, x, levels = c(0.05))$decision)

            normalityrejectionsy <- (statcompute(21, y, levels = c(0.05))$decision + statcompute(6, y, levels = c(0.05))$decision + statcompute(2, y, levels = c(0.05))$decision + statcompute(7, y, levels = c(0.05))$decision)

            # Render the text
            report <- c(
                paste0("The independent *t*-test assumes that scores in both groups (", xlabel," and ", ylabel, ") are normally distributed. If the normality assumption is violated, the Type 1 error rate of the test is no longer controlled, and can substantially increase beyond the chosen significance level. Formally, a normality test based on the data is incorrect, and the normality assumption should be tested on additional (e.g., pilot) data. Nevertheless, a two-step procedure (testing the data for normality, and using alternatives for the traditional *t*-test if normality is violated, seems to work well (see [Rochon, Gondan, & Kieser, 2012](http://www.biomedcentral.com/1471-2288/12/81))."),
                
                "### Tests for normality",

                "Four tests for normality are reported below for both groups. [Yap and Sim (2011, p. 2153)](http://www.tandfonline.com/doi/pdf/10.1080/00949655.2010.520163) recommend: \"If the distribution is symmetric with low kurtosis values (i.e. symmetric short-tailed distribution), then the D'Agostino-Pearson and Shapiro-Wilkes tests have good power. For symmetric distribution with high sample kurtosis (symmetric long-tailed), the researcher can use the JB, Shapiro-Wilkes, or Anderson-Darling test.\" The Kolmogorov-Smirnov (K-S) test is often used, but no longer recommended, and not included here.", 
                "  ",
                "If a normality test rejects the assumptions that the data is normally distributed (with *p* < .05) non-parametric or robust statistics have to be used (robust analyses are provided below).",  
                "  ",
                paste0("**The normality assumption was rejected in ", normalityrejectionsx, " out of 4 normality tests for the ",xlabel, "condition, and in ", normalityrejectionsy, " out of 4 normality tests for the ", ylabel, " condition.**"),
                "  ",
                paste0("Test Name  | *p*-value ", xlabel, " | *p*-value ", ylabel), 
                "------------- | :--------------: | :-------------:",
                paste0("Shapiro-Wilk  | *p* ", ifelse(statcompute(21, x, levels = c(0.05))$pvalue>0.001,' = ', ' < ')," ", ifelse(statcompute(21, x, levels = c(0.05))$pvalue>0.001, round(statcompute(21, x, levels = c(0.05))$pvalue, digits=3), '0.001'),"  |   *p* ", ifelse(statcompute(21, y, levels = c(0.05))$pvalue>0.001,' = ', ' < ')," ", ifelse(statcompute(21, y, levels = c(0.05))$pvalue>0.001, round(statcompute(21, y, levels = c(0.05))$pvalue, digits=3), '0.001')),   
                paste0("D'Agostino-Pearson  | *p* ", ifelse(statcompute(6, x, levels = c(0.05))$pvalue>0.001,' = ', ' < ')," ", ifelse(statcompute(6, x, levels = c(0.05))$pvalue>0.001, round(statcompute(6, x, levels = c(0.05))$pvalue, digits=3), '0.001')," |  *p* ", ifelse(statcompute(6, y, levels = c(0.05))$pvalue>0.001,' = ', ' < ')," ", ifelse(statcompute(6, y, levels = c(0.05))$pvalue>0.001, round(statcompute(6, y, levels = c(0.05))$pvalue, digits=3), '0.001')),
                paste0("Anderson-Darling  | *p* ", ifelse(statcompute(2, x, levels = c(0.05))$pvalue>0.001,' = ', ' < ')," ", ifelse(statcompute(2, x, levels = c(0.05))$pvalue>0.001, round(statcompute(2, x, levels = c(0.05))$pvalue, digits=3), '0.001'),"  | *p* ", ifelse(statcompute(2, y, levels = c(0.05))$pvalue>0.001,' = ', ' < ')," ", ifelse(statcompute(2, y, levels = c(0.05))$pvalue>0.001, round(statcompute(2, y, levels = c(0.05))$pvalue, digits=3), '0.001')),    
                paste0("Jarque-Berra  | *p* ", ifelse(statcompute(7, x, levels = c(0.05))$pvalue>0.001,' = ', ' < ')," ", ifelse(statcompute(7, x, levels = c(0.05))$pvalue>0.001, round(statcompute(7, x, levels = c(0.05))$pvalue, digits=3), '0.001')," |   *p* ", ifelse(statcompute(7, y, levels = c(0.05))$pvalue>0.001,' = ', ' < ')," ", ifelse(statcompute(7, y, levels = c(0.05))$pvalue>0.001, round(statcompute(7, y, levels = c(0.05))$pvalue, digits=3), '0.001')),
                "  ",
                "  ",
                "In very large samples (when the test for normality has close to 100% power) tests for normality can result in significant results even when data is normally distributed, based on minor deviations from normality. In very small samples (e.g., n = 10), deviations from normality might not be detected, but this does not mean the data is normally distributed.  Always look at a plot of the data in addition to the test results.",
                
                "### Histogram, kernel density plot (black line) and normal distribution (red line) of difference scores",
                "The density (or proportion of the observations) is plotted on the y-axis. The grey bars are a histogram of the scores in the two groups. Judging whether data is normally distributed on the basis of a histogram depends too much on the number of bins (or bars) in the graph. A kernel density plot (a non-parametric technique for density estimation) provides an easier way to check the normality of the data by comparing the shape of the density plot (the black line) with a normal distribution (the red dotted line, based on the observed mean and standard deviation). For independent *t*-tests, the dependent variables in both conditions should be normally distributed."
            )

        } else if (input$test_type == "dependent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            diff <- data_proc[["diff"]]
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            alpha <- input$alpha
            H1 <- input$alt_hyp
            ConfInt <- input$conf_int

            # Test normality 
            normalityrejections <- (statcompute(21, diff, levels = c(0.05))$decision + statcompute(6, diff, levels = c(0.05))$decision + statcompute(2, diff, levels = c(0.05))$decision + statcompute(7, diff, levels = c(0.05))$decision)

            report <- c(
                paste0("The dependent *t*-test assumes that *difference* scores are normally distributed and that the variances of the two groups are equal. It does *not* assume the data within each measurement (so within the ", xlabel," and ", ylabel," condition) are normally distributed. If the normality assumption is violated, the Type 1 error rate of the test is no longer controlled, and can substantially increase beyond the chosen significance level. Formally, a normality test based on the data is incorrect, and the normality assumption should be tested on additional (e.g., pilot) data. Nevertheless, a two-step procedure (testing the data for normality, and using alternatives for the traditional *t*-test if normality is violated) works well (see [Rochon, Gondan, & Kieser, 2012](http://www.biomedcentral.com/1471-2288/12/81))."),

                "### Tests for normality",

                "[Yap and Sim (2011, p. 2153)](http://www.tandfonline.com/doi/pdf/10.1080/00949655.2010.520163) recommend: \"If the distribution is symmetric with low kurtosis values (i.e. symmetric short-tailed distribution), then the D'Agostino-Pearson and Shapiro-Wilkes tests have good power. For symmetric distribution with high sample kurtosis (symmetric long-tailed), the researcher can use the JB, Shapiro-Wilkes, or Anderson-Darling test.\" The Kolmogorov-Smirnov (K-S) test is often used, but no longer recommended, and not included here.",
                "  ",
                "If a normality test rejects the assumptions that the data is normally distributed (with *p* < .05) non-parametric or robust statistics have to be used (robust analyses are provided below).",  
                "  ",
                paste0("**The normality assumption was rejected in ", normalityrejections," out of 4 normality tests (Anderson-Darling, D'Agostino-Pearson, and Shapiro-Wilk).**"),
                "  ",
                paste0("Test Name  | *p*-value"), 
                paste0("------------- | :-------------:"),
                paste0("Shapiro-Wilk  | *p* ", ifelse(statcompute(21, diff, levels = c(0.05))$pvalue>=0.001," = ", " < ")," ", ifelse(statcompute(21, diff, levels = c(0.05))$pvalue>=0.001, round(statcompute(21, diff, levels = c(0.05))$pvalue, digits=3), "0.001")),
                paste0("D'Agostino-Pearson  | *p* ", ifelse(statcompute(6, diff, levels = c(0.05))$pvalue>0.001," = ", " < ")," ", ifelse(statcompute(6, diff, levels = c(0.05))$pvalue>0.001, round(statcompute(6, diff, levels = c(0.05))$pvalue, digits=3), "0.001")),
                paste0("Anderson-Darling  | *p* ", ifelse(statcompute(2, diff, levels = c(0.05))$pvalue>0.001," = ", " < ")," ", ifelse(statcompute(2, diff, levels = c(0.05))$pvalue>0.001, round(statcompute(2, diff, levels = c(0.05))$pvalue, digits=3), "0.001")),
                paste0("Jarque-Berra  | *p* ", ifelse(statcompute(7, diff, levels = c(0.05))$pvalue>0.001," = ", " < ")," ", ifelse(statcompute(7, diff, levels = c(0.05))$pvalue>0.001, round(statcompute(7, diff, levels = c(0.05))$pvalue, digits=3), "0.001")),
                "   ",
                "   ",
                "In very large samples (when the test for normality has close to 100% power) tests for normality can result in significant results even when data is normally distributed, based on minor deviations from normality. In very small samples (e.g., n = 10), deviations from normality might not be detected, but this does not mean the data is normally distributed.  Always look at a plot of the data in addition to the test results.",

                "### Histogram, kernel density plot (black line) and normal distribution (red line) of difference scores",

                "The density (or proportion of the observations) is plotted on the y-axis. The grey bars are a histogram of the difference scores. Judging whether data is normally distributed on the basis of a histogram depends too much on the number of bins (or bars) in the graph. A kernel density plot (a non-parametric technique for density estimation) provides an easier way to check the normality of the data by comparing the shape of the density plot (the black line) with a normal distribution (the red dotted line, based on the observed mean and standard deviation). For dependent t-tests, the main DV is the *difference score*, and therefore the difference score should be normally distributed."
            )
        }

        html_out <- renderMarkdown(text = report)
        return(html_out)
    })

    output$hist_plot      <- renderPlot({
        
        if (input$test_type == "independent") {
        
            ### loading the data
            data_user <- get_proc_data()
            x_data_proc <- data_user$x_data_proc
            y_data_proc <- data_user$y_data_proc
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            
            
            ### density plot with normal distribution (red) and kernel desity plot
            plot_x <- 
                ggplot(x_data_proc, aes_string(x=measurelabel))  + 
                geom_histogram(colour="black", fill="grey", aes(y = ..density..)) +
                stat_function(fun = dnorm, args = c(mean=mean(x), sd=sd(x)), size = 1, color = "red", lty=2) +
                geom_density(fill=NA, colour="black", size = 1) +
                xlab(measurelabel)  + 
                ggtitle(xlabel) + 
                theme_bw(base_size=14) + 
                theme(panel.grid.major.x = element_blank(), 
                    panel.grid.minor.x = element_blank())

            plot_y <- ggplot(y_data_proc, aes_string(x=measurelabel))  + 
                  geom_histogram(colour="black", fill="grey", aes(y = ..density..)) +
                  stat_function(fun = dnorm, args = c(mean=mean(y), sd=sd(y)), size = 1, color = "red", lty=2) +
                  geom_density(fill=NA, colour="black", size = 1) +
                  xlab(measurelabel)  + 
                  ggtitle(ylabel) + 
                  theme_bw(base_size=14) + 
                  theme(panel.grid.major.x = element_blank(), 
                        panel.grid.minor.x = element_blank())

            figure <- grid.arrange(plot_x, plot_y, ncol=2)
            
            return(figure)

        } else if (input$test_type == "dependent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            diff <- data_proc[["diff"]]
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            alpha <- input$alpha
            H1 <- input$alt_hyp
            ConfInt <- input$conf_int


            #density plot with normal distribution (red) and kernel desity plot
            plot1 <- ggplot(data_proc, aes(x=diff))  + 
              geom_histogram(colour="black", fill="grey", 
                             aes(y = ..density..)) +
              stat_function(fun = dnorm, args = c(mean=mean(data_proc$diff), 
                            sd=sd(data_proc$diff)), size = 1, color = "red", 
                            lty=2) +
              geom_density(fill=NA, colour="black", size = 1) +
              xlab(ylabelstring) + 
              ggtitle("Difference scores") + 
              theme_bw(base_size=14) + 
              theme(panel.grid.major.x = element_blank(), 
                    panel.grid.minor.x = element_blank())


            #density plot with normal distribution (red) and kernel desity plot
            plot2 <- ggplot(data_proc, aes_string(x=xlabel))  + 
              geom_histogram(colour="black", fill="grey", 
                             aes(y = ..density..)) +
              stat_function(fun = dnorm, args = c(mean=mean(x), sd=sd(x)), 
                            size = 1, color = "red", lty=2) +
              geom_density(fill=NA, colour="black", size = 1) +
              xlab(ylabelstring)  + 
              ggtitle(xlabel) + 
              theme_bw(base_size=14) + 
              theme(panel.grid.major.x = element_blank(), 
                    panel.grid.minor.x = element_blank())

            #density plot with normal distribution (red) and kernel desity plot
            plot3 <- ggplot(data_proc, aes_string(x=ylabel))  + 
              geom_histogram(colour="black", fill="grey", 
                             aes(y = ..density..)) +
              stat_function(fun = dnorm, args = c(mean=mean(y), sd=sd(y)), 
                            size = 1, color = "red", lty=2) +
              geom_density(fill=NA, colour="black", size = 1) +
              xlab(ylabelstring) + 
              ggtitle(ylabel) + 
              theme_bw(base_size=14) + 
              theme(panel.grid.major.x = element_blank(), 
                    panel.grid.minor.x = element_blank())

            return(grid.arrange(plot1, plot2, plot3, ncol=3))
        }
    })

    output$qqplot_text    <- renderText({

        xlabel <- input$xlabel
        ylabel <- input$ylabel
        
        if (input$test_type == "independent") {
            report <- c(
                "### Q-Q-plot",
                paste0("In the Q-Q plots for the ", xlabel, " and ", ylabel, " conditions the points should fall on the line. Deviations from the line in the upper and lower quartiles indicates the tails of the distributions are thicker or thinner than in the normal distribution. An S-shaped curve with a dip in the middle indicates data is left-skewed (more values to the right of the distribution), while a bump in the middle indicates data is right-skewed (more values to the left of the distribution). For interpretation examples, see [here](http://emp.byui.edu/BrownD/Stats-intro/dscrptv/graphs/qq-plot_egs.htm).")
            )
        } else if (input$test_type == "dependent") {
            report <- c(
                "In the Q-Q plot for the difference scores the points should fall on the line. Deviations from the line in the upper and lower quartiles indicates the tails of the distributions are thicker or thinner than in the normal distribution. An S-shaped curve with a dip in the middle indicates data is left-skewed (more values to the right of the distribution), while a bump in the middle indicates data is right-skewed (more values to the left of the distribution). For interpretation examples, see [here](http://emp.byui.edu/BrownD/Stats-intro/dscrptv/graphs/qq-plot_egs.htm)."
            )
        }

        html_out <- renderMarkdown(text = report)
        return(html_out)
    })

    output$qq_plot        <- renderPlot({
        
        if (input$test_type == "independent") {

            # loading the data
            data_user <- get_proc_data()
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring

            
            ### Q-Q plot
            qq_x <- ggplot_qqnorm(x, line = "quantile") + 
                ggtitle(xlabel) + 
                theme_bw(base_size=14) + 
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())

            qq_y <- ggplot_qqnorm(y, line = "quantile") + 
                ggtitle(ylabel) + 
                theme_bw(base_size=14) + 
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
            
            figure <- grid.arrange(qq_x, qq_y, ncol=2)

            return(figure)

        } else if (input$test_type == "dependent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            diff <- data_proc[["diff"]]
            x <- data_user$x
            y <- data_user$y
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            

            #Q-Q plot
            qq1 <- ggplot_qqnorm(diff, line = "quantile") + 
                ggtitle("Difference scores") + 
                theme_bw(base_size=14)  + 
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())

            qq2 <- ggplot_qqnorm(x, line = "quantile") + 
                ggtitle(xlabel) + 
                theme_bw(base_size=14)  + 
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
            
            qq3 <- ggplot_qqnorm(y, line = "quantile") + 
                ggtitle(ylabel) + 
                theme_bw(base_size=14)  + 
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())

            figure <- grid.arrange(qq1, qq2, qq3, ncol=3)

            return(figure)
        }
    })

    output$eqvar_text     <- renderText({
        
        if (input$test_type == "independent") {

            # loading the data
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring


            ### Testing equality of variances

            pvalueLevene <- leveneTest(data_proc[[measurelabel]] ~ as.factor(data_proc[[factorlabel]]))$"Pr(>F)"[1:1]
            if (pvalueLevene < 0.05) {
                equalvar <- "the assumption that variances are equal is rejected (consider reporting robust statistics)."
            } else if (pvalueLevene >= 0.05) {
                equalvar<-"the assumption that variances are equal is not rejected."
            }

            report <- c(
                "## Equal variances assumption",

                "In addition to the normality assumption, a second assumption of Student's *t*-test is that variances in both groups are equal. As [Ruxton (2006)](http://beheco.oxfordjournals.org/content/17/4/688.full) explains: \"If you want to compare the central tendency of 2 populations based on samples of unrelated data, then the unequal variance (or Welch's) *t*-test should always be used in preference to the Student's *t*-test or Mann-Whitney U test.\" This is preferable to the more traditional two-step approach of first testing equality of variances using Levene's test, and then deciding between Student's and Welch's *t*-test. The degrees of freedom for Welch's *t*-test is typically not a round number.",

                "### Levene's test",

                paste0("The equality of variances assumption is typically examined with Levene's test, although as explained above, Welch's test is used below regardless of the outcome. Levene's test for equality of variances (*p* ", ifelse(pvalueLevene>0.001,' = ', ' < ')," ", ifelse(pvalueLevene>0.001,round(pvalueLevene, digits=3), '0.001'),") indicates that ", equalvar)
            )
        } else if (input$test_type == "dependent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc_long <- data_user$data_proc_long
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel  
            sd1<-sd(x) #standard deviation of group 1
            sd2<-sd(y) #standard deviation of group 2

            # Testing equality of variances
            pvalueLevene <- leveneTest(data_proc_long[[measurelabel]] ~ as.factor((data_proc_long[[factorlabel]])))$"Pr(>F)"[1:1]
            if (pvalueLevene < 0.05) {
                equalvar <- "the assumption that variances are equal is rejected (consider reporting robust statistics)."
            } else if (pvalueLevene >= 0.05) {
                equalvar <- "the assumption that variances are equal is not rejected."
            }
            cat("Levene's test for equality of variances (p = ", round(pvalueLevene, digits=2),") indicates that ",equalvar,sep="")

            # finally, sreating a report
            report <- c(
                "## Equal variances assumption",

                paste0("In addition to the normality assumption, a second assumption of the *t*-test is that variances in both groups are equal. The variance is the standard deviation, squared, and the assumption is thus that the variance in the ", xlabel," condition (", round(sd1^2, digits = 2),") equals that in the ", ylabel," condition (", round(sd2^2, digits = 2),"). [Markowski & Markowski (1990)](http://www.jstor.org/stable/2684360) show that if sample sizes are equal, violations of the equal variance assumption do not lead to unsatisfactory performance (defined as actual significance levels falling outside a 0.03-0.07 boundary for a nominal alpha level of 0.05)."), 

                "### Levene's test",

                paste0("This equality of variances assumption is typically examined with Levene's test, although in small samples, Levene's test can have low power, and thus fail to reject the null-hypothesis that variances are equal, even when they are unequal. Levene's test for equality of variances (*p* ", ifelse(pvalueLevene>0.001,' = ', ' < ')," ", ifelse(pvalueLevene>0.001, round(pvalueLevene, digits=3), 0.001),") indicates that ", equalvar)
            )
        }

        html_out <- renderMarkdown(text = report)
        return(html_out)
    })

    # ----
    # Frequentist tab
    # ----
    
    output$ttestOut       <- renderText({
        
        validate_test_input()

        # creating a report depending on the type of test
        if (input$test_type == "independent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            alpha <- input$alpha
            H1 <- input$alt_hyp
            ConfInt <- input$conf_int

            # Basic stats
            sd1<-sd(x) #standard deviation of group 1
            sd2<-sd(y) #standard deviation of group 2
            n1 <- length(x) #number of individuals
            n2 <- length(y) #number of individuals
            m_diff<-mean(x)-mean(y)
            
            # Always performs Welch's t-test for unequal variances which is better than Levene's test followed by Student's t-test
            ttestresult<-t.test(x, y, alternative = H1, paired = FALSE, var.equal = FALSE, conf.level = ConfInt)
            tvalue<-ttestresult$statistic  # store t-value from dependent t-test
            pvalue<-ttestresult$p.value  # store p-value from dependent t-test
            CI_diff<-ttestresult$conf.int  # store confidence interval of mean difference
            s_av <- sqrt((sd1^2+sd2^2)/2)  # calculate average standard deviation for effect size calculation

            # Specify direction of difference
            if (mean(x) > mean(y)) {direction <- "greater than"}
            if (mean(x) < mean(y)) {direction <- "smaller than"}
            if (pvalue < alpha) {surprising <- "surprising"}
            if (pvalue >= alpha) {surprising <- " not surprising"}

            ### Cohen's d
            d <- smd(Mean.1= mean(x), Mean.2=mean(y), s.1=sd(x), s.2=sd(y), n.1=n1, n.2=n2, Unbiased=TRUE) #Use MBESS to calc d unbiased (Hedges g)
            if (is.finite(d) == FALSE) {
                d <- smd(Mean.1= mean(x), Mean.2=mean(y), s.1=sd(x), s.2=sd(y), n.1=n1, n.2=n2, Unbiased=FALSE)
            }  # In large samples, smd function gives error when Unbiased=TRUE. Difference in d and g no longer noticable, so then unbiased d is calculated.

            ci_l_d <- ci.smd(ncp = tvalue, n.1 = n1, n.2 = n2, conf.level=1-.05)$Lower.Conf.Limit.smd
            ci_u_d <- ci.smd(ncp = tvalue, n.1 = n1, n.2 = n2, conf.level=1-.05)$Upper.Conf.Limit.smd

            # Common Langaue Effect Size (McGraw & Wong, 1992)
            CL <- pnorm(abs(m_diff)/sqrt(sd1^2+sd2^2))

            # Interpret size of effect (last resort - use only if effect size cannot be compared to other relevant effects in the literature)
            if (abs(d) < 0.2) {effectsize <- "tiny"}
            if (0.2 <= abs(d) && abs(d) < 0.5) {effectsize <- "small"}
            if (0.5 <= abs(d) && abs(d) < 0.8) {effectsize <- "medium"}
            if (abs(d) >= 0.8) {effectsize <- "large"}

            # finally, creating a report
            report <- c(
                "## Frequentist statistics",
                paste0("A *p*-value is the probability of obtaining the observed result, or a more extreme result, assuming the null-hypothesis is true. It is not the probability that the null-hypothesis or the alternative hypothesis is true (for such inferences, see Bayesian statistics below). In repeated sampling, ", 100*ConfInt,"% of future ", 100*ConfInt, "% confidence intervals can be expected to contain the true population paramters (e.g, the mean difference or the effect size). Confidence intervals are not a statement about the probability that a single confidence interval contains the true population parameter, but a statement about the probability that future confidence intervals will contain the true population parameter. Hedges' *g* (also referred to as *d*~unbiased~, see Borenstein, Hedges, Higgins, & Rothstein, 2009) is provided as best estimate of Cohen's *d*, but the best estimate of the confidence interval is based on *d* (as recommended by Cumming, 2012). Hedges's *g* and the ", 100*ConfInt, "% CI around the effect size are calculated using the MBESS package by ([Kelley (2007](http://dx.doi.org/10.3758/BF03192993)). The common language effect size expresses the probability that in any random pairing of two observations from both groups, the observation from one group is higher than the observation from the other group, see [McGraw & Wong, 1992](http://dx.doi.org/10.1037/0033-2909.111.2.361). Default interpretations of the size of an effect as provided here should only be used as a last resort, and it is preferable to interpret the size of the effect in relation to other effects in the literature, or in terms of its practical significance."),
                "### Results",
                paste0("The mean ", ylabelstring," of participants in the ", xlabel," condition (*M* = ", round(mean(x), digits = 2),", *SD* = ", round(sd1, digits = 2),", *n* = ", n1,") was ", direction," the mean of participants in the ", ylabel," condition (*M* = ", round(mean(y), digits = 2),", *SD* = ", round(sd2,digits=2),", *n* = ", n2,"). The difference between the two measurements (*M* = ", round(m_diff, digits=2),", ", 100*ConfInt,"% CI = [", round(CI_diff[1:1], digits=2),";", round(CI_diff[2:2],digits=2),"]) was analyzed with Welch's *t*-test, *t*(", round(ttestresult$parameter, digits=2),") = ", round(tvalue, digits=2),", *p* ", ifelse(pvalue>0.001,' = ', ' < ')," ", ifelse(pvalue>0.001,formatC(round(pvalue, digits=3),digits=3, format='f'), '0.001'),", Hedges' *g* = ", round(d, digits=2),", ", 100*ConfInt,"% CI [", round(ci_l_d, digits=2),";", round(ci_u_d, digits=2),"]. This can be considered a ", effectsize," effect. The observed data is ", surprising," under the assumption that the null-hypothesis is true. The Common Language effect size (McGraw & Wong, 1992) indicates that the likelihood that the ", ylabelstring," of a random person in the ", xlabel," condition is ", direction," the ", ylabelstring," of a random person in the ", ylabel," condition is ", round(100*CL, digits=0),"%.")
            )
        } else if (input$test_type == "dependent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            diff <- data_proc[["diff"]]
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            alpha <- input$alpha
            H1 <- input$alt_hyp
            ConfInt <- input$conf_int

            # Calculate based on data
            r <- cor(x, y)  # correlation between dependent measures
            m_diff <- mean(diff)  # mean of difference scores
            s_diff <- sd(diff)  # standard deviation of difference scores
            sd1 <- sd(x)  # standard deviation of group 1
            sd2 <- sd(y)  # standard deviation of group 2
            N <- length(x)  # number of pairs

            ttestresult <- t.test(x, y, alternative = H1, paired = TRUE, var.equal = TRUE, conf.level = ConfInt)
            tvalue <- ttestresult$statistic  # store t-value from dependent t-test
            pvalue <- ttestresult$p.value  # store p-value from dependent t-test
            CI_diff <- ttestresult$conf.int  # store confidence interval of mean difference
            s_av <- sqrt((sd1^2+sd2^2)/2)  # calculate average standard deviation for effect size calculation

            # Specify direction of difference
            if (mean(x)>mean(y)) {direction  <- "greater than"}
            if (mean(x)<mean(y)) {direction  <- "smaller than"}
            if (pvalue < alpha)  {surprising <- "surprising"}
            if (pvalue >= alpha) {surprising <- "not surprising"}


            ### Effect sizes and 95% CI
            
            # Cohen's d_av, using s_av as standardizer
            d_av <- m_diff/s_av
            d_unb <- (1-(3/(4*(N-1)-1)))*d_av  # note this is approximation of correction for Hedges'g - ESCI uses accurate correction, so should we.
            nct_limits <- conf.limits.nct(t.value = tvalue, df=N-1, conf.level = ConfInt)
            ci_l_d_av <- nct_limits$Lower.Limit*s_diff/(s_av*sqrt(N))
            ci_u_d_av <- nct_limits$Upper.Limit*s_diff/(s_av*sqrt(N))

            # Interpret size of effect (last resort - use only if effect size cannot be compared to other relevant effects in the literature)
            if (abs(d_av) < 0.2){effectsize <- "tiny"}
            if (0.2 <= abs(d_av) && abs(d_av) < 0.5){effectsize <- "small"}
            if (0.5 <= abs(d_av) && abs(d_av) < 0.8){effectsize <- "medium"}
            if (abs(d_av) >= 0.8){effectsize <- "large"}

            # Cohen's d_z, using s_diff as standardizer
            d_z <- tvalue/sqrt(N)
            ci_l_d_z <- nct_limits$Lower.Limit/sqrt(N)  # Not sure about this formula, but gives same results as Wuensch's files
            ci_u_d_z <- nct_limits$Upper.Limit/sqrt(N)  # Not sure about this formula

            # Common Langaue Effect Size (McGraw & Wong, 1992)
            CL <- pnorm(abs(m_diff/s_diff))

            # finally, creating a report
            report <- c(
                "## Frequentist statistics",

                paste0("A *p*-value is the probability of obtaining the observed result, or a more extreme result, assuming the null-hypothesis is true. It is not the probability that the null-hypothesis or the alternative hypothesis is true (for such inferences, see Bayesian statistics below). In repeated sampling, ", 100*ConfInt,"% of future ", 100*ConfInt,"% confidence intervals can be expected to contain the true population parameters (e.g, the mean difference or the effect size). Confidence intervals are not a statement about the probability that a single confidence interval contains the true population parameter, but a statement about the probability that future confidence intervals will contain the true population parameter. Hedges' *g* (also referred to as *d*~unbiased~, see Borenstein, Hedges, Higgins, & Rothstein, 2009) is provided as best estimate of Cohen's *d*, but the best estimate of the confidence interval is based on *d*~av~ (as recommended by Cumming, 2012). Hedges's *g* and the ", 100*ConfInt,"% CI around the effect size are calculated using the MBESS package by ([Kelley (2007](http://dx.doi.org/10.3758/BF03192993)). The common language effect size expresses the probability that in any random pairing of two observations from both groups, the observation from one group is higher than the observation from the other group, see [McGraw & Wong, 1992](http://dx.doi.org/10.1037/0033-2909.111.2.361). In a dependent *t*-test, the effect size Cohen's *d* can be calculated by using a standardizer that controls for the correlation between observations (*d*~av~) or not (*d*~z~). Both are provided, but *d*~av~ (or actually it's unbiased estimate, *g*~av~) is recommended. For a discussion, see [Lakens, 2013](http://journal.frontiersin.org/Journal/10.3389/fpsyg.2013.00863/full). Default interpretations of the size of an effect as provided here should only be used as a last resort, and it is preferable to interpret the size of the effect in relation to other effects in the literature, or in terms of its practical significance."),

                "### Results",

                paste0("The mean ", ylabelstring," of participants in the ", xlabel," condition (*M* = ", round(mean(x), digits = 2),", *SD* = ", round(sd1, digits = 2),")  was ", direction," the mean of participants in the ", ylabel," condition (*M* = ", round(mean(y), digits = 2),", *SD* = ", round(sd2,digits=2),", *r* = ", round(r, digits = 2),"). The difference between measurements (*M* = ", round(m_diff, digits=2),", *SD* = ", round(s_diff, digits=2),", ", 100*ConfInt,"% CI = [", round(CI_diff[1:1], digits=2),";", round(CI_diff[2:2],digits=2),"]) was analyzed with a dependent *t*-test, *t*(", round(ttestresult$parameter, digits=2),") = ", round(tvalue, digits=2),", *p* ", ifelse(pvalue>0.001," = ", " < ")," ", ifelse(pvalue>0.001, formatC(round(pvalue, digits=3),digits=3, format="f"), "0.001"),", Hedges' *g* = ", round(d_unb, digits=2),", ", 100*ConfInt,"% CI [", round(ci_l_d_av, digits=2),";", round(ci_u_d_av, digits=2),"] (or *d*~z~ = ", round(d_z, digits=2),", ", 100*ConfInt,"% CI [", round(ci_l_d_z, digits=2),";", round(ci_u_d_z, digits=2),"]). This can be considered a ", effectsize," effect. The observed data is ", surprising," under the assumption that the null-hypothesis is true. The Common Language effect size (McGraw & Wong, 1992) indicates that after controlling for individual differences, the likelihood that a persons ", ylabelstring," in the ", xlabel," condition is ", direction," the ", ylabelstring," in the ", ylabel," condition is ", round(100*CL, digits=0),"%.")
            )
        }

        html_out <- renderMarkdown(text = report)
        return(html_out)
    })

    # ----
    # Bayesian tab
    # ----
    
    output$bayesOut       <- renderText({
        
        validate_test_input()

        # creating a report, depending on type of the test
        if (input$test_type == "independent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            H1 <- input$alt_hyp
            ConfInt <- input$conf_int
            InAHurry <- input$InAHurry
            BFrscale <- input$BFrscale

            # basic stats
            sd1<-sd(x) #standard deviation of group 1
            sd2<-sd(y) #standard deviation of group 2
            n1 <- length(x) #number of individuals
            n2 <- length(y) #number of individuals
            m_diff<-mean(x)-mean(y)

            # getting the t-value from a t-test
            ttestresult <- t.test(x, y, alternative = H1, paired = FALSE, var.equal = FALSE, conf.level = ConfInt)
            tvalue <- ttestresult$statistic


            ### BayesFactor
            if (H1 == "two.sided") {
              BF <- ttest.tstat(t = tvalue, n1 = n1, n2 = n2, 
                                rscale = BFrscale, simple=TRUE)   
            } else if (H1 == "greater") {
              BF <- ttest.tstat(t = tvalue, n1 = n1, n2 = n2, nullInterval = c(0, Inf), rscale = BFrscale, simple = TRUE)
            } else if (H1 == "less") {
              BF <- ttest.tstat(t = tvalue, n1 = n1, n2 = n2, nullInterval = c(-Inf, 0), rscale = BFrscale, simple = TRUE)
            }
            if (BF != Inf) {round(BF, digits=2)}
            if (BF == Inf) {BF <- "practically infinitely high"}

            ### BEST
            if (!InAHurry) {
                BESTout <- BESTmcmc(x, y)
                BESTdiff  <-  BESTout$mu1 - BESTout$mu2
                BESTHDI <- hdi(BESTdiff, credMass = ConfInt)
                mu <- mean(BESTdiff)
                HDI_l <- BESTHDI[1]
                HDI_u <- BESTHDI[2]
                Rhat <- attr(BESTout, "Rhat")
                neff <- attr(BESTout, "n.eff")
                ifelse(Rhat[1]<1.1 && Rhat[1]<1.1 && neff[1]>10000 && neff[2]>10000, BESTacceptable<-"acceptable", BESTacceptable<-"not acceptable - check the HDI calculation")
            } else {
                mu <- "NOT CALCULATED DUE TO TIME CONSTRAINTS"
                HDI_l <- "NOT CALCULATED"
                HDI_u <- "NOT CALCULATED"
            }
            

            ### Interpret strength of evidence of Bayes Factor following Jeffreys (1961)
            # this could be placed in a separate function
            if (0.33 < BF && BF <= 1){evidence <- "anecdotal evidence for H0"}
            if (0.1 < BF && BF <=0.33){evidence <- "moderate evidence for H0"}
            if (0.03 < BF && BF <= 0.1){evidence <- "strong evidence for H0"}
            if (0.01 < BF && BF <= 0.03){evidence <- "very strong evidence for H0"}
            if (BF <=0.01){evidence <- "decisive evidence for H0"}
            if (1 < BF && BF <= 3){evidence <- "anecdotal evidence for H1"}
            if (3 < BF && BF <=10){evidence <- "moderate evidence for H1"}
            if (10 < BF && BF <= 30){evidence <- "strong evidence for H1"}
            if (30 < BF && BF <= 100){evidence <- "very strong evidence for H1"}
            if (BF > 100){evidence <- "decisive evidence for H1"}

            ### finally, creating a report
            report <- c(
                "## Bayesian statistics",

                paste0("Bayesian statistics can quantify the relative evidence in the data for either the alternative hypothesis or the null hypothesis. Bayesian statistics require priors to be defined. In the Bayes Factor calculation reported below, a non-informative Jeffreys prior is placed on the variance of the normal population, while a Cauchy prior is placed on the standardized effect size (for details, [see Morey & Rouder, 2011](http://drsmorey.org/bibtex/upload/Morey:Rouder:2011.pdf)). Calculations are performed using the [BayesFactor package](http://cran.r-project.org/web/packages/BayesFactor/BayesFactor.pdf). For a detailed explanation of an independent *t*-test, see [this post by Richard Morey](http://bayesfactor.blogspot.nl/2014/02/bayes-factor-t-tests-part-2-two-sample.html). Default interpretations of the strength of the evidence are provided but should not distract from the fact that strength of evidence is a continuous function of the Bayes Factor. A second popular Bayesian approach relies on estimation, and the mean posterior and ", 100*ConfInt, "% higest density intervals (HDI) are calculated following recommendations by [Kruschke, (2013)](http://www.indiana.edu/~kruschke/BEST/BEST.pdf) based on vague priors. According to Kruschke (2010, p. 34): 'The HDI indicates which points of a distribution we believe in most strongly. The width of the HDI is another way of measuring uncertainty of beliefs. If the HDI is wide, then beliefs are uncertain. If the HDI is narrow, then beliefs are fairly certain.' To check the convergence and fit of the HDI simulations, the Brooks-Gelman-Rubin scale reduction factor for both groups should be smaller than 1.1 (For ", xlabel," : ", ifelse (!InAHurry, Rhat[1], 'NOT CALCULATED'),", and for ", ylabel,": ", ifelse (!InAHurry, Rhat[2], 'NOT CALCULATED'),") and the effective sample size should be larger than 10000 (for ", xlabel,":  ", ifelse (!InAHurry,round(neff[1]), 'NOT CALCULATED'),", and for ",  ylabel,": ", ifelse (!InAHurry,round(neff[2]), 'NOT CALCULATED'),"). Thus, the HDI simulation is ", ifelse (!InAHurry, BESTacceptable, 'NOT CALCULATED'),"."),

                "### Results",

                paste0("The JZS \\(BF_{10}\\) (with r scale = ", BFrscale,") = ", round(BF, digits=2),". This indicates the data are ", round(BF, digits=2)," (or \\(log_e\\) BF =", round(log(BF), digits=2),") times more likely under the alternative hypothesis, than under the null hypothesis. This data provides ", evidence,". The posterior mean difference is ", ifelse(!InAHurry,round(mu, digits=2), 'NOT CALCULATED'),", ", 100*ConfInt,"% HDI = [", ifelse(!InAHurry,round(HDI_l, digits=2), 'NOT CALCULATED'),"; ", ifelse(!InAHurry,round(HDI_u, digits=2), 'NOT CALCULATED'),"].")
            )
        } else if (input$test_type == "dependent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            diff <- data_proc[["diff"]]
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            H1 <- input$alt_hyp
            ConfInt <- input$conf_int
            InAHurry <- input$InAHurry
            BFrscale <- input$BFrscale

            ### BayesFactor
            
            # We need t-test results first
            N <- length(x)  
            ttestresult <- t.test(x, y, alternative = H1, paired = TRUE, 
                                  var.equal = TRUE, conf.level = ConfInt)
            tvalue <- ttestresult$statistic  # store t-value from dependent t-test

            # computing BF
            if (H1 == "two.sided") {
              BF <- ttest.tstat(t = tvalue, n1 = N, rscale = BFrscale, simple=TRUE)
            } else if (H1 == "greater") {
              BF <- ttest.tstat(t = tvalue, n1 = N, nullInterval = c(0, Inf), rscale = BFrscale, simple=TRUE)
            } else if (H1 == "less") {
              BF <- ttest.tstat(t = tvalue, n1 = N, nullInterval = c(-Inf, 0), rscale = BFrscale, simple=TRUE)
            }

            if (BF != Inf) {round(BF, digits=2)}
            if (BF == Inf) {BF <- "practically infinitely high"}

            ### calculating HIB-BEST
            if (!InAHurry) {
                BESTout1g <- BESTmcmc(diff)
                BESTHDI <- hdi(BESTout1g$mu, credMass = ConfInt)
                mu <- summary(BESTout1g)[1,1]
                HDI_l <- BESTHDI[1]
                HDI_u <- BESTHDI[2]
                Rhat <- attr(BESTout1g, "Rhat")
                neff <- attr(BESTout1g, "n.eff")
                ifelse(Rhat[1]<1.1 && neff[1]>10000, 
                    BESTacceptable <- "acceptable", 
                    BESTacceptable <- "not acceptable - check the HDI calculation")  # user cannot due anything with this outcome...
            } else {
                mu <- "NOT CALCULATED DUE TO TIME CONSTRAINTS"
                HDI_l <- "NOT CALCULATED"
                HDI_u <- "NOT CALCULATED"
            }

            
            ### Interpret strength of evidence of Bayes Factor following Jeffreys (1961)
            # this could be placed in a separate function
            if (0.33 < BF && BF <= 1){evidence <- "anecdotal evidence for H0"}
            if (0.1 < BF && BF <=0.33){evidence <- "moderate evidence for H0"}
            if (0.03 < BF && BF <= 0.1){evidence <- "strong evidence for H0"}
            if (0.01 < BF && BF <= 0.03){evidence <- "very strong evidence for H0"}
            if (BF <=0.01){evidence <- "decisive evidence for H0"}
            if (1 < BF && BF <= 3){evidence <- "anecdotal evidence for H1"}
            if (3 < BF && BF <=10){evidence <- "moderate evidence for H1"}
            if (10 < BF && BF <= 30){evidence <- "strong evidence for H1"}
            if (30 < BF && BF <= 100){evidence <- "very strong evidence for H1"}
            if (BF > 100){evidence <- "decisive evidence for H1"}

            ### finally, creating a report
            report <- c(
                "## Bayesian statistics",

                paste0("Bayesian statistics can quantify the relative evidence in the data for either the alternative hypothesis or the null hypothesis. Bayesian statistics require priors to be defined. In the Bayes Factor calculation reported below, a non-informative Jeffreys prior is placed on the variance of the normal population, while a Cauchy prior is placed on the standardized effect size (for details, [see Morey & Rouder, 2011](http://drsmorey.org/bibtex/upload/Morey:Rouder:2011.pdf)). Calculations are performed using the [BayesFactor package](http://cran.r-project.org/web/packages/BayesFactor/BayesFactor.pdf). Default interpretations of the strength of the evidence are provided but should not distract from the fact that strength of evidence is a continuous function of the Bayes Factor. A second popular Bayesian approach relies on estimation, and the mean posterior and ", 100*ConfInt,"% higest density intervals (HDI) are calculated following recommendations by [Kruschke, (2013)](http://www.indiana.edu/~kruschke/BEST/BEST.pdf) based on vague priors. According to Kruschke (2010, p. 34): 'The HDI indicates which points of a distribution we believe in most strongly. The width of the HDI is another way of measuring uncertainty of beliefs. If the HDI is wide, then beliefs are uncertain. If the HDI is narrow, then beliefs are fairly certain.' To check the convergence and fit of the HDI simulations, the Brooks-Gelman-Rubin scale reduction factor for the difference score should be smaller than 1.1 (it is ", ifelse (!InAHurry, Rhat[1], 'NOT CALCULATED'),") and the effective sample size should be larger than 10000 (it is ", ifelse (!InAHurry, round(neff[1]), 'NOT CALCULATED'),"). Thus, the HDI simulation is ", ifelse (!InAHurry,BESTacceptable, 'NOT CALCULATED'),"."),

                "### Results",

                paste0("The JZS \\(BF_{10}\\) (with r scale = ", BFrscale,") = ", round(BF, digits=2),". This indicates the data are ", round(BF, digits = 2)," (or \\(log_e\\) BF =", round(log(BF), digits=2),") times more probable under the alternative hypothesis, than under the null hypothesis. This data provides ", evidence,". The posterior mean difference is ", ifelse(!InAHurry, round(mu, digits=2), 'NOT CALCULATED'),", ", 100*ConfInt,"% HDI = [", ifelse(!InAHurry, round(HDI_l, digits=2), 'NOT CALCULATED'),"; ", ifelse(!InAHurry, round(HDI_u, digits=2), 'NOT CALCULATED'),"].")
            )
        }

        html_out <- renderMarkdown(text = report)
        return(html_out)
    })

    # ----
    # Robust stats tab
    # ----
    
    output$robustOut      <- renderText({
       
        validate_test_input()

        # creating a report
        if (input$test_type == "independent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            H1 <- input$alt_hyp
            ConfInt <- input$conf_int
            InAHurry <- input$InAHurry
            alpha <- input$alpha
            bootstraps <- input$bootstraps

            
            ### Robust Statistics

            yuentest <- yuenbt(x, y, tr=0.2, alpha=1-ConfInt, nboot=599, side=T)  #for details of this function, see Wilcox, 2012, p. 163).

            # Specify direction of difference
            if (yuentest$est.1 > yuentest$est.2) {direction2 <- "greater than"}
            if (yuentest$est.1 < yuentest$est.2) {direction2 <- "smaller than"}
            if (yuentest$p.value < alpha) {surprising2 <- "surprising"}
            if (yuentest$p.value >= alpha) {surprising2 <- " not surprising"}


            ### Robust d (d_t) based on Algina, Keselman, and Penfield (2005). 
            if(!InAHurry) {
                d_robust_sum <- bootES(data_proc, R=bootstraps, data.col = measurelabel, group.col = factorlabel, contrast = c(xlabel, ylabel), effect.type = "akp.robust.d")
                d_robust <- d_robust_sum$t0
                d_robust_ci_l <- d_robust_sum$bounds[1:1]
                d_robust_ci_u <- d_robust_sum$bounds[2:2]
            } else {
                d_robust <- "NOT CALCULATED DUE TO TIME CONSTRAINTS"
                d_robust_ci_l <- "NOT CALCULATED"
                d_robust_ci_u <- "NOT CALCULATED"
            }
            
            ### Interpret size of effect (last resort - use only if effect size cannot be compared to other relevant effects in the literature)
            if(!InAHurry) {
                if (abs(d_robust) < 0.2) {
                    effectsize2 <- "tiny"
                } else if (0.2 <= abs(d_robust) && abs(d_robust) < 0.5) {effectsize2 <- "small"
                } else if (0.5 <= abs(d_robust) && abs(d_robust) < 0.8) {effectsize2 <- "medium"
                } else if (abs(d_robust) >= 0.8) {
                    effectsize2 <- "large"
                }
            } else {
                effectsize2 <- "EFFECT SIZE NOT DETERMINED"
            }

            # finally, creating a report
            report <- c(
                "## Robust statistics",

                "Values in the tails of the distribution can have a strong influence on the mean. If values in the tails differ from a normal distribution, the power of a test is reduced and the effect size estimates are biased, even under slight deviations from normality (Wilcox, 2012). One way to deal with this problem is to remove the tails in the analysis by using *trimmed means*. A recommended percentage of trimming is 20% from both tails (Wilcox, 2012), which means inferences are based on the 60% of the data in the middle of the distribution. Yuen's method can be used to compare trimmed means (when the percentage of trimming is 0%, Yuen's method reduces to Welch's *t*-test). Here, a bootstrapped version of Yuen's (1974) adaptation of Welch's two-sample test with trimmed means and windsorized variances is used that returns symmetric confidence intervals (see Keselman, Othman, Wilcox, & Fradette, 2004). Robust effect sizes and their confidence intervals are calculated using bootES by [Kirby and Gerlanc (2013)](http://web.williams.edu/Psychology/Faculty/Kirby/bootes-kirby-gerlanc-in-press.pdf) following Algina, Keselman, and Penfield (2005).",

                "### Results",

                paste0("The 20% trimmed mean ", ylabelstring," of participants in the ", xlabel," condition (*M* = ", round(yuentest$est.1, digits = 2),") was ", direction2," the 20% trimmed mean of participants in the ", ylabel," condition (*M* = ", round(yuentest$est.2, digits = 2),"). The difference in ", ylabelstring," between the conditions (*M* = ", round(yuentest$est.dif, digits = 2),", ", 100*ConfInt,"% symmetric CI [", round(yuentest$ci[1], digits = 2),";", round(yuentest$ci[2], digits = 2),"]) was analyzed using the Yuen-Welch test for 20% trimmed means, *t* = ", round(yuentest$test.stat, digits = 2),", *p* ", ifelse(yuentest$p.value>=0.001," = ", " < ")," ", ifelse(yuentest$p.value>=0.001,formatC(round(yuentest$p.value, digits = 3)), '0.001'),", Robust \\(d_t\\) = ", ifelse(!InAHurry,round(d_robust, digits = 2),'NOT CALCULATED'),",  ", 100*ConfInt,"% CI = [", ifelse(!InAHurry,round(d_robust_ci_l, digits = 2), 'NOT CALCULATED'),";", ifelse(!InAHurry,round(d_robust_ci_u, digits = 2), 'NOT CALCULATED'),"]). The observed data is ", surprising2," under the assumption that the null-hypothesis is true. This can be considered a ", effectsize2," effect.")
            )
        } else if (input$test_type == "dependent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            diff <- data_proc[["diff"]]
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            subgrouplabel <- input$subgrouplabel
            subgrouptokeep <- input$subgrouptokeep
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            alpha <- input$alpha
            H1 <- input$alt_hyp
            ConfInt <- input$conf_int


            ### Robust Statistics
            
            yuend <- yuendv2(x, y, tr = 0.2, alpha = ConfInt)

            # Specify direction of difference
            if (yuend$est1>yuend$est2)  {direction2  <- "greater than"}
            if (yuend$est1<yuend$est2)  {direction2  <- "smaller than"}
            if (yuend$p.value < alpha)  {surprising2 <- "surprising"}
            if (yuend$p.value >= alpha) {surprising2 <- "not surprising"}

            # Interpret size of effect (last resort - use only if effect size cannot be compared to other relevant effects in the literature)
            if (abs(yuend$Effect.Size) < 0.15) {
                effectsize2 <- "tiny"
            } else if (0.15 <= abs(yuend$Effect.Size) && abs(yuend$Effect.Size) < 0.35) {
                effectsize2 <- "small"
            } else if (0.35 <= abs(yuend$Effect.Size) && abs(yuend$Effect.Size) < 0.5){
                effectsize2 <- "medium"
            } else if (abs(yuend$Effect.Size) >= 0.5){
                effectsize2 <- "large"
            }

            report <- c(
                "## Robust statistics",

                "Values in the tails of the distribution can have a strong influence on the mean. If values in the tails differ from a normal distribution, the power of a test is reduced and the effect size estimates are biased, even under slight deviations from normality (Wilcox, 2012). One way to deal with this problem is to remove the tails in the analysis by using *trimmed means*. A recommended percentage of trimming is 20% from both tails (Wilcox, 2012), which means inferences are based on the 60% of the data in the middle of the distribution. Yuen's method can be used to compare trimmed means (when the percentage of trimming is 0%, Yuen's method reduces to Welch's *t*-test). The equivalent of Cohen's *d* for within designs is not yet available, so the explanatory effect size is reported ([Wilcox & Tian, 2011](http://dx.doi.org/10.1080/02664763.2010.498507)). Explanatory power (Xi, replace in the output below by the Greek lowercase Xi symbol) is the robust equivalent of omega squared (unbiased eta squared, or *r* squared), and thus related to \\(d_z\\) in size, not to \\(d_{av}\\). The effect size convention of small, medium, and large corresponds approximately to \\(\\xi = 0.15\\), 0.35 and 0.50.", 

                "### Results",

                paste0("The 20% trimmed mean ", ylabelstring," of participants in the ", xlabel," condition (*M* = ", round(yuend$est1, digits = 2),")  was ", direction2," the 20% trimmed mean of participants in the ", ylabel," condition (*M* = ", round(yuend$est2, digits = 2),"). The difference in ", ylabelstring," between the conditions (*M* = ", round(yuend$dif, digits = 2),", ", 100*ConfInt,"% CI [", round(yuend$ci[1], digits = 2),";", round(yuend$ci[2], digits = 2),"]) was analyzed using the Yuen-Welch test for 20% trimmed means, *t*(", yuend$df,") = ", round(yuend$teststat, digits = 2),", *p* ", ifelse(yuend$p.value>0.001,' = ', ' < ')," ", ifelse(yuend$p.value>0.001, formatC(round(yuend$p.value, digits = 3), digits=3, format='f'), '0.001'),", \\(\\xi = ", round(yuend$Effect.Size, digits=2),"\\). The observed data is ", surprising2," under the assumption that the null-hypothesis is true. This can be considered a ", effectsize2," effect.")
            )
        }

        html_out <- renderMarkdown(text = report)
        return(html_out)
    })

    # ----
    # Results figure tab
    # ----
    
    output$results_plot   <- renderPlot({
        
        if (input$test_type == "independent") {
            
            # loading the data
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            ConfInt <- input$conf_int
            
            # prepare the data for the plot
            bsci <- function(data_proc, 
                             group.var = match(factorlabel,names(data_proc)), 
                             dv.var = match(measurelabel,names(data_proc)), 
                             difference = FALSE, 
                             pooled.error = FALSE, 
                             conf.level = ConfInt) {

                data <- subset(data_proc, select=c(group.var, dv.var))
                fact <- factor(data[[1]], levels = c(xlabel,ylabel))
                dv <- data[[2]]
                J <- nlevels(fact)
                N <- length(dv)
                ci.mat <- matrix(,J,3, dimnames=list(levels(fact), c('lower', 'mean', 'upper')))
                ci.mat[,2] <- tapply(dv, fact, mean)
                n.per.group <- tapply(dv, fact, length)
                if(difference==TRUE) diff.factor= 2^0.5/2 else diff.factor=1
                if(pooled.error==TRUE) {
                    for(i in 1:J) {
                        moe <- summary(lm(dv ~ 0 + fact))$sigma/(n.per.group[[i]])^0.5 * qt(1-(1-conf.level)/2,N-J) * diff.factor
                        ci.mat[i,1] <- ci.mat[i,2] - moe
                        ci.mat[i,3] <- ci.mat[i,2] + moe
                        }
                    }
                if(pooled.error==FALSE) {
                     for(i in 1:J) {
                        group.dat <- subset(data, data[1]==levels(fact)[i])[[2]]
                        moe <- sd(group.dat)/sqrt(n.per.group[[i]]) * qt(1-(1-conf.level)/2,n.per.group[[i]]-1) * diff.factor
                        ci.mat[i,1] <- ci.mat[i,2] - moe
                        ci.mat[i,3] <- ci.mat[i,2] + moe
                    }
                }
                
                return(ci.mat)
            }

            # change matrix output from functions to dataframe, add CI from between, add labels and means 
            ci.sum <- 
                as.data.frame(
                    bsci(data_proc, 
                         group.var = match(factorlabel, names(data_proc)), 
                         dv.var = match(measurelabel, names(data_proc)), 
                         difference = TRUE)
                    )
            ci.sum[[factorlabel]] <- c(xlabel, ylabel)
            ci.sum[[measurelabel]] <- c(mean(x), mean(y))


            ### producing a figure, depending on some user inputs
            figure <- 
                ggplot(ci.sum, 
                       aes_string(x=factorlabel, y=measurelabel, group=1)) 

            if (input$plotType == "meansPlot") {
                figure <- figure + 
                    geom_point(size=4) +
                    geom_errorbar(width=.2, size=0.5, 
                                  aes(ymin=lower, ymax=upper)) +
                    ggtitle(paste0("Means and ", 100*ConfInt, "% CI"))

            } else if (input$plotType == "barPlot") {
                figure <- figure + 
                    geom_bar(position=position_dodge(.9), 
                             colour="black", 
                             stat="identity", 
                             fill="white") +
                    geom_errorbar(width=.1, size=0.5, aes(ymin=lower, ymax=upper)) +
                    ggtitle(paste0("Bar chart displaying means and ", 100*ConfInt, "% CI"))    
                if (input$individualData) {
                    figure <- figure + 
                        geom_point(data=data_proc, alpha=0.2) +
                        ggtitle(paste0("Bar chart displaying means, individual datapoints, and ", 100*ConfInt, "% CI"))
                }                 
            
            } else if (input$plotType == "violinPlot") {
                figure <- figure + 
                    geom_errorbar(width=.1, size=1, 
                                  aes(ymin=lower, ymax=upper)) +
                    geom_point(size=2) +
                    geom_violin(data = data_proc, 
                                aes_string(group=factorlabel), 
                                alpha=0) +
                    ggtitle(paste0("Means and ", 100*ConfInt, "% CI, and violin plot"))
            } 

            # setting the font sizes and labels according to the input
            figure <- figure + 
                ylab(ylabelstring) +
                xlab(xlabelstring) + 
                theme_bw(base_size=16) +
                theme(axis.title.y = element_text(vjust = 1.8),
                      axis.title.x = element_text(vjust = -.8),
                      plot.title = element_text(vjust = 1.8),
                      panel.grid.major.x = element_blank())
            
            return(figure)

        } else if (input$test_type == "dependent") {

            # loading the data and user inputs
            data_user <- get_proc_data()        
            data_proc <- data_user$data_proc
            data_proc_long <- data_user$data_proc_long
            diff <- data_proc[["diff"]]
            x <- data_user$x
            y <- data_user$y
            factorlabel <- input$factorlabel
            measurelabel <- input$measurelabel
            xlabel <- input$xlabel
            ylabel <- input$ylabel
            xlabelstring <- input$xlabelstring
            ylabelstring <- input$ylabelstring
            ConfInt <- input$conf_int
            

            cm.ci <- function(data_proc, conf.level = ConfInt, difference = TRUE) {
                
                # cousineau-morey within-subject CIs
                k = ncol(data_proc)
                if (difference == TRUE) {
                    diff.factor = 2^0.5/2
                } else { 
                    diff.factor = 1
                }
                n <- nrow(data_proc)
                df.stack <- stack(data_proc)
                index <- rep(1:n, k)
                p.means <- tapply(df.stack$values, index, mean)
                norm.df <- data_proc - p.means + (sum(data_proc)/(n * k))
                t.mat <- matrix(, k, 1)
                mean.mat <- matrix(, k, 1)
                for (i in 1:k) t.mat[i, ] <- t.test(norm.df[i])$statistic[1]
                for (i in 1:k) mean.mat[i, ] <- colMeans(norm.df[i])
                c.factor <- (k/(k - 1))^0.5
                moe.mat <- mean.mat/t.mat * qt(1 - (1 - conf.level)/2, n - 1) * c.factor * diff.factor
                ci.mat <- matrix(, k, 2)
                dimnames(ci.mat) <- list(names(data_proc), c("lower", "upper"))
                for (i in 1:k) {
                    ci.mat[i, 1] <- mean.mat[i] - moe.mat[i]
                    ci.mat[i, 2] <- mean.mat[i] + moe.mat[i]
                }

                return(ci.mat)
            }

            bs.ci <- function(data_proc, conf.level = ConfInt, difference = FALSE) {
                # between-subject CIs
                k = ncol(data_proc)
                n <- nrow(data_proc)
                df.stack <- stack(data_proc)
                group.means <- colMeans(data_proc, na.rm = TRUE)
                if (difference == TRUE) {
                    ci.mat <- (confint(lm(values ~ 0 + ind, df.stack)) - group.means) * 2^0.5/2 + group.means
                } else {
                    ci.mat <- confint(lm(values ~ 0 + ind, df.stack))
                }
                dimnames(ci.mat) <- list(names(data_proc), c("lower", "upper"))
                
                return(ci.mat)
            }

            #change matrix output from functions to dataframe, add CI from between, add labels and means 
            ci.sum <- as.data.frame(cm.ci(data_proc[2:3]))
            ci.sum[[factorlabel]] <- c(xlabel,ylabel)
            ci.sum[[measurelabel]] <- c(mean(x),mean(y))
            ci.sum[["lower.between"]] <- as.data.frame(bs.ci(data_proc[2:3]))$lower
            ci.sum[["upper.between"]] <- as.data.frame(bs.ci(data_proc[2:3]))$upper


            ### producing a figure, depending on some user inputs
            figure <- 
                ggplot(ci.sum, 
                       aes_string(x=factorlabel, y=measurelabel, group=1)) 
            

            if (input$plotType == "meansPlot") {
                figure <- figure + 
                    geom_point(size=4) +
                    geom_errorbar(width=.5, size=0.5, aes(ymin=lower, ymax=upper)) +
                    geom_errorbar(width=.3, size=0.5, aes(ymin=lower.between, ymax=upper.between)) +
                    ggtitle(paste0("Means and ", 100*ConfInt, "% CI (between & within)"))

            } else if (input$plotType == "barPlot") {
                figure <- figure + 
                    geom_bar(position=position_dodge(.9), 
                             colour="black", 
                             stat="identity", 
                             fill="white") +
                    geom_errorbar(width=.5, size=0.5, aes(ymin=lower, ymax=upper)) +
                    geom_errorbar(width=.3, size=0.5, aes(ymin=lower.between, ymax=upper.between)) +
                    ggtitle(paste0("Means and ", 100*ConfInt, "% CI (between & within)"))
                if (input$individualData) {
                    figure <- figure + 
                        geom_point(data=data_proc_long, alpha=0.25) +
                        ggtitle(paste0("Means, datapoints, and ", 100*ConfInt, "% CI (between & within)"))
                }                 
            
            } else if (input$plotType == "violinPlot") {
                figure <- figure + 
                    geom_errorbar(width=.25, size=0.5, aes(ymin=lower, ymax=upper)) +
                    geom_errorbar(width=0, size=1, aes(ymin=lower.between, ymax=upper.between)) +
                    geom_point(size=2) +
                    geom_violin(data = data_proc_long, 
                                aes_string(group=factorlabel), 
                                alpha=0) +
                    ggtitle(paste0("Means, violin plot, and two-tiered ", 100*ConfInt, "% within (crossbars) and\nbetween (endpoints of lines) confidence intervals following\nMorey (2008) and Baguley (2012)"))
            }   



            # setting the font sizes and labels according to the input
            figure <- figure + 
                ylab(ylabelstring) +
                xlab(xlabelstring) + 
                theme_bw(base_size=16) +
                theme(axis.title.y = element_text(vjust = 1.8),
                      axis.title.x = element_text(vjust = -.8),
                      plot.title = element_text(vjust = 1.8),
                      panel.grid.major.x = element_blank())
            
            return(figure)
        } 
    })
    
})
