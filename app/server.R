

library(shiny)
library(reshape2)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


library("MASS") 
library("akima") 
library("robustbase") 
library("cobs") 
library("robust") 
library("mgcv") 
library("scatterplot3d") 
library("quantreg") 
library("rrcov") 
library("lars") 
library("pwr") 
library("trimcluster") 
library("mc2d") 
library("psych") 
library("Rfit")
library("MBESS") 
library("BayesFactor") 
library("PoweR") 
library("ggplot2") 
library("reshape2") 
library("plyr") 
library("devtools") 
library("rmarkdown")
library("gmodels") 
library("HLMdiag") 
library("car") 
library("gridExtra") 
library("bootES") 
library("BEST")



# Define server logic 
shinyServer(function(input, output) {

    # data file loaded by the user
    get_data <- reactive({

        # input$data_file will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.

        user_file <- input$data_file

        if (is.null(user_file)) return(NULL)
        
        data <- read.csv(user_file$datapath, header=input$header, sep=input$sep, quote=input$quote)
        return(data)
    })
    
    
    



    # ------------------------------------------------------------------
    # OUTPUTS
    # ------------------------------------------------------------------
    
    # ----
    # Data tab
    # ----
    
    output$data_table <- renderTable({
    
        # fetch the data file and return it to the user
        data_user <- get_data()
        return(data_user)
    })


    # ----
    # Diagnostics tab
    # ----

    output$outlier_plot <- renderPlot({
    
        # loading the data
        data_user <- get_data()
        
        # preprocessing
        
        alldata<-na.omit(alldata)#rows with missing data are removed
        
        #IMPORTANT: Do not use spaces, hyphens, or other symbols in names. If variable names in your file include spaces, change the names.

        #Define names of the two groups, as specified by the grouping variable (e.g., 'high' and 'low', or '1' and '2'). Difference is computed as x-y (so reverse labels as desired) 
        xlabel<-"high" #name group 1 - needs to match the datafile (R is case-sensitive)!
        ylabel<-"low" #name group 2 - needs to match the datafile (R is case-sensitive)!

        #Define name (header) of the grouping column in your data file (e.g., condition, time).
        factorlabel<-"condition" #needs to match the datafile (R is case-sensitive)!
        #Define name of the dependent measure column in your data file (e.g., reaction times, self-reported happiness)
        measurelabel<-"answer"  #needs to match the datafile (R is case-sensitive)!
        #Below names for axis are used. These CAN include spaces.
        xlabelstring<-"condition" #define variables to be used for axis (can be replaced by "Any Label")
        ylabelstring<-"answer (1-10)"  #define variables to be used for axis
        
        
        options(scipen=20) #disable scientific notation for numbers smaller than x (i.e., 10) digits (e.g., 4.312e+22)

        #Remove other conditions in your datafile (only keep groups specified above)
        alldata<-subset(alldata, alldata[[factorlabel]]==xlabel|alldata[[factorlabel]]==ylabel)

        x.alldata<-subset(alldata, alldata[[factorlabel]]==xlabel)
        y.alldata<-subset(alldata, alldata[[factorlabel]]==ylabel)
        x<-x.alldata[[measurelabel]]
        y<-y.alldata[[measurelabel]]

        
        #Test normality 

        normalityrejectionsx<-(statcompute(21, x, levels = c(0.05))$decision + statcompute(6, x, levels = c(0.05))$decision + statcompute(2, x, levels = c(0.05))$decision + statcompute(7, x, levels = c(0.05))$decision)

        normalityrejectionsy<-(statcompute(21, y, levels = c(0.05))$decision + statcompute(6, y, levels = c(0.05))$decision + statcompute(2, y, levels = c(0.05))$decision + statcompute(7, y, levels = c(0.05))$decision)

        #Testing equality of variances

        pvalueLevene<-leveneTest(alldata[[measurelabel]] ~ as.factor(alldata[[factorlabel]]))$"Pr(>F)"[1:1]
        if (pvalueLevene < 0.05){equalvar<-"the assumption that variances are equal is rejected (consider reporting robust statistics)."}
        if (pvalueLevene >= 0.05){equalvar<-"the assumption that variances are equal is not rejected."}



        # producing a figure
        ggplot(alldata, aes(factor(eval(parse(text=paste(factorlabel)))), eval(parse(text=paste(measurelabel))))) +
          geom_boxplot()+
          ylab(ylabelstring)  + xlab(xlabelstring) + theme_bw(base_size=14) + 
          theme(panel.grid.major.x = element_blank())

      })

    
    # ----
    # Frequentist tab
    # ----
    
    output$ttestOut <- renderPrint({
    
        # loading the data
        data_user <- get_data()

        sd1<-sd(x) #standard deviation of group 1
        sd2<-sd(y) #standard deviation of group 2
        n1 <- length(x) #number of individuals
        n2 <- length(y) #number of individuals
        m_diff<-mean(x)-mean(y)
        #Always performs Welch's t-test for unequal variances which is better than Levene's test followed by Student's t-test
        ttestresult<-t.test(x, y, alternative = H1, paired = FALSE, var.equal = FALSE, conf.level = ConfInt)
        tvalue<-ttestresult$statistic #store t-value from dependent t-test
        pvalue<-ttestresult$p.value #store p-value from dependent t-test
        CI_diff<-ttestresult$conf.int #store confidence interval of mean difference
        s_av <- sqrt((sd1^2+sd2^2)/2) #calculate average standard deviation for effect size calculation

        #Specify direction of difference
        if (mean(x)>mean(y)){direction<-"greater than"}
        if(mean(x)<mean(y)){direction<-"smaller than"}
        if(pvalue < alpha){surprising<-"surprising"}
        if(pvalue >= alpha){surprising<-" not surprising"}

        #Cohen's d
        require(MBESS)
        d<-smd(Mean.1= mean(x), Mean.2=mean(y), s.1=sd(x), s.2=sd(y), n.1=n1, n.2=n2, Unbiased=TRUE) #Use MBESS to calc d unbiased (Hedges g)
        if(is.finite(d)==FALSE){d<-smd(Mean.1= mean(x), Mean.2=mean(y), s.1=sd(x), s.2=sd(y), n.1=n1, n.2=n2, Unbiased=FALSE)}#In large samples, smd function gives error when Unbiased=TRUE. Difference in d and g no longer noticable, so then unbiased d is calculated.
        ci_l_d<-ci.smd(ncp = tvalue, n.1 = n1, n.2 = n2, conf.level=1-.05)$Lower.Conf.Limit.smd
        ci_u_d<-ci.smd(ncp = tvalue, n.1 = n1, n.2 = n2, conf.level=1-.05)$Upper.Conf.Limit.smd

        #Common Langaue Effect Size (McGraw & Wong, 1992)
        CL<-pnorm(abs(m_diff)/sqrt(sd1^2+sd2^2))

        #Interpret size of effect (last resort - use only if effect size cannot be compared to other relevant effects in the literature)
        if (abs(d) < 0.2){effectsize<-"tiny"}
        if (0.2 <= abs(d) && abs(d) < 0.5){effectsize<-"small"}
        if (0.5 <= abs(d) && abs(d) < 0.8){effectsize<-"medium"}
        if (abs(d) >= 0.8){effectsize<-"large"}

        #Robust Statistics

        require(WRS)
        yuentest<-yuenbt(x,y, tr=0.2,alpha=1-ConfInt,nboot=599,side=T) #for details of this function, see Wilcox, 2012, p. 163).

        #Specify direction of difference
        if(yuentest$est.1>yuentest$est.2){direction2<-"greater than"}
        if(yuentest$est.1<yuentest$est.2){direction2<-"smaller than"}
        if(yuentest$p.value < alpha){surprising2<-"surprising"}
        if(yuentest$p.value >= alpha){surprising2<-" not surprising"}

        #Robust d (d_t) based on Algina, Keselman, and Penfield (2005). 
        require(bootES)
        if(InAHurry!="YES"){d_robust_sum<- bootES(alldata, R=bootstraps, data.col = measurelabel, group.col = factorlabel, contrast = c(xlabel, ylabel), effect.type = "akp.robust.d")}
        ifelse((InAHurry!="YES"),d_robust<-d_robust_sum$t0,d_robust<-"NOT CALCULATED DUE TO TIME CONSTRAINTS")
        ifelse((InAHurry!="YES"),d_robust_ci_l<-d_robust_sum$bounds[1:1],d_robust_ci_l<-"NOT CALCULATED")
        ifelse((InAHurry!="YES"),d_robust_ci_u<-d_robust_sum$bounds[2:2],d_robust_ci_u<-"NOT CALCULATED")

        #Interpret size of effect (last resort - use only if effect size cannot be compared to other relevant effects in the literature)

        if(InAHurry!="YES"){if (abs(d_robust) < 0.2){effectsize2<-"tiny"}}
        if(InAHurry!="YES"){if (0.2 <= abs(d_robust) && abs(d_robust) < 0.5){effectsize2<-"small"}}
        if(InAHurry!="YES"){if (0.5 <= abs(d_robust) && abs(d_robust) < 0.8){effectsize2<-"medium"}}
        if(InAHurry!="YES"){if (abs(d_robust) >= 0.8){effectsize2<-"large"}}
        if(InAHurry!="NO"){effectsize2<-"EFFECT SIZE NOT DETERMINED"}
        
        out <- render('report.Rmd')
    })


    # ----
    # Bayesian tab
    # ----
    
    output$bayesOut <- renderPrint({
    
        # loading the data
        data_user <- get_data()

        # some help variables for plotting
        #BayesFactor
        require(BayesFactor)

        if(H1 == "two.sided"){
          BF<-ttest.tstat(t = tvalue, n1 = n1, n2 = n2, rscale = BFrscale, simple=TRUE)   
        } else if (H1 == "greater"){
          BF <- ttest.tstat(t = tvalue, n1 = n1, n2 = n2, nullInterval = c(0, Inf), rscale = BFrscale, simple = TRUE)
        } else if (H1 == "less"){
          BF <- ttest.tstat(t = tvalue, n1 = n1, n2 = n2, nullInterval = c(-Inf, 0), rscale = BFrscale, simple = TRUE)
        }

        if(BF!=Inf){round(BF, digits=2)}
        if(BF==Inf){BF<-"practically infinitely high"}

        require(BEST) #To calculate HIB

        if(InAHurry!="YES"){BESTout<-BESTmcmc(x,y)}
        if(InAHurry!="YES"){BESTdiff <- BESTout$mu1 - BESTout$mu2}
        if(InAHurry!="YES"){BESTHDI<-hdi(BESTdiff, credMass = ConfInt)}
        ifelse((InAHurry!="YES"),mu<-mean(BESTdiff),mu<-"NOT CALCULATED DUE TO TIME CONSTRAINTS")
        ifelse((InAHurry!="YES"),HDI_l<-BESTHDI[1],HDI_l<-"NOT CALCULATED")
        ifelse((InAHurry!="YES"),HDI_u<-BESTHDI[2],HDI_u<-"NOT CALCULATED")
        if(InAHurry!="YES"){Rhat<-attr(BESTout, "Rhat")}
        if(InAHurry!="YES"){neff<-attr(BESTout, "n.eff")}
        if(InAHurry!="YES"){ifelse(Rhat[1]<1.1 && Rhat[1]<1.1 && neff[1]>10000 && neff[2]>10000, BESTacceptable<-"acceptable", BESTacceptable<-"not acceptable - check the HDI calculation")} 

        #Interpret strength of evidence of Bayes Factor following Jeffreys (1961)
        if (0.33 < BF && BF <= 1){evidence<-"anecdotal evidence for H0"}
        if (0.1 < BF && BF <=0.33){evidence<-"moderate evidence for H0"}
        if (0.03 < BF && BF <= 0.1){evidence<-"strong evidence for H0"}
        if (0.01 < BF && BF <= 0.03){evidence<-"very strong evidence for H0"}
        if (BF <=0.01){evidence<-"decisive evidence for H0"}
        if (1 < BF && BF <= 3){evidence<-"anecdotal evidence for H1"}
        if (3 < BF && BF <=10){evidence<-"moderate evidence for H1"}
        if (10 < BF && BF <= 30){evidence<-"strong evidence for H1"}
        if (30 < BF && BF <= 100){evidence<-"very strong evidence for H1"}
        if (BF > 100){evidence<-"decisive evidence for H1"}
    })

    
})
