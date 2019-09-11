library(shiny)
library(RColorBrewer)
library(shinythemes)


fig.width <- 900
fig.height <- 950
text1  <- "blah blah"
text2  <- "blah blahx"
p3 <- function(x) {formatC(x, format="f", digits=3)}
p4 <- function(x) {formatC(x, format="f", digits=4)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p1 <- function(x) {print(formatC(x, format="f", digits=1),quote=FALSE)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p0 <- function(x) {formatC(x, format="f", digits=0)}


ui <- fluidPage(
  
  # App title ----
  titlePanel("Sample size/power for an RCT based on a continuous biomarker endpoint that is log-normal distributed"),
  
  # Sidebar layout with input and output definitions ----
  div(p("Imagine you are using the result of a laboratory test as the primary endpoint in your RCT. Laboratory tests (potential biomarkers) are typically skewed and therefore a natural log transformation is often applied so that the distribution is more Normal looking. 
                  This means the original distribution is assumed log-normal. In many situations we carry out inferences on logarithims of quantites and then transform the results back to an interpretable scale. Thus often using normal theory, distribution of odds ratios and hazard ratios are in fact log-normal distributions.
        The objective is to work out the number of patient results required to pick up at a given alpha level a proportional change on the original log-normal scale of the biomarker distribution. We transform to the Normal distribution and perform the calculations there. A t-test approach is used to power\\calculate the required sample size, ultimately an ANCOVA model would be fit to the transformed data with baseline biomarker as a covariate and treatment as the covariate of interest.
        Along the way we investigate the relationship between the log-normal and Normal distribution and also use simulation to answer the question.")),
  
  
  
  br(),
  actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
    onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/crossover-trial/master/ABBA.crossover/app.R', '_blank')"),   
  actionButton("resample", "Simulate a new sample"),
  br(),
  # br(),
  # actionButton(inputId='ab1', label="R code here", 
  #              icon = icon("th"), 
  #              onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/crossover-trial/master/ABBA.crossover/app.R', '_blank')"),
  
  br(),
  br(),
  p(strong("Generate true population parameters:")),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      div(p("Select the log-normal mean and the log-normal SD, enter the effect size of interest 'delta'.")),
      tags$a(href = "https://en.wikipedia.org/wiki/Log-normal_distribution", "More about log-normal distribution."),
      br(),
      br(),
      
      sliderInput("mu",
                  "log-normal mean",
                  min=0, max=20000, step=1, value=4278, ticks=FALSE),  #225
      sliderInput("a",
                  "log-normal standard deviation",
                  min=1, max=20000, step=1, value=4404, ticks=FALSE), #15
      sliderInput("z",
                  "delta: hypothesised log-normal proportional change, this will be logged",
                  min=0, max=2, step=.05, value=0.8, ticks=FALSE),
      
      sliderInput("b",
                  "Significance level",
                  min=0, max=.5, step=.01, value=0.05, ticks=FALSE),
      sliderInput("c",
                  "power",
                  min=0, max=1, step=.01, value=.85, ticks=FALSE),
      sliderInput("d",
                  "Patients in one arm",
                  min=0, max=10000, step=1, value=261, ticks=FALSE),
      sliderInput("e",
                  "expected missing/non evaluable",
                  min=0, max=.9, step=.01, value=0, ticks=FALSE)
      
      
      
    ),
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  
                  
                  tabPanel("Plot", value=5, h3("Treatment effect"),
                           p('On the top left, we have a histogram of the null distribution of the biomarker with N equal to the'
                             , strong('Patients in one arm') ,'Top right we have the biomarker treatment distribution, 
                           a proportional difference.
                             A division of the right mean by the left mean will equal the slider input delta. 
                             The bottom histograms are drawn from normal distributions with appropriate mean and sd. 
                             Here the treatment effect is a difference log(delta). Density curves from the distributions are superimposed.
                              The geometric mean is simply computing the arithmetic mean of the logarithm-transformed values and then using
                              the exponentiation to return the computation to the original scale.
                           Click the ',strong('Simulate a new sample'),' button for another repeat of the experiment.'),
                           br(),plotOutput("plot")#,     
                          
                  ),
               
                  
                  tabPanel("Sample size for a t test", value=5, h3("Sample Size for a two-sample t-test"),
                           p('The goal of this analysis is to estimate the sample size for a test intended to
                      determine if there is a difference in the outcome of interest. The computations are based on a ',strong('t-test') ,' for two independent 
                      countinuous populations and are performed for a two-sided hypothesis test.'),
                           br(),
                           tags$a(href = "http://en.wikipedia.org/wiki/Student's_t-test", "More detail about t-test."),
                           br(),
                           br(),
                           
                           p('Look left at the true population parameters, in this example ',strong('power'),'is fixed,
                                   the ',strong('sd') ,'below is the Normal distribution sd and follows 
                                   from the ',strong('log-normal standard deviation,'), 'the', strong('Mean') ,'is 
                                   not required for the calculation, change the', strong('log-normal mean') ,' to see. The difference', strong('delta') ,
                             'is the hypothesised difference of importance and is the log of delta, 
                                   the sample size ', strong('n') ,' is', strong('Patients in one arm') ,' selected on the left. 
                                   The canned power calculation function in R is used to determine the',strong('power.'),
                             'This result' ,strong('does not'),' take into account the' ,strong('expected missing/non evaluable'),'selection.'),
                           br(),
                           
                           p('Interesting to compare the symmetry around 1 (no change), select',strong('delta 0.8'),'and', strong('delta 1.25')), 
                           verbatimTextOutput("summary")),
                  
                  
                  
                  
                  tabPanel("Power simulation", value=3, h3("Power using simulation"),
                           p('Look left at the true population parameters, 
                                   the ',strong('SD') ,'below is the Normal distribution SD and follows 
                                   from the ',strong('log-normal standard deviation,'), 'the', strong('Mean') ,'is 
                                   transformed to the Normal distribution from that shown left 
                                   ', strong('log-normal mean.') ,'The', strong('Difference') ,'is the hypothesised difference of importance: the', strong('log of delta,'),' 
                                   the', strong('Patients in one arm') ,' is that selected on the left. 
                                   Based on these inputs simulation is used to determine the',strong('power.'),
                             'This result also takes into account the' ,strong('expected missing/non evaluable'),'selection. 
                             Click the ',strong('Simulate a new sample'),' button for another repeat of the experiment.'),
                           br(),
                           
                           
                           tableOutput("table")),
                  
                  tabPanel("Log-normal and Normal dist.", value=3, 
                           h3("Demonstrating successful transformation between Normal and log-normal"),
                           p('Using the mean and standard deviation on the Normal scale in the rlnorm function we simulate values a large number of times. The second plot is simulated using the rnorm function and then exponentiating. 
                             They are identical as expected.  Click the  ',strong('Simulate a new sample'),' button for another repeat of the experiment.'),
                           br(),plotOutput("plot2")),   
                  
                  
                  
                  
                  tabPanel("Notes", value=3, #h3("Some notes and a further reference"),
                           p('Formulae to swap back and forth between the log-normal and the normal distribution'),
                           br(),
                           
                           withMathJax(
                             helpText('We have
                               $${{Y}\\sim{LN}}{\\left({\\mu,\\thinspace\\sigma^2}\\right)}\\!$$')),
                           
                           withMathJax(
                             helpText('then, 
                               $${{log(Y)}\\sim{N}}{\\left({\\mu,\\thinspace\\sigma^2}\\right)}\\!$$')),
                           
                           withMathJax(
                             helpText('We wish to know, 
                               $${ \\sim{N} \\left({\\mu,\\thinspace\\sigma^2}\\right)}\\!$$')),
                            
                           helpText('Let\'s calculate the normal distribution parameters,$${ \\phi =sqrt({\\sigma^2}+{\\mu^2}  )}\\!$$') ,
                           
                           helpText('$${ \\mu =log({\\mu^2}/{\\phi}  )}\\!$$') ,

                          helpText('$${ \\sigma = sqrt(log({\\phi^2}/{\\mu^2}  ))}\\!$$',

                          'Now let\'s revert back to log-normal distribution parameters,
                                                $${ \\mu = \\exp({\\mu}+{0.5\\sigma^2}  )}\\!$$'),

                          helpText('
                                                $${ \\sigma = {\\mu}({sqrt(\\exp(\\sigma^2)-1)}  )}\\!$$'),

                          helpText('Alternative equivalent parameterisation of sigma...
                                                $${ \\sigma = sqrt(\\exp(2\\mu+\\sigma^2) (\\exp(\\sigma^2)-1)  )}\\!$$'),
                          
                          
                          br(),
                          tags$a(href = "https://blogs.sas.com/content/iml/2014/06/04/simulate-lognormal-data-with-specified-mean-and-variance.html", "Blog on simulating lognormal data"),
                          br(),
                          br(),
                          br()
                 
                  
                  
      ))
      
    )
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- shinyServer(function(input, output) {
  
  logN.2.Norm <- reactive({
    
    v <- input$a^2
    m <- input$mu
    
    phi = sqrt(v + m^2);
    mu    = log(m^2/phi)           # mean of log(Y)     
    sigma = sqrt(log(phi^2/m^2))   # std dev of log(Y)  
    
    return(list(mu = mu, sigma = sigma))
    
  }) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # use Normal mean and sd to get back the lognormal mean and lognormal SD!
  
  Norm.2.logN <- reactive({
    
    xx <- logN.2.Norm()
    
    v <- xx$sigma^2
    mu <- xx$mu
    
    logN.mu <- exp(mu+0.5* v)
    logN.SD <- logN.mu*sqrt(exp(v)-1)
    
    return(list(mu = logN.mu, sigma = logN.SD))
    
  }) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  random.sample <- reactive({
    
    xx <- logN.2.Norm()
    
    sd <- xx$sigma
    
    mu <- xx$mu
    
    zz <- power.t.test(n= input$d, delta =log(input$z), sd=sd, sig.level=input$b,
                       power=NULL, type="two.sample", alternative=c("two.sided"))
    
    
    return(list(zz = zz, n = ceiling(zz$n), power=zz$power))
    
    
  }) 
  
  txt<-reactive({
    
    zzz<-"something"
    
  })
  
  # --------------------------------------------------------------------------
  
  
  # Dummy line to trigger off button-press
  
  simulate <- reactive({
    
    sample <- random.sample()
    
    sample <- random.sample2()
    
    xx <- logN.2.Norm()
    
    sd <- xx$sigma
    
    mu <- xx$mu
    
    N <- (sample$n)
    
    delta =log(input$z)
    
    N1 <- N2 <- floor(N*(1-input$e/2))
    
    sims <- 9999
    
    pow <- mean(replicate(sims, t.test( log(rlnorm(N1, meanlog= mu,        sdlog=sd)) ,
                                         log(rlnorm(N2, meanlog= mu+delta,  sdlog=sd)))$p.value)<0.05)  
    
    d <- as.data.frame(cbind(pow = p4(pow) , sd=p4(sd), mu=p4(mu), N= p0(N), delta=p4(delta), p0(sims)))
    names(d) <- c("Power", "SD", "Mean", "Patients in one arm","Difference","Simulations")

    return(list(d))
  }) 
  
  
  # --------------------------------------------------------------------------
  # This is where a new sample is instigated only random noise is required to be generated
  random.sample2 <- reactive({
    # Dummy line to trigger off button-press
    foo <- input$resample
    sample <- random.sample()

  })
  
  #---------------------------------------------------------------------------
  # Plot a scatter of the data  
  output$plot <- renderPlot({         
    
    # Get the current regression data
    sample <- random.sample()
    sample <- random.sample2()
    xx <- logN.2.Norm()
    
    sd <- xx$sigma
    
    mu <- xx$mu
    
    delta =log(input$z)
    
    N <- sample$n
    
    A <- lapply(1:N, function(i) rlnorm(1, meanlog=mu,       sdlog=sd))
    B <- lapply(1:N, function(i) rlnorm(1, meanlog=mu+delta, sdlog=sd))
    
    A <- unlist(A)
    B <- unlist(B)
    
    A.GM <- exp(mean(log(A)))
    B.GM <- exp(mean(log(B)))
    
    par(mfrow=c(2,2))
   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    x  <- rlnorm(1000, mu, sd)  
    x1 <- rlnorm(1000, mu+delta, sd)  
    
    r <- range(c(x,x1))
    d <- dlnorm(r[1]:r[2], meanlog = mean(log(x)), sdlog = sd(log(x)))
    MASS::truehist(A, ylim = range(d), yaxt='n' ,nbins=100,  axes=FALSE,
                   main=paste0("N=",input$d," realisations from log-normal mu=",input$mu,", SD=",input$a,
                               ",\n Geo.Mean=",p3(A.GM)), col = "#75AADB", border = "white", xlab="Original biomarker scale")
    lines(r[1]:r[2], d, col = "#75AADB")
    Axis(side=1, labels=TRUE)
    Axis(side=2, labels=FALSE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    d <- dlnorm(r[1]:r[2], meanlog = mean(log(x1)), sdlog = sd(log(x1)))
    MASS::truehist(B, ylim = range(d), yaxt='n' , nbins=100,  axes=FALSE,
                   main=paste0("N=",input$d," realisations true log-normal mu=",input$mu*input$z,", SD=",input$a,
                               ", \nGeo.Mean=",p3(B.GM) ,"\nTreatment effect ",input$mu*input$z," / ",input$mu,"=",input$z), col = "red", border = "white", xlab="Original biomarker scale")
    lines(r[1]:r[2], d, col="red")
    Axis(side=1, labels=TRUE)
    Axis(side=2, labels=FALSE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    x<-seq(-8+mu,+8+mu,by=0.02)
    MASS::truehist(log(A), yaxt='n' , nbins=100,  axes=FALSE,
                   main=paste0("N=",input$d," realisations true Normal mu=",p3(mu),", SD=",
                               p3(sd),""), col = "#75AADB", border = "white",  xlab="log biomarker scale")
    curve(dnorm(x,mu,sd), add=TRUE)
    Axis(side=1, labels=TRUE)
    Axis(side=2, labels=FALSE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    x<-seq(-8+mu,+8+mu,by=0.02)
    MASS::truehist(log(B), yaxt='n'   , nbins=100, axes=FALSE, 
                   main=paste0("N=",input$d," realisations true Normal mu=",p3(mu+delta),
                               ", SD=",p3(sd)," \nTreatment effect ", p2(mu+delta)," - ",p3(mu),"=",
                               p3(log(input$z))), col = "red", border = "white", xlab="log biomarker scale" )
    curve(dnorm(x,mu+delta,sd), add=TRUE)
    Axis(side=1, labels=TRUE)
    Axis(side=2, labels=FALSE)
    
    par(mfrow=c(1,1))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  })
  
  
  output$plot2 <- renderPlot({         
    
    # pull in
    sample <- random.sample()
    sample <- random.sample2()
    
    xx <- logN.2.Norm()
    
    sd <- xx$sigma
    
    mu <- xx$mu
    
    delta =log(input$z)
    
    par(mfrow=c(2,1))
    # now lets use the result to sumulate logN data and check, input ~N(mu, sigma) params
    sims <- 5000000
    x <- rlnorm(n = sims, meanlog =  mu, sdlog = sd)
    h<-hist(x, breaks=1000, xlim=c(0, 10000), axes=TRUE, ylab="Frequency",
         main=paste0(sims, " simulations from rlnorm using inputs mu=",p3(mu),", SD=",p3(sd),""), col = "#75AADB", border = "white", xlab="original biomarker scale") 
   # Axis(side=1, labels=TRUE)
    #Axis(side=2, labels=FALSE)
   # curve(dlnorm, col = 2, meanlog=mu, sdlog=sd,add = TRUE)
     
    
    # check again simulate Normal data from N(mu, sigma), exponentiate and plot
    z <- exp(rnorm(sims, mu,   sd ))
    muz <- mean( (z))
    sdz <- sd( (z))
    hist(z, breaks=1000,  xlim=c(0, 10000),axes=TRUE,ylab="Frequency",
         main=paste0(sims, " simulations from rnorm using mu=",p3(mu),", SD=",p3(sd)," then exponentiated. \nActual mean=",p0(muz)," and sd=",p0(sdz),", compare to slider inputs"), 
         col = "#75AADB", border = "white", xlab="original biomarker scale")   # plot of log lognormal
    #curve(dnorm(x,mu+delta,sd), add=TRUE)
   # Axis(side=1, labels=TRUE)
    #Axis(side=2, labels=FALSE)
    #summary(z)
    par(mfrow=c(1,1))
    
    # check
    
    #######################
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  output$summary2 <- renderPrint({
    #txt()$zzz"ABC
    print("ABC")
  })
  
  
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    (random.sample()$zz)
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    print(simulate()$d)
  })
  
  
  output$table <- renderTable({
    head(simulate())
  })
})



#})
# Create Shiny app ----


# Run the application 
shinyApp(ui = ui, server = server)