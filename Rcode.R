library(shiny)
library(RColorBrewer)
library(shinythemes)
library(shinyWidgets)

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

 
      
      mux <- mu <-4278
      sdx <- a=4404 #"log-normal standard deviation",
       zx<-z<-0.8# delta: hypothesised log-normal proportional change, this will be logged",
       b=0.05 #"Significance level",
       c=.85 # power 
       dx <- d <-300  #"Patients in one arm",
       e =.20 # "expected missing/non evaluable",
                  
    
    
     
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  logN.2.Norm  
    
    v <- a^2
    m <- mu
    
    phi = sqrt(v + m^2);
    mu    = log(m^2/phi)           # mean of log(Y)     
    sigma = sqrt(log(phi^2/m^2))   # std dev of log(Y)  
    
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # use Normal mean and sd to get back the lognormal mean and lognormal SD!
  
  # Norm.2.logN 
    
    v <- sigma^2
    mu <- mu
    
    logN.mu <- exp(mu+0.5* v)
    logN.SD <- logN.mu*sqrt(exp(v)-1)
    
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# test
  
    
    sd <- sigma
   
    zz <- power.t.test(n= d, delta =log(z), sd=sd, sig.level=b,
                       power=NULL, type="two.sample", alternative=c("two.sided"))
    
    
  n = zz$n 
                   power=zz$power 
    
    
 
  # --------------------------------------------------------------------------
 
   
    
    sd <- sigma
  
    
    N <- (n)
    
    delta =log(z)
    
    N1 <- N2 <- floor(N*(1-e/1))  # correct this, initially I divided by 2 - I am not sure why?
    
    sims <- 9999
    
    pow <- mean(replicate(sims, t.test( log(rlnorm(N1, meanlog= mu,        sdlog=sd)) ,
                                        log(rlnorm(N2, meanlog= mu+delta,  sdlog=sd)))$p.value)<0.05)  
    
    d <- as.data.frame(cbind(pow = p4(pow) , sd=p4(sd), mu=p4(mu), N= p0(N), delta=p4(delta), p0(sims)))
    names(d) <- c("Power", "SD", "Mean", "Patients in one arm","Difference","Simulations")
    
    print(d)
  
  # --------------------------------------------------------------------------
  
  #---------------------------------------------------------------------------
 
    sd <- sigma
    
    mu <- mu
    
    delta =log(z)
    
    N <- n
    
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
                   main=paste0("N=",dx," realisations from log-normal mu=",mux,", SD=",a,
                               ",\n Geo.Mean=",p3(A.GM)), col = "#75AADB", border = "white", xlab="Original biomarker scale")
    lines(r[1]:r[2], d, col = "#75AADB")
    Axis(side=1, labels=TRUE)
    Axis(side=2, labels=FALSE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    d <- dlnorm(r[1]:r[2], meanlog = mean(log(x1)), sdlog = sd(log(x1)))
    MASS::truehist(B, ylim = range(d), yaxt='n' , nbins=100,  axes=FALSE,
                   main=paste0("N=",dx," realisations true log-normal mu=",mux*z,", SD=",a,
                               ", \nGeo.Mean=",p3(B.GM) ,"\nTreatment effect ",mux*zx," / ",mux,"=",zx), col = "red", border = "white", xlab="Original biomarker scale")
    lines(r[1]:r[2], d, col="red")
    Axis(side=1, labels=TRUE)
    Axis(side=2, labels=FALSE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    x<-seq(-8+mu,+8+mu,by=0.02)
    MASS::truehist(log(A), yaxt='n' , nbins=100,  axes=FALSE,
                   main=paste0("N=",dx," realisations true Normal mu=",p3(mu),", SD=",
                               p3(sd),""), col = "#75AADB", border = "white",  xlab="log biomarker scale")
    curve(dnorm(x,mu,sd), add=TRUE)
    Axis(side=1, labels=TRUE)
    Axis(side=2, labels=FALSE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    x<-seq(-8+mu,+8+mu,by=0.02)
    MASS::truehist(log(B), yaxt='n'   , nbins=100, axes=FALSE, 
                   main=paste0("N=",dx," realisations true Normal mu=",p3(mu+delta),
                               ", SD=",p3(sd)," \nTreatment effect ", p2(mu+delta)," - ",p3(mu),"=",
                               p3(log(z))), col = "red", border = "white", xlab="log biomarker scale" )
    curve(dnorm(x,mu+delta,sd), add=TRUE)
    Axis(side=1, labels=TRUE)
    Axis(side=2, labels=FALSE)
    
    par(mfrow=c(1,1))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
 
  
  
 
    # pull in

    
    sd <- sigma

    
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
 