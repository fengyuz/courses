library(BRugs)

# ----------------------------------------------------------------------
#     Dose   - dose level
#     pdes   - maximum probability of toxicity
#
#     ff     - integer value describing the functional form for the
#              density of probability :
#              1 - Hyperbolic tangent
#              2 - logistic
#              3 - Power
#     new     - TRUE if it is the beginning of an experiment, else FALSE
#     prior.a - vector containing the information to generate the
#               prior distribution of probability
#     in.file - name of the file where the history of the experiment is
#               saved into
#     out.file - name of the file where the results of the experiment
#                are written into
#     alpha    - the confidence level
#     burnin   --> number of burn-in steps
#     iteration--> number of iteration steps
# ----------------------------------------------------------------------
crm <- function(Dose, pdes, ff = 1, new = 1, prior.a =c(1,10000,1,1),
        in.file="filin", out.file="filout", alpha=0.05, burnin = 2000, iteration = 10000)
{
    k <- length(Dose )   
    # Get prior sampling data
    sampled.a <- getprior(prior.a)    
    if ( new ) {
        # Beginning of a study
        toxicity <- rep( 0, k )
        No.toxicity <- rep( 0, k )
        npac <- 1
    }
    else {
        # Continuation of the study
        data <- scan(in.file )
        toxicity <- data[1:k]
        No.toxicity <- data[(k+1):(2*k)]
        npac <- sum(toxicity) + sum(No.toxicity) + 1
    }
    par( mfrow=c(1,2) )        
    # main loop
    repeat {
        cat("\n ***** START SEQUENCE ", npac, "*****\n")
        plot( density(sampled.a), xlab="a", ylab="prior   distribution",
                type="l" )       
        # ix is the proposed dose level at which the j-th patient is treated        
        ans <- nextdose(npac, Dose, ff, pdes, sampled.a)        
        ix <- ans[1]
        mu <- ans[2]        
        repeat {
            cat("\n\n RECOMMENDED DOSE LEVEL FOR PATIENT ", npac, "IS:", ix)
            ans <- get.dose(k)            
            if (ans==-2)
                ans <- ix            
            if (ans==-1)
                cat("\n\n EXIT WITHOUT SAVING THE RESULTS")
            else if (ans==0)
                cat("\n\n EXIT AND SAVE THE RESULTS SO FAR")            
            if (ans<1) {
                cat("\n\n DO YOU REALLY WANT TO EXIT ? (Y/N)  ")
                yn <- readline()
                if (yn=="y" || yn=="Y") break
                else                    next
            }            
            # y.j is the outcome ( 0 or 1 ) from the treatment of
            # a patient at dose level ix
            cat("\n ENTER TOXICITY OUTCOME FOR PATIENT ", npac)
            y.j <- get.answer( )            
            # give the user a last chance to modify the treatment and outcome
            cat("\n\n\t\t ENTERED VALUES:")
            cat("\n DOSE LEVEL ...", ans)
            cat("\n TOXICITY .....")
            if (y.j) cat(" YES") else cat(" NO")
            cat("\n PRESS `RETURN' IF OK, OR ANY OTHER KEY TO ENTER NEW VALUES ")
            key <- readline()
            if (nchar(key)==0) break
        }        
        if (ans==-1) return (invisible())
        else if (ans==0)  break        
        ix <- ans        
        # update the arrays used in computing the likelihoods
        toxicity[ix] <- toxicity[ix] + y.j
        No.toxicity[ix] <- No.toxicity[ix] + 1 - y.j
        # compute the posterior probabilities
        sampled.a <- Posterior(ff, prior.a, toxicity, No.toxicity, Dose, burnin, iteration)
        npac <- npac + 1
    }    
    # save data for study continuation
    save.data(toxicity, No.toxicity, in.file)    
    # write the results in a file on the disk
    WriteTable( toxicity, No.toxicity, mu, out.file )    
    # compute the confidence intervals
    conf.int( Dose, alpha, sampled.a, mu,  ff, pdes, out.file )
}

# ----------------------------------------------------------------------
#     User inputs the OUTCOME from the treatment at a dose level:
#     0 - no toxicity
#     1 -    toxicity
# ----------------------------------------------------------------------
get.answer <- function( ) {   
    repeat {
        cat("\n ( 0 - no toxicity, 1 - toxicity )   ")
        ans <- as.numeric(readline())
        if (is.na(ans)) next
        if (ans != floor(ans)) next
        if (ans==0 || ans==1) return(ans)
    }
}

# ----------------------------------------------------------------------
#     ask the user to input an integer number <= n.
#     the number is checked to belong to [-1,n] and also to be
#     an integer.  `ENTER' returns to the caller with no action,
#
#     n - biggest number to be accepted
# ----------------------------------------------------------------------
get.dose <- function( n ) {
    repeat {
        cat("\n\n ENTER DOSE LEVEL BETWEEN 1 AND ", n)
        cat("\n (`RETURN' TO ACCEPT RECOMMENDATION, -1 TO EXIT, 0 TO EXIT AND SAVE DATA)  ")
        
        ans <- readline()
        if ( nchar(ans)==0 ) return( -2 )
        ans <- as.integer(ans)
        if (is.na(ans)) next
        if ( -1<=ans && ans<=n ) return( ans )
    }
}


# ----------------------------------------------------------------------
# generates a vector of values for prior distribution
#
#     info --> vector containing the information to generate the
#               prior distribution for one parameter.  Contains:
#               [1] - the prior distribution type:
#                     1 - gamma: mean=a*b; var=a*b*b
#                     2 - uniform: a+b*unif(0,1)
#               [2] - number of sampling points for prior
#               [3] - a: first parameter of the prior distribution
#               [4] - b: second parameter of the prior distribution
#    
# ----------------------------------------------------------------------
getprior <- function(info) {
    type <- info[1]
    n    <- info[2]
    a    <- info[3]
    b    <- info[4]    
    if ( type==1 ) {
        prior<-rgamma(n,a)*b        
    }
    else if ( type==2 ) {
        prior<-a+runif(n)*b
    }
    return (prior)
}


# ----------------------------------------------------------------------
#     returns the vector containing sampled data from winbug
#
#     ff        --> the model applied in the study
#                    1 - hyperbolic tangent
#                    2 - logistic
#                    3 - Power
#     prior.info  --> vector of prior distribution of parameter a
#     Dose     --> vector containing the dose level
#     tox      --> number of successes (toxicities)
#     notox    --> number of failed patients (no-toxicities)
#     burnin   --> number of burn-in steps
#     iteration--> number of iteration steps
# ----------------------------------------------------------------------
Posterior <- function(ff, prior.info, tox, notox, Dose, burnin, iteration )
{    
    all.patient <- tox + notox
    datan <-all.patient[all.patient!=0]
    datas <-tox[all.patient!=0]
    datad <-Dose[all.patient!=0]
    k <- length(datan)
    if (k == 1)
    {
        datan <- c(datan, 0)
        datas <- c(datas, 0)
        datad <- c(datad, 0)
    }
    mydata <- list(N1 = k, s = datas,n = datan,d = datad, low = prior.info[3], high = prior.info[4])
    bugsData(mydata, file="data.txt")    
    if (prior.info[1] == 1)
    {
        if (ff == 1)
            modelCheck("HTGamma.txt")
        else if ( ff == 2)
            modelCheck("LogisticGamma.txt")
        else if (ff == 3)
            modelCheck("PowerGamma.txt")
    }
    else if (prior.info[1] == 2)
    {
        if (ff == 1)
            modelCheck("HTUnif.txt")
        else if ( ff == 2)
            modelCheck("LogisticUnif.txt")
        else if (ff == 3)
            modelCheck("PowerUnif.txt")
    }        
    modelData("data.txt")
    modelCompile()
    modelInits("initdat.txt")
    #modelGenInits()
    seed1 <- round(runif(1)*10000)
    modelSetSeed(seed1)
    modelUpdate(burnin)
    samplesSet(c("a"))
    modelUpdate(iteration)    
    t<- samplesSample("a")
    return(t)
}


# ----------------------------------------------------------------------
#     find the new dose and posteria mean of a
#
#     npac     --> the patient currently being treated
#     Dose     --> vector of allowable dose values
#     ff       --> integer value:
#                    1 - hyperbolic tangent
#                    2 - logistic
#                    3 - Power
#     pdes     --> desired probability of event (toxicity)
#
#     sample.a  --> sampled data of variable a
#
# ----------------------------------------------------------------------
nextdose <- function(npac, Dose, ff, pdes, sample.a) {
    k <- length( Dose )    
    mu <- mean(sample.a)
    tick <- round(as.single(Dose),2)
    t1<-rep(0,k)
    for(i in 1:k)
    {
        if ( ff == 1 )         
            t1[i] <- mean((1+exp(-2*Dose[i]))^(-sample.a))
        else if ( ff == 2 )
            t1[i] <- mean(1/(1+exp(-3-sample.a*Dose[i])))
        else if ( ff == 3)
            t1[i] <- mean(Dose[i]^exp(sample.a))
    }        
    plot( Dose, t1, ylim=c(0,1), pch="*", xlab="Dose",
            ylab="probability of toxicity", axes=F )
    axis( 1, tick, as.character(tick) )
    axis( 2, las=1 )
    box( )
    abline( pdes, 0, lty=2 )        
    if (npac==1)
        cat("\n MEAN VALUE FOR PARAMETER \"A\" UNDER PRIOR DISTRIBUTION = ")
    else
        cat("\n UPDATED MEAN VALUE FOR PARAMETER \"A\" WITH ", npac-1, " PATIENTS ENTERED = ")    
    cat( round( mu, 2 ) )    
    # choose the closest dose to x0
    ix <- order( abs(t1-pdes))[1]    
    return(c(ix, mu))
}

# ----------------------------------------------------------------------
#
# Saves data for study continuation
#
#     toxicity    --> array containing the number of recorded toxicities
#                     at each dose level.
#     No.toxicity --> array containing the number of recorded
#                     non-toxicities at each dose level.
#     infile      --> the name of the file where the above arrays will
#                     be saved for study continuation
# ----------------------------------------------------------------------
save.data <- function( toxicity, No.toxicity, in.file) {   
    k <- length(toxicity)
    write( toxicity, in.file, ncol=k )
    write( No.toxicity, in.file, ncol=k, append=T )
}

# -----------------------------------------------------------------------
#     write in a file, on the same line, text,
#     followed by numeric fields in a required format
# -----------------------------------------------------------------------
output <- function( file.name, txt, num, form, coda, sep ) {
    k <- length( num )
    chnum <- as.character( c( num, form ) )
    txt.out <- c( txt, format( chnum ) )
    txt.out <- c( txt.out[0:k+1], coda)
    write( txt.out, file.name, ncol=50, append=T )
    write( sep, file.name, append=T )
}

# ----------------------------------------------------------------------
#     toxicity    --> array containing the number of recorded toxicities
#                      at each dose level.
#     No.toxicity --> array containing the number of recorded
#                     non-toxicities at each dose level.
#     mu          --> latest mean value of a
#     out.file    --> the name of the file where the summary of the study
#                     will be recorded
# ----------------------------------------------------------------------
WriteTable <- function( toxicity, No.toxicity, mu, out.file ) {
    k <- length( toxicity )
    sep <- "|-----------------------------------------------------------|"
    form <- "1234"
    coda <- " |"
    write( sep, out.file )
    subjects <- toxicity + No.toxicity
    no.subjects <- sum( subjects )
    txt <- "| dose levels i            |"
    output( out.file, txt, 1:k, form, coda, sep )
    txt <- "| number of toxicities     |"
    output( out.file, txt, toxicity, form, coda, sep )
    txt <- "| patients treated         |"
    output( out.file, txt, subjects, form, coda, sep )
    txt <- "| % patients with toxicity |"
    chnum <- format( round( toxicity/subjects*100, 0 ) )
    output( out.file, txt, chnum, form, coda, sep )
    txt <- "\n Total percentage of observed toxicity: "
    chnum <- format( round( sum(toxicity)/no.subjects*100, 1 ) )
    coda <- ""
    sep  <- ""
    output( out.file, txt, chnum, form, coda, sep )
    cat ("\n\n\n results are written in the file ",out.file, "\n\n\n")
    txt <- "\n\n posterior mean of a: "
    chnum <- format( round( mu, 2 ) )
    output( out.file, txt, chnum, form, coda, sep )
}

# -----------------------------------------------------------------------
#     Return the calculation models
#     ff       --> integer value:
#                    1 - hyperbolic tangent
#                    2 - logistic
#                    3 - Power
# -----------------------------------------------------------------------
which.f <- function( ff, dose, a ) {
    return( switch(ff,
                    function(dose,a) {(1+exp(-2*dose))^(-a)},
                    function(dose,a) {1/(1+exp(-3-a*dose))},
                    function(dose,a) {dose^exp(a)} ) )
}

# ----------------------------------------------------------------------
#     solve the equation giving the value of x
#
#     ff   --> functional form for the probability density
#     pdes --> desired probability of event
#     a    --> the parameter of the model
# ----------------------------------------------------------------------
find.x <- function( ff, pdes, a ) {
    if ( ff == 1 ) {
        A <- log( pdes ) / a
        x <- -0.5*log( exp( -A )-1 )
    }
    else if ( ff == 2 )
        x <- -(3+log( (1-pdes)/pdes ))/a
    else if ( ff == 3)
        x <- exp(log(pdes)/exp(a))
    return( x )
}

#-----------------------------------------------------------------------
#     find the cumulative distribution F(x) from the probability
#     distribution y=f(x)
#
#     Dose    --> vector of doses
#     alpha   --> confidence level
#     x,       --> posterior distribution of mu
#     mu      --> mean value of x
#     ff      --> integer value choosing the functional form for the
#                 distribution of probability used in the study
#     pdes    --> desired level of probability for toxicity
#     out.file --> name of the file where results are to be written
# ----------------------------------------------------------------------
conf.int <- function( Dose, alpha, x, mu, ff, pdes, out.file ) {
    n <- length( x )
    ci.low <- 0.5*alpha
    ci.high <- 1 - ci.low
    z <- 1:200/200
    y <- quantile(x, z)    
    plot(y, z, xlab="a", ylab="F(a)", type="l", main="CDF" )
    psi <- which.f( ff )
    a.low <- quantile( x, ci.low )
    segments( 0,ci.low, a.low,ci.low )
    segments( a.low,ci.low, a.low,0 )    
    a.high  <- quantile( x, ci.high )
    segments( 0,ci.high, a.high,ci.high, lty=2 )
    segments( a.high,ci.high, a.high,0, lty=2 )
    txt <- c(as.character( (ci.high-ci.low)*100), "% C.I. (",
            as.character( ci.low*100 ), "% ... ",
            as.character( ci.high*100 ), "% ) =",
            format( round( a.low, 2 ) ), "...",
            format( round( a.high, 2 ) ))
    write("\n Confidence Intervals for parameter a", out.file,
            ncol=10, append=T )
    write( txt, out.file, ncol=10, append=T )    
    x <- seq( min( Dose ), max( Dose ), len=51 )
    main1 <- "toxicity probability for true value of a \n and limits of the confidence interval"
    plot( x, psi( x, mu ), ylim=c(0,1), xlab="dose", ylab="toxicity",
            type="l", main=main1, axes=F, lty=1 )
    tick <- round( as.single(Dose), 2 )
    axis( 1, tick, as.character( tick ) )
    axis(2, las=1)
    box()    
    lines( x, psi( x, a.low ), lty=3 )
    lines( x, psi( x, a.high ), lty=4 )
    legend( min(x), 1, c(paste("lower 2.5%, a=",round(a.low,2)),
                    paste("mean ........, a=",round(mu,2)),
                    paste("upper 2.5%, a=",round(a.high,2))),
                    lty=c(3,1,4) )
    abline( pdes, 0, lty=2 )
    min.dose <- find.x( ff, pdes, a.low )
    max.dose <- find.x( ff, pdes, a.high )
    txt[7] <- format( round( min.dose, 2 ) )
    txt[9] <- format( round( max.dose, 2 ) )
    write("\n Confidence Intervals for doses", out.file,
            ncol=10, append=T )
    write( txt, out.file, ncol=10, append=T )
}

par(mfrow=c(1,2))
p.tox0 <- c(.05,.15,.3,.45,.6)
# Hyperbolic Tangent
dose  <- -log(1/p.tox0-1)/2.0
pdes <-.3
crm(dose,pdes,ff=1)



