#-----------------------------------------------------------------------
#
#                              Description
#
#     Displays the model function psi(dose,a), before performing
#     a Continual Reassessment study
#
#                              Parameters
#
#     dose   --> vector of doses
#     scale  --> scaling factor
#     lambda --> power used in the dose transformation
#     ff     --> functional form for psi
#                1 - hyperbolic tangent
#                2 - logistic
#                3 - power
#     a      --> set of values for the parameter of the model.
#                There will be a curve for each value of a
#     hl     --> vector of values for which horizontal lines will be drawn
#     vl     --> vector of values for which vertical   lines will be drawn
#-----------------------------------------------------------------------
explore <- function( dose, scale, lambda, ff, a, hl=NA, vl=NA ) {   
    if ( scale <= 0 )
        stop("scale MUST be positive")
    d <- dose / scale    
    if ( lambda == 0 )      d <- log( d )
    else if ( lambda != 1 ) d <- d^lambda    
    # build up the extended dose vector
    d1 <- min( d )
    d2 <- max( d )
    half <- 0.5*(d2-d1)    
    x <- c(seq(d1-half, d2+half, len=20))
    x <- sort(c(x,d))    
    # select the functional form
    if ( ff == 1 ) {
        y <- f.tanh( x, a )
        what <- "hyperbolic tangent"
    }
    else if ( ff ==2 ) {
        y <- f.log( x, a )
        what <- "logistic"
    }
    else if ( ff == 3){
        y <- f.pow( x, a )
        what <- "power"        
    }    
    matplot( x, y, ylim=c(0,1), type="l", lty=1:length(a), xlab="Dose",
            ylab="Probability of Toxicity", las=1,
            main=paste(what, " scale =", scale, "  lambda =", lambda) )
    abline(h=hl, v=vl)
    a <- as.single( a )
    lena <- 1:length(a)
    legend(0.75 * max(x) + 0.25 * min(x), 0.18, paste("a =", round(
                            as.single(a), 2)), lty=lena, col=lena)    
    return( d )
}

#-----------------------------------------------------------------------
#     computes the values for the function describing the model
#
#     d --> vector of doses
#     a --> vector containing the parameters of the model
#-----------------------------------------------------------------------
f.pow <- function( d, a ) {
    ncol <- length( a )
    y <- matrix(NA, nrow=length(d), ncol)
    for ( j in 1:ncol ) {
        y[,j] <- d^exp(a[j])
    }
    return( y )
}

#-----------------------------------------------------------------------
#     computes the values for the function describing the model
#
#     d --> vector of doses
#     a --> vector containing the parameters of the model
#-----------------------------------------------------------------------
f.log <- function( d, a ) {
    ncol <- length( a )
    y <- matrix(NA, nrow=length(d), ncol)
    for ( j in 1:ncol ) {
        y[,j] <- 1/(1+exp(-3-d*a[j]))
    }
    return( y )
}

#-----------------------------------------------------------------------
#     computes the values for the function describing the model
#
#     d --> vector of doses
#     a --> vector containing the parameters of the model
#-----------------------------------------------------------------------
f.tanh <- function( d, a ) {
    ncol <- length( a )
    y <- matrix(NA, nrow=length(d), ncol)
    for ( j in 1:ncol ) {
        y[,j] <- 1/(1+exp(-2*d))^a[j]
    }
    return( y )
}

p.tox0 <- c(.05,.15,.3,.45,.6)
# Power model
dose <-exp(log(p.tox0)/2.71828183)
pdes <-.3
par(mfrow=c(1,1))
apara<-((1:4)/5+0.2)
explore(dose,scale=1,lambda=1,ff=3,a=apara)



