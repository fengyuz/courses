#-----------------------------------------------------------------------
#
#                              Description
#
#        The program crm.explorer.R is used to generate graphs for exploring 
#        different types of CRM model with different parameters. 
#
#                              Parameters
#
#     sdose   --> vector of sdoses
#     scale  --> scaling factor
#     lambda --> power used in the sdose transformation
#     ff     --> functional form for the dose-toxicity curvepsi
#                1 - hyperbolic tangent
#                2 - logistic
#                3 - power
#     theta  --> set of values for the parameter of the model.
#                There will be a curve for each value of theta
#     hl     --> vector of values for which horizontal reference lines will be drawn
#     vl     --> vector of values for which vertical referencelines will be drawn
#     legend.on     -->  whether display the legend
#     legend.x      -->  x position of the legend 
#     legend.y      -->  y position of the legend
#     ...           -->  additional graphical parameters
#-----------------------------------------------------------------------
crm.explore <- function(sdose, scale, lambda, ff, theta, hl=NA, vl=NA, 
                        legend.on=T, legend.x=NA, legend.y=NA, ...)  
{   
    if ( scale <= 0 )
        stop("scale MUST be positive")
    d <- sdose / scale    
    if ( lambda == 0 )      d <- log( d )
    else if ( lambda != 1 ) d <- d^lambda    
    # build up the extended sdose vector
    x <- seq(min(d), max(d), len=50) 
    # select the functional form
    if ( ff == 1 ) {
        y <- f.tanh( x, theta )
        what <- "Hyperbolic tangent model  /"
    }
    else if ( ff ==2 ) {
        y <- f.log( x, theta )
        what <- "Logistic model  /"
    }
    else if ( ff == 3){
        y <- f.pow( x, theta )
        what <- "Power model  /"        
    }    
    matplot( x, y, ylim=c(0,1), type="l", xlab="sdose",
            ylab="probability of toxicity", las=1,
            main=paste(what, " scale =", scale, "  lambda =", lambda), ...)
    abline(h=hl, v=vl)
    if (legend.on)
    {
        theta <- as.single( theta )
        if (is.na(legend.x)) legend.x <- 0.75*max(x)
        if (is.na(legend.y)) legend.y <- 0.15
        legend(legend.x, legend.y, paste("theta =", round(
                            as.single(theta), 2)), ...)    
    }
    return( d )
}

#-----------------------------------------------------------------------
#     computes the values for the function describing the model
#
#     d     --> vector of sdoses
#     theta --> vector containing the parameters of the model
#-----------------------------------------------------------------------
f.pow <- function( d, theta ) {
    ncol <- length( theta )
    y <- matrix(NA, nrow=length(d), ncol)
    for ( j in 1:ncol ) {
        y[,j] <- d^exp(theta[j])
    }
    return( y )
}

#-----------------------------------------------------------------------
#     computes the values for the function describing the model
#
#     d     --> vector of sdoses
#     theta --> vector containing the parameters of the model
#-----------------------------------------------------------------------
f.log <- function( d, theta ) {
    ncol <- length( theta )
    y <- matrix(NA, nrow=length(d), ncol)
    for ( j in 1:ncol ) {
        y[,j] <- 1/(1+exp(-3-d*theta[j]))
    }
    return( y )
}

#-----------------------------------------------------------------------
#     computes the values for the function describing the model
#
#     d     --> vector of sdoses
#     theta --> vector containing the parameters of the model
#-----------------------------------------------------------------------
f.tanh <- function(d, theta) {
    ncol <- length( theta )
    y <- matrix(NA, nrow=length(d), ncol)
    for ( j in 1:ncol ) {
        y[,j] <- 1/(1+exp(-2*d))^theta[j]
    }
    return(y)
}

p.tox0 <- c(.05,.15,.3,.45,.6)
# Power model
sdose <-exp(log(p.tox0)/2.71828183)
par(mfrow=c(1,1))
theta.0<-((1:4)/5+0.2)
crm.explore(sdose,scale=1,lambda=1,ff=3,theta=theta.0, col=c("red","blue","green","black"),
            lwd=c(2,2,2,2), lty=1:length(theta.0))


p.tox0 <- c(.01,.15,.3,.45,.99)


