library(BRugs)
### phaseIsim.R

# -----------------------------------------------------------------------
#     Main calculation function
#
#     nsim      --> Number of simulations
#     npat      --> Number of patients
#     sdose     --> Dose levels
#     prob.tox  --> True probability of toxcity at each dose level
#     design    --> 2-CRM method 3-3+3 method
#     ff        --> Models applied in the CRM: 
#                1 hyperbolic tangent 
#                2 Logistic
#                3 Power
#     prior.theta   --> vector containing the information for prior:
#               [1] - the prior distribution type:
#                     1 - gamma: mean=a*b; var=a*b*b
#                     2 - uniform: a+b*unif(0,1)
#               [2] - a: first parameter of the prior distribution
#               [3] - b: second parameter of the prior distribution
#     target.tox     --> Target toxicity
#     burnin.itr     --> Number of burn-in iterations in MCMC
#     production.itr --> Number of production iterations in MCMC
#     nseed          --> Random number seed
#     outfile        --> Output file name
#     
# -----------------------------------------------------------------------
phaseIsim <- function(nsim, npat, sdose, prob.tox, design='CRM', ff=2, prior.theta=c(1, 1, 1), 
        target.tox=0.3, crm.group.size=1, crm.stop=6, crm.min=18, std.stop=6,
        burnin.itr=1000, production.itr=1000, nseed=11, outfile="fileout")
{
    set.seed(nseed)    
    k <- length(sdose)    
    work <- list()
    work$dose <- sdose
    work$prob <- prob.tox
    form <- "1234"
    glob.tox <- glob.notox <- rep(0, k)
    mtd <- rep(0, k+3)    
    # print the input data into the output file
    write("\n\n\t\t\tINPUT DATA\n\n", outfile)
    write(paste("number of simulations ......",nsim), outfile, app=T)
    write(paste("number of patients .........",npat), outfile, app=T)
    txt <- "dose levels ................"
    chnum <- c(format(round(sdose,2)), form)
    output(outfile, txt, chnum, "")
    write("probability of toxicity:", outfile, app=T)
    chnum <- c(format(round(prob.tox,2)), form)
    output(outfile, "", chnum, "")
    if ( design == 'CRM' || design == 'crm') {
        mean.theta <- rep(NA, nsim)
        write(paste("target toxicity level .....",target.tox), outfile, app=T)
        write("", outfile, app=T)
        if ( ff == 1 )
            write("functional form: hyperbolic tangent",
                    outfile,app=T)
        else if ( ff == 2 )
            write("functional form: logistic", outfile, app=T)
        else if ( ff == 3 )
            write("functional form: Power", outfile, app=T)
        write("prior distribution of the parameter:", outfile, app=T)
        write(paste("\tnumber of sampled prior",prior.theta[2]),
                outfile,app=T)
        if ( prior.theta[1] == 1 )
            write("\tGAMMA distribution:",  outfile, app=T)
        else if ( prior.theta[1] == 2 )
            write("\tUNIFORM distribution:", outfile, app=T)
        write(paste("\t\ta= ",prior.theta[2]), outfile, app=T)
        write(paste("\t\tb= ",prior.theta[3]), outfile, app=T)

        write("modified CRM", outfile, app=T)
        write(paste("patients treated at a level to decide MTD ....",
                        crm.stop),  outfile, app=T)
        write(paste("patients treated simultaneously at a level ...",
                        crm.group.size), outfile, app=T)
        write(paste("minimum number of patients in the CRM design .......",
                        crm.min),  outfile, app=T)
        write(paste("number of burn-in iterations in MCMC .......",
                        burnin.itr),  outfile, app=T)
        write(paste("number of production iterations in MCMC ....",
                        production.itr),  outfile, app=T)
        write("toxicity from given toxicity probabilities", outfile, app=T)
    }
    else if ( design == '3+3' ) {
        write("\n3+3 rule; 0/3, escalate", outfile, app=T)
        write("1/3: 3 more patients treated at the same level", outfile, app=T)
        write("2/6, 2/3, or 3/3: exceed MTD MTD=one level lower than",outfile, app=T)
        write("stopped level; need at least 6 patients treated at MTD\n",outfile, app=T)
    }
    write(paste("random seed ..........................",nseed), outfile, app=T)
    write(paste("name of the output file: ", outfile), outfile, app=T)
    total.patients <- 0
    for (i in 1:nsim) {
        work$tox <- work$notox <- rep(0, k)
        if (design == '3+3') {
            ans <- pat3.sim(npat, work, design, std.stop)
        }
        else {
            ans <- crm.sim( npat, work, design, target.tox, ff,
                    prior.theta, outfile, crm.group.size, crm.stop, crm.min, burnin.itr, production.itr)
            print(i)
            print(ans$mean.theta)
            mean.theta[i] <- ans$mean.theta
        }
        total.patients <- total.patients + ans$npat
        glob.tox   <- glob.tox   + ans$work$tox
        glob.notox <- glob.notox + ans$work$notox
        mtd[ans$mtd] <- mtd[ans$mtd] + 1
    }
    cat("\n\ntoxicity ......", glob.tox)
    cat("\nno toxicity ...", glob.notox)
    cat("\nMTD ...........", mtd)
    # write the summary
    write("\n\n\t\t\tOUTPUT RESULTS\n\n", outfile, app=T)
    avgpat <- total.patients / nsim
    WriteTable(glob.tox, glob.notox, mtd, avgpat, outfile)
    if (design == 'CRM' || design == 'crm') {
        write (paste("\n\nmean value of theta ........................",
                        round(mean(mean.theta),2)), outfile, app=T)
        write (paste("standard deviation of theta ................",
                        round(sqrt(var(mean.theta)),2)), outfile, app=T)
    }
    else {
        write(paste("\n\nMTD below first dose level ...........",
                        round(100*mtd[k+2]/nsim,2),"%"), outfile, app=T)
        write(paste("MTD above last  dose level ...............",
                        round(100*mtd[k+1]/nsim,2),"%"), outfile, app=T)
    }
    write(paste("out of patients ..........................",
                    round(100*mtd[k+3]/nsim,2),"%"), outfile, app=T)
    return (invisible())
}

# -----------------------------------------------------------------------
#     write in a file, on the same line, text,
#     followed by numeric fields in a required format
#
#     file.name --> name of the file to write to
#     txt       --> text to be written
#     num       --> vector of numerical values to be written,
#                   with the format appended
#     coda      --> the ending of the line
# -----------------------------------------------------------------------
output <- function( file.name, txt, num, coda ) {
    k <- length( num )
    txt.out <- c(txt, format(num))
    txt.out[k+1] <- coda
    write( txt.out, file.name, ncol=132, append=T )
}

# ----------------------------------------------------------------------
#     npat - number of patients in the study
#
#     work   - list containing as components:
#              $dose: dose levels used in the study
#              $prob: probability of toxicity at each dose level
#              $tox:  number of toxicities at each dose level
#              $notox: number of non-toxicities at each dose level
#
#     method.dose - method to compute the dose level:
#        For all the methods, escalate the dose level for no toxicity
#        outcome from the 3 patients treated
#                   3 - 1 toxicity: treat three more patients at
#                        the same level; if no toxicity, escalate; else:
#                        2 or 3 toxicities: if >= std.stop patients treated at
#                           previous level: MTD=previous level
#                           treat 3 more patients at previous level;
#                           else: go back one more level;
#                           if the level is the first one, MTD can not be det.
#
# ----------------------------------------------------------------------
pat3.sim <- function(npat, work, method.dose, std.stop)
{
    # npat must be a multiple of 3
    if (npat%%3 > 0) {
        stop("\n\nNumber of patients treated MUST be a multiple of 3\n")
    }    
    k <- length(work$dose)
    ngroup <- npat %/% 3
    npat <- 0    
    first <- 0
    dose.level <- 1
    mtd <- NA    
    # --- main loop    
    for (group in 1:ngroup) {        
        # --- enter a group of 3 patients, at dose.level
        npat <- npat + 3        
        # --- get the number of toxicities from the treatment at dose.level      
        ntox <- sum(runif(3) <= work$prob[dose.level])        
        # --- update the arrays used in computing the toxicity        
        work$tox[dose.level]   <- work$tox[dose.level] + ntox
        work$notox[dose.level] <- work$notox[dose.level] + 3 - ntox        
        # --- take a decision based on the number of toxicities from 3 pacients        
        if (ntox == 0) {
            if (first==2) {
                mtd <- dose.level
                break
            }            
            dose.level <- dose.level + 1
            first <- 0
        }
        else {            
            # one or more toxicities                          
            if (first == 2) {                    
                # previous level was exceed MTD and less than 6 patients
                # treated at this level                    
                if (ntox == 1) {
                    mtd <- dose.level
                    break
                }
                else {                        
                    # 2 or 3 toxicities; go back one more level
                    dose.level <- dose.level - 1
                    if (dose.level == 0) {
                        mtd <- 0
                        break
                    }
                }
            }
            else if (ntox>1 || (ntox==1&&first==1)) {                
                # exceed MTD
                dose.level <- dose.level - 1
                if (dose.level == 0) {
                    mtd <- 0
                    break
                }                    
                if (work$tox[dose.level]+work$notox[dose.level] >= std.stop) {
                    mtd <- dose.level
                    break
                }
                else {                        
                    # less than 6 patients treated at this level; can't decide yet                        
                    first <- 2
                }
            }
            else {
                # one toxicity, no previous exceed MTD
                # treat 3 more patients at the same dose level                    
                if (first == 0) {
                    first <- 1
                }
            }            
        }
        if (dose.level==0 || dose.level>k) {
            mtd <- dose.level
            break
        }
    }
    if (is.na( mtd )) {
        mtd <- k + 3
    }
    else {
        if (mtd == 0)             mtd <- k + 2
    }
    return (list(mtd=mtd, mean.theta=NA, work=work, npat=npat))
}

# ----------------------------------------------------------------------
# generates a vector of values and prior distribution
#
#     info --> vector containing the information for prior
#               [1] - the prior distribution type:
#                     1 - gamma: mean=a*b; var=a*b*b
#                     2 - uniform: a+b*unif(0,1)
#               [2] - a: first parameter of the prior distribution
#               [3] - b: second parameter of the prior distribution
#    
# ----------------------------------------------------------------------
getprior <- function(info, n) {
    type <- info[1]
    a    <- info[2]
    b    <- info[3]    
    if ( type==1 ) {
        prior<-rgamma(n,a)*b        
    }
    else if ( type==2 ) {
        prior<-a+runif(n)*b
    }
    return (prior)
}

# ----------------------------------------------------------------------
#     patmax - total number of patients in the study
#      
#     work   - list containing as components:
#              $dose: dose levels used in the study
#              $prob: probability of toxicity at each dose level
#              $tox:  number of toxicities at each dose level
#              $notox: number of non-toxicities at each dose level
#
#     method.dose - method to compute the dose level:
#                    1 - accepted dose level from computation
#                    2 - do not allow jumps over a dose level, unless
#                        a patient was already treated at that dose level
#      
#     pdes   - maximum probability of toxicity
#      
#     ff     - integer value describing the functional form for the
#              density of probability :
#              1 - hyperbolic tangent
#              2 - logistic
#              3 - Power
#
#     prior.theta - vector containing the information to generate the
#               prior distribution of probability
#
#     out.file - name of the file where the results of the experiment
#                are written into
#
#     theLot - number of patients to be treated at a dose level.
#              Used only by `crm'
#
#     stop.crit - stop the trial if this number of patients were already
#                 treated at the proposed dose level.  This dose level
#                 will be the MTD.
#                 Used only by `crm'
#
#     crm.min - minimum number of patients to be treated before taking
#               a decision regarding the MTD
#
#     brunin.itr - number of burn-in iterations in MCMC
#
#     production.itr - number of production iterations in MCMC
# ----------------------------------------------------------------------

crm.sim <- function(patmax, work, method.dose, pdes,
        ff, prior.theta, out.file, theLot=1, stop.crit=6, crm.min=18, burnin.itr=1000, production.itr=1000)
{
    k <- length( work$dose )
    # Get prior sampling data
    sampled.data <- getprior(prior.theta, production.itr)
    old.dose <- 0
    # main loop    
    npat <- 0
    while ( npat < patmax ) 
    {              
        # ix is the proposed dose level at which this patient is treated        
        ans <- nextdose(work$dose, ff, pdes, sampled.data)        
        ix <- ans[1]
        mean.theta <- ans[2]
        if (method.dose == 'CRM' || method.dose == 'crm') {
            if (npat == 0) {
                # first lot of patients; ALWAYS start at dose level 1
                ix <- 1
            }
            else if (ix>old.dose+1 && (work$tox[ix]+work$notox[ix]==0)) {
                # do not allow dose jumps, if no previous treatment at this level
                ix <- old.dose + 1
            }            
            old.dose <- ix
        }        
        # check for the termination of the trial        
        if ( (work$tox[ix]+work$notox[ix] >= stop.crit) && (npat >= crm.min) ) {
            return ( list(mtd=ix, mean.theta=mean.theta, work=work, npat=npat) )   
        }        
        npat <- npat + theLot        
        # y.j is the outcome ( 0 or 1 ) from the treatment of a patient
        # at dose level ix        
        y.j <- sum( runif(theLot) < work$prob[ix] )        
        # update the arrays used in computing the likelihoods        
        work$tox[ix]   <- work$tox[ix] + y.j
        work$notox[ix] <- work$notox[ix] + theLot - y.j                
        # compute the posterior probabilities        
        sampled.data <- Posterior(ff, prior.theta, work, burnin.itr, production.itr) 
    }    
    # if here, we treated all the patients and no MTD was found    
    return ( list(mtd=k+3, mean.theta=mean.theta, work=work, npat=patmax) )
}

# ----------------------------------------------------------------------
#     Sample the posterior distribution using winbug
#
#     ff     - integer value describing the functional form for the
#              density of probability :
#              1 - hyperbolic tangent
#              2 - logistic
#              3 - Power
#     prior.info - vector containing the information to generate the
#                  prior distribution of probability
#     work   - list containing as components:
#              $dose: dose levels used in the study
#              $prob: probability of toxicity at each dose level
#              $tox:  number of toxicities at each dose level
#              $notox: number of non-toxicities at each dose level
#     burnin - number of burnin steps
#     iteration - number of iteration steps
# ----------------------------------------------------------------------
Posterior <- function(ff, prior.info, work, burnin.itr, production.itr ) 
{    
    all.patient <- work$tox + work$notox 
    datan <-all.patient[all.patient!=0]
    datas <-work$tox[all.patient!=0]
    datad <-work$dose[all.patient!=0]
    k <- length(datan)
    if (k == 1)
    {
        datan <- c(datan, 0)
        datas <- c(datas, 0)
        datad <- c(datad, 0)
    }
    mydata <- list(N1 = k, s = datas,n = datan,d = datad, low = prior.info[2], high = prior.info[3])
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
    seed1 <- round(runif(1, 1, 14))
    modelSetRN(seed1)
    modelUpdate(burnin.itr)
    samplesSet(c("theta"))
    modelUpdate(production.itr)    
    t<- samplesSample("theta")
    return(t)
}

# ----------------------------------------------------------------------
#     find the new dose and posteria mean of theta
#
#     Dose     --> vector of allowable dose values
#     ff       --> integer value:
#                    1 - hyperbolic tangent
#                    2 - logistic
#                    3 - Power
#     pdes     --> desired probability of event (toxicity)
#
#     sample.theta  --> sampled data of variable theta
#                     
# ----------------------------------------------------------------------
nextdose <- function(Dose, ff, pdes, sample.theta) {
    k <- length( Dose )    
    mean.theta <- mean(sample.theta)
    if ( ff == 1 ) {
        temp <- log( pdes ) / sample.theta
        x <- -0.5*log( exp( -temp )-1 )
    }
    else if ( ff == 2 )
        x <- -(3+log( (1-pdes)/pdes ))/sample.theta
    else if ( ff == 3)
        x <- exp(log(pdes)/exp(sample.theta))
    x0 <- mean(x)
    # Choose the closest dose to x0
    ix <- order( abs(Dose-x0))[1]    
    return(c(ix, mean.theta))
}

# ----------------------------------------------------------------------
#     toxicity    --> array containing the number of recorded toxicities
#                     at each dose level.
#     No.toxicity --> array containing the number of recorded
#                     non-toxicities at each dose level.
#     mtd         --> array containing maximum tolerated doses
#                     at each dose level
#     avgpat      --> average number of patients at a simulation
#     out.file    --> the name of the file where the summary of the study
#                     will be recorded
# ----------------------------------------------------------------------
WriteTable <- function( toxicity, No.toxicity, mtd, avgpat, out.file )
{
    k <- length( toxicity )
    nsim <- sum(mtd)    
    hed <- "|===================|="
    dsh <- "|-------------------|-"
    for (i in 1:k) {
        dsh <- paste(dsh, "------", sep="")
        hed <- paste(hed, "======", sep="")
    }
    dsh <- paste(dsh, "|", sep="")
    hed <- paste(hed, "|", sep="")
    form <- "12345"
    coda <- "|"
    write( dsh, out.file, app=T )
    subjects <- toxicity + No.toxicity
    total.subjects <- sum( subjects )
    txt <- "| dose levels       |"
    chnum <- c(as.character(1:k), form)
    output(out.file, txt, chnum, coda)
    write(hed, out.file, append=T)
    txt <- "| avg # of toxicity |"
    chnum <- c(format(round(toxicity/nsim,1)), form)
    output( out.file, txt, chnum, coda )
    write(dsh, out.file, append=T)
    txt <- "| avg # at dose     |"
    chnum <- c(format(round(subjects/nsim,1)), form)
    output( out.file, txt, chnum, coda )
    write(hed, out.file, append=T)
    txt <- "| % dose toxicity   |"
    chnum <- c(format(round(toxicity*100.0/subjects,1)), form)
    output( out.file, txt, chnum, coda )
    write(dsh, out.file, append=T)
    txt <- "| % patients        |"
    chnum <- c(format(round(subjects*100/total.subjects,1)), form)
    output(out.file, txt, chnum, coda)
    write(dsh, out.file, append=T)
    txt <- "| % of MTD dose     |"
    chnum <- c(format(round(mtd[1:k]*100.0/sum(mtd),1)), form)
    output(out.file, txt, chnum, coda)
    write(dsh, out.file, append=T)
    txt <- paste("\n\naverage number of patients for one simulation ...",
            round(avgpat,2))
    write(txt, out.file, append=T)    
    txt <- paste("\noverall toxicity ............................... ",
            round(100*sum(toxicity)/total.subjects,1),"%")      
    write(txt, out.file, append=T)    
    cat ("\n\n results are written in the file ",out.file, "\n\n")    
    return (invisible())
}


p.tox0 <- c(.05,.15,.3,.45,.6)
# Hyperbolic Tangent
s.dose  <- -log(1/p.tox0-1)/2.0
phaseIsim(nsim=100, npat=30, sdose=s.dose, prob.tox=p.tox0, design='CRM', ff = 1, crm.group.size=3, outfile='HT.txt')

# Logistic
s.dose  <- log(p.tox0/(1-p.tox0)) - 3
phaseIsim(nsim=100, npat=30, sdose=s.dose, prob.tox=p.tox0, design='CRM', ff = 2, crm.group.size=3, outfile='Logistic.txt')

# Power
s.dose <-exp(log(p.tox0)/2.71828183)
phaseIsim(nsim=100, npat=30, sdose=s.dose, prob.tox=p.tox0, design='CRM', ff = 3, crm.group.size=3, outfile='Power.txt')

# 3+3 method
phaseIsim(nsim=10000, npat=30, sdose=s.dose, prob.tox=p.tox0, design='3+3', outfile='3plus3.txt')
