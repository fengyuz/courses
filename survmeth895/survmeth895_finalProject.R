# merged <- merged[,c("poll.err", "state", "samplesize", "daystillvote", "rep12.frac",
#                     "At.Least.Bachelor.s.Degree",
#                     "Median.Earnings.2010.dollars",
#                     "African.American.Population",
#                     "Asian.American.Population",
#                     "Farming.fishing.and.forestry.occupations")]

# # Add state sample size
# stsampsize <- poll[, c("state", "samplesize")]
# stsampsize <- aggregate(poll$samplesize,
#                         by = list(state=poll$state), FUN = sum)
# names(stsampsize)[2] <- "st.samplesize"
# poll <- merge(poll, stsampsize, by = "state")

# # Weight each poll within a state by sample size
# poll$wt <- poll$samplesize / poll$st.samplesize
# poll.wt <- poll
# poll.wt[,c("rawpoll_trump", "daystillvote")] <-
#   poll.wt[,c("rawpoll_trump", "daystillvote")] * poll.wt$wt
# poll.agg <- aggregate(poll.wt[,!(colnames(poll.wt) %in% c("state, st.samplesize, wt"))],
#                       by = poll.wt$state, FUN = sum)

# poll[,c("rawpoll_trump", "daystillvote")] <- poll[,c("rawpoll_trump", "daystillvote")] * poll$wt
# poll.agg <- aggregate(poll.agg[,c("samplesize", "rawpoll_trump", "daystillvote")],
#                       by = list(state = poll.agg$state), FUN = sum)

## par(mfcol = c(3,1))
## hist(as.numeric(as.Date(poll$forecastdate, "%m/%d/%y") - as.Date(poll$enddate, "%m/%d/%Y")))
## hist(as.numeric(as.Date(poll$forecastdate, "%m/%d/%y") - as.Date(poll$startdate, "%m/%d/%Y")))
## hist(as.numeric(as.Date(poll$forecastdate, "%m/%d/%y") - as.Date(poll$middledate, "%m/%d/%Y")))

library(MASS)

# vote result dataset
data("USA_county_data")
# Selection variables to be included
to.be.weighted <- c(
                    "At.Least.Bachelor.s.Degree",
                    "Median.Earnings.2010.dollars",
                    "African.American.Population",
                    "Farming.fishing.and.forestry.occupations",
                    "Asian.American.Population",
                    "Poverty.Rate.below.federal.poverty.threshold",
                    "median_age",
                    "Unemployment",
                    "Violent.crime"
                    )
to.be.summed <- c("Total.Population",
                  "votes16_trumpd",  "total16",
                  "rep12", "total12")
vote <- USA_county_data[, c("State", to.be.summed, to.be.weighted)]



# Clean out unwanted observations
vote <- vote[complete.cases(vote),]
vote <- vote[(vote$State != "Hawaii" &
                        vote$State != "Alaska" &
                        vote$State != "District of Columbia"),]
vote$State <- as.factor(vote$State)



# Add state population to each county
stpop <- aggregate(vote$Total.Population,
                   by = list(State = vote$State), FUN = sum)
colnames(stpop)[2] = "st.pop"
vote <- merge(vote, stpop, by = "State")
# Weight the variables
vote$wt <- vote$Total.Population / vote$st.pop
vote.wt <- vote
vote.wt[,to.be.weighted] <- vote.wt[,to.be.weighted] * vote.wt$wt



# Aggregate by state
vote.agg <- aggregate(vote.wt[,!(names(vote.wt) %in% c("State", "st.pop", "wt"))],
                      by = list(State=vote.wt$State), FUN = sum)
vote.agg$rep16.frac <- vote.agg$votes16_trumpd / vote.agg$total16
vote.agg$rep12.frac <- vote.agg$rep12 / vote.agg$total12
vote.agg$turnout16 <- vote.agg$total16 / vote.agg$Total.Population
# Make vote.agg compatible to poll.agg
colnames(vote.agg)[1] = "state"
vote.agg$state <- as.character(vote.agg$state)


# poll dataset
poll <- read.csv("~/data/presidential_2016/silver_poll.csv")


# Convert poll date to days before the voting day
poll$middledate <- (as.Date(poll$enddate, "%m/%d/%Y") - as.Date(poll$startdate, "%m/%d/%Y")) / 2 + as.Date(poll$startdate, "%m/%d/%Y")
poll$daystillvote <- as.numeric(as.Date(poll$forecastdate, "%m/%d/%y") - as.Date(poll$middledate, "%m/%d/%Y"))
poll$poll.rep.frac <- poll$rawpoll_trump / 100
# Clean out the unwanted observations
poll <- poll[poll$type == "polls-only",]
poll <- poll[,c("state", "samplesize", "poll.rep.frac",
                "daystillvote", "pollster")]
poll <- poll[complete.cases(poll),]
poll <- poll[(poll$state != "Hawaii" &
                        poll$state != "Alaska" &
                        poll$state != "U.S." &
                        poll$state != "District of Columbia"),]
poll[(poll$state=="Nebraska CD-1"|
          poll$state=="Nebraska CD-2"|
          poll$state=="Nebraska CD-3"),]$state <- "Nebraska"
poll[(poll$state=="Maine CD-1"|
          poll$state=="Maine CD-2"|
          poll$state=="Maine CD-3"),]$state <- "Maine"
poll$state <- as.character(poll$state)
colnames(poll)[2] <- "poll.size"



# Combine poll result and vote result
merged <- merge(poll, vote.agg, by = "state")
merged$poll.err <- abs(merged$poll.rep.frac - merged$rep16.frac)

# Add state region and state abbreviations to dataset
data(state)
state <- state.name
stregion <- data.frame(state, state.region, state.abb)
merged <- merge(merged, stregion, by = "state")

# Linear regression
covariates <- colnames(merged)[!(colnames(merged) %in% c("state",
                                      "Total.Population", "votes16_trumpd",
                                      "total16", "rep12", "total12",
                                      "rep16.frac", "poll.err", "pollster", "state.abb",
                                      "turnout16"
                                      ))]
fit <- lm(as.formula(paste0("poll.err~",paste(covariates, collapse = "+"))),
          data = merged)

poll.err.st <- aggregate(merged$poll.err, by = list(state.abb=merged$state.abb), FUN = mean)
poll.repfrac.st <- aggregate(merged$poll.rep.frac, by = list(state.abb=merged$state.abb), FUN = mean)
poll.err.st <- merge(poll.err.st, poll.repfrac.st, by = "state.abb")
plot(poll.err.st$x.y, poll.err.st$x.x, pch=".")
text(poll.err.st$x.y, poll.err.st$x.x, labels = poll.err.st$state.abb)
plot(merged$poll.rep.frac, merged$poll.err, col=poll$daystillvote)









