british_election <- carData::BEPS # BEPS: British Election Panel Study

british_election$ID <- seq.int(nrow(british_election))
british_election$gender <- as.numeric(british_election$gender)

idxData <- dfidx(british_election, 
                 choice = "vote", 
                 shape = "wide", 
                 idx = c("ID"),
                 alt.levels = c("Liberal Democrat", "Labour", "Conservative"))


ml_full <- mlogit(vote ~ 0 | age + economic.cond.national + economic.cond.household + gender | 0, 
              data = idxData)

ml_noLD <- mlogit(vote ~ 0 | age + economic.cond.national + economic.cond.household + gender | 0, 
              data = idxData, alt.subset = c("Labour", "Conservative"))

ml_noL <- mlogit(vote ~ 0 | age + economic.cond.national + economic.cond.household + gender | 0, 
                  data = idxData, alt.subset = c("Liberal Democrat", "Conservative"))

ml_noC <- mlogit(vote ~ 0 | age + economic.cond.national + economic.cond.household + gender | 0, 
                  data = idxData, alt.subset = c("Liberal Democrat", "Labour"))

hmftest(ml_full, ml_noLD)

hmftest(ml_full, ml_noL)

hmftest(ml_full, ml_noC)


stargazer(ml_full, ml_noLD, ml_noL, ml_noC, type = "text")
