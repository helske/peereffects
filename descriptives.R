library(data.table)

# Table S1
df <- readRDS("W:/JouniH/peereffects/May2025/data.rds")
setDT(df)
df[, father := 1]
df[, year := as.numeric(as.character(year))]
df[, reformleave := 0]
df[leave == 1 & reform2013 == 1, reformleave := 1]
# PEER
df[, .N, by = .(peer_leave, reform2013, leave)]
df[, median(leave_length), by = .(peer_leave, reform2013)]
df[leave_length > 0, median(leave_length), by = .(peer_leave, reform2013)]
df[leave_length > 18, median(leave_length), by = .(peer_leave, reform2013)]
# EDUCATION
df[year >= 2010 & year < 2013 & leave_length > 18, 
   .N, by = .(education)][order(education), N]/
  df[year >= 2010 & year < 2013, .N, by = .(education)][order(education), N]
df[year >= 2013 & leave_length > 18, .N, by = .(education)][order(education), N]/
  df[year >= 2013, .N, by =.(education)][order(education), N]
df[year < 2013, median(leave_length), by = .(education)]
df[year >= 2013, median(leave_length), by = .(education)]
df[year < 2013 & leave_length > 0, median(leave_length), by = .(education)]
df[year >= 2013 & leave_length > 0, median(leave_length), by = .(education)]
df[year < 2013 & leave_length > 18, median(leave_length), by = .(education)]
df[year >= 2013 & leave_length > 18, median(leave_length), by = .(education)]


# Table S2
df <- readRDS("data.rds")
setDT(df)
df[, father := 1]
df[, year := as.numeric(as.character(year))]
df[, reformleave := 0]
df[leave == 1 & reform2013 == 1, reformleave := 1]
df[leave_length > 0, .N, by = .(reform2013)]
df[leave_length > 18, .N, by = .(reform2013)]
df[, .N, by = .(reform2013)]
df[leave_length > 0, median(leave_length), by = .(reform2013)]
df[leave_length > 18, median(leave_length), by = .(reform2013)]
df[leave_length > 0, .N, by = .(first_birth, reform2013)]
df[leave_length > 18, .N, by = .(first_birth, reform2013)]
df[, .N, by = .(first_birth, reform2013)]
df[leave_length > 0, median(leave_length), by = .(first_birth, reform2013)]
df[leave_length > 18, median(leave_length), by = .(first_birth, reform2013)]
rm(df); gc()
income <- readRDS("income01.rds")
setDT(income)
df <- readRDS("birthRegPL.rds")
setDT(df)
df <- df[is.na(fatherID) == FALSE & birYear >= 2008 & birYear <= 2017]
gc()
df[income, on = .(birYear = vuosi, fatherID = shnro), c("wage", "entrep") := .(i.palk, i.yrtu)]
df[entrep > wage, boss := 1]
df <- df[is.na(boss) == TRUE]
df <- df[birYear > 2008 | (birYear == 2008 & birMon >= 4)]
gc()
employ <- readRDS("employ_0120.rds")
setDT(employ)
df[employ, on = .(birYear = vuosi, fatherID = shnro), 
   c("ptoim2", "ent", "wp") := .(i.ptoim2, i.syrtun2, i.sykstun2)]
rm(employ); gc()
df[is.na(wp) == FALSE, wID := wp]
df[is.na(wp) == TRUE, wID := ent]
df[, father := 1]
df[fatherLeave == 0, anyleave := 0]
df[fatherLeave > 0, anyleave := 1]
df[fatherLeave <= 18, sololeave := 0]
df[fatherLeave > 18, sololeave := 1]
df[, startYear := as.numeric(substr(startDate, 1, 4))]
df[startYear <= 2009, yrGroup := 1]
df[startYear %in% c(2010:2012), yrGroup := 2]
df[startYear %in% c(2013:2017), yrGroup := 3]
df[, .N, by = .(yrGroup, father, anyleave)][, N/sum(N), by = .(yrGroup)]
df[ptoim2 == "11", .N, by = .(yrGroup, father, anyleave)][, N/sum(N), by = .(yrGroup)]
df[fatherLeave > 0, median(fatherLeave), by = .(yrGroup)]
df[fatherLeave > 0 & ptoim2 == "11", median(fatherLeave), by = .(yrGroup)]
df[, .N, by = .(yrGroup, father, sololeave)]
df[ptoim2 == "11", .N, by = .(yrGroup, father, sololeave)]
df[fatherLeave > 18, median(fatherLeave), by = .(yrGroup)]
df[fatherLeave > 18 & ptoim2 == "11", median(fatherLeave), by = .(yrGroup)]

# Table S3

income <- readRDS("income01.rds")
setDT(income)
employ <- readRDS("employ_0120.rds")
setDT(employ)
employ <- employ[ptoim2 == 11]
gc()
employ[is.na(sykstun2) == FALSE, wID := sykstun2]
employ[is.na(sykstun2) == TRUE, wID := syrtun2]
employ <- employ[is.na(wID) == FALSE]
gc()
df <- readRDS("birthRegPL.rds")
setDT(df)
df <- df[is.na(fatherID) == FALSE & birYear >= 2008 & birYear <= 2017]
gc()
df <- df[birYear > 2008 | (birYear == 2008 & birMon >= 4)]
gc()
df[income, on = .(birYear = vuosi, fatherID = shnro), 
   c("wage", "entrep") := .(i.palk, i.yrtu)]
rm(income);gc()
df[entrep > wage, boss := 1]
df <- df[is.na(boss) == TRUE]
employ[, father := 0]
employ[df, on = .(shnro = fatherID, vuosi = birYear), c("father") := 1]
employ[, employee := 1]
employ <- employ[vuosi > 2007 & vuosi < 2018]
gc()
emp1 <- employ[, lapply(.SD, sum), by = .(wID, vuosi), .SDcols = c("employee", "father")]
emp1 <- emp1[father > 0]
emp1[vuosi %in% c(2008, 2009), birCat := 1]
emp1[vuosi %in% c(2010:2012), birCat := 2]
emp1[vuosi %in% c(2013:2017), birCat := 3]
emp2 <- emp1[, lapply(.SD, mean), by = .(wID, birCat), .SDcols = c("employee")]
emp2[, wp := 1]
emp2[employee <= 9, propCat := 1] #1-9
emp2[employee > 9 & employee < 50, propCat := 2]#10-49
emp2[employee > 49 & employee < 250, propCat := 3]#50-249
emp2[employee >= 250, propCat := 4]
employ[vuosi %in% c(2008, 2009), birCat := 1]
employ[vuosi %in% c(2010:2012), birCat := 2]
employ[vuosi %in% c(2013:2017), birCat := 3]
employ[emp2, on = .(wID, birCat), c("propCat") := .(i.propCat)]
emp3 <- emp2[, lapply(.SD, sum), by = .(propCat, birCat), .SDcols = c("wp")]
emp4 <- emp3[, lapply(.SD, sum), by = .(birCat), .SDcols = c("wp")]
employ[father == 1, .N, by = .(propCat, birCat)]
employ[father == 1, .N, by = .(birCat)]

# Table S4

basic <- readRDS("basic_0120.rds")
setDT(basic)
income <- readRDS("income01.rds")
setDT(income)
employ <- readRDS("employ_0120.rds")
setDT(employ)
employ[basic, on = .(vuosi, shnro), sukup := i.sukup]
rm(basic); gc()
employ <- employ[ptoim2 == 11]
gc()
employ[is.na(sykstun2) == FALSE, wID := sykstun2]
employ[is.na(sykstun2) == TRUE, wID := syrtun2]
employ <- employ[is.na(wID) == FALSE]
gc()
df <- readRDS("birthRegPL.rds")
setDT(df)
df <- df[is.na(fatherID) == FALSE & birYear >= 2008 & birYear <= 2017]
gc()
df <- df[birYear > 2008 | (birYear == 2008 & birMon >= 4)]
gc()
df[income, on = .(birYear = vuosi, fatherID = shnro), c("wage", "entrep") := .(i.palk, i.yrtu)]
rm(income);gc()
df[entrep > wage, boss := 1]
df <- df[is.na(boss) == TRUE]
employ[, father := 0]
employ[df, on = .(shnro = fatherID, vuosi = birYear), c("father") := 1]
employ[, male := fifelse(sukup == 1, 1, 0)]
employ[, employee := 1]
employ <- employ[vuosi > 2007 & vuosi < 2018]
gc()
emp1 <- employ[, lapply(.SD, sum), by = .(wID, vuosi), 
               .SDcols = c("employee", "father", "male")]
emp1 <- emp1[father > 0]
emp1[, maleprop := (male/employee)]
employ[emp1, on = .(wID, vuosi), 
       c("nemp", "maleprop") := .(i.employee, i.maleprop)]
rm(emp1)
gc()
df[, startYear := as.numeric(substr(startDate, 1, 4))]
df[startYear <= 2009, birCat := 1]
df[startYear %in% c(2010:2012), birCat := 2]
df[startYear %in% c(2013:2017), birCat := 3]
df[employ, on = .(fatherID = shnro, birYear = vuosi),
   c("wID", "maleprop", "nemp") := .(i.wID, i.maleprop, i.nemp)]
df <- df[is.na(wID) == FALSE & is.na(birCat) == FALSE]
gc()
df[, anyleave := fifelse(fatherLeave > 0, 1, 0)]
df[, quota := fifelse(fatherLeave > 18, 1, 0)]
dfWP <- df[, lapply(.SD, mean, na.rm = TRUE), by = .(wID, birCat), 
           .SDcols = c("anyleave", "quota", "nemp", "maleprop")]
dfWP[, lapply(.SD, mean, na.rm = TRUE), by = .(birCat), 
     .SDcols = c("anyleave", "quota", "nemp", "maleprop")]
dfWP[, lapply(.SD, median, na.rm = TRUE), by = .(birCat), .SDcols = c("nemp")]
df2 <- readRDS("data.rds")
setDT(df2)
df2[, year := as.numeric(as.character(year))]
df2[year == 2009, birCat := 1]
df2[year %in% c(2010:2012), birCat := 2]
df2[year %in% c(2013:2017), birCat := 3]
df2[, maleprop := (numMen/numEmp)]
df2[, anyleave := fifelse(leave_length > 0, 1, 0)]
df2[, quota := fifelse(leave_length > 18, 1, 0)]
dfWP2 <- df2[, lapply(.SD, mean, na.rm = TRUE), by = .(workplace_id, birCat), 
             .SDcols = c("anyleave", "quota", "numEmp", "maleprop")]
dfWP2[, lapply(.SD, mean, na.rm = TRUE), by = .(birCat), 
      .SDcols = c("anyleave", "quota", "numEmp", "maleprop")]
dfWP2[, lapply(.SD, median, na.rm = TRUE), by = .(birCat), .SDcols = c("numEmp")]

# Table S5
library(gtsummary)
library(dplyr)

d <- readRDS("data.rds")
d |> 
  select(
    leave, reform2013, month, roll_sex_ratio, region, occupation, industry, 
    sector, age, partner_age, education, partner_education, income_decile, 
    partner_income_decile, partner_higher_income, peer_leave, log_roll_emp, 
    experienced, experienced_extra, past_leaves, timegap_months, education_peer) |> 
  tbl_summary()

