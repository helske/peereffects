## Create data for peer effects models
library(dplyr)
library(data.table)
library(forcats)


f <- function(n, father_id, leave, time, education, reform2010, reform2013, 
              birth_date, leave_start, leave_length) {
  
  d <- data.table(
    own_leave_lag = rep(NA_integer_, length(n)), 
    peer_leave = NA_integer_, 
    exact_timegap_lag = NA_real_, 
    exact_timegap_own = NA_real_,
    father_id_lag = NA_integer_,
    education_lag = NA_integer_, 
    past_leaves_before_peer = 0, # number of leaves before peer excluding focal's leaves
    past_leave_takers_before_peer = 0, # number of unique leave takers before peer excluding peer (if earlier child)
    past_leave_takers_before_peer2 = 0, # number of unique leave takers before peer including peer (if earlier child)
    own_before_2010reform_lag = NA_integer_,
    own_before_2013reform_lag = NA_integer_,
    before_2010reform_lag = NA_integer_,
    before_2013reform_lag = NA_integer_,
    birth_date_lag = NA_integer_,
    leave_start_lag = NA_integer_,
    leave_length_lag = NA_integer_,
    n_earlier_births = 0
  )
  
  # handle each father separately, need to exclude own children from lagged leave variables
  
  for(i in 2:nrow(d)) {
    
    d[i, father_id_lag := father_id[i - 1]]
    same_father <- father_id[1:(i - 1)] == father_id[i]
    idx_own <- which(same_father)
    idx_diff <- which(!same_father)
    k <- length(idx_own)
    if (k > 0) {
      k2 <- idx_own[k]
      d[i, own_leave_lag := leave[k2]]
      d[i, own_before_2010reform_lag := reform2010[k2] == 0L]
      d[i, own_before_2013reform_lag := reform2013[k2] == 0L]
      d[i, exact_timegap_own := time[i] - time[k2]]
    }
    k <- length(idx_diff)
    if(k > 0) {
      d[i, n_earlier_births := k]
      k2 <- idx_diff[k]
      d[i, peer_leave := leave[k2]]
      d[i, before_2010reform_lag:= reform2010[k2] == 0L]
      d[i, before_2013reform_lag:= reform2013[k2] == 0L]
      d[i, exact_timegap_lag := time[i] - time[k2]]
      d[i, education_lag := education[k2]]
      d[i, birth_date_lag := birth_date[k2]]
      d[i, leave_start_lag := leave_start[k2]]
      d[i, leave_length_lag := leave_length[k2]]
      if(k > 1) {
        names(leave[1:(i - 2)]) <- father_id[1:(i - 2)]
        tmp <- leave[idx_diff]
        d[i, past_leaves_before_peer := sum(tmp)]
        d[i, past_leave_takers_before_peer := length(unique(names(tmp)))]
        # all fathers who took leave excluding focal and peer father
        tmp <- which(!(father_id[1:(i - 2)] %in% father_id[(i - 1):i]) & leave[1:(i - 2)])
        if (length(tmp) > 0) {
          idx <- father_id[1:(i - 2)][tmp]
          d[i, past_leave_takers_before_peer := length(unique(idx))]
        }
        # all fathers who took leave excluding only focal
        tmp <- which(!(father_id[1:(i - 1)] %in% father_id[i]) & leave[1:(i - 1)])
        if (length(tmp) > 0) {
          idx <- father_id[1:(i - 1)][tmp]
          d[i, past_leave_takers_before_peer2 := length(unique(idx))]
        }
      }
    }
  }
  d
}


#### Load data ####
fulldata <- readRDS("W:/Simon/PREDLIFE/PeerEffects/Data/reform2009_longformat_Apr24.rds")

reclassify_occup <- function(x) {
  x[is.na(x)] <- "XXXXX"
  factor(substr(as.character(x), 1, 1), levels = c("X", 0:9))
}
edulevels <- c("Basic", "Upper Secondary", "Lower tertiary", "Higher tertiary")
d0 <- fulldata %>% 
  mutate(
    father_id = droplevels(shnro), 
    workplace_id = droplevels(workplaceID)
  ) %>% 
  select(
    father_id, workplace_id, partner_id = partnerID, 
    region = mkunta, age = ika, partner_age = pAge, leave, 
    highEdu2, pEdu, occup, income, pWage, 
    birMon, indust, numEmp, numMen, birDate, start, start2, 
    spec_mat, vuosi, firstBir, inJob, jobStart, jobEnd, rollNumMean, rollnMenMean,
    rollSR
  ) 
rm(fulldata);gc();
d0 <- d0 %>% 
  mutate(
    leave_length = leave,
    leave = as.integer(leave > 18),
    occupation = reclassify_occup(occup),
    education = highEdu2, # basic, upper secondary, bachelors, higher tertiary
    partner_education = pEdu, # basic, upper secondary, bachelors, higher tertiary
    log_income = log(1 + exp(income)),  # fix zero income
    partner_log_income = log(1 + exp(pWage)), # fix zero income
    leave_start = ifelse(is.na(start), start2, start), # if mother's leave is missing (didn't take), use father's
    leave_start = ifelse(is.na(leave_start), birDate, leave_start), # if still missing (no leave), use birth date
    reform2013 = as.integer(leave_start >= as.Date("2013-01-01")),
    reform2010 = as.integer(leave_start >= as.Date("2010-01-01")),
    birth_date = birDate,
    time = as.numeric(birDate - as.Date("2008-01-01")),
    month = factor(birMon, levels = 1:12),
    year = factor(vuosi),
    num_emp = numEmp,
    num_men = numMen,
    log_roll_men = log(rollnMenMean),
    log_roll_emp = log(rollNumMean),
    roll_sex_ratio = rollSR,
    industry = factor(ifelse(indust %in% c(0, 99), NA, indust))
  ) %>% 
  group_by(workplace_id, occupation) %>% 
  mutate(
    group_id = cur_group_id(),
    n = row_number()
  ) %>% 
  select(-c(income, pWage, birMon, vuosi, rollNumMean, start, start2,
            rollnMenMean, rollSR, indust, pEdu, highEdu2, occup))

d0$group_id <- factor(d0$group_id)
d0$father_id <- as.integer(d0$father_id)

length(unique(d0$workplace_id)) #47439
length(unique(d0$group_id)) #103490
length(unique(d0$father_id)) #183111

# create income decile variables for each year
# for partners 16% has no income, so define deciles without these
# (for fathers its only 1.6% so doesn't matter that much)
d0 <- d0 %>% 
  group_by(year) %>% 
  mutate(
    income_decile = cut(
      log_income, 
      quantile(log_income, seq(0, 1, by = 0.1), na.rm = TRUE),
      labels = 1:10,
      include.lowest = TRUE),
    partner_income_decile = cut(
      partner_log_income, 
      c(0,  quantile(partner_log_income[partner_log_income > 0], seq(0.1, 1, by = 0.1), na.rm = TRUE)),
      labels = 1:10,
      include.lowest = TRUE)
  )
# remove groups with only one father
rm_one_father <- d0 %>% 
  group_by(group_id) %>% 
  summarise(one_father = n() == 1)

sum(rm_one_father$one_father) #49582
rm_id <- rm_one_father %>% filter(one_father == TRUE) %>% pull(group_id)

d <- d0 %>% 
  ungroup() %>% 
  filter(!(group_id %in% rm_id)) %>% 
  select(workplace_id, group_id, father_id, partner_id, n, birth_date, 
         month, year, time, reform2010, reform2013, leave,
         occupation, education, partner_education, 
         log_income, partner_log_income, income_decile, 
         partner_income_decile, age, partner_age, 
         region, industry, log_roll_emp, log_roll_men, roll_sex_ratio, 
         numMen, numEmp, first_birth = firstBir, spec_mat, leave_start, 
         leave_length) 
rm(d0);gc();
length(unique(d$workplace_id)) #38162
length(unique(d$group_id)) #53908
length(unique(d$father_id)) #150952

# renumber fathers from 1
d <- d %>%
  group_by(group_id) %>%
  arrange(birth_date, .by_group = TRUE) %>% 
  mutate(
    n = row_number()
  ) %>% 
  droplevels()

#### switch to data.table for speed ####
setDT(d)
cnames <- c("own_leave_lag", "peer_leave", "exact_timegap_lag", "exact_timegap_own", 
            "father_id_lag", "education_lag", 
            "past_leaves_before_peer", "past_leave_takers_before_peer", "past_leave_takers_before_peer2",
            "own_before_2010reform_lag", "own_before_2013reform_lag", "before_2010reform_lag", 
            "before_2013reform_lag", "birth_date_lag", "leave_start_lag", "leave_length_lag",
            "n_earlier_births")

d[, (cnames) := f(n, father_id, leave, time, education, reform2010, 
                  reform2013, birth_date, leave_start, leave_length), by = group_id]

# switch back to dplyr for convenience, create additional variables
m <- 3
max_m <- 48

d <- d %>% 
  ungroup() %>% 
  mutate(
    own_after_peer = as.integer(father_id == father_id_lag),
    own_previous_birth = factor(case_when(
      is.na(first_birth) ~ NA,
      first_birth == 1 ~ "no earlier birth",
      is.na(own_leave_lag) ~ "birth before 2009",
      own_before_2013reform_lag == 0 & own_leave_lag == 1 ~ "leave, eligible to 2013 reform",
      own_before_2013reform_lag == 0 & own_leave_lag == 0 ~ "no leave, eligible to 2013 reform",
      own_before_2013reform_lag == 1 & own_leave_lag == 1 ~ "leave, not eligible to 2013 reform",
      own_before_2013reform_lag == 1 & own_leave_lag == 0 ~ "no leave, not eligible to 2013 reform"
    ), levels = c(
      "no earlier birth", 
      "birth before 2009", 
      "no leave, not eligible to 2013 reform",
      "leave, not eligible to 2013 reform",
      "no leave, eligible to 2013 reform",
      "leave, eligible to 2013 reform")),
    own_previous_birth_peer_timing = factor(
      droplevels(interaction(own_after_peer, own_previous_birth)),
      labels = c(
        "no earlier birth", 
        "birth before 2009", 
        "no leave, not eligible to 2013 reform, before peer",
        "no leave, not eligible to 2013 reform, after peer",
        "leave, not eligible to 2013 reform, before peer",
        "leave, not eligible to 2013 reform, after peer",
        "no leave, eligible to 2013 reform, before peer",
        "no leave, eligible to 2013 reform, after peer",
        "leave, eligible to 2013 reform, before peer",
        "leave, eligible to 2013 reform, after peer"
      )
    ),
    own_leave_lag = factor(
      ifelse(is.na(own_leave_lag) & first_birth == 0, 3, own_leave_lag + 1),
      labels = c("no leave", "leave", "unknown")),
    own_before_2009_lag = ifelse(
      own_leave_lag == "unknown", 1, 0),   
    own_before_2010reform_lag = ifelse(
      own_leave_lag == "unknown", 1, own_before_2010reform_lag),   
    own_before_2013reform_lag = ifelse(
      own_leave_lag == "unknown", 1, own_before_2013reform_lag),
    n_past_leaves = cut(
      past_leaves_before_peer,
      c(0, 1, 2, 5, max(past_leaves_before_peer)),
      right = FALSE,
      include.lowest = TRUE),
    n_past_leave_takers = cut(
      past_leave_takers_before_peer,
      c(0, 1, 2, 5, max(past_leave_takers_before_peer)),
      right = FALSE,
      include.lowest = TRUE),
    n_past_leave_takers2 = cut(
      past_leave_takers_before_peer2,
      c(0, 1, 2, 5, max(past_leave_takers_before_peer2)),
      right = FALSE,
      include.lowest = TRUE),
    education_difference_peer = factor(
      case_when(
        education_lag < education ~ "lower",
        education_lag == education ~ "same",
        education_lag > education ~ "higher",
      ), 
      levels = c("lower", "same", "higher"), 
      labels = c("lower", "same", "higher")),
    education_difference_partner = factor(
      case_when(
        partner_education < education ~ "lower",
        partner_education == education ~ "same",
        partner_education > education ~ "higher",
      ), 
      levels = c("lower", "same", "higher"), 
      labels = c("lower", "same", "higher")),
    education = factor(education, levels = 1:4, labels = edulevels),
    partner_education = factor(partner_education, levels = 1:4, labels = edulevels),
    education_lag = factor(education_lag, levels = 1:4, labels = edulevels),
    partner_higher_income = factor(partner_log_income > log_income, 
                                   labels = c("lower", "higher")),
    earlier_own_births = as.integer(first_birth == 0),
    timegap_months = cut(
      exact_timegap_lag,
      include.lowest = TRUE,
      breaks = c(30.4167 * seq(0, max_m, by = m), max(time)),
      labels = paste0(seq(0, max_m, by = m), 
                      rep(c("-",""), c(length(seq(0, max_m, by = m)) - 1, 1)), 
                      c(seq(m, max_m, by = m), "+"))
    ),
    own_leave_lag = tidyr::replace_na(own_leave_lag, replace = "no leave"),
    own_before_2013reform_lag = tidyr::replace_na(own_before_2013reform_lag, replace = 0),
    own_before_2010reform_lag = tidyr::replace_na(own_before_2010reform_lag, replace = 0),
    own_leave_lag_isleave = as.integer(own_leave_lag == "leave"),
    log_roll_men = ifelse(is.na(log_roll_men), log(numMen), log_roll_men),
    log_roll_emp = ifelse(is.na(log_roll_emp), log(numEmp), log_roll_emp),
    roll_sex_ratio = ifelse(is.na(roll_sex_ratio), numMen / numEmp, roll_sex_ratio),
    reform2013 = factor(reform2013, labels = c("not eligible", "eligible"))
  ) %>% droplevels()

# use only fathers eligible to 2010 reform and child is born _after_ January 2010, 
# and only those father's who have at least one peer
d <- d %>% 
  ungroup() %>% 
  filter(reform2010 == 1 & birth_date >= as.Date("2010-02-01") & !is.na(peer_leave)) %>% 
  select(-reform2010)

# remove 2073 births from 227 groups where peer has the same birth date as focal
# remove births either starting form first duplicate, or remove births before last duplicate
# which ever leads to less removals
d <- d %>% 
  group_by(group_id) %>% 
  mutate(
    same_date_first = min(n() + 1, head(which(exact_timegap_lag == 0), 1)),
    same_date_last = max(0, tail(which(exact_timegap_lag == 0), 1)),
    keep = if(first(same_date_first) > first(same_date_last)) 1:n() < same_date_first else 1:n() > same_date_last
  ) 
sum(d$keep == FALSE) #2073
length(unique(d$group_id[d$keep == FALSE])) #227

d <- d %>% 
  filter(keep) %>% 
  select(-c(same_date_first, same_date_last, keep))
# remove industry 22 as there is only 10 fathers with that code (2 groups)
# also 26 because while there is 72 fathers, none of them is before 2013 
# and we want to model the effect of reform on industry
# and remove 31 as well as there's only 86 fathers 
# in all other industries there are >>100 fathers and groups
table(d$industry)
d <- d %>% 
  filter(industry != 22 & industry != 26 & industry != 31) %>% 
  droplevels()
# remove also armed forces occupation as there is only 58 fathers in 12 groups
# there are >>100 fathers in others
table(d$occupation)
d <- d %>% 
  filter(occupation != 0) %>% 
  droplevels()

# remove groups with missing values in age, education, or income variables
summary_rm <- d %>% 
  group_by(group_id) %>% 
  summarise(
    na_edu = any(is.na(education)) | any(is.na(partner_education)),
    na_age = any(is.na(age)) | any(is.na(partner_age)),
    na_inc = any(is.na(log_income)) | any(is.na(partner_log_income))
  )
colSums(summary_rm[, -1])
# na_edu na_age na_inc 
#    496    496    321
rm_id <- summary_rm %>% 
  filter(na_edu | na_age | na_inc) %>% pull(group_id)
length(rm_id) # 738

d <- d %>% 
  group_by(group_id) %>% 
  filter(!(group_id %in% rm_id))

d <- d %>% 
  mutate(
    t = row_number(),
    group_id = cur_group_id(),
    group_size = n()) %>% 
  ungroup() %>% 
  droplevels()


saveRDS(d, file = "W:/JouniH/peereffects/data.rds")

nrow(d) # 123074 fathers
length(unique(d$workplace_id)) # 28932  workplaces
length(unique(d$group_id)) # 40305
group_sizes <- d %>% group_by(group_id) %>% 
  summarise(n = group_size[1]) %>% pull(n)
c(mean = mean(group_sizes), quantile(group_sizes, c(0.05,0.25,0.5,0.75,0.95)))
#     mean       5%      25%      50%      75%      95% 
# 3.079177 1.000000 1.000000 2.000000 3.000000 10.000000
c(mean = mean(d$n_earlier_births), 
  quantile(d$n_earlier_births, c(0.05,0.25,0.5,0.75,0.95)))
#     mean        5%       25%       50%       75%       95% 
# 5.183189  1.000000  1.000000  3.000000  6.000000 18.000000 

d %>% 
  mutate(before2013 = year %in% 2010:2012) %>% 
  group_by(before2013) %>% 
  summarise(
    uptake_percentage = mean(leave_length > 0),
    quota_uptake_percentage = mean(leave),
    median_leave_length = median(leave_length[leave_length > 0]),
    median_quota_length = median(leave_length[leave_length > 18]),
  )
# # A tibble: 2 x 5
# before2013 uptake_percentage quota_uptake_percentage median_leave_length median_quota_length
#  <lgl>                  <dbl>                   <dbl>               <int>               <dbl>
# 1 FALSE                  0.880                   0.491                  32                  53
# 2 TRUE                   0.868                   0.352                  18                  54

