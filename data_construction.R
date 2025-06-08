## Create data for peer effects models
library(data.table)

#### Load data ####
d <- readRDS("W:/Simon/PREDLIFE/PeerEffects/Data/reform2009_longformat_Sept24.rds")

reclassify_occup <- function(x) {
  x[is.na(x)] <- "XXXXX"
  factor(substr(as.character(x), 1, 1), levels = c("X", 0:9))
}
edulevels <- c("Basic", "Upper secondary", "Lower tertiary", "Higher tertiary")
setdroplevels(d)

# select only relevant variables
d <- d[, .(father_id = shnro, 
           workplace_id = workplaceID, 
           partner_id = partnerID, 
           region = mkunta, 
           age = ika, 
           partner_age = pAge, 
           leave, 
           education = highEdu2, # basic, upper secondary, bachelors, higher tertiary
           partner_education = pEdu, # basic, upper secondary, bachelors, higher tertiary
           occup, 
           income, 
           partner_income = pWage, 
           month = birMon, 
           indust, 
           birth_date = birDate, 
           start, 
           start2, 
           spec_mat, 
           year = vuosi, 
           first_birth = firstBir, 
           numMen,
           numEmp,
           rollNumMean,
           rollSR, 
           sector)
]
# use non-rolling values for the first time points
d[, log_roll_emp := ifelse(is.na(rollNumMean), log(numEmp), log(rollNumMean))]
d[, roll_sex_ratio := ifelse(is.na(rollSR), numMen / numEmp, rollSR)]
#d[, numMen := NULL]
#d[, numEmp := NULL]
d[, rollNumMean := NULL]
d[, rollSR := NULL]

# basic transformations
d[, father_id := as.integer(father_id)]
d[, workplace_id := as.integer(workplace_id)]
d[, 
  `:=`(
    leave_length = leave,
    occupation = reclassify_occup(occup),
    # if mother's leave is missing (didn't take), use father's
    leave_start = ifelse(is.na(start), start2, start),
    time = as.numeric(birth_date - as.Date("2008-01-01")),
    month = factor(month, levels = 1:12),
    year = factor(year),
    industry = factor(ifelse(indust %in% c(0, 99), NA, indust)),
    sector = factor(sector, labels = c("private", "public"))
  )
]
d[, 
  `:=`(
    leave = as.integer(leave > 18),
    # if still missing (no leave), use birth date
    leave_start = ifelse(is.na(leave_start), birth_date, leave_start)
  )
]
d[, reform2013 := as.integer(leave_start >= as.Date("2013-01-01"))]
d[, reform2010 := as.integer(leave_start >= as.Date("2010-01-01"))]
d[, group_id := .GRP, by = .(workplace_id, occupation)]
d[, `:=`(
  start = NULL,
  start2 = NULL,
  indust = NULL,
  occup = NULL)
]
setkey(d, group_id, birth_date)

# Create variables which depend on all rows

# create income decile variables for each year
# for fathers 1.6% and for partners 16% has no income, define deciles without these
d[, 
  income_decile := {
    qs <- c(-Inf, quantile(income[is.finite(income)], seq(0.1, 1, by = 0.1), na.rm = TRUE))
    cut(income, qs, labels = 1:10, include.lowest = TRUE)
  },
  by = year
]
d[, 
  partner_income_decile := {
    qs <- c(-Inf, quantile(partner_income[is.finite(partner_income)], seq(0.1, 1, by = 0.1), na.rm = TRUE))
    cut(partner_income, qs, labels = 1:10, include.lowest = TRUE)
  },
  by = year
]

# variables related to father's previous leave taking
setkey(d, father_id, birth_date)
d[, own_leave_lag := shift(leave, type = "lag"), by = father_id]
d[, own_reform2010_lag := shift(reform2010, type = "lag"), by = father_id]
d[, own_reform2013_lag := shift(reform2013, type = "lag"), by = father_id]
d[, time_lag := shift(time, type = "lag"), by = father_id]
d[, own_timegap := time - time_lag]
d[, time_lag := NULL]

# variables related to peer
setkey(d, group_id, birth_date)

f <- function(father_id, leave, time, education, reform2010, reform2013) {
  n <- length(father_id)
  d <- data.table(
    peer_id = rep(NA_integer_, n),
    peer_leave = rep(NA_integer_, n),
    peer_timegap = rep(NA_real_, n),
    peer_education = rep(NA_real_, n),
    peer_reform2010 = rep(NA_integer_, n),
    peer_reform2013 = rep(NA_integer_, n),
    past_leaves = integer(n)
  )
  if (n > 1) {
    for (i in 2:n) { # from second father onwards
      current_father <- father_id[i]
      idx <- which(father_id[1:(i - 1)] != current_father)
      k <- length(idx)
      if (k > 0) {
        j <- idx[k]
        peer_father <- father_id[j]
        d[i, peer_id := peer_father]
        d[i, peer_leave := leave[j]]
        d[i, peer_timegap := time[i] - time[j]]
        d[i, peer_education := education[j]]
        d[i, peer_reform2010 := reform2010[j]]
        d[i, peer_reform2013 := reform2013[j]]
        # number of unique fathers who took leave and are not current or peer
        if (k > 1) {
          x <- father_id[idx]
          y <- leave[idx]
          tmp <- x[y & !(x %in% c(current_father, peer_father))]
          if (length(tmp) > 0) {
            d[i, past_leaves := length(unique(tmp))]
          }
        }
      }
    }
  }
  d
}
cnames <- c("peer_id", "peer_leave", "peer_timegap", "peer_education", 
            "peer_reform2010", "peer_reform2013", "past_leaves") 
d[, (cnames) := f(father_id, leave, time, education, reform2010, reform2013), by = group_id]

nrow(d)
length(unique(d$workplace_id))
length(unique(d$father_id))

# use only fathers eligible to 2010 reform and child is born _after_ January 2010, 
# and only those father's who have at least one peer
d <- d[reform2010 == 1 & birth_date >= as.Date("2010-02-01") & !is.na(peer_leave)]
d[, reform2010 := NULL]

nrow(d)
length(unique(d$workplace_id))
length(unique(d$father_id))

length(unique(d$group_id[d$peer_timegap==0])) # 228
# trim groups where peer has the same birth date as focal
# remove births either starting from first duplicate, or remove births before last duplicate
# which ever leads to less removals when accounting for potential missing values
d[, has_missing := is.na(income) | is.na(partner_education) | is.na(partner_age) | is.na(first_birth)]
d <- d[, 
       {
         idx <- which(peer_timegap == 0)
         if (length(idx) == 0) {
           .SD
         } else {
           first_idx <- idx[1]
           last_idx <- idx[length(idx)]
           n_before <- first_idx - 1
           n_after <- .N - last_idx
           first <- last <- 0
           if (n_before > 0) {
             first <- n_before - sum(has_missing[1:n_before])
           }
           if (n_after > 0) {
             last <- n_after - sum(has_missing[(last_idx + 1):.N])
           }
           if (first >= last) {
             .SD[1:n_before]
           } else {
             .SD[(last_idx + 1):.N]
           }
         }
       }, 
       by = group_id
]

# remove fathers with missing data (income, partner_education, partner_age, first_birth)
d <- d[which(!has_missing)]
d[, has_missing := NULL]

# remove groups which have special maternity leave
d <- d[, if(all(is.na(spec_mat))) .SD, by = group_id]
d[, spec_mat := NULL]

# remove groups where own previous child and peer had same birth date (none)
d <- d[!(group_id %in% d[which(own_timegap == peer_timegap), group_id])]

# remove industry 22 as there is only 10 fathers with that code (2 groups)
# also 26 with 66 and 31 with 86 
# in all other industries there are >>100 fathers
table(d$industry)
d <- d[, if (!any(industry %in% c(22, 26, 31))) .SD, by = group_id]
# remove also armed forces occupation as there is only 58 fathers
# there are >>100 fathers in others
table(d$occupation)
d <- d[, if (!any(occupation == 0)) .SD, by = group_id]

# additional variables

d[, partner_higher_income := factor(
  partner_income > income, 
  levels = c(FALSE, TRUE), 
  labels = c("lower", "higher")
)
]
d[, income := NULL]
d[, partner_income := NULL]
d[, education_peer := factor(
  fcase(
    education > peer_education, "lower",
    education == peer_education, "same",
    education < peer_education, "higher"
  ),
  levels = c("lower", "same", "higher")
)
]
d[, education_partner := factor(
  fcase(
    education > partner_education, "lower",
    education == partner_education, "same",
    education < partner_education, "higher"
  ),
  levels = c("lower", "same", "higher")
)
]
d[, education := factor(education, levels = 1:4, labels = edulevels)]
d[, partner_education := factor(partner_education, levels = 1:4, labels = edulevels)]

d[, own_after_peer := factor(
  fcase(
    first_birth == 1, "no",
    is.na(own_timegap), "no",
    own_timegap > peer_timegap, "no",
    own_timegap < peer_timegap, "yes"
  )
)
]
# remove fathers who had previous child after the peer as these makes the peer effect complex to define
d <- d[own_after_peer == "no"]

d[, own_leave_lag := factor(
  fcase(
    first_birth == 1,"birth before 2009", #"first birth",  # this is only added with interaction with experienced
    is.na(own_leave_lag), "birth before 2009",
    own_leave_lag == 0, "no leave",
    own_leave_lag == 1, "leave"
  ), levels = c(
    #"first birth",
    "birth before 2009", 
    "no leave",
    "leave"
  )
)
]
d[, own_reform2013_lag := factor(
  fcase(
    #own_leave_lag == "first birth", "first birth",  # this is only added with interaction with experienced
    own_leave_lag == "birth before 2009", "not eligible",
    own_reform2013_lag == 0, "not eligible",
    own_reform2013_lag == 1, "eligible"
  ),
  levels = c(
   #"first birth",
    "not eligible",
    "eligible"
  )
)
]

d[, reform2013 := factor(reform2013, labels = c("not eligible", "eligible"))]

# predictor for those with earlier births
d[, experienced_extra := droplevels(interaction(d[, c("own_leave_lag", "own_reform2013_lag", "reform2013")]))]
d[, experienced := as.integer(first_birth == 0)]
# timegap between peer and focal father
m <- 3
max_m <- 48
d[, timegap_months := cut(
  peer_timegap,
  include.lowest = TRUE,
  breaks = c(30.4167 * seq(0, max_m, by = m), max(time)),
  labels = paste0(seq(0, max_m, by = m), 
                  rep(c("-",""), c(length(seq(0, max_m, by = m)) - 1, 1)), 
                  c(seq(m, max_m, by = m), "+"))
)
]

d[, past_leaves := cut(
  past_leaves, c(0:5, max(past_leaves)), 
  right = FALSE, include.lowest = TRUE,
  labels = c(0, 1, 2, 3, 4, "5+")
)
]

setdroplevels(d)
d[, workplace_id := .GRP, by = workplace_id]
d[, group_id := .GRP, by = group_id]
d[, father := seq_len(.N), by = group_id]
d[, n := .N, by = workplace_id]
# for modelling, use noncentered parameterization of random intercept for workplaces with < 20 observations
d[, ncp_u := n < 20]
# order based on ncp_u etc, make indexing in Stan easier
setorder(d, -ncp_u, workplace_id, group_id, birth_date)
# check that every possible birth date occurs (makes time indexing easier):
length(seq(as.Date("2010-02-01"), as.Date("2017-12-31"), by = "day"))
length(unique(d$time))

# remove extra variables
d[, own_reform2010_lag := NULL]
d[, peer_reform2010 := NULL]
d[, peer_timegap := NULL]
d[, own_timegap := NULL]
d[, peer_id := NULL]
d[, own_after_peer := NULL]
saveRDS(d, file = "W:/JouniH/peereffects/May2025/data.rds")

nrow(d) # 117837 births
length(unique(d$father_id)) # 97003 fathers
length(unique(d$workplace_id)) # 28304  workplaces
length(unique(d$group_id)) # 38895 groups
# 
# d |>
#   group_by(reform2013) |>
#   summarise(
#     uptake_percentage = mean(leave_length > 0),
#     quota_uptake_percentage = mean(leave),
#     median_leave_length = median(leave_length[leave_length > 0]),
#     median_quota_length = median(leave_length[leave_length > 18])
#   )
# # A tibble: 2 x 5
#  reform2013   uptake_percentage quota_uptake_percentage median_leave_length median_quota_length
#  <fct>                    <dbl>                   <dbl>               <dbl>               <dbl>
# 1 not eligible             0.867                   0.355                  18                  54
# 2 eligible                 0.880                   0.496                  33                  53

prop.table(table(d$peer_leave))
prop.table(table(d$first_birth))
d |>
  group_by(reform2013, first_birth) |> 
  summarize(median = median(leave_length))
d |> filter(leave_length > 0) |> 
  group_by(reform2013, first_birth) |> 
  summarize(median = median(leave_length))
d |> filter(leave == 1) |> 
  group_by(reform2013, first_birth) |> 
  summarize(median = median(leave_length))  


d |> 
  group_by(reform2013, peer_leave) |> 
  summarize(prop = mean(leave))

d |> 
  group_by(reform2013, peer_leave) |> 
  summarize(median = median(leave_length))  

d |> filter(leave_length > 0 ) |> 
  group_by(reform2013, peer_leave) |> 
  summarize(median = median(leave_length))  

d |> filter(leave == 1) |> 
  group_by(reform2013, peer_leave) |> 
  summarize(median = median(leave_length))  

d |> 
  group_by(own_leave_lag) |> 
  summarize(median = median(leave_length))
d |> 
  group_by(own_leave_lag) |> 
  summarize(prop = mean(leave))

d |>
  group_by(reform2013, first_birth) |> 
  summarize(prop = mean(leave))

# number of groups per workplace
# table(d |> dplyr::group_by(workplace_id) |> dplyr::summarise(n=dplyr::n_distinct(group_id)) |> dplyr::pull(n))
# 1     2     3     4     5     6     7     8 
# 21301  4619  1534   582   196    61     8     3

# number of observations per workplace:
# quantile(d |> dplyr::group_by(workplace_id) |> dplyr::summarise(n=dplyr::n()) |> dplyr::pull(n), seq(0, 1, by = 0.1))
#  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#   1    1    1    1    2    2    3    4    5    10   85  
# summary(d$father)


d |> mutate(x = year %in% 2010:2012) |> 
  group_by(x, workplace_id) |>
  summarise(
    uptake_percentage = mean(leave_length > 0),
    quota_uptake_percentage = mean(leave),
    mean_n = mean(numEmp),
    prop_men = mean(numMen / numEmp)
  ) |> 
  group_by(x) |> 
  summarise(
    uptake_percentage = mean(uptake_percentage),
    quota_uptake_percentage = mean(quota_uptake_percentage),
    median_n = median(mean_n),
    mean_n = mean(mean_n),
    prop_men = mean(prop_men)
  )

## appendix

d |>
  group_by(reform2013, peer_leave) |> 
  summarize(prop = 100*mean(leave), median = median(leave_length))


d |>
  group_by(education, reform2013) |> 
  summarize(prop = 100*mean(leave), median = median(leave_length))


d |> filter(leave_length>0) |> 
  group_by(reform2013, peer_leave) |> 
  summarize(median = median(leave_length))


d |>  filter(leave_length>0) |> 
  group_by(education, reform2013) |> 
  summarize( median = median(leave_length))


d |> filter(leave==1) |> 
  group_by(reform2013, peer_leave) |> 
  summarize(median = median(leave_length))


d |>  filter(leave==1) |> 
  group_by(education, reform2013) |> 
  summarize( median = median(leave_length))

mean(d$first_birth)


d |> group_by(reform2013) |> 
  summarize(quota = mean(leave), any_leave = mean(leave_length > 0))
d |> group_by(reform2013, first_birth) |> 
  summarize(quota = 100*mean(leave), any_leave = 100*mean(leave_length > 0))

d |> filter(leave_length > 0 ) |> 
  group_by(reform2013) |> 
  summarize(duration = median(leave_length))
d |> filter(leave_length > 0 ) |> 
  group_by(reform2013, first_birth) |> 
  summarize(duration = median(leave_length))


d |> filter(leave == 1) |> 
  group_by(reform2013) |> 
  summarize(duration = median(leave_length))
d |> filter(leave == 1) |> 
  group_by(reform2013, first_birth) |> 
  summarize(duration = median(leave_length))

d |> 
  group_by(reform2013) |> 
  summarize(quota = 100*mean(leave), any_leave = 100*mean(leave_length > 0))

d |> 
  group_by(reform2013) |> 
  summarize(N = mean(leave), any_leave = 100*mean(leave_length > 0))

prop.table(table(d$own_leave_lag[d$first_birth == 0],d$leave[d$first_birth == 0]),margin=1)

d |> 
  filter(first_birth == 0) |> 
  group_by(own_leave_lag) |> 
  summarise(median = median(leave_length), prop = mean(leave))
