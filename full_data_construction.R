#Data preparation code for Heterogeneous workplace peer effects in fathers' parental leave uptake in Finland#

#Note that filepaths on D:/ may differ in other projects (file names will remain consistent, however). For paths to own workplace drive, W:/user/ has been used - replace as needed

#Details on the variables included can be found in the metadata files on FIONA

#rm() and gc() are spread throughout the script to minimise memory usage and return unused memory to the shared environment for potential use by others

#Some encoding issues caused by the KELA data - using UTF encoding to fix these issues messes with Finnish characters (e.g. in file paths)

#Libraries####
#for reading in data
library(haven)
#for data manipulation
library(data.table)

#Custom function for industry classification####
#Statistics Finland uses TOL classification for industries
#See metadata files for employment/TOL classifications on Stats Fin website

#This function converts the numeric form in FOLK files to the letter form of the overall industry, rather than the specific multi-digit factor

#should only be used on data post-2007 (classification changed in 2008)
industCode <- function(x){
  #where x is "toimiala" or "toimiala2" (industry)
  
  numCodeShort = as.numeric(as.character(substr(x, 1, 2))) #takes the first two digits (the important bit for overall industry)
  
  numCodeMed = as.numeric(as.character(substr(x, 1, 3)))
  numCodeLong = as.numeric(as.character(substr(x, 1, 4)))
  
  n3 = c(423, 433, 491, 493, 501, 503, 511, 841, 843, 842, 931, 932)
  n4 = c(4391, 8422)
  
  in1 = ifelse(numCodeMed %in% n3, 1, 0)
  in2 = ifelse(numCodeLong %in% n4, 1, 0)
  
  code = ifelse(in1 == 0 & in2 == 0, numCodeShort, ifelse(in2 == 1, numCodeLong, numCodeMed))
  
  mat1 = matrix(c(0, 0, #X, unknown
                  1, 1, #A
                  1, 2, #A
                  1, 3, #A
                  2, 5, #B
                  2, 6, #B
                  2, 7, #B
                  2, 8, #B
                  2, 9, #B
                  3, 10, #C
                  3, 11, 
                  3, 12, 
                  3, 13,
                  3, 14,
                  3, 15,
                  3, 16,
                  3, 17,
                  3, 18, 
                  3, 19,
                  3, 20,
                  3, 21,
                  3, 22,
                  3, 23,
                  3, 24,
                  3, 25,
                  3, 26,
                  3, 27,
                  3, 28,
                  3, 29,
                  3, 30,
                  3, 31,
                  3, 32,
                  3, 33,
                  4, 35, #D
                  5, 36, #E
                  5, 37,
                  5, 38,
                  5, 39, 
                  6, 41, #F
                  6, 42,
                  6, 43,
                  7, 423, #Skilled labourers
                  7, 433,
                  7, 4391,
                  8, 45, #G
                  8, 46,
                  8, 47,
                  9, 49, #H
                  9, 50,
                  9, 51,
                  9, 52,
                  9, 53,
                  10, 491, #passenger transport
                  10, 493,
                  10, 501,
                  10, 503,
                  10, 511,
                  11, 55, #accommodation
                  12, 56, #food services
                  13, 58, #J
                  13, 59,
                  13, 60,
                  13, 61,
                  13, 62,
                  13, 63,
                  14, 64, #K
                  14, 65,
                  14, 66,
                  15, 68, #L
                  16, 69, #legal
                  17, 70, #head offices, consultancy
                  18, 71, #archi & engi
                  19, 72, #science r&d
                  20, 73, #advertising and marketing
                  21, 74, #other pro stuff
                  22, 75, #vet
                  23, 77, #admin and support services
                  23, 78,
                  23, 79,
                  23, 80,
                  23, 81,
                  23, 82,
                  24, 841, #state admin
                  24, 8421,
                  24, 843,
                  25, 842, #community services (police etc.)
                  26, 8422, #defence
                  27, 85, #education
                  28, 86, #human health
                  29, 87, #social care
                  29, 88,
                  30, 90, #creative
                  31, 91, #cultural
                  32, 931, #sports
                  33, 92, #other
                  33, 932,
                  33, 94,
                  33, 95,
                  33, 96,
                  99, 97, #to be removed
                  99, 98,
                  99, 99), ncol = 2, byrow = TRUE)
  
  return(mat1[match(code, mat1[, 2]), 1])
  
}

#Data pre-prep####
#Basic file####
#File containing the 'basic' information for people
#Variables: year, ID, sex, age, age of youngest child, municipality, region, education, syntyp2 is origin (1 = finn/finland, 2 = finn/abroad, 3 = for/finland, 4 = for/abroad), matriculation exam 0 = no, 4 = yes

#2001-2010
basic1 <- read_dta("D:/ready-made/CONTINUOUS/FOLK_PERUS_C/folk_20012010_tua_perus22tot_1.dta", col_select = c("vuosi", "shnro", "sukup", "ika", "penulaika", "kunta", "mkunta", "ututku_aste", "syntyp2", "yotutk"))
setDT(basic1)

#change all "" values to NA
basic1[basic1 == ""] <- NA
basic1 <- basic1[ika >= 18]
gc()

#2011-2020
basic2 <- read_dta("D:/ready-made/CONTINUOUS/FOLK_PERUS_C/folk_20112020_tua_perus22tot_1.dta", col_select = c("vuosi", "shnro", "sukup", "ika", "penulaika", "kunta", "mkunta", "ututku_aste", "syntyp2", "yotutk"))
setDT(basic2)

#change all "" values to NA
basic2[basic2 == ""] <- NA
basic2 <- basic2[ika >= 18]
gc()

#create a list with the data.tables in
l1 <- list(basic1, basic2)
rm(basic1); rm(basic2); gc()

#combine data.tables with rbindlist
l2 <- rbindlist(l1, use.names = TRUE)
rm(l1); gc()

#if education missing, we class as basic only (1)
l2[is.na(ututku_aste) == TRUE, ututku_aste := 1]

#convert columns to appropriate form to reduce object size
#character to factor
charCol <- names(l2)[sapply(l2, is.character)]
l2[, (charCol) := lapply(.SD, as.factor), .SDcols = charCol]
gc()

#numeric to integer
numCol <- names(l2)[sapply(l2, is.numeric)]
l2[, (numCol) := lapply(.SD, as.integer), .SDcols = numCol]
gc()

saveRDS(l2, "W:/user/basic_0120.rds")

rm(l2); gc()

#Employment####
#Variables: year, ID, enterprise id, establishment id, employment status, occupation,  start date of current employment, industry, start date of employment, end date of employment 

#2001-2010
employ1 <- read_dta("D:/ready-made/CONTINUOUS/FOLK_TKT_C/folk_20012010_tua_tkt21tot_1.dta", col_select = c("vuosi", "shnro", "syrtun2", "sykstun2", "ptoim2", "ammattikoodi", "toimiala2", "alkupvm2", "loppupvm2"))
setDT(employ1)

#change all "" values to NA
employ1[employ1 == ""] <- NA
gc()

#2011-2020
employ2 <- read_dta("D:/ready-made/CONTINUOUS/FOLK_TKT_C/folk_20112018_tua_tkt21tot_1.dta", col_select = c("vuosi", "shnro", "syrtun2", "sykstun2", "ptoim2", "ammattikoodi", "toimiala2", "alkupvm2", "loppupvm2"))
setDT(employ2)

#change all "" values to NA
employ2[employ2 == ""] <- NA
gc()

employ3 <- read_dta("D:/ready-made/CONTINUOUS/FOLK_TKT_C/folk_tkt_2019_1.dta", col_select = c("vuosi", "shnro", "syrtun2", "sykstun2", "ptoim2", "ammattikoodi", "toimiala2", "alkupvm2", "loppupvm2"))
setDT(employ3)

#change all "" values to NA
employ3[employ3 == ""] <- NA
gc()

employ4 <- read_dta("D:/ready-made/CONTINUOUS/FOLK_TKT_C/folk_tkt_2020_1.dta", col_select = c("vuosi", "shnro", "syrtun2", "sykstun2", "ptoim2", "ammattikoodi", "toimiala2", "alkupvm2", "loppupvm2"))
setDT(employ4)

#change all "" values to NA
employ4[employ4 == ""] <- NA
gc()

#create a list with the data.tables in
l1 <- list(employ1, employ2, employ3, employ4)
rm(employ1); rm(employ2); rm(employ3); rm(employ4); gc()

#join the data.tables together
l2 <- rbindlist(l1, use.names = TRUE)
rm(l1); gc()

#convert columns to appropriate form to reduce object size
#character to factor
charCol <- names(l2)[sapply(l2, is.character)]
l2[, (charCol) := lapply(.SD, as.factor), .SDcols = charCol]
gc()

#numeric to integer
numCol <- names(l2)[sapply(l2, is.numeric)]
l2[, (numCol) := lapply(.SD, as.integer), .SDcols = numCol]
gc()

saveRDS(l2, "W:/user/employ_0120.rds")
rm(l2); gc()

#Income####
#Variables: year, ID, wage income, entrepreneurial income
#2001-2010
income1 <- read_dta("D:/ready-made/CONTINUOUS/FOLK_TULO_C/folk_20012010_tua_tulo21tot_1.dta", col_select = c("vuosi", "shnro", "palk", "yrtu"))
setDT(income1)

#change all "" values to NA
income1[income1 == ""] <- NA
gc()

#2011-2020
income2 <- read_dta("D:/ready-made/CONTINUOUS/FOLK_TULO_C/folk_20112019_tua_tulo21tot_1.dta", col_select = c("vuosi", "shnro", "palk", "yrtu"))
setDT(income2)

#change all "" values to NA
income2[income2 == ""] <- NA
gc()

#create a list with the data.tables in
l1 <- list(income1, income2)
rm(income1); rm(income2); gc()

#join the data.tables together
l2 <- rbindlist(l1, use.names = TRUE)
rm(l1); gc()

#convert columns to appropriate form to reduce object size
#character to factor
charCol <- names(l2)[sapply(l2, is.character)]
l2[, (charCol) := lapply(.SD, as.factor), .SDcols = charCol]
gc()

#numeric to integer
numCol <- names(l2)[sapply(l2, is.numeric)]
l2[, (numCol) := lapply(.SD, as.integer), .SDcols = numCol]
gc()

saveRDS(l2, "W:/user/income01.rds")
rm(l2); gc()

#Birth register####
#variables: LAPSEN_SYNTYMAPVM (time & date of birth), monisiki etc (multiple birth)
df <- read_dta("D:/f89/external/thl_data/thl_synre_korjattu.dta", col_select = c("shnro_lapsi", "shnro_aiti", "LAPSEN_SYNTYMAPVM", "MONISIKI_SYNNYTYSTUNNUS"))
setDT(df)

#remove those without mother information
df <- df[shnro_aiti != ""]
gc()

#change all "" values to NA
df[df == ""] <- NA
gc()

#birth date is in an odd format
#take first 9 characters (DDMONYYYY)
df[, birDay := as.numeric(substr(LAPSEN_SYNTYMAPVM, 1, 2))]
df[, birMonName := substr(LAPSEN_SYNTYMAPVM, 3, 5)]
df[, birYear := as.numeric(substr(LAPSEN_SYNTYMAPVM, 6, 9))]

#make character month numeric
months <- toupper(month.abb)
df[, birMon := match(birMonName, months)]

#remove columns that are not needed
remCol <- c("LAPSEN_SYNTYMAPVM", "birMonName")
df[, (remCol) := NULL]

#reduce to births from 1990 - children born in the sample are unlikely to have siblings before this point, so reasonable start date for counting births
df <- df[birYear >= 1990]

#convert columns to appropriate form to reduce object size
#character to factor
charCol <- names(df)[sapply(df, is.character)]
df[, (charCol) := lapply(.SD, as.factor), .SDcols = charCol]
gc()

#numeric to integer
numCol <- names(df)[sapply(df, is.numeric)]
df[, (numCol) := lapply(.SD, as.integer), .SDcols = numCol]
gc()

saveRDS(df, "W:/user/birthregister00.rds")
rm(df); gc()

#Kela parental leave####

#directory of files: D:/e17/external/Kela_data/Kela_vanhempainp?iv?rahat
#variables:
# SAAJA_HETU - ID
# LAPSI_HETU - child ID
# PUOLISO_HETU - spouse ID
# MATY - payment type
# PALKM - number of days (in month)
# YHTE - sum of payment
# ETOSA - type of benefit
# JALJ - part of benefit (e.g. if first 18 for fathers)

kela09 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2009_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela10 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2010_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela11 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2011_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela12 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2012_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela13 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2013_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela14 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2014_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela15 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2015_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela16 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2016_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela17 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2017_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela18 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2018_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))
kela19 <- read_dta("D:/f89/external/Kela_data/Kela_vanhempainp?iv?rahat/vanhempainpaivaraha_2019_s.dta", col_select = c("shnro_saaja", "shnro_lapsi", "shnro_puoliso", "maty", "palkm", "yhte", "pamk", "etosa", "jalj", "mapv", "makalpv", "maklopv", "akalpv", "aklopv"))

#create a list with the data.tables in
l1 <- list(kela09, kela10, kela11, kela12, kela13, kela14, kela15, kela16, kela17, kela18, kela19)
#remove individual files
rm(kela09); rm(kela10); rm(kela11); rm(kela12); rm(kela13); rm(kela14); rm(kela15); rm(kela16); rm(kela17); rm(kela18); rm(kela19); gc()

#combine all
l2 <- rbindlist(l1, use.names = TRUE)
rm(l1); gc()

#change all "" values to NA
l2[l2 == ""] <- NA

#for women, payments with shnro_lapsi = NA are pre-birth (but should be linked to the birth) - need to order first, then nafill
#to get number of days correct, we need to remove overlapping months etc.
#order the rows by id and start date
l2 <- l2[order(shnro_saaja, makalpv)]

#row numbers
l2[, rownum := 1:.N]

#take row number of first observation by id and start/end dates of the leave
l2[l2[, .SD[1], by = .(shnro_saaja, akalpv)], on = .(shnro_saaja, akalpv), "shnro_lapsi2" := i.rownum]
l2[l2[, .SD[1], by = .(shnro_saaja, aklopv)], on = .(shnro_saaja, aklopv), "shnro_lapsi3" := i.rownum]

#if same as rownum, first use rownum
l2[rownum == shnro_lapsi2 & rownum == shnro_lapsi3, shnro_lapsi4 := rownum]
#if only different from 3
l2[rownum == shnro_lapsi2 & rownum != shnro_lapsi3, shnro_lapsi4 := shnro_lapsi3]
#all else start
l2[rownum != shnro_lapsi2, shnro_lapsi4 := shnro_lapsi2]

#need the consecutive marker for aklopv to next row akalpv 
l2[, oldak := shift(aklopv, 1), by = .(shnro_saaja)]
#shift previous row child ID (by individual)
l2[, lastlaps := shift(shnro_lapsi, 1), by = .(shnro_saaja)]

l2[(as.Date(akalpv) == as.Date(oldak) | as.Date(akalpv) == (as.Date(oldak) + 1)| as.Date(oldak) > as.Date(akalpv)) & (shnro_lapsi == lastlaps | is.na(lastlaps) == TRUE), marked := 0]

#Check previous row codes for kind of leave
l2[, c("prevetosa", "prevjalj") := .(shift(etosa, 1), shift(jalj, 1)), by = .(shnro_saaja)]
#Note, the codes for etosa and jalj can mess with the script everytime it is opened (due to the characters used in the original dataset) - use l2[, levels(etosa)] and then copy the odd character into the relevant places in the script
l2[marked == 0 & is.na(shnro_lapsi) == TRUE & is.na(lastlaps) == TRUE & etosa == "?"R" & prevetosa == "?"V" & jalj == "?"R" & prevjalj == "?"V", marked := NA]

#marks the starting ones
l2[is.na(marked) == TRUE, marked := 1]

#As only starts have marked == 1, and every other marked == 0, each one should be assigned to a different child correctly
l2[, shnro_lapsi5 := cumsum(marked)]

#keep those with correct payment types
l2 <- l2[maty %in% c("S", "T", "V")] #regular payment, retrospective payment, one-off payment
gc()

#type of benefit has to be of certain kind
#this is a bit messed up because of coding - the double commas do not work
#have to levels(as.factor(etosa)) and then copy it across
l2 <- l2[etosa %in% c("IR", "IV", "E?"", "?"R", "?"V", "OR", "OV")]
gc()

#remove duplicated rows (via start date, end date, number of days, type of benefit)
l2 <- l2[, .SD[1], by = .(shnro_saaja, shnro_lapsi5, makalpv, maklopv, palkm, etosa, jalj)]
gc()

#select only benefits > 0 
l2 <- l2[palkm > 0]
gc()

#mark if overlapping start date
l2[, overlap := .N, by = .(shnro_saaja, shnro_lapsi5, makalpv)]

#remove longer than 25 - as this could affect men if payments are mostly retrospective, only do this if overlapping
l2 <- l2[(overlap > 1 & palkm <= 25) | overlap == 1]

#mark if overlapping end
l2[, overlap2 := .N, by = .(shnro_saaja, shnro_lapsi5, maklopv)]

#marker of whether the payment was for one day only (i.e. makalpv = maklopv)
#this can be a problem if overlaps
l2[makalpv == maklopv, sameMark := 1]
l2[sameMark == 1 & overlap2 == 2, remove := 1]

l2 <- l2[is.na(remove) == TRUE]
gc()
#mark if there is overlap in the dates - this should not be the case
#while goes to 12 based on previous trial and error (number stops decreasing, meaning overlaps eliminated)
x = 1

while(x <= 12){
l2[, prevDate := shift(maklopv, 1), by = .(shnro_saaja, shnro_lapsi5)]
l2[as.Date(maklopv) <= as.Date(prevDate), overlap3 := 1]
l2 <- l2[is.na(overlap3) == TRUE]

gc()

x = x + 1
print(l2[, .N])
}

#shift samemark - needed to make sure the 1 day does not take precedence
l2[, sameMark2 := shift(sameMark, 1), by = .(shnro_saaja, shnro_lapsi5)]

#mark overlap if start date = previous end date
l2[makalpv == prevDate, overlap3 := 1]
l2[l2[overlap3 == 1], on = .(shnro_saaja, shnro_lapsi5), "check" := i.overlap3]

#to remove the single days that are causing an overlap issue, need to back-shift
l2[, overlap4 := shift(overlap3, 1, type = "lead")]
#remove if overlap4 is 1 and has sameMark
l2 <- l2[is.na(overlap4) == TRUE | (overlap4 == 1 & is.na(sameMark) == TRUE)]
gc()

#Remove excess cols
remCols <- c("prevDate", "remove", "overlap", "overlap2", "overlap4", "overlapX", "mark2", "check", "check2")
l2[, (remCols) := NULL]
gc()

#RUNNING BLOCK OF CODE FROM EARLIER AGAIN TO FIX SOME ISSUES WITH NAs AND CHILDREN NOT BEING CORRECTLY ATTRIBUTED
l2[, marked := NULL]

l2[, oldak := shift(aklopv, 1), by = .(shnro_saaja)]
l2[, lastlaps := shift(shnro_lapsi, 1), by = .(shnro_saaja)]
l2[(as.Date(akalpv) == as.Date(oldak) | as.Date(akalpv) == (as.Date(oldak) + 1)| as.Date(oldak) > as.Date(akalpv)) & (shnro_lapsi == lastlaps | is.na(lastlaps) == TRUE), marked := 0]

l2[, c("prevetosa", "prevjalj") := .(shift(etosa, 1), shift(jalj, 1)), by = .(shnro_saaja)]
l2[marked == 0 & is.na(shnro_lapsi) == TRUE & is.na(lastlaps) == TRUE & etosa == "?"R" & prevetosa == "?"V" & jalj == "?"R" & prevjalj == "?"V", marked := NA]

l2[is.na(marked) == TRUE, marked := 1]
l2[, shnro_lapsi5 := cumsum(marked)]

#take first row of each child ID when not missing
kpl2 <- l2[is.na(shnro_lapsi) == FALSE, .SD[1], by = .(shnro_saaja, shnro_lapsi)]
#merge back to all as a different shnro_lapsi5 code
l2[kpl2, on = .(shnro_saaja = shnro_saaja, shnro_lapsi = shnro_lapsi), "shnro_lapsi5" := i.shnro_lapsi5]

#take first by shnro_lapsi5 (which includes those with missing shnro_lapsi)
kpl1 <- l2[, .SD[1], by = .(shnro_saaja, shnro_lapsi5)]
kpl1[, year := substr(akalpv, 1, 4)]
kpl1[, num := .N, by = .(shnro_saaja, year)]
#marker to show more than 1 in year (for whatever reason)
l2[kpl1, on = .(shnro_saaja = shnro_saaja, shnro_lapsi5 = shnro_lapsi5), year := i.year]
l2[kpl1, on = .(shnro_saaja = shnro_saaja, year = year), marker := i.num]


l2[l2[is.na(shnro_lapsi) == FALSE, .SD[1], by = .(shnro_saaja, shnro_lapsi5)], on = .(shnro_saaja = shnro_saaja, shnro_lapsi5 = shnro_lapsi5), shnro_lapsActual := i.shnro_lapsi]

#check length of shnro_lapsi5 before and then do it again after the next section - if number drops too much, then we have a problem
l2[, length(unique(shnro_lapsi5))] #1114738

#nafill locf and nocb - if == then fill with either, if not equal then leave as NA
l2[is.na(shnro_lapsActual) == FALSE, newshnro := shnro_lapsi5]
l2[, newMark1 := nafill(newshnro, type = 'locf'), by = .(shnro_saaja)]
l2[, newMark2 := nafill(newshnro, type = 'nocb'), by = .(shnro_saaja)]

l2[newMark1 == newMark2 & is.na(newMark1) == FALSE, newshnro := newMark1]

l2[is.na(newshnro) == TRUE, newshnro := shnro_lapsi5]

remcols <- c("sameMark", "overlap3", "sameMark2", "shnro_lapsi2", "shnro_lapsi3", "shnro_lapsi4", "rownum", "pamk")
l2[, c(remcols) := NULL]

l2[, rownum := 1:.N]
l2[jalj == "I?"" & is.na(shnro_lapsi) == TRUE, mark3 := 1]
l2[jalj == "I?"" & shnro_lapsi5 != newMark1 & newMark1 == newMark2, mark5 := 1]

l2[, mark3 := nafill(mark3, type = "nocb"), by = .(shnro_saaja)]
#View(l2[mark3 == 1]) #only 161 obs, so manual fixing - this perhaps could have been automated (And possibly needs to be if more years become available)

l2[, mark3 := NULL]

#manual replacement
l2[rownum == 334125, newshnro := 39506]
l2[rownum == 876608, newshnro := 104063]
l2[rownum == 1268605, newshnro := 105609]
l2[rownum == 1268606, newshnro := 105609]
l2[rownum == 2146379, newshnro := 254717]
l2[rownum == 2704725, newshnro := 321262]
l2[rownum == 2723691, newshnro := 323560]
l2[rownum == 2723692, newshnro := 323560]
l2[rownum == 5702591, newshnro := 678301]
l2[rownum == 5702592, newshnro := 678301]
l2[rownum == 5915493, newshnro := 703668]
l2[rownum == 6601858, newshnro := 784771]
l2[rownum == 6601859, newshnro := 784771]
l2[rownum == 8031746, newshnro := 955098]

#Filling in missing shnro_lapsi from mother ID
#jalj == IR indicates taken at same time as mother, so match year to mother that way if shnro_puoliso not missing
#via birthReg - actual date of birth + a month
l3 <- l2[is.na(shnro_lapsi) == TRUE & jalj == "IR", .SD[1], by = .(newshnro)]
l3[, makalpv := as.Date(makalpv)]
l3[, year := as.numeric(year)]

#load in prepared birth register
birthReg <- readRDS("W:/user/birthregister00.rds")
setDT(birthReg)

#make birth date a date variable
birthReg[, birth := as.Date(paste(birYear, birMon, birDay, sep = "-"))]
#add 30 days to cover instances where leave may not start immediately on the birth day (e.g. father leave first 18 days)
birthReg[, birth2 := (birth + 30)]
l3[birthReg, on = .(shnro_puoliso = shnro_aiti, year = birYear, makalpv < birth2), "shnro_lapsi" := i.shnro_lapsi]
l3[is.na(shnro_lapsi) == FALSE, .N]

l2[, makalpv := as.Date(makalpv)]
l2[l3[is.na(shnro_lapsi) == FALSE], on = .(shnro_saaja = shnro_saaja, newshnro = newshnro, etosa = etosa, jalj = jalj, maklopv = maklopv, makalpv = makalpv), shnro_lapsi := i.shnro_lapsi]

#running line again to give new correct shnro_lapsActual
l2[, shnro_lapsActual := NULL]
l2[l2[is.na(shnro_lapsi) == FALSE, .SD[1], by = .(shnro_saaja, newshnro)], on = .(shnro_saaja = shnro_saaja, newshnro = newshnro), shnro_lapsActual := i.shnro_lapsi]

#start and end of leave periods
l2[l2[, .SD[1], by = .(newshnro)], on = .(newshnro = newshnro), "startA" := i.makalpv]
l2[l2[, .SD[.N], by = .(newshnro)], on = .(newshnro = newshnro), "stopA" := i.aklopv]

#convert columns to appropriate form to reduce object size
#character to factor
charCol <- names(l2)[sapply(l2, is.character)]
l2[, (charCol) := lapply(.SD, as.factor), .SDcols = charCol]
gc()

#numeric to integer
numCol <- names(l2)[sapply(l2, is.numeric)]
l2[, (numCol) := lapply(.SD, as.integer), .SDcols = numCol]
gc()

saveRDS(l2, "W:/user/kela_parbenefits.rds")
"

#Combining kela parental leave and birth register####
#Files for getting kela info into the birth register, or getting a marker for NOT being in the birth register

library(data.table)
library(haven)

birthReg <- readRDS("W:/user/birthregister00.rds")
setDT(birthReg)

birthReg <- birthReg[birYear < 2020]
gc()

kelaparleave <- readRDS("W:/user/kela_parbenefits.rds")
setDT(kelaparleave)

kidInfo <- readRDS("W:/Simon/rdsTemp/kidinfo.rds")
setDT(kidInfo)

#merge kidInfo to birthReg
birthReg[kidInfo, on = .(shnro_aiti = shnro_m, shnro_lapsi = shnro), c("fatherID", "adoptMoth", "adoptFat") := .(i.shnro_f, i.shnro_am, i.shnro_af)]

#set birth as a date AND birth + 2 (as premature births may not have birth starting after leave)
birthReg[, birth := as.Date(paste(birYear, birMon, birDay, sep = "-"))]
birthReg[, birth2 := (birth + 4)]

#modification of the kela file
#set start and stop periods as dates
kelaparleave[, stopA := as.Date(stopA)]
kelaparleave[, startA := as.Date(startA)]

#sum parental leave
kelaparleaveFull <- kelaparleave[, lapply(.SD, sum), by = .(shnro_saaja, newshnro), .SDcols = c("palkm")]
#take various important (for later) variables from first row of child info from kela data to this new object
kelaparleaveFull[kelaparleave[, .SD[1], by = .(newshnro)], on = .(newshnro = newshnro), c("shnro_lapsActual", "startA", "stopA", "shnro_puoliso", "etosa", "jalj") := .(i.shnro_lapsActual, i.startA, i.stopA, i.shnro_puoliso, i.etosa, i.jalj)]

#replace the full kela with this new summed object
kelaparleave <- kelaparleaveFull
rm(kelaparleaveFull); gc()

#merging kela to birth reg
birthReg[, birth := as.Date(birth)]

#MOTHERS
birthReg[kelaparleave[is.na(shnro_lapsActual) == FALSE], on = .(shnro_aiti = shnro_saaja, shnro_lapsi = shnro_lapsActual), "shnro_lapsiM" := i.newshnro]

birthReg[kelaparleave[is.na(shnro_lapsActual) == TRUE], on = .(shnro_aiti = shnro_saaja, birth2 >= startA, birth <= stopA), "shnro_lapsiM" := i.newshnro]

#FATHERS
birthReg[kelaparleave[is.na(shnro_lapsActual) == FALSE], on = .(fatherID = shnro_saaja, shnro_lapsi = shnro_lapsActual), "shnro_lapsiF" := i.newshnro]

#ADOPTIONS FATHERS
birthReg[kelaparleave[is.na(shnro_lapsActual) == FALSE], on = .(adoptFat = shnro_saaja, shnro_lapsi = shnro_lapsActual), "shnro_lapsiAF" := i.newshnro]

#ADOPTIONS MOTHERS
birthReg[kelaparleave[is.na(shnro_lapsActual) == FALSE], on = .(adoptMoth = shnro_saaja, shnro_lapsi = shnro_lapsActual), "shnro_lapsiAM" := i.newshnro]

birthReg[kelaparleave, on = .(shnro_lapsiM = newshnro), c("motherLeave", "etosaM", "jaljM") := .(i.palkm, i.etosa, i.jalj)]
birthReg[kelaparleave, on = .(shnro_lapsiF = newshnro), c("fatherLeave", "etosa", "jalj") := .(i.palkm, i.etosa, i.jalj)]
birthReg[birYear == 2008 & ((etosa == "IV" & jalj == "IV") | (etosa == "IV" & jalj == "IS") | (etosaM == "?"V" & jaljM == "?"V") | (etosaM == "?"R" & jaljM == "?"R")), inc08 := 1] #if 2008 birth and either parent on parental leave or mother on maternity, can include
birthReg[kelaparleave, on = .(shnro_lapsiAM = newshnro), "admotherLeave" := i.palkm]
birthReg[kelaparleave, on = .(shnro_lapsiAF = newshnro), "adfatherLeave" := i.palkm]

birthReg[kelaparleave, on = .(shnro_lapsiM = newshnro), "startDate" := i.startA]
birthReg[kelaparleave, on = .(shnro_lapsiF = newshnro), "startDate2" := i.startA]
birthReg[is.na(startDate) == TRUE, startDate := startDate2]
birthReg[, startDate2 := NULL]

#mark instances of special maternity - this may be due to premature births, pregnancy complications, or because the employment is classed as hazardous and requires pregnant mothers to go on leave early for sake of own and foetal health
birthReg[kelaparleave[etosa == "E?""], on = .(shnro_lapsiM = newshnro), "spec_mat" := 1]

birthReg[is.na(shnro_aiti) == FALSE & is.na(motherLeave) == TRUE, motherLeave := 0]
birthReg[is.na(fatherID) == FALSE & is.na(fatherLeave) == TRUE, fatherLeave := 0]
birthReg[is.na(adoptMoth) == FALSE & is.na(admotherLeave) == TRUE, admotherLeave := 0]
birthReg[is.na(adoptFat) == FALSE & is.na(adfatherLeave) == TRUE, adfatherLeave := 0]

#marker for the 'bad twin' - the one with no leave ascribed
birthReg[birthReg[is.na(MONISIKI_SYNNYTYSTUNNUS) == FALSE & motherLeave > 0, .SD[1], by = .(MONISIKI_SYNNYTYSTUNNUS, shnro_aiti)], on = .(shnro_aiti = shnro_aiti, MONISIKI_SYNNYTYSTUNNUS = MONISIKI_SYNNYTYSTUNNUS), "twinLeave" := i.motherLeave]
birthReg[twinLeave > 0 & motherLeave == 0, twinMark := 1]

#In birthReg, created a date variable out of the birth day, month, year, then order by birth date
birthReg[is.na(birMon) == FALSE & is.na(birDay) == FALSE, birDate := as.Date(paste(birYear, birMon, birDay, sep = "-"))]
birthReg <- birthReg[order(birDate)]

#sequence births by father ID (for later identification of first births)
birthReg[, numBirth := 1:.N, by = .(fatherID)]
birthReg[numBirth == 1, firstBir := 1]
birthReg[numBirth > 1, firstBir := 0]

birthReg[, .N, by = .(firstBir, birYear)]

birthReg <- birthReg[is.na(twinMark) == TRUE | (is.na(motherLeave) == TRUE & is.na(fatherLeave) == TRUE & is.na(admotherLeave) == TRUE & is.na(adfatherLeave) == TRUE)]

birthReg[, c("twinMark", "twinLeave") := NULL]

saveRDS(birthReg, "W:/user/birthRegPL.rds")

#due to missingness, we should mark those without info in the birthReg file
kelaparleave[!(shnro_saaja %in% c(birthReg[, shnro_aiti], birthReg[,fatherID], birthReg[,adoptMoth], birthReg[,adoptFat])), missing := 1]

#If father and IR, most likely indicates starts around birth
kelaparleave[missing == 1 & jalj == "IR", fatBirth := 1]

#then remove those from kelaparleave present in birthReg
kelaparleave <- kelaparleave[missing == 1]

saveRDS(kelaparleave, "W:/user/missingPL.rds")
rm(kelaparleave); gc()

"
#Actual data preparation####
#FOLK basic
basic <- readRDS("W:/Simon/rdsTemp/basic_0120.rds")
setDT(basic)

#Medical birth register with KELA PL associated
birthReg <- readRDS("W:/Simon/rdsTemp/birthRegPL.rds")
setDT(birthReg)

#Mark whether the births were twins
birthReg[is.na(MONISIKI_SYNNYTYSTUNNUS) == FALSE, isTwin := 1]

#For those missing from the medical birth register (but potentially able to be joined)
kelaparleave <- readRDS("W:/Simon/rdsTemp/missingPL.rds")
setDT(kelaparleave)

#FOLK employment
employment <- readRDS("W:/Simon/rdsTemp/employ_0120.rds")
setDT(employment)

#Retain only those employed (11)
employment <- employment[ptoim2 == 11]
gc()

#Education converted to 4-level education
basic[ututku_aste == "1", highEdu2 := 1] #basic only
basic[ututku_aste == "3" | ututku_aste == "4", highEdu2 := 2] #upper secondary
basic[ututku_aste == "5" | ututku_aste == "6", highEdu2 := 3] #lower tertiary
basic[ututku_aste == "7" | ututku_aste == "8", highEdu2 := 4] #higher tertiary

#In birthReg, created a date variable out of the birth day, month, year, then order by birth date
birthReg[is.na(birMon) == FALSE & is.na(birDay) == FALSE, birDate := as.Date(paste(birYear, birMon, birDay, sep = "-"))]
birthReg <- birthReg[order(birDate)]

#merge the birth register info to basic for mothers
basic[birthReg, on = .(shnro = shnro_aiti, vuosi = birYear), c("birDay", "birMon", "leave", "start", "spec_mat", "isTwin") := .(i.birDay, i.birMon, i.motherLeave, i.startDate, i.spec_mat, i.isTwin)]

#ditto for fathers
basic[birthReg, on = .(shnro = fatherID, vuosi = birYear), c("birDay", "birMon", "leave", "start", "spec_mat", "isTwin", "numBirth", "partnerID", "inc08") := .(i.birDay, i.birMon, i.fatherLeave, i.startDate, i.spec_mat, i.isTwin, i.numBirth, i.shnro_aiti, i.inc08)]

basic[birthReg, on = .(partnerID = shnro_aiti, birDay = birDay, birMon = birMon), "start2" := i.startDate]
rm(birthReg); gc()


#this will only work for fathers - mothers  will be wrong, but this does not matter for the purpose of this script
#kelaparleave making fatBirth into birYear, birMonth, birDay
kelaparleave[, birYear := as.numeric(substr(as.character(startA), 1, 4))]
kelaparleave[, birMon := as.numeric(substr(as.character(startA), 6,7))]
kelaparleave[, birDay := as.numeric(substr(as.character(startA), 9,10))]
#merge summed leave days
basic[kelaparleave[fatBirth == 1], on = .(shnro = shnro_saaja, vuosi = birYear), c("birDay", "birMon", "leave", "IRdate") := .(i.birDay, i.birMon, i.palkm, 1)]
basic[kelaparleave[is.na(fatBirth) == TRUE & birMon >= 10], on = .(shnro = shnro_saaja, vuosi = birYear), "bad" := 1]

kelaparleave[, birYear2 := (birYear - 1)]

basic[kelaparleave[is.na(fatBirth) == TRUE & birMon <= 9], on = .(shnro = shnro_saaja, vuosi = birYear2), "bad" := 1]

rm(kelaparleave);gc()

#father indicator
basic[is.na(birMon) == FALSE & sukup == 1, fatherInd := 1]

basic[fatherInd == 1 & is.na(leave) == TRUE, leave := 0]

#mother indicator
basic[is.na(birMon) == FALSE & sukup == 2, motherInd := 1]

#income
income <- readRDS("W:/user/income01.rds")
setDT(income)

basic[income, on = .(vuosi, shnro), c("wage", "entrep") := .(i.palk, i.yrtu)]

rm(income)
gc()

basic[is.na(entrep) == TRUE, entrep := 0]
basic[is.na(wage) == TRUE, wage := 0]

basic[, wage1 := shift(wage, 1), by = .(shnro)] 

basic[basic, on = .(vuosi = vuosi, partnerID = shnro), c("pWage", "pEdu", "pAge") := .(i.wage1, i.highEdu2, i.ika)]

#getting employment info
basic[employment, on = .(shnro = shnro, vuosi = vuosi), c("enterprise", "establ", "indust", "occup", "jobStart") := .(i.syrtun2, i.sykstun2, i.toimiala2, i.ammattikoodi, i.alkupvm2)]

#Use custom industCode function to reclassify industries
basic[, indust := industCode(indust)]
basic[, occup := substr(as.character(occup), 1,2)]
#create workplace ID variable from enterprise and establishment, then remove those without the correct info
basic[is.na(establ) == FALSE, workplaceID := establ]
basic[is.na(establ) == TRUE, workplaceID := enterprise]
basic <- basic[is.na(workplaceID) == FALSE]
gc()

#create workplace ID variable from enterprise and establishment, then remove those without the correct info
employment[is.na(sykstun2) == FALSE, wID := sykstun2]
employment[is.na(sykstun2) == TRUE, wID := syrtun2]
employment <- employment[is.na(wID) == FALSE]
gc()

#merge last employment observation to correct ID and workplace ID to create jobEnd variable
basic[employment[, .SD[.N], by = .(shnro, wID)], on = .(shnro = shnro, workplaceID = wID), "jobEnd" := i.loppupvm2]
rm(employment); gc()

#Remove industries with missing info
basic <- basic[indust != 0 & indust != 99]
gc()

#if entrep income greater than wage, likely the boss 
basic[entrep > wage, boss := 1]
#remove bosses
basic <- basic[is.na(boss) == TRUE]
basic[, entrep := NULL]
basic[, wage := NULL]
gc()

#number of employees in workplace
basic[, numEmp := .N, by = .(workplaceID, vuosi)]
basic[, empInd := fifelse(numEmp <= 250, 0, 1)]

basic[basic[fatherInd == 1 & vuosi %in% c(2008:2017), .SD[1], by = .(workplaceID)], on = .(workplaceID), firstYear := i.vuosi]
basic[basic[fatherInd == 1 & vuosi %in% c(2008:2017), .SD[.N], by = .(workplaceID)], on = .(workplaceID), lastYear := i.vuosi]
basic[, lastYear := (lastYear + 1)]

basic[basic[empInd == 1 & vuosi >= firstYear & vuosi <= lastYear, .SD[1], by = .(workplaceID)], on = .(workplaceID), removeWP := i.empInd]
gc()

basic[sukup == 1, numMen := .N, by = .(workplaceID, vuosi)]

workplace <- basic[sukup == 1, .SD[1], by = .(workplaceID, vuosi)]

#implement adaptive rolling
workplace[, B := 1:.N, by = .(workplaceID)]
workplace[B > 4, B := 4]
#rolling numEmp and numMen
#start as a sum as need to remove current year
workplace[, rollNum := frollsum(numEmp, B, na.rm = TRUE, adaptive = TRUE), by = .(workplaceID)]
workplace[, rollnMen := frollsum(numMen, B, na.rm = TRUE, adaptive = TRUE), by = .(workplaceID)]

#remove current year
workplace[, rollNum := (rollNum - numEmp)]
workplace[, rollnMen := (rollnMen - numMen)]

workplace[, rollNumMean := (rollNum/(B-1))]
workplace[, rollnMenMean := (rollnMen/(B-1))]

#merge back to basic
basic[workplace, on = .(workplaceID, vuosi), c("rollNumMean", "rollnMenMean") := .(i.rollNumMean, i.rollnMenMean)]

rm(workplace); gc()

basic[, rollSR := (rollnMenMean/rollNumMean)]

#subset to smaller workplaces
basic[, .N, by = removeWP]
basic <- basic[is.na(removeWP) == TRUE]
gc()

basic[, sumBad := sum(bad), by = .(workplaceID, vuosi)]
basic[, .N, by = .(sumBad)] #only a small number of workplaces affected
basic <- basic[is.na(sumBad) == TRUE]
gc()

basic[is.na(fatherInd) == TRUE, fatherInd := 0]
basic[, fatherInd2 := cumsum(fatherInd), by = .(shnro)]
basic[fatherInd2 > 0, .N]

#subset to fatherInd2 only
basic <- basic[fatherInd2 > 0 & ((vuosi > 2008 & vuosi < 2018) | inc08 == 1)]
gc()

#reform date - based on start of leave
basic[, reformDate := substr(as.character(start), 1, 7)]
basic[, reformDate2 := as.numeric(substr(as.character(start), 1, 4))]

basic[fatherInd == 1 & reformDate2 <= 2012, reform2 := "a.pre"]
basic[fatherInd == 1 & reformDate2 >= 2013, reform2 := "b.post"]

#within-reform child - based on leave
basic[fatherInd == 1 & (reformDate == "2012-12" | reformDate == "2013-01"), withReform := "a.during"]
basic[reform2 == "a.pre" & is.na(withReform) == TRUE, withReform := "b.before"]
basic[reform2 == "b.post" & is.na(withReform) == TRUE, withReform := "c.after"]


#properly order
basic[is.na(birMon) == FALSE & is.na(birDay) == FALSE, birDate := as.Date(paste(vuosi, birMon, birDay, sep = "-"))]
basic <- basic[order(birDate)]

#sum up number of men in place (to be able to exclude workplaces with only one father)
basic[fatherInd == 1, flag3 := .N, by = .(workplaceID)]

basic4 <- basic[flag3 > 1 & fatherInd == 1]
setDT(basic4)

#Shifting needs to account for possibility that last birth was also same ind.
basic4[, lagEnd := shift(jobEnd, 1), by = .(workplaceID)]
basic4[, lagID := shift(shnro, 1), by  = .(workplaceID)]
basic4[shnro == lagID, marker := 1]

while(basic4[marker == 1, .N] > 0){
  basic4[, marker2 := shift(marker, 1, type = "lead"), by = .(shnro, workplaceID)]
  print(basic4[marker == 1 | marker2 == 1, .N])
  basicX <- basic4[marker == 1 | marker2 == 1]
  basicX[, lagEnd2 := shift(lagEnd, 1), by = .(shnro, workplaceID)]
  basicX[, lagID2 := shift(lagID, 1), by = .(shnro, workplaceID)]
  
  basic4[basicX, on = .(shnro, workplaceID, vuosi), c("lagEnd", "lagID") := .(i.lagEnd2, i.lagID2)]
  rm(basicX); gc()
  basic4[marker == 1 & (lagID != shnro | is.na(lagID) == TRUE), marker := NA]
  basic4[, marker2 := NULL]
}

basic4[, "lagID" := NULL]

basic4[, jobStart := as.Date(jobStart)]
basic4[, lagEnd := as.Date(lagEnd)]

basic4[jobStart <= lagEnd | is.na(lagEnd) == TRUE, inJob := 1]
basic4[jobStart > lagEnd, inJob := 0]

#log transform the income
basic4[, income := log(wage1)]
basic4[, pWage := log(pWage)]

basic4[is.na(pWage), pWage := 0]

#firstbirth
basic4[numBirth == 1, firstBir := 1]
basic4[numBirth > 1, firstBir := 0]

saveRDS(basic4, "W:/user/peer_effects_data_longformat.rds")
