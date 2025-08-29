# Phenotyping the UKBB for early onset AF analyses

# requirements ====
library(data.table)
library(lubridate)
library(readxl)
library(yaml)
source("/home/rstudio-server/early_onset_af/resources/ukbb_extraction_utils.R")


# read extracted data ====
data_dir  <- "/mnt/project/early_onset_af_data"
demog     <- fread(file.path(data_dir, "data_participant.tsv"))
hesin     <- fread(file.path(data_dir, "data_hesin.tsv"))
diag      <- fread(file.path(data_dir, "data_hesin_diag.tsv"))
oper      <- fread(file.path(data_dir, "data_hesin_oper.tsv"))
gp        <- fread(file.path(data_dir, "data_gp_clinical.tsv"))


# rename data columns ====
config_fp <- "/home/rstudio-server/early_onset_af/resources/ukbb_extraction_config.yml"
config    <- read_yaml(config_fp)
demog     <- rename_ukbb_cols(demog, config$participant$columns)
hesin     <- rename_ukbb_cols(hesin, config$hesin$columns)
diag      <- rename_ukbb_cols(diag,  config$hes_diag$columns)
oper      <- rename_ukbb_cols(oper,  config$hes_oper$columns)
gp        <- rename_ukbb_cols(gp,    config$gp_clinical$columns)


# read the codes ==== 
codes_xlsx  <- "/home/rstudio-server/early_onset_af/resources/AF_Phenos_UKBB_Nick.xlsx"
code_sheets <- excel_sheets(codes_xlsx)
codes       <- rbindlist(lapply(setNames(code_sheets,code_sheets), function(sheet) {
  read_excel(codes_xlsx, sheet = sheet)
}), idcol = "pheno")


# clean the codes & add self-reported codes ==== 
codes <- codes[, .(pheno     = sub("_definition","", tolower(pheno)), 
                   code_type = tolower(Category), 
                   code      = sub("\\.", "", Code), 
                   desc      = Description)]

self_reported_codes <- list(
  list(name = "af",            code = "1471", code_type = "ukbb_self_reported_illness",   desc = "atrial fibrillation"),
  list(name = "cad",           code = "1074", code_type = "ukbb_self_reported_illness",   desc = "angina"),
  list(name = "cad",           code = "1075", code_type = "ukbb_self_reported_illness",   desc = "heart attack/myocardial infarction"),
  list(name = "cad",           code = "1070", code_type = "ukbb_self_reported_procedure", desc = "coronary angioplasty (ptca) +/- stent"),
  list(name = "cad",           code = "1095", code_type = "ukbb_self_reported_procedure", desc = "coronary artery bypass grafts (cabg)"),
  list(name = "heart_failure", code = "1076", code_type = "ukbb_self_reported_illness",   desc = "heart failure/pulmonary odema"),
  list(name = "ppm_incident",  code = "1548", code_type = "ukbb_self_reported_procedure", desc = "pacemaker insertion"),
  list(name = "icd_incident",  code = "1550", code_type = "ukbb_self_reported_procedure", desc = "defibrillator/icd insertion")
)

codes <- rbind(codes,
               data.table(pheno     = sapply(self_reported_codes, function(x) x$name),
                          code_type = sapply(self_reported_codes, function(x) x$code_type),
                          code      = sapply(self_reported_codes, function(x) x$code),
                          desc      = sapply(self_reported_codes, function(x) x$name)))


# ethnicity codes ====
ethnicity_codes <- list(
  white                 = 1,
  british               = 1001,
  white_black_caribbean =	2001,
  indian                = 3001,
  caribbean             = 4001,
  mixed                 = 2,
  irish                 =	1002,
  white_black_african	  = 2002,
  pakistani             = 3002,
  african	              = 4002,
  asian_or_asian_british=	3,
  any_other_white       =	1003,
  white_asian           =	2003,
  bangladeshi           =	3003,
  any_other_black       =	4003,
  black_or_black_british=	4,
  any_other_mixed       =	2004,
  any_other_asian       =	3004,
  chinese               =	5,
  other_ethnic_group    = 6)


# take wanted columns ==== 
cohort <- demog[, list(eid               = eid,
                       dob               = as.Date(paste(year_of_birth, month_of_birth, "01", sep = "-"), format = "%Y-%m-%d"),
                       assessment_date   = as.Date(assessment_date_1), 
                       assessment_age    = as.integer(assessment_age_1),
                       date_lost_fu      = as.Date(date_lost_fu),
                       reason_lost_fu    = factor(reason_lost_fu, levels = 1:5, labels = c("death_reported_by_relative", "nhs_indicate_lost", "emigrated", "emigrated", "withdrawal")),
                       sex               = factor(sex, levels = 0:1, labels = c("female", "male")),
                       ethnicity         = factor(ethnicity_1, levels = unlist(ethnicity_codes), labels = names(ethnicity_codes)),
                       ethnicity_group   = factor(sub("([0-9])00[0-9]", "\\1", ethnicity_1), levels = unlist(ethnicity_codes), labels = names(ethnicity_codes)),
                       genetic_sex       = factor(genetic_sex, levels = 0:1, labels = c("female", "male")),
                       genetic_ethnicity = factor(genetic_ethnicity, levels = 1, labels = c("caucasian")), 
                       smoking_status    = factor(smoking_status_1, levels = 0:2, labels = c("never", "previous", "current")),
                       alcohol_intake    = factor(alcohol_intake_1, levels = 1:6, labels = c("Daily or almost daily", "Three or four times a week", "Once or twice a week", "One to three times a month", "Special occasions only", "Never")))]


# check dob ====
stopifnot("Failed to parse some date of births" = all(!is.na(cohort$dob)))
stopifnot("some ages / dob indicate cohort age <37, is this right?" = all(cohort$dob <= as.Date("1972-12-31")))


# self reported illness codes ====
self_rep_code_regex <- "self_rep_ill_[0-9]+"
self_rep_year_regex <- "self_rep_ill_year_[0-9]+"
self_rep_code_cols <- grep(self_rep_code_regex, names(demog), value = TRUE)
self_rep_year_cols <- grep(self_rep_year_regex, names(demog), value = TRUE)
demog[, (self_rep_code_cols) := lapply(.SD, as.character), .SDcols = self_rep_code_cols]
demog[, (self_rep_year_cols) := lapply(.SD, as.numeric),   .SDcols = self_rep_year_cols]
self_rep_illness <- melt(demog,
                         id.vars = "eid",
                         measure = patterns(self_rep_code_regex, self_rep_year_regex),
                         variable.name = "element",
                         value.name = c("code", "year"),
                         na.rm = TRUE)
self_rep_illness <- self_rep_illness[year != -1 & year != -3] # unknown / prefer not to answer
self_rep_illness[, `:=`(date      = lubridate::ymd(paste0(as.character(floor(year)), "-01-01")) + lubridate::days(as.integer(365.25 * (year - floor(year)))),
                        year      = NULL,
                        element   = NULL,
                        code      = as.character(code),
                        code_type = "ukbb_self_reported_illness")]


# self reported procedure codes ====
self_rep_proc_code_regex <- "self_rep_proc_[0-9]+"
self_rep_proc_year_regex <- "self_rep_proc_year_[0-9]+"
self_rep_proc_code_cols <- grep(self_rep_proc_code_regex, names(demog), value = TRUE)
self_rep_proc_year_cols <- grep(self_rep_proc_year_regex, names(demog), value = TRUE)
demog[, (self_rep_proc_code_cols) := lapply(.SD, as.character), .SDcols = self_rep_proc_code_cols]
demog[, (self_rep_proc_year_cols) := lapply(.SD, as.numeric),   .SDcols = self_rep_proc_year_cols]
self_rep_oper <- data.table::melt(demog,
                                  id.vars = "eid",
                                  measure = patterns(self_rep_proc_code_regex, self_rep_proc_year_regex),
                                  variable.name = "element",
                                  value.name = c("code", "year"),
                                  na.rm = TRUE)
self_rep_oper <- self_rep_oper[year != -1 & year != -3] # unknown / prefer not to answer
self_rep_oper[, `:=`(date      = lubridate::ymd(paste0(as.character(floor(year)), "-01-01")) + lubridate::days(as.integer(365.25 * (year - floor(year)))),
                     year      = NULL,
                     element   = NULL,
                     code      = as.character(code),
                     code_type = "ukbb_self_reported_procedure")]


# join self reported diseases and procedures ====
self_rep_illness <- rbind(self_rep_illness, self_rep_oper)
self_rep_illness[, source := "ukbb"]


# check self report illness table ====
stopifnot("unable to parse dates for self-reported illness codes" = all(!is.na(self_rep_illness$date)))
stopifnot("are you sure something happened before 1900?" = all(self_rep_illness$date > as.Date("1900-01-01")))


# get the inpatient diagnosis codes ====
hesin[is.na(epistart) | epistart == "", epistart := admidate]
diag[hesin, date := as.Date(i.epistart), on = c("eid", "ins_index")]
diag[diag_icd9 == "", diag_icd9 := NA_character_]
diag[diag_icd10 == "", diag_icd10 := NA_character_]
diag <- melt(diag,
             id.vars = c("eid", "date"),
             measure.vars  = c("diag_icd9", "diag_icd10"),
             variable.name = "code_type",
             value.name = "code",
             na.rm = TRUE)
diag[, code_type := data.table::fcase(code_type == "diag_icd9", "icd9",
                                      code_type == "diag_icd10", "icd10")]
diag[, source := "hes"]


# get the inpatient procedure codes ====
oper[hesin, date := as.Date(i.epistart), on = c("eid", "ins_index")]
oper[oper3 == "", oper3 := NA_character_]
oper[oper4 == "", oper4 := NA_character_]
oper <- data.table::melt(oper,
                         id.vars = c("eid", "date"),
                         measure.vars  = c("oper3", "oper4"),
                         variable.name = "code_type",
                         value.name = "code",
                         na.rm = TRUE)
oper[, code_type := data.table::fcase(code_type == "oper3", "opcs3",
                                      code_type == "oper4", "opcs4")]
oper[, source := "hes"]


# get the GP diagnosis codes ====
gp[read_2 == "", read_2 := NA_character_]
gp[read_3 == "", read_3 := NA_character_]
gp <- melt(gp,
           id.vars = c("eid", "date"),
           measure.vars  = c("read_2", "read_3"),
           variable.name = "code_type",
           value.name = "code",
           na.rm = TRUE)
gp[, code_type := fcase(code_type == "read_2", "read2",
                        code_type == "read_3", "read3")]
gp[, source := "gp"]
gp[, date := as.Date(date)]


# death data ====
icd_cols  <- grep(paste0(c("^cause_of_death_primary_.*", "^cause_of_death_secondary_.*"), collapse = "|"), names(demog), value = TRUE)
date_cols <- grep("^date_of_death.*", names(demog), value = TRUE)
for (col in icd_cols) demog[is.na(col) | get(col) == "", (col) := NA_character_]
demog[, (icd_cols) := lapply(.SD, as.character), .SDcols = icd_cols]
demog[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]
death <- rbindlist(list(
  primary = melt(demog[, mget(c("eid", icd_cols, date_cols))],
                 id.vars = "eid",
                 measure = patterns("^date_of_death","^cause_of_death_primary"),
                 variable.name = "element",
                 value.name = c("date_of_death","code"), na.rm = TRUE
  ),
  secondary = melt(demog[, mget(c("eid", icd_cols, date_cols))],
                   id.vars = "eid",
                   measure = patterns("^date_of_death", "^cause_of_death_secondary"),
                   variable.name = "element",
                   value.name = c("date_of_death", "code"), na.rm = TRUE
  )),
  idcol = "position")
death <- death[, .(eid, code, date = date_of_death, code_type="icd10", source = "death")]


# bind together the diagnostic codes ====
combined <- rbindlist(list(death, self_rep_illness, diag, oper, gp), use.names=TRUE)


# check and remove date errors - in the future ====
cat(combined[date > Sys.Date(), .N], "diagnoses coded as in the future - removing\n")
combined <- combined[date < Sys.Date(), ] 


# add date of death & follow up end ====
max_data_date <- max(combined$date, na.rm=TRUE)
cohort[death[, .SD[which.min(date)], by="eid"], `:=`(death = TRUE, date_of_death = i.date), on="eid"][is.na(death), death := FALSE]
cohort[!is.na(date_of_death) & !is.na(date_lost_fu), date_lost_fu := date_of_death]
cohort[, date_last_fu := pmin(date_of_death, date_lost_fu, max_data_date, na.rm = T)]


# read in the codes and annotate
combined <- codes[combined, on = c("code", "code_type"), allow.cartesian = TRUE]
combined <- combined[!is.na(pheno)]
combined[, found_in := paste0(code_type, "[", source, "]")]
combined[, found_in := paste0(unique(found_in), collapse=";"), by="eid"]


# annotate first AF 
cohort[combined[combined[, .I[which.min(date)], by = c("eid", "pheno")]$V1][pheno == "af"], 
       c("af", "af_first_date", "af_found_in") := list(TRUE, as.Date(i.date), found_in), on = "eid"]
cohort[is.na(af), af := FALSE]


# add death to the cohort ====
phenos <- unique(codes$pheno)
for (g in phenos) {
  cohort[, paste0("death_", g) := FALSE]
  cohort[combined[source=="death" & pheno == g], paste0("death_", g) := TRUE, on = "eid"]
}


# run phenotyping of early onset AF ====
cohort[, age_first_af := interval(dob, af_first_date) / years(1)]
cohort[, early_onset_af := interval(dob, af_first_date) / years(1) <= 60]


# save cohort ====
setcolorder(cohort, grep("af", names(cohort), value=T), after = "date_last_fu")
fwrite(cohort, "/home/rstudio-server/cohort.tsv.gz", sep="\t")
system("dx upload /home/rstudio-server/cohort.tsv.gz --path /early_onset_af_data/cohort.tsv.gz")


# save outcomes ====
fwrite(combined, "/home/rstudio-server/cohort_outcomes.tsv.gz", sep="\t")
system("dx upload /home/rstudio-server/cohort_outcomes.tsv.gz --path /early_onset_af_data/cohort_outcomes.tsv.gz")






# fake dataset for testing ====
if (FALSE) {
  set.seed(123)
  N <- 100000
  cohort_fake <- data.table(
    eid = 1:N + 10000,
    dob = sample(seq(as.Date("1930-01-01"), as.Date("1975-12-31"), by="day"), N, replace=TRUE),
    assessment_date = sample(seq(as.Date("2006-01-01"), as.Date("2010-12-31"), by="day"), N, replace=TRUE),
    sex = sample(c("male","female"), N, replace=TRUE),
    ethnicity = sample(c("british","irish","other european","indian","chinese"), N, replace=TRUE, prob=c(0.6,0.05,0.1,0.15,0.1)),
    ethnicity_group = sample(c("white","asian","black","mixed"), N, replace=TRUE, prob=c(0.7,0.15,0.1,0.05)),
    genetic_sex = sample(c("male","female"), N, replace=TRUE),
    genetic_ethnicity = sample(c("caucasian","african","east_asian","south_asian","other"), N, replace=TRUE, prob=c(0.65,0.1,0.1,0.1,0.05))
  )
  cohort_fake[, smoking_status := sample(c("never","former","current"), N, replace=TRUE, prob=c(0.5,0.3,0.2))]
  cohort_fake[, alcohol_intake := sample(c("none","low","moderate","high"), N, replace=TRUE, prob=c(0.1,0.4,0.4,0.1))]
  cohort_fake[, assessment_age := as.integer(floor(interval(dob, assessment_date) / years(1)))]
  cohort_fake[, death := rbinom(.N, 1, 0.1) == 1]
  cohort_fake[, date_of_death := fifelse(death, assessment_date + sample(0:5000, .N, replace=TRUE), as.Date(NA))]
  cohort_fake[, date_last_fu := pmax(date_of_death, assessment_date + sample(0:5000, .N, replace=TRUE), na.rm=TRUE)]
  cohort_fake[, date_lost_fu := fifelse(runif(.N) < 0.05, assessment_date + sample(0:5000, .N, replace=TRUE), as.Date(NA))]
  cohort_fake[, reason_lost_fu := fifelse(!is.na(date_lost_fu), sample(c("moved","withdrawn"), .N, replace=TRUE), NA_character_)]
  
  cohort_fake[, af := rbinom(.N, 1, 0.05) == 1]
  cohort_fake[, af_first_date := fifelse(af, assessment_date - sample(0:5000, .N, replace=TRUE), as.Date(NA))]
  cohort_fake[, age_first_af := as.numeric(floor(interval(dob, af_first_date) / years(1)))]
  cohort_fake[is.na(af_first_date), age_first_af := NA_real_]
  cohort_fake[, early_onset_af := !is.na(age_first_af) & age_first_af < 60]
  cohort_fake[, af_found_in := sample(c("primary_care","hospital_records"), .N, replace=TRUE)]
  
  cohort_fake[, death := rbinom(.N, 1, 0.1) == 1]
  cohort_fake[, date_of_death := fifelse(death, assessment_date + sample(0:5000, .N, replace=TRUE), as.Date(NA))]
  cohort_fake[, date_last_fu := pmax(date_of_death, assessment_date + sample(0:5000, .N, replace=TRUE), na.rm=TRUE)]
  cohort_fake[, date_lost_fu := fifelse(runif(.N) < 0.05, assessment_date + sample(0:5000, .N, replace=TRUE), as.Date(NA))]
  cohort_fake[, reason_lost_fu := fifelse(!is.na(date_lost_fu), sample(c("moved","withdrawn"), .N, replace=TRUE), NA_character_)]
  
  # Cardiovascular deaths
  cv_cols <- c("death_af", "death_cad","death_heart_failure","death_ventricular_arrhythmia",
               "death_scd","death_ppm_incident","death_icd_incident")
  for (col in cv_cols) {
    cohort_fake[, (col) := rbinom(.N, 1, 0.05) == 1]
  }
  cohort_fake[1:3]
  fwrite(cohort_fake, "/home/rstudio-server/fake_cohort.tsv.gz", sep="\t")
  system("dx upload /home/rstudio-server/fake_cohort.tsv.gz --path /early_onset_af_data/fake_cohort.tsv.gz")
  
  
  
  set.seed(123)
  N <- 100000  # number of rows in fake combined dataset
  
  # Define some fake phenotypes, codes, and descriptions
  phenos <- c("af", "cad", "heart_failure", "ventricular_arrhythmia", "scd")
  code_types <- c("icd10", "read2", "read3")
  codes <- list(
    af = c("I480","I489"),
    cad = c("I252"),
    heart_failure = c("I500","I501"),
    ventricular_arrhythmia = c("I489"),
    scd = c("I462","I490")
  )
  descs <- list(
    af = c("Paroxysmal atrial fibrillation","Unspecified atrial fibrillation and atrial flutter"),
    cad = c("Old myocardial infarction"),
    heart_failure = c("Congestive heart failure","Left ventricular failure, unspecified"),
    ventricular_arrhythmia = c("Cardiac arrest, cause unspecified"),
    scd = c("Sudden cardiac death","Ventricular fibrillation")
  )
  sources <- c("death","hes","ukbb_self_reported_illness","ukbb_self_reported_procedure")
  
  # Generate fake combined table
  combined_fake <- data.table(
    pheno = sample(phenos, N, replace=TRUE),
    code_type = sample(code_types, N, replace=TRUE),
    eid = sample(1:N + 10000, N, replace=TRUE),
    date = sample(seq(as.Date("2006-01-01"), as.Date("2023-08-01"), by="day"), N, replace=TRUE),
    source = sample(sources, N, replace=TRUE)
  )
  
  # Assign code and description based on phenotype
  combined_fake[, code := sapply(pheno, function(x) sample(codes[[x]], 1))]
  combined_fake[, desc := sapply(pheno, function(x) sample(descs[[x]], 1))]
  
  # Generate a fake found_in column: combination of code_type[source]
  combined_fake[, found_in := paste0(code_type, "[", source, "]")]
  
  # Inspect
  head(combined_fake)
  
  fwrite(combined_fake, "/home/rstudio-server/fake_cohort_outcomes.tsv.gz", sep="\t")
  system("dx upload /home/rstudio-server/fake_cohort_outcomes.tsv.gz --path /early_onset_af_data/fake_cohort_outcomes.tsv.gz")
  
}
