# Phenotyping the UKBB for HERMES3 GWAS analyses

# requirements
library(data.table)

# data dir
data_dir <- file.path(Sys.getenv("DATA_DIR"), "ukbb_81499_20241114")

# read the data
withdraw  <- fread(file.path(data_dir, "withdraw81499_55_20230915.txt"))
demog     <- fread(file.path(data_dir, "data_participant.tsv"))
hesin     <- fread(file.path(data_dir, "data_hesin.tsv"))
diag      <- fread(file.path(data_dir, "data_hesin_diag.tsv"))
oper      <- fread(file.path(data_dir, "data_hesin_oper.tsv"))
self_oper <- fread(file.path(data_dir, "data_self_reported_procedures.tsv"))

# read the codes
codes <- fread(system.file("extdata", "hermes_3_codes", "hermes_3_codes_20250702.tsv", package = "heRmes"))
self_reported_codes <- list(
  list(name = "Heart Failure",                      code = "1076", code_type = "ukbb_self_reported_illness"),
  list(name = "Myocardial infarction",              code = "1075", code_type = "ukbb_self_reported_illness"),
  list(name = "Hypertrophic cardiomyopathy",        code = "1588", code_type = "ukbb_self_reported_illness"),
  list(name = "Coronary artery bypass grafting",    code = "1095", code_type = "ukbb_self_reported_procedure"),
  list(name = "Percutaneous coronary intervention", code = "1070", code_type = "ukbb_self_reported_procedure")
)
codes <- rbind(codes,
               data.table(Concept     = paste0(sapply(self_reported_codes, function(x) x$name), " Self Reported"),
                          Code        = sapply(self_reported_codes, function(x) x$code),
                          Source      = sapply(self_reported_codes, function(x) x$code_type),
                          Description = sapply(self_reported_codes, function(x) x$name)))
codes[, `:=`(code      = Code,
             code_type = fcase(Source=="ICD10", "icd10",
                               Source=="ICD9",  "icd9",
                               Source=="OPCS4", "opcs4",
                               Source=="ukbb_self_reported_illness", "ukbb_self_reported_illness",
                               Source=="ukbb_self_reported_procedure", "ukbb_self_reported_procedure"))]
codes <- codes[!is.na(code_type)]

# ethnicity codes
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


# long data of codes
cohort <- demog[, list(eid     = eid,
                       age     = as.integer(`21022-0.0`),
                       sex     = factor(`31-0.0`, levels = 0:1, labels = c("female", "male")),
                       ethnicity = factor(`21000-0.0`, levels = unlist(ethnicity_codes), labels = names(ethnicity_codes)),
                       ethnicity_group = factor(sub("([0-9])00[0-9]", "\\1", `21000-0.0`), levels = unlist(ethnicity_codes), labels = names(ethnicity_codes)),
                       genetic_sex = factor(`22001-0.0`, levels = 0:1, labels = c("female", "male")),
                       genetic_ethnicity = factor(`22006-0.0`, levels = 1, labels = c("caucasian")))]
cohort[demog, paste0("pc", 1:12) := mget(paste0("i.22009-0.", 1:12)), on = "eid"]


# check
stopifnot("Failed to parse some date of births" = all(!is.na(cohort$dob)))
stopifnot("some ages / dob indicate cohort age <37, is this right?" = all(cohort$dob <= as.Date("1972-01-01")))

# self reported illness codes
self_rep_code_regex <- "20002-[0-9]+\\.[0-9]+"
self_rep_year_regex <- "20008-[0-9]+\\.[0-9]+"
self_rep_code_cols <- grep(self_rep_code_regex, names(demog), value = TRUE)
self_rep_year_cols <- grep(self_rep_year_regex, names(demog), value = TRUE)
demog[, (self_rep_code_cols) := lapply(.SD, as.character), .SDcols = self_rep_code_cols]
demog[, (self_rep_year_cols) := lapply(.SD, as.numeric),   .SDcols = self_rep_year_cols]
self_rep_illness <- data.table::melt(demog,
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

# self reported procedure codes
self_rep_proc_code_regex <- "20004-[0-9]+\\.[0-9]+"
self_rep_proc_year_regex <- "20010-[0-9]+\\.[0-9]+"
self_rep_proc_code_cols <- grep(self_rep_proc_code_regex, names(self_oper), value = TRUE)
self_rep_proc_year_cols <- grep(self_rep_proc_year_regex, names(self_oper), value = TRUE)
self_oper[, (self_rep_proc_code_cols) := lapply(.SD, as.character), .SDcols = self_rep_proc_code_cols]
self_oper[, (self_rep_proc_year_cols) := lapply(.SD, as.numeric),   .SDcols = self_rep_proc_year_cols]
self_rep_oper <- data.table::melt(self_oper,
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

# join self reported diseases and procedures
self_rep_illness <- rbind(self_rep_illness, self_rep_oper)

# check self report illness table
stopifnot("unable to parse dates for self-reported illness codes" = all(!is.na(self_rep_illness$date)))
stopifnot("are you sure something happened before 1900?" = all(self_rep_illness$date > as.Date("1900-01-01")))

# get the inpatient diagnosis codes
hesin[is.na(epistart) | epistart == "", epistart := admidate]
diag[hesin, date := as.Date(i.epistart), on = c("eid(participant - eid)", "ins_index")]
diag[, eid := `eid(participant - eid)`]
diag[diag_icd9 == "", diag_icd9 := NA_character_]
diag[diag_icd10 == "", diag_icd10 := NA_character_]
diag <- data.table::melt(diag,
                         id.vars = c("eid", "date"),
                         measure.vars  = c("diag_icd9", "diag_icd10"),
                         variable.name = "code_type",
                         value.name = "code",
                         na.rm = TRUE)
diag[, code_type := data.table::fcase(code_type == "diag_icd9", "icd9",
                                      code_type == "diag_icd10", "icd10")]

# get the inpatient procedure codes
oper[hesin, date := as.Date(i.epistart), on = c("eid(participant - eid)", "ins_index")]
oper[, eid := `eid(participant - eid)`]
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

# bind together the diagnostic codes
combined <- rbind(self_rep_illness, diag, oper)

# read in the codes and annotate
combined <- codes[combined, on = c("code" = "code", "code_type" = "code_type"), allow.cartesian = TRUE]
combined <- combined[!is.na(Concept)]

# keep only unique, first occurance (in any coding system)
combined <- combined[combined[, .I[which.min(date)], by = c("eid", "Concept")]$V1]

# add to the cohort
concepts <- unique(codes$Concept)
for (g in concepts) {

  col_name <- tolower(gsub(" ", "_", gsub("[()]","",g)))
  cohort[combined[Concept == g], paste0(col_name, c("", "_first_date")) := list(TRUE, as.Date(i.date)), on = "eid"]
  cohort[is.na(base::get(col_name)), (col_name) := FALSE]

}

# add the withdrawal flags
cohort[withdraw, withdrawal := TRUE, on = c("eid" = "V1")]
cohort[is.na(withdrawal), withdrawal := FALSE]

# run phenotyping

# any ischaemic ICD codes
ischaemic_cols <- c("myocardial_infarction", "coronary_artery_bypass_grafting", "percutaneous_coronary_intervention", "thrombolysis_coronary", "ischaemic_cardiomyopathy")
cohort[, ischaemic := rowSums(.SD) > 0, .SDcols = ischaemic_cols]
cohort[, ischaemic_first_date := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = paste0(ischaemic_cols, "_first_date")]

# combined NICM ICD codes
nicm_cols <- c("dilated_cardiomyopathy", "dilated_cardiomyopathy_associated_with", "left_ventricular_systolic_dysfunction")
cohort[, nicm_comb := rowSums(.SD) > 0, .SDcols = nicm_cols]
cohort[, nicm_comb_first_date := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = paste0(nicm_cols, "_first_date")]

# any ischaemic self reported codes
self_isch_cols <- c("myocardial_infarction_self_reported", "coronary_artery_bypass_grafting_self_reported", "percutaneous_coronary_intervention_self_reported")
cohort[, self_isch := rowSums(.SD) > 0, .SDcols = self_isch_cols]
cohort[, self_isch_first_date := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = paste0(self_isch_cols, "_first_date")]

# any self reported HF codes
self_hf_col = c("heart_failure_self_reported")
cohort[, self_hf := rowSums(.SD) > 0, .SDcols = self_hf_col]
cohort[, self_hf_first_date := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = paste0(self_hf_col, "_first_date")]

# any self reported HCM codes
self_hcm_col = c("hypertrophic_cardiomyopathy_self_reported")
cohort[, self_hcm := rowSums(.SD) > 0, .SDcols = self_hcm_col]
cohort[, self_hcm_first_date := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = paste0(self_hcm_col, "_first_date")]

# HF exclusions
cohort[, hf_exclude := withdrawal == TRUE |
                       congenital_heart_disease==TRUE |  # all congenital heart disease
                       (heart_failure==FALSE & (self_hf==TRUE | ischaemic==TRUE | self_isch==TRUE)) | # non-HF but with ischaemic ICD history or self reported heart failure or ischaemic history
                       (heart_failure==TRUE  & (ischaemic==TRUE & ischaemic_first_date > heart_failure_first_date))] # HF but with first ischaemic event after the HF diagnosis
# pheno 1
cohort[, pheno1 := congenital_heart_disease==FALSE & heart_failure==TRUE]

# pheno 2
cohort[, pheno2 := hf_exclude==FALSE & heart_failure==TRUE & ischaemic==TRUE]

# pheno 3
cohort[, pheno3 := hf_exclude==FALSE & heart_failure==TRUE & ischaemic==FALSE]

# HF controls
cohort[, hf_control := hf_exclude==FALSE & pheno1==FALSE & pheno2==FALSE & pheno3==FALSE]



# DCM exclusions
cohort[, cm_exclude := withdrawal == TRUE |
                       congenital_heart_disease==TRUE |  # all congenital heart disease
                       hypertrophic_cardiomyopathy==TRUE | self_hcm==TRUE | # all HCM
                       restrictive_cardiomyopathy==TRUE] # all RCM

# pheno 4
cohort[, pheno4 := cm_exclude==FALSE &
                   !(dilated_cardiomyopathy==TRUE  & (ischaemic==TRUE & ischaemic_first_date <= dilated_cardiomyopathy_first_date)) & # DCM but with first ischaemic event prior to the DCM diagnosis
                   dilated_cardiomyopathy==TRUE]

# pheno 5
cohort[, pheno5 := cm_exclude==FALSE &
         !(nicm_comb==TRUE & (ischaemic==TRUE & ischaemic_first_date <= nicm_comb_first_date)) &  # NICM but with first ischaemic event prior to the NICM diagnosis
         nicm_comb==TRUE]

# CM controls
cohort[, cm_control := cm_exclude==FALSE &
                       pheno4==FALSE &
                       pheno5==FALSE &
                       ischaemic==FALSE &
                       self_isch==FALSE]


# check HF phenotyping
base_cols <- c("eid", "age", "sex", "ethnicity", "ethnicity_group","genetic_sex", "genetic_ethnicity", paste0("pc",1:12))
sum_cols <- names(cohort)[!names(cohort) %in% base_cols
                          &
                          !grepl("date", names(cohort))]
summary <- data.table (name = c("total", sum_cols), sex = "all", N = c(nrow(cohort), cohort[, .(sapply(.SD, sum)), .SDcols = sum_cols]$V1))
summary <- rbind(summary,
                 data.table(name = rep(sum_cols, 2), cohort[, .(N = sapply(.SD, sum)), .SDcols = sum_cols, by = "sex"]), fill=TRUE)
summary

# write summary
fwrite(dcast(summary, name ~ sex, value.var = "N"),
       file = file.path(Sys.getenv("DATA_DIR"), "ukbb_81499_20241114", "new_hermes3_phenotype_summary.tsv"),
       sep  = "\t")


cat("Total =", summary[name=="total",N], "; Sum (exc/crtl/1) = ", sum(summary[name%in%c("hf_exclude",paste0("pheno1"),"hf_control"), N]), "\n")
cat("Total =", summary[name=="total",N], "; Sum (exc/crtl/2/3) = ", sum(summary[name%in%c("hf_exclude",paste0("pheno",2:3),"hf_control"), N]), "\n")
cat("Total =", summary[name=="total",N], "; Sum (exc/crtl/5) = ", sum(summary[name%in%c("cm_exclude",paste0("pheno5"),"cm_control"), N]), "\n")

# write out
fwrite(cohort[, mget(c(base_cols[base_cols != "eid"],
                       paste0("pheno", 1:3), "hf_exclude", "hf_control",
                       paste0("pheno", 4:5), "cm_exclude", "cm_control"))],
       file = file.path(Sys.getenv("DATA_DIR"), "ukbb_81499_20241114", "hermes3_phenotypes.tsv.gz"),
       sep  = "\t")

# random test samples
set.seed(123)
cohort[hf_exclude==T][sample.int(.N, 3), eid] # 5607008-Y 3463412-Y 3399308-Y
set.seed(123)
cohort[hf_control==T][sample.int(.N, 3), eid] # 5617489-Y 1398510-Y 3387157-Y
set.seed(123)
cohort[pheno1==T][sample.int(.N, 3), eid] # 5087228-Y 3558314-Y 5669956-Y
set.seed(123)
cohort[pheno2==T][sample.int(.N, 3), eid] # 4964821-Y 5145334-Y 4110018-Y
set.seed(123)
cohort[pheno3==T][sample.int(.N, 3), eid] # 1535528-Y 1660078-Y 4720288-Y
set.seed(123)
cohort[pheno4==T][sample.int(.N, 3), eid] # 4547830-Y 5543541-Y 4762107-Y
set.seed(123)
cohort[pheno5==T][sample.int(.N, 3), eid] # 2469121-Y 2659022-Y 1340307-Y
set.seed(123)
cohort[cm_control==T][sample.int(.N, 3), eid] # 5335780-Y 1276673-Y 3304835-Y
set.seed(123)
cohort[cm_exclude==T][sample.int(.N, 3), eid] # 2946309-Y 4951293-Y 4815931-Y



