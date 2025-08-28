#' @title filter_ukbb_data_dict
#'
#' @param dict_path, str, path to the dataset.data_dictionary.csv
#' @param entity_name, str, entity name
#' @param columns_list, list, list of lists representing UKBB column name and search strategy list(name=, search=).
#'   name must be a valid column name in the data_dictionary and search either "matches"
#'   for exact matches, or starts with to match cases of multiple instances (repeated measures usually)
#'
#' @returns a filtered subset of the data_dictionary
#'
filter_ukbb_data_dict <- function(dict_path, entity_name, columns_list) {

    data_dict <- fread(dict_path)

    d <- lapply(columns_list, function(x) {

        d0 <- data.table()
        if (x$search=="matches") {
            d0 <- data_dict[entity==entity_name & name==x$name]
        } else if (x$search=="startswith") {
            d0 <- data_dict[entity==entity_name & grepl(paste0("^", x$name), name)]
        }

        if (nrow(d0)==0) {
            cat(glue("Code [{x$name}] not found in data dictionary\n"))
            stop("Code not found error")
        }

        d0

    }) |> rbindlist(idcol = "item")

    return(d)
}



#' @title extract_ukbb_data
#'
#' @param dataset, str, a valid dataset id - format "{projectid}:{recordid}"
#' @param fields, str, vector of UK-BB format column names e.g. p31
#' @param entity, str, string of length one - the entity to extract from e.g. participants
#' @param output, str, output path to save to, the extension will be used to determine the output format (options .csv or .tsv)
#' @param header_style, str, "FIELD-NAME"(default),"FIELD-TITLE","NONE", or "UKB-FORMAT"
#' @param coding_option, str, "RAW"(default),"REPLACE","EXCLUDE"
#' @param verbose, logical, whether to print output
#'
#' @returns NULL side effect is starting a table-exporter job which outputs the file to /hermes3_data directory in the RAP
#'
#' @importFrom glue glue
#' @importFrom tools file_ext file_path_sans_ext
#'
extract_ukbb_data <- function(dataset, fields, entity, output, header_style="FIELD-NAME", coding_option="RAW", verbose=TRUE) {

  # check options
  header_style <- match.arg(header_style, choices = c("FIELD-NAME","FIELD-TITLE","NONE","UKB-FORMAT"))
  coding_option <- match.arg(coding_option, choices = c("RAW","REPLACE","EXCLUDE"))

  # flag is gzip requested
  if (grepl("\\.gz$", output)) {
    warning("Compressed file output not available, removing .gz")
    output <- sub("\\.gz", "", output)
  }

  # validate the extension
  if (tools::file_ext(output) == "csv") {
    output_format <- "CSV"
  } else if (tools::file_ext(output) == "tsv") {
    output_format <- "TSV"
  } else {
    stop("Invalid file extension. Only .csv or .tsv are allowed.")
  }

  # extract base name (without extension) and directory
  output_name <- tools::file_path_sans_ext(basename(output))
  output_dir <- dirname(output)

  # string for the fields
  field_str <- paste0('-ifield_names="', fields, '"', collapse=" ")

  # command to execute
  cmd <- glue::glue(
    "dx run table-exporter ",
    "-idataset_or_cohort_or_dashboard={dataset} ",
    "-ioutput={output_name} ",
    "-ioutput_format={output_format} ",
    "-iheader_style={header_style} ",
    "-icoding_option={coding_option} ",
    "{field_str} ",
    "-ientity={entity} ",
    "--destination {output_dir}/"
  )

  # run
  o <- system(cmd, intern = TRUE)

  # print output
  if (verbose) {
    cat(o, sep = "\n")
  }
}




#' @title rename_ukbb_cols
#'
#' @param data, data.table
#' @param table_config, list, table config from the ukbb_extract_config.yml file. list(std_name: list(name: ..., search: ...), ...)
#'
#' @returns data.table
#'
#' @import data.table
#'
rename_ukbb_cols <- function(data, col_config) {
    
    for (i in seq_along(col_config)) {
        
        ukb_name <- col_config[[i]][["name"]]
        std_name <- names(col_config)[i]
        strategy <- col_config[[i]][["search"]]
        
        if (strategy=="matches") {
            
            data.table::setnames(data, ukb_name, std_name)
            
        } else if (strategy=="startswith") {
            
            regex     <- paste0("^", ukb_name)
            matches   <- names(data)[grepl(regex, names(data))]
            new_names <- paste0(std_name, "_", 1:length(matches))           
            data.table::setnames(data, matches, new_names)
            
        }
    }
    return(data)
}