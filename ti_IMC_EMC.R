

################################################################################
#
# Type: plugin.
#
# Name: ti_IMC_EMC.
#
# Description: R script to calculate the Import Market Concentration index (IMC)
#              and the Export Market Concentration index (EMC) starting from 
#              elements 5610 (Import Quantity), 5910 (Export Quantity), 5622
#              (Import Value), and 5922 (Export Value).
#              The following is a brief description of the indicators.
#              IMC: measures, for each product, the level of import market 
#                   concentration by country of destination. It allows assessing
#                   whether a large portion of commodity imports is acquired by
#                   a small number of countries or, conversely, if imports are 
#                   evenly distributed among many countries.  
#              EMC: measures, for each product, the level of export market
#                   concentration by country of origin. It allows assessing
#                   whether a large portion of commodity exports is accounted 
#                   for by a small number of countries or, conversely, if 
#                   exports are evenly distributed among many countries.
#
# Author's email: riccardo.giubilei@fao.org
#
################################################################################



# Plugin initialization ---------------------------------------------------

# Plugin name
plugin_name <- 'ti_IMC_EMC'

# Print starting message
message(paste("The", plugin_name, "plugin is running."))



# Packages ----------------------------------------------------------------

# Print packages-loading message
message(paste("The", plugin_name, "plugin is loading packages."))

# Load packages
suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(data.table)
}) 


# Environment -------------------------------------------------------------

if (faosws::CheckDebug()) {
  
  # Connect to .yml file
  library(faoswsModules)
  SETT <- faoswsModules::ReadSettings("sws_trade_indicators.yml")
  
  # Set up offline environment
  server <- SETT[["server"]]
  token <- SETT[["token"]]
  faosws::GetTestEnvironment(baseUrl = server,
                             token = token)
  
  # Load helper functions
  source('plugin_helper_functions.R')
  
}


# Import datatables -------------------------------------------------------

# Print datatable-importing message
message(paste("The", plugin_name, "plugin is importing datatables."))


### Key commodities datatable ###

# Get item codes from key commodities datatable
cpc_codes <- faosws::ReadDatatable(table = "key_commodities_codes")$commodity_code


### Reporter countries ###

# Import reporter countries datatable
reporters_by_year <- faosws::ReadDatatable(table = "reporters_by_year_new_version")

# Get GeographicArea codes from reporter countries
m49_codes <- unique(reporters_by_year$m49)

# Get years from reporter countries
reporters_cols <- colnames(reporters_by_year)
years_cols <- grep('year_', reporters_cols, value = TRUE)
years_reporters <- sub('year_', '', years_cols)

# Exclude years for which there are no observations
na_number <- colSums(is.na(reporters_by_year))
empty_cols <- reporters_cols[na_number == nrow(reporters_by_year)]
NA_years_reporters <- sub('year_', '', empty_cols)
years_reporters <- setdiff(years_reporters,
                           NA_years_reporters)
# NB: this is not strictly necessary as countryReports() already excludes these 
#     years, but it allows to download the minimal set of data that is required.

# NB: years can be possibly further refined using input parameters.


### Country thresholds ###

# Import thresholds datatable
country_threshold <- faosws::ReadDatatable(table = "trade_outlier_country_thresholds")

# Change area name from datatable
colnames(country_threshold)[1] <- 'geographicAreaM49'

# Divide thresholds by 1000 because we are working with thousands
country_threshold[, threshold := threshold / 1000]  

# Fill in NA values with 1 (thousand)
country_threshold[is.na(threshold), 'threshold'] <- 1L



# Input parameters --------------------------------------------------------

# Print input parameters message
message(paste("The", plugin_name, "plugin is reading the input parameters."))

### Input domain ###

# Retrieve parameters' values
trade_dataset <- swsContext.computationParams$trade_source


### Years ###

# Retrieve parameters' values
starting_year <- swsContext.computationParams$starting_year
ending_year <- swsContext.computationParams$ending_year

# If NULL, replace them with years from reporter countries datatable
if (is.null(starting_year)) starting_year <- min(years_reporters)
if (is.null(ending_year)) ending_year <- max(years_reporters)

# Check that the starting year does not come after the ending year
stopifnot("The starting year cannot come after the ending year." = 
            starting_year <= ending_year)

# Compute input year-range 
input_years <- starting_year:ending_year

# Final set of years as intersection between reporters and user interface input
selected_years <- intersect(years_reporters,
                            input_years)


### Query-only parameter ###

# Retrieve parameter's value
query_only <- swsContext.computationParams$query_only

# If query_only is 'yes', retrieve input keys from the query
if (query_only == 'yes') {
  
  # Input keys
  query_geo <- query_keys('geographicAreaM49')
  query_elem <- query_keys('measuredElementTrade')
  query_item <- query_keys('measuredItemCPC')
  query_years <- query_keys('timePointYears')
  # N.B.: query_elem is only used to choose which results to return; the others
  #       are used to get input data, and will override the three corresponding
  #       vectors of keys.
  
  # Replace m49_codes with the query_geo that are in m49_codes (reporter 
  # countries), if any; otherwise, throw warning and use the original m49_codes
  if (any(query_geo %in% m49_codes)) {
    m49_codes <- query_geo[query_geo %in% m49_codes]
  } else {
    warning('The queried key(s) for geographicAreaM49 is not present among the reporter countries. All the reporter countries will be considered instead.')
  }
  
  # Replace selected_years with the query_years that are in selected_years, if
  # any; otherwise, throw warning and use the original selected_years
  if (any(query_years %in% selected_years)) {
    selected_years <- query_years[query_years %in% selected_years]
  } else {
    warning('The queried key(s) for timePointsYears is not included in the years specified when running the plugin. The latter are used instead.')
  }
  
}



# Import data -------------------------------------------------------------

# Print input data-importing message
message(paste("The", plugin_name, "plugin is importing input data."))

# Trade dataset: Import Quantity (5610), Export Quantity (5910), Import Value 
#                (5622), and Export Value (5922)
data_market <- sws2dataset(dataset = trade_dataset,
                           keys = list(geo = m49_codes,
                                       elem = c('5610', '5910',
                                                '5622', '5922'),   
                                       item = cpc_codes,
                                       years = selected_years),
                           elem_colnames = c('import_q', 'export_q',
                                             'import_v', 'export_v'))



# IMC and EMC indicators --------------------------------------------------

# Print indicator-calculation message
message(paste("The", plugin_name, "plugin is computing the indicators."))

# Filter out years for which countries do not report
is.reporter <- countryReports(countries = data_market$geographicAreaM49,
                              years = data_market$timePointYears)
data_market <- data_market[is.reporter]

# Merge to add thresholds to the dataset
data_imcemc <- merge(data_market,
                     country_threshold,
                     by = 'geographicAreaM49')

# Create two separate datasets (imports and exports), and:
# 1. Remove cases where the quantity is below the country's threshold;
# 2. Remove cases where the value is NA (would be ignored anyway in the sum)
data_imc <- data_imcemc[import_q > threshold & 
                          sapply(import_v, function (i) isFALSE(is.na(i))),
                        .(geographicAreaM49, measuredItemCPC, timePointYears,
                          import_q, import_v, threshold)]
data_emc <- data_imcemc[export_q > threshold & 
                          sapply(export_v, function (i) isFALSE(is.na(i))), 
                        .(geographicAreaM49, measuredItemCPC, timePointYears,
                          export_q, export_v, threshold)]


## Import market concentration index ##

# World imports calculation
data_imc[, world_import := sum(import_v),
         by = .(measuredItemCPC, timePointYears)]

# IMC calculation
data_imc[, imc := (sqrt(sum(((import_v / world_import) ^ 2))) -
                     sqrt(1 / .N)) / (1 - sqrt(1 / .N)), 
         by = .(measuredItemCPC, timePointYears)]
# N.B.: .N is the total number of importing countries.

# Replace NaNs with 1
data_imc[is.nan(imc), "imc"] <- 1


## Export market concentration index ##

# World exports calculation
data_emc[, world_export := sum(export_v),
         by = .(measuredItemCPC, timePointYears)]

# EMC calculation
data_emc[, emc := (sqrt(sum(((export_v / world_export) ^ 2))) - 
                     sqrt(1 / .N)) / (1 - sqrt(1 / .N)), 
         by = .(measuredItemCPC, timePointYears)]
# N.B.: .N is the total number of exporting countries.

# Replace NaNs with 1
data_emc[is.nan(emc), "emc"] <- 1



# Save back the results and finalize --------------------------------------

# Print save message
message(paste("The", plugin_name, "plugin is saving the results back to the session."))

# If query_elem does not exist, initialize it
if (isFALSE(exists('query_elem'))) query_elem <- '0'

# Throw warning if none of the indicators is included in the queried elements
select_elem <- intersect(c('507', '508'),
                         query_elem)
nelem <- length(select_elem)
if (nelem == 0)
  warning('The indicators are not included in the queried key(s) for measuredElementTrade. Both of them will be returned.')

# Check which indicator(s) should be computed
compute_imc <- if ('507' %in% select_elem | nelem == 0) TRUE else FALSE
compute_emc <- if ('508' %in% select_elem | nelem == 0) TRUE else FALSE

# IMC
if (isTRUE(compute_imc)) {
  
  # Save the results back to the session
  imc_save <- 
    faosws::SaveData(domain = 'trade',
                     dataset = 'trade_indicators',
                     data = data_imc[, .(geographicAreaM49 = geographicAreaM49,
                                         measuredItemCPC = measuredItemCPC,
                                         timePointYears = timePointYears,
                                         Value = imc,
                                         measuredElementTrade = '507',
                                         flagObservationStatus = fifelse(is.na(get('imc')),
                                                                         NA_character_,
                                                                         'E'),
                                         flagMethod = fifelse(is.na(get('imc')),
                                                              NA_character_,
                                                              'i'))],
                     waitTimeout = 100000)
  
  # Log
  base_string <- paste("Log for the IMC indicator:",
                       "%d written observations, %d appended, %d ignored, %d discarded.")
  message(do.call(sprintf, c(base_string,
                             lapply(imc_save[1:4], FUN = identity))))
  
}

# EMC
if (isTRUE(compute_emc)) {
  
  # Save the results back to the session
  emc_save <- 
    faosws::SaveData(domain = 'trade',
                     dataset = 'trade_indicators',
                     data = data_emc[, .(geographicAreaM49 = geographicAreaM49,
                                            measuredItemCPC = measuredItemCPC,
                                            timePointYears = timePointYears,
                                            Value = emc,
                                            measuredElementTrade = '508',
                                            flagObservationStatus = fifelse(is.na(get('emc')),
                                                                            NA_character_,
                                                                            'E'),
                                            flagMethod = fifelse(is.na(get('emc')),
                                                                 NA_character_,
                                                                 'i'))],
                     waitTimeout = 100000)
  
  # Log
  base_string <- paste("Log for the EMC indicator:",
                       "%d written observations, %d appended, %d ignored, %d discarded.")
  message(do.call(sprintf, c(base_string,
                             lapply(emc_save[1:4], FUN = identity))))
  
}

# Print information
#print(sessionInfo())
#print(version)

# Print ending message 
message(paste("Process completed successfully!"))

