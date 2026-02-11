library(DBI)
library(tidyverse)
library(BSOLutils)


# SQL connection to OF and data pull
sql_connection <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "MLCSU-BI-SQL",
                            Database = "EAT_Reporting_BSOL", Trusted_Connection = "True")


# For MI
sql1 <- "Select T1.*, T2.observation
  from [EAT_Reporting_BSOL].[OF].[OF2_Indicator_SQL_Data] T1
  LEFT JOIN [EAT_Reporting_BSOL].[OF].[OF2_Reference_Population] T2
ON T1.aggregation_id = T2.aggregation_id
AND T1.age_group_code = T2.age_code
AND T1.ethnicity_code = T2.ethnicity_code
AND T1.imd_code = T2.imd_code
AND T1.sex_code = T2.sex_code
WHERE T1.indicator_id = 51
and T1.denominator IS NULL
and aggregation_type = 'ICB (Resident)'
and start_date < convert(datetime, '20250401', 112)
and (age_group_code <15 OR age_group_code = '999')"

#Get data
MI <- dbGetQuery(sql_connection, sql1)




# Aggregate data
MI_agg <-
    MI %>%
    filter(start_date == max(MI$start_date)) %>% # select most recent date
    mutate(
           rt = numerator / denominator) %>%
    dplyr::select(numerator, denominator = observation, age_group_code
                  , sex_group_code = sex_code, imd_code, rt, start_date)

# run function

MI_out <- ISR_deprivation(MI_agg)


# Optional plot
ISR_deprivation_plot(MI_out)



# For Stroke:
sql2 <- "Select T1.*, T2.observation
  from [EAT_Reporting_BSOL].[OF].[OF2_Indicator_SQL_Data] T1
  LEFT JOIN [EAT_Reporting_BSOL].[OF].[OF2_Reference_Population] T2
ON T1.aggregation_id = T2.aggregation_id
AND T1.age_group_code = T2.age_code
AND T1.ethnicity_code = T2.ethnicity_code
AND T1.imd_code = T2.imd_code
AND T1.sex_code = T2.sex_code
WHERE T1.indicator_id = 50
and T1.denominator IS NULL
and aggregation_type = 'ICB (Resident)'
and start_date < convert(datetime, '20250401', 112)
and (age_group_code <15 OR age_group_code = '999')"

#Get data
Stroke <- dbGetQuery(sql_connection, sql2)




# Aggregate data
Stroke_agg <-
    Stroke %>%
    filter(start_date == max(MI$start_date)) %>% # select most recent date
    mutate(
        rt = numerator / denominator) %>%
    dplyr::select(numerator, denominator = observation, age_group_code
                  , sex_group_code = sex_code, imd_code, rt, start_date)

# run function

Stroke_out <- ISR_deprivation(Stroke_agg)


# Optional plot
ISR_deprivation_plot(Stroke_out)
