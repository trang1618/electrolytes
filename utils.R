read_d <- function(lyte) paste0(data_path, lyte, '_d.csv') %>% data.table::fread()
read_ori <- function(var){
  dat <- paste0(ori_data_path, var, '.csv') %>%
    data.table::fread(colClasses = c(visit_num = 'character')) %>%
    distinct()
  
  # fix issues with duplicates in the daily_labs tables
  if (grepl('_daily_labs', var)){
    lyte <- gsub('_daily_labs', '', var)
    cols <- paste(c('min', 'max'), lyte, 'val', sep = '_')
    dat <- dat %>%
      group_by(visit_num, day_in_unit) %>%
      summarise(!!(cols[1]) := min(!!(sym(cols[1]))),
                !!(cols[2]) := max(!!(sym(cols[2]))))
  }
  
  if ('day_in_unit' %in% colnames(dat)){
    dat <- dat %>%
      mutate(day_in_unit = gsub(' 00:00:00', '', day_in_unit))
  }
  
  dat
}

add_F <- function(df, var){
  # add FALSE to the remaining visits,
  # e.g. visits that are not in the table acetazolamide.csv
  # will have `on_acetazolamide = F`
  
  tibble(visit_num = setdiff(k_all_visits, df$visit_num)) %>%
    mutate((!!var) := FALSE) %>%
    rbind(df)
}

visits <- function(codename){# dat_name and col_name
  paste0(ori_data_path, codename[[1]], '.csv') %>%
    data.table::fread(colClasses = c('character')) %>%
    mutate(!!(codename[[2]]) := TRUE) %>%
    add_F(codename[[2]])
}

# code_names <- c(
#   'aki-codes', 'is_aki',
#   'ckd-codes', 'is_ckd',
#   'esrd-codes', 'is_esrd',
#   'dialysis-codes', 'is_dialysis',
#   'dialysis-proc-patients', 'given_dialysis',
#   'paralyzed-codes', 'is_paralyzed',
#   'rhabdo-codes', 'is_rhabdo',
#   'chf-codes', 'is_chf',
#   'cad-codes', 'is_cad',
#   'parathyroid-codes', 'is_parathyroid',
#   'sarcoid-codes', 'is_sarcoid',
#   'afib-codes', 'is_afib',
#   'tpn-patients', 'on_tpn',
#   'loop-diuretics', 'on_loop_diuretic',
#   'thiazides', 'on_thiazide',
#   'acetazolamide', 'on_acetazolamide',
#   'spironolactone', 'on_spironolactone') %>%
#   matrix(nrow = 2) %>%
#   t() %>%
#   as_tibble()
# 
# write_csv(code_names, path = 'codenames.csv')
