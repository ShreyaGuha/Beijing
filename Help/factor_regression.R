library( data.table)

data_daily <- data.table( date = seq.Date( from = as.Date( '1999-01-01'),
                                           to = as.Date( '1999-12-31'),
                                           by = 'day'),
                          pm25 = sample( seq( 0, 5, .1), 365, replace = T),
                          dow = 1:7)

# intercept--factors interpretable relative to base day (e.g., day 1)
pm_lm1 <- lm( pm25 ~ as.factor( dow), data = data_daily)

# no intercept--factors interpretable as absolute
pm_lm2 <- lm( pm25 ~ -1 + as.factor( dow), data = data_daily)
summary( pm_lm)
