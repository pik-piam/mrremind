context('readREMIND-EU')

test_that(
  desc = 'Test readREMIND_EU() false subtype',
  code = expect_error(
    object = readREMIND_EU(subtype = NA),
    regexp = paste('^Invalid subtype -- supported subtypes are:'))
)

test_that(
  desc = 'Test readREMIND_EU() missing file',
  code = expect_error(
      object = readREMIND_EU(subtype = 'fixed_shares_FE_calibration_data'),
      regexp = paste('\'pm_fe_demand_REMIND-EU.csv\' does not exist in',
                     'current working directory'))
)
