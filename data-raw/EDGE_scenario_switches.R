library(tidyverse)

EDGE_scenario_switches <- tribble(
    ~scenario,       ~`EDGE-Industry_steel.stock.estimate`,
    'SDP',           'low',
    'SDP_EI',        'low',
    'SDP_MC',        'low',
    'SDP_RC',        'low',
    'SSP1',          'low',
    'SSP2',          'med',
    'SSP2Ariadne',   'med',  
    'SSP3',          'med',
    'SSP4',          'med',
    'SSP5',          'high')

usethis::use_data(EDGE_scenario_switches, internal = TRUE)

