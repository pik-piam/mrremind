library(tidyverse)

EDGE_scenario_switches <- bind_rows(
    tribble(
        ~scenario,   ~`EDGE-Industry_steel.stock.estimate`,
        'SDP',       'low',
        'SDP_EI',    'low',
        'SDP_MC',    'low',
        'SDP_RC',    'low',
        'SSP1',      'low',
        'SSP2',      'med',
        'SSP2EU',    'med',  
        'SSP3',      'med',
        'SSP4',      'med',
        'SSP5',      'high') %>% 
        pivot_longer(-'scenario', names_to = 'switch'),
    
    tribble(
        ~scenario,   ~`EDGE-Industry_scenario.mask.OECD`,
        'SSP4',      'SSP2') %>% 
        pivot_longer(-'scenario', names_to = 'switch'),
    
    tribble(
        ~scenario,   ~`EDGE-Industry_scenario.mask.non-OECD`,
        'SSP4',      'SSP1') %>% 
        pivot_longer(-'scenario', names_to = 'switch'),
    
    NULL) %>% 
    pivot_wider(names_from = 'switch')

usethis::use_data(EDGE_scenario_switches, internal = TRUE, overwrite = TRUE)

