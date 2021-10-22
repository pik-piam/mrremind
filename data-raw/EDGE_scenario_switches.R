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
    
    # steel stock lifetime convergence ----
    tribble(
        ~scenario,   ~`EDGE-Industry_steel.stock.lifetime.base.scenario`,
        'SDP',       'SSP2',
        'SDP_EI',    'SSP2',
        'SDP_MC',    'SSP2',
        'SDP_RC',    'SSP2',
        'SSP1',      'SSP2',
        'SSP2',      'SSP2',
        'SSP2EU',    'SSP2',  
        'SSP3',      'SSP2',
        'SSP4',      'SSP4',
        'SSP5',      'SSP2') %>% 
        pivot_longer(-'scenario', names_to = 'switch'),
    
    tribble(
        ~scenario,   ~`EDGE-Industry_steel.stock.lifetime.convergence.year`,
        'SDP',       '2100',
        'SDP_EI',    '2100',
        'SDP_MC',    '2100',
        'SDP_RC',    '2100',
        'SSP1',      '2100',
        'SSP2',      '2100',
        'SSP2EU',    '2100',  
        'SSP3',      '2100',
        'SSP4',      '2010',
        'SSP5',      '2100') %>% 
        pivot_longer(-'scenario', names_to = 'switch'),

    tribble(
        ~scenario,   ~`EDGE-Industry_steel.stock.lifetime.convergence.factor`,
        'SDP',       '1.25',
        'SDP_EI',    '1.25',
        'SDP_MC',    '1.25',
        'SDP_RC',    '1.25',
        'SSP1',      '1.25',
        'SSP2',      '1',
        'SSP2EU',    '1',  
        'SSP3',      '1',
        'SSP4',      '1',
        'SSP5',      '0.75') %>% 
        pivot_longer(-'scenario', names_to = 'switch'),

    # secondary steel share limits ----
    # Linear convergence from the historic value in the year <from> to the value
    # <target> in the year <by>.
    # Corresponds to code in 
    # REMIND:/modules/37_industry/subsectors/datainput.gms
    tribble(
        ~scenario,   ~from,   ~by,   ~target,
        'SDP',       2015,    2050,   0.9,
        'SDP_EI',    2015,    2050,   0.9,
        'SDP_MC',    2015,    2050,   0.9,
        'SDP_RC',    2015,    2050,   0.9,
        'SSP1',      2015,    2050,   0.9,
        'SSP2',      2015,    2050,   0.9,
        'SSP2EU',    2015,    2050,   0.9,
        'SSP3',      2015,    2050,   0.9,
        'SSP4',      2015,    2050,   0.9,
        'SSP5',      2015,    2050,   0.9) %>% 
        pivot_longer(-'scenario', names_to = 'switch', 
                     values_transform = list(value = as.character)) %>% 
        mutate(switch = paste0('EDGE-Industry_secondary.steel.max.share.',
                               switch)),
    
    NULL) %>% 
    pivot_wider(names_from = 'switch')

usethis::use_data(EDGE_scenario_switches, internal = TRUE, overwrite = TRUE)
