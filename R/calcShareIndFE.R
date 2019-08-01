
calcShareIndFE <- function() {
  
  x <- readSource("REMIND_11Regi",subtype="shareIndFE")
  
  w <- calcOutput("IO",subtype="output",aggregate=FALSE)
  w <- w[,2010,intersect(getNames(x, dim = 1),getNames(w, dim=2))] 
  w <- dimSums(w,dim=c(3.1,3.3))
  
  x <- x[,,intersect(getNames(x, dim = 1),getNames(w))] #removing feh2i due to missing weight
  
  return(list(x           = x,
              weight      = w,
              unit        = "ratio",
              description = "share of industry sub-sectors in FE use"))
}

# Extension notes MJP 2019-07-25 
# # Add shares for H2, heat, and electricity, since they are needed for the new
# # emissions accounting by Renato.
# bind_rows(
#   # Read electricity shares from 2010 IEA data
#   calcOutput('IO', subtype = 'output_Industry_subsectors') %>%
#     as.data.frame() %>%
#     as_tibble() %>%
#     filter(2010 == Year,
#            grepl('^feel[^_]*_.*', Data2)) %>%
#     select(region = Region, pf = Data2, value = Value) %>%
#     character.data.frame() %>%
#     separate(pf, c('type', 'sector'), sep = '_') %>%
#     mutate(type = sub('feel.*$', 'feeli', type)) %>%
#     group_by(region, sector, type) %>%
#     summarise(value = sum(value)) %>%
#     group_by(region, type) %>%
#     mutate(share = value / sum(value)) %>%
#     ungroup() %>%
#     arrange(region, type) %>%
#     select(-value) %>%
#     filter('otherInd' != sector) %>%
#     # put 100 % of heat into otherInd
#     complete(nesting(region, sector), type = c('feeli', 'fehei')) %>%
#     mutate(share = round(ifelse('fehei' == type, 0, share), 2)) %>%
#     spread(sector, share),
# 
#   # use gas shares for H2
#   read_delim(
#     file = '~/PIK/swap/inputdata/sources/REMIND_11Regi/shareIndustyFE.csv',
#     delim = ';',
#     col_types = 'ccddd',
#     skip = 3) %>%
#     filter('fegai' == type) %>%
#     mutate(type = 'feh2i')
# ) %>%
#   write_delim(
#     path = '~/PIK/swap/inputdata/sources/REMIND_11Regi/shareIndustyFE.csv',
#     delim = ';',
#     append = TRUE,
#     col_names = FALSE)
