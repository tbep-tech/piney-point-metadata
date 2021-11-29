library(tidyverse)
library(sf)
library(here)

# kbrdat to pts and dat, save to csv --------------------------------------

data(kbrdat)

kbrpts <- kbrdat %>% 
  select(station) %>% 
  unique %>% 
  mutate(
    lng = st_coordinates(.)[, 1], 
    lat = st_coordinates(.)[, 2]
  ) %>% 
  st_set_geometry(NULL)

kbrdat <- kbrdat %>% 
  st_set_geometry(NULL) %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  mutate_if(function(x) inherits(x, 'Date'), as.character)

write.csv(kbrdat, here('data-raw/kbrdat.csv'), row.names = F)
write.csv(kbrpts, here('data-raw/kbrpts.csv'), row.names = F)

# all others save to csv --------------------------------------------------

fls <- c('rswqdat', 'rsstatloc', 'rstrndat', 'rstrnpts', 'rsphydat', 'rsphypts')
names(fls) <- c('wqdat', 'wqpts', 'trndat', 'trnpts', 'phydat', 'phypts')

for(i in seq_along(fls)){
  
  fl <- fls[[i]]
  flnm <- names(fls[i])
  
  cat(fl, '\n')
  
  rdt <- paste0('data/', fl, '.RData') %>% 
    here
  load(file = rdt)
  
  dat <- get(fl)
  
  # make lat/lon separate columns
  if(grepl('pts$', flnm))
    dat <- dat %>% 
      mutate(
        lng = st_coordinates(.)[, 1], 
        lat = st_coordinates(.)[, 2]
      ) %>% 
      st_set_geometry(NULL) %>% 
    select(-matches('col'))

  # filter dates < Oct 1, make date as character
  if(!grepl('pts$', flnm))
    dat <- dat %>% 
      filter(date < as.Date('2021-10-01')) %>% 
      mutate_if(function(x) inherits(x, 'Date'), as.character)
  
  flout <- paste0('data-raw/', flnm, '.csv') %>% 
    here
  
  write.csv(dat, flout, row.names = F)
  
}
