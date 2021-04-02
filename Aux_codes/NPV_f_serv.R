
library(tidyverse)
library(sf)
library(tmap)
library(zoo)

# Converts year strings 'X2000' to integers
add_yearnum <- function(df) {
  df %>% mutate( yearnum = (Year %>% str_sub(2) %>% as.numeric())) %>% return()
}

# This is the period over which we'll calculate the NPV of CRVf 
# Absolute results are somewhat sensitive to changes in the 
# length of this period, but not by much. Specially insensitive
# when using a high drate (10%)
syear = 2015
eyear = 2050

# Intertemporal discount rate
#drate = 0.03
drate = 0.10

# Uses the outputs from other scripts


aggtab = read.csv("Output/CRV_scen.csv") %>% add_yearnum() %>%
  mutate(CRVl2 = CRVln) 

# SHP grid
#baseshp = st_read("Input_Data/GIS/worldCR_BR_Area.shp")
baseshp = st_read("Input_Data/GIS/BrazilCR.shp")


# States shapefile for plotting
#stateshp = st_read("Input_Data/GIS/EstadosBR_IBGE_LLWGS84.shp")
stateshp = st_read("Input_Data/GIS/bra_admbnda_adm1_ibge_2020.shp")


munic2cr = read.csv("Input_Data/GIS/munic_to_cr.csv") # This table is used to get the CR gridcells in each munic


lpindata = read.csv("Output/scenarios_land_price.csv")
cpindata = read.csv("Output/scenarios_agb_value.csv")


# This interpolates the variables described between years.
# GLOBIOM output is 5-yearly, so we need to interpolate
# CRV to get yearly values before discounting and summing for NPV.
# Sure there is a way to do this by just adjusting the 
# discounting equation, but interpolating was easier at
# the time.
interpolate_df <- function(df, syear, eyear) {
  newyearsnum = data.frame(yearnum = seq(syear,eyear))
  
  df %>% left_join(newyearsnum,., by = "yearnum") %>%
    mutate(CRVl2 = na.approx(CRVl2, maxgap = Inf, rule = 2),
           dEDD_total = na.approx(dEDD_total, maxgap = Inf, rule = 2),
           dEDD_ghg = na.approx(dEDD_ghg, maxgap = Inf, rule = 2),
           dEDD_luc = na.approx(dEDD_luc, maxgap = Inf, rule = 2),
           soyarea = na.approx(soyarea, maxgap = Inf, rule = 2),
           areasoyp = na.approx(areasoyp, maxgap = Inf, rule = 2),
           dyt = na.approx(dyt, maxgap = Inf, rule = 2),
           dyl = na.approx(dyl, maxgap = Inf, rule = 2),
           dyg = na.approx(dyg, maxgap = Inf, rule = 2),
           Y0 = na.approx(Y0, maxgap = Inf, rule = 2),
           Country = first(Country),
           area_km2 = first(area_km2)) %>%
    dplyr::select(-Year, -Country) %>%
    return()
}

# Nest and interpolate
aggtib <- aggtab %>% 
  nest(-ID,-Scenario) %>%
  mutate(data = map(data, ~interpolate_df(.,syear,eyear)))
# df = aggtib$data[[1]]

# Unnest
aggtib <-  aggtib %>% unnest(cols = c(data))

# Calculate present values (PVs) of 
# each year at drate. The CRVf variable in the 
# paper is CRVl2 here.
# Period years start at 0 in the first year (syear, originally 2015)
aggtib = aggtib %>% 
  mutate(yearperiod = yearnum - syear,
         CRVl2_PV = CRVl2/(1 + drate)^yearperiod) 

# Now sum PV's to get the Net Present Value (NPV)
# Also get time means of areas and area fractions
# for illustrative comparisons. 
npvaggtib = aggtib %>% 
  group_by(ID,Scenario) %>%
  summarise(CRVl2_NPV = sum(CRVl2_PV),
            soyarea = mean(soyarea, na.rm = T),
            Forarea = mean(Forarea, na.rm = T),
            area_km2 = mean(area_km2, na.rm = T),
            soyperc = 100*10*soyarea/area_km2,
            forperc = 100*Forarea) %>%
  ungroup() %>% drop_na

npvaggtib %>% summary


write.csv(npvaggtib,paste0("Output/CRVp",drate,".csv"))


