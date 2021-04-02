################################################
#   Estimate lost soy revenue for historical 
#   and future analysis of soy losses due to 
#   exposure to extreme heat
#
################################################


Packages <- c("dplyr","tidyverse","RColorBrewer","stringr",
              "ggthemes","ggplot2","ggpubr","formatR","ggExtra",
              "gdata","readxl","ggridges","Rcpp","data.table",
              "sf","ncdf4","raster","fasterize","tmap",
              "ggspatial","classInt","data.table","exactextractr",
              "rasterVis","rgdal","viridis","reticulate")
lapply(Packages, library, character.only = TRUE)


#==============================================
#   Input Data
#==============================================

in.dir <- "Input_Data/"
out.dir <- "Output/"
deltaEDD.folder <- "Output/DEDD/"


# Gridcells
CRshp <- st_read(paste0(in.dir,"GIS/BrazilCR.shp"))
CRcsv <- read.csv(paste0(in.dir,"GIS/BrazilCR.csv"))
mask <- readOGR(paste0(in.dir,"GIS/bra_admbnda_adm0_ibge_2020.shp"))
biomes <- read.csv(paste0(in.dir,"GIS/CR_biome.csv")) %>% 
  dplyr::select(-X,-Bioma)

# Reference grid
nctemplate <- raster(paste0(in.dir,"Xavier/refgrid_xavier.nc"), 
                     level = 1, varname = "tmax")


## Deforestation and agricultural area data (2000-2012)
# Forest area from Mapbiomas (m^2)
mapbiomas <- read.csv("Input_Data/MAPBIOMAS/reduced_mapbiomas_6.csv") %>%
  dplyr::select(-nid,-perimeter,-.geo,-system.index,-area,-areatot) %>%
  pivot_longer(area1985:newcroparea2019,names_to = "Parameter",values_to = "val") %>%
  mutate(Par = substr(Parameter,1,nchar(Parameter)-4),
         Year = paste0("X",substr(Parameter,nchar(Parameter)-3,nchar(Parameter)))) %>%
  dplyr::select(-Parameter)


# Cropland area for soy, from Dias et al (2016) (ha)
areasoy <- brick(paste0(in.dir,"Dias/LUSOYBEAN20002012.nc"),var="landuse")
Y0 <- read.csv(paste0(in.dir,"Dias/Y0.csv"))

# Deforestation and agricultural area projections from Globiom-BR 2015-2050 
# 1000ha for "NatVeg" (native vegetarion area) 
# "CrpSoy" (cropland area) and "Soya" (soy area)
# Nondimentional for cfrac 
gl.scenarios <- c("IDC3","IDC3NoFC","IDC3ZD")

globiom <- lapply(gl.scenarios, function(x) 
  read.csv(paste0(in.dir,"GLOBIOM-BR/Output_",x,".CSV"),
           col.names = c("Scenario","Class","Country","ID","Parameter","Year","Val")))


# #==============================================
# #==============================================
# #
# #   Calculate yield loss and CRV 
# #
# #==============================================
# #==============================================

prices.s <- c(233.2,276.7,223.1,217.5,317.3,453.3,378.6,385,484.3,537.8)


EDD_sens <- 0.005 # Schlenker and Roberts (2009)



#==============================================
#   Historical 
#==============================================
years <- paste0("X",c(2005,2010,2012))

prices.s.his <- data.frame(Year = years,
                           Prices = prices.s[c(3,8,10)])

# Base yield

# Soy area
soyarea <- areasoy
names(soyarea) <- paste0("X",c(2000:2012))
soyarea <- soyarea[[which(names(soyarea) %in% years)]]
soyarea <- data.frame(CRshp$ID, exact_extract(soyarea,CRshp,"sum"))
colnames(soyarea) <- c("ID",years)
soyarea <- soyarea %>%
  pivot_longer(X2005:X2012,names_to = "Year",values_to = "soyarea")

# Forest loss and forest area
#   Floss is in km2
natveg.his <- mapbiomas %>%
  filter(Par == "area")  %>%
  dplyr::select(ID,Year,val) %>%
  pivot_wider(names_from = Year, values_from = val) 
natveg.his <- left_join(CRcsv,natveg.his) 

floss.his <- data.frame(ID = natveg.his$ID,
                        do.call(cbind,lapply(years, 
                                             function(x) (natveg.his[,x]-natveg.his[,"X1985"])/1000000))) #transform to km2
colnames(floss.his) <- c("ID",years)
floss.his <- floss.his %>%
  pivot_longer(X2005:X2012,names_to = "Year",values_to = "Floss")
floss.his$Floss[which(floss.his$Floss>0)] <- NA



natveg.his <- data.frame(ID = natveg.his$ID, 
                         do.call(cbind,lapply(years, 
                                              function(x) (natveg.his[,x]*0.000001/natveg.his[,"area_km2"]))))
colnames(natveg.his) <- c("ID",years)
natveg.his <- natveg.his %>%
  pivot_longer(X2005:X2012,names_to = "Year",values_to = "Forarea") #forarea in index form 0-1


# read delta_EDDs
dEDD <- brick(lapply(years, function(x) 
  raster(paste0(deltaEDD.folder,"Soybeans.deltadd._Hist_",substr(x,2,5),"_.nc"),
         var = "edd")))

dEDDtab <- data.frame(CRshp$ID, exact_extract(dEDD,CRshp,"mean"))
colnames(dEDDtab) <- c("ID",years)
dEDDtab <- dEDDtab %>%
  pivot_longer(X2005:X2012,names_to = "Year",values_to = "dEDD")
dEDDtab$dEDD[which(dEDDtab$dEDD<0)] <- NA

sampl <- CRcsv %>%  
  left_join(natveg.his) %>%
  left_join(soyarea)  %>% left_join(biomes) %>%
  mutate(areasoyp = soyarea*0.01/area_km2) %>% 
  dplyr::select(ID,Year,Forarea,areasoyp,CD_Bioma) %>% 
  filter(CD_Bioma %in% c(1,3),areasoyp>0.01) %>%
  dplyr::select(ID) %>% unique

samplh <- sampl

finaltab.his <- CRcsv %>% 
  left_join(dEDDtab) %>% 
  left_join(floss.his) %>%
  left_join(soyarea) %>% 
  left_join(natveg.his) %>% left_join(prices.s.his) %>%
  left_join(Y0) %>% right_join(sampl) %>%
  mutate(dy = exp(-EDD_sens*dEDD) - 1) %>%
  mutate(CRVn = -dy*Y0*Prices, #ton/ha * $/ton = $/ha
         areasoyp = soyarea*0.01/area_km2)


# results
finaltab.his <- finaltab.his %>%
  filter(CRVn>0)
write.csv(finaltab.his,paste0(out.dir,"CRV_his.csv"))



#==============================================
#   Future Scenarios
#==============================================

years <- paste0("X",c(2015,2020,2025,2030,2035,2040,2045,2050))
soy_v <-537.8 # mean(prices.s)

yd.shifter <- c(1.138,1.214,1.279,1.344,1.410,1.476,1.539,1.602)
Y0 <- calc(yieldsoy, fun = mean, na.rm = T)
Y0 <- data.frame(CRshp$ID, exact_extract(Y0,CRshp,"mean"))
colnames(Y0) <- c("ID","Y0")
Y0s <- data.frame(Y0$ID,do.call(cbind,lapply(yd.shifter, function(x)
  Y0$Y0*x)))
colnames(Y0s) <- c("ID",years)
Y0s <- Y0s %>% pivot_longer(X2015:X2050, values_to = "Y0", names_to = "Year")

finaltab <- list()
samplf <- list()
for (i in 1:length(globiom))
{
  # Soy area (1000 ha)
  soyarea <- globiom[[i]] %>%
    filter(Parameter == "Soya")  %>%
    mutate(Year = paste0("X",Year), soyarea = Val) %>%
    filter(Year %in% years) %>%
    dplyr::select(ID,Year,soyarea)
  
  
  # Florest area and forest loss  (1000 ha)
  natveg <- globiom[[i]] %>%
    filter(Parameter == "NatVeg")  %>%
    dplyr::select(ID,Year,Val) %>%
    mutate(Year = paste0("X",Year)) %>% 
    pivot_wider(names_from = Year, values_from = Val)
  
  floss <- data.frame(ID = natveg$ID,
                      do.call(cbind,lapply(years, 
                                           function(x) (natveg[,x]-natveg[,"X2000"]))))
  floss <- floss %>%
    pivot_longer(X2015:X2050,names_to = "Year",values_to = "floss")
  print(summary(floss))
  natveg <- left_join(CRcsv,natveg)
  natveg <- data.frame(ID = natveg$ID,
                       do.call(cbind,lapply(years, 
                                            function(x) (natveg[,x]*10/natveg[,"area_km2"]))))
  colnames(natveg) <- c("ID",years)
  natveg <- natveg %>%
    pivot_longer(X2015:X2050,names_to = "Year",values_to = "Forarea")
  
  # read delta_EDDs
  dEDD_total <- brick(lapply(seq(2015,2050,by = 5), function(x) 
    raster(paste0(deltaEDD.folder,"Soybeans.deltadd._",x,gl.scenarios[i],"DT_t_.nc"),
           var = "edd")))
  dEDD_luc <- brick(lapply(seq(2015,2050,by = 5), function(x) 
    raster(paste0(deltaEDD.folder,"Soybeans.deltadd._",x,gl.scenarios[i],"DT_luc_.nc"),
           var = "edd")))
  dEDD_ghg <- brick(lapply(seq(2015,2050,by = 5), function(x) 
    raster(paste0(deltaEDD.folder,"Soybeans.deltadd._",x,"DT_ghg_.nc"),
           var = "edd")))
  
  dEDD_total <- data.frame(CRshp$ID, exact_extract(dEDD_total,CRshp,"mean"))
  colnames(dEDD_total) <- c("ID",years)
  dEDD_total <- dEDD_total %>%
    pivot_longer(X2015:X2050,names_to = "Year",values_to = "dEDD_total")
  
  dEDD_luc <- data.frame(CRshp$ID, exact_extract(dEDD_luc,CRshp,"mean"))
  colnames(dEDD_luc) <- c("ID",years)
  dEDD_luc <- dEDD_luc %>%
    pivot_longer(X2015:X2050,names_to = "Year",values_to = "dEDD_luc")
  
  dEDD_ghg <- data.frame(CRshp$ID, exact_extract(dEDD_ghg,CRshp,"mean"))
  colnames(dEDD_ghg) <- c("ID",years)
  dEDD_ghg <- dEDD_ghg %>%
    pivot_longer(X2015:X2050,names_to = "Year",values_to = "dEDD_ghg")
  
  
  # calculate delta_y and CRV
  samplf[[i]] <- CRcsv %>%  
    left_join(natveg) %>%
    left_join(soyarea)  %>% left_join(biomes) %>%
    mutate(areasoyp = soyarea*10/area_km2) %>% 
    dplyr::select(ID,Year,Forarea,areasoyp,CD_Bioma) %>% 
    filter(areasoyp>0.01,CD_Bioma %in% c(1,3)) %>%
    dplyr::select(ID) %>% unique
  
  
  finaltab[[i]] <- CRcsv %>% right_join(samplf[[i]]) %>%
    left_join(dEDD_total) %>% left_join(dEDD_ghg) %>% left_join(dEDD_luc) %>% 
    left_join(floss) %>%
    left_join(soyarea) %>% left_join(natveg) %>%
    left_join(Y0s) %>%
    mutate(dyt = exp(-EDD_sens*dEDD_total) - 1,
           dyl = exp(-EDD_sens*dEDD_luc) - 1,
           dyg = exp(-EDD_sens*dEDD_ghg) - 1) %>%
    mutate(CRVln = (-dyl)*Y0*soy_v,
           areasoyp = soyarea*10/area_km2,
           Scenario = gl.scenarios[i])
  
  
}


aggtab <- do.call(rbind,finaltab) %>% right_join(samplf) %>% 
  left_join(biomes) %>%
  filter(CRVln>0,CD_Bioma %in% c(1,3))
write.csv(aggtab,paste0(out.dir,"CRV_scen.csv"))



