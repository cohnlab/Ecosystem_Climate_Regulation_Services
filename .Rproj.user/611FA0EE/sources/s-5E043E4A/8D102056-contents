################################################
#   Estimate temperature changes for historical 
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
out.dir.dt <- "Output/DT/"

# Gridcells
CRshp <- st_read(paste0(in.dir,"GIS/BrazilCR.shp"))
CRcsv <- read.csv(paste0(in.dir,"GIS/BrazilCR.csv"))
mask <- readOGR(paste0(in.dir,"GIS/bra_admbnda_adm0_ibge_2020.shp"))
biomes <- read.csv(paste0(in.dir,"GIS/CR_biome.csv")) %>% 
  dplyr::select(-X,-Bioma)

# Reference grid
#nctemplate <- raster(paste0(in.dir,"Xavier/refgrid_xavier.nc"), 
#                     level = 1, varname = "tmax")

# Ecosystem conversion-temp coefficients
L_NL <- read.csv(paste0(in.dir,"WD_L_NL_A.csv"))

# Future climate - CMIP6, deltaT in relation to 1995-2005 (degreeC)
ssp245 <- read.csv(paste0(in.dir,"CMIP6/Soybeans.ssp245.gccdtemp.all.extended.csv"))

## Deforestation and agricultural area data (2000-2012)
# Forest area from Mapbiomas (m^2)
mapbiomas <- read.csv("Input_Data/MAPBIOMAS/reduced_mapbiomas_6.csv") %>%
  dplyr::select(-nid,-perimeter,-.geo,-system.index,-area,-areatot) %>%
  pivot_longer(area1985:newcroparea2019,names_to = "Parameter",values_to = "val") %>%
  mutate(Par = substr(Parameter,1,nchar(Parameter)-4),
         Year = paste0("X",substr(Parameter,nchar(Parameter)-3,nchar(Parameter)))) %>%
  dplyr::select(-Parameter)

# Cropland area for soy, from Dias et al (2016) (ha)
soyarea <- read.csv("Input_Data/Dias/prepross_soyarea.csv",row.names = 1)
Y0 <- read.csv(paste0(in.dir,"Dias/Y0.csv"))

# Deforestation and agricultural area projections from Globiom-BR 2015-2050 
# 1000ha for "NatVeg" (native vegetarion area) 
# "CrpSoy" (cropland area) and "Soya" (soy area)
# Nondimentional for cfrac 
gl.scenarios <- c("IDC3","IDC3NoFC","IDC3ZD")

globiom <- lapply(gl.scenarios, function(x) 
  read.csv(paste0(in.dir,"GLOBIOM-BR/Output_",x,".CSV"),
           col.names = c("Scenario","Class","Country","ID","Parameter","Year","Val")))


#==============================================
#==============================================
# 
#   Main calculations
# 
#==============================================
#==============================================

#==============================================
#   Temperature change - Historical
#==============================================

years <- paste0("X",c(2000,2005,2010,2012,2015))

# Estimate DeltaT_LUC

natveg <- mapbiomas %>%
  filter(Par == "area")  %>%
  dplyr::select(ID,Year,val) %>%
  pivot_wider(names_from = Year, values_from = val) %>%
  left_join(CRcsv) %>%
  dplyr::select(-Country)

natveg.n <- data.frame(ID = natveg$ID,
                       do.call(cbind,lapply(years,
                                            function(x) -0.000001*((natveg[,x]-natveg[,"X1985"])/natveg[,"area_km2"])))) %>%
  pivot_longer(years,names_to = "Year",values_to = "NVloss") %>%
  mutate(ID = as.character(ID))
natveg.n[which(natveg.n$NVloss<0),"NVloss"] <- 0

nvloss15 <- data.frame(ID = natveg$ID,
                       do.call(cbind,lapply(years,
                                            function(x) -0.000001*((natveg[,x]-natveg[,"X1985"]))))) %>%
  pivot_longer(years,names_to = "Year",values_to = "NVloss") %>%
  mutate(ID = as.character(ID)) %>% filter(Year == "X2015") %>%
  rename(NVloss15 = NVloss) %>% dplyr::select(ID,NVloss15) 

nvloss15[which(is.na(nvloss15$NVloss15)),"NVloss15"] <- 0


sareap <- CRcsv %>%  
  left_join(soyarea)  %>%
  mutate(areasoyp = soyarea*0.01/area_km2) %>% dplyr::select(ID,areasoyp)

crfrac <- mapbiomas %>% 
  filter(Year == "X2012") %>% 
  pivot_wider(names_from = Par,values_from = val) %>% 
  left_join(sareap) %>%
  mutate(crfrac = newcroparea*areasoyp/croparea) %>% dplyr::select(ID,crfrac)


dLUC <- natveg.n %>%
  left_join(L_NL) %>% left_join(crfrac) %>%
  replace(is.na(.), 0) %>%
  mutate(DT_NL = NVloss*NL,
         DT_L =L*crfrac,
         DT_luc = (NVloss*NL)+(L*crfrac))

# generate output
dLUCh <- dLUC %>%
  dplyr::select(Year,ID,L,NL,NVloss,crfrac,DT_NL,DT_L,DT_luc) %>%
  filter(DT_NL>0)
write.csv(dLUCh,paste0(out.dir,"DT_his.csv"))

shp <- dLUCh %>%
  mutate(Year = substr(Year,4,5)) %>%
  pivot_wider(names_from = Year,values_from = NVloss:DT_luc) %>%
  left_join(CRshp)



# Generate DeltaT maps

dLUCh <- dLUC %>% 
  dplyr::select(ID,Year,DT_luc) %>%
  pivot_wider(names_from = Year,values_from = DT_luc)
for (i in years)
{
  
  shp <- left_join(CRshp,dLUCh)
  ras <- rasterize(shp,nctemplate,i)
  
  writeRaster(ras, paste0(out.dir.dt,"Hist_",substr(i,2,5),".nc"), overwrite=TRUE, format="CDF",
              varname="Temperature", varunit="degC",
              longname="Temperature",
              xname="X",   yname="Y",zname="nbands",
              zunit="numeric")
}



dts <- c("DT_luc","DT_NL","DT_L")

for (i in dts)
{
  dLUCi <- dLUC %>%
    filter(Year == "X2012") %>%
    dplyr::select(ID,i) 
  shp <- left_join(CRshp,dLUCi)
  ras <- rasterize(shp,nctemplate,i)
  
  writeRaster(ras, paste0(out.dir.dt,"Hist_2012",i,".nc"), overwrite=TRUE, format="CDF",
              varname="Temperature", varunit="degC",
              longname="Temperature",
              xname="X",   yname="Y",zname="nbands",
              zunit="numeric")
}




# #==============================================
# #   Temperature change - Future scenarios
# #==============================================
# 
years <- paste0("X",c(2015,2020,2025,2030,2035,2040,2045,2050))
factors <- c("DT_t","DT_ghg","DT_luc")

# Estimate DeltaT_LUC

dLUC <- list()
for (i in 1:length(globiom))
{
  
  tab.globiom <- globiom[[i]]
  
  natveg <- tab.globiom %>%
    filter(Parameter == "NatVeg")  %>%
    dplyr::select(ID,Year,Val) %>%
    mutate(Year = paste0("X",Year)) %>%
    pivot_wider(names_from = Year, values_from = Val) %>%
    left_join(CRcsv) %>%
    dplyr::select(-Country)
  
  natveg.n <- data.frame(area_km2 = natveg$area_km2,
                         ID = natveg$ID,
                         do.call(cbind,lapply(years,
                                              function(x) -10*(natveg[,x]-natveg[,"X2015"])))) %>%
    pivot_longer(years,names_to = "Year",values_to = "NVloss") 
  
  natveg.n$NVloss[which(is.na(natveg.n$NVloss))] <- NA
  natveg.n <- natveg.n %>% left_join(nvloss15) %>%
    mutate(NVloss = (NVloss + NVloss15)/area_km2) %>% dplyr::select(ID,Year,NVloss)
  natveg.n$NVloss[which(natveg.n$NVloss<0)] <- NA
  natveg.n$NVloss[which(natveg.n$NVloss>1)] <- 1
  
  
  
  crfrac <- globiom[[i]] %>%
    filter(Parameter == "CROPFRAC") %>%
    dplyr::select(ID,Year,Val) %>% mutate(cfrac = Val,Year = paste0("X",Year))
  
  dLUC[[i]] <- natveg.n %>%
    left_join(L_NL) %>% left_join(crfrac) %>%
    replace(is.na(.), 0) %>%
    mutate(DT_NL = NVloss*NL,
           DT_L =L*cfrac,
           DT_luc = (NVloss*NL)+(L*cfrac),
           Year = as.numeric(substr(Year,2,5)),
           Scenario = gl.scenarios[i])
}

dLUC.sc <- do.call(rbind,dLUC) %>%
  dplyr::select(Year,ID,Scenario,L,NL,NVloss,cfrac,DT_NL,DT_L,DT_luc)

ghg <- rbind(data.frame(ssp245,Scenario = "IDC3"),
             data.frame(ssp245,Scenario = "IDC3NoFC"),
             data.frame(ssp245,Scenario = "IDC3ZD"))

# Estimate DeltaT_Total
deltat <- ghg %>% left_join(dLUC.sc) %>%
  dplyr::mutate(DT_luc = replace_na(DT_luc, 0),
                DeltaT = replace_na(DeltaT, 0)) %>%
  mutate(DT_t = DeltaT + DT_luc) %>%
  mutate(DT_ghg = DeltaT) %>%
  dplyr::select(ID,Year,Scenario,L,NL,NVloss,cfrac,DT_NL,DT_L,DT_t,DT_ghg,DT_luc)


# generate output
out <- deltat  %>% filter(NVloss>0)
write.csv(out,paste0(out.dir,"DT_scen.csv"))


shp <- out %>% left_join(CRshp) %>%
  filter(Year == 2050,Scenario == "IDC3")


# generate output

shp <- deltat %>%
  mutate(Year = substr(Year,4,5)) %>%
  pivot_wider(names_from = Year,values_from = NVloss:DT_luc) %>%
  left_join(CRshp)

### Generate DeltaT maps

deltat <- deltat %>%
  dplyr::select(ID,Scenario,Year,DT_t,DT_ghg,DT_luc)

# DeltaT maps -- GHG

shp <- ssp245[,-1] %>% 
  mutate(Year = paste0("X",Year)) %>%
  pivot_wider(names_from = Year,values_from = DeltaT)
shp <- left_join(CRshp,shp) 


for (i in years)
{
  
  ras <- rasterize(shp,nctemplate,i)
  writeRaster(ras, paste0(out.dir.dt,substr(i,2,5),factors[2],".nc"), overwrite=TRUE, format="CDF",
              varname="DT", varunit="degC",
              longname="Temperature",
              xname="X",   yname="Y",zname="nbands",
              zunit="numeric")
}

# DeltaT maps -- DeltaLUC + Total

fac <- factors[c(1,3)]

for (k in 1:length(gl.scenarios))
{
  DT <- lapply(fac, function(x) deltat %>% filter(Scenario == gl.scenarios[k]) %>%
                 dplyr::select(ID,Year,x) %>%
                 mutate(Year = paste0("X",Year)) %>% unique %>%
                 pivot_wider(names_from = Year,values_from = x))
  for (j in 1:length(DT))
  {
    shp <- left_join(CRshp,DT[[j]])
    for (i in years)
    {
      ras <- rasterize(shp,nctemplate,i)
      
      writeRaster(ras, paste0(out.dir.dt,substr(i,2,5),gl.scenarios[k],fac[j],".nc"), overwrite=TRUE, format="CDF",
                  varname="DT", varunit="degC",
                  longname="Temperature",
                  xname="X",   yname="Y",zname="nbands",
                  zunit="numeric")
    }
  }
}

