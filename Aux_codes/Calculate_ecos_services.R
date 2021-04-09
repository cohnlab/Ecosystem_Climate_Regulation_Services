################################################
#   Estimate ecosystem services for 
#   regulation of extreme heat in neighboring 
#   agriculture - future analysis 
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
out.dir.dt <- "Output/DT_B/"
deltaEDD.folder <- "Output/DEDD_B/"


# Gridcells
CRshp <- st_read(paste0(in.dir,"GIS/BrazilCR.shp"))
CRcsv <- read.csv(paste0(in.dir,"GIS/BrazilCR.csv"))
mask <- readOGR(paste0(in.dir,"GIS/bra_admbnda_adm0_ibge_2020.shp"))
biomes <- read.csv(paste0(in.dir,"GIS/CR_biome.csv")) %>% 
  dplyr::select(-X,-Bioma)

# Reference grid
nctemplate <- raster(paste0(in.dir,"Xavier/refgrid_xavier.nc"), 
                     level = 1, varname = "tmax")

# Ecosystem conversion-temp coefficients
L_NL <- read.csv(paste0(in.dir,"WD_L_NL_B.csv")) %>% dplyr::select(ID,NL)

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


# Yield for soy, from Dias et al (2016) (ton/ha)
yieldsoy <- brick(paste0(in.dir,"Dias/YIELDSOYBEAN20002012.nc"),var="Yield")
yieldsoy <- reclassify(yieldsoy, c(0, NA))


# Deforestation and agricultural area projections from Globiom-BR 2015-2050 
# 1000ha for "NatVeg" (native vegetarion area) 
# "CrpSoy" (cropland area) and "Soya" (soy area)
# Nondimentional for cfrac 
gl.scenarios <- c("IDC3","IDC3NoFC","IDC3ZD")

globiom <- lapply(gl.scenarios, function(x) 
  read.csv(paste0(in.dir,"GLOBIOM-BR/Output_",x,".CSV"),
           col.names = c("Scenario","Class","Country","ID","Parameter","Year","Val")))


# ==============================================
#   Temperature change - Future scenarios
# ==============================================

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
    left_join(CRcsv) %>%
    dplyr::select(-Country) %>%
    mutate(nvp = Val*10/area_km2)
  
  
  dLUC[[i]] <- natveg %>%
    left_join(L_NL) %>% 
    replace(is.na(.), 0) %>%
    mutate(DT_luc = (nvp*NL),
           Scenario = gl.scenarios[i])
}

dLUC.sc <- do.call(rbind,dLUC) %>%
  dplyr::select(Year,ID,Scenario,nvp,DT_luc)

deltat <- dLUC.sc
# generate output
out <- deltat  %>% filter(nvp>0)
write.csv(out,paste0(out.dir,"DT_scen_b.csv"))


shp <- out %>% left_join(CRshp) %>%
  filter(Year == 2050,Scenario == "IDC3")


# generate output

shp <- deltat %>%
  mutate(Year = substr(Year,3,4)) %>%
  dplyr::select(ID,Scenario,Year,DT_luc)   %>%
  pivot_wider(names_from = Year,values_from = DT_luc) %>%
  left_join(CRshp)



# DeltaT maps -- DeltaLUC + Total

fac <- factors[3]

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



#==============================================
#   Calculate delta_EDDs - All
#==============================================

library(reticulate)
use_python("/usr/bin/python")

py_run_file("Compute_EDD/compute_deltaEDD_from_deltaTB.py")


# ==============================================
# ==============================================
# 
#    Calculate yield loss and CRV 
# 
# ==============================================
# ==============================================

prices.s <- c(233.2,276.7,223.1,217.5,317.3,453.3,378.6,385,484.3,537.8)


EDD_sens <- 0.005 # Schlenker and Roberts (2009)

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
    mutate(Year = paste0("X",Year), soyarea = Val*1000) %>%
    filter(Year %in% years) %>%
    dplyr::select(ID,Year,soyarea)
  
  # Florest area and forest loss  (1000 ha)
  natveg <- globiom[[i]] %>%
    filter(Parameter == "NatVeg")  %>%
    dplyr::select(ID,Year,Val) %>%
    mutate(Year = paste0("X",Year),Forarea = Val*1000) %>% dplyr::select(-Val)
  
  
  
  dEDD_luc <- brick(lapply(seq(2015,2050,by = 5), function(x) 
    raster(paste0(deltaEDD.folder,"Soybeans.deltadd._",x,gl.scenarios[i],"DT_luc_.nc"),
           var = "edd")))
  
  
  dEDD_luc <- data.frame(CRshp$ID, exact_extract(dEDD_luc,CRshp,"mean"))
  colnames(dEDD_luc) <- c("ID",years)
  dEDD_luc <- dEDD_luc %>%
    pivot_longer(X2015:X2050,names_to = "Year",values_to = "dEDD_luc")
  
  
  
  finaltab[[i]] <- CRcsv %>% 
    left_join(dEDD_luc) %>% left_join(soyarea) %>% left_join(natveg) %>%
    left_join(Y0s)  %>% left_join(biomes)%>%
    mutate(dyl = exp(-EDD_sens*dEDD_luc) - 1) %>%
    mutate(CRVln = (-dyl*Y0*soy_v*soyarea)/Forarea,
           areasoyp = soyarea*0.01/area_km2,
           Scenario = gl.scenarios[i]) %>% 
    filter(areasoyp>0.01,Forarea>0,CD_Bioma %in% c(1,3))
}



aggtab <- do.call(rbind,finaltab) %>% 
  filter(CRVln>0,CD_Bioma %in% c(1,3))
write.csv(aggtab,paste0(out.dir,"CRV_scen_B.csv"))





