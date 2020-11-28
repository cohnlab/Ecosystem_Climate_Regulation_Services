################################################
#   Title
#   Date
#   Authors: Gabriel Abrahao and Rafaela Flach
#
################################################


Packages <- c("dplyr","tidyverse","RColorBrewer","stringr",
              "ggthemes","ggplot2","ggpubr","formatR","ggExtra",
              "gdata","readxl","ggridges","Rcpp","data.table",
              "sf","ncdf4","raster","fasterize","tmap",
              "ggspatial","classInt","data.table","exactextractr",
              "rasterVis","rgdal","viridis","reticulate", "wesanderson")
lapply(Packages, library, character.only = TRUE)


#==============================================
#   Input Data
#==============================================

in.dir <- "Input_Data/"
fig.dir <- "Figures/"
deltaEDD.folder <- "Output/DEDD/"

# Gridcells
CRshp <- st_read(paste0(in.dir,"GIS/BrazilCR.shp"))
CRcsv <- read.csv(paste0(in.dir,"GIS/BrazilCR.csv"))
mask <- readOGR(paste0(in.dir,"GIS/bra_admbnda_adm0_ibge_2020.shp"))

# Reference grid
nctemplate <- raster(paste0(in.dir,"GIS/refgrid_xavier.nc"), 
 level = 1, varname = "tmax")

# Reference info
biomes <- read.csv(paste0(in.dir,"GIS/CR_biome.csv"))
CRmun <- read.csv(paste0(in.dir,"GIS/CRmun.csv"))

#==============================================
#   Input Data - Input data
#==============================================
## Deforestation and agricultural area data (2000-2012)
# Forest area from Mapbiomas (m^2)
mapbiomas <- read.csv(paste0(in.dir,"MAPBIOMAS/mapbiomas_processed.csv"))

# Yield for soy, from Dias et al (2016) (ton/ha)


# Base yield
Y0 <- read.csv(paste0(in.dir,"Dias/Y0.csv"),row.names = 1)

years <- paste0("X",c(2015,2020,2025,2030,2035,2040,2045,2050))
yd.shifter <- c(1.138,1.214,1.279,1.344,1.410,1.476,1.539,1.602)
Y0s <- data.frame(Y0$ID,do.call(cbind,lapply(yd.shifter, function(x)
  Y0$Y0*x)))
colnames(Y0s) <- c("ID",years)
Y0s <- Y0s %>% pivot_longer(X2015:X2050, values_to = "Y0", names_to = "Year")


# Cropland area for soy, from Dias et al (2016) (ha)
areasoy <- brick(paste0(in.dir,"Dias/LUSOYBEAN20002012.nc"),var="landuse")

# Deforestation and agricultural area projections from Globiom-BR 2015-2050 
# 1000ha for "NatVeg" (native vegetarion area) 
# "CrpSoy" (cropland area) and "Soya" (soy area)
# Nondimentional for cfrac 
gl.scenarios <- c("IDC3","IDC3NoFC","IDC3ZD")

globiom <- lapply(gl.scenarios, function(x) 
  read.csv(paste0(in.dir,"GLOBIOM-BR/Output_",x,".CSV"),
           col.names = c("Scenario","Class","Country","ID","Parameter","Year","Val")))
globiom <- lapply(1:length(globiom), function(x) data.frame(globiom[[x]],Scen = gl.scenarios[x]))


# Forest biomass scenarios
lpindata = read.csv("Output/scenarios_land_price.csv")

# Land price scenarios
cpindata = read.csv("Output/scenarios_agb_value.csv")

#==============================================
#   Input Data - Results
#==============================================

DT.his <- read.csv("Output/DT_his.csv",row.names = 1)
DT.scen <- read.csv("Output/DT_scen.csv",row.names = 1)

finaltab.his <- read.csv("Output/CRV_his.csv",row.names = 1)
finaltab.scen <- read.csv("Output/CRV_scen.csv",row.names = 1)

#==============================================
#   Output - Main results
#==============================================

# -----------------------------
#   Table 2 - main CRVp his info
# -----------------------------
subpars <- c("NVloss","dy","areasoyp")
pars <- c("NVloss","DTluc","dEDD","dy","areasoyp","Prices","CRV")
parnames <- c("Native Vegetation loss (%)","Temperature change (C)",
              "Change in Extreme Degree Days (dEDD)","Change in productivity (%)",
              "Soy area (%)","Prices (2005$ ton-1)","CRV (2005$ ha-1 yr-1)")

newtable <-  DT.his %>%
  left_join(biomes) %>% left_join(finaltab.his) %>%
  filter(CD_Bioma %in% c(1,3),Year!= "X2019",CRV<30000)
names(newtable) <- names(newtable) %>%
  gsub("_", "",.) 

hisstat <- newtable %>%
  group_by(Year,CDBioma) %>%
  summarise_at(pars, funs(mean(., na.rm = TRUE), 
      max(., na.rm = TRUE), 
      min(., na.rm = TRUE),
      sd(., na.rm = TRUE))) %>%
  pivot_longer(NVloss_mean:CRV_sd,names_sep = "_", 
               names_to = c("Parameter",".value")) %>% 
  filter(Year == "X2012") %>% ungroup  

hisstat[which(hisstat$Parameter %in% subpars),c("mean","max","min","sd")] <- 
  100*hisstat[which(hisstat$Parameter %in% subpars),c("mean","max","min","sd")]
hisstat <- hisstat %>%
  mutate(CDBioma = plyr::mapvalues(CDBioma,c(1,3),c("Amazon","Cerrado")),
         Parameter = plyr::mapvalues(Parameter,pars,parnames),
         mean = paste0(round(mean,digits = 2)," (",round(sd,digits=2),")"),
         max = round(max,digits = 2),
         min = round(min,digits = 2)) %>% dplyr::select(-Year,-sd) %>%
  pivot_wider(names_from = CDBioma,values_from = mean:min) 

write.csv(hisstat,"Figures/Main/Table_2.csv")

# -----------------------------
#   Figure 3 - DRV versus forest area
# -----------------------------

plottab <- finaltab.his %>% filter(CRV>0,CRV<10000,Year == "X2012")

lims.i <- seq(0,95,by = 5)/100
lims.f <- seq(5,100,by = 5)/100

list1 <- data.frame(Mean_CRV = unlist(lapply(1:20, function(x) 
  (plottab %>% filter(Forarea > lims.i[x]) %>% filter(Forarea < lims.f[x]))[,"CRV"] %>% mean(na.rm=TRUE))),
  Forest_area = seq(5,100,by = 5),Par = "Forest area")

f3Ma <- annotate_figure(ggscatter(list1,"Forest_area","Mean_CRV",
                                  size = 2.5,
                                  color = "#FF5733",
                                  xlab = "Forest area (%)",
                                  ylab = "Mean CRV (2005$ ha-1 yr-1)"),bottom = "") 


lims.i <- seq(0,95,by = 5)/100
lims.f <- seq(5,100,by = 5)/100

list2 <- data.frame(Mean_CRV = unlist(lapply(1:20, function(x) 
  (plottab %>% filter(areasoyp > lims.i[x]) %>% 
     filter(areasoyp < lims.f[x]))[,"CRV"] %>% mean(na.rm=TRUE))),
  Forest_area = seq(5,100,by = 5),
  Par = "Soy area",
  count = unlist(lapply(1:20, function(x) 
    nrow(plottab %>% filter(areasoyp > lims.i[x]) %>% 
	filter(areasoyp < lims.f[x])))))
	   
f3Mb <- annotate_figure(ggscatter(list2,"Forest_area","Mean_CRV",
                                  size = 2.5,
                                  color = "#FF5733",
                                  xlab = "Soy area (%)",
                                  ylab = "Mean CRV (2005$ ha-1 yr-1)"),bottom = "") 

final <- rbind(list1,list2)

f3M <- ggarrange(f3Ma,f3Mb)

png(paste0(fig.dir,"Main/Figure_3.png"),width = 20,height = 12,units = "cm",res=360)
print(f3M)
dev.off()
# -----------------------------
#   Figure 4 - DeltaEDD maps
# -----------------------------

or <- colorRampPalette(colors = c("#FFFFFF","#ffd830","#ffa830","#d87848","#904860"))

patt <- c("2050IDC3DT_luc_","2050DT_ghg","2050IDC3DT_t_")
titles <- c("Land use","Climate Change","Total")

ncs <- unlist(lapply(patt,function(x) list.files(deltaEDD.folder,pattern=x)))
plottable <- brick(lapply(ncs, function(x) raster(paste0(deltaEDD.folder,x),var="edd")))

f4M <- levelplot(plottable, 
                 margin = FALSE,
                 col.regions=or(210),                   
                 at=c(-Inf,seq(0, 200, len=201),Inf),
                 names.attr=titles, 
                 layout = c(3, 1),
                 main = "Change in EDD (2050, IDC3 scenario)") + 
  layer(sp.polygons(mask))

png(paste0(fig.dir,"Main/Figure_4.png"),width = 15,height = 7,units = "cm",res=360)
print(f4M)
dev.off()

# -----------------------------
#   Figure 5 - CRV non-linearity
# -----------------------------

plottable <- finaltab.scen %>% dplyr::select(ID,Year,Scenario,floss,lprodt,lprodl,lprodg) %>%
  filter(lprodt<0,floss<0) %>%
  pivot_longer(floss:lprodg,names_to="Parameter",values_to="Val") %>%
  group_by(Year,Scenario,Parameter) %>%
  summarize(sum = sum(Val, na.rm = TRUE)) %>% ungroup() %>%
  pivot_wider(names_from = Parameter,values_from = sum) %>%
  mutate(CRV1 = (lprodl)*prices.f,
         CRV2 = (lprodt-lprodg)*prices.f) %>%
  pivot_longer(CRV1:CRV2,names_to = "Parameter",values_to = "sum") %>%
  mutate(Year = as.numeric(substr(Year,2,5))) %>%
  filter(Year > 2016)


plottable$Parameter <- factor(plottable$Parameter,levels = c("CRV1","CRV2"),
                         labels = c("LUC + CC","LUC"))
						 
f5M <- ggplot(plottable, 
                  aes(x=Year, y=-sum/100000000, color=Scenario,linetype = Parameter)) +
  geom_line(size = 1) + theme_pubr() + 
  scale_color_manual(name = "Land use scenario",values=wes_palette("Rushmore")[2:5]) +
  scale_linetype_manual(name = "Heating contributors",values = c("solid","dashed")) +
  theme(legend.position="right") + labs(y= "Landscape-aggregated CRV (2005$/year)", x = "Year")

png(paste0(fig.dir,"Main/Figure_5.png"),width = 16,height = 10,units = "cm",res=360)
print(f5M)
dev.off()

# -----------------------------
#   Figure 6
# -----------------------------


npvaggtib <- read.csv("Output/CRVp0.1.csv",row.names = 1)

# Land prices =========================================================

lpdata = lpindata %>% left_join(.,munic2cr, by = "Geocodigo") %>% 
  filter(lpscenario %in%c("lp_hist","lp_trend"), 
         year %in% c(2012,2050), 
         var == "Media_A_IGPDI") %>% dplyr::select(-year) %>%
  pivot_wider(names_from = lpscenario,values_from=value) %>%
  drop_na() 

npvaggtib = npvaggtib %>%
  left_join(lpdata, by = "ID") 

# Carbon prices ======================================================
cpindata %>% filter(var %in% c("cvalueha")) %>% filter((agbscenario =="agb_incp"))

cpdata = cpindata %>%
  filter(lpscenario %in%c("agb_hist","agb_incp"), 
         year %in% c(2014,2050), 
         var == "cvalueha") %>% dplyr::select(-year) %>%
  pivot_wider(names_from = agbscenario,values_from=cvalueha) %>%
  drop_na() 

# Join carbon values
npvaggtib = npvaggtib %>%
  left_join(cpdata, by = "ID") 

npvaggtib = npvaggtib %>%
  mutate(perc_lp_hist = 100*CRVl2_NPV/lp_hist,
         perc_lp_trend = 100*CRVl2_NPV/lp_trend,
         perc_agb_hist = 100*CRVl2_NPV/agb_hist,
         perc_agb_incp = 100*CRVl2_NPV/agb_incp,
         perc_lp_agb_low =  100*(CRVl2_NPV+agb_hist)/lp_hist,
         perc_lp_agb_high = 100*(CRVl2_NPV+agb_incp)/lp_trend) %>%
  mutate(CRVl2_NPV_C_hist = CRVl2_NPV + agb_hist,
         CRVl2_NPV_C_incp = CRVl2_NPV + agb_incp
  )


# Merge IDC3 scenario with shapefile for plotting
npvaggshp = npvaggtib %>% filter(Scenario == "IDC3") %>% dplyr::select(-Scenario) %>%
  gather("var","value",-ID) %>%
  left_join(baseshp, by = "ID") %>%
  st_as_sf()

# Base map (tmap object to be summed)
usebbox = st_bbox(c(xmin = -74, xmax = -34.7, ymax = 5, ymin = -33.5))  
basemap = tm_shape(stateshp, bbox = usebbox) + tm_borders()

map_perc_all = npvaggshp %>%
  filter(var %in% c("perc_lp_hist", "perc_lp_trend",
                    "perc_agb_hist", "perc_agb_incp",
                    "perc_lp_agb_low", "perc_lp_agb_high")) %>%
  mutate(var = recode(as.factor(var),
                      "perc_lp_hist" =  "CRV_PV vs. 2015 Land prices",
                      "perc_lp_trend" = "CRV_PV vs. 2050 Land prices",
                      "perc_agb_hist" = "CRV_PV vs. 2015 Carbon prices",
                      "perc_agb_incp" = "CRV_PV vs. 2050 Carbon prices",
                      "perc_lp_agb_low" =  "(CRV_PV + Carbon) vs. 2015 Land prices",
                      "perc_lp_agb_high" = "(CRV_PV + Carbon) vs. 2050 Land prices")) %>%
  mutate(var = factor(var,levels(var)[c(1,2,5,6,4,3)])) %>% # Reorder plots
  mutate(value = as.numeric(value)) %>%
  tm_shape(bbox = usebbox) + tm_fill(col = "value",
                                     breaks = seq(0,200,20),
                                     palette = "Spectral",
                                     title = "CRV_PV as fraction of benchmark (%)",
                                     legend.is.portrait = F,
                                     text.to.columns = T) + 
  tm_facets("var", nrow = 3) +
  basemap +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom",
            # legend.position = c(0.01, 1.2),
            legend.position = c(0.01, 0.8),
            # legend.outside.ss = 0.9,
            legend.width = 1.3,
            panel.label.size = 1.2)

tmap_save(map_perc_all, file = paste0("Figures/Main/Figure_6.png"), width = 2300, height = 4000)


# -----------------------------
#   Table 3
# -----------------------------


tab3 <- npvaggtib %>% 
  mutate(islarg_lp_hist = as.numeric(CRVl2_NPV > lp_hist),
         islarg_lp_trend = as.numeric(CRVl2_NPV > lp_trend)) %>% 
  group_by(Scenario) %>%
  summarise(frac_islarg_lp_hist = 100*mean(as.numeric(perc_lp_hist>100),na.rm=TRUE),
            frac_islarg_lp_trend = 100*mean(as.numeric(perc_lp_trend>100),na.rm=TRUE),
            frac_islarg_50pp_lp_hist = 100*mean(as.numeric(perc_lp_hist>50),na.rm=TRUE),
            frac_islarg_50pp_lp_trend = 100*mean(as.numeric(perc_lp_trend>50),na.rm=TRUE),
            frac_islarg_agb_hist = 100*mean(as.numeric(perc_agb_hist>100),na.rm=TRUE),
            frac_islarg_agb_incp = 100*mean(as.numeric(perc_agb_incp>100),na.rm=TRUE),
            frac_islarg_50pp_agb_hist = 100*mean(as.numeric(perc_agb_hist>50),na.rm=TRUE),
            frac_islarg_50pp_agb_incp = 100*mean(as.numeric(perc_agb_incp>50),na.rm=TRUE),
            frac_islarg_lp_agb_low = 100*mean(as.numeric(perc_lp_agb_low>100),na.rm=TRUE),
            frac_islarg_lp_agb_high = 100*mean(as.numeric(perc_lp_agb_high>100),na.rm=TRUE),
            frac_islarg_50pp_lp_agb_low = 100*mean(as.numeric(perc_lp_agb_low>50),na.rm=TRUE),
            frac_islarg_50pp_lp_agb_high = 100*mean(as.numeric(perc_lp_agb_high>50),na.rm=TRUE)) %>%
  as.data.frame() 
write.csv(tab3,"Figures/Main/Table_3.csv", row.names = F)



#==============================================
#   Output - Supplementary figures
#==============================================

# historical deforestation

def <- mapbiomas %>% 
  filter(Par == "area") %>% 
  group_by(Year) %>%
  summarise(sum = sum(val)/10000000000) %>% ungroup %>%
  mutate(Year = as.numeric(substr(Year,2,5)))

fig.s2 <- annotate_figure(ggline(def,"Year","sum",
ylab = bquote('Native vegetarion area (million ha)')),
      bottom = text_grob("Data source: Mapbiomas"))

png(paste0(fig.dir,"SM/def_his.png"),width = 16,height = 10,units = "cm",res=360)
plot(fig.s2)
dev.off()

# historical soy areas

soyarea <- data.frame(Year = as.character(2000:2012), 
  area = cellStats(areasoy,stat="sum")/1000000)

fig.s3 <- annotate_figure(ggline(soyarea,"Year","area",
ylab = "Soy crop area (million ha)"),
      bottom = text_grob("Data source: Dias et al. (2016)"))

png(paste0(fig.dir,"SM/soy_his.png"),width = 16,height = 10,units = "cm",res=360)
plot(fig.s3)
dev.off()

# historical productivity
Y0f <- rasterize(left_join(CRshp,Y0),nctemplate,"Y0")

org <- colorRampPalette(colors = brewer.pal(9,"Greens"))

fig.s4 <- levelplot(Y0f, 
                    margin = FALSE,
                    col.regions=org(100),                   
                    at=seq(0, 3.5,by = 0.2),
                    main = "Baseline productivity (ton/ha)") +
  layer(sp.polygons(mask))
png(paste0(fig.dir,"SM/base_prod.png"),width = 15,height = 7,units = "cm",res=360)
print(fig.s4)
dev.off()

# projected deforestation

f.def <- do.call(rbind,globiom) %>%left_join (biomes) %>%
  filter(Parameter == "NatVeg",Year > 2016,CD_Bioma %in% c(1,3)) %>%
  group_by(Scen,Year) %>% 
  summarize(natveg = sum(Val)/1000) %>% ungroup()

fig.s5 <- annotate_figure(ggline(f.def,"Year","natveg",
color = "Scen",
ylab = "Native vegetation (mi ha)",
palette = wes_palette("Rushmore")[2:5],
point.size = 0),
      bottom = "Source: Globiom-BR")
png(paste0(fig.dir,"SM/def_scen.png"),width = 16,height = 10,units = "cm",res=360)
plot(fig.s5)
dev.off()

# projected soy areas

f.soy <- do.call(rbind,globiom) %>%
  filter(Parameter == "Soya",Year > 2016) %>%
  group_by(Scen,Year) %>% 
  summarize(soy = sum(Val)/1000) %>% ungroup()

fig.s6 <- ggline(f.soy,"Year","soy",
                 color = "Scen",
                 size = 1.2,
                 ylab = "Projected soy area (mi ha)",
                 palette = wes_palette("Rushmore")[2:5],
                 point.size = 0)
png(paste0(fig.dir,"SM/soy_scen.png"),width = 16,height = 10,units = "cm",res=360)
plot(fig.s6)
dev.off()

# projected productivity

yd <- data.frame(Years = seq(2015,2050,by=5), shifter = yd.shifter) %>%
  filter(Years > 2016)

fig.s7 <- ggline(yd,"Years","shifter",
                 size = 1.2,
                 ylab = "Multiplying factor from baseline yield",
                 point.size = 0.5)

png(paste0(fig.dir,"SM/scen_prod.png"),width = 16,height = 10,units = "cm",res=360)
plot(fig.s7)
dev.off()


# FIGURE S9

npvaggtib <- read.csv("Output/CRVp0.03.csv",row.names = 1)

# Land prices =========================================================

lpdata = lpindata %>% left_join(.,munic2cr, by = "Geocodigo") %>% 
  filter(lpscenario %in%c("lp_hist","lp_trend"), 
         year %in% c(2012,2050), 
         var == "Media_A_IGPDI") %>% dplyr::select(-year) %>%
  pivot_wider(names_from = lpscenario,values_from=value) %>%
  drop_na() 

npvaggtib = npvaggtib %>%
  left_join(lpdata, by = "ID") 

# Carbon prices ======================================================
cpindata %>% filter(var %in% c("cvalueha")) %>% filter((agbscenario =="agb_incp"))

cpdata = cpindata %>%
  filter(lpscenario %in%c("agb_hist","agb_incp"), 
         year %in% c(2014,2050), 
         var == "cvalueha") %>% dplyr::select(-year) %>%
  pivot_wider(names_from = agbscenario,values_from=cvalueha) %>%
  drop_na() 

# Join carbon values
npvaggtib = npvaggtib %>%
  left_join(cpdata, by = "ID") 

npvaggtib = npvaggtib %>%
  mutate(perc_lp_hist = 100*CRVl2_NPV/lp_hist,
         perc_lp_trend = 100*CRVl2_NPV/lp_trend,
         perc_agb_hist = 100*CRVl2_NPV/agb_hist,
         perc_agb_incp = 100*CRVl2_NPV/agb_incp,
         perc_lp_agb_low =  100*(CRVl2_NPV+agb_hist)/lp_hist,
         perc_lp_agb_high = 100*(CRVl2_NPV+agb_incp)/lp_trend) %>%
  mutate(CRVl2_NPV_C_hist = CRVl2_NPV + agb_hist,
         CRVl2_NPV_C_incp = CRVl2_NPV + agb_incp
  )


# Merge IDC3 scenario with shapefile for plotting
npvaggshp = npvaggtib %>% filter(Scenario == "IDC3") %>% dplyr::select(-Scenario) %>%
  gather("var","value",-ID) %>%
  left_join(baseshp, by = "ID") %>%
  st_as_sf()

# Base map (tmap object to be summed)
usebbox = st_bbox(c(xmin = -74, xmax = -34.7, ymax = 5, ymin = -33.5))  
basemap = tm_shape(stateshp, bbox = usebbox) + tm_borders()

map_perc_all = npvaggshp %>%
  filter(var %in% c("perc_lp_hist", "perc_lp_trend",
                    "perc_agb_hist", "perc_agb_incp",
                    "perc_lp_agb_low", "perc_lp_agb_high")) %>%
  mutate(var = recode(as.factor(var),
                      "perc_lp_hist" =  "CRV_PV vs. 2015 Land prices",
                      "perc_lp_trend" = "CRV_PV vs. 2050 Land prices",
                      "perc_agb_hist" = "CRV_PV vs. 2015 Carbon prices",
                      "perc_agb_incp" = "CRV_PV vs. 2050 Carbon prices",
                      "perc_lp_agb_low" =  "(CRV_PV + Carbon) vs. 2015 Land prices",
                      "perc_lp_agb_high" = "(CRV_PV + Carbon) vs. 2050 Land prices")) %>%
  mutate(var = factor(var,levels(var)[c(1,2,5,6,4,3)])) %>% # Reorder plots
  mutate(value = as.numeric(value)) %>%
  tm_shape(bbox = usebbox) + tm_fill(col = "value",
                                     breaks = seq(0,200,20),
                                     palette = "Spectral",
                                     title = "CRV_PV as fraction of benchmark (%)",
                                     legend.is.portrait = F,
                                     text.to.columns = T) + 
  tm_facets("var", nrow = 3) +
  basemap +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom",
            # legend.position = c(0.01, 1.2),
            legend.position = c(0.01, 0.8),
            # legend.outside.ss = 0.9,
            legend.width = 1.3,
            panel.label.size = 1.2)

tmap_save(map_perc_all, file = paste0("Figures/SM/Figure_S9.png"), width = 2300, height = 4000)


# FIGURE S8
# Maps of CRV_NPV (+carbon) value magnitudes
figs8 <- npvaggshp %>%
  filter(var %in% c("CRVl2_NPV","CRVl2_NPV_C_hist","CRVl2_NPV_C_incp")) %>%
  mutate(var = recode(as.factor(var),
                      "CRVl2_NPV" = "CRV_NPV",
                      "CRVl2_NPV_C_hist" = "CRV_NPV + 2015 Carbon value",
                      "CRVl2_NPV_C_incp" = "CRV_NPV + 2050 Carbon value")) %>%
  mutate(value = as.numeric(value)) %>%
  tm_shape() + tm_fill(col = "value",
                       title = "2005USD/ha",
                       breaks = seq(0,8000,500),
                       palette = "viridis") + 
  tm_facets("var",nrow = 3) +
  basemap

tmap_save(figs8, file = paste0("Figures/SM/Figure_S8.png"), width = 2300, height = 4000)

