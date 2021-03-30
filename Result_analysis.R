################################################
################################################
#
#   Title: Analysis codes of manuscript:
#           By reducing exposure of crops to extreme heat, 
#           ecosystem conservation can aid agricultural development in Brazil
#   Date: 19 Mar 2021
#   Authors: Gabriel Abrahão and Rafaela Flach
#
################################################
################################################


setwd("C:/Users/rafae/Dropbox/AgroServ/Dev Paper/WD_Paper/WDPaper - Copia/")

Packages <- c("dplyr","tidyverse","RColorBrewer","stringr",
              "ggthemes","ggplot2","ggpubr","formatR","ggExtra",
              "gdata","readxl","ggridges","Rcpp","data.table",
              "sf","ncdf4","raster","fasterize","tmap","gridExtra",
              "ggspatial","classInt","data.table","exactextractr",
              "rasterVis","rgdal","viridis","reticulate", "wesanderson")
lapply(Packages, library, character.only = TRUE)


#==============================================
#   Input Data - 1
#==============================================

in.dir <- "C:/Users/rafae/Dropbox/AgroServ/Dev Paper/WD_Paper/WDPaper - Copia/Input_Data/"
fig.dir <- "C:/Users/rafae/Dropbox/AgroServ/Dev Paper/WD_Paper/WDPaper - Copia/Figures_new/"
deltaEDD.folder <- "C:/Users/rafae/Dropbox/AgroServ/Dev Paper/WD_Paper/WDPaper - Copia/Output/DEDD/"

# Gridcells
CRshp <- st_read(paste0(in.dir,"GIS/BrazilCR.shp"))
CRcsv <- read.csv(paste0(in.dir,"GIS/BrazilCR.csv"))
mask <- readOGR(paste0(in.dir,"GIS/bra_admbnda_adm1_ibge_2020.shp"))
baseshp = st_read("Input_Data/GIS/BrazilCR.shp")
stateshp = st_read("Input_Data/GIS/bra_admbnda_adm1_ibge_2020.shp")
munic2cr = read.csv("Input_Data/GIS/munic_to_cr.csv")

# Reference grid
nctemplate <- raster(paste0(in.dir,"GIS/refgrid_xavier.nc"), 
                     level = 1, varname = "tmax")

# Reference info
biomes <- read.csv(paste0(in.dir,"GIS/CR_biome.csv"))
CRmun <- read.csv(paste0(in.dir,"GIS/CRmun.csv"))

#==============================================
#   Input Data - 2
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
DT.scenb <- read.csv("Output/DT_scen_B.csv",row.names = 1)

finaltab.his <- read.csv("Output/CRV_his.csv",row.names = 1)
finaltab.scen <- read.csv("Output/CRV_scen.csv",row.names = 1)
finaltab.scenb <- read.csv("Output/CRV_scen_B.csv",row.names = 1)

npvh.0.1 <- read.csv("Output/CRV_hist_new0.1.csv")
npvh.0.03 <- read.csv("Output/CRV_hist_new0.03.csv")

npvfa.0.1 <- read.csv("Output/CRVp0.1.csv",row.names = 1)
npvfa.0.03 <- read.csv("Output/CRVp0.03.csv",row.names = 1)

npvfb.0.1 <- read.csv("Output/CRVp0.1_B.csv",row.names = 1)
npvfb.0.03 <- read.csv("Output/CRVp0.03_B.csv",row.names = 1)


#==============================================
#   Output - Main results
#==============================================

#----------------------------------------------------
#   Historical DeltaEDDs
#----------------------------------------------------

or <- colorRampPalette(colors = c("#FFFFFF","#ffd830","#ffa830","#d87848","#904860"))

patt <- c("_L_","_NL_","2012DT_luc")
patt.names <- c("Local","Non-local","Total biogeophysical")


ncs <- unlist(lapply(patt,function(x) list.files(deltaEDD.folder,pattern=x)))
plottable <- brick(lapply(ncs, function(x) raster(paste0(deltaEDD.folder,x),var="edd")))

f4M <- levelplot(plottable, 
                 margin = FALSE,
                 col.regions=or(210),                   
                 at=c(-Inf,seq(0, 40, len=201),Inf),
                 names.attr=patt.names, 
                 scales = list(x=list(draw=FALSE),
                               y=list(draw=FALSE)),
                 layout = c(3, 1),
                 xlab = "",
                 ylab = "",
                 main = "Changes in EDD - 2012 (degree days)") + 
  layer(sp.polygons(mask))

png(paste0(fig.dir,"Hist_EDD.png"),width = 15,height = 7,units = "cm",res=360)
print(f4M)
dev.off()


#----------------------------------------------------
#   All Future EDDs
#----------------------------------------------------

or <- colorRampPalette(colors = c("#FFFFFF","#ffd830","#ffa830","#d87848","#904860"))

scenarios <- c("IDC3","IDC3NoFC","IDC3ZD")
scen.names <- c("Baseline","No Forest Code","Zero Deforestation")


patt <- lapply(scenarios, function(x) 
  c(paste0("2050",x,"DT_luc_"),"2050DT_ghg",paste0("2050",x,"DT_t_")))

titles <- c("Biogeophysical","Biogeochemical","Total")

f <- list()
for (i in 1:length(scenarios))
{
  ncs <- unlist(lapply(patt[[i]],function(x) list.files(deltaEDD.folder,pattern=x)))
  plottable <- brick(lapply(ncs, function(x) raster(paste0(deltaEDD.folder,x),var="edd")))
  
  f[[i]] <- levelplot(plottable, 
                      margin = FALSE,
                      col.regions=or(210),                   
                      at=c(-Inf,seq(0, 200, len=201),Inf),
                      names.attr=titles, 
                      scales = list(x=list(draw=FALSE),
                                    y=list(draw=FALSE)),
                      layout = c(3, 1),
                      xlab = "",
                      ylab = "",
                      main = paste0("Change in soy exposure to extreme \n degree days - ",scen.names[i]," scenario")) + 
    layer(sp.polygons(mask))
  
  png(paste0(fig.dir,paste0("Figure_EDD_panel",scen.names[i],".png")),width = 15,height = 7,units = "cm",res=360)
  print(f[[i]])
  dev.off()
  
}

panel <- ggarrange(plotlist=f,ncol=1,nrow=3)

png(paste0(fig.dir,paste0("Figure_EDD_panel.png")),width = 18,height = 20,units = "cm",res=360)
print(panel)
dev.off()

#----------------------------------------------------
#   Historical map panel =  
#       soy area, dLUC, dT, dEDD, CRV
#----------------------------------------------------

or <- colorRampPalette(colors = c("#FFFFFF","#ffd830","#ffa830","#d87848","#904860"))


param1 <- c("DT_NL","DT_L","NVloss","dEDD")
param.names1 <- c("Temperature change \n non-local effect (degreeC)",
                  "Temperature change \n local effect (degreeC)",
                  "Vegetation loss (%) \n",
                  "Change in exposure to \n extreme heat - EDD (degree days) \n")

param2 <- c("areasoyp","dy","CRVn","loss")
param.names2 <- c("Soy area \n (% of gridcell)", 
                  "Productivity loss due \n to extreme heat (%)",
                  "Lost soy revenue from ecosystem \n conversion in 2012 (2005$/ha)",
                  "Total lost soy revenue from ecosystem \n conversion in 2012 (10\U00B3 2005$)")


newtable1 <-  DT.his %>%
  right_join(finaltab.his) %>% 
  filter(Year == "X2012") %>%
  dplyr::select(Year,ID,param1) %>% mutate(NVloss = 100*NVloss)

newtable2 <-  DT.his %>%
  right_join(finaltab.his) %>% filter(areasoyp > 0.01) %>%
  filter(Year == "X2012") %>%
  mutate(dy = -100*dy,
         areasoyp = 100*areasoyp,
         loss = dy*Y0*soyarea*0.001) %>%
  dplyr::select(Year,ID,param2) 

shp <- left_join(CRshp, newtable1)

f1 <- list()
for (i in 1:length(param1))
{
  print(i)
  ncs <- rasterize(shp,nctemplate,param1[i])
  
  f1[[i]] <- levelplot(ncs, 
                       margin = FALSE,
                       col.regions=or(210),
                       names.attr=param1[i],
                       xlab = "",
                       ylab = "",
                       scales = list(x=list(draw=FALSE),
                                     y=list(draw=FALSE)),                     
                       colorkey=list(space="right"),
                       main = list(label=paste0(param.names1[i]),
                                   cex=0.8)) + 
    layer(sp.polygons(mask))
  
}



shp <- left_join(CRshp, newtable2)

f2 <- list()
for (i in 1:2)
{
  print(i)
  ncs <- rasterize(shp,nctemplate,param2[i])
  
  f2[[i]] <- levelplot(ncs, 
                       margin = FALSE,
                       col.regions=or(210),
                       names.attr=param2[i],
                       xlab = "",
                       ylab = "",
                       scales = list(x=list(draw=FALSE),
                                     y=list(draw=FALSE)),
                       colorkey=list(space="right"),
                       main = list(label=paste0(param.names2[i]),
                                   cex=0.95)) + 
    layer(sp.polygons(mask))
}

orn <- colorRampPalette(colors = c("#fff7fb","#d0d1e6",
                                   "#74a9cf","#3690c0","#0570b0","#045a8d",
                                   "#023858"))
bounds <- list(c(0,150),c(0,450))
for (i in 3:4)
{
  ncs <- rasterize(shp,nctemplate,param2[i])
  
  f2[[i]] <- levelplot(ncs, 
                       margin = FALSE,
                       col.regions=orn(210),
                       names.attr=param2[i],
                       xlab = "",
                       ylab = "",
                       at=c(-Inf,seq(bounds[[i-2]][1], bounds[[i-2]][2], len=201),Inf),
                       scales = list(x=list(draw=FALSE),
                                     y=list(draw=FALSE)),
                       colorkey=list(space="right"),
                       main = list(label=paste0(param.names2[i]),
                                   cex=0.90)) + 
    layer(sp.polygons(mask))
}

panel1 <- ggarrange(plotlist=f1,ncol=2,nrow=2,labels = "AUTO")
panel2 <- ggarrange(plotlist=f2,ncol=2,nrow=2,labels = "AUTO")

png(paste0(fig.dir,paste0("Figure_hist_panel1.png")),width = 20,height = 20,units = "cm",res=360)
print(panel1)
dev.off()

png(paste0(fig.dir,paste0("Figure_hist_panel2.png")),width = 20,height = 20,units = "cm",res=360)
print(panel2)
dev.off()

#----------------------------------------------------
#   Future CRV maps
#----------------------------------------------------


param <- c("areasoyp","NVloss","CRVl","CRVl2")
param.names <- c("Soy area (%)", "Vegetation loss (%)",
                 "Extreme heat regulation value - \n Biogeophysical (2005$)",
                 "Extreme heat regulation value - \n Biogeochemical + physical (2005$)")

newtable <-  DT.scen %>%
  right_join(finaltab.scen %>% mutate(Year = as.numeric(substr(Year,2,5)))) %>%
  filter(Year == 2050) %>%
  dplyr::select(ID,Scenario,NVloss,areasoyp,CRVl,CRVl2)


orn <- colorRampPalette(colors = c("#fff7fb","#ece7f2","#d0d1e6","#a6bddb",
                                   "#74a9cf","#3690c0","#0570b0","#045a8d",
                                   "#023858"))

panel <- list()
for (i in 1:length(scenarios))
{
  shp <- left_join(CRshp, newtable %>% filter(Scenario == scenarios[[i]]))
  f <- list()
  for (j in 1:2)
  {
    
    ncs <- rasterize(shp,nctemplate,param[j])
    
    f[[j]] <- levelplot(ncs,
                        margin = FALSE,
                        col.regions=or(210),
                        names.attr=param[j],
                        xlab = "",
                        ylab = "",
                        scales = list(x=list(draw=FALSE),
                                      y=list(draw=FALSE)),
                        main = list(label=paste0(param.names[j]),
                                    cex=0.85)) +
      layer(sp.polygons(mask))
  }
  
  for (j in 3:4)
  {
    
    ncs <- rasterize(shp,nctemplate,param[j])
    
    f[[j]] <- levelplot(ncs,
                        margin = FALSE,
                        col.regions=orn(210),
                        names.attr=param[j],
                        xlab = "",
                        ylab = "",
                        at=c(-Inf,seq(0, 1000, len=201),Inf),
                        scales = list(x=list(draw=FALSE),
                                      y=list(draw=FALSE)),
                        main = list(label=paste0(param.names[j]),
                                    cex=0.75)) +
      layer(sp.polygons(mask))
  }
  panel[[i]] <- annotate_figure(ggarrange(plotlist=f,nrow=2,ncol=2),
                                top = scen.names[i])
  png(paste0(fig.dir,paste0("Figure_scen_panel",scen.names[i],".png")),width = 15,height = 15,units = "cm",res=360)
  print(panel[[i]])
  dev.off()
}


#----------------------------------------------------
#   Historical summary table
#----------------------------------------------------

subpars <- c("NVloss","dy","areasoyp")
pars <- c("areasoyp","NVloss","DT_luc","dEDD","dy","DT_L","CRVn")
parnames <- c("Soy area (%)","Native Vegetation loss (%)","Temperature change (C)",
              "Change in EDDs (degree days)","Change in productivity (%)",
              "Extreme heat regulation value - Local effect (2005$ ha-1 yr-1)",              
              "Extreme heat regulation value (2005$ ha-1 yr-1)")

newtable <-  DT.his %>%
  right_join(finaltab.his) %>% left_join(biomes) %>% 
  filter(Year == "X2012",areasoyp>0.01) %>%
  mutate(DT_L = DT_L*CRVn/DT_luc) %>%
  dplyr::select(Year,ID,CD_Bioma,pars) %>% 
  pivot_longer(NVloss:CRVn,names_to="par",values_to="val") %>%
  group_by(Year,CD_Bioma,par) %>%
  summarise(meanv = mean(val, na.rm = TRUE), 
            sdv = sd(val, na.rm = TRUE),
            meana = mean(areasoyp, na.rm = TRUE)*100, 
            sda = sd(areasoyp, na.rm = TRUE)*100,
            wm = weighted.mean(val,areasoyp,na.rm=TRUE)) %>% ungroup %>% 
  dplyr::select(-Year) %>%
  mutate(CD_Bioma = plyr::mapvalues(CD_Bioma,c(1,3),c("Amazon","Cerrado"))) 

newtable2 <-  DT.his %>%
  right_join(finaltab.his) %>%
  filter(Year == "X2012",areasoyp>0.01) %>%
  mutate(DT_L = DT_L*CRVn/DT_luc) %>%
  dplyr::select(Year,ID,pars) %>%
  pivot_longer(NVloss:CRVn,names_to="par",values_to="val") %>%
  group_by(Year,par) %>%
  summarise(meanv = mean(val, na.rm = TRUE), 
            sdv = sd(val, na.rm = TRUE),
            meana = mean(areasoyp, na.rm = TRUE)*100, 
            sda = sd(areasoyp, na.rm = TRUE)*100,
            wm = weighted.mean(val,areasoyp,na.rm=TRUE)) %>% ungroup %>% 
  dplyr::select(-Year) %>%
  mutate(CD_Bioma = "Total")
newtable <- rbind(newtable,newtable2)

newtable[which(newtable$par %in% subpars),c("meanv","sdv","wm")] <- 
  100*newtable[which(newtable$par %in% subpars),c("meanv","sdv","wm")]

newtable <- newtable %>%
  mutate(Parameter = plyr::mapvalues(par,pars,parnames),
         Mean = paste0(round(meanv,digits = 2)," (",round(sdv,digits=2),")"),
         `Weighted Mean` = round(wm,digits = 2),
         `Soy area` = paste0(round(meana,digits = 2)," (",round(sda,digits=2),")")) %>% 
  dplyr::select(CD_Bioma,Parameter,Mean,`Weighted Mean`,`Soy area`) %>%
  pivot_wider(names_from = CD_Bioma,values_from = Mean:`Soy area`,
              names_glue = "{CD_Bioma} {.value}") 


write.csv(newtable,paste0(fig.dir,"Table_2_.csv"))

#----------------------------------------------------
#  CRV versus forest and soy area
#----------------------------------------------------

plottab <- finaltab.his

lims.i <- seq(0,95,by = 5)/100
lims.f <- seq(5,100,by = 5)/100

list1 <- data.frame(Mean_CRV = unlist(lapply(1:20, function(x) 
  (plottab %>% filter(Forarea > lims.i[x]) %>% filter(Forarea < lims.f[x]))[,"CRVn"] %>% mean(na.rm=TRUE))),
  Forest_area = seq(5,100,by = 5),Par = "Native vegetation area") %>%
  filter(Forest_area <100)

f3Ma <- annotate_figure(ggscatter(list1,"Forest_area","Mean_CRV",
                                  size = 2.5,
                                  color = "#FF5733",
                                  xlab = "Native vegetation area in gridcell (%)",
                                  ylab = "Extreme heat regulation value (2005$ ha-1 yr-1)") + 
                          font("xlab", size = 11)+
                          font("ylab", size = 11),
                        bottom = "") 


lims.i <- seq(0,95,by = 5)/100
lims.f <- seq(5,100,by = 5)/100

list2 <- data.frame(Mean_CRV = unlist(lapply(1:20, function(x) 
  (plottab %>% filter(areasoyp > lims.i[x]) %>% 
     filter(areasoyp < lims.f[x]))[,"CRV"] %>% mean(na.rm=TRUE))),
  Forest_area = seq(5,100,by = 5),
  Par = "Soy area",
  count = unlist(lapply(1:20, function(x) 
    nrow(plottab %>% filter(areasoyp > lims.i[x]) %>% 
           filter(areasoyp < lims.f[x]))))) %>%
  drop_na()

f3Mb <- annotate_figure(ggscatter(list2,"Forest_area","Mean_CRV",
                                  size = 2.5,
                                  color = "#FF5733",
                                  xlab = "Soy area in gridcell (%)",
                                  ylab = "Extreme heat regulation value (2005$ ha-1 yr-1)") + 
                          font("xlab", size = 11)+
                          font("ylab", size = 11),
                        bottom = "") 


f3M <- ggarrange(f3Ma,f3Mb)

png(paste0(fig.dir,"Figure_CRVvsforest_soy.png"),width = 20,height = 12,units = "cm",res=360)
print(f3M)
dev.off()



#----------------------------------------------------
#  Future CRV over time
#----------------------------------------------------


t.scen.a <- finaltab.scen %>% dplyr::select(ID,Year,Scenario,soyarea,CRVln) %>%
  rename(CRVfa = CRVln)
t.scen.b <- finaltab.scenb %>% dplyr::select(ID,Year,Scenario,Forarea,CRVln) %>%
  rename(CRVfb = CRVln)

plottablea <- t.scen.a %>% 
  filter(soyarea>0) %>%
  pivot_longer(CRVfa,names_to = "var",values_to = "value") %>%
  group_by(Year,Scenario,var) %>%
  summarize(Sum = 0.000001*sum(value*soyarea, na.rm = TRUE),
            Weightedm = weighted.mean(value,soyarea,na.rm=TRUE),
            area = mean(soyarea)) %>% ungroup() %>%
  mutate(Year = as.numeric(substr(Year,2,5))) %>%
  filter(Year > 2016) %>%
  pivot_longer(Sum:Weightedm, names_to = "Par",values_to="value") %>%
  mutate(Par = paste0(Par,var))


plottablea <- plottablea %>%
  mutate(Scenario = factor(Scenario,levels = c("IDC3","IDC3NoFC","IDC3ZD"),
                           labels = c("Baseline","No Forest Code","Zero Deforestation")),
         Par = factor(Par,levels = c("SumCRVfa","WeightedmCRVfa"),
                      labels = c("Total lost soy value \n from ecosystem conversion \n (10 \U2079 2005$/year)",
                                 "Lost soy value from \n ecosystem conversion \n (2005$/ha.year)")))

plottableb <- t.scen.b %>% 
  filter(Forarea>0) %>%
  pivot_longer(CRVfb,names_to = "var",values_to = "value") %>%
  group_by(Year,Scenario,var) %>%
  summarize(Sum = 0.000000001*sum(value*Forarea, na.rm = TRUE),
            Weightedm = weighted.mean(value,Forarea,na.rm=TRUE),
            area = mean(Forarea)) %>% ungroup() %>%
  mutate(Year = as.numeric(substr(Year,2,5))) %>%
  filter(Year > 2016) %>%
  pivot_longer(Sum:Weightedm, names_to = "Par",values_to="value") %>%
  mutate(Par = paste0(Par,var))


plottableb <- plottableb %>%
  mutate(Scenario = factor(Scenario,levels = c("IDC3","IDC3NoFC","IDC3ZD"),
                           labels = c("Baseline","No Forest Code","Zero Deforestation")),
         Par = factor(Par,levels = c("SumCRVfb","WeightedmCRVfb"),
                      labels = c("Total value of protected ecosystems \n for soy sector  (10 \U2079 2005$/year)",
                                 "Value of protected ecosystems \n for soy sector  (2005$/ha.year)")))



f5A <- ggplot(plottablea, 
              aes(x=Year, y=value, color=Scenario)) +
  geom_line(size = 1) + theme_pubr() + 
  scale_color_manual(limits= c("Baseline","No Forest Code","Zero Deforestation"),
                     labels = c("Baseline","No Forest Code","Zero Deforestation"),
                     name = "Land use scenario",
                     values=wes_palette("GrandBudapest1")[2:5]) +
  theme(legend.position="right") + 
  facet_wrap("Par",scales = "free",strip.position = 'left') +
  labs(x = "Year") +
  ggtitle("Lost soy value from ecosystem conversion") +
  theme(strip.text = element_text(size=12),
        axis.title = element_blank()) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

f5B <- ggplot(plottableb, 
              aes(x=Year, y=value, color=Scenario)) +
  geom_line(size = 1) + theme_pubr() + 
  scale_color_manual(limits= c("Baseline","No Forest Code","Zero Deforestation"),
                     labels = c("Baseline","No Forest Code","Zero Deforestation"),
                     name = "Land use scenario",
                     values=wes_palette("GrandBudapest1")[2:5]) +
  theme(legend.position="right") + 
  facet_wrap("Par",scales = "free",strip.position = 'left') +
  labs(x = "Year") +
  ggtitle("Value of protected ecosystems for soy") +
  theme(strip.text = element_text(size=12),
        axis.title = element_blank()) +
  theme(strip.background = element_blank(),
        strip.placement = "left")

newf5 <- ggarrange(f5A,f5B,common.legend = TRUE,legend = "bottom",nrow=2)

png(paste0(fig.dir,"Figure_future.png"),width = 20,height = 20,units = "cm",res=360)
print(newf5)
dev.off()


#----------------------------------------------------
#   Benchmarking - Historical + Future A
#----------------------------------------------------
npvh <- npvh.0.1[,-1] 
npvfa <- npvfa.0.1


# Land prices
lpdata <- lpindata %>% left_join(.,munic2cr, by = "Geocodigo") %>% 
  filter(lpscenario == "lp_hist",var == "Media_A_IGPDI",year == 2012) %>% 
  mutate(lp_hist = value) %>%
  dplyr::select(ID,area,lp_hist) %>% drop_na() 


# process - historical

npvh <- npvh %>% dplyr::select(ID,CRVl2_NPV) %>% 
  rename(npvh = CRVl2_NPV)
npvfa <- npvfa %>% 
  dplyr::select(ID,Scenario,CRVl2_NPV) %>% 
  rename(npvfa = CRVl2_NPV)

npv <- npvfa %>% left_join(npvh) %>% left_join(lpdata) 


npv <- npv %>%
  mutate(land_prices = lp_hist,
         perc_lp_h = 100*npvh/lp_hist,
         perc_lp_hfa = 100*npvfa/lp_hist,
         perc_lp_t = 100*(npvfa+npvh)/lp_hist)

npv1 <- npv
# Merge IDC3 scenario with shapefile for plotting
npvaggshp <- npv %>% filter(Scenario == "IDC3") %>% 
  dplyr::select(ID,land_prices,perc_lp_h,perc_lp_hfa,perc_lp_t) %>% 
  unique() %>%
  left_join(baseshp, by = "ID") %>%
  st_as_sf()


scenarios <-  c("land_prices","perc_lp_h","perc_lp_hfa","perc_lp_t") 
scen.names <- c("Agricultural land prices (2005$)",
                "Historical extreme heat reg. value \n vs. land prices",
                "Future extreme heat reg. value \n vs. land prices",
                "Historical + future extreme heat reg. value \n vs. land prices")

bounds <- list(c(0,6000),c(0,150),c(0,150),c(0,150))

f <- list()
for (i in 1:length(scenarios))
{
  ncs <- rasterize(npvaggshp,nctemplate,scenarios[i])
  
  f[[i]] <- levelplot(ncs, 
                      margin = FALSE,
                      col.regions=rev(magma(22)),                   
                      xlab = "",
                      ylab = "",
                      at=c(-Inf,seq(bounds[[i]][1], bounds[[i]][2], len=20),Inf),
                      scales = list(x=list(draw=FALSE),
                                    y=list(draw=FALSE)),
                      main = scen.names[i]) + 
    layer(sp.polygons(mask))
}

panel <- ggarrange(plotlist=f,ncol=2,nrow=2)


png(paste0(fig.dir,"Figura_Benchmarking_h_fa.png"),width = 20,height = 20,units = "cm",res=360)
print(panel)
dev.off()


#----------------------------------------------------
#   future NPV - table
#----------------------------------------------------
npvfa <- npvfa.0.1 %>% rename(npvfa = CRVl2_NPV) %>% dplyr::select(ID,Scenario,npvfa,soyarea)
npvfb <- npvfb.0.1 %>% rename(npvfb = CRVl2_NPV) %>% dplyr::select(ID,Scenario,npvfb,Forarea)

npv1 <- npvfa %>%
  group_by(Scenario) %>%
  summarize(mean = mean(npvfa,na.rm=TRUE),
            weighted.mean = weighted.mean(npvfa,soyarea,na.rm=TRUE)) %>% 
  ungroup %>%
  mutate(Scenario = factor(Scenario,levels = c("IDC3","IDC3NoFC","IDC3ZD"),
                           labels = c("Baseline","No Forest Code","Zero Deforestation")))
npv2 <- npvfb %>%
  group_by(Scenario) %>%
  summarize(mean = mean(npvfb,na.rm=TRUE),
            weighted.mean = weighted.mean(npvfb,Forarea,na.rm=TRUE)) %>% 
  ungroup %>%
  mutate(Scenario = factor(Scenario,levels = c("IDC3","IDC3NoFC","IDC3ZD"),
                           labels = c("Baseline","No Forest Code","Zero Deforestation")))

write.csv(npv,paste0(fig.dir,"tablenpv01.csv"))


npvfa <- npvfa.0.03 %>% rename(npvfa = CRVl2_NPV) %>% dplyr::select(ID,Scenario,npvfa,soyarea)
npvfb <- npvfb.0.03 %>% rename(npvfb = CRVl2_NPV) %>% dplyr::select(ID,Scenario,npvfb,soyarea)

npv <- left_join(npvfa,npvfb) %>%
  pivot_longer(c(npvfa,npvfb),names_to= "par",values_to="values") %>%
  group_by(Scenario,par) %>%
  summarize(mean = mean(values,na.rm=TRUE),
            weighted.mean = weighted.mean(values,soyarea,na.rm=TRUE)) %>% 
  ungroup %>%
  mutate(Scenario = factor(Scenario,levels = c("IDC3","IDC3NoFC","IDC3ZD"),
                           labels = c("Baseline","No Forest Code","Zero Deforestation")))
write.csv(npv,paste0(fig.dir,"tablenpv003.csv"))
#----------------------------------------------------
#   Benchmarking - Future B
#----------------------------------------------------
#2 should have forest land prices, 
#heat reg/forest land price, 
#heat reg/carbon price,  
#and (CRV+carbon)/ forest land price

npvfb <- npvfb.0.1


# Land prices
lpdata <- lpindata %>% left_join(.,munic2cr, by = "Geocodigo") %>% 
  filter(lpscenario == "lp_hist",var == "Media_N_IGPDI",year == 2012) %>% 
  mutate(lp_hist = value) %>%
  dplyr::select(ID,lp_hist) %>% drop_na() 


# Carbon prices
cpdata <- cpindata %>%
  filter(agbscenario == "agb_hist", var == "cvalueha",
         year == 2014) %>% 
  mutate(agb_hist = value) %>%
  dplyr::select(-year,-value,-agbscenario,-var) %>% drop_na() 


npvfb <- npvfb %>% 
  dplyr::select(ID,Scenario,CRVl2_NPV) %>% 
  rename(npvfb = CRVl2_NPV)

npv <- npvfb %>% left_join(cpdata) %>% left_join(lpdata) 

npv <- npv %>% 
  mutate(land_prices = lp_hist,
         carbon_prices = agb_hist,
         crv = npvfb,
         crv_cp = npvfb+agb_hist)
npv2 <- npv

# Merge IDC3 scenario with shapefile for plotting
npvaggshp <- npv %>% filter(Scenario == "IDC3") %>% 
  dplyr::select(ID,Scenario,land_prices,carbon_prices,crv,crv_cp) %>% 
  unique() %>%
  left_join(baseshp, by = "ID") %>%
  st_as_sf()


scenarios <-  c("land_prices","carbon_prices","crv","crv_cp") 
scen.names <- c("Native vegetation land price ($2005/ha)",
                "Carbon price ($2005/ha)",
                "Value of protected ecosystems \n for soy sector  ($2005/ha)",
                "Value of protected ecosystems for \n  soy sector + carbon price ($2005/ha)")
#bounds <- list(c(0,4000),c(0,150),c(0,150),c(0,150))



f <- list()
for (i in 1:length(scenarios))
{
  ncs <- rasterize(npvaggshp,nctemplate,scenarios[i])
  
  f[[i]] <- levelplot(ncs, 
                      margin = FALSE,
                      col.regions=rev(magma(22)),                   
                      at=c(-Inf,seq(0, 2000, len=20),Inf),
                      xlab = "",
                      ylab = "",
                      scales = list(x=list(draw=FALSE),
                                    y=list(draw=FALSE)),
                      main = scen.names[i]) + 
    layer(sp.polygons(mask))
}

panel <- ggarrange(plotlist=f,ncol=2,nrow=2,labels="AUTO")


png(paste0(fig.dir,"Figure_Benchmarking_fb.png"),width = 20,height = 20,units = "cm",res=360)
print(panel)
dev.off()

#----------------------------------------------------
#   statistics Benchmarking
#----------------------------------------------------
npv1.1 <- npv1 %>% group_by(Scenario) %>%
  summarise(count_lp_h = 100*mean(as.numeric(perc_lp_h>100),na.rm=TRUE),
            count_lp_hfa= 100*mean(as.numeric(perc_lp_hfa>100),na.rm=TRUE),
            count_lp_t = 100*mean(as.numeric(perc_lp_t>100),na.rm=TRUE),
            mean_lp_h = mean(perc_lp_h,na.rm=TRUE),
            mean_lp_hfa= mean(perc_lp_hfa,na.rm=TRUE),
            mean_lp_t = mean(perc_lp_t,na.rm=TRUE)) %>%
  ungroup() %>% mutate(across(2:7, round, 0)) %>%
  as.data.frame() 

npv2.1 <- npv2 %>% group_by(Scenario) %>%
  summarise(count_fb_lp = 100*mean(as.numeric(perc_fb_lp>100),na.rm=TRUE),
            count_fb_cp = 100*mean(as.numeric(perc_fb_cp>100),na.rm=TRUE),
            count_fbcp_lp = 100*mean(as.numeric(perc_fbcp_lp>100),na.rm=TRUE),
            mean_fb_lp = mean(perc_fb_lp,na.rm=TRUE),
            mean_fb_cp = mean(perc_fb_cp,na.rm=TRUE),
            mean_fbcp_lp = mean(perc_fbcp_lp,na.rm=TRUE)) %>%
  ungroup() %>% mutate(across(2:7, round, 0)) %>%
  as.data.frame() 


write.csv(npv1.1,paste0(fig.dir,"Table_Benchmarking.csv"), row.names = F)

write.csv(npv2.1,paste0(fig.dir,"Table_Benchmarking2.csv"), row.names = F)


