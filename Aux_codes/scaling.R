################################################
#   Title: WD scale analysis
#   Date
#   Authors: Rafaela Flach
#
################################################

Packages <- c("dplyr","tidyverse","RColorBrewer","stringr",
              "ggthemes","ggplot2","ggpubr","formatR","ggExtra",
              "gdata","readxl","ggridges","Rcpp","data.table",
              "sf","ncdf4","raster","fasterize","tmap",
              "ggspatial","classInt","data.table","exactextractr",
              "rasterVis","rgdal","viridis","reticulate","wesanderson")
lapply(Packages, library, character.only = TRUE)

in.dir <- "Downscaling_analysis/"

shfiles <- list.files(paste0(in.dir,"FOREST_TILES_FIX_2/"),pattern = ".shp")

shpoints <- lapply(shfiles, function(x) st_read(paste0(in.dir,"FOREST_TILES_FIX_2/",x)))


shpoints <- do.call(rbind,shpoints)

summaries <- shpoints %>% as_tibble() %>%
  dplyr::select(car_code,bufsize,CD_Bioma,R30,R500,R1000,R5000,R10000,R50000) %>% 
  pivot_longer(R30:R50000, names_to = "Resolution",values_to = "Val") %>%
  drop_na() %>%
  pivot_wider(names_from = bufsize,values_from = Val) %>%
  mutate(`1` = `1000`,
         `1-2` = (`2000`*pi*2000^2 - `1000`*pi*1000^2)/(pi*2000^2 - pi*1000^2),
         `2-4` = (`4000`*pi*4000^2 - `2000`*pi*2000^2)/(pi*4000^2 - pi*2000^2),
         `4-10` = (`10000`*pi*10000^2 - `4000`*pi*4000^2)/(pi*10000^2 - pi*4000^2),
         `10-50` = (`50000`*pi*50000^2 - `10000`*pi*10000^2)/(pi*50000^2 - pi*10000^2)) %>%
  dplyr::select(car_code,CD_Bioma,Resolution,`1`,`1-2`,`2-4`,`4-10`,`10-50`) %>%
  pivot_longer(`1`:`10-50`,values_to = "Val",names_to = "bufsize") %>%
  group_by(bufsize,Resolution,CD_Bioma) %>%
  summarise(Mean = mean(Val),
            Median = median(Val),
            SD = sd(Val),
            N = n()) %>% 
  ungroup() %>%
  mutate(Resolution = factor(x = Resolution,
                             levels = c("R30","R500","R1000","R5000","R10000","R50000"),
                             labels = c("30 m","500 m","1000 m","5000 m","10000 m","50000 m"),
                             ordered = TRUE),
         bufsize = factor(x = bufsize,
                          levels = c("1","1-2","2-4","4-10","10-50"),
                          labels = c("1","1-2","2-4","4-10","10-50")),
         CD_Bioma = plyr::mapvalues(CD_Bioma, from = c(1,3),to = c("Amazon","Cerrado"))) %>%
  rename(Buffer_size = bufsize) %>% arrange(Resolution)

summaries2 <- shpoints %>% as_tibble() %>%
  dplyr::select(car_code,bufsize,CD_Bioma,R30,R500,R1000,R5000,R10000,R50000) %>% 
  pivot_longer(R30:R50000, names_to = "Resolution",values_to = "Val") %>%
  drop_na() %>%
  pivot_wider(names_from = bufsize,values_from = Val) %>%
  mutate(`1` = `1000`,
         `1-2` = (`2000`*pi*2000^2 - `1000`*pi*1000^2)/(pi*2000^2 - pi*1000^2),
         `2-4` = (`4000`*pi*4000^2 - `2000`*pi*2000^2)/(pi*4000^2 - pi*2000^2),
         `4-10` = (`10000`*pi*10000^2 - `4000`*pi*4000^2)/(pi*10000^2 - pi*4000^2),
         `10-50` = (`50000`*pi*50000^2 - `10000`*pi*10000^2)/(pi*50000^2 - pi*10000^2)) %>%
  dplyr::select(car_code,CD_Bioma,Resolution,`1`,`1-2`,`2-4`,`4-10`,`10-50`) %>%
  pivot_longer(`1`:`10-50`,values_to = "Val",names_to = "bufsize") %>%
  mutate(Resolution = factor(x = Resolution,
                             levels = c("R30","R500","R1000","R5000","R10000","R50000"),
                             labels = c("30 m","500 m","1000 m","5000 m","10000 m","50000 m"),
                             ordered = TRUE),
         bufsize = factor(x = bufsize,
                             levels = c("1","1-2","2-4","4-10","10-50"),
                             labels = c("1","1-2","2-4","4-10","10-50"),
                             ordered = TRUE),
         CD_Bioma = plyr::mapvalues(CD_Bioma, from = c(1,3),to = c("Amazon","Cerrado")))

newpal <- colorRampPalette(wes_palette("Zissou1"))

plot1 <- ggline(summaries,"Buffer_size","Mean",
                color = "Resolution",
                plot_type = "l",
                size = 1.2,
                facet.by = "CD_Bioma",
                xlab = "Halo radii (km)",
                ylab = "Conversion fraction (mean)",
                palette = rev(newpal(6)),
                point.size = 0)

test2 <- ggerrorplot(summaries2, x = "bufsize", y = "Val", 
                     desc_stat = "mean_sd", 
                     size = 0.7,
                     color = "Resolution", 
                     palette = rev(newpal(6)),
                     facet.by = "CD_Bioma",
                     xlab = "Halo radii (km)",
                     ylab = "Conversion fraction (mean+sd)",
                     position = position_dodge(0.6))


png(paste0(in.dir,"scale_res_lineplot.png"),width = 25,height = 12,units = "cm",res=360)
print(plot1)
dev.off()

png(paste0(in.dir,"scale_res_errorplot.png"),width = 25,height = 12,units = "cm",res=360)
print(test2)
dev.off()






## ===============================================

#     Recalculate Coefficient

## ===============================================

coef.c <- read.csv(paste0(in.dir,"coef_pp.csv"),sep = ";") %>%
  mutate(Buffer_size = str_remove(Buffer_size,"km"))


scaling.f <- summaries %>%
  filter(Resolution %in% c("30 m","50000 m"),
         Buffer_size != 1) %>%
  dplyr::select(Buffer_size,Resolution,CD_Bioma,Mean) %>%
  pivot_wider(names_from = Resolution,values_from=Mean) %>%
  mutate(scalar = `30 m`/`50000 m`) %>%
  dplyr::select(Buffer_size,CD_Bioma,scalar) %>%
  pivot_wider(names_from = CD_Bioma,values_from = scalar) %>%
  mutate(Buffer_size = factor(x = Buffer_size,
         levels = c("1-2","2-4","4-10","10-50"),
         labels = c("1-2","2-4","4-10","10-50"), ordered = TRUE)) %>%
  arrange(Buffer_size)

final_coef <- coef.c %>%
  dplyr::select(Buffer_size,Coef.) %>%
  right_join(scaling.f) %>%
  mutate(Amazon_scaled = Amazon*Coef.,
         Cerrado_scaled = Cerrado*Coef.)



## ===============================================

#     Plot coefficients

## ===============================================


coef.pp <- read.csv(paste0(in.dir,"coef_pp.csv"),sep = ";") %>%
  mutate(Buffer_size = str_remove(Buffer_size,"km")) %>%
  `colnames<-`(c("Halo_Radii","Coefficient","SD","t","P","Lower_interval","Higher_interval")) %>%
  mutate(Halo_Radii = factor(Halo_Radii,levels = c("1-2","2-4","4-10","10-50"),
                             labels = c("1-2","2-4","4-10","10-50"),ordered = TRUE))


coef.km <- read.csv(paste0(in.dir,"coef_km.csv"),sep = ";") %>%
  mutate(atmax = str_remove_all(atmax,"_area_km|f_"),
         atmax = str_replace(atmax,"to","-")) %>%
  `colnames<-`(c("Halo_Radii","Coefficient","SD","t","P","Lower_interval","Higher_interval")) %>%
  mutate(Halo_Radii = factor(Halo_Radii,levels = c("1-2","2-4","4-10","10-50"),
                           labels = c("1-2","2-4","4-10","10-50"),ordered = TRUE))


p1 <- ggplot(coef.pp, aes(x=Halo_Radii, y= Coefficient)) + 
  geom_linerange(aes(ymin=Coefficient-SD, ymax=Coefficient+SD),
                  size=0.9, color="#25476c") + 
  geom_point(color="#25476c",size=2.2) +
  ylab("\u0394AT for a 100 p.p. ecosystem conversion (\u00B0 C) ") + 
  xlab("Halo radii") +
  theme(axis.text=element_text(size=14),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=12)) +
  theme_bw()

p2 <- ggplot(coef.km, aes(x=Halo_Radii, y= Coefficient)) + 
  geom_linerange(aes(ymin=Coefficient-SD, ymax=Coefficient+SD),
                 size=0.9, color="#25476c") + 
  geom_point(color="#25476c",size=2.2) +
  ylab("\u0394AT per km\u00B2 ecosystem conversion (\u00B0 C) ") + 
  xlab("Halo radii") +
  theme(axis.text=element_text(size=14),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=12)) +
  theme_bw()

coefs <- ggarrange(p2,p1,ncol=2)

png(paste0(in.dir,"Coefficients.png"),width = 20,height = 9,units = "cm",res=360)
print(coefs)
dev.off()