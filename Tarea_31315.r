#cargar librerias
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
library(readxl)
library(vctrs)
library(Kendall)
library(raster)
library(ggrepel)
options(scipen = 999)

#path = "C:/Users/alanp/Documents/5to/cs datos espaciales/tarea2" #aqui poner el path de la carpeta para correr todo sin cambiar a cada rato
path = "/Users/itallo/Documents/GitHub/Tarea2_final"
# Cargar funciones --------------------------------------------------------
#Leemos el raster y lo cortamos al area de interes
km = read_sf(paste0(path, "/cuenca.kml"))
km = mutate(km, Description = "Rio Aconcagua",altura = 1021)
write_sf(km, paste0(path, "/cuenca.geojson"))
img.folder = paste0(path, "/landsat")
files = list.files(img.folder, pattern = "SR_B", full.names = TRUE)
imgs = rast(files)
imgs

img_ext = ext(imgs)
img_ext

img_ext[2]-img_ext[1]#medimos ancho y largo de la imagen
img_ext[4]-img_ext[3]
st_crs(imgs) == st_crs(km) 

img.crs = st_crs(imgs)
img.crs

v = st_transform(x = km, crs = img.crs)

st_crs(imgs) == st_crs(v)#verificamos que se tenga el mismo crs
imgs.c = crop(imgs, vect(v))
plot(imgs.c)

imgs.m = mask(imgs, vect(v))#creamos mascara de la cuenca
plot(imgs.m)

imgs.cm = crop(imgs.m, vect(v))
plot(imgs.cm)

imgs.r = project(imgs.cm, vect(km), method = "near")
cuenca = imgs.r[[5]]#porque 5???
plot(cuenca)

#Importamos el LandCover Zhao
lc.file = paste0(path, "/LC_CHILE_2014_b.tif")
lc = rast(lc.file)

#Cortamos el Lancover al area de nuestra cuenca
lc.proj = project(lc, cuenca, method = "bilinear") #Metodo bilinear para datos categoricos
lc.mask = mask(lc.proj, cuenca)
lc.crop = crop(lc.mask, cuenca)
plot(lc.crop)

'CR2MET2.5'

# carpeta donde se encuentran los archivos de PP CR2MET
pp.dir = paste0(path, "/PPCR2MET2.5") #directorio datos de precipitación

files = list.files(pp.dir, full.names = TRUE, pattern = "nc$");files
pp = rast(files,subds = "pr")
pp    #datos de CR2MET almacenados en pp

# crear vector de fechas
fechas.pp = seq(
  ymd("1995-01-01"),
  ymd("2015-12-31"),
  by = "days"
)
# asignamos las fechas como nombre de las capas
names(pp) = fechas.pp

# extraer valores dentro de la cuenca
extr = terra::extract(pp, km)
as_tibble(extr)

# calcular precipitacion promedio de la cuenca
pp.day = extr %>%
  select(-ID) %>% 
  drop_na() %>% 
  summarise_all(mean) %>% 
  pivot_longer(cols = 1:ncol(.), names_to = "fecha", values_to = "pp")
pp.day

# PP mensual
pp.month = pp.day %>% 
  mutate(fecha = as_date(fecha),
         fecha = floor_date(fecha, unit = "month")) %>% 
  group_by(fecha) %>% 
  summarise(pp = sum(pp))
pp.month




'MODIS'
#Importamos los datos modis del area y les ponemos el formato que necesitamos
dir=paste0(path, "/modis")

files = list.files(dir, full.names = TRUE, pattern = "_ET_500");files#datos evotranspiracion

et = rast(files)
fechas.et = names(et) %>% 
  str_sub(start = 26, end = 32) %>%
  as.Date("%Y%j")
names(et) = fechas.et

hist(et[[1]], breaks = 20)
summary(et[[1]])

q90 = quantile(et, probs = 0.9)
plot(q90)

cortes = seq(0,50,10);cortes

plot(et, breaks=cortes)
plot(et[[1]], breaks=cortes, main = "ET MODIS 2000-01-01")

#Obtenemos las imagenes mensuales a partir de diarias
daily_to_monthly = function(x, dates, fun = "mean"){
  mes = floor_date(as_date(dates), unit = "month")
  lista.meses = mes %>% unique
  n = length(lista.meses)
  x.mensual = rast()
  
  for (i in 1:n) {
    posicion = grep(lista.meses[i], mes)
    if (fun == "mean"){
      r = mean(x[[posicion]], na.rm = TRUE)
    }else if(fun == "sum"){
      r = sum(x[[posicion]], na.rm = TRUE)
    }else{
      message("No se reconoce la funcion utilizada, escriba: fun = sum o mean como argumento")
    }
    x.mensual = c(x.mensual, r)
  }
  names(x.mensual) = lista.meses
  
  return(x.mensual)
}

#Obtenemos las imagenes anuales a partir de mensuales
to_yearly = function(x, dates, fun = "mean"){
  y = floor_date(as_date(dates), unit = "year")
  lista.y = y %>% unique
  n = length(lista.y)
  x.anual = rast()
  
  for (i in 1:n) {
    posicion = grep(lista.y[i], y)
    if (fun == "mean"){
      r = mean(x[[posicion]], na.rm = TRUE)
    }else if(fun == "sum"){
      r = sum(x[[posicion]], na.rm = TRUE)
    }else{
      message("No se reconoce la funcion utilizada, escriba: fun = sum o mean como argumento")
    }
    x.anual = c(x.anual, r)
  }
  names(x.anual) = lista.y

  return(x.anual)
}

et.m = daily_to_monthly(et, dates = fechas.et, fun = "sum")
et.m
et.y = to_yearly(et.m, dates = names(et.m), fun = "sum")
et.y
et

#Limpiamos y ordenamos los datos
cat.id = values(lc.crop) %>% unique();cat.id
cat.id = cat.id[-1];cat.id
cat.id = sort(cat.id);cat.id

fac = res(et)[1]/res(lc)[1];fac
lc.agg = aggregate(lc.crop, fact = fac, fun = "modal")

plot(lc.crop, main = "LandCover resolucion original")
plot(lc.agg, main = "LandCover de baja resolucion")

# Obtenenemos la proyecci?n del raster de origen
lc.proj <- crs(lc.agg)

# Proyectamos el raster de destino
et.proj <- project(et, lc.proj)

# Resampleamos el raster lc.agg utilizando 'resample()' y lo ajustamos
lc.r <- resample(lc.agg, et.proj, method = "bilinear")
lc.r = crop(lc.r, cuenca)
plot(lc.r, main = "LandCover resampleado")

rcl = c(100,150,1,
        210,232,2,
        241,252,3,
        310,330,4,
        410,450,5,
        500,1210,6);rcl
rcl.mat = matrix(rcl, ncol = 3, byrow = TRUE);rcl.mat
lc.cats = tibble(ID = 1:6,
                 nombre = c("Cultivos","Bosque Nativo","Plantaciones","Praderas","Matorrales","Otros"))
lc.cats
lc.reclass = classify(lc.r, rcl.mat, right=NA)
levels(lc.reclass) = lc.cats

plot(lc.reclass, main = "LandCover Reclasificado", col = c("yellow","purple","red","blue","green", "white"))

#reproyectamos lo rasters para que tengan el mismo extend
lc.reclass.reprojected <- project(lc.reclass, crs(et))

lc.reclass.resampled <- resample(lc.reclass.reprojected, et.cropped)

et.cropped <- crop(et.cropped, lc.reclass.resampled)

est.zonal <- zonal(et.cropped, lc.reclass.resampled, fun = "mean", na.rm = TRUE) %>% as_tibble;est.zonal
est.zonal = est.zonal %>% pivot_longer(cols = 2:265, names_to = "fecha", values_to = "ET");est.zonal

hist(est.zonal$ET)
summary(est.zonal$ET)
quantile(est.zonal$ET, 0.8, na.rm = TRUE)

# filtramos categorias de interes
zonal.df = est.zonal %>%
  rename(ID = 1) %>% 
  mutate(fecha = as_date(fecha),
         ID = factor(ID))
zonal.df

ggplot(zonal.df %>% filter(ID != "Otros"))+
  geom_line(aes(x = fecha, y = ET, color = ID), linewidth = 1) +
  scale_x_date(limits = c(ymd("2002-12-27"), ymd("2016-6-31"))) +
  labs(x = "Fecha",y = "Evapotranspiraci?n real (mm)", 
       title = "Evapotranspiraci?n mensual por categoria de cobertura de suelo",
       color = "Land Cover")

'LANDCOVER ZHAO'
#Proyectamos las categorias solicitadas en nuestro raster segun el LandCover Zhao
#Consideramos solo las categorias de cultivos(100), bosques(200), plantaciones(250), praderas(300) y matorrales(400)

#Modificamos pixeles de un raster seleccionaremos todos los pixeles diferentes a 100, 200, 300 y 400 y les asiga NA
#Tambien asignamos los pixeles especificos de cada seccion del LandCover a solo uno por categoria general
lc.crop[lc.crop >= 500] = NA 
lc.crop[lc.crop < 100] = NA
lc.crop[lc.crop < 200 & lc.crop >=100] = 1
lc.crop[lc.crop < 235 & lc.crop >=200] = 2
lc.crop[lc.crop < 300 & lc.crop >=235] = 3
lc.crop[lc.crop < 400 & lc.crop >=300] = 4
lc.crop[lc.crop < 500 & lc.crop >=400] = 5

writeRaster(lc.crop, paste0(path, "/"), overwrite = TRUE)

plot(lc.crop, col = c("yellow","purple","red","blue","green"), 
     main = "Landcover por categorias") #Ploteamos lc cortado, cambiamos los colores a las categorias

lc.crop_vec <- as.vector(lc.crop) #Convertimos lc.crop en un vector

color_counts <- table(lc.crop_vec) #Obtenenemos la tabla de cantidades de las categor?as de color

# Creamos un gr?fico de barras de la cantidad por categoria
text(x = barplot(table(lc.crop_vec), col = c("yellow","purple","red","blue","green"),
                 main = "Cantidad por categoria",
                 xlab = "Categoria", ylab = "Cantidad",
                 ylim= c(0,6500000)),
     y = table(lc.crop_vec),
     labels = table(lc.crop_vec),
     pos = 3)

#caudal cr2

q.month = read_csv(paste0(path, "/Caudal_Aconcagua.csv")) %>% 
  pivot_longer(cols = 2:13, names_to = "mes",values_to = "caudal") # pivotear columnas
q.month

# vector de fechas mensuales
fechas = seq(ym("1995-01"), ym("2015-12"), by = "months")

# crear columna con fechas
q.month = q.month %>% 
  mutate(fecha = fechas) %>% 
  select(fecha, caudal)
q.month

# calcular caudal medio anual
q.year = q.month %>% 
  mutate(fecha = floor_date(fecha,unit = "year")) %>% 
  group_by(fecha) %>% 
  summarise_all(mean)
q.year

