# heatmap of map

setwd("/home/irza/Projects/dv-poster")

library(ggplot2)
library("rgdal")



# indonesia map
idn_shape <- readOGR(dsn = "indo_shp", layer="INDONESIA_PROP")
idn_shape_df <- fortify(idn_shape)

ggplot() +
  geom_polygon(data = idn_shape_df, aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3)

# time zone indonesia 
idn_tz_shape <- readOGR(dsn = "indo_tz_shp", layer="combined-shapefile")
idn_tz_shape_df <- fortify(idn_tz_shape)

ggplot() +
  geom_polygon(data = idn_tz_shape_df[idn_tz_shape_df$group!=1.2,], aes(x=long, y = lat, group = group), colour="black", fill=NA) +
  coord_fixed(1.3)

# combined map and time zone
ggplot() +
  geom_polygon(data = idn_shape_df, aes(x=long, y = lat, group = group)) + 
  geom_polygon(data = idn_tz_shape_df[idn_tz_shape_df$group!=1.2,], aes(x=long, y = lat, group = group), colour="blue", fill=NA) +
  coord_fixed(1.3)

# colored map
ggplot(data = idn_shape_df) + 
  geom_polygon(aes(x = long, y = lat, fill = id, group = group), color = "white") + 
  geom_polygon(data = idn_tz_shape_df[idn_tz_shape_df$group!=1.2,], aes(x=long, y = lat, group = group), colour="blue", fill=NA) +
  coord_fixed(1.3) +
  guides(fill=FALSE) 


# add province in the shape file dataframe
unique(idn_shape_df[c("id")])

id_province = data.frame(idn_shape$ID, idn_shape$Propinsi)
colnames(id_province) <- c("ID","Province")
id_province$id <- paste(seq.int(nrow(id_province))-1)
dim(id_province)
write.csv(id_province,file="id_province.csv",row.names=FALSE)

idn_shape_df$province <- with(id_province, Province[match(idn_shape_df$id, id)])

# add the commodity volume avg to idn_shape_df, draw the density !!!


