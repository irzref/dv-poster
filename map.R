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

# helper : create the csv file and manually assign the name of province the same as in shape file
# write.csv(id_province,file="id_province.csv",row.names=FALSE)

idn_shape_df$province <- with(id_province, Province[match(idn_shape_df$id, id)])

# add the commodity volume sum to idn_shape_df, draw the density !!!
idn_shape_df$export.vol <- with(all_commodities_by_province_edited, Sum.New[match(idn_shape_df$province, Province.New)])

# need this to remove background and outline of graph
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# plot the map
# add label for each time zone and west,middle,east !!!

map_export_volume <- ggplot(data = idn_shape_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=export.vol), data=idn_shape_df, color='white') +
  guides(fill=guide_legend(title="Export Volume")) +
  geom_polygon(color = "white", fill = NA) +
  geom_polygon(data = idn_tz_shape_df[idn_tz_shape_df$group!=1.2,], aes(x=long, y = lat, group = group), colour="red", fill=NA) +
  theme_bw() +
  ggtitle("Export Volume of Fishery Commodities 2001 - 2012 by Export Origin") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=105, y=-13, label= "UTC+07:00 WESTERN") + 
  annotate("text", x=120, y=-13, label= "UTC+08:00 CENTRAL") +
  annotate("text", x=133, y=-13, label= "UTC+09:00 EASTERN") +
  ditch_the_axes

# map_export_volume
map_export_volume + scale_fill_gradient(trans = "log10")



