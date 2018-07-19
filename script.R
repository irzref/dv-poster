setwd("/home/irza/Projects/dv-poster")

library("ggplot2")
library("rgdal")
library(dplyr)

# dataset
exprt_origin <- read.csv("asalprovinsiekspor.csv")
exprt_destination <- read.csv("eksporperikanannasional.csv")



# dataset analysis

# export origin
unique(exprt_origin[c("NamaKomoditi", "KomoditiID")])
unique(exprt_origin[c("Wilayah")])
dim(unique(exprt_origin[c("ProvinsiID")]))
unique(exprt_origin[c("NamaProvinsi")])
dim(unique(exprt_origin[c("NamaProvinsi")]))
table(exprt_origin$NamaProvinsi, exprt_origin$NamaKomoditi) 
#View(exprt_origin[exprt_origin$KomoditiID==0,])

# export destination
unique(exprt_destination[c("NamaKomoditi", "KomoditiID")])
table(exprt_destination$Negara, exprt_destination$NamaKomoditi)
#View(exprt_destination[exprt_destination$KomoditiID==0 & exprt_destination$NegaraID!=31,])



# diagram 1
# comparison of fishery development between western part and middle and eastern part aggregated by each province (map)

# remove the national data, get only data for each province
all_commodities <- exprt_origin[exprt_origin$ProvinsiID!=0 & exprt_origin$KomoditiID==0,]
#View(all_commodities)

# aggregate the sum for each province througout the years
all_commodities_by_province <- aggregate(all_commodities$Volume, by=list(Region=all_commodities$Wilayah,Province=all_commodities$NamaProvinsi),FUN=sum)
#View(all_commodities_by_province)

# helper : create the csv file and manually assign the name of province the same as in shape file
# write.csv(all_commodities_by_province,file="all_commodities_by_province.csv",row.names=FALSE)

# load the manually edited all_commodities_by_province !!!
all_commodities_by_province_edited <- read.csv("all_commodities_by_province.csv")

# create new all_commodities_by_province !!!
all_commodities$NamaProvinsiNew <- all_commodities$NamaProvinsi
all_commodities$NamaProvinsiNew[all_commodities$NamaProvinsiNew=='Kepulauan Riau'] <- 'Riau'
all_commodities_by_province <- aggregate(all_commodities$Volume, by=list(Region=all_commodities$Wilayah,Province=all_commodities$NamaProvinsiNew),FUN=sum)

# merge new all_commodities_by_province to edited one !!!
all_commodities_by_province_edited$Sum.New <- with(all_commodities_by_province, x[match(all_commodities_by_province_edited$Province, Province)])

# remove row with empty value in Province.New ('Sulawesi Barat') !!!
# remove row with empty value in Sum.New ('Kepulauan Riau') !!!
all_commodities_by_province_edited <- all_commodities_by_province_edited[rowSums(is.na(all_commodities_by_province_edited)) == 0 & all_commodities_by_province_edited$Province.New!="",]

# indonesia map must be converted to heat map !!!
# indonesia map
idn_shape <- readOGR(dsn = "indo_shp", layer="INDONESIA_PROP")
idn_shape_df <- fortify(idn_shape)

# time zone indonesia 
idn_tz_shape <- readOGR(dsn = "indo_tz_shp", layer="combined-shapefile")
idn_tz_shape_df <- fortify(idn_tz_shape)

# add province in the shape file dataframe
id_province = data.frame(idn_shape$ID, idn_shape$Propinsi)
colnames(id_province) <- c("ID","Province")
id_province$id <- paste(seq.int(nrow(id_province))-1)
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
  guides(fill=guide_legend(title="Export Volume in Kilogram")) +
  geom_polygon(color = "white", fill = NA) +
  geom_polygon(data = idn_tz_shape_df[idn_tz_shape_df$group!=1.2,], aes(x=long, y = lat, group = group), colour="red", fill=NA) +
  theme_bw() +
  ggtitle("Total Export Volume of Fishery Commodities 2001 - 2012 by Export Origin") +
  theme(plot.title = element_text(hjust = 0.5, size = 23), legend.title=element_text(size=12), legend.text=element_text(size=12), legend.margin=margin(l = -1, unit='cm')) +
  annotate("text", x=105, y=-13, label= "UTC+07:00 WESTERN", size = 7) + 
  annotate("text", x=120, y=-13, label= "UTC+08:00 CENTRAL", size = 7) +
  annotate("text", x=133, y=-13, label= "UTC+09:00 EASTERN", size = 7) +
  ditch_the_axes

# map_export_volume
map_export_volume + scale_fill_gradient(trans = "log10")






# diagram 2
# comparison of fishery development between western part and middle and eastern part aggregated by years, region

all_commodities_by_year_province <- aggregate(all_commodities$Volume, by=list(Year=all_commodities$Tahun,Province=all_commodities$NamaProvinsi),FUN=sum)
#View(all_commodities_by_year_province)

all_commodities_by_year_province$Part <- with(all_commodities_by_province_edited, Part[match(all_commodities_by_year_province$Province, Province)])
#View(all_commodities_by_year_province)

colnames(all_commodities_by_year_province)[3] <- "Volume"

all_commodities_by_year_part <- aggregate(all_commodities_by_year_province$Volume, by=list(Year=all_commodities_by_year_province$Year,Part=all_commodities_by_year_province$Part),FUN=sum)
colnames(all_commodities_by_year_part)[3] <- "Volume"
#View(all_commodities_by_year_part)

test <- aggregate(all_commodities$Volume, by=list(Year=all_commodities$Tahun),FUN=sum)
#View(test)

# plot back to back bar chart
plotting_commodity_df <-
  all_commodities_by_year_part %>%
  mutate(Volume = if_else(Part == "WEST", -Volume, Volume))
#View(plotting_commodity_df)

the_commodity_order <- plotting_commodity_df$Year[plotting_commodity_df$Part=="WEST"]

p <- 
  plotting_commodity_df %>% 
  ggplot(aes(x = Year, y = Volume, group = Part, fill = Part)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = the_commodity_order) +
  # another trick!
  scale_y_continuous(breaks = seq(-4e+05, 3e+05, 1e+05), 
                     labels = abs(seq(-4e+05, 3e+05, 1e+05))) +
  labs(x = "Year", y = "Volume in Kilogram", title = "Export Volume by Export Origin") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r=10,l=3), size=25),
        axis.text = element_text(size=12),
        axis.ticks = element_blank(),
        axis.title = element_text(size=15),
        axis.line.x.bottom =  element_line(size = 1, colour = "black"),
        panel.border = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("WEST", "EAST"),
                    labels=c("WESTERN", "CENTRAL and EASTERN"))

p








# diagram 3
# comparison of sum volume of  each commodity aggregated by commodity, top 10 country (stacked barchart) !!!

# remove the "other country" 
exprt_destination_country <- exprt_destination[exprt_destination$NegaraID!=31,]

# aggregate by country and commodity
exprt_destination_by_country_commodity <- aggregate(exprt_destination_country$Volume, by=list(Commodity=exprt_destination_country$NamaKomoditi,Country=exprt_destination_country$Negara),FUN=sum)
colnames(exprt_destination_by_country_commodity)[3] <- "Volume"
#View(exprt_destination_by_country_commodity)

# order the country by volume
exprt_destination_by_country_all_commodity <- exprt_destination_by_country_commodity[exprt_destination_by_country_commodity$Commodity=="semua komoditi - all commodities",]
exprt_destination_by_country_all_commodity <- exprt_destination_by_country_all_commodity[order(exprt_destination_by_country_all_commodity$Volume, decreasing = TRUE),]
#View(exprt_destination_by_country_all_commodity)

# take top 8
exprt_destination_by_country_top_8 <- exprt_destination_by_country_all_commodity[1:8,]
#View(exprt_destination_by_country_top_8)

exprt_destination_by_country_top_8_commodity <- exprt_destination_by_country_commodity[is.element(exprt_destination_by_country_commodity$Country, exprt_destination_by_country_top_8$Country),]
exprt_destination_by_country_top_8_commodity <- exprt_destination_by_country_top_8_commodity[exprt_destination_by_country_top_8_commodity$Commodity!="semua komoditi - all commodities",]
#View(exprt_destination_by_country_top_8_commodity)

exprt_destination_by_country_top_8_commodity$CountryEng[exprt_destination_by_country_top_8_commodity$Country=="Cina"] <- "China"
exprt_destination_by_country_top_8_commodity$CountryEng[exprt_destination_by_country_top_8_commodity$Country=="Amerika Serikat"] <- "USA"
exprt_destination_by_country_top_8_commodity$CountryEng[exprt_destination_by_country_top_8_commodity$Country=="Hongkong"] <- "Hongkong"
exprt_destination_by_country_top_8_commodity$CountryEng[exprt_destination_by_country_top_8_commodity$Country=="Jepang"] <- "Jepan"
exprt_destination_by_country_top_8_commodity$CountryEng[exprt_destination_by_country_top_8_commodity$Country=="Malaysia"] <- "Malaysia"
exprt_destination_by_country_top_8_commodity$CountryEng[exprt_destination_by_country_top_8_commodity$Country=="Singapura"] <- "Singapore"
exprt_destination_by_country_top_8_commodity$CountryEng[exprt_destination_by_country_top_8_commodity$Country=="Taiwan"] <- "Taiwan"
exprt_destination_by_country_top_8_commodity$CountryEng[exprt_destination_by_country_top_8_commodity$Country=="Thailand"] <- "Thailand"

# set the order
exprt_destination_by_country_top_8_commodity$Total <- with(exprt_destination_by_country_top_8, Volume[match(exprt_destination_by_country_top_8_commodity$Country, Country)])
exprt_destination_by_country_top_8_commodity_ordered <- exprt_destination_by_country_top_8_commodity[order(exprt_destination_by_country_top_8_commodity$Total, decreasing = TRUE),]
#View(exprt_destination_by_country_top_8_commodity_ordered)

exprt_destination_by_country_top_8_commodity$CountryEng <- factor(exprt_destination_by_country_top_8_commodity$CountryEng, levels = unique(exprt_destination_by_country_top_8_commodity$CountryEng[order(exprt_destination_by_country_top_8_commodity$Total, decreasing = TRUE)]))

# plot
ggplot(exprt_destination_by_country_top_8_commodity, aes(x = CountryEng, y = Volume, fill = Commodity)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Country", y = "Volume in Kilogram", title = "Top 8 Export Destinations by Total Export Volume 2001 - 2012") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r=10,l=3), size=25),
        axis.text = element_text(size=12),
        axis.ticks = element_blank(),
        axis.title = element_text(size=15),
        axis.line.x.bottom =  element_line(size = 1, colour = "black"),
        panel.border = element_blank(),
        panel.grid = element_blank())
