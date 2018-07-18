setwd("/home/irza/Projects/dv-poster")

exprt_origin <- read.csv("asalprovinsiekspor.csv")
exprt_destination <- read.csv("eksporperikanannasional.csv")
population <- read.csv2("bps-file.csv")


library("ggplot2")



# dataset analysis

dim(unique(population[c("nama_item_vertical_variabel")]))

unique(exprt_origin[c("NamaKomoditi", "KomoditiID")])
unique(exprt_origin[c("Wilayah")])
dim(unique(exprt_origin[c("ProvinsiID")]))
unique(exprt_origin[c("NamaProvinsi")])
dim(unique(exprt_origin[c("NamaProvinsi")]))
table(exprt_origin$NamaProvinsi, exprt_origin$NamaKomoditi) 
View(exprt_origin[exprt_origin$KomoditiID==0,])

unique(exprt_destination[c("NamaKomoditi", "KomoditiID")])
table(exprt_destination$Negara, exprt_destination$NamaKomoditi)
View(exprt_destination[exprt_destination$KomoditiID==0,])



# comparison of fishery development between western part and middle and eastern part aggregated by each province (map)

all_commodities <- exprt_origin[exprt_origin$ProvinsiID!=0 & exprt_origin$KomoditiID==0,]
View(all_commodities)

all_commodities_by_province <- aggregate(all_commodities$Volume, by=list(Region=all_commodities$Wilayah,Province=all_commodities$NamaProvinsi),FUN=mean)
View(all_commodities_by_province)

write.csv(all_commodities_by_province,file="all_commodities_by_province.csv",row.names=FALSE)


# load the all_commodities_by_province !!!
# merge value of all riau provinces and take average of it, set the value to the all_commodities_by_province !!!
# indonesia map must be converted to heat map !!!



# comparison of fishery development between western part and middle and eastern part aggregated by years, region

all_commodities_by_year_province <- aggregate(all_commodities$Volume, by=list(Year=all_commodities$Tahun,Region=all_commodities$Wilayah),FUN=sum)
View(all_commodities_by_year_province)


# remove the following method and use only the Part variable from all_commodities_by_province !!!

all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Sumatera"] <- "West"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Jawa"] <- "West"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Kalimantan"] <- "West"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Bali - Nusa Tenggara"] <- "East"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Sulawesi"] <- "East"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Maluku - Papua"] <- "East"


# comparison of avg volume of  each commodity aggregated by commodity, top 10 country (stacked barchart) !!!