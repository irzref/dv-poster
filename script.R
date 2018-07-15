setwd("/home/irza/Projects/dv-poster")

exprt_origin <- read.csv("asalprovinsiekspor.csv")
exprt_destination <- read.csv("eksporperikanannasional.csv")

library("ggplot2")



# dataset analysis

unique(exprt_origin[c("NamaKomoditi", "KomoditiID")])
unique(exprt_origin[c("Wilayah")])
table(exprt_origin$NamaProvinsi, exprt_origin$NamaKomoditi) 
View(exprt_origin[exprt_origin$KomoditiID==0,])

unique(exprt_destination[c("NamaKomoditi", "KomoditiID")])
table(exprt_destination$Negara, exprt_destination$NamaKomoditi)
View(exprt_destination[exprt_destination$KomoditiID==0,])



# comparison of fishery development between western part and middle and eastern part aggregated by each province (map)

all_commodities <- exprt_origin[exprt_origin$ProvinsiID!=0 & exprt_origin$KomoditiID==0,]
View(all_commodities)

all_commodities_by_province <- aggregate(all_commodities$Volume, by=list(Region=all_commodities$Wilayah,Province=all_commodities$NamaProvinsi),FUN=sum)
View(all_commodities_by_province)

# indonesia map must be converted to heat map !!!



# comparison of fishery development between western part and middle and eastern part aggregated by years, region

all_commodities_by_year_province <- aggregate(all_commodities$Volume, by=list(Year=all_commodities$Tahun,Region=all_commodities$Wilayah),FUN=sum)
View(all_commodities_by_year_province)

all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Sumatera"] <- "West"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Jawa"] <- "West"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Kalimantan"] <- "West"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Bali - Nusa Tenggara"] <- "East"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Sulawesi"] <- "East"
all_commodities_by_year_province$Region_new[all_commodities_by_year_province$Region=="Maluku - Papua"] <- "East"