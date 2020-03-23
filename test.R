# import porsanger data
# set working dir
setwd("./data/capture_recapture/porsanger")

setwd("./2018")
po_kar_jun18 <- read.table("karasjok-måsøy_juni18.txt", header=T, sep="\t")
po_kar_aug18 <- read.table("karasjok-måsøy_aug18.txt", header=T, sep="\t")
po_kar_sep18 <- read.table("karasjok-måsøy_sep18.txt", header=T, sep="\t")

po_po_jun18 <- read.table("porsanger_juni18.txt", header=T, sep="\t")
po_po_aug18 <- read.table("porsanger_aug18.txt", header=T, sep="\t")
po_po_sep18 <- read.table("porsanger_sep18.txt", header=T, sep="\t")

setwd("../2019") # for 2019 we only have the data from august so fare, I'm trying to nagg Nigel to send us the rest soon.
po_kar_jun_sep19 <- read.table("karma19.txt", header=T, sep="\t") 
#karma19.txt has one column less that the other files,
# add it to ease merging of df's
po_kar_jun_sep19$indnum_old <-NA # adding missing column
po_kar_jun_sep19 <- po_kar_jun_sep19[,c(1:8,16,9:15)] #reordering columns



po_kar_aug19 <- read.table("karasjok-måsøy_aug19.txt", header=T, sep="\t")


po_po_jun_sep19 <- read.table("pors19.txt", header=T, sep="\t")
po_po_jun_sep19$indnum_old <-NA
po_po_jun_sep19 <- po_po_jun_sep19[,c(1:8,16,9:15)]
po_po_aug19 <- read.table("porsanger_aug19.txt", header=T, sep="\t")

# merge to one dataframe per sampling desing per year
po_kar_18 <- rbind(po_kar_jun18, po_kar_aug18, po_kar_sep18)
po_kar_19 <- rbind(po_kar_jun_sep19, po_kar_aug19)

po_po_18 <- rbind(po_kar_jun18, po_kar_aug18, po_kar_sep18)
po_po_19 <- rbind(po_kar_jun_sep19, po_kar_aug19)

#merge to one df per year
po_18 <- rbind(po_kar_18,po_po_18)
po_19 <- rbind(po_kar_19,po_po_19)

# subset by species
gs18 <- po_18[po_18$species=="GRAASIDEMUS",]
gs19 <- po_19[po_19$species=="GRAASIDEMUS",]

#subset by season
gs18_sp <- gs18[gs18$seas=="SPRING",]
gs18_su <- gs18[gs18$seas=="SUMMER",]
gs18_au <- gs18[gs18$seas=="AUTUMN",]

gs19_sp <- gs19[gs19$seas=="SPRING",]
gs19_su <- gs19[gs19$seas=="SUMMER",]
gs19_au <- gs19[gs19$seas=="AUTUMN",]

uniq_indiv_gs <-c(length(unique(gs18_sp$Nindnum)), length(unique(gs18_su$Nindnum)), length(unique(gs18_au$Nindnum)),
                  length(unique(gs19_sp$Nindnum)), length(unique(gs18_su$Nindnum)), length(unique(gs18_au$Nindnum)))

#plot
plot(uniq_indiv_gs)
