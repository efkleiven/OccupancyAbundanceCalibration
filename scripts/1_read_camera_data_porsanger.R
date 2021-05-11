
# set wd
setwd("./data/cameratrap/porsanger/manual")


# select libraries
library("readxl")
library("dplyr")

# spesify filenames, done differently for files made by rolf and files made by eivind
filenames_E1 <- c("G01_19.csv","G01_20.csv","G02_19.csv","G02_20.csv","G08_19.csv","G08_20.csv")
filenames_E2 <- c("G11_19.txt","G11_20.txt","G11_20_2.txt","G15_19.txt","G15_20.txt","G15_20_2.txt","G16_19.txt",
                  "G16_20.txt","G16_20_2.txt","G19_19.txt","G19_20.txt","G19_20_2.txt","G20_19.txt", "G20_20.txt","G20_20_2.txt")
filenames_R <- c("G_6_2019_2020.xlsx","G4_2018_2019_mai.xlsx","G4_2019_2020.xlsx","G4_2019_jun_aug.xlsx","G4_2020_aug_nov.xlsx","G6__2018_2019.xlsx",   
                 "G6_2020_aug_nov.xlsx","T1-2_2019_2020.xlsx","T1.2_2018_2019.xlsx","T2-2_2018_2019.xlsx","T2_1_2018_2019.xlsx","T2_1_2019_2020.xlsx",
                 "T2_2_2019_2020.xlsx","T3.2_2018_2019.xlsx","T3.2_2019_2020.xlsx","T5.2_2018_2019.xlsx","T5_2_2019_2020.xlsx")

# make empty list to import files
E1_import <- list()
E2_import <- list()
R_import <- list()

# import files
for(i in 1:length(filenames_E1)){
E1_import[[i]] <- read.csv(filenames_E1[i],skip=1)  
}

for(i in 1:length(filenames_E2)){
  E2_import[[i]] <- read.delim(filenames_E2[i],skip=1, sep=",")  
}

for(i in 1:length(filenames_R)){
  R_import[[i]] <- read_excel(filenames_R[i])  
}

# format eivinds files

# make lists into df
e_dat1 <- do.call(rbind, E1_import)
e_dat2 <- do.call(rbind, E2_import)

e_dat <- rbind(e_dat1,e_dat2)
str(e_dat)

e_dat$DS[is.na(e_dat$DS)]<-0
e_dat$E[is.na(e_dat$E)]<-0
e_dat$G[is.na(e_dat$G)]<-0
e_dat$L[is.na(e_dat$L)]<-0
e_dat$M[is.na(e_dat$M)]<-0
e_dat$R[is.na(e_dat$R)]<-0
e_dat$S[is.na(e_dat$S)]<-0
e_dat$V[is.na(e_dat$V)]<-0
e_dat$W[is.na(e_dat$W)]<-0

e_dat$species <- ifelse(e_dat$DS=="Yes", "DS", 
                    ifelse(e_dat$E=="Yes", "E",
                        ifelse(e_dat$G=="Yes", "G",
                             ifelse(e_dat$L=="Yes", "L",
                                ifelse(e_dat$M=="Yes", "M",
                                   ifelse(e_dat$R=="Yes", "R",
                                      ifelse(e_dat$S=="Yes", "S",
                                         ifelse(e_dat$V=="Yes", "V",
                                            ifelse(e_dat$W=="Yes", "W", NA)))))))))

# check that the species column look ok
table(e_dat$species)
unique(e_dat$species)

# remove all empty images(species = NA)
e_dat3 <- filter(e_dat, !is.na(species))
e_dat4 <- select(e_dat, Location, Trigger, Date, Time, species)
str(e_dat4)       
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# format rolfs files
r_dat1 <- do.call(rbind, R_import)

# fix time format
r_dat1$Time <- format(r_dat1$Time, "%H:%M:%S")

r_dat2 <- select(r_dat1, Location, Trigger, Date, Time, Art)

# rename species column to match e_dat
names(r_dat2)[5] <- "species"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#merge rolfs and eivinds files
dat <- rbind(e_dat4,r_dat2)
str(dat)

# write .rda
save(dat, file="porsanger_imported.rda")
