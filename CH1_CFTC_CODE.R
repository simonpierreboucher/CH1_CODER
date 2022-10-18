install.packages("Quandl")
library(Quandl)

setwd("~/Desktop/codeR_CH1")

CFTC_GC_COT<-Quandl("CFTC/088691_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_GC_PC<-Quandl("CFTC/088691_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_CL_COT<-Quandl("CFTC/067651_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_CL_PC<-Quandl("CFTC/067651_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_HG_COT<-Quandl("CFTC/085692_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_HG_PC<-Quandl("CFTC/085692_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_SI_COT<-Quandl("CFTC/084691_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_SI_PC<-Quandl("CFTC/084691_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_NG_COT<-Quandl("CFTC/023651_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_NG_PC<-Quandl("CFTC/023651_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_PA_COT<-Quandl("CFTC/075651_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_PA_PC<-Quandl("CFTC/075651_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")

### Crude Oil 
CFTC_CL_PC_p1<-CFTC_CL_PC
CFTC_CL_PC_p1$Date<-CFTC_CL_PC_p1$Date+1
CFTC_CL_PC_p2<-CFTC_CL_PC
CFTC_CL_PC_p2$Date<-CFTC_CL_PC_p1$Date+2
CFTC_CL_PC_p3<-CFTC_CL_PC
CFTC_CL_PC_p3$Date<-CFTC_CL_PC_p1$Date+3
CFTC_CL_PC_m1<-CFTC_CL_PC
CFTC_CL_PC_m1$Date<-CFTC_CL_PC_p1$Date-1
CFTC_CL_PC_m2<-CFTC_CL_PC
CFTC_CL_PC_m2$Date<-CFTC_CL_PC_p1$Date-2
CFTC_CL_PC_m3<-CFTC_CL_PC
CFTC_CL_PC_m3$Date<-CFTC_CL_PC_p1$Date-3

CL_PC<-rbind(CFTC_CL_PC,CFTC_CL_PC_m1,CFTC_CL_PC_m2,CFTC_CL_PC_m3,CFTC_CL_PC_p1,CFTC_CL_PC_p2,CFTC_CL_PC_p3)

CL_PC$MSCT<-(CL_PC$`Noncommercial Long`+CL_PC$`Noncommercial Short`)/(2*CL_PC$`Open Interest`)
CL_PC$NLS<-(CL_PC$`Noncommercial Long`- CL_PC$`Noncommercial Short`)/ CL_PC$`Open Interest`

CL_PC$WT1<-1+(CL_PC$`Noncommercial Short`/(CL_PC$`Commercial Long`+CL_PC$`Commercial Short`))
CL_PC$WT2<-1+(CL_PC$`Noncommercial Long`/(CL_PC$`Commercial Long`+CL_PC$`Commercial Short`))

CL_PC$WT=ifelse(CL_PC$`Commercial Short` > CL_PC$`Commercial Long`,CL_PC$WT1,CL_PC$WT2)




CFTC_CL_COT_p1<-CFTC_CL_COT
CFTC_CL_COT_p1$Date<-CFTC_CL_COT_p1$Date+1
CFTC_CL_COT_p2<-CFTC_CL_COT
CFTC_CL_COT_p2$Date<-CFTC_CL_COT_p1$Date+2
CFTC_CL_COT_p3<-CFTC_CL_COT
CFTC_CL_COT_p3$Date<-CFTC_CL_COT_p1$Date+3
CFTC_CL_COT_m1<-CFTC_CL_COT
CFTC_CL_COT_m1$Date<-CFTC_CL_COT_p1$Date-1
CFTC_CL_COT_m2<-CFTC_CL_COT
CFTC_CL_COT_m2$Date<-CFTC_CL_COT_p1$Date-2
CFTC_CL_COT_m3<-CFTC_CL_COT
CFTC_CL_COT_m3$Date<-CFTC_CL_COT_p1$Date-3



CL_COT<-rbind(CFTC_CL_COT,CFTC_CL_COT_m1,CFTC_CL_COT_m2,CFTC_CL_COT_m3,CFTC_CL_COT_p1,CFTC_CL_COT_p2,CFTC_CL_COT_p3)

CL_COT$MM_NLS<-(CL_COT$`Money Manager Longs` - CL_COT$`Money Manager Shorts`)/CL_COT$`Open Interest`
CL_COT$SWAP_NLS<-(CL_COT$`Swap Dealer Longs` - CL_COT$`Swap Dealer Shorts`)/CL_COT$`Open Interest`


### Gold 

CFTC_GC_PC_p1<-CFTC_GC_PC
CFTC_GC_PC_p1$Date<-CFTC_GC_PC_p1$Date+1
CFTC_GC_PC_p2<-CFTC_GC_PC
CFTC_GC_PC_p2$Date<-CFTC_GC_PC_p1$Date+2
CFTC_GC_PC_p3<-CFTC_GC_PC
CFTC_GC_PC_p3$Date<-CFTC_GC_PC_p1$Date+3
CFTC_GC_PC_m1<-CFTC_GC_PC
CFTC_GC_PC_m1$Date<-CFTC_GC_PC_p1$Date-1
CFTC_GC_PC_m2<-CFTC_GC_PC
CFTC_GC_PC_m2$Date<-CFTC_GC_PC_p1$Date-2
CFTC_GC_PC_m3<-CFTC_GC_PC
CFTC_GC_PC_m3$Date<-CFTC_GC_PC_p1$Date-3

GC_PC<-rbind(CFTC_GC_PC,CFTC_GC_PC_m1,CFTC_GC_PC_m2,CFTC_GC_PC_m3,CFTC_GC_PC_p1,CFTC_GC_PC_p2,CFTC_GC_PC_p3)

GC_PC$MSCT<-(GC_PC$`Noncommercial Long`+GC_PC$`Noncommercial Short`)/(2*GC_PC$`Open Interest`)
GC_PC$NLS<-(GC_PC$`Noncommercial Long`- GC_PC$`Noncommercial Short`)/ GC_PC$`Open Interest`

GC_PC$WT1<-1+(GC_PC$`Noncommercial Short`/(GC_PC$`Commercial Long`+GC_PC$`Commercial Short`))
GC_PC$WT2<-1+(GC_PC$`Noncommercial Long`/(GC_PC$`Commercial Long`+GC_PC$`Commercial Short`))

GC_PC$WT=ifelse(GC_PC$`Commercial Short` > GC_PC$`Commercial Long`,GC_PC$WT1,GC_PC$WT2)




CFTC_GC_COT_p1<-CFTC_GC_COT
CFTC_GC_COT_p1$Date<-CFTC_GC_COT_p1$Date+1
CFTC_GC_COT_p2<-CFTC_GC_COT
CFTC_GC_COT_p2$Date<-CFTC_GC_COT_p1$Date+2
CFTC_GC_COT_p3<-CFTC_GC_COT
CFTC_GC_COT_p3$Date<-CFTC_GC_COT_p1$Date+3
CFTC_GC_COT_m1<-CFTC_GC_COT
CFTC_GC_COT_m1$Date<-CFTC_GC_COT_p1$Date-1
CFTC_GC_COT_m2<-CFTC_GC_COT
CFTC_GC_COT_m2$Date<-CFTC_GC_COT_p1$Date-2
CFTC_GC_COT_m3<-CFTC_GC_COT
CFTC_GC_COT_m3$Date<-CFTC_GC_COT_p1$Date-3



GC_COT<-rbind(CFTC_GC_COT,CFTC_GC_COT_m1,CFTC_GC_COT_m2,CFTC_GC_COT_m3,CFTC_GC_COT_p1,CFTC_GC_COT_p2,CFTC_GC_COT_p3)

GC_COT$MM_NLS<-(GC_COT$`Money Manager Longs` - GC_COT$`Money Manager Shorts`)/GC_COT$`Open Interest`
GC_COT$SWAP_NLS<-(GC_COT$`Swap Dealer Longs` - GC_COT$`Swap Dealer Shorts`)/GC_COT$`Open Interest`

### Silver

CFTC_SI_PC_p1<-CFTC_SI_PC
CFTC_SI_PC_p1$Date<-CFTC_SI_PC_p1$Date+1
CFTC_SI_PC_p2<-CFTC_SI_PC
CFTC_SI_PC_p2$Date<-CFTC_SI_PC_p1$Date+2
CFTC_SI_PC_p3<-CFTC_SI_PC
CFTC_SI_PC_p3$Date<-CFTC_SI_PC_p1$Date+3
CFTC_SI_PC_m1<-CFTC_SI_PC
CFTC_SI_PC_m1$Date<-CFTC_SI_PC_p1$Date-1
CFTC_SI_PC_m2<-CFTC_SI_PC
CFTC_SI_PC_m2$Date<-CFTC_SI_PC_p1$Date-2
CFTC_SI_PC_m3<-CFTC_SI_PC
CFTC_SI_PC_m3$Date<-CFTC_SI_PC_p1$Date-3

SI_PC<-rbind(CFTC_SI_PC,CFTC_SI_PC_m1,CFTC_SI_PC_m2,CFTC_SI_PC_m3,CFTC_SI_PC_p1,CFTC_SI_PC_p2,CFTC_SI_PC_p3)

SI_PC$MSCT<-(SI_PC$`Noncommercial Long`+SI_PC$`Noncommercial Short`)/(2*SI_PC$`Open Interest`)
SI_PC$NLS<-(SI_PC$`Noncommercial Long`- SI_PC$`Noncommercial Short`)/ SI_PC$`Open Interest`

SI_PC$WT1<-1+(SI_PC$`Noncommercial Short`/(SI_PC$`Commercial Long`+SI_PC$`Commercial Short`))
SI_PC$WT2<-1+(SI_PC$`Noncommercial Long`/(SI_PC$`Commercial Long`+SI_PC$`Commercial Short`))

SI_PC$WT=ifelse(SI_PC$`Commercial Short` > SI_PC$`Commercial Long`,SI_PC$WT1,SI_PC$WT2)




CFTC_SI_COT_p1<-CFTC_SI_COT
CFTC_SI_COT_p1$Date<-CFTC_SI_COT_p1$Date+1
CFTC_SI_COT_p2<-CFTC_SI_COT
CFTC_SI_COT_p2$Date<-CFTC_SI_COT_p1$Date+2
CFTC_SI_COT_p3<-CFTC_SI_COT
CFTC_SI_COT_p3$Date<-CFTC_SI_COT_p1$Date+3
CFTC_SI_COT_m1<-CFTC_SI_COT
CFTC_SI_COT_m1$Date<-CFTC_SI_COT_p1$Date-1
CFTC_SI_COT_m2<-CFTC_SI_COT
CFTC_SI_COT_m2$Date<-CFTC_SI_COT_p1$Date-2
CFTC_SI_COT_m3<-CFTC_SI_COT
CFTC_SI_COT_m3$Date<-CFTC_SI_COT_p1$Date-3



SI_COT<-rbind(CFTC_SI_COT,CFTC_SI_COT_m1,CFTC_SI_COT_m2,CFTC_SI_COT_m3,CFTC_SI_COT_p1,CFTC_SI_COT_p2,CFTC_SI_COT_p3)

SI_COT$MM_NLS<-(SI_COT$`Money Manager Longs` - SI_COT$`Money Manager Shorts`)/SI_COT$`Open Interest`
SI_COT$SWAP_NLS<-(SI_COT$`Swap Dealer Longs` - SI_COT$`Swap Dealer Shorts`)/SI_COT$`Open Interest`

### NATURAL GAZ

CFTC_NG_PC_p1<-CFTC_NG_PC
CFTC_NG_PC_p1$Date<-CFTC_NG_PC_p1$Date+1
CFTC_NG_PC_p2<-CFTC_NG_PC
CFTC_NG_PC_p2$Date<-CFTC_NG_PC_p1$Date+2
CFTC_NG_PC_p3<-CFTC_NG_PC
CFTC_NG_PC_p3$Date<-CFTC_NG_PC_p1$Date+3
CFTC_NG_PC_m1<-CFTC_NG_PC
CFTC_NG_PC_m1$Date<-CFTC_NG_PC_p1$Date-1
CFTC_NG_PC_m2<-CFTC_NG_PC
CFTC_NG_PC_m2$Date<-CFTC_NG_PC_p1$Date-2
CFTC_NG_PC_m3<-CFTC_NG_PC
CFTC_NG_PC_m3$Date<-CFTC_NG_PC_p1$Date-3

NG_PC<-rbind(CFTC_NG_PC,CFTC_NG_PC_m1,CFTC_NG_PC_m2,CFTC_NG_PC_m3,CFTC_NG_PC_p1,CFTC_NG_PC_p2,CFTC_NG_PC_p3)

NG_PC$MSCT<-(NG_PC$`Noncommercial Long`+NG_PC$`Noncommercial Short`)/(2*NG_PC$`Open Interest`)
NG_PC$NLS<-(NG_PC$`Noncommercial Long`- NG_PC$`Noncommercial Short`)/ NG_PC$`Open Interest`

NG_PC$WT1<-1+(NG_PC$`Noncommercial Short`/(NG_PC$`Commercial Long`+NG_PC$`Commercial Short`))
NG_PC$WT2<-1+(NG_PC$`Noncommercial Long`/(NG_PC$`Commercial Long`+NG_PC$`Commercial Short`))

NG_PC$WT=ifelse(NG_PC$`Commercial Short` > NG_PC$`Commercial Long`,NG_PC$WT1,NG_PC$WT2)




CFTC_NG_COT_p1<-CFTC_NG_COT
CFTC_NG_COT_p1$Date<-CFTC_NG_COT_p1$Date+1
CFTC_NG_COT_p2<-CFTC_NG_COT
CFTC_NG_COT_p2$Date<-CFTC_NG_COT_p1$Date+2
CFTC_NG_COT_p3<-CFTC_NG_COT
CFTC_NG_COT_p3$Date<-CFTC_NG_COT_p1$Date+3
CFTC_NG_COT_m1<-CFTC_NG_COT
CFTC_NG_COT_m1$Date<-CFTC_NG_COT_p1$Date-1
CFTC_NG_COT_m2<-CFTC_NG_COT
CFTC_NG_COT_m2$Date<-CFTC_NG_COT_p1$Date-2
CFTC_NG_COT_m3<-CFTC_NG_COT
CFTC_NG_COT_m3$Date<-CFTC_NG_COT_p1$Date-3



NG_COT<-rbind(CFTC_NG_COT,CFTC_NG_COT_m1,CFTC_NG_COT_m2,CFTC_NG_COT_m3,CFTC_NG_COT_p1,CFTC_NG_COT_p2,CFTC_NG_COT_p3)

NG_COT$MM_NLS<-(NG_COT$`Money Manager Longs` - NG_COT$`Money Manager Shorts`)/NG_COT$`Open Interest`
NG_COT$SWAP_NLS<-(NG_COT$`Swap Dealer Longs` - NG_COT$`Swap Dealer Shorts`)/NG_COT$`Open Interest`


### Palladium

CFTC_PA_PC_p1<-CFTC_PA_PC
CFTC_PA_PC_p1$Date<-CFTC_PA_PC_p1$Date+1
CFTC_PA_PC_p2<-CFTC_PA_PC
CFTC_PA_PC_p2$Date<-CFTC_PA_PC_p1$Date+2
CFTC_PA_PC_p3<-CFTC_PA_PC
CFTC_PA_PC_p3$Date<-CFTC_PA_PC_p1$Date+3
CFTC_PA_PC_m1<-CFTC_PA_PC
CFTC_PA_PC_m1$Date<-CFTC_PA_PC_p1$Date-1
CFTC_PA_PC_m2<-CFTC_PA_PC
CFTC_PA_PC_m2$Date<-CFTC_PA_PC_p1$Date-2
CFTC_PA_PC_m3<-CFTC_PA_PC
CFTC_PA_PC_m3$Date<-CFTC_PA_PC_p1$Date-3

PA_PC<-rbind(CFTC_PA_PC,CFTC_PA_PC_m1,CFTC_PA_PC_m2,CFTC_PA_PC_m3,CFTC_PA_PC_p1,CFTC_PA_PC_p2,CFTC_PA_PC_p3)

PA_PC$MSCT<-(PA_PC$`Noncommercial Long`+PA_PC$`Noncommercial Short`)/(2*PA_PC$`Open Interest`)
PA_PC$NLS<-(PA_PC$`Noncommercial Long`- PA_PC$`Noncommercial Short`)/ PA_PC$`Open Interest`

PA_PC$WT1<-1+(PA_PC$`Noncommercial Short`/(PA_PC$`Commercial Long`+PA_PC$`Commercial Short`))
PA_PC$WT2<-1+(PA_PC$`Noncommercial Long`/(PA_PC$`Commercial Long`+PA_PC$`Commercial Short`))

PA_PC$WT=ifelse(PA_PC$`Commercial Short` > PA_PC$`Commercial Long`,PA_PC$WT1,PA_PC$WT2)




CFTC_PA_COT_p1<-CFTC_PA_COT
CFTC_PA_COT_p1$Date<-CFTC_PA_COT_p1$Date+1
CFTC_PA_COT_p2<-CFTC_PA_COT
CFTC_PA_COT_p2$Date<-CFTC_PA_COT_p1$Date+2
CFTC_PA_COT_p3<-CFTC_PA_COT
CFTC_PA_COT_p3$Date<-CFTC_PA_COT_p1$Date+3
CFTC_PA_COT_m1<-CFTC_PA_COT
CFTC_PA_COT_m1$Date<-CFTC_PA_COT_p1$Date-1
CFTC_PA_COT_m2<-CFTC_PA_COT
CFTC_PA_COT_m2$Date<-CFTC_PA_COT_p1$Date-2
CFTC_PA_COT_m3<-CFTC_PA_COT
CFTC_PA_COT_m3$Date<-CFTC_PA_COT_p1$Date-3



PA_COT<-rbind(CFTC_PA_COT,CFTC_PA_COT_m1,CFTC_PA_COT_m2,CFTC_PA_COT_m3,CFTC_PA_COT_p1,CFTC_PA_COT_p2,CFTC_PA_COT_p3)

PA_COT$MM_NLS<-(PA_COT$`Money Manager Longs` - PA_COT$`Money Manager Shorts`)/PA_COT$`Open Interest`
PA_COT$SWAP_NLS<-(PA_COT$`Swap Dealer Longs` - PA_COT$`Swap Dealer Shorts`)/PA_COT$`Open Interest`


### Copper

CFTC_HG_PC_p1<-CFTC_HG_PC
CFTC_HG_PC_p1$Date<-CFTC_HG_PC_p1$Date+1
CFTC_HG_PC_p2<-CFTC_HG_PC
CFTC_HG_PC_p2$Date<-CFTC_HG_PC_p1$Date+2
CFTC_HG_PC_p3<-CFTC_HG_PC
CFTC_HG_PC_p3$Date<-CFTC_HG_PC_p1$Date+3
CFTC_HG_PC_m1<-CFTC_HG_PC
CFTC_HG_PC_m1$Date<-CFTC_HG_PC_p1$Date-1
CFTC_HG_PC_m2<-CFTC_HG_PC
CFTC_HG_PC_m2$Date<-CFTC_HG_PC_p1$Date-2
CFTC_HG_PC_m3<-CFTC_HG_PC
CFTC_HG_PC_m3$Date<-CFTC_HG_PC_p1$Date-3

HG_PC<-rbind(CFTC_HG_PC,CFTC_HG_PC_m1,CFTC_HG_PC_m2,CFTC_HG_PC_m3,CFTC_HG_PC_p1,CFTC_HG_PC_p2,CFTC_HG_PC_p3)

HG_PC$MSCT<-(HG_PC$`Noncommercial Long`+HG_PC$`Noncommercial Short`)/(2*HG_PC$`Open Interest`)
HG_PC$NLS<-(HG_PC$`Noncommercial Long`- HG_PC$`Noncommercial Short`)/ HG_PC$`Open Interest`

HG_PC$WT1<-1+(HG_PC$`Noncommercial Short`/(HG_PC$`Commercial Long`+HG_PC$`Commercial Short`))
HG_PC$WT2<-1+(HG_PC$`Noncommercial Long`/(HG_PC$`Commercial Long`+HG_PC$`Commercial Short`))

HG_PC$WT=ifelse(HG_PC$`Commercial Short` > HG_PC$`Commercial Long`,HG_PC$WT1,HG_PC$WT2)




CFTC_HG_COT_p1<-CFTC_HG_COT
CFTC_HG_COT_p1$Date<-CFTC_HG_COT_p1$Date+1
CFTC_HG_COT_p2<-CFTC_HG_COT
CFTC_HG_COT_p2$Date<-CFTC_HG_COT_p1$Date+2
CFTC_HG_COT_p3<-CFTC_HG_COT
CFTC_HG_COT_p3$Date<-CFTC_HG_COT_p1$Date+3
CFTC_HG_COT_m1<-CFTC_HG_COT
CFTC_HG_COT_m1$Date<-CFTC_HG_COT_p1$Date-1
CFTC_HG_COT_m2<-CFTC_HG_COT
CFTC_HG_COT_m2$Datecod<-CFTC_HG_COT_p1$Date-2
CFTC_HG_COT_m3<-CFTC_HG_COT
CFTC_HG_COT_m3$Date<-CFTC_HG_COT_p1$Date-3



HG_COT<-rbind(CFTC_HG_COT,CFTC_HG_COT_m1,CFTC_HG_COT_m2,CFTC_HG_COT_m3,CFTC_HG_COT_p1,CFTC_HG_COT_p2,CFTC_HG_COT_p3)

HG_COT$MM_NLS<-(HG_COT$`Money Manager Longs` - HG_COT$`Money Manager Shorts`)/HG_COT$`Open Interest`
HG_COT$SWAP_NLS<-(HG_COT$`Swap Dealer Longs` - HG_COT$`Swap Dealer Shorts`)/HG_COT$`Open Interest`

### Save dataset in RDA data 

save(CL_COT,file="CL_COT.Rda")
save(CL_PC,file="CL_PC.Rda")
save(GC_COT,file="GC_COT.Rda")
save(GC_PC,file="GC_PC.Rda")
save(HG_COT,file="HG_COT.Rda")
save(HG_PC,file="HG_PC.Rda")
save(NG_COT,file="NG_COT.Rda")
save(NG_PC,file="NG_PC.Rda")
save(PA_COT,file="PA_COT.Rda")
save(PA_PC,file="PA_PC.Rda")
save(SI_COT,file="SI_COT.Rda")
save(SI_PC,file="SI_PC.Rda")