library(quantmod)
library(plyr)
library(dplyr)
## Renewables companies from biofeuls digest
getSymbols(c("ABGB","ADM","ALGBE.PA","AMRS","AMTX","ANDE","BAS.F","BIOA-U","BX.TO","CDXS","CRBN.AS","CZZ",
             "DD","DNOVF","DSM.AS","DYAI","EVGN","GEVO","GPP","GPRED","MTBX","MON","NTOIF","NZMB.F","PEIX",
             "REGI","SYT","SZYM","UPM1V.HE","WM"))
## Create an index from companies involved with renewables, selection from Biofuels Digest
indxR <- as.xts(merge(ABGB,ADM,ALGBE.PA,AMRS,AMTX,ANDE,BAS.F,`BIOA-U`,BX.TO,CDXS,CRBN.AS,CZZ,
                  DD,DNOVF,DSM.AS,DYAI,EVGN,GEVO,GPP,GPRED,MTBX,MON,NTOIF,NZMB.F,PEIX,
                  REGI,SYT,SZYM,UPM1V.HE,WM))
datesR  <- index(indxR)
indxR.df <- as.data.frame(indxR)
	indxR.df <- tbl_df(indxR.df)
## Break out the different OCHL variables in the index so they can be aggregated
indxR.Os <- indxR.df %>% select(contains("Open"))
	indxR.Os <- mutate(indxR.Os,sum.Open = rowSums(indxR.Os, na.rm = TRUE))
indxR.Hs <- indxR.df %>% select(contains("High"))
	indxR.Hs <- mutate(indxR.Hs, sum.High = rowSums(indxR.Hs, na.rm = TRUE)) 
indxR.Ls <- indxR.df %>% select(contains("Low"))
	indxR.Ls <- mutate(indxR.Ls, sum.Low = rowSums(indxR.Ls, na.rm = TRUE))
indxR.Cls <- indxR.df %>% select(contains("Close"))
	indxR.Cls <- mutate(indxR.Cls, sum.Close = rowSums(indxR.Cls, na.rm = TRUE))
indxR.Vs <- indxR.df %>% select(contains("Volume"))
	indxR.Vs <- mutate(indxR.Vs, sum.Volume = rowSums(indxR.Vs, na.rm = TRUE))
indxR.As <- indxR.df %>% select(contains("Adjusted"))
	indxR.As <- mutate(indxR.As, sum.Adjusted = rowSums(indxR.As, na.rm = TRUE))
## Bind them back together
indxR.s <- bind_cols(data.frame(datesR),data.frame(indxR.Os$sum.Open),data.frame(indxR.Hs$sum.High),
                      data.frame(indxR.Ls$sum.Low), data.frame(indxR.Cls$sum.Close), 
                      data.frame(indxR.Vs$sum.Volume), data.frame(indxR.As$sum.Adjusted))
## Create a new xts object for chartSeries to use
indxR. <- indxR.s[,-1]
indxR.x <- as.xts(indxR., order.by = datesR)
chartSeries(indxR.x, name= "The Digest Equity Index; Advanced Bioeconomy", subset = '2011::2016-01', 
            TA = c("addVo()","addEVWMA()"), theme = chartTheme("white",up.col = "blue", dn.col="red"))
            
