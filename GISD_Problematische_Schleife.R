#### SNIPPET

for(mykennziffer in "Kreiskennziffer") {
    myname <-  exportlist$Namen[exportlist$Kennziffern==mykennziffer]
    mylabel<-  exportlist$Label[exportlist$Kennziffern==mykennziffer]
    print(paste("Level:",myname,"Label:",mylabel))
  
  # Datensatzerstellung
  outputdata <- RawResult  
  
  # Datensatz zum Mergen der jeweiligen Kennziffer (Gemeinde, Kreis etc.) erstellen
  outputdata$Group <- outputdata[[mykennziffer]]
  mergedataset  <- outputdata %>% dplyr::select(ID=mykennziffer,myname,Bundesland) %>% 
    group_by(ID) %>% filter(row_number()==1) %>% ungroup() 
  names(mergedataset)[1]=mykennziffer
  
  # Aggregation
  outputdata.agg <- outputdata %>% 
    group_by(Group,Jahr) %>% 
    dplyr::select(Group,Jahr,Bevoelkerung, GISD_Score, TS_Bildung, TS_Einkommen, TS_Arbeitswelt) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score, Bevoelkerung), 
              TS_Bildung = weighted.mean(TS_Bildung, Bevoelkerung), 
              TS_Einkommen = weighted.mean(TS_Einkommen, Bevoelkerung),
              TS_Arbeitswelt = weighted.mean(TS_Arbeitswelt, Bevoelkerung),
              Bevoelkerung = sum(Bevoelkerung))
              
              
              
              # Daten bereinigen
              names(outputdata.agg)[1] <- mykennziffer
              outputdata.agg <- merge(outputdata.agg,mergedataset,by=mykennziffer) %>%  
                dplyr::select(mykennziffer,myname,Jahr,Bundesland,"Bevoelkerung",GISD_Score, TS_Bildung, TS_Einkommen, TS_Arbeitswelt) %>%
                group_by(Jahr) %>% as.tibble()
              
              
              
              
              # Rekodierung
              outputdata.agg <- outputdata.agg %>%  mutate(GISD_Score = round((GISD_Score -min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6),
                                                           GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9)),
                                                           GISD_5 = findInterval(GISD_5, c(1:5)),
                                                           GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)),
                                                           GISD_10 = findInterval(GISD_10, c(1:10)),
                                                           GISD_k = findInterval(GISD_5, c(1,2,5))) 
              summary(outputdata.agg %>% select(contains("GISD")))

}
####