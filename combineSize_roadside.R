#sample code to read fish sizes and summarize
#sample code to read fish sizes and summarize

library(readxl)
library(tidyverse)
library(lme4)
library(nlme)
library(ggplot2)
library(gridExtra)
library(grid)

rm(list=ls()); graphics.off(); #clears environment and closes plots

#################################


sizeAll <- read_excel("Roadside_size_10-17-21.xlsx")
sizeAll$Photo_Name<-sizeAll$`Photo ID`

sizeAll$Fish_ID_Number<-sizeAll$`Label Number`

sizeAll$Photo_Name <- gsub(".jpg", "", sizeAll$Photo_Name) #remove.jpg

sizeAll$Photo_Name <- gsub("-1", "", sizeAll$Photo_Name)#remove -1
sizeAll$Sizer <- "Aya"


total1 <-  read_excel("Roadside_final_data_2021-10-08.xlsx")


total1$Taxonomic_Code[total1$Taxonomic_Code=='Unknown'] = 'unknown' #fixCaps
total1$Taxonomic_Code[total1$Taxonomic_Code=='parrotfish'] = 'Scaridae' #take to latin
total1$Taxonomic_Code[total1$Taxonomic_Code=='Parrotfish'] = 'Scaridae' #take to latin
total1$Taxonomic_Code[total1$Taxonomic_Code=='Naso Unicornis'] = 'Naso unicornis' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Mulloidichtys sp.'] = 'Mulloidichthys' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Parupeneus sp.'] = 'Parupeneus' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Epinephelus sp.'] = 'Epinephelus' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Myripristinae'] = 'Myripristis' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Mulloidichtys sp.'] = 'Mulloidichthys' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Mulloidichtys'] = 'Mulloidichthys' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Spiny Lobster'] = 'Spiny lobster' #make consistent


#total1 <-  filter(total1, Observation_Round == "Initial")#Final_Keep == "TRUE" #we don't need this for roadside because dont have multiple ids

mergedSize_all <- merge(sizeAll, total1,all.x = TRUE,all.y = FALSE, by=c("Photo_Name", "Fish_ID_Number"))

mergedSizeTest <- merge(sizeAll, total1,all.x = TRUE,all.y = TRUE, by=c("Photo_Name", "Fish_ID_Number"))
write.csv(mergedSizeTest, "mergedSizeTest.csv")

mergedSize <-filter(mergedSize_all , Length!= "DNE")

mergedSize$Length <- as.numeric(mergedSize$Length) 
mergedSize$Length <- round(mergedSize$Length ,digit=2)

mergedSize$Event <- toupper(mergedSize$Event)

outings1 <- read_excel("Rdsd_data_2020_2021-06-17.xlsx")
outings1$Event <- toupper(outings1$eventId)
outings1$FixedDate <-  as.Date(outings1$eventDay, "%Y/%m/%D")

outings2 <- outings1 
 
outings2$eventVillage[outings2$eventVillage=='Vaiare'] = 'Vaihere'
outings2$eventVillage[outings2$eventVillage=='Varari'] = 'Vaihere'
mergedSizeFull <- merge(mergedSize, outings2 ,all.x = TRUE,all.y = FALSE, by=c("Event"))

mergedSizeFull  <-filter(mergedSizeFull  , Fish_ID_Number>0)#this is temporary but should be unnecessary once aya fizes ids.


write.csv(mergedSizeFull, "mergedSizeRoadsideFull.csv")
library(readxl)
library(tidyverse)
library(lme4)
library(nlme)
library(ggplot2)
library(gridExtra)
library(grid)

rm(list=ls()); graphics.off(); #clears environment and closes plots

#################################


sizeAll <- read_excel("Roadside_size_10-17-21.xlsx")
sizeAll$Photo_Name<-sizeAll$`Photo ID`

sizeAll$Fish_ID_Number<-sizeAll$`Label Number`

sizeAll$Photo_Name <- gsub(".jpg", "", sizeAll$Photo_Name) #remove.jpg

sizeAll$Photo_Name <- gsub("-1", "", sizeAll$Photo_Name)#remove -1
sizeAll$Sizer <- "Aya"


total1 <-  read_excel("Roadside_final_data_2021-10-08.xlsx")


total1$Taxonomic_Code[total1$Taxonomic_Code=='Unknown'] = 'unknown' #fixCaps
total1$Taxonomic_Code[total1$Taxonomic_Code=='parrotfish'] = 'Scaridae' #take to latin
total1$Taxonomic_Code[total1$Taxonomic_Code=='Parrotfish'] = 'Scaridae' #take to latin
total1$Taxonomic_Code[total1$Taxonomic_Code=='Naso Unicornis'] = 'Naso unicornis' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Mulloidichtys sp.'] = 'Mulloidichthys' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Parupeneus sp.'] = 'Parupeneus' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Epinephelus sp.'] = 'Epinephelus' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Myripristinae'] = 'Myripristis' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Mulloidichtys sp.'] = 'Mulloidichthys' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Mulloidichtys'] = 'Mulloidichthys' #make consistent
total1$Taxonomic_Code[total1$Taxonomic_Code=='Spiny Lobster'] = 'Spiny lobster' #make consistent


#total1 <-  filter(total1, Observation_Round == "Initial")#Final_Keep == "TRUE" #we don't need this for roadside because dont have multiple ids

mergedSize_all <- merge(sizeAll, total1,all.x = TRUE,all.y = FALSE, by=c("Photo_Name", "Fish_ID_Number"))

mergedSizeTest <- merge(sizeAll, total1,all.x = TRUE,all.y = TRUE, by=c("Photo_Name", "Fish_ID_Number"))
write.csv(mergedSizeTest, "mergedSizeTest.csv")

mergedSize <-filter(mergedSize_all , Length!= "DNE")

mergedSize$Length <- as.numeric(mergedSize$Length) 
mergedSize$Length <- round(mergedSize$Length ,digit=2)

mergedSize$Event <- toupper(mergedSize$Event)

outings1 <- read_excel("Rdsd_data_2020_2021-06-17.xlsx")
outings1$Event <- toupper(outings1$eventId)
outings1$FixedDate <-  as.Date(outings1$eventDay, "%Y/%m/%D")

outings2 <- outings1 
 
outings2$eventVillage[outings2$eventVillage=='Vaiare'] = 'Vaihere'
outings2$eventVillage[outings2$eventVillage=='Varari'] = 'Vaihere'
mergedSizeFull <- merge(mergedSize, outings2 ,all.x = TRUE,all.y = FALSE, by=c("Event"))

mergedSizeFull  <-filter(mergedSizeFull  , Fish_ID_Number>0)#this is temporary but should be unnecessary once aya fizes ids.


write.csv(mergedSizeFull, "mergedSizeRoadsideFull.csv")

mergedSize <- select(mergedSizeFull, "Photo_Name", "Fish_ID_Number", "FixedDate", "Event","eventVillage","fisherCode",  "Length", "Sizer","Taxonomic_Code")



write.csv(mergedSize, "mergedSizeRoadside.csv")
save(mergedSize, file = "mergedSize.Rdata")



countSize <- mergedSize   %>%
  group_by(Taxonomic_Code) %>%
  summarise(n = n(), avg_size = mean(Length))


countSize2 <- mergedSize   %>%
  group_by(Taxonomic_Code, Sizer) %>%
  summarise(n = n(),avg_size = mean(Length))


countSize3 <- mergedSize   %>%
  group_by(Taxonomic_Code, Sizer) %>%
  summarise(n = n(),avg_size = mean(Length))


countSize3 <-countSize3 %>% 
  group_by(Taxonomic_Code) %>%
  mutate(n_all = sum(n))

countSize4 <-countSize3 %>% 
  filter(n_all>50)

  dev.new()
p1<-ggplot(data=countSize4, aes(x=Taxonomic_Code, y=avg_size, fill=Sizer),
) +
  geom_bar(stat="identity", position=position_dodge())+
  ylab('Avg Size cm')
#xlim(1,12)+ scale_x_continuous(breaks = seq(1, 12, by = 1))
p1+ coord_flip()+ theme(text = element_text(size = 16))  +
  theme(legend.position="bottom")  

dev.new()
p1<-ggplot(data=countSize4, aes(x=Taxonomic_Code, y=n_all, fill=Sizer),
) +
  geom_bar(stat="identity", position=position_dodge())+
  ylab('Total caught')
#xlim(1,12)+ scale_x_continuous(breaks = seq(1, 12, by = 1))
p1+ coord_flip()+ theme(text = element_text(size = 16))  +
  theme(legend.position="bottom")  




  

  

                           
    
  
  sizeTrendPlot <- function(mergedSize, TaxaName)
  {
#TaxaName = "Scaridae"
  dev.new()
      
      p1 <- ggplot(filter(mergedSize, Taxonomic_Code == TaxaName) , aes(x=FixedDate, y=Length)) +
      geom_point() + ylab(TaxaName)+
      geom_smooth(method=lm)
      

      p2 <- ggplot(filter(mergedSize, Taxonomic_Code == TaxaName) , aes(x=FixedDate, y=Length, color=fisherCode)) +
      geom_point() + ylab(TaxaName)+
      geom_smooth(method=lm, aes(fill=fisherCode))
      grid.arrange(p1,p2, ncol=2)
      
      #grid.arrange(p1, ncol=1)
      
    
      
      model <- glm(formula = Length ~ FixedDate, family = gaussian, data = filter(mergedSize, Taxonomic_Code == TaxaName))
      summary(model)
      
      model2 <- lmer(formula = Length ~ FixedDate + (1|eventVillage), data = filter(mergedSize, Taxonomic_Code == TaxaName))
      summary(model2)
      
      # extract coefficients
      coefs <- data.frame(coef(summary(model2)))
      # use normal distribution to approximate p-value
      coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
      coefs
      
      
      reg <- lm(formula = Length ~ FixedDate, data = filter(mergedSize, Taxonomic_Code == TaxaName))
      regSummary = summary(reg)
      
      data = filter(mergedSize, Taxonomic_Code == TaxaName)
      PctChange = round((regSummary$coefficients[2,1]*365)/mean(data$Length)*100,0)
      
      
     resultsRow <-(c(TaxaName, reg$df.residual, PctChange, round(regSummary$r.squared,2) , round(regSummary$coefficients[2,4],3) ))
       
  }
  
 
  
  resultsRow <-sizeTrendPlot(mergedSize, "Scaridae")
   ResultsTable<- resultsRow

   resultsRow <-sizeTrendPlot(mergedSize, "Myripristis")
ResultsTable<- rbind(ResultsTable, resultsRow)

resultsRow <-sizeTrendPlot(mergedSize, "Mulloidichthys")
ResultsTable<- rbind(ResultsTable, resultsRow)
      
resultsRow <-sizeTrendPlot(mergedSize, "Parupeneus")
ResultsTable<- rbind(ResultsTable, resultsRow)

resultsRow <-sizeTrendPlot(mergedSize, "Holocentridae")
ResultsTable<- rbind(ResultsTable, resultsRow)
          
resultsRow <-sizeTrendPlot(mergedSize, "Siganus spinus")
ResultsTable<- rbind(ResultsTable, resultsRow)

resultsRow <-sizeTrendPlot(mergedSize, "Siganus argenteus")
ResultsTable<- rbind(ResultsTable, resultsRow)

resultsRow <-sizeTrendPlot(mergedSize, "Lethrinidae")
ResultsTable<- rbind(ResultsTable, resultsRow)

resultsRow <-sizeTrendPlot(mergedSize, "Epinephelus")
ResultsTable<- rbind(ResultsTable, resultsRow)

resultsRow <-sizeTrendPlot(mergedSize, "Sargocentron")
ResultsTable<- rbind(ResultsTable, resultsRow)

resultsRow <-sizeTrendPlot(mergedSize, "Cephalopholis argus")
ResultsTable<- rbind(ResultsTable, resultsRow)

resultsRow <-sizeTrendPlot(mergedSize, "Naso unicornis")
ResultsTable<- rbind(ResultsTable, resultsRow)

resultsRow <-sizeTrendPlot(mergedSize, "Acanthurus nigricauda")
ResultsTable<- rbind(ResultsTable, resultsRow)


write.csv(ResultsTable, "ResultsTable.csv")