library(readxl)
library(xlsx)
library(tidyverse)
library(magrittr)
library(data.table)
library(gdata)

#### Importation ----
files.list <- list.files(path="transformed_data",recursive=T,pattern='*.xls')  #get files list from folder

df1 <- data.frame()
df2 <- data.frame()
for (i in 1:length(files.list)){                                           
  wb <- loadWorkbook(files.list[i])           #select a file & load workbook
  sheet <- getSheets(wb)                      #get sheet list
  meses <- names(sheet)
  meses <- meses[meses != "Resumo Anual"]
  print(files.list[i])

  for (j in 1:length(meses)){
    # DF1
    print(meses[j])
    dataCol <- gsub("\\(2)|Dezembro 1881", "", meses[j])
    
    tmp<-read.xlsx(files.list[i], sheetIndex=j, colIndex= c(2:26), startRow=20, endRow=52, as.data.frame=TRUE, header=F)
    tmp <- tbl_df(tmp) %>% mutate_all(as.character)
    dataCol <- rep(dataCol, nrow(tmp))
    
    tmp <- cbind(dataCol, tmp)
    df1 <- rbind(df1,tmp)
    
    # DF2 (Anemometro)
    dataCol2 <- gsub("\\(2)|Dezembro 1881", "", meses[j])
    
    if(!(meses[j]=="Dezembro 1867" || meses[j]=="Janeiro 1868" || meses[j]=="Fevereiro 1868")){
      anemometroCell <- read.xlsx(files.list[i], sheetIndex=j, colIndex= 27, startRow=12, endRow=12, as.data.frame=FALSE, header=F)
      anemometroCell2 <- read.xlsx(files.list[i], sheetIndex=j, colIndex= 27, startRow=10, endRow=10, as.data.frame=FALSE, header=F)
      
      if(anemometroCell != "" && length(anemometroCell) > 0 && anemometroCell == "ANEMOMETRO") {
        tmp2<-read.xlsx(files.list[i], sheetIndex=j, colIndex= c(2,27:28), startRow=20, endRow=52, as.data.frame=TRUE, header=F)
        if (nrow(tmp2) != nrow(tmp)) {
          diff_rows <- nrow(tmp) - nrow(tmp2)
          diff_df <- data.frame(matrix(data = NA, nrow = diff_rows, ncol = 3))
          diff_df <- cbind(dataCol2, diff_df)
          names(diff_df) <- names(df2)
          
          tmp2 <- tbl_df(tmp2) %>% mutate_all(as.character)
          dataCol2 <- rep(dataCol2, nrow(tmp2))
          
          tmp2 <- cbind(dataCol2, tmp2)
          names(df2) <- names(tmp2)
          df2 <- rbind(df2, tmp2)
          df2 <- rbind(df2, diff_df)
          
        } else {
          tmp2 <- tbl_df(tmp2) %>% mutate_all(as.character)
          dataCol2 <- rep(dataCol2, nrow(tmp2))
          
          tmp2 <- cbind(dataCol2, tmp2)
          names(df2) <- names(tmp2)
          df2 <- rbind(df2, tmp2)
        }
      }
      else if(anemometroCell2 != "" && length(anemometroCell2) > 0 && anemometroCell2 == "ANEMOMETRO") {
        tmp2<-read.xlsx(files.list[i], sheetIndex=j, colIndex= c(2,27:28), startRow=20, endRow=52, as.data.frame=TRUE, header=F)
        if (nrow(tmp2) != nrow(tmp)) {
          diff_rows <- nrow(tmp) - nrow(tmp2)
          diff_df <- data.frame(matrix(data = NA, nrow = diff_rows, ncol = 3))
          diff_df <- cbind(dataCol2, diff_df)
          names(diff_df) <- names(df2)
          
          tmp2 <- tbl_df(tmp2) %>% mutate_all(as.character)
          dataCol2 <- rep(dataCol2, nrow(tmp2))
          
          tmp2 <- cbind(dataCol2, tmp2)
          names(df2) <- names(tmp2)
          df2 <- rbind(df2, tmp2)
          df2 <- rbind(df2, diff_df)
          
        } else {
          tmp2 <- tbl_df(tmp2) %>% mutate_all(as.character)
          dataCol2 <- rep(dataCol2, nrow(tmp2))
          
          tmp2 <- cbind(dataCol2, tmp2)
          names(df2) <- names(tmp2)
          df2 <- rbind(df2, tmp2)
        }
      }
      else {
        # Catch Missing Anemometro Data
        df_empty <- data.frame(matrix(data = NA, nrow = 33, ncol = 3))
        dataCol2 <- rep(dataCol2, nrow(df_empty))
        df_empty <- cbind(dataCol2, df_empty)
        names(df_empty) <- names(df2)
        df2 <- rbind(df2, df_empty)
        remove(df_empty)
      }
    } else {
      # Catch Invalid Anemometro Data
      df_empty <- data.frame(matrix(data = NA, nrow = 33, ncol = 3))
      dataCol2 <- rep(dataCol2, nrow(df_empty))
      df_empty <- cbind(dataCol2, df_empty)
      names(df_empty) <- names(df2)
      df2 <- rbind(df2, df_empty)
      remove(df_empty)
    }
  }
}

df2[1:2] <- list(NULL)
df <- cbind(df1, df2)