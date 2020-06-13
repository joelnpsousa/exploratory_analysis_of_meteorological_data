glimpse(df) #df overview

df <- df[!(is.na(df$X2)),] 
df <- df[!(df$dataCol == ""), ]  #remove columns with empty date

df$data <- paste(df$X2,df$dataCol)   #create column data
df <- df[!(df$data == "31 Abril 1876"), ]  #remove april 31 row
df[1:2] <- list(NULL)  # delete columns X2 and dataCol

df <- select(df, data, everything()) # reorder columns
names(df) <- c("data","pressao9h","pressao12h","pressao15h","pressao.media",
               "temp.9h.exp","temp.9h.som","temp.12h.exp","temp.12h.som","temp.15h.exp",
               "temp.15h.som","temp.maxima","temp.minima","temp.media",
               "tensao9h","tensao12h","tensao15h",
               "humidade9h","humidade12h","humidade15h",
               "precipitacao","ozono","rumo9h","rumo12h","rumo15h",
               "velocidade.absoluta","velocidade.horaria") #rename columns

df <- df %>% mutate(across(starts_with("rumo"), toupper))

df$rumo9h <- gsub("\\..*| .*","",df$rumo9h)
df$rumo9h <- gsub("O|o", "W", df$rumo9h)
df$rumo9h <- gsub("FRESCO|F|NNEM|ESSE", "", df$rumo9h)

unique(df$rumo9h)
df %>% count(rumo9h) %>% arrange(n)

df$rumo12h <- gsub("\\..*| .*|,.*|:.*","",df$rumo12h)
df$rumo12h <- gsub("O|o", "W", df$rumo12h)
df$rumo12h <- gsub("EE|NWW|P|SEE|WENW|WFCW|WNSW|ESSE", "", df$rumo12h)

df$rumo15h <- gsub("\\..*| .*|:.*", "", df$rumo15h)
df$rumo15h <- gsub("O|o", "W", df$rumo15h)
df$rumo15h <- gsub("Ã‘|EESE|M|NNWE|SNW|WSWM|ESSE","",df$rumo15h)

df$rumo9h[df$rumo9h==""]<-NA
df$rumo12h[df$rumo12h==""]<-NA
df$rumo15h[df$rumo15h==""]<-NA

df <- df %>% mutate(across(starts_with("rumo"), as.factor)) #change column types 

#pre2 <- df

ix <- c(2:22,26:27)
df[ix] <- lapply(df[ix], as.numeric)  #change column types

#column date
strptime(df$data, "%m/%d/%Y")
df$data <- parse_date(df$data,"%d %B %Y",locale=locale("pt"))
mutate(df, data=as.Date(data, format= "%y-%m-%d"))