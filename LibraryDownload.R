list.of.packages <- c("shinydashboard","DT","plotly","shiny","pdftools","plyr", "ggplot2","plotrix","plotly","reshape2","gridExtra","grid","readxl","XLConnect","qdapTools","readxl","xlsx","data.table","gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages,new.packages)
print("Downloaded all packages now run server.R")
