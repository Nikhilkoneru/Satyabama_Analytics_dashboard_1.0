timestamp()
if (Sys.getenv("R_ARCH")=="/x64"){
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_101')
}else{
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_111')}
library(pdftools)
library(plyr)
library(ggplot2)
library(plotrix)
library(plotly)
library(reshape2)
library(gridExtra)
library(grid)
library(readxl)
library(XLConnect)
library(qdapTools)
library(readxl)
library(xlsx)
library(data.table)
library(gridExtra)
shinyServer(function(input, output) {
  dataset1=reactive({ 
    validate(
      need(input$uploadFile1, 'upload'),
      need(input$uploadFile2 , 'upload'),
      need(input$uploadFile3 , 'upload'),
      need(input$sub, 'upload'),
      need(input$selectexam,'SelectExamination')
    )
    error = ""
    try({
      inFile1 <- input$uploadFile1
    },silent = TRUE)
    try({
      inFile2 <- input$uploadFile2
    },silent = TRUE)
    try({
      inFile3 <- input$uploadFile3
    },silent = TRUE)
    studentdata<-readWorksheetFromFile(inFile2$datapath, 1)
    subsub<-readWorksheetFromFile(inFile3$datapath, 1)
    noofsub <- input$sub
    wewant <- (noofsub*2)-1
    if(input$selectexam==1){
    txt <- pdf_text(inFile1$datapath)
    x <- ""
    for (i in 1:length(txt)){
      x = paste0(x,txt[i])
    }
    error <- "Error:please clean the data"
    actualtext <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "",x))
    d <- strsplit(actualtext, ",", fixed=TRUE)
    removingheader <- d[[1]]
    removingpasstag <- removingheader[removingheader != "PASS"]
    removingfailtag <- removingpasstag[removingpasstag != "FAIL"]
    maindata <- removingfailtag
    c <- list()
    j <- 1
    specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
    for(i in 1:length(maindata))
    {
      suppressWarnings(try(
        if((specify_decimal(strwidth(maindata[i],units = 'in'),8)=="0.65625000")&(is.na(as.numeric(maindata[i]))==FALSE)){
          c[j] <- c(i)  
          j = j+1
        },silent=TRUE))
    }
    h <- list()
    for(i in 1:length(c)){
      suppressWarnings(try(
        if(i<(length(c))){
          h[[i]]<-c(maindata[c[[i]]:(c[[i+1]]-2)])
        }else{
          h[[i]]<-c(maindata[c[[i]]:length(maindata)])
        },silent=TRUE))
    }
    g<- list()
    for(i in 1:length(h)){
      for(j in 1:length(h[[i]])){
        suppressWarnings(try(
          if((substr(h[[i]][j], 2, 2)=="-")|(substr(h[[i]][j], 3, 3)=="-")){
            g[[i]] <- h[[i]][-c(2:j)]
          },silent = TRUE))
      }
    }
    rm(txt,actualtext,d,removingheader,removingpasstag,removingfailtag,maindata,h)
    for(i in 1:length(g)){
      for(j in 1:length(g[[i]])){
        suppressWarnings(try(
          if(is.na(as.numeric(g[[i]][j]))==TRUE){
            g[[i]][j] <- strtrim(g[[i]][j],8)
          },silent=TRUE))
      }
    }
    k <- list()
    suppressWarnings(try(for(i in 1:length(g)){
      l <- 1
      for(j in 1:length(g[[i]])){
        suppressWarnings(try(
          if((nchar(g[[i]][j])==8)&(is.na(as.numeric(g[[i]][j]))==TRUE)){
            if(l==1){
              k[i][l] <- g[[i]][j-1]
            }else{
              k[[i]][l] <- g[[i]][j-1] 
            }
            k[[i]][l+1] <- g[[i]][j]
            l = l+2
          },silent=TRUE))
      }
      k[[i]][l] <- g[[i]][length(g[[i]])]
    },silent=TRUE))
   
    if(length(k)==length(c))
    {
      print("SUCCESS")
      error = "SUCCESS"
    }else{
      print("ERROR:CAME PLEASE CLEAN THE DATA")
      error = "ERROR:CAME PLEASE CLEAN THE DATA"
    }
    finallist <- list()
    suppressWarnings(try(
      for(i in 1:length(k)){
        finallist[[i]] <- k[[i]][c(1,(length(k[[i]])-wewant):(length(k[[i]])))]
      },silent=TRUE))
    
    
    xx <- lapply(finallist, unlist)
    max <- max(sapply(xx, length))
    aaa <- do.call(rbind, lapply(xx, function(z)c(z, rep(NA, max-length(z)))))
    aaa <- data.frame(aaa,stringsAsFactors = FALSE)
    sublist <- vector()
    removenames <- vector()
    sublist[1] <- c("regno")
    
    f <- 2
    h <- 1
    suppressWarnings(try(
      for(j in 1:length(aaa)){
        
        if((nchar(aaa[1,][j])==8)&(is.na(as.numeric(aaa[1,][j]))==TRUE)){
          sublist[f] <- unname(unlist(aaa[1,][j]))
          removenames[h] <- c(j) 
          f <- f+1
          h <- h+1
        }
      },silent=TRUE))
    final <- aaa[-c(removenames)]
    names(final) <- c(sublist)
    final1 <- final
    cols.num <- names(final1)
    final1[cols.num] <- suppressWarnings(try( sapply(final1[cols.num],as.numeric),silent=TRUE))
    #inputdata
    my_data <- final
    my_data[2:(noofsub+1)][my_data[2:(noofsub+1)]=="***"] <- "AAA"
    }else if(input$selectexam==2){
      datas <- readWorksheetFromFile(inFile1$datapath, 1)
      datas <- datas[,-c(1,3)]
      colnames(datas) <- c("regno",colnames(datas)[2:length(colnames(datas))])
      my_data <- datas
      my_data[2:(noofsub+1)][my_data[2:(noofsub+1)]=="a"] <- "AAA"
    }
    ######################CLEANING DATA AND MAKING DATAFRAME READY ELIMINATING STUDENTS ABSENTS IN THE EXCELL SHEET AND CONVERTING TO ZERO
    Abscentlist<- my_data[apply(my_data[2:(noofsub+1)], MARGIN = 1, function(x) any(x =="AAA")),]
    my_data[2:(noofsub+1)][my_data[2:(noofsub+1)]=="AAA"] <- 0
    my_data[2:(noofsub+1)][my_data[2:(noofsub+1)]=="NA"] <- 0
    my_data[2:(noofsub+1)] <- sapply(my_data[2:(noofsub+1)], as.numeric)
    my_data <- merge(my_data,studentdata,by = c("regno","regno"),all.x = T, sort=F)
    noofcolumns <- noofsub+5
    my_data <- my_data[,c(1,noofcolumns-3,noofcolumns-2,noofcolumns-1,noofcolumns,2:(noofcolumns-4))]
    if(input$selectexam==1){
    sorteddataframe <- transform(my_data, Total=rowSums(my_data[,6:noofcolumns]),Percentage =rowSums(my_data[,6:noofcolumns])/(noofsub),Noofpass = rowSums(my_data[,6:noofcolumns] >= 50),Nooffail = noofsub-rowSums(my_data[,6:noofcolumns] >= 50))
    }else if(input$selectexam==2){
      sorteddataframe <- transform(my_data, Total=rowSums(my_data[,6:noofcolumns]),Percentage =rowSums(my_data[,6:noofcolumns])/(noofsub/2),Noofpass = rowSums(my_data[,6:noofcolumns] >= 25),Nooffail = noofsub-rowSums(my_data[,6:noofcolumns] >= 25))
    }
    finalxlsl <- sorteddataframe[with(sorteddataframe,order(Percentage,decreasing = TRUE)),]
    finalxlsl <- transform(finalxlsl,DeptRank= c(1:dim(finalxlsl)[1]))
    df_list <- split(finalxlsl, as.factor(finalxlsl$section))
    for(i in 1:length(df_list)){
      df_list[[i]] <-  transform(df_list[[i]],classRank= c(1:dim(df_list[[i]])[1]))
    }
    df.Base <- do.call("rbind", df_list)
    finalprint <- df.Base[with(df.Base,order(DeptRank,decreasing = FALSE)),]
    savingxl <- finalprint
    savingxl[6:noofcolumns][match(Abscentlist$regno, savingxl$regno), ] <-Abscentlist[2:(noofsub+1)]
    nikhiltest <- data.frame(names(finalxlsl[6:noofcolumns]))
    names(nikhiltest) <- c("subcode")
    namesofsub <- merge(nikhiltest,subsub,by = c("subcode","subcode"),all.x = T, sort=F)
    subcolnames <-c("regno","rollno","names","gender","section",namesofsub[[2]],"Total","Percentage","Noofpass","Nooffail","DeptRank","ClassRank")
    one <- finalprint[6:noofcolumns]/10
    roundUpNice <- function(x, nice=c(1,2,3,4,5,6,7,8,9,10)) {
      if(length(x) != 1) stop("'x' must be of length 1")
      10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
    }
    cgpa <- sapply(one, function(x){
      sapply(x, function(x){
        roundUpNice(x)
      })
    })
    cgpa <- data.frame(cgpa)
    colmulti <- namesofsub[[3]]
    for (i in 1:length(cgpa)){
      cgpa[i] <- cgpa[i]*colmulti[i]
    }
    cgpa <- rowSums(cgpa)/sum(colmulti)
    finalprint <- transform(finalprint,CGPA=cgpa)
    savingxl  <- transform(savingxl,CGPA=cgpa)
    colnames(finalprint) <- c(subcolnames,"CGPA")
    colnames(savingxl) <- c(subcolnames,"CGPA")
    finalprint <- finalprint[,c(1:(noofcolumns+2),noofcolumns+7,(noofcolumns+3):(noofcolumns+6))]
    savingxl <- savingxl[,c(1:(noofcolumns+2),noofcolumns+7,(noofcolumns+3):(noofcolumns+6))]
    list_of_df<- list(finalprint,savingxl,error)
    if(input$selectexam==1){
      rm(aaa,Abscentlist,df.Base,final,final1,finalprint,finalxlsl,my_data,namesofsub,nikhiltest,one,savingxl,sorteddataframe,studentdata,c,cgpa,colmulti,df_list,f,finallist,g,h,j,k,i,cols.num,l,max,noofcolumns,noofsub,removenames,subcolnames,sublist,wewant,x,xx)
    }else{
      rm(datas,Abscentlist,my_data,noofcolumns,colmulti,sorteddataframe,finalxlsl,df_list,df.Base,finalprint,savingxl,nikhiltest,namesofsub,subcolnames,one,cgpa)  
    }
    return(list_of_df)
  })
  
  
  summarymain <- function(summarydata){
    mina <- vector()
    firstquarda <- vector()
    mediana <- vector()
    meana  <- vector()
    thirdquarda  <- vector()
    maxa     <- vector()
    listofeach <- 6*dim(summarydata[[1]])[2]
    minf <- 1
    firstquardf <- 1
    medianf <- 1
    meanf <- 1
    thirdquardf <- 1
    maxf <- 1
    for( f in 1:length(summarydata)){
      e <- 1
      while(e <= listofeach){
        mina[minf] <- summarydata[[f]][e]
        minf <- minf+1
        e <- e+1
        firstquarda[firstquardf] <- summarydata[[f]][e]
        firstquardf <- firstquardf+1
        e <- e+1
        mediana[medianf] <- summarydata[[f]][e]
        medianf <- medianf+1
        e <- e+1
        meana[meanf] <- summarydata[[f]][e]
        meanf <- meanf+1
        e <- e+1
        thirdquarda[thirdquardf] <- summarydata[[f]][e]
        thirdquardf <- thirdquardf+1
        e <- e+1
        maxa[maxf] <- summarydata[[f]][e]
        maxf <- maxf+1
        e <- e+1
      }}
    a <- colnames(summarydata[[1]])
    p <- vector()
    name <- vector()
    rown <- names(summarydata)
    k <- 0
    w <- 1
    for(g in 1:length(mina)/length(colnames(summarydata[[1]])))
    {
      k <- k+1
      for (h in 1:length(colnames(summarydata[[1]]))){
        if (w <= length(mina)){
          p[w] <- a[h]
          name[w]  <- rown[k]
          w <- w+1
        }
        
      }
    }
    thesummary <- data.frame(name,p,mina,firstquarda,mediana,meana,thirdquarda,maxa)
    names(thesummary) <- c("SECTION","SUBJECT","MINIMUM-MARK","1ST-QUARDINENT","MEDIAN","MEAN","3RD-QUARDINENT","MAXIMUM-MARK")
    return(thesummary)
  }
  mysummary <- function(tempdata1,tempdata2) {
    
    validate(
      need(input$uploadFile1, ''),
      need(input$uploadFile2 , ''),
      need(input$sub, ''),
      need(input$selectexam,'SelectExamination')
    )
    noofpasslist <- vector()
    nooffaillist <- vector()
    noofabscents <- vector()
    noofpresent  <- vector()
    noofabovesf  <- vector()
    avg          <- vector()
    avg2         <- vector()
    total        <- vector()
    passperc     <- vector()
    passperc2     <- vector()
    j <- 1
    noofcolumns <- input$sub+5
    for (i in 6:noofcolumns){
      total[j]  <- dim(tempdata1)[[1]]
      if(input$selectexam==1){
      noofpasslist[j] <- length(tempdata1[i][apply(tempdata1[i] >= 50, 1, all),])
      nooffaillist[j] <- length(tempdata1[i][apply(tempdata1[i] < 50, 1, all),])
      }else{
        noofpasslist[j] <- length(tempdata1[i][apply(tempdata1[i] >= 25, 1, all),])
        nooffaillist[j] <- length(tempdata1[i][apply(tempdata1[i] < 25, 1, all),])
      }
      noofabscents[j] <- length(tempdata2[i][apply(tempdata2[i] == "AAA", 1, all),])
      noofpresent[j]  <- length(tempdata2[i][apply(tempdata2[i] != "AAA", 1, all),])
      
      avg[j]          <- format(round(colMeans(tempdata1[i])[[1]], 3), nsmall = 3)
      avg2[j]         <- format(round(mean(as.numeric((tempdata2[apply(tempdata2[i] != "AAA",1,all),][i])[[1]])), 3), nsmall = 3)
      passperc[j]    <- (noofpasslist[j]/noofpresent[j])*100
      passperc2[j]  <- (noofpasslist[j]/total[j])*100
      j <- j+1
    }
    dataf <- data.frame(total,noofpasslist,nooffaillist,noofpresent,noofabscents,passperc2,passperc,avg,avg2)   
    names(dataf) <- c("TOTAL NO OF STUDENTS","NOOFPASS","NOOFFAIL","NOOFPRESENT","NOOFABSENT","PASS-PERCENTAGE","PASS-PERCENTAGE(PRESENT-STUDENTS)","AVERAGE-MARK","AVERAGE-MARK(PRESENTSTUDENTS)")
    row.names(dataf) <- names(tempdata1[6:noofcolumns])
    dataf2 <- data.frame(
      dim(tempdata1)[[1]],
      paste0(format(round((dim(tempdata1[apply(tempdata1[noofcolumns+5] == 0, 1, all),])[[1]]/dim(tempdata1)[[1]])*100, 3), nsmall = 3),"%"),
      dim(tempdata1[apply(tempdata1[noofcolumns+5] == 0, 1, all),])[[1]],
      dim(tempdata1[apply(tempdata1[noofcolumns+5] == 1, 1, all),])[[1]],
      dim(tempdata1[apply(tempdata1[noofcolumns+5] == 2, 1, all),])[[1]],
      dim(tempdata1[apply(tempdata1[noofcolumns+5] == 3, 1, all),])[[1]],
      dim(tempdata1[apply(tempdata1[noofcolumns+5] > 3, 1, all),])[[1]],
      dim(tempdata1[apply(tempdata1[noofcolumns+2] >= 50, 1, all),])[[1]],
      dim(tempdata1[apply(tempdata1[noofcolumns+2] >= 75, 1, all),])[[1]],
      dim(tempdata1[apply(tempdata1[noofcolumns+2] >= 80, 1, all),])[[1]])   
    names(dataf2) <- c("TOTAL NO OF STUDENTS","PASS PERCENTAGE","CLEAREDALL","ONEARREAR","TWOARREAR","THREEARREAR","MORETHAN3ARREAR","Above 50%","Above 75%","Above 80%")
    dataf <- dataf[with(dataf,order(dataf$'PASS-PERCENTAGE',decreasing = TRUE)),]
    row.names(dataf2) <- c("count")
    
    list_of_df<- list(dataf,dataf2)
    return(list_of_df)
  }
  
  particularsub <- function(data1,selection){
    validate(
      need(input$uploadFile1, ''),
      need(input$uploadFile2 , ''),
      need(input$sub, '')
    )
    sub <- c(toString(selection))
    df <- data.frame(data1$regno,data1$rollno,data1$gender, data1$section,data1[sub][[1]],data1$Total,data1$Percentage)
    names(df) <- c("REGNO","ROLLNO","GENDER","SECTION",toString(selection),"Total","Percentage")
    return(df)
  }
  output$selectme <- renderUI({
    validate(
      need(input$uploadFile1, ''),
      need(input$uploadFile2 , ''),
      need(input$uploadFile3 , ''),
      need(input$sub, '')
    )
    noofcolumns <- input$sub+5
    listfromreacctive <- dataset1()
    finalprint <- listfromreacctive[[1]]
    selectInput("selectsection","Section", choices=c("ALL",unique(finalprint$section)), multiple = FALSE,
                selectize = TRUE, width = NULL, size = NULL)
  })
  output$projects<-  DT::renderDataTable({
    validate(
      need(input$uploadFile1, 'STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$selectsection,''),
      need(input$selectexam,'SelectExamination'),
      need(input$radio,'')
      
    )
    noofcolumns <- input$sub+5
    listfromreacctive <- dataset1()
    if(input$selectexam==2){
    listfromreacctive[[2]] <- listfromreacctive[[2]][-c(noofcolumns+3)]}
    if(input$radio==1){
      listfromreacctive[[2]] <- listfromreacctive[[2]][with(listfromreacctive[[2]],order(Percentage,decreasing = TRUE)),]
    }else{
      listfromreacctive[[2]] <- listfromreacctive[[2]][with(listfromreacctive[[2]],order(regno,decreasing = FALSE)),]
    }
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste0(input$selectsection,'STUDENTSMARKS', '.xls') 
      },
      content = function(file) {
        if(input$selectsection=="ALL"){
          write.xlsx2(listfromreacctive[[2]], file,row.names = FALSE)}
        else{
          write.xlsx2(subset(listfromreacctive[[2]],subset = listfromreacctive[[2]][5] == input$selectsection), file,row.names = FALSE)
        }
      }
    )
    if(input$selectsection=="ALL"){
      DT::datatable(listfromreacctive[[2]],options = list(scrollX = TRUE),rownames=FALSE)
    }else{
      DT::datatable(subset(listfromreacctive[[2]],subset = listfromreacctive[[2]][5] == input$selectsection),options = list(scrollX = TRUE),rownames=FALSE)
    }
  })
  output$projectsss<-  DT::renderDataTable({
    validate(
      need(input$uploadFile1, 'STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$selectsection,''),
      need(input$slider2,''),
      need(input$selectexam,'SelectExamination'),
      need(input$radio2,'')
    )
    noofcolumns <- input$sub+5
    listfromreacctive <- dataset1()
    if(input$radio2==1){
      listfromreacctive[[2]] <- listfromreacctive[[2]][with(listfromreacctive[[2]],order(Percentage,decreasing = TRUE)),]
    }else{
      listfromreacctive[[2]] <- listfromreacctive[[2]][with(listfromreacctive[[2]],order(regno,decreasing = FALSE)),]
    }
    if(input$selectexam==2){
      listfromreacctive[[2]] <- listfromreacctive[[2]][-c(noofcolumns+3)]}
    if(input$selectsection!="ALL"){
      printa <- subset(listfromreacctive[[2]],subset = listfromreacctive[[2]][5] == input$selectsection)}else{
        printa <- listfromreacctive[[2]]}
    sliderinput1 <- input$slider2
    output$downloadData1 <- downloadHandler(
      filename = function() { 
        paste0(input$selectsection,'-SECTION-',sliderinput1[1],":",sliderinput1[2],'STUDENTSMARKS', '.xls') 
      },
      content = function(file) {
        write.xlsx2(subset(printa,subset = printa$Percentage >= sliderinput1[1] & printa$Percentage <= sliderinput1[2]), file,row.names = FALSE)}
    )
    mynew <- subset(printa,subset = printa$Percentage >= sliderinput1[1] & printa$Percentage <= sliderinput1[2])
    DT::datatable(mynew,options = list(scrollX = TRUE),rownames=FALSE)
  })
  get_noti=function(){
    validate(
      need(input$uploadFile1, ''),
      need(input$uploadFile2 , ''),
      need(input$uploadFile3 , ''),
      need(input$sub, ''),
      need(input$selectsection,'')
      
    )
    listfromreacctive <- dataset1()
    finalprint <- listfromreacctive[[3]]
    if (finalprint=="SUCCESS"){
      notification <- notificationItem(icon = icon("thumbs-up"), status = "danger", paste0("CORE LOGIC :",finalprint))
    }else{
      notification <- notificationItem(icon = icon("exclamation-triangle"), status = "danger",finalprint)
    }
    notification$children[[1]] <- a(href="#shiny-tab-dashboard","onclick"=paste0("clickFunction('",paste0(substr(as.character(runif(1, 0, 1)),1,6),"noti"),"'); return false;"),list(notification$children[[1]]$children))
    return(notification)
  }
  output$dropdown=renderMenu({dropdownMenu(get_noti())})
  
  output$variants <- renderUI({
    validate(
      need(input$uploadFile1, 'STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$selectsection,'')
    )
    noofcolumns <- input$sub+5
    listfromreacctive <- dataset1()
    finalprint <- listfromreacctive[[1]]
    selectInput("subject","SELECT SUBJECT", choices=names(finalprint[6:noofcolumns]), multiple = FALSE,
                selectize = TRUE, width = NULL, size = NULL)
  })
  
  output$dt2<-  DT::renderDataTable({
    validate(
      need(input$uploadFile1, 'STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    if(input$selectsection!="ALL"){
      mydata <- mysummary(subset(listfromreacctive[[1]],subset = listfromreacctive[[1]][5] == input$selectsection),
                          subset(listfromreacctive[[2]],subset = listfromreacctive[[2]][5] == input$selectsection))
    }else{
      mydata <- mysummary(listfromreacctive[[1]],listfromreacctive[[2]])}
    output$downloadData5 <- downloadHandler(
      filename = function() { 
        paste0(input$selectsection,'-SECTION-SUBJECTS-INFO.xls') 
      },
      content = function(file) {
        write.xlsx2(mydata[[1]], file,row.names = TRUE)
      }
    )
    DT::datatable(mydata[[1]],options = list(scrollX = TRUE),rownames=TRUE)
  })
  output$dt3<-  DT::renderDataTable({
    validate(
      need(input$uploadFile1, 'STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    if(input$selectsection!="ALL"){
      mydata <- mysummary(subset(listfromreacctive[[1]],subset = listfromreacctive[[1]][5] == input$selectsection),
                          subset(listfromreacctive[[2]],subset = listfromreacctive[[2]][5] == input$selectsection))
    }else{
      mydata <- mysummary(listfromreacctive[[1]],listfromreacctive[[2]])}
    output$downloadData2 <- downloadHandler(
      filename = function() { 
        paste0(input$selectsection,'-SECTION-RESULTS-INFO.xls') 
      },
      content = function(file) {
        write.xlsx2(mydata[[2]], file,row.names = TRUE)
      }
    )
    DT::datatable(mydata[[2]],options = list(scrollX = TRUE),rownames=TRUE)
  })
  output$summary <- DT::renderDataTable({
    validate(
      need(input$uploadFile1, 'STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    if(input$selectsection!="ALL"){
      finalprint <- subset(listfromreacctive[[1]],subset = listfromreacctive[[1]][5] == input$selectsection)}
    else{finalprint <- listfromreacctive[[1]]}
    section <- finalprint$section
    dff <- summarymain(by(finalprint[6:noofcolumns],section,summary))
    output$downloadData3 <- downloadHandler(
      filename = function() { 
        paste0(input$selectsection,'-SECTION-summary-INFO.xls') 
      },
      content = function(file) {
        write.xlsx2(dff, file,row.names = TRUE)
      }
    )
    DT::datatable(dff,options = list(scrollX = TRUE),rownames=TRUE)
  })
  
  output$subjectwise<-  DT::renderDataTable({
    validate(
      need(input$uploadFile1, 'STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$subject,''),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    if(input$selectsection!="ALL"){
      printa <- subset(listfromreacctive[[2]],subset = listfromreacctive[[2]][5] == input$selectsection)
    }else{
      printa <- listfromreacctive[[2]]}
    selection <- input$subject
    mynew1 <- particularsub(printa,selection)
    #mynew1 <- mynew1[with(mynew1,order(mynew1[5],decreasing = TRUE)),]
    output$downloadData4 <- downloadHandler(
      filename = function() { 
        paste0(input$selectsection,'-SECTION-',selection,'-.xls') 
      },
      content = function(file) {
        write.xlsx2(mynew1, file,row.names = FALSE)
      }
    )
    DT::datatable(mynew1,options = list(scrollX = TRUE),rownames=FALSE)
  })
  output$abscentlist<-  DT::renderDataTable({
    validate(
      need(input$uploadFile1, 'STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$selectsection,''),
      need(input$selectexam,'SelectExamination')
      
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    savingxl <- listfromreacctive[[2]]
    
    if(input$selectexam==2){
      savingxl <- listfromreacctive[[2]][-c(noofcolumns+3)]}
    final1 <- savingxl[apply(savingxl[6:noofcolumns], MARGIN = 1, function(x) any(x =="AAA")),]
    output$downloadData6 <- downloadHandler(
      filename = function() { 
        paste0(input$selectsection,'SECTION-ABSCENT', '.xls') 
      },
      content = function(file) {
        write.xlsx2(final1, file,row.names = FALSE)
      }
    )
    DT::datatable(final1,options = list(scrollX = TRUE),rownames=FALSE)
  })
  output$progressBox1 <- renderValueBox({
    validate(
      need(input$uploadFile1, 'UPLOAD STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'UPLOAD STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'UPLOAD SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    if(input$selectsection!="ALL"){
      finalprint1 <- subset(listfromreacctive[[1]],subset = listfromreacctive[[1]][5] == input$selectsection)
      finalprint2 <- subset(listfromreacctive[[2]],subset = listfromreacctive[[2]][5] == input$selectsection)}
    else{
      finalprint1 <- listfromreacctive[[1]]
      finalprint2 <- listfromreacctive[[2]]
    }
    mydata <- mysummary(finalprint1,finalprint2)
    valueBox(
      mydata[[2]][[2]],"PASS PERCENTAGE", icon = icon("list")
    )
  })
  output$progressBox2 <- renderValueBox({
    validate(
      need(input$uploadFile1, ''),
      need(input$uploadFile2 , ''),
      need(input$uploadFile3 , ''),
      need(input$sub, ''),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    if(input$selectsection!="ALL"){
      finalprint1 <- subset(listfromreacctive[[1]],subset = listfromreacctive[[1]][5] == input$selectsection)
      finalprint2 <- subset(listfromreacctive[[2]],subset = listfromreacctive[[2]][5] == input$selectsection)
    }else{
      finalprint1 <- listfromreacctive[[1]]
      finalprint2 <- listfromreacctive[[2]]
    }
    mydata <- mysummary(finalprint1,finalprint2)
    valueBox(
      mydata[[2]][[3]],"NO OF STUDENTS CLEARED ALL SUBJECTS", icon = icon("list")
    )
  })
  output$progressBox3 <-renderValueBox({
    validate(
      need(input$uploadFile1, ''),
      need(input$uploadFile2 , ''),
      need(input$uploadFile3 , ''),
      need(input$sub, ''),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    if(input$selectsection!="ALL"){
      finalprint1 <- subset(listfromreacctive[[1]],subset = listfromreacctive[[1]][5] == input$selectsection)
    finalprint2 <- subset(listfromreacctive[[2]],subset = listfromreacctive[[2]][5] == input$selectsection)}
    else{
      finalprint1 <- listfromreacctive[[1]]
      finalprint2 <- listfromreacctive[[2]]
    }
    mydata <- mysummary(finalprint1,finalprint2)
    valueBox(
      row.names(mydata[[1]])[1],"HIGHEST AVERAGE SUBJECT", icon = icon("list")
    )
  })
  output$progressBox4 <- renderInfoBox({
    validate(
      need(input$uploadFile1, ''),
      need(input$uploadFile2 , ''),
      need(input$uploadFile3 , ''),
      need(input$sub, ''),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    if(input$selectsection!="ALL"){
      dfd <- subset(listfromreacctive[[1]],subset = listfromreacctive[[1]][5] == input$selectsection)}
    else{
      dfd <- listfromreacctive[[1]]
    }
    infoBox(
      "1ST RANK", paste0(dfd[1,][3],"-",dfd[1,][noofcolumns+2],"%"),  icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  output$progressBox5 <- renderInfoBox({
    validate(
      need(input$uploadFile1, ''),
      need(input$uploadFile2 , ''),
      need(input$uploadFile3 , ''),
      need(input$sub, ''),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    if(input$selectsection!="ALL"){
      dfd <- subset(listfromreacctive[[1]],subset = listfromreacctive[[1]][5] == input$selectsection)}
    else{
      dfd <- listfromreacctive[[1]]
    }
    infoBox(
      "2ND RANK", paste0(dfd[2,][3],"-",dfd[2,][noofcolumns+2],"%"),  icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    )
  })
  output$graph6 <- renderPlotly({
    validate(
      need(input$uploadFile1, 'STUDENTS MARKS PDF FILE'),
      need(input$uploadFile2 , 'STUDENTS DETAILS XLS FILE'),
      need(input$uploadFile3 , 'SUBJECT CODE XLS FILE'),
      need(input$sub, 'ENTER NO OF SUBJECTS'),
      need(input$regreg,'')
      
    )
    noofcolumns <- input$sub+5
    listfromreacctive <- dataset1()
    finalprint <- listfromreacctive[[1]]
    sectionmarkslist <- subset(finalprint,subset = regno == input$regreg)
    ll2 <- sectionmarkslist[6:noofcolumns]
    a <- melt(ll2)
    ploter <- suppressWarnings(try(plot_ly(a,x = ~variable,y= ~value,color = ~variable)%>%
      layout(title=paste0(sectionmarkslist[[3]],"-",input$regreg)),silent = TRUE))
    return(ploter)
  })
  output$progressBox6 <- renderInfoBox({
    validate(
      need(input$uploadFile1, ''),
      need(input$uploadFile2 , ''),
      need(input$uploadFile3 , ''),
      need(input$sub, ''),
      need(input$selectsection,'')
    )
    listfromreacctive <- dataset1()
    noofcolumns <- input$sub+5
    if(input$selectsection!="ALL"){
      dfd <- subset(listfromreacctive[[1]],subset = listfromreacctive[[1]][5] == input$selectsection)}
    else{
      dfd <- listfromreacctive[[1]]
    }
    infoBox(
      "3RD RANK", paste0(dfd[3,3],"-",dfd[3,noofcolumns+2],"%"),  icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
})
