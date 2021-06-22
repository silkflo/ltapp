#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 
#

library(readr)
library(magrittr)
library(lazytrade)
library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
library(DT)
library(plotly)
library(randomcoloR)


app_server <- function( input, output, session ) {
  
  
  #-----------DATA MANAGEMENT-------------- 
  file_path <- reactive({
    
    #  Terminals <- normalizePath(Sys.getenv('PATH_T1'), winslash = '/')
    #  file_path <- paste0(Terminals,"/OrdersResultsT",1,".csv")      
    Terminals <- normalizePath(Sys.getenv(paste0('PATH_T',input$Terminal)), winslash = '/')
    file_path <- paste0(Terminals,"/OrdersResultsT",input$Terminal,".csv")
  })
  #-----------------------------------------
  DF_Stats <- reactive({ 
    
    if(file.exists(file_path())){
      DF_Stats <- read.csv(file_path(), col.names = c("MagicNumber","Ticket","EntryTime","ExitTime","Profit","Symbol","Type"))
      
      DF_Stats <- data.frame(MagicNumber = DF_Stats$MagicNumber,
                             Ticket = DF_Stats$Ticket,
                             EntryTime = as.POSIXct(DF_Stats$EntryTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                             ExitTime = as.POSIXct(DF_Stats$ExitTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                             Profit = DF_Stats$Profit,
                             Symbol = DF_Stats$Symbol,
                             Type = DF_Stats$Type)
    }else{"NO DATA"}
  })
  #---------------------------------------      
  magicNumber <- reactive({
    if(file.exists(file_path())){
      unique(DF_Stats()$MagicNumber)
    }else{"NO DATA"}
  })
  #---------------------------------------    
  symbol <- reactive({
    if(file.exists(file_path())){  
      symbol <-  unique(DF_Stats()$Symbol)
      symbol[order(symbol)]
    }else{"NO DATA"}
  })
  #---------------------------------------
  pair <- reactive({
    if(file.exists(file_path())){    
      as.vector(unique(DF_Balance()$Symbol))
    }else{"NO DATA"}
  })
  
  
  #---------------------------------------    
  
  #-----------MANAGE SIDEBAR-------------    
  #Refresh data 
  observeEvent(input$Refresh,{
    updateSelectInput(session, inputId = "MagicNum", label = NULL, choices = c("All",magicNumber()), selected = NULL)
    updateSelectInput(session, inputId = "Symbol", label = NULL, choices = c("All",symbol()), selected = NULL)
    
    #  path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    #  
    #  
    #  
    #  Terminals <- normalizePath(Sys.getenv(paste0('PATH_T',input$Terminal)), winslash = '/')
    #  
    #  acc_file_T1 <- file.path(path_user, "_DATA/AccountResultsT1.rds")
    #  acc_file_T3 <- file.path(path_user, "_DATA/AccountResultsT3.rds")
    #  
    #  
    #  if(input$Terminal == 1 ){
    #    DFT1Act <- read_csv(file.path(Terminals, 'AccountResultsT1.csv'),
    #                        col_names = c("DateTime", "Balance", "Equity","Profit"),
    #                        col_types = "cddd")
    #    DFT1Act$DateTime <- ymd_hms(DFT1Act$DateTime)
    #    if (!file.exists(acc_file_T1)) {
    #      write_rds(DFT1Act, acc_file_T1)
    #    } else {
    #      read_rds(acc_file_T1) %>% 
    #        bind_rows(DFT1Act) %>% 
    #        arrange(DateTime) %>% 
    #        head(1000) %>% 
    #        write_rds(acc_file_T1)
    #    }
    #  }
    #  if(input$Terminal == 3 ){
    #    DFT3Act <- read_csv(file.path(Terminals, 'AccountResultsT3.csv'),
    #                        col_names = c("DateTime", "Balance", "Equity","Profit"),
    #                        col_types = "cddd")
    #    DFT3Act$DateTime <- ymd_hms(DFT3Act$DateTime)
    #    
    #    if (!file.exists(acc_file_T3)) {
    #      write_rds(DFT3Act, acc_file_T3)
    #    }else {
    #      read_rds(acc_file_T3) %>% 
    #        bind_rows(DFT3Act) %>% 
    #        arrange(DateTime) %>% 
    #        head(1000) %>% 
    #        write_rds(acc_file_T3)
    #    }    
    #  }
    #  
    
  })
  
  
  observeEvent(input$Terminal,{
    updateSelectInput(session, inputId = "MagicNum", label = NULL, choices = c("All",magicNumber()), selected = NULL)
    updateSelectInput(session, inputId = "Symbol", label = NULL, choices = c("All",symbol()), selected = NULL)
  }) 
  
  #update Symbol choices
  observeEvent(input$MagicNum,{
    if(input$MagicNum == "All"){
      updateSelectInput(session, "Symbol",label = "Select the symbol",choices = c("All",symbol()),selected = "All")
    }else{
      pair <- DF_Stats()%>%group_by(Symbol)%>%filter(MagicNumber == input$MagicNum)%>%select(Symbol)%>%unique()
      updateSelectInput(session, "Symbol",label = "Select the symbol",choices = c("All",symbol()),selected = pair)
    }
  })
  #update Magic Number
  observeEvent(input$Symbol,{
    if(input$Symbol == "All" || input$Symbol == 1)
    {
      updateSelectInput(session, "MagicNum",label = "Select Magic Number",choices = c("All",magicNumber()),selected = "All")
    }else
    {
      MN <- DF_Stats()%>%group_by(MagicNumber)%>%filter(Symbol==input$Symbol)%>%select(MagicNumber)%>%unique()
      updateSelectInput(session, "MagicNum",label = "Select Magic Number",choices =  c("All",magicNumber()) ,selected = as.integer(unlist(MN)[1]))
    }
  })
  
  #---------DATA TAB---------------  
  Stats <- reactive({
    
    if(input$Time == "Entry Time"){time <- DF_Stats()$EntryTime} 
    else {time <- DF_Stats()$ExitTime}
    
    if(input$MagicNum == "All"){
      DF_Stats <- DF_Stats()%>%filter(time >= input$From, time <= paste0(input$To," 23:59:59"))
    }
    else{
      DF_Stats <- DF_Stats()%>%filter(MagicNumber == input$MagicNum, time >= input$From, time <= input$To)
    }
  })
  
  output$data <- DT::renderDataTable({
    if(file.exists(file_path())) {
      Stats <- data.frame(MagicNumber = Stats()$MagicNumber,
                          Ticket = Stats()$Ticket,
                          EntryTime = as.character(Stats()$EntryTime),
                          ExitTime = as.character(Stats()$ExitTime),
                          Profit = Stats()$Profit,
                          Symbol = Stats()$Symbol,
                          Type = Stats()$Type)
      
      
      
      switch(input$Sort,
             "MagicNumber" =  Stats[order(Stats$MagicNumber,decreasing = T),],
             "Ticket" =  Stats[order(Stats$Ticket,decreasing = T),],
             "EntryTime" =  Stats[order(Stats$EntryTime,decreasing = T),],
             "ExitTime" =  Stats[order(Stats$ExitTime,decreasing = T),],
             "Profit"=  Stats[order(Stats$Profit,decreasing = T),],
             "Symbol"=  Stats[order(Stats$Symbol,decreasing = T),])
      
      datatable(Stats,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
        pageLength = 10, autoWidth = TRUE))
      
    }else{"NO DATA"}
  })
  
  
  
  DF_Balance <- reactive({
    
    Balance <- c()
    DF_Balance <- Stats()
    #mutate(DF_Balance,ExitTime =  as.POSIXct(DF_Balance$ExitTime, format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Cairo"))
    DF_Balance <- DF_Balance %>% subset(select = -c(MagicNumber,Ticket,EntryTime,Type)) 
    DF_Balance <- DF_Balance[order(DF_Balance$Symbol,decreasing = F),]
    #group_by(DF_Balance,Stats()$Symbol)
    #pair <- as.vector(unique(Stats()$Symbol))
    
    for(i in  1:nrow(DF_Balance)){
      if (i==1){
        Balance[i] <- DF_Balance$Profit[i]
      }else if(DF_Balance$Symbol[i] != DF_Balance$Symbol[i-1] && i>1){
        Balance[i] <- DF_Balance$Profit[i]
      }else{
        Balance[i] <- DF_Balance$Profit[i]+ Balance[i-1]
      }
    }
    
    DF_Balance <- DF_Balance %>%  mutate(Balance) 
    # DF_Balance[order(DF_Balance$Symbol,decreasing = F),]
    #  DF_Balance[order(DF_Balance$ExitTime,decreasing = F),]
    
    DF_Balance
  })
  
  DF_Balance_All <- reactive({
    DF_Balance <- Stats()
    DF_Balance <- DF_Balance %>% subset(select = -c(MagicNumber,Ticket,EntryTime,Type)) 
    DF_Balance <- DF_Balance[order(DF_Balance$ExitTime,decreasing = F),]
    
    Balance <- c()
    for(i in  1:nrow(DF_Balance)){
      if (i==1){
        Balance[i] <- DF_Balance$Profit[i]
        
      }else{
        Balance[i] <- DF_Balance$Profit[i]+ Balance[i-1]
      }
    }
    
    DF_Balance <- DF_Balance %>%  mutate(Balance) 
  })
  
  
  #----------GRAPH TAB-----------------
  output$profitGraph <- renderPlotly({
    if(file.exists(file_path())&& nrow(Stats()>0)){
      
      color <- c("red", "black", "blue","green","orange","purple", "pink","cornflowerblue", "darkgreen","indianred3","magenta","mediumpurple3", "midnightblue","orchid4","palegreen","skyblue","slateblue4", "tomato1")
      colorList <- vector("list",length(pair()))
      
      for (i in 1 : length(pair())){
        Ps <- list(target = pair()[i], value = list(marker =list(color = sample(color,1))))
        colorList[[i]] <- Ps
      }
      
      if(input$MagicNum == "All"){
        graph <-  plot_ly(
          type = 'scatter',
          x = Stats()$ExitTime,
          y = Stats()$Profit,
          text = paste("<br>Time: ", Stats()$ExitTime,
                       "<br>Profit: ", Stats()$Profit,
                       "<br>Symbol: ", Stats()$Symbol),
          hoverinfo = 'text',
          mode = 'markers',
          transforms = list(
            list(
              type = 'groupby',
              groups = Stats()$Symbol,
              styles = colorList)))
      }else
      {
        plot_ly(
          x = Stats()$ExitTime,
          y = Stats()$Profit,
          type = "scatter",
          mode = 'markers',
          marker = list(color = sample(color,1)),
          name = paste0(input$Symbol," PROFIT"))
      }
    } 
  })
  
  
  output$balanceGraph <- renderPlotly({
    
    if(file.exists(file_path()) && nrow(DF_Balance()>0)){
      color <- c("red", "black", "blue","green","orange","purple", "pink","cornflowerblue", "darkgreen","indianred3","magenta","mediumpurple3", "midnightblue","orchid4","palegreen","skyblue","slateblue4", "tomato1")
      colorList <- vector("list",length(pair()))
      
      for (i in 1 : length(pair())){
        Ps <- list(target = pair()[i], value = list(line =list(color = sample(color,1))))
        colorList[[i]] <- Ps
      }
      
      graph <-  plot_ly(
        type = 'scatter',
        x = DF_Balance()$ExitTime,
        y = DF_Balance()$Balance,
        text = paste("<br>Time: ", DF_Balance()$ExitTime,
                     "<br>Balance: ", DF_Balance()$Balance,
                     "<br>Symbol: ", DF_Balance()$Symbol),
        hoverinfo = 'text',
        mode = 'lines',
        transforms = list(
          list(
            type = 'groupby',
            groups = DF_Balance()$Symbol,
            styles = colorList)))
    }
  })  
  
  
  #------------ACCOUNT TAB--------------  
  
  accountResults <- reactive({
    
    
    if(file.exists(account_path())){
      df_AR <- readr::read_rds(account_path())
      
      df_AR <-  data.frame(DateTime =df_AR$DateTime,
                           Balance = df_AR$Balance,
                           Equity = df_AR$Equity,
                           Profit = df_AR$Profit
      )
      df_AR <- df_AR%>%filter(DateTime >= input$From, DateTime <= paste0(input$To," 23:59:59"))
    } else{"NO DATA"}
  })
  
  output$watchDogReport <- DT::renderDataTable({
    if(file.exists(account_path())){
      df_AR <-  data.frame(DateTime =as.character(accountResults()$DateTime),
                           Balance = accountResults()$Balance,
                           Equity = accountResults()$Equity,
                           Profit = accountResults()$Profit)
      
      datatable(df_AR,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
        pageLength = 10, autoWidth = TRUE))
      
    }else{"NO DATA"}
  }) 
  
  output$equityGraph <- renderPlotly({
    
    if(file.exists(account_path())){
      
      
      graph <-  plot_ly(
        type = 'scatter',
        x = accountResults()$DateTime,
        y = accountResults()$Balance,
        text = paste("<br>Time: ", accountResults()$DateTime,
                     "<br>Balance: ", accountResults()$Balance),
        hoverinfo = 'text',
        mode = 'lines',
        name = 'Balance',
        line = list(color = 'blue'))
      
      graph <- graph %>% add_trace(
        type = 'scatter',
        x = accountResults()$DateTime,
        y = accountResults()$Equity,
        text = paste("<br>Time: ", accountResults()$DateTime,
                     "<br>Equity: ", accountResults()$Equity),
        hoverinfo = 'text',
        mode = 'lines',
        name = 'Equity',
        line = list(color = 'red')
      )
    }
  }) 
  
  
  
  
  #-------REPORT TAB-----------------
  buyProfit <- reactive({
    
    if(input$MagicNum == "All"){
      allProfit <-  Stats()  %>%
        group_by(Type) %>% 
        filter(Type == 0) %>%
        summarise( Profit = sum(Profit)) %>%
        select(-c(Type)) %>%
        mutate(Symbol = "ALL PAIR")
      
      
      buyProfit <-  Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 0) %>% 
        summarise( Profit = sum(Profit)) %>% 
        subset(select = c(Symbol,Profit)) %>%
        rbind(allProfit)
      
    } else{
      buyProfit <-  Stats()  %>%
        group_by(Symbol, Type) %>% 
        summarise( Profit = sum(Profit)) %>% 
        filter(Type == 0) %>% 
        subset(select = c(Symbol,Profit))
    }
  })
  
  
  sellProfit <- reactive({
    
    if(input$MagicNum == "All"){
      allProfit <-  Stats()  %>%
        group_by(Type) %>% 
        filter(Type == 1) %>%
        summarise( Profit = sum(Profit)) %>%
        select(-c(Type)) %>%
        mutate(Symbol = "ALL PAIR")
      
      sellProfit <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise( Profit = sum(Profit)) %>% 
        subset(select = c(Symbol,Profit)) %>%
        rbind(allProfit)
      
    } else{
      sellProfit <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise( Profit = sum(Profit)) %>% 
        subset(select = c(Symbol,Profit))
    }
  })
  
  buyTrade <- reactive({
    
    if(input$MagicNum == "All"){
      allTrade <-  Stats()  %>%
        group_by(Type) %>% 
        filter(Type == 0) %>%
        summarise( Buy_Trade = n()) %>%
        select(-c(Type)) %>%
        mutate(Symbol = "ALL PAIR")
      
      buyTrade <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 0) %>% 
        summarise( Buy_Trade = n()) %>% 
        subset(select = c(Symbol,Buy_Trade)) %>%
        rbind(allTrade)
      
    } else{
      buyTrade <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        summarise(Buy_Trade = n()) %>% 
        filter(Type == 0) %>% 
        subset(select = c(Symbol,Buy_Trade))
    }
  })
  
  sellTrade <- reactive({
    
    if(input$MagicNum == "All"){
      allTrade <-  Stats()  %>%
        group_by(Type) %>% 
        filter(Type == 1) %>%
        summarise( Sell_Trade = n()) %>%
        select(-c(Type)) %>%
        mutate(Symbol = "ALL PAIR")
      
      sellTrade <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise( Sell_Trade = n()) %>% 
        subset(select = c(Symbol,Sell_Trade)) %>%
        rbind(allTrade)
    } else{
      sellTrade <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise(Sell_Trade = n()) %>% 
        subset(select = c(Symbol,Sell_Trade))
    }
  })
  
  output$result <- DT::renderDataTable({
    
    if(file.exists(file_path())&&nrow(Stats())>0){
      DF_allPair <- DF_Balance_All()
      allPair <- round(DF_allPair[nrow(DF_allPair),4],2)
      final_Balance  <- vector("numeric", length(pair()))
      
      for (i in 1: length(pair())){
        pairBalance <-  DF_Balance() %>% filter(Symbol == pair()[i])
        final_Balance[i]  <- round(pairBalance[nrow(pairBalance),4],2)
      }
      
      Final_Balance <- data.frame(Symbol = pair(),
                                  Final_Balance = final_Balance) 
      
      if(input$MagicNum == "All"){
        All_Pair <- c("ALL PAIR", allPair)
        
        Final_Balance <- rbind(Final_Balance,All_Pair)  
        Final_Balance <- left_join(x = Final_Balance,y = buyProfit(), by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance,y = sellProfit(), by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance, y = buyTrade(),by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance, y = sellTrade(),by = "Symbol") 
        
        
        
      }else{
        Final_Balance <- left_join(x = Final_Balance,y = buyProfit(), by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance,y = sellProfit(), by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance, y = buyTrade(),by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance, y = sellTrade(),by = "Symbol") 
      }
      FB <- as.data.frame(Final_Balance)
      FB[is.na(FB)] <- 0
      FB <- FB %>% cbind(FB$Buy_Trade + FB$Sell_Trade) %>%
        set_names(c("Symbol","Final_Balance","Buy_Profit","Sell_Profit","Buy_Trade","Sell_Trade", "Total_Trade"))
      
      datatable(FB,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
        pageLength = 28, autoWidth = TRUE))
    }
    
  })
  
  output$profitFactor <- renderText({
    if(file.exists(file_path()) && nrow(Stats())>0){
      negProfit <- Stats()%>%filter(Profit<0)%>%select(Profit)%>%summarise(Loss = abs(sum(Profit)))
      posProfit <- Stats()%>%filter(Profit>0)%>%select(Profit)%>%summarise(Gain = abs(sum(Profit)))
      
      
      if(negProfit == 0 ){"Only Gain"}
      else if (posProfit == 0){"Only Loss"}
      else{as.double(round( posProfit/(0.001+negProfit),2))}
    }else{
      "NO DATA"
    }
  })
  
  output$maxProfit <- renderText({
    if(file.exists(file_path())&&nrow(Stats())>0){max(Stats()$Profit)}
    else{"NO DATA"}
  })
  
  output$minProfit <- renderText({
    if(file.exists(file_path())&&nrow(Stats())>0){min(Stats()$Profit)}
    else{"NO DATA"}
  })
  
  output$totalTrade <- renderText({
    if(file.exists(file_path())){
      nrow(Stats())}
    else{"NO DATA"}
  })
  
  
  output$totalProfit <- renderText({
    if(nrow(DF_Balance_All())>0){
      DF_allPair <- DF_Balance_All()
      round(DF_allPair[nrow(DF_allPair),4],2)
    }else{"0"}
  })
  
  
  account_path <- reactive({
    
    #  Terminals <- normalizePath(Sys.getenv('PATH_T1'), winslash = '/')
    #  file_path <- paste0(Terminals,"/OrdersResultsT",1,".csv")      
    pathDSS <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    paste0(pathDSS,"/_DATA/AccountResultsT",input$Terminal,".rds")
  })
  
  
  ###################################################################
  ####################### - MT INSPECTION - #########################
  ###################################################################
  
  #-----------MANAGE SIDEBAR-------------    
  #Refresh data 
  observeEvent(input$RefreshMT,{
    updateSelectInput(session, inputId = "MagicNumMT", label = NULL, choices = c("All",magicNumberMT()), selected = NULL)
    updateSelectInput(session, inputId = "SymbolMT", label = NULL, choices = c("All",symbolMT()), selected = NULL)
    
  })
  
  
  observeEvent(input$TerminalMT,{
    updateSelectInput(session, inputId = "MagicNumMT", label = NULL, choices = c("All",magicNumberMT()), selected = NULL)
    updateSelectInput(session, inputId = "SymbolMT", label = NULL, choices = c("All",symbolMT()), selected = NULL)
  }) 
  
  #update Symbol choices
  observeEvent(input$MagicNumMT,{
    if(input$MagicNumMT == "All"){
      updateSelectInput(session, "SymbolMT",label = "Select the symbol",choices = c("All",symbolMT()),selected = "All")
    }else{
      pair <- DF_StatsMT()%>%group_by(Symbol)%>%filter(MagicNumber == input$MagicNumMT)%>%select(Symbol)%>%unique()
      updateSelectInput(session, "SymbolMT",label = "Select the symbol",choices = c("All",symbolMT()),selected = pair)
    }
  })
  #update Magic Number
  observeEvent(input$SymbolMT,{
    magicNumber <- unique(DF_StatsMT()$MagicNumber)
    
    if(input$SymbolMT == "All" || input$SymbolMT == 1)
    {
      updateSelectInput(session, "MagicNumMT",label = "Select Magic Number",choices = c("All",magicNumber),selected = "All")
    }else
    {
      MN <- DF_StatsMT()%>%group_by(MagicNumber)%>%filter(Symbol==input$SymbolMT)%>%select(MagicNumber)%>%unique()
      updateSelectInput(session, "MagicNumMT",label = "Select Magic Number",choices =  c("All",magicNumber) ,selected = as.integer(unlist(MN)[1]))
      
    }
  })
  
  observeEvent(input$FromMT,{
    if(input$SymbolMT != 1 && input$SymbolMT != "All"){
      
      
      oldestDate <-  Ai_Rsiadx()$X1[nrow(Ai_Rsiadx())]
      rownumber <- c(rownames(Ai_Rsiadx()))
      time <- c(Ai_Rsiadx()$X1)
      time_DF <- data.frame(rownumber,time)
      timeFilter <- paste0(input$FromMT," 01:00:00")
      row <- time_DF %>% filter(time == timeFilter)%>%select(rownumber)
      for (i in 1:2){
        if(nrow(row)==0){
          
          timeFilter <- paste0(input$FromMT + i," 01:00:00")
          print(timeFilter)
          row <- time_DF %>% filter(time == timeFilter)%>%select(rownumber)
        }
        
        
      }
      row <- row[1,]
      
      updateSliderInput(session,"rows",value = row)
    }
  })
  
  observeEvent(input$Now,{
    if(input$SymbolMT != 'All')
    {
      updateSliderInput(session,"rows",value = nrow(Ai_Rsiadx())-64)
    }
  })
  
  #-----------------REACTIVE EVENTS-----------------
  
  DF_StatsMT <- reactive({
    
    Terminals <- normalizePath(Sys.getenv(paste0('PATH_T',input$TerminalMT)), winslash = '/')
    file_path <- paste0(Terminals,"/OrdersResultsT",input$TerminalMT,".csv")
    DF_StatsMT <- read.csv(file_path, col.names = c("MagicNumber","Ticket","EntryTime","ExitTime","Profit","Symbol","Type"))
    
    DF_StatsMT <- data.frame(MagicNumber = DF_StatsMT$MagicNumber,
                             Ticket = DF_StatsMT$Ticket,
                             EntryTime = as.POSIXct(DF_StatsMT$EntryTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                             ExitTime = as.POSIXct(DF_StatsMT$ExitTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                             Profit = DF_StatsMT$Profit,
                             Symbol = DF_StatsMT$Symbol,
                             Type = DF_StatsMT$Type)
    
  })
  
  
  MarketTypeLog <- reactive({
    
    
    MN <- DF_StatsMT() %>% select(MagicNumber)%>% group_by(MagicNumber) 
    MN <- unique(MN)
    path_user <- normalizePath(Sys.getenv(paste0("PATH_T",input$TerminalMT)), winslash = '/')
    
    MTList = list()
    for(i in 1:nrow(MN)){
      
      magicNumber <- MN[i,]
      marketLogPath <- paste0(path_user,"/MarketTypeLog",magicNumber,".csv")
      marketFile <- read.csv(marketLogPath, header = FALSE, sep = ",",
                             col.names = c("MagicNumber","Ticket","MTcode","TimeHold","MarketType"))
      marketFile$i <- i
      MTList[[i]] <- marketFile
      
      
    }
    
    MarketTypeLog <- data.table::rbindlist(MTList)
    
    MarketTypeLog <- merge(x = MarketTypeLog,y = DF_StatsMT(),by = "Ticket", All.x= TRUE)
  })
  
  #
  Ai_Rsiadx <- reactive({
    
    if(input$SymbolMT != "character(0)"){
      path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
      path_data <- file.path(path_user, "_DATA/6_06")
      Ai_Rsiadx <-readr::read_rds(file.path(path_data,  paste0('AI_RSIADX',input$SymbolMT,'60.rds')))
      
      
      #https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
      Ai_Rsiadx <- Ai_Rsiadx[order(Ai_Rsiadx$X1),]
    }
  })
  
  SystemControlMT <- reactive({
    
    Terminals <- normalizePath(Sys.getenv(paste0('PATH_T',3)), winslash = '/')
    file_path <- paste0(Terminals,"/OrdersResultsT",3,".csv")
    DF_StatsMT <- read.csv(file_path, col.names = c("MagicNumber","Ticket","EntryTime","ExitTime","Profit","Symbol","Type"))
    
    DF_StatsMT <- data.frame(MagicNumber = DF_StatsMT$MagicNumber,
                             Ticket = DF_StatsMT$Ticket,
                             EntryTime = as.POSIXct(DF_StatsMT$EntryTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                             ExitTime = as.POSIXct(DF_StatsMT$ExitTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                             Profit = DF_StatsMT$Profit,
                             Symbol = DF_StatsMT$Symbol,
                             Type = DF_StatsMT$Type)
    
    MN <- DF_StatsMT %>% select(MagicNumber)%>% group_by(MagicNumber) 
    MN <- unique(MN)
    path_user <- normalizePath(Sys.getenv(paste0("PATH_T",3)), winslash = '/')
    
    MTList = list()
    for(i in 1:nrow(MN)){
      
      magicNumber <- MN[i,]
      SystemControlPath <- paste0(path_user,"/SystemControlMT",magicNumber,".csv")
      marketFile <- read.csv(SystemControlPath, header = FALSE, sep = ",",
                             col.names = c("MarketType","Policy"))
      marketFile <- marketFile %>% cbind(magicNumber)
      marketFile$i <- i
      MTList[[i]] <- marketFile
      
      
    }
    
    SystemControlMT <- data.table::rbindlist(MTList)
    SystemControlMT <- SystemControlMT %>% filter(MarketType != 'MarketType')
    
  })
  
  
  symbolMT <- reactive({
    if(file.exists(file_path())){  
      symbol <-  unique(DF_StatsMT()$Symbol)
      symbol[order(symbol)]
    }else{"NO DATA"}
  })
  
  magicNumberMT <- reactive({
    if(file.exists(file_path())){
      unique(DF_StatsMT()$MagicNumber)
    }else{"NO DATA"}
  })
  
  #######VIEW TAB####################
  
  output$marketType <- renderText({
    
    if(input$SymbolMT != "All"){
      Terminals <- normalizePath(Sys.getenv('PATH_T1'), winslash = '/')
      file_path <- paste0(Terminals,paste0('/AI_MarketType_',input$SymbolMT,'60.csv'))
      MT <- read.csv(file_path,col.names = c("MT","Prediction"))
      MTString <- toString(MT$MT)
      PredictionString <- toString(MT$Prediction)
      
      paste(MTString," ", PredictionString)
    } else{"Please select a symbol"}
  })
  
  #graphs and outputs
  output$closePrice <- renderPlotly({
    if(input$SymbolMT != "All"){
      # generate bins based on input$bins from ui.R
      #  path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
      #  path_data <- file.path(path_user, "_DATA")
      #  CP <-readr::read_rds(file.path(path_data,  paste0('AI_RSIADX',input$SymbolMT,'60.rds')))
      toRow <- input$rows + 63
      
      graph <-  plot_ly(
        type = 'scatter',
        x = Ai_Rsiadx()$X1[input$rows:toRow],
        y = Ai_Rsiadx()$X2[input$rows:toRow],
        mode = 'lines',
        name = 'Market Type',
        line = list(color = 'red'))
    } 
  })
  
  output$systemControlMT <- DT::renderDataTable({
    if(input$SymbolMT != 'All'){
      
      InputMN <- input$MagicNumMT
      InputMNLeft <- substr(InputMN,1,4)
      #get right character of the Magic Number
      substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
      }
      InputMNRight <- substrRight(InputMN,2)
      InputMN <- paste0(InputMNLeft,3,InputMNRight)
      
      
      SCMT <- SystemControlMT() %>% 
        filter(MagicNumber == InputMN)  %>%
        select(MarketType,Policy) 
      
      
    }
    
  })
  
  output$closePriceTable <- DT::renderDataTable({
    if(input$SymbolMT != "All"){
      
      toRow <- input$rows + 63
      CPDF <-  data.frame( Date =as.character(Ai_Rsiadx()$X1[input$rows:toRow]),
                           Close_price = format(round(Ai_Rsiadx()$X2[input$rows:toRow],5), nsmall = 5))
      
      # CPDF <- CPDF[order(c(input$rows:toRow),decreasing = TRUE),]
      
      datatable(CPDF,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
        pageLength = 64, autoWidth = TRUE))
    }
  })
  
  
  
  #############STAT TAB######################
  
  PairID_DF <- reactive({
    
    pair <- c("AUDCAD","AUDCHF","AUDJPY","AUDNZD","AUDUSD","CADCHF","CADJPY","CHFJPY","EURAUD","EURCAD","EURCHF","EURGBP","EURJPY","EURNZD","EURUSD","GBPAUD","GBPCAD","GBPCHF","GBPJPY","GBPNZD","GBPUSD","NZDCAD","NZDCHF","NZDJPY","NZDUSD","USDCAD","USDCHF","USDJPY")
    pairId <- c("00",	"01",	"02",	"03",	"04",	"05",	"06",	"07",	"08",	"09",	"10",	"11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	"21",	"22",	"23",	"24",	"25",	"26",	"27")
    PairID_DF <- data.frame(pair,pairId)
    
  })
  
  pathControl <- reactive({
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    pairID <- PairID_DF() %>% filter(pair == input$SymbolMT)
    pair <- pairID[2]
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    
    
    path_T1 <- normalizePath(Sys.getenv('PATH_T1'), winslash = '/')
    OrderResultPath <- paste0(path_T1,"/OrdersResultsT1.csv")
    OrderResult <- read.csv(OrderResultPath, header = FALSE)
    
    OrderResult <- OrderResult%>%select(V1,V6)%>%unique()
    
    MagicNumber <- OrderResult %>% 
      filter(V6 == input$SymbolMT) %>%
      select(V1)
    getMN <- MagicNumber%>%pull(V1)
    pathControl <- paste0(path_user,"/_DATA/control/",getMN,".rds")
    
  })
  
  output$MTResult <- renderPlotly({
    
    if(input$MagicNumMT == "All"){
      
      MarketTypeLog <- MarketTypeLog() %>% filter(MarketTypeLog()$EntryTime >= input$FromMT)
      if(nrow(MarketTypeLog)>0){
        MarketTypeResult <- aggregate(MarketTypeLog$Profit, by=list(MarketType=MarketTypeLog$MarketType), FUN=sum)
        
        hist <- plot_ly(
          x = MarketTypeResult$MarketType,
          y = MarketTypeResult$x,
          name = "MT Result",
          type = "bar")
        
        
        hist <- hist %>% layout(title = "PROFIT BY MARKET TYPE",
                                yaxis = list(title = 'Profit')
        )
        
      }
    }else{
      
      MarketTypeLog <- MarketTypeLog() %>% filter(MarketTypeLog()$MagicNumber.x == input$MagicNumMT , MarketTypeLog()$EntryTime >= input$FromMT)
      
      if(nrow(MarketTypeLog>0)){
        MarketTypeResult <- aggregate(MarketTypeLog$Profit, by=list(MarketType=MarketTypeLog$MarketType), FUN=sum)
        
        
        hist <- plot_ly(
          x = MarketTypeResult$MarketType,
          y = MarketTypeResult$x,
          name = "MT Result",
          type = "bar")
      }
    }
    
  })
  
  output$controlGraph <- renderPlotly({
    
    if(file.exists(pathControl())){
      
      controlData <- readRDS(pathControl())
      control <-c(controlData$alpha,controlData$gamma,controlData$epsilon)
      controlData <- data.frame(control)
      
      
      graph <- plot_ly(data = controlData, x = c("alpha","gamma","epsilon"), y = controlData[,1],
                       type = 'scatter',
                       mode = "markers",
                       marker = list(size = 50,
                                     color = 'rgba(255, 0, 0, .9)',
                                     line = list(color = 'rgba(0, 0, 0, .8)',
                                                 width = 2)))
      graph <- graph %>% layout(
        title = "Control Parameter")
      
    }
    
    
    
  })
  
  
  
  
  
  output$MarketLog <- DT::renderDataTable({
    
    MarketTypeLog <- MarketTypeLog()
    
    if(input$SymbolMT == "All"){
      MTLog <- MarketTypeLog %>% filter(EntryTime >= input$FromMT)
      datatable(MTLog,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
        pageLength = 10, autoWidth = TRUE))
      
    } else
    {
      MTLog <- MarketTypeLog %>% filter(Symbol == input$SymbolMT, EntryTime >= input$FromMT)
      datatable(MTLog,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
        pageLength = 10, autoWidth = TRUE))
    }
    
  })
  
  
  ###################################################################
  ####################### - MODEL INSPECTION - ######################
  ###################################################################
  
  
  readResult <- reactive({
    
    pathDSS <- normalizePath(Sys.getenv("PATH_DSS"), winslash = '/')
    file_path <- paste0(pathDSS,"/_DATA/analyse_resultM60.csv")
    DF_Result <- readr::read_csv(file_path) 
    #Col.names = c("TR_Level","NB_hold","Symbol","MaxPerf","FrstQntlPerf"))
    
    
    
  })
  
  output$AnalyseResult <- DT::renderDataTable({
    
    
    DF_Result <- data.frame(TR_Level = readResult()$TR_Level,
                            NB_hold = readResult()$NB_hold,
                            Symbol =readResult()$Symbol,
                            MaxPerf = format(round(readResult()$MaxPerf,2),nsmall=2),
                            FrstQntlPerf = format(round(readResult()$FrstQntlPerf,2),nsmall=2))
    
    datatable(DF_Result,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE))
    
  })
  
  dataResult <- reactive({
    pathDSS <- normalizePath(Sys.getenv("PATH_DSS"), winslash = '/')
    file_path <- paste0(pathDSS,"/_DATA/analyse_resultM60_data.csv")
    DF_DataResult <- readr::read_csv(file_path)
    
  })
  
  
  output$dataRes <- DT::renderDataTable({
    DF_Data_Result <- data.frame(PnL_NB = round(dataResult()$PnL_NB,5),
                                 TotalTrades = dataResult()$TotalTrades,
                                 TR_Level = dataResult()$TR_Level,
                                 NB_hold = dataResult()$NB_hold,
                                 Symbol = dataResult()$Symbol,
                                 FinalOutcome = dataResult()$FinalOutcome)
    
    datatable(DF_Data_Result,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE))
  })
  
  output$strategyTestResults <- renderPlot({
    ggplot(dataResult(), aes(x = NB_hold, y = PnL_NB,
                             #size = TotalTrades, 
                             col = as.factor(Symbol)))+geom_point()+
      ggtitle("Strategy Test results")
  })
  
  output$modelPerformance <- renderPlot({
    ggplot(readResult(), aes(x = MaxPerf, y = Symbol,
                             col = TR_Level, 
                             size = NB_hold))+geom_point()+
      geom_vline(xintercept=0.001)+ 
      scale_x_continuous()+
      ggtitle("Model Performance")
  })
  
  observeEvent(input$RefreshM60,{
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_logs <- file.path(path_user, "_MODELS")
    
    # file names
    filesToAnalyse <-list.files(path = path_logs,
                                pattern = "-60.rds",
                                full.names=TRUE)
    
    # aggregate all files into one
    for (VAR in filesToAnalyse) {
      # VAR <- filesToAnalyse[1]
      if(!exists("dfres")){dfres <<- readr::read_rds(VAR)}  else {
        dfres <<- readr::read_rds(VAR) %>% dplyr::bind_rows(dfres)
      }
    }
    ## Analysis of model quality records
    # file names
    filesToAnalyse1 <-list.files(path = path_logs,
                                 pattern = "M60.csv",
                                 full.names=TRUE)
    
    # aggregate all files into one
    for (VAR in filesToAnalyse1) {
      # VAR <- filesToAnalyse1[1]
      if(!exists("dfres1")){dfres1 <<- readr::read_csv(VAR)}  else {
        dfres1 <<- readr::read_csv(VAR) %>% dplyr::bind_rows(dfres1)
      }
    }
    
    write.csv(dfres1,paste0(path_user,"/_DATA/analyse_resultM60.csv"), row.names=FALSE)
    write.csv(dfres,paste0(path_user,"/_DATA/analyse_resultM60_data.csv"), row.names=FALSE)
    
    session$reload()
    
  })
  
  
  
  
  ##################################################################################
  ############################# PERFORMANCE ########################################
  ##################################################################################
  
  #-------Reactive---------#
  
  perf_log <- reactive({
    pathDSS <- normalizePath(Sys.getenv("PATH_DSS"), winslash = '/')
    file_path <- paste0(pathDSS,"/_LOGS/perf_logs60.rds")
    perf_log <- readr::read_rds(file_path) %>%
      filter(TimeTest >= input$FromCopy, TimeTest <= paste0(input$ToCopy," 23:59:59"))
  })
  
  
  input_log <- reactive({
    pathDSS <- normalizePath(Sys.getenv("PATH_DSS"), winslash = '/')
    file_path <- paste0(pathDSS,"/_SIM/all_results.rds")
    
    input_log <- readr::read_rds(file_path) %>%
      filter(TimeTest >= input$FromCopy, TimeTest <= paste0(input$ToCopy," 23:59:59"))
    input_log$TimeTest <-  sort(input_log$TimeTest,decreasing = TRUE)
    input_log <- data.frame(TimeTest = as.POSIXct(input_log$TimeTest,format = "%d-%m-%Y %H:%M:%S"),
                            Folder = as.character(input_log$Folder),
                            MeanPerf = round(input_log$MeanPerf,2),
                            HighPerf = round(input_log$HighPerf,2))
    
    for (i in 1:nrow(input_log)){
      
      input_log[i,2] <- substr(input_log[i,2],nchar(input_log[i,2]) - 3,nchar(input_log[i,2]))
      
    }
    input_log
    
  })
  
  
  #---------Observe event--------------#
  
  #copy date from Result tab
  observeEvent(input$From,{
    updateDateInput(session,inputId = "FromCopy",label = NULL,value = input$From)
    
  })
  
  observeEvent(input$To,{
    updateDateInput(session,inputId = "ToCopy",label = NULL,value = input$To)
  })
  
  
  output$perfLog <- DT::renderDataTable({
    perfLog <- data.frame(TimeTest = as.character(perf_log()$TimeTest),
                          MeanPerf = round(perf_log()$MeanPerf,2),
                          Quantil = round(perf_log()$Quantil,2))
    datatable(perfLog,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE))  
  })
  
  output$inputLog <- DT::renderDataTable({
    
    inputLog <- input_log()
    
    # dw <- inputLog %>% select(Folder)
    
    
    # for (i in 1:nrow(inputLog)){
    #  
    #   inputLog[i,2] <- substr(inputLog[i,2],23,27)
    # 
    # }
    
    # inputLog <- data.frame(TimeTest = as.character(inputLog$TimeTest),
    #                       Version = as.character(inputLog$Folder),
    #                       MeanPerf = round(inputLog$MeanPerf,2),
    #                       HighPerf = round(inputLog$HighPerf,2))
    datatable(input_log(),class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE))  
  })
  
  output$perfGraph <- renderPlotly({
    
    
    perfLog <- data.frame(TimeTest = as.Date(perf_log()$TimeTest,"%Y-%m-%d", tz="GMT"),
                          MeanPerf = perf_log()$MeanPerf,
                          Quantil = perf_log()$Quantil)
    
    fig <- perfLog %>% plot_ly(x = perfLog$TimeTest, type="candlestick",
                               open = perfLog$MeanPerf, close = perfLog$Quantil,
                               high = perfLog$MeanPerf, low =perfLog$Quantil,
                               text = paste("<br>Time: ", perfLog$TimeTest,
                                            "<br>MeanPerf: ", perfLog$MeanPerf,
                                            "<br>Quantile:",perfLog$Quantil)
    ) 
    fig <- fig %>% layout(title = list(text = "Open = MeanPerf - Close = Quantile", x = 0),
                          xaxis = list(rangeslider = list(visible = F))) 
  }) 
  
  output$inputGraph <- renderPlotly({
    
    inputLog <- input_log()
    # 
    # inputLog$Folder <- as.integer(substr(inputLog[,2],3,5))
    # 
    # version <- unique(inputLog[,2])
    # 
    graph <- plot_ly(data = inputLog, x = inputLog[,3], y = inputLog[,4], color = inputLog[,2], alpha = 0.7,
                     type = 'scatter',
                     mode = "markers",
                     text = paste0("Version: ",inputLog[,2], '$<br>Date: ',inputLog[,1]),
                     marker = list(size = 50,
                                   width = 2))
    
    graph <- graph %>% layout(title = 'Data Writer Performances',
                              xaxis = list(title = 'MeanPerformance'),
                              yaxis = list(title = 'HighPerformance'))
    
    
  })
  
  #---------------END CODE------------------------------------------
  output$console <- renderPrint({
    
    print(SystemControlMT())
    
  })
  
  
}
