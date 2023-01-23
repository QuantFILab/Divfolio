box <- shinydashboard::box

#Request Conditions
req.cons.file <- function(f,req.colname){
  con.num <- (sum(sapply(f, class) %in% c('interger', 'numeric')) == 0)
  con.colname <- (sum(req.colname %in% colnames(f)) != length(req.colname))
  con.na <- (sum(is.na(f)) != 0)
  return(c(con.num,con.colname,con.na))
}

#Popover tag
pop.tag <- function(title,id.pop){
  return(tags$span(title, bsButton(id.pop, label = "", icon = icon("info"), style = "info", size = "extra-small")))
}

#Popover call
pop.call <- function(id.pop, text){
  return(
    bsPopover(
      id = id.pop,
      title = "More information",
      content = text,
      placement = "right",
      trigger = "focus",
      options = list(container = "body")))
}

multipywtoreturn <- function(w,return_mat){
  
  #Separate Block of Return
  rebdate <- w$Date
  returnmat <- lapply(1:(length(rebdate)), function(i) {if(i < length(rebdate)){return_mat %>% dplyr::filter((Date >= rebdate[i]) & (Date < rebdate[i+1]))}else{return_mat %>% dplyr::filter(Date >= rebdate[i])}})
  returnmat <- returnmat %>% lapply(function(m) {m[is.na(m)] <- 0; ifelse(length(m[m == 0]) == 0, m <- m, m[m == 0] <- rnorm(length(m[m == 0]),0.0001,0.0001)); m})                                                                                    
  
  #Multiply by weight
  wm <- lapply(seq_along(rebdate), function(i) apply(returnmat[[i]][,-1],1, function(x) x*w[i,-1]) %>% bind_rows()) 
  names(wm) <- rebdate
  
return(wm)
}

round_tab <- function(x){
  x[, sapply(x, class) %in% c('numeric','integer')] <- as.data.frame(apply(x[, sapply(x, class) %in% c('numeric','integer')],2,round,6))
  #x[, sapply(x, names) %in% c('date','Date')] <- data.frame(apply(x[, sapply(x, names) %in% c('date','Date')],2,as.Date))
  return(x)
}
everyother <- function(x){
  lx <- length(seq_along(x))
  if(lx <= 16){
    y <- x
  }else{
    y <- x[seq(1,lx,floor(lx/6))]
  }
  return(y)
  }


check <- function(ticker){
  ticker <- toupper(ticker)
  profile <- paste0('https://finance.yahoo.com/quote/',ticker,'/profile?p=',ticker) %>% read_html()
  comn <- profile %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/h3') %>% html_text()
  return(ifelse(identical(comn, character(0)),0,1))
}

key_detail <- function(ticker){
  url_req <- paste0("https://query1.finance.yahoo.com/v7/finance/quote?symbols=",toupper(ticker))
  fin.res <- tryCatch(
    { res <- fromJSON(url_req)
      as.list(res$quoteResponse$result)},
    error = function(cond){
      NULL  
    }
  )
  return(fin.res)
}

modules_data <- function(ticker, module){
  url_req <- paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/",toupper(ticker),"?modules=", module)
  res <- fromJSON(url_req)
  return(as.list(res$quoteSummary$result[[1]]))
}



hist_esg <- function(ticker){
  url_req <- paste0("https://query2.finance.yahoo.com/v1/finance/esgChart?symbol=",toupper(ticker))
  res <- fromJSON(url_req)
  peerGroup <- res$esgChart$result$peerGroup
  
  symbolSeries <- res$esgChart$result$symbolSeries %>% as.list() %>% bind_cols() %>% `colnames<-`(c("Time","ESG","G","E","S"))
  symbolSeries$Time <- as.Date(as.POSIXct(symbolSeries$Time, origin="1970-01-01"))
  peerSeries <- res$esgChart$result$peerSeries %>% as.list() %>% bind_cols() %>% `colnames<-`(c("Time","ESG","G","E","S"))
  peerSeries$Time <- as.Date(as.POSIXct(peerSeries$Time, origin="1970-01-01"))
  return(list(symbolSeries, peerSeries))
}

current_esg <- function(ticker){
  url_req <- paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/",toupper(ticker),"?modules=esgScores")
  esgScores <- tryCatch(
    { res <-fromJSON(url_req)
    esgScores <- res$quoteSummary$result$esgScores %>% as.list()},
    error = function(cond){
      NULL  
    }
  )
  return(esgScores)
}

plot_hist_esg <- function(ticker){
  data_plot <- hist_esg(ticker)[[1]]
  m_data <- melt(data_plot, id.vars = 'Time')
  ggplot(data = m_data, aes(x = as.Date(Time), y = value, group = variable, color = variable)) +
    geom_line(size = 1.2)
}

get_stock_profile <- function(ticker){
  re <- rep('-',14)
  ticker <- toupper(ticker)
  comp_data <- key_detail(ticker)
  esg_data <- current_esg(ticker) 
  prof <- tryCatch(
    {modules_data(ticker,'assetProfile')},
    error = function(cond){
      NULL
    })
  comn <- comp_data$longName
  
  if(identical(comn, character(0))||(comn == "")||(is.na(comn))){re <- rep('-',14); re[1] <- 'Unavailable'; re[5] <- 'black'}else{
    if(is.null(prof)){
      sector <- NA
      subsector <- NA
    }else{
      sector <- prof$sector
      subsector <- prof$industry
    }
    market <- paste0(comp_data$exchange, "currency in ",comp_data$currency)
    esg <- rep("-",7)
    com_name <- paste0(comn," (", comp_data$symbol,")")
    esg[2] <- "black"
    if(!is.null(esg_data)){
      esg[3] <- as.integer(esg_data$totalEsg$fmt)
      esg[4] <- as.integer(esg_data$environmentScore$fmt)
      esg[5] <- as.integer(esg_data$socialScore$fmt)
      esg[6] <- as.integer(esg_data$governanceScore$fmt)
      esg[1] <- esg_data$esgPerformance
      esg[7] <- paste0(esg_data$percentile$fmt,"th percentile")
      if(esg[1] == "OUT_PERF"){
        esg[2] <- "red"
        esg[1] <- 'High'
      }
      if(esg[1] == "AVG_PERF"){
        esg[2] <- "yellow"
        esg[1] <- "Medium"
      }
      if(esg[1] == "UNDER_PERF"){
        esg[2] <- "green"
        esg[1] <- "Low"
      }
      if(esg[1] == "LAG_PERF"){
        esg[2] <- "blue"
        esg[1] <- "Negligible"
      }
    }else{
      esg <- rep("-",7)
    }
    re <- c(com_name,sector,subsector,esg,comp_data$marketCap,comp_data$trailingPE,comp_data$trailingAnnualDividendRate, market)
  }
  
  return(re)
}
                                                     
                                                     
get_esg_profile <- function(ticker){
  ticker <- toupper(ticker)
  comp_data <- key_detail(ticker)
  if(length(comp_data) == 0){
    fin <- rep(NA,9)
  }else{
    esg_data <- current_esg(ticker) 
    prof <- tryCatch(
      {modules_data(ticker,'assetProfile')},
      error = function(cond){
        NULL
      })
    comn <- comp_data$longName
    if(is.null(prof)){
      sector <- NA  
      subsector <- NA
    }else{
      sector <- prof$sector
      subsector <- prof$industry
    }
    esg <- rep("-",5)
    com_name <- paste0(comn," (", comp_data$symbol,")")
    if(!is.null(esg_data)){
      esg[2] <- as.integer(esg_data$totalEsg$fmt)
      esg[3] <- as.integer(esg_data$environmentScore$fmt)
      esg[4] <- as.integer(esg_data$socialScore$fmt)
      esg[5] <- as.integer(esg_data$governanceScore$fmt)
      esg[1] <- gsub("OUT_PERF","High", esg_data$esgPerformance) %>% gsub("AVG_PERF","Medium", .) %>% gsub("UNDER_PERF","Low", .) %>% gsub("LAG_PERF", "Negligible",.)
    }
    fin <- c(com_name,ticker,sector,subsector,esg)
  }
  return(fin)
}

get_data <-function(ticker){
  ticker <- toupper(ticker)
  tick_name <- getSymbols(ticker,src='yahoo')
  df <- data.frame(Date=index(get(tick_name)),coredata(get(tick_name)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  return(df)
}

candlestick_plot <- function(df,ticker){
  ticker <- toupper(ticker)
  # create Bollinger Bands
  bbands <- BBands(df[,c("High","Low","Close")])
  
  # join and subset data
  df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= df[1,1])
  
  # colors column for increasing and decreasing
  for (i in 1:length(df[,1])) {
    if (df$Close[i] >= df$Open[i]) {
      df$direction[i] = 'Increasing'
    } else {
      df$direction[i] = 'Decreasing'
    }
  }
  
  #i <- list(line = list(color = '#17BECF'))
  #d <- list(line = list(color = '#7F7F7F'))
  
  # plot candlestick chart
  
  fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                        open = ~Open, close = ~Close,
                        high = ~High, low = ~Low, name = ticker)
  #increasing = i, decreasing = d) 
  fig <- fig %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                           line = list(color = '#ccc', width = 0.5),
                           legendgroup = "Bollinger Bands",
                           hoverinfo = "none", inherit = F) 
  fig <- fig %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                           line = list(color = '#ccc', width = 0.5),
                           legendgroup = "Bollinger Bands", inherit = F,
                           showlegend = FALSE, hoverinfo = "none") 
  fig <- fig %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                           line = list(color = '#E377C2', width = 0.5),
                           hoverinfo = "none", inherit = F) 
  fig <- fig %>% layout(yaxis = list(title = "Price", automargin = TRUE), xaxis = list(rangeslider = list(visible = F)))
  
  
  # plot volume bar chart
  fig2 <- df 
  # fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = "Volume",
  #                          color = ~direction) 
  fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = "Volume") 
  fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))
  
  # create rangeselector buttons
  rs <- list(visible = TRUE, x = 0.5, y = -0.055,
             xanchor = 'center', yref = 'paper',
             font = list(size = 9),
             buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward')
             ))
  
  # subplot with shared x axis
  fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
                 shareX = TRUE, titleY = TRUE)
  fig <- fig %>% layout(title = paste0(ticker," :",df[1,1]," - ",Sys.Date()),
                        xaxis = list(rangeselector = rs),
                        legend = list(orientation = 'h', x = 0.5, y = 1,
                                      xanchor = 'center', yref = 'paper',
                                      font = list(size = 10),
                                      bgcolor = 'transparent'))
  
  fig
}


plotstart <- function(found){
  num_factor <- found[, sapply(found, class) %in% c('numeric','integer'), drop=FALSE] %>% apply(2,mean)
  num_factor_div <- found %>% dplyr::filter(Status == 'Invest') %>% .[, sapply(found, class) %in% c('numeric','integer'), drop=FALSE] %>% apply(2,mean)
  perc <- ((num_factor_div - num_factor)/num_factor)*100
  tab <- do.call("rbind", list(num_factor,num_factor_div,perc)) 
  N <-  c(nrow(found),sum(found$Status == 'Invest'), 100*sum(found$Status == 'Invest')/nrow(found))
  tab <-  cbind(tab,N)
  rownames(tab) <- c("Before Divest","After Divest","Relative Change")
  colnames(tab) <- c('ESG', 'Environment', 'Social', 'Governance', 'Number of Assets') 
  return(data.frame(tab))
}

auto_selection <- function(found, fact, crit, upper){
  self <- found %>% select(Ticker, fact)
  q <- quantile(unlist(found %>% select(fact)),crit, na.rm = TRUE)
  if(upper == 1){
    selq <- (self %>% filter_at(2,all_vars(. > q)))$Ticker
  }else{
    selq <- (self %>% filter_at(2,all_vars(. < q)))$Ticker
  }
  Statusup <- ifelse(found$Ticker %in% selq, 'Invest','Divest')
  foundup <- found
  foundup$Status <- Statusup
  return(foundup)
}



performance_plot <- function(df,ticker){
  ticker <- toupper(ticker)
  df <- xts(df[,-1], order.by=df$Date) 
  df$Ret <- OpCl(df)
  perf <- c(mean(df$Ret), sd(df$Ret), mean(df$Ret)/sd(df$Ret) ,maxDrawdown(df$Ret),
            SortinoRatio(df$Ret), VaR(df$Ret, p=.95, method="historical"))
  
  for(i in rev(c('1 week','1 month','3 months','1 year'))){
    df_r <- df %>% last(i)
    perf <- rbind(perf,c(mean(df_r$Ret), sd(df_r$Ret), mean(df_r$Ret)/sd(df_r$Ret) ,maxDrawdown(df_r$Ret),
                         SortinoRatio(df_r$Ret), VaR(df_r$Ret, p=.95, method="historical")))
  }
  
  levelf <- c("all",rev(c('1 week','1 month','3 months','1 year')))
  colnames(perf) <- c("Return","Volatility","Sharpe","MDD","Sortino","VaR")
  rownames(perf) <- levelf
  perf <- data.frame(perf) 
  
  
  fig <- melt(perf %>% rownames_to_column('Time')) %>% `colnames<-`(c("Time","Measure","Value"))
  fig$Time <- factor(fig$Time, levels = rev(levelf))
  fig <- fig %>% plot_ly(x = ~Time, y = ~Value, color = ~Measure, colors= viridis_pal(option="D")(3))
  
  return(list(round(perf[rev(levelf),],4) %>% rownames_to_column('Time'),fig))
}

# auto_select <- function(found,selcrit,percent){
# found  
# }

return_summary <- function(x){
  xnew <- x %>% dplyr::select(ticker,ret.adjusted.prices) %>% drop_na() %>%
    group_by(ticker) %>% summarise(Return = mean(ret.adjusted.prices), Volatility = sd(ret.adjusted.prices),
                                   Sharpe = mean(ret.adjusted.prices)/sd(ret.adjusted.prices),
                                   MDD = maxDrawdown(ret.adjusted.prices),
                                   Sortino = SortinoRatio(ret.adjusted.prices)[,1],
                                   Return.cumulative = Return.cumulative(ret.adjusted.prices),
                                   VaR = VaR(ret.adjusted.prices, p=.95, method="historical")[,1],
                                   Days = length(ret.adjusted.prices)
    )
  xnew[,2:ncol(xnew)] <- round(xnew[,2:ncol(xnew)],6)
  colnames(xnew) <- c("Ticker", "Return", "Volatility", "Sharpe", "MDD", "Sortino", "Return.cumulative", "VaR", "Days", "Weight")
  return(xnew)
}

return_summary_update <- function(x){
  xnew <- x %>% melt(id.vars = 'Date') %>% `colnames<-`(c('Date','ticker','ret.adjusted.prices')) %>% group_by(ticker) %>% summarise(Return = mean(ret.adjusted.prices), Volatility = sd(ret.adjusted.prices),
                                   Sharpe = mean(ret.adjusted.prices)/sd(ret.adjusted.prices),
                                   MDD = maxDrawdown(ret.adjusted.prices),
                                   Sortino = SortinoRatio(ret.adjusted.prices)[,1],
                                   Return.cumulative = Return.cumulative(ret.adjusted.prices),
                                   VaR = VaR(ret.adjusted.prices, p=.95, method="historical")[,1],
                                   Days = length(ret.adjusted.prices)
    )
  xnew[,2:ncol(xnew)] <- round(xnew[,2:ncol(xnew)],6)
  colnames(xnew) <- c("Ticker", "Return", "Volatility", "Sharpe", "MDD", "Sortino", "Return.cumulative", "VaR", "Days", "Weight")
  return(xnew)
}

boxplot_assets <- function(asset_return, rankby){
  melt_asset <- melt(asset_return, id.var = "Date") %>% `colnames<-`(c("Date",'Ticker', 'Return'))
  returnsummary  <- return_summary_update(asset_return)
  ticker_arr <- as.vector((returnsummary %>% dplyr::arrange(get(rankby)))$Ticker)
  melt_asset$Ticker <- factor(melt_asset$Ticker, levels = ticker_arr)
  
  p <- ggplot(melt_asset, aes(x= Ticker, y=Return, fill=Ticker)) +
  geom_boxplot(alpha=0.7) +
    geom_violin(width=1.4, alpha=0.3) +
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=Ticker), alpha=0.6) +
    stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = "Company's Tickers",
         y = "Return") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          legend.position="none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  fig <- ggplotly(p) 
  
  return(fig)
}



# correlation_asset <- function(asset_return){
#   cormat <- cor(asset_return[,-1])
#   eigenmat <- eigen(cormat)
#   eivector <- eigenmat$vectors
#   Tab_Sil <- lapply(2:(ncol(eivector)-1), function(i) {
#     clara(x = eivector[,1:i], k = i, metric = "euclidean", samples = 4000, pamLike = TRUE)$silinfo$avg.width
#   }) %>% t() %>% bind_cols()
#   min(which(Tab_Sil == max(Tab_Sil)))
#   clara(x = t(eivector), k = 5, metric = "euclidean", samples = 4000, pamLike = TRUE)
#   plot_ly(z = cormat, type = "heatmap", xgap = 1, ygap = 1, x = colnames(cormat), y = colnames(cormat))
#   return(heatmap,cortab)
# }


box_constrain <- function(Weight, Short_Limit){
  Crit <- sum(Weight[Weight < 0])
  if(Crit < -Short_Limit){
    Weight[Weight < 0] <-  -(Weight[Weight < 0]/Crit)*Short_Limit
    Weight[Weight >= 0] <-  (1+Short_Limit)*(Weight[Weight >= 0]/sum(Weight[Weight >= 0]))
    Weight_Box <- Weight
  }else{
    Weight_Box <- Weight
  }
  return(Weight_Box)
}



allocation <- function(x,type,tau,reb,lim = 0, filew){
  #One Column Matrix
  one <- as.matrix(rep(1,ncol(x)-1))
  # Set Date
  first <- head(x[-(1:tau),],1)$Date
  last <- tail(x[-(1:tau),],1)$Date
  
  rebdate <- x[seq(reb,nrow(x),reb),]$Date

  returnmat <- lapply(seq_along(rebdate), function(i) {rowre <- which(x$Date == as.Date(rebdate[i])); x[(rowre-tau):(rowre-1),-1]}) %>%
    lapply(function(m) {m[is.na(m)] <- 0; m}) %>% lapply(function(m) {ifelse(length(m[m == 0]) == 0, m <- m,m[m == 0] <- rnorm(length(m[m == 0]),0.0001,0.0001)); m}) 
  

    covmat <- lapply(returnmat, function(y) cov(y))
    invcov <- lapply(covmat, function(y) {s <- as.matrix(solve(as.matrix(y),tol = 1E-1000)); s[lower.tri(s)] <- t(s)[lower.tri(s)]; s} ) 
    
    #Check Accuracy
    crit <- sapply(seq_along(covmat), function(i) sum(covmat[[i]]%*%invcov[[i]]))
    if(sum((crit >= (ncol(x)-1) - 1E-10)&&(crit <= (ncol(x)-1) + 1E-10)) != 1){
      covmat <- lapply(returnmat, function(y) {covr <- as.matrix(covOGK(y,sigmamu = s_mad)$cov); dimnames(covr) <- list(colnames(x)[-1],colnames(x)[-1]); covr})
      invcov <- lapply(covmat, function(y) {s <- as.matrix(solve(as.matrix(y),tol = 1E-1000)); s[lower.tri(s)] <- t(s)[lower.tri(s)]; s} )}

 
  if(is.null(filew)){
  switch(as.character(type),
         "0" = {w <- matrix(rep(1/ncol(x[,-1]),(ncol(x[,-1])*length(rebdate))),length(rebdate),ncol(x[,-1])) %>% `colnames<-`(colnames(x[,-1])) %>% as.data.frame(); covmat <- NULL},
         "1" = {w <- lapply(returnmat, function(y) {m <- apply(y,2,mean); m <- m+ifelse(min(m) >= 0,0,-min(m)+0.001); m/sum(m)}) %>% bind_rows() %>% as.data.frame(); covmat <- NULL},
         "2" = {w <- lapply(invcov, function(y) {numer <- t(one)%*%y;  numer/sum(numer)}) %>% lapply(as.data.frame) %>% bind_rows()},
         "3" = {w <- sapply(seq_along(invcov), function(i) {mu <- matrix(apply(returnmat[[i]] ,2,mean)); numer <- t(mu)%*%invcov[[i]];
         numer/sum(numer)}) %>% t() %>% as.data.frame() %>% `colnames<-`(colnames(x)[-1])},
         "4" = {w <- lapply(covmat, function(y) {ec <- eigen(y); as.matrix(apply(ec$vectors,2,function(n) n/sum(n)))%*%matrix(ec$values/sum(ec$values))}) %>%
           lapply(function(m) as.data.frame(t(m))) %>% bind_rows() %>% `colnames<-`(colnames(x)[-1]) }
  )
  
  if(lim != 0){w <- apply(w,1,box_constrain, Short_Limit = lim) %>% t() %>% data.frame()}
  
  w <- cbind(rebdate,w) %>% as.data.frame() %>% `colnames<-`(c('Date',colnames(w)))
  }else{
  w <- filew
  w[,2:ncol(w)] <- round(w[,2:ncol(w)],6)
  }
    
  return(list(w,returnmat,covmat,rebdate))
}

sep_pos_neg <- function(w){
  wp <- w
  wp[wp <= 0] <- 0
  wn <- w
  wn[wn > 0] <- 0
  return(list(wp,wn))
}



asset_weight_plot <- function(w,selcom){
  sel <- w[,c('Date',selcom)]
  colnames(sel) <- c('Date', 'Weight')
  sel$Date <- as.character(sel$Date)
  p <- ggplot(sel, aes(x= Date,y= Weight, group= 1)) + 
    geom_area(fill="#440154FF", alpha=0.5) +
    geom_line(color="#440154FF", size= 0.7) +
    geom_point(size=3, color="#440154FF") +
    labs(title = paste0("Investment Weight of ",selcom),
         x = "Date",
         y = "Weight") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks = everyother) 
  fig <- ggplotly(p) 
  return(fig)
}

all_performance_measure <- function(xcol){
  Return = mean(xcol)
  Volatility = sd(xcol)
  Sharpe = mean(xcol)/sd(xcol)
  MDD = maxDrawdown(xcol)
  Sortino = SortinoRatio(xcol)[,1]
  Return.cumulative = Return.cumulative(xcol)
  VaR = VaR(xcol, p=.95, method="historical")[,1]
  Days = length(xcol)
  ftab <- data.frame(Return,Volatility,Sharpe,MDD,Sortino,Return.cumulative,VaR,Days) %>% `rownames<-`(NULL)
  return(ftab)
}

all_performance_table <- function(return_mat,wmat){
  rebdate <- wmat$Date
  block <- multipywtoreturn(wmat,return_mat)
  return_col_block <- block %>% lapply(rowSums) 
  ftab <- lapply(return_col_block,all_performance_measure) %>% bind_rows()
  Date <- c(as.character(rebdate),'Overall') 
  all_per <- as.vector(block %>% bind_rows() %>% rowSums()) %>% all_performance_measure()
  ftab <- rbind(ftab,all_per)
  pertab <- cbind(Date,ftab)
  return(pertab)
}

plot_performance_table <- function(pertab){
  #Delete OVerall
  pertab <- pertab[-nrow(pertab),]
  pertab$Date <- as.Date(pertab$Date)
  meltm <- melt(pertab %>% dplyr::select(-Days), id.vars = "Date") %>% `colnames<-`(c('Date','Risk','Value'))
  
  p <- ggplot(meltm, aes(x= Date,y= Value, group = Risk, fill=Risk)) + 
    geom_area(alpha=0.5) +
    geom_line(size= 0.5) +
    geom_point(size=1) +
    scale_fill_viridis(discrete = TRUE) +
    facet_wrap(~Risk, scales = "free_y", ncol = 2) +
    labs(title = "Risk Profiles",
         x = "Date",
         y = "Weight") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
          legend.position="none",
          strip.text= element_text(size = 10, face = "bold" ),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines"))
  fig <- ggplotly(p, width = 1000, height=1000)
  
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  #one col
  
  return(fig)
}


plot_radar <- function(pertab){
  last_row <- pertab[nrow(pertab),-c(1,ncol(pertab))]
  tabnew <- pertab[-nrow(pertab),-ncol(pertab)]
  tabnewmelt <- melt(tabnew, id.vars = 'Date')
  
  p <- ggplot(tabnewmelt, aes(x= factor(0), y=value, fill=variable )) +
    geom_boxplot(alpha=0.7) +
    geom_violin(width=1.4, alpha=0.3) +
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=variable), alpha=0.6) +
    stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = "",
         y = "Score") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          legend.position="none",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          strip.text= element_text(size = 10, face = "bold" ),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines")) +
  facet_wrap(. ~ variable, scales="free_y", ncol = 3) 
  fig <- ggplotly(p)
  
  return(fig)
}


ex_var_efficient <- function(invc,mu){
  invc <- as.matrix(invc)
  mu <- as.matrix(mu)
  one <- as.matrix(rep(1,ncol(invc)))
  a <- t(one)%*%invc%*%one
  b <- t(one)%*%invc%*%mu
  c <- t(mu)%*%invc%*%mu
  
  exgmv <- b/a
  vargmv <- 1/a
  extan <- c/b
  vartan <- c/b^2
  covgmvtan <- 1/a
  
  asig <- vartan - vargmv 
  csig  <- vargmv + 0.001
  
  alpcalp <- sqrt(csig*asig)/asig
  alp <- seq(-alpcalp,alpcalp,(2*alpcalp)/100)
  exeff <- as.vector(1-alp)*as.vector(exgmv) + as.vector(alp)*as.vector(extan)
  sdeff <- sqrt(as.vector((alp^2))*as.vector(vartan) + (as.vector(1-alp)^2)*as.vector(vargmv)  + 2*as.vector(alp)*as.vector(1-alp)*as.vector(covgmvtan))
  meltplot <- data.frame(exeff,sdeff) 
  return(meltplot)
}

efficient_fontier <- function(x,y){
  rebdate <- unique(c(x$Date[1],y$Date))
  returnmat <- lapply(seq_along(rebdate)[-length(rebdate)], function(i) {rowtoday <- which(x$Date == as.Date(rebdate[i]));
  rowfuture <- which(x$Date == as.Date(rebdate[i+1])) - 1; x[(rowtoday):(rowfuture),-1]}) %>% lapply(function(m) {m[is.na(m)] <- 0; m}) %>% lapply(function(m) {ifelse(length(m[m == 0]) == 0, m <- m,m[m == 0] <- rnorm(length(m[m == 0]),0.0001,0.0001)); m}) 
  
  wmat <- lapply(1:nrow(y), function(i) as.matrix(returnmat[[i]])%*%t(as.matrix(y[i,-1]))) %>% lapply(function(m) {mm <- data.frame(mean(m),sd(m)) %>% `colnames<-`(c('Mu','SD')); mm}) %>% bind_rows()
  wmat$Date <- y$Date
    
    
  covmat <- lapply(returnmat, function(y) {cov(y)})
  invcov <- lapply(covmat, function(y) {solve(y,tol = 1E-100)}) 
  
  crit <- sapply(seq_along(covmat), function(i) sum(covmat[[i]]%*%invcov[[i]]))
  if(sum((crit >= (ncol(x)-1) - 1E-10)&&(crit <= (ncol(x)-1) + 1E-10)) != 1){
  covmat <- lapply(returnmat, function(y) {covr <- as.matrix(covOGK(y,sigmamu = s_mad)$cov); dimnames(covr) <- list(colnames(x)[-1],colnames(x)[-1]); covr})
  invcov <- lapply(covmat, function(y) {s <- as.matrix(solve(as.matrix(y),tol = 1E-100)); s[lower.tri(s)] <- t(s)[lower.tri(s)]; s} )}
  ef <- lapply(seq_along(invcov), function(i) {res <- ex_var_efficient(invcov[[i]],apply(returnmat[[i]] ,2,mean)); res$Date <- rebdate[i]; res}) %>% bind_rows()
  
  return(list(ef,wmat))
}


plot_efficient_fontier <- function(ef,w,wmat,seldate){
  wmat$Date <- as.character(wmat$Date)
  wmatsub <- subset(wmat, Date %in% seldate) %>% `colnames<-`(c('exeff','sdeff','Date'))
  efsub <- subset(ef, Date %in% seldate)
  
  p <- ggplot(efsub,aes(x = sdeff,y = exeff, colour = Date)) +
    geom_point() +
    scale_colour_viridis_d()+
    geom_point(data = wmatsub, shape = 8, size =4) +
    labs(x = "Standard Deviation",
         y = "Expected Return",
         colour = 'Portfolio'
         ) +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(vjust = 0.5, hjust=1))
  fig <- ggplotly(p)
  return(p)
}



plot_fundamental <- function(w,found){
  common <- intersect(colnames(w),found$Ticker)
  w <- w[,c('Date',common)]
  num_factor <- found[, sapply(found, class) %in% c('numeric','integer'), drop=FALSE]
  coverage_num <- apply(num_factor, 2, function(x) (1- sum(is.na(x))/length(x))*100)
  posw <- w[,-1]
  posw[posw < 0] <- 0 
  negw <- w[,-1]
  negw[negw >= 0] <- 0
  poswa <- NULL
  for(j in 1:ncol(num_factor)){
    poswa <- rbind(poswa,sapply(1:nrow(posw), function(i) {x <- num_factor[,j]; y <- posw[i,];
    x <- x[!is.na(x)]; y <- y[!is.na(x)]; sum(x*y)/sum(y)}))
  }
  poswa <- round(poswa,2) %>% `colnames<-`(w$Date) %>% `rownames<-`(names(coverage_num))
  
  negwa <- NULL
  for(j in 1:ncol(num_factor)){
    negwa <- rbind(negwa,sapply(1:nrow(negw), function(i) {x <- num_factor[,j]; y <- negw[i,];
    x <- x[!is.na(x)]; y <- y[!is.na(x)]; sum(x*y)/sum(y)}))
  }
  negwa <- round(negwa,2) %>% `colnames<-`(w$Date) %>% `rownames<-`(names(coverage_num))
  
  meltpos <- melt(poswa) %>% `colnames<-`(c('Factor','Date','Score'))
  meltpos$Position <- 'Long'
  meltneg <- melt(negwa) %>% `colnames<-`(c('Factor','Date','Score'))
  meltneg$Position <- 'Short'
  meltplogt <- rbind(meltpos,meltneg)
 
  
  p <- ggplot(meltplogt, aes(x = Date)) + 
    geom_bar(data = subset(meltplogt, Position == "Long"), 
             aes(y = Score), stat = "identity", position = "dodge", color="black", fill="#440154FF", alpha = 0.5) +
    geom_bar(data = subset(meltplogt, Position == "Short"), 
             aes(y = -Score), stat = "identity", position = "dodge", color="black", fill="#FDE725FF", alpha = 0.5) + 
    geom_hline(yintercept = 0,colour = "grey90") +
    labs(x = "Date",
         y = "Score") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="none",
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines")) +
    scale_x_discrete(breaks = everyother) +
    facet_wrap(. ~ Factor, scales="free_y", ncol = 2)
    
    fig <- ggplotly(p)
    
    ppos <- ggplot(meltpos, aes(x= factor(0), y=Score, fill=Factor)) +
      geom_boxplot(alpha=0.7) +
      geom_violin(width=1.4, alpha=0.3) +
      stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
      scale_fill_viridis(discrete = TRUE) +
      labs(x = " ",
           y = "Score") +
      theme_bw() +
      theme(text = element_text(family = 'Fira Sans'),
            legend.position="none",
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            strip.text= element_text(size = 10, face = "bold" ),
            strip.placement = "outside",
            strip.background = element_blank(),
            panel.spacing.y = unit(1, "lines"))   +
            scale_x_discrete(breaks = everyother) +
    facet_wrap(. ~ Factor, scales="free_y", ncol = 2) 
    
    figpos <- ggplotly(ppos) 
    
    meltneg$Score <- as.numeric(gsub(NaN, 0, meltneg$Score))
    
    pneg <- ggplot(meltneg, aes(x= factor(0), y=Score, fill=Factor)) +
      geom_boxplot(alpha=0.7) +
      geom_violin(width=1.4, alpha=0.3) +
      stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
      scale_fill_viridis(discrete = TRUE) +
      labs(x = " ",
           y = "Score") +
      theme_bw() +
      theme(text = element_text(family = 'Fira Sans'),
            legend.position="none",
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            strip.text= element_text(size = 12, face = "bold" ),
            strip.placement = "outside",
            strip.background = element_blank(),
            panel.spacing.y = unit(1, "lines")) +
    facet_wrap(. ~ Factor, scales="free_y", ncol = 2)
    figneg <- ggplotly(pneg) 
    
    
    tabpos <- t(poswa) %>% data.frame() %>% rownames_to_column('Date')
    tabpos$Position <- 'Long'
    tabneg <- t(negwa) %>% data.frame() %>% rownames_to_column('Date')
    tabneg$Position <- 'Short'
    
    tabfig <- rbind(tabpos,tabneg)
    
    return(list(fig,figpos,figneg,tabfig))
}




plot_portfolio_weight <- function(w,found,selcat,seltime){
  
  plotw <- w %>% dplyr::filter(Date == seltime) %>%.[-1] %>% t() %>% data.frame() %>%
    rownames_to_column('Ticker') %>% `colnames<-`(c('Ticker', 'Weight')) %>% merge(found,by='Ticker')
 
  selfound <- plotw  %>% dplyr::select(Weight,Ticker,selcat) %>% `colnames<-`(c('Weight','Ticker','Select')) 
  
  bool <- ifelse(class(selfound$Select) %in% c('integer','numeric'),FALSE,TRUE)
         
  p <- ggplot(data=selfound, aes(x=Ticker, y=Weight,  fill = Select)) +
    geom_bar(stat="identity", alpha = 0.7, color="black") +
    scale_fill_viridis(discrete = bool, name = selcat) +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  fig <- ggplotly(p) 
  
  return(fig)

}
####################################File4##########################################



div_schedule <- function(typesc,para,w,enddate = NULL){
  Date <- as.Date(w$Date)
  switch(as.character(typesc),
         "0" = {sch <- rep(0,length(Date))},
         "1" = {indt <- which(Date == enddate); t <- (1:length(Date)); sch  <- para[1]*(t - indt); sch[indt:length(sch)] <- 0},
         "2" = {indt <- which(Date == enddate); t <- (1:length(Date)); sch  <- (t)^(-para[2]); sch[indt:length(sch)] <- 0}) 
  
 return(data.frame(Date,sch) %>% `colnames<-`(c('Date','Bound')))
}


div_preview <- function(typesc, para, enddate = NULL, w, found){
  Schedule <- div_schedule(typesc,para,w,enddate)
  Bound <- unlist(Schedule$Bound,use.names =  FALSE)
  divasset <- (found %>% dplyr::filter(Status == "Divest"))$Ticker
  divw <- w[,divasset]
  divestsum <- lapply(sep_pos_neg(divw),rowSums) %>% bind_cols() %>% as.data.frame() %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short')) %>% melt() %>% `colnames<-`(c('Date','Position','Weight'))
  divestsum$Bound <- c(Schedule$Bound,-(Schedule$Bound))
  
  p <- ggplot(divestsum, aes(x = Date)) + 
    geom_bar(data = subset(divestsum, Position == "Long"),
             aes(y = Weight, fill = Position), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    geom_bar(data = subset(divestsum, Position == "Short"),
             aes(y = Weight, fill = Position), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    geom_hline(yintercept = 0,colour = "grey90") +
    geom_line(data = subset(divestsum, Position == "Long"),
              aes(y = Bound, group = 1), color="black", size = 1) +
    geom_line(data = subset(divestsum, Position == "Short"),
              aes(y = Bound, group = 1), color="black", size =  1) +
    labs(x = "Date",
         y = "Sum of Divestable Weights") +
    theme_bw() +
    scale_x_discrete(breaks = everyother) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="top")
  
  return(ggplotly(p))
}



plot_indiv <- function(w,found){
  invasset <- (found %>% dplyr::filter(Status == "Invest"))$Ticker
  divsset <- (found %>% dplyr::filter(Status == "Divest"))$Ticker
  invw <- w[,invasset]
  divw <- w[,divsset]
  
  investsum <- lapply(sep_pos_neg(invw),rowSums) %>% bind_cols() %>% as.data.frame() %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short')) %>% melt() %>% `colnames<-`(c('Date','Position','Weight'))
  divestsum <- lapply(sep_pos_neg(divw),rowSums) %>% bind_cols() %>% as.data.frame() %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short')) %>% melt() %>% `colnames<-`(c('Date','Position','Weight'))
  investsum$Status <- "Invest"
  divestsum$Status <- "Divest"
  sumall <- rbind(investsum,divestsum)
  
  p <- ggplot(sumall, aes(x = Date)) + 
    geom_bar(data = subset(sumall, Position == "Long"), 
             aes(y = Weight), stat = "identity", position = "dodge", color="black", fill="#31688EFF", alpha = 0.5) +
    geom_bar(data = subset(sumall, Position == "Short"), 
             aes(y = Weight), stat = "identity", position = "dodge", color="black", fill="#FDE725FF", alpha = 0.5) + 
    geom_hline(yintercept = 0,colour = "grey90") +
    facet_wrap(~Status, ncol = 1) +
    labs( x = "Date",
         y = "Weight") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="none",
          strip.text= element_text(size = 10, face = "bold" ),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines")) +
    scale_x_discrete(breaks = everyother)
  
  fig <- ggplotly(p)
  
  return(fig)
  
}




div_weight <- function(w,Schedule,found){
  Invest_List <- found %>% dplyr::filter(Status == 'Invest') %>% .$Ticker
  Divest_List <- found %>% dplyr::filter(Status == 'Divest') %>% .$Ticker
  Bound <- unlist(Schedule$Bound,use.names =  FALSE)
  final_frame <- c()
  for(k in 1:nrow(w)){
    Invest <- w[,Invest_List][k,]
    Divest <- w[,Divest_List][k,]
 
    Long_Divest <- Divest[which(Divest >= 0)] 
    Short_Divest <- Divest[which(Divest < 0)]
    if(length(Short_Divest)==0){Short_Divest <- NULL}
    Long_Divest_Sum <- if (length(Long_Divest)==0) {0} else sum(Long_Divest)
    Short_Divest_Sum <- if (length(Short_Divest)==0) {0} else -sum(Short_Divest)
    
    if((Long_Divest_Sum > Bound[k]) || (Short_Divest_Sum > Bound[k])){
      if(Long_Divest_Sum > Bound[k]){
        Long_Divest_Scale <- (Long_Divest/Long_Divest_Sum)*Bound[k]
      }else{
        Long_Divest_Scale <- Long_Divest
      }
      if(Short_Divest_Sum  > Bound[k]){
        Short_Divest_Scale <- (Short_Divest/Short_Divest_Sum)*Bound[k]
      }else{
        Short_Divest_Scale <- Short_Divest
      }
      
      Excess_Long <- if (length(Long_Divest)==0) {0} else sum(Long_Divest) - sum(Long_Divest_Scale)
      Excess_Short <- if (length(Short_Divest)==0) {0} else -sum(Short_Divest) + sum(Short_Divest_Scale)
      New_Invest <- Invest + (Invest/sum(Invest))*(Excess_Long - Excess_Short)
      
      List_of_Weight <- list(New_Invest,
                             Long_Divest_Scale, Short_Divest_Scale)
      
      Combine_Weight <- Filter(Negate(is.null), List_of_Weight) %>% 
        bind_cols()
      
      Weight_Divest <- Combine_Weight[,colnames(w)[-1]]
    }else{
      Weight_Divest <- w[k,-1]
    }
    
    final_frame <- rbind(final_frame, Weight_Divest)
  }
   Date <- w$Date
   
  return(as.data.frame(cbind(Date,final_frame)))
}



plot_div_Sch <- function(w,found, Schedule, div_w){
  divasset <- (found %>% dplyr::filter(Status == "Divest"))$Ticker
  divw <- w[,divasset]
  divestsum <- lapply(sep_pos_neg(divw),rowSums) %>% bind_cols() %>% as.data.frame() %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short')) %>% melt() %>% `colnames<-`(c('Date','Position','Weight'))
  divestsum$Status <- 'Before-Divest'
  
  divestsum$Bound <- c(Schedule$Bound,-(Schedule$Bound))
  
  
  invasset <- (found %>% dplyr::filter(Status == "Invest"))$Ticker
  invw <- w[,invasset]
  investsum <- lapply(sep_pos_neg(invw),rowSums) %>% bind_cols() %>% as.data.frame() %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short')) %>% melt() %>% `colnames<-`(c('Date','Position','Weight'))
  investsum$Status <- 'Before-Divest'
  
  
  divwaf <- div_w[,divasset]
  divestwafsum <- lapply(sep_pos_neg(divwaf),rowSums) %>% bind_cols() %>% as.data.frame() %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short')) %>% melt() %>% `colnames<-`(c('Date','Position','Weight'))
  divestwafsum$Status <- 'Divest'
  
  
  invwaf <- div_w[,invasset]
  investafsum <- lapply(sep_pos_neg(invwaf),rowSums) %>% bind_cols() %>% as.data.frame() %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short')) %>% melt() %>% `colnames<-`(c('Date','Position','Weight'))
  investafsum$Status <- 'Divest'

  
  melt_frame_div <- bind_rows(divestsum,divestwafsum)
  melt_frame_inv <- bind_rows(investsum,investafsum)
  
  melt_frame_div$Status <- factor(melt_frame_div$Status, levels = c('Before-Divest','Divest'))
  melt_frame_inv$Status <- factor(melt_frame_inv$Status, levels = c('Before-Divest','Divest'))
  
  
  p <- ggplot(melt_frame_div, aes(x = Date)) + 
    geom_bar(data = subset(melt_frame_div, Position == "Long"),
             aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    geom_bar(data = subset(melt_frame_div, Position == "Short"),
             aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    geom_hline(yintercept = 0,colour = "grey90") +
    geom_line(data = subset(divestsum, Position == "Long"),
              aes(y = Bound, group = 1), color="black", size = 0.7) +
    geom_line(data = subset(divestsum, Position == "Short"),
              aes(y = Bound, group = 1), color="black", size = 0.7) +
    labs(x = "Date",
         y = "Sum of Weights") +
    theme_bw() +
    scale_x_discrete(breaks = everyother) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  fig1 <- ggplotly(p)
  
  p <- ggplot(melt_frame_inv %>% arrange(desc(Weight)), aes(x = Date)) + 
    geom_bar(data = subset(melt_frame_inv, Position == "Long"),
    aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    geom_bar(data = subset(melt_frame_inv, Position == "Short"),
    aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    geom_hline(yintercept = 0,colour = "grey90") +
    labs(x = "Date",
         y = "Sum of Weights") +
    theme_bw() +
    scale_x_discrete(breaks = everyother) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
     
  
  fig2 <- ggplotly(p)
  
  
  return(list(fig1,fig2))
  
}


asset_weight_plot_div <- function(w,w_div,selcom){
  selben <- w[,c('Date',selcom)]
  selben$Status <- 'Before-Divest'
  seldiv <- w_div[,c('Date',selcom)]
  seldiv$Status <- 'Divest'
  sel <- rbind(selben,seldiv)
  colnames(sel) <- c('Date', 'Weight', 'Status')
  
  p <- ggplot(sel, aes(x= Date, y = Weight, colour = Status, group= Status, fill = Status)) + 
    geom_area(position = "identity", alpha=0.5, color = 'black') +
    geom_line(position = "identity", size= 0.5, color = 'black') +
    geom_point(position = "identity",size=2, color = 'black') +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = "Date",
         y = "Weight") +
    theme_bw() +
    scale_x_discrete(breaks = everyother) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  fig <- ggplotly(p)
  
    
  return(fig)
}


plot_portfolio_weight_div <- function(w,w_div,found,selcat,seltime){
  
  plotw <- w %>% dplyr::filter(Date == seltime) %>%.[-1] %>% t() %>% data.frame() %>%
    rownames_to_column('Ticker') %>% `colnames<-`(c('Ticker', 'Weight')) %>% merge(found,by='Ticker')
  plotw$Status <- 'Before-Divest'
  
  plotwdiv <- w_div %>% dplyr::filter(Date == seltime) %>%.[-1] %>% t() %>% data.frame() %>%
    rownames_to_column('Ticker') %>% `colnames<-`(c('Ticker', 'Weight')) %>% merge(found,by='Ticker')
  plotwdiv$Status <- 'Divest'
  
  plotw <- rbind(plotw,plotwdiv)
  
  selfound <- plotw  %>% dplyr::select(Weight,Ticker,selcat,Status) %>% `colnames<-`(c('Weight','Ticker','Select', 'Status')) 
  
  bool <- ifelse(class(selfound$Select) %in% c('integer','numeric'),FALSE,TRUE)
  
  p <- ggplot(data=selfound, aes(x=Ticker, y=Weight,  fill = Select, group = Status)) +
    geom_bar(stat="identity", position = "dodge", alpha = 0.7, color="black") +
    scale_fill_viridis(discrete = bool, name = selcat) +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          #legend.position='none',
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  fig <- ggplotly(p) 
  
  return(fig)
  
}

plot_radar_div <- function(pertab,pertabdiv){

  tabnew <- pertab[-nrow(pertab),-ncol(pertab)]
  tabnewmelt <- melt(tabnew, id.vars = 'Date')
  tabnewmelt$Status <- "Before-Divest"
  
  tabnewdiv <- pertabdiv[-nrow(pertabdiv),-ncol(pertabdiv)]
  tabnewmeltdiv <- melt(tabnewdiv, id.vars = 'Date')
  tabnewmeltdiv$Status <- "Divest"
  
  tabnewmeltall <- rbind(tabnewmelt,tabnewmeltdiv)
    
  p <- ggplot(tabnewmeltall, aes(x= Status, y=value, fill=variable, group = Status)) +
    geom_boxplot(alpha=0.7) +
    geom_violin(width=1.4, alpha=0.3) +
    stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = "",
         y = "Score") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          legend.position="none",
          strip.text= element_text(size = 10, face = "bold" ),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines"))  +
    facet_wrap(. ~ variable, scales="free_y", ncol = 3) 
  
  fig <- ggplotly(p)
  
  return(fig)
}

plot_performance_table_div <- function(pertab,pertabdiv){
  pertab <- pertab[-nrow(pertab),]
  pertab$Status <- 'Before-Divest'
  
  pertabdiv <- pertabdiv [-nrow(pertabdiv),]
  pertabdiv $Status <- 'Divest'
  
  pertaball <- rbind(pertab, pertabdiv)
  
  meltm <- melt(pertaball %>% dplyr::select(-Days), id.vars = c("Date","Status")) %>% `colnames<-`(c('Date','Status','Risk','Score'))
  
  p <- ggplot(meltm, aes(x= Date,y= Score, group = Status, fill=Status)) + 
    geom_area(alpha=0.5, position = "identity") +
    geom_line(size= 0.5, position = "identity") +
    geom_point(size=1, position = "identity") +
    scale_fill_viridis(discrete = TRUE, name = "Portfolio") +
    facet_wrap(~Risk, scales = "free_y", ncol = 2) +
    labs(x = "Date",
         y = "Weight") +
    theme_bw() +
    scale_x_discrete(breaks = everyother,) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
          strip.text= element_text(size = 10, face = "bold" ),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines"),
          legend.key.size = unit(4, 'cm'),
          legend.text = element_text(size=15),
          legend.title = element_text(size=17))
  fig <- ggplotly(p, width = 1000, height=1000)
  return(fig)
}


plot_fundamental_div <- function(w,w_div,found){
  common <- intersect(colnames(w),found$Ticker)
  w <- w[,c('Date',common )]
  w_div <- w_div[,c('Date',common)]
  w_all <- rbind(w,w_div)
  num_factor <- found[, sapply(found, class) %in% c('numeric','integer'), drop=FALSE]
  coverage_num <- apply(num_factor, 2, function(x) (1- sum(is.na(x))/length(x))*100)
  posw <- w_all[,-1]
  posw[posw < 0] <- 0 
  negw <- w_all[,-1]
  negw[negw >= 0] <- 0
  poswa <- NULL
  for(j in 1:ncol(num_factor)){
    poswa <- rbind(poswa,sapply(1:nrow(posw), function(i) {x <- num_factor[,j]; y <- posw[i,];
    x <- x[!is.na(x)]; y <- y[!is.na(x)]; sum(x*y)/sum(y)}))
  }
  poswa <- round(poswa,2) %>% `colnames<-`(rep(w$Date,2)) %>% `rownames<-`(names(coverage_num))
  
  meltpos <- rbind(cbind(melt(poswa[,1:nrow(w)]), 'Before-Divest') %>% `colnames<-`(c('Factor','Date','Score', 'Status')), cbind(melt(poswa[,(nrow(w)+1):(nrow(w_all))]),'Divest') %>% `colnames<-`(c('Factor','Date','Score', 'Status'))) 
  meltpos$Position <- 'Long'
  
  negwa <- NULL
  for(j in 1:ncol(num_factor)){
    negwa <- rbind(negwa,sapply(1:nrow(negw), function(i) {x <- num_factor[,j]; y <- negw[i,];
    x <- x[!is.na(x)]; y <- y[!is.na(x)]; sum(x*y)/sum(y)}))
  }
  negwa <- round(negwa,2) %>% `colnames<-`(rep(w$Date,2)) %>% `rownames<-`(names(coverage_num))
  
  meltneg <- rbind(cbind(melt(negwa[,1:nrow(w)]), 'Before-Divest') %>% `colnames<-`(c('Factor','Date','Score', 'Status')), cbind(melt(negwa[,(nrow(w)+1):(nrow(w_all))]),'Divest') %>% `colnames<-`(c('Factor','Date','Score', 'Status'))) 
  meltneg$Score <- gsub(NaN, 0, meltneg$Score)
  meltneg$Score <- as.numeric(meltneg$Score)
  meltneg$Position <- 'Short'
  
  meltplogt <- rbind(meltpos,meltneg)
  
  meltplogt$Status <- factor(meltplogt$Status, levels = c('Before-Divest','Divest'))
  
  p <- ggplot(meltplogt, aes(x = Date)) + 
    geom_bar(data = subset(meltplogt, Position == "Long"), 
             aes(y = Score, fill = Status), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.2, color="black") +
    geom_bar(data = subset(meltplogt, Position == "Short"), 
             aes(y = (-1)*Score, fill = Status), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.2, color="black") + 
    geom_hline(yintercept = 0,colour = "grey90") +
    facet_wrap(~Factor, scales = "free_y", ncol = 2, strip.position = 'right') +
    labs(x = "Date",
         y = "Score") +
    theme_bw() +
    scale_x_discrete(breaks = everyother) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          strip.text= element_text(size = 10, face = "bold" ),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines"))
  
  fig <- ggplotly(p)
  
  ppos <- ggplot(meltpos, aes(x= Status, y=Score, fill=Factor, group = Status)) +
    geom_boxplot(alpha=0.7) +
    geom_violin(width=1.4, alpha=0.3) +
    stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
    
    scale_fill_viridis(discrete = TRUE) +
    labs(x = " ",
         y = "Score") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          legend.position="none",
          strip.text= element_text(size = 12, face = "bold" ),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines")) + 
    facet_wrap(. ~ Factor, scales="free_y", ncol = 2)
  
  figpos <- ggplotly(ppos) 
  
  pneg <- ggplot(meltneg, aes(x= Status, y=Score, fill=Factor, group = Status)) +
    geom_boxplot(alpha=0.7) +
    geom_violin(width=1.4, alpha=0.3) +
    stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
    
    scale_fill_viridis(discrete = TRUE) +
    labs(x = " ",
         y = "Score") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          legend.position="none",
          strip.text= element_text(size = 12, face = "bold" ),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines")) + 
    facet_wrap(. ~ Factor, scales="free_y", ncol = 2)
  
  figneg <- ggplotly(pneg) 
  
  
  tabpos <- t(poswa[,(nrow(w)+1):(nrow(w_all))]) %>% data.frame() %>% rownames_to_column('Date')
  tabpos$Position <- 'Long'
  tabneg <- t(negwa[,(nrow(w)+1):(nrow(w_all))]) %>% data.frame() %>% rownames_to_column('Date')
  tabneg$Position <- 'Short'
  
  tabfig <- rbind(tabpos,tabneg)
  
  
  return(list(fig,figpos,figneg,tabfig))
}

diff_summary <- function(pertab,pertabdiv,tabfig,tabfigdiv,ly){
  diff_per <- 100*(pertabdiv[-nrow(pertabdiv),-c(1,ncol(pertabdiv))] - pertab[-nrow(pertab),-c(1,ncol(pertab))])/pertab[-nrow(pertab),-c(1,ncol(pertab))]
  diff_per$Date <- as.Date(pertab$Date[-nrow(pertab)])
  diff_fund <- 100*(tabfigdiv[,-c(1,ncol(tabfigdiv))] - tabfig[,-c(1,ncol(tabfig))])/tabfig[,-c(1,ncol(tabfig))]
  diff_fund$Date <- as.Date(tabfig$Date)
  
  tablechange <- merge(diff_per, diff_fund, by = 'Date')
  tablechange[,2:ncol(tablechange)] <- round(tablechange[,2:ncol(tablechange)],6)
  
  
  melttab <- melt(tablechange, id.vars = 'Date') %>% `colnames<-`(c('Date','Factor','Value'))
  
  if(is.na(ly)){ylim.set <- NULL}else{ylim.set <- c(-ly,ly)}
  
  boxchange <- ggplot(melttab, aes(x= Factor, y=Value, fill=Factor)) +
    geom_boxplot(alpha=0.7) +
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=Factor), alpha=0.6) +
    stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
    scale_fill_viridis(discrete = TRUE) +
    coord_cartesian(clip = "off", ylim = ylim.set) +
    #facet_wrap(. ~ Factor, strip.position = 'right') +
    labs(x = " ",
         y = "Percentage of Relative Change") +
    geom_hline(yintercept = 0,colour = "black", linetype = "twodash", alpha = 0.5) +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          legend.position="none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)) 
  
  fig <- ggplotly(boxchange)
  return(list(tablechange,fig))
}

DTtable <- function(tab){
  return(DT::renderDataTable({tab}, 
                      selection = 'none', 
                      server= FALSE,
                      rownames = FALSE,
                      extensions = 'Buttons',
                      options = list(
                        paging = TRUE,
                        searching = TRUE,
                        fixedColumns = TRUE,
                        #autoWidth = TRUE,
                        ordering = TRUE,
                        dom = 'Bfrtip',
                        buttons = c('csv', 'excel'),
                        scrollX = TRUE, 
                        pageLength = 5
                      )))
}


multicomp <- function(div_dynamic,selcom){
  sel <- div_dynamic %>% select(Date, selcom, PORTNAME)
  colnames(sel) <- c('Date','Weight','Port')
  sel$Port <- factor(sel$Port, levels =  unique(sel$Port)) 
  p <- ggplot(sel, aes(x= Date, y = Weight, colour = Port, group= Port, fill = Port)) + 
    geom_bar(stat="identity", position = "dodge", alpha = 0.7, color="black") +
    #scale_fill_viridis(discrete = bool, name = selcat) +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = "Date",
         y = "Weight") +
    theme_bw() +
    scale_x_discrete(breaks = everyother) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  fig <- ggplotly(p)
  return(fig)
}


sep_and_sum <- function(data){
 
 data_w <- data %>% select(-c('Date','PORTNAME'))
 
 pos <- data_w
 pos[pos < 0] <- 0
 pos_w <- rowSums(pos)
 
 neg <- data_w
 neg[neg >= 0] <- 0
 neg_w <- rowSums(neg)
 x <- data.frame(data$Date, pos_w, neg_w, data$PORTNAME)
 colnames(x) <- c('Date','Long','Short', 'PORTNAME')
 return(x)
}


plot_div_Sch_comp <- function(div_dynamic,found){
  
  divasset <- (found %>% dplyr::filter(Status == "Divest"))$Ticker
  divw <- div_dynamic[,c("Date", divasset,"PORTNAME")] %>% group_split(PORTNAME)
  divw <- divw %>% lapply(sep_and_sum) %>% bind_rows()
  
  invasset <- (found %>% dplyr::filter(Status == "Invest"))$Ticker
  invw <- div_dynamic[,c("Date", invasset,"PORTNAME")] %>% group_split(PORTNAME)
  invw <- invw %>% lapply(sep_and_sum) %>% bind_rows()

  
  divw$PORTNAME <- factor(divw$PORTNAME, levels = unique(div_dynamic$PORTNAME))
  invw$PORTNAME <- factor(invw$PORTNAME, levels = unique(div_dynamic$PORTNAME))

  
  p <- ggplot(divw, aes(x = Date)) + 
    geom_bar(data = divw %>% select(-Short), 
             aes(y = Long, fill =PORTNAME), position = "dodge", stat = "identity", color="black") +
    geom_bar(data = divw %>% select(-Long), 
             aes(y = Short, fill =PORTNAME), position = "dodge", stat = "identity", color="black") + 
    geom_hline(yintercept = 0,colour = "grey90") +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = "Date",
         y = "Sum of Weights") +
    theme_bw() +
    scale_x_discrete(breaks = everyother) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  fig1 <- ggplotly(p)
  
  
  p <- ggplot(invw, aes(x = Date)) + 
    geom_bar(data = invw %>% select(-Short), 
             aes(y = Long, fill =PORTNAME), position = "dodge", stat = "identity", color="black") +
    geom_bar(data = invw %>% select(-Long), 
             aes(y = Short, fill =PORTNAME), position = "dodge", stat = "identity", color="black") + 
    geom_hline(yintercept = 0,colour = "grey90") +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = "Date",
         y = "Sum of Weights") +
    theme_bw() +
    scale_x_discrete(breaks = everyother) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  fig2 <- ggplotly(p)
  
  
  return(list(fig1,fig2))
  
}
  

  


compareplot <- function(hist_return,div_dynamic, found){
  
  list_port <- div_dynamic %>%  group_split(PORTNAME)
  nameport <- unique(div_dynamic$PORTNAME)
  
  pertab <- bind_rows(lapply(seq_along(list_port), function(i) all_performance_table(hist_return,list_port[[i]][,-ncol(list_port[[i]])])))
  pertab$Port <- factor(rep(nameport,unlist(lapply(list_port, function(x) nrow(x) + 1))), levels = nameport)
  
  
  pertabm <- melt(pertab) %>% filter(Date != 'Overall')
  
    p1 <- ggplot(pertabm, aes(x= Port, y=value, fill=variable, group = Port)) +
      geom_boxplot(alpha=0.7) +
      #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=variable), alpha=0.6) +
      geom_violin(width=1.4, alpha=0.3) +
      stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
      facet_wrap(. ~ variable, scales="free_y", ncol = 2) +
      scale_fill_viridis(discrete = TRUE) +
      labs(x = "",
           y = "Score") +
      theme_bw() +
      theme(text = element_text(family = 'Fira Sans'),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
            legend.position="none",
            strip.text= element_text(size = 10, face = "bold" ),
            strip.placement = "outside",
            strip.background = element_blank(),
            panel.spacing.y = unit(1, "lines")) 

    fig1 <- ggplotly(p1,width = 500, height = 700) 
    
    pertabm2 <- pertabm  %>% `colnames<-`(c('Date','Status','Risk','Value'))
    
    p2 <- ggplot(pertabm2, aes(x= Date,y= Value)) + 
      geom_line(size= 0.5, position = "identity",aes(group = Status)) +
      guides(group = 'none') +
      geom_point(size=2, position = "identity", aes(shape=Status, fill=Status)) +
      scale_fill_viridis(discrete = TRUE, name = "Portfolio") +
      facet_wrap(~Risk, scales = "free_y", ncol = 2) +
      labs(x = "Date",
           y = "Weight") +
      theme_bw() +
      scale_x_discrete(breaks = everyother) +
      theme(text = element_text(family = 'Fira Sans'),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
            strip.text= element_text(size = 12, face = "bold" ),
            strip.placement = "outside",
            strip.background = element_blank(),
            panel.spacing.y = unit(1, "lines"),
            legend.text = element_text(size=14),
            legend.key.size = unit(7, 'cm'),
            legend.title = element_text(size=12))+
      scale_x_discrete(breaks = everyother) 
    fig2 <- ggplotly(p2, width = 1000, height=900)
    num_factor <- found[, sapply(found, class) %in% c('numeric','integer'), drop=FALSE]
    coverage_num <- apply(num_factor, 2, function(x) (1- sum(is.na(x))/length(x))*100)
    posw <- div_dynamic[,c(-1,-ncol(div_dynamic))]
    posw[posw < 0] <- 0 
    negw <- div_dynamic[,c(-1,-ncol(div_dynamic))]
    negw[negw >= 0] <- 0
    poswa <- NULL
    for(j in 1:ncol(num_factor)){
      poswa <- rbind(poswa,sapply(1:nrow(posw), function(i) {x <- num_factor[,j]; y <- posw[i,];
      x <- x[!is.na(x)]; y <- y[!is.na(x)]; sum(x*y)/sum(y)}))
    }
    poswa <- round(poswa,2) %>% `colnames<-`(div_dynamic$Date) %>% `rownames<-`(names(coverage_num))
    parti <- split(1:ncol(poswa), ceiling(1:ncol(poswa)/nrow(list_port[[1]])))
    meltpos <- bind_rows(lapply(seq_along(parti), function(i) {x <- melt(poswa[,parti[[i]]]); x$port <- nameport[i]; x})) %>% `colnames<-`(c('Factor','Date','Score', 'Port'))
    meltpos$Position <- 'Long'
    negwa <- NULL
    for(j in 1:ncol(num_factor)){
      negwa <- rbind(negwa,sapply(1:nrow(negw), function(i) {x <- num_factor[,j]; y <- negw[i,];
      x <- x[!is.na(x)]; y <- y[!is.na(x)]; sum(x*y)/sum(y)}))
    }
    negwa <- round(negwa,2) %>% `colnames<-`(div_dynamic$Date) %>% `rownames<-`(names(coverage_num))
    meltneg <- bind_rows(lapply(seq_along(parti), function(i) {x <- melt(negwa[,parti[[i]]]); x$port <- nameport[i]; x})) %>% `colnames<-`(c('Factor','Date','Score', 'Port'))
    meltneg$Position <- 'Short'
    meltplogt <- rbind(meltpos,meltneg)
    meltplogt$Port <- factor(meltplogt$Port, levels = nameport)
    
    p <- ggplot(meltplogt, aes(x = Date,y = Score, fill = Port)) + 
      geom_bar(data = subset(meltplogt, Position == "Long"), 
               aes(y = Score, fill = Port), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.2, color="black") +
      geom_bar(data = subset(meltplogt, Position == "Short"), 
               aes(y = (-1)*Score, fill = Port), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.2, color="black") + 
      geom_hline(yintercept = 0,colour = "grey90") +
      facet_wrap(~Factor, scales = "free_y", ncol = 1, strip.position = 'right') +
      labs(x = "Date",
           y = "Score") +
      theme_bw() +
      scale_fill_viridis(discrete = TRUE, name = "Portfolio") +
      scale_x_discrete(breaks = everyother) +
      theme(text = element_text(family = 'Fira Sans'),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
            strip.text= element_text(size = 12, face = "bold" ),
            strip.placement = "outside",
            strip.background = element_blank(),
            panel.spacing.y = unit(1, "lines"),
            legend.key.size = unit(1.2, 'cm'),
            legend.text = element_text(size=10),
            legend.title = element_text(size=14))+
      scale_x_discrete(breaks = everyother) 
    
    fig <- ggplotly(p,width = 1000, height=1000) 
    
    meltpos$Port <- factor(meltpos$Port, levels = nameport)
      
    ppos <- ggplot(meltpos, aes(x= Port, y=Score, fill=Factor, group = Port)) +
      geom_boxplot(alpha=0.7) +
      geom_violin(width=1.4, alpha=0.3) +
      stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
      facet_wrap(. ~ Factor, scales="free_y", ncol = 1, strip.position = 'right') +
      scale_fill_viridis(discrete = TRUE) +
      labs(x = " ",
           y = "Score") +
      theme_bw() +
      theme(text = element_text(family = 'Fira Sans'),
            legend.position="none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
            strip.text= element_text(size = 10, face = "bold" ),
            strip.placement = "outside",
            strip.background = element_blank(),
            panel.spacing.y = unit(1, "lines")) 
    
    figpos <- ggplotly(ppos, width = 500, height = 700) 
    meltneg$Score[is.nan(meltneg$Score)] <- 0
    meltneg$Port <- factor(meltneg$Port, levels = nameport)
    
    pneg <- ggplot(meltneg, aes(x= Port, y=Score, fill=Factor, group = Port)) +
      geom_boxplot(alpha=0.7) +
      geom_violin(width=1.4, alpha=0.3) +
      stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
      facet_wrap(. ~ Factor, scales="free_y", ncol = 1, strip.position = 'right') +
      scale_fill_viridis(discrete = TRUE) +
      labs(x = " ",
           y = "Score") +
      theme_bw() +
      theme(text = element_text(family = 'Fira Sans'),
            legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
            strip.text= element_text(size = 10, face = "bold" ),
            strip.placement = "outside",
            strip.background = element_blank(),
            panel.spacing.y = unit(1, "lines")) 
    
    figneg <- ggplotly(pneg, width = 500, height = 700) 
  
    
    tabf <- rbind(meltneg,meltpos)
    tabfn <- spread(tabf,Factor,Score) %>% dplyr::arrange(Port,Position,Date) 

    return(list(pertab,tabfn,fig1,fig2,fig,figpos,figneg))
}
 
  diff_summary_mul <- function(pertab,tabfn,ben.port,ly){
    name.port <- unique(pertab$Port)
    pertab$Port <- factor(pertab$Port, levels = name.port)
    list.perf <- pertab %>% dplyr::filter(Date != 'Overall')%>% group_split(Port)
    list.att <- tabfn %>% group_split(Port)
    list.att <- lapply(list.att, function(x) {y <- x %>% group_split(Position); 
    y[[1]][,sapply(y[[1]], class) %in% c('numeric','integer')] + y[[2]][,sapply(y[[2]], class) %in% c('numeric','integer')]} )
    ben.index <- which(ben.port %in% name.port)
    ben.index <- c(ben.index,length(list.perf)+1)
    rem.port <- setdiff(name.port,ben.port)
    
    dif.perf <- list()
    dif.att <- list()
    d <- 1
    for(i in 1:(length(ben.index)-1)){
      for(j in (ben.index[i]+1):(ben.index[i+1]-1)){
        ben.perf.dum <- list.perf[[ben.index[i]]][,sapply(list.perf[[ben.index[i]]], class) %in% c('numeric','integer')] 
        perf.dum <-list.perf[[j]][,sapply(list.perf[[j]], class) %in% c('numeric','integer')]
        re.perf <- 100*(perf.dum - ben.perf.dum)/ben.perf.dum
        re.perf[re.perf== 'NaN'] <- 0
        re.perf$Port <- rem.port[d]
        dif.perf <- append(dif.perf,list(re.perf))
        ben.att.dum <- list.att[[ben.index[i]]][,sapply(list.att[[ben.index[i]]], class) %in% c('numeric','integer')] 
        att.dum <-list.att[[j]][,sapply(list.att[[j]], class) %in% c('numeric','integer')]
        re.att <- 100*(att.dum - ben.att.dum)/ben.att.dum
        re.att[re.att== 'NaN'] <- 0
        dif.att <- append(dif.att,list(re.att))
        d <- d + 1
      }
    }
    
    dif.perf.tab <-  bind_rows(dif.perf) %>% dplyr::select(-Days)
    dif.att.tab <- bind_rows(dif.att)
    final.tab <- cbind(dif.perf.tab,dif.att.tab)
    final.tab$Port <- factor(final.tab$Port, levels = name.port)
    melttab <- melt(final.tab, id.vars = 'Port') %>% `colnames<-`(c('Port','Factor','Value'))
    
    if(is.na(ly)){ylim.set <- NULL}else{ylim.set <- c(-ly,ly)}
      
    boxchange <- ggplot(melttab, aes(x= Factor, y=Value, fill=Factor)) +
      geom_boxplot(alpha=0.7) +
      stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
      scale_fill_viridis(discrete = TRUE) +
      coord_cartesian(clip = "off", ylim = ylim.set) +
      facet_wrap(. ~ Port, strip.position = 'right', ncol = 1) +
      labs(x = " ",
           y = "Percentage of Relative Change") +
      geom_hline(yintercept = 0,colour = "black", linetype = "twodash", alpha = 0.5) +
      theme_bw() +
      theme(text = element_text(family = 'Fira Sans'),
            legend.position="none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 15),
            strip.text= element_text(size = 15, face = "bold" ),
            strip.placement = "outside",
            strip.background = element_blank(),
            panel.spacing.y = unit(1, "lines")) 
    
    fig <- ggplotly(boxchange, width = 1000, height=1000)
    Date <- rep(unique(tabfn$Date), length(name.port)-1)
    return(list(cbind(Date,final.tab),fig))
  }
  

pmr <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else mean(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}

pmrcum <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else sum(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}

prisk <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else sd(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}

sharpe <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else (mean(portreturn[i:(i-tau+1)])-0)/sd(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}

pVaR <- function(portreturn,tau,conf){
  x <- sapply(seq_along(portreturn),function(i) if (i < tau) NA else quantile(portreturn[i:(i-tau+1)], conf))
  return(x[!is.na(x)])
}

MDD <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else (min(portreturn[i:(i-tau+1)])-max(portreturn[i:(i-tau+1)]))/(max(portreturn[i:(i-tau+1)])))
  return(x[!is.na(x)])
}

SR <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else (mean(portreturn[i:(i-tau+1)]) - 0)/sqrt(((1/(tau-1))*sum((portreturn[i:(i-tau+1)]-mean(portreturn[i:(i-tau+1)]))^2*((portreturn[i:(i-tau+1)]- 0) < 0)))))
  x <- x[!is.na(x)]
  x[abs(x) > 5] <- NA
  return(x[!is.na(x)])
}

agg.perf <- function(Port_Return,tau,date){
  Date <- tail(date,length(date) - tau+1)
  Arg_Return <- pmr(Port_Return,tau)
  Arg_Cumsum <- pmrcum(Port_Return, tau)
  Arg_Sd <- prisk(Port_Return, tau)
  Arg_Sharpe <- sharpe(Port_Return, tau)
  Arg_VaR <- pVaR(Port_Return, tau, 0.05)
  Arg_MDD <- MDD(Port_Return,tau)
  Arg_SR <- SR(Port_Return,tau)
  dat <- as.data.frame(list(return = Arg_Return, cumsum = Arg_Cumsum, sd = Arg_Sd, sharpe = Arg_Sharpe, var = Arg_VaR, mdd = Arg_MDD, sortino = Arg_SR))
  colnames(dat) <- c('Return','Cumulative Return','Volatility','Sharpe','VaR','Max Drawdown','Sortino')
  return(cbind(Date,dat))
}


Annual_Best_Cluster <- function(X, sept){
  limt <- ceiling(nrow(X)/sept)
  X$year <- rep(1:limt, each = sept)[1:nrow(X)]
  ben.date <- rownames(X)[seq(1,length(X$year),sept)]
  X_Year <- X %>% group_split(year) %>% lapply(function(x) ungroup(x) %>% select(-'year'))
  Final_Tab <- c()
  for(i in 1:length(X_Year)){
    Tab_Sil <- lapply(1:(ncol(X_Year[[i]])-1), function(j) {
      clara(x = t(scale(X_Year[[i]])), k = j, metric = "euclidean", samples = 4000, pamLike = TRUE)$silinfo$avg.width
    }) 
    Final_Tab <- rbind(Final_Tab,Tab_Sil)
  }
  Final_Tab <- as.data.frame(Final_Tab) %>%'rownames<-'(ben.date) %>%'colnames<-'(1:(ncol(X)-2))
  Final_Tab[Final_Tab == "NULL"] <- 0
  Max_Sil <- apply(Final_Tab,1,function(x) max(unlist(x)))
  Final_Tab$Best <- names(Final_Tab)[apply(Final_Tab, 1, function(i) which(i %in% Max_Sil)[1])]
  return(Final_Tab)
}

Matrix_Cluster <- function(X,Perf,Best_Cluster_Year, sept){
  limt <- ceiling(nrow(X)/sept)
  X$year <- rep(1:limt, each = sept)[1:nrow(X)]
  ben.date <- rownames(X)[seq(1,length(X$year),sept)]
  X_Year <- X %>% group_split(year) %>% lapply(function(x) ungroup(x) %>% select(-'year'))
  Final_Tab <- c()
  for(i in 1:length(X_Year)){
    Tab_Clus <- clara(x = t(scale(X_Year[[i]])), k = Best_Cluster_Year$Best[i], metric = "euclidean", samples = 4000, pamLike = TRUE)$cluster
    Final_Tab <- rbind(Final_Tab,Tab_Clus)
  }
  rownames(Final_Tab) <- ben.date
  meltmap <- as.data.frame(Final_Tab) %>% rownames_to_column('Date') %>% melt(id.vars='Date')
  meltmap$Performance <- Perf 
  return(meltmap)
}


plot.hm <- function(each.perf,best.clus,sept){
  perf.names <- c('Return','Cumulative Return','Volatility','Sharpe','VaR','Max Drawdown','Sortino')
  Heatmap_Melt <- lapply(1:length(each.perf), function(i) Matrix_Cluster(each.perf[[i]],perf.names[i],best.clus[[i]],sept)) %>% bind_rows()
  
  Heatmap_Melt$variable <- factor(Heatmap_Melt$variable, levels=colnames(each.perf[[1]]))
  Heatmap_Melt$Performance<- factor(Heatmap_Melt$Performance, levels=perf.names)
 
  
  p <- ggplot(Heatmap_Melt, aes(Date,variable, fill=value)) +
    geom_raster() +
    geom_tile(colour="white",size=0.05) +
    geom_text(aes(label = value), colour="white") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), panel.grid.major = element_blank(),legend.position="none") +
    scale_fill_viridis(discrete = FALSE)  +
    labs(x = "Date", y = "Portfolio Type") +
    facet_wrap(~Performance, ncol = 2) +
    theme(axis.text.x = element_text(face= "bold",size = 10),
          axis.text.y = element_text(face= "bold", size = 10),
          strip.text.x = element_text(size = 10),
          axis.title = element_text(size=15),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          strip.text= element_text(size = 7, face = "bold" ),
          strip.placement = "outside",
          strip.background = element_blank())
  
  fig <- ggplotly(p, width = 1000, height=1000)
  return(fig)

}


clust.option <- function(Div_Dynamic,Return,tau,sept){
  perf.names <- c('Return','Cumulative Return','Volatility','Sharpe','VaR','Max Drawdown','Sortino')
  portname <- unique(Div_Dynamic$PORTNAME)
  Div_Dynamic$PORTNAME <- factor(Div_Dynamic$PORTNAME, levels = portname)
  return_list <- Div_Dynamic %>% group_split(PORTNAME) %>% lapply(function(x) {x %>% dplyr::select(-PORTNAME) %>% multipywtoreturn(Return) %>% bind_rows() %>% rowSums()})
  Date <- Return$Date[Return$Date >= unique(Div_Dynamic$Date)[1]]
  perf_list <- lapply(return_list,agg.perf,tau = tau,date = Date) %>% bind_rows()
  perf_list$Port <- rep(portname, each = nrow(perf_list)/length(portname))

  mperf <- melt(perf_list, id.vars = c('Date','Port'))
  mperf$Port <- factor(mperf$Port, levels = unique(Div_Dynamic$PORTNAME))
  
  p <- ggplot(data = mperf, aes(x = as.Date(Date), y = value, group = Port, fill = Port, color = Port)) +
    geom_line() +
    viridis::scale_color_viridis(discrete = TRUE) +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +
    labs(title = "Risk Profiles",
         x = "Date",
         y = "Aggregate Score") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          #legend.position="none",
          strip.text= element_text(size = 12, face = "bold" ),
          legend.text = element_text(size=10),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing.y = unit(1, "lines"))
  fig <- ggplotly(p, width = 1000, height=1000)
  
  perf_list$Port <- factor(perf_list$Port, levels = unique(Div_Dynamic$PORTNAME))
  each.perf <- melt(perf_list, id.var = c('Date','Port')) %>% group_split(variable) %>% lapply(function(x) {y <- dcast(x[,-3], Date ~  Port); y <- y %>% column_to_rownames('Date'); y})
  best.clus <- lapply(each.perf,Annual_Best_Cluster, sept = sept)
  
  best.clus.tab <- lapply(seq_along(best.clus), function(i) {x <- best.clus; x[[i]]$perf <- perf.names[i]; x[[i]] <- rownames_to_column(x[[i]], 'Date') ; x[[i]]}) %>% bind_rows()
  fig2 <- plot.hm(each.perf,best.clus,sept)
  return(list(perf_list,best.clus.tab,fig,fig2))
}


sturcture.cov <- function(return_mat){
  ret_gl <- return_mat
  all.ticker <- colnames(ret_gl)
  cols <- colSums(ret_gl) 
  if(!identical(cols[cols == 0],numeric(0))){
    exc.ticker <- names(cols[cols==0])
    ret_gl <- ret_gl[,setdiff(all.ticker,exc.ticker)]
  }
  
  X.npn = huge.npn(ret_gl, npn.func="truncation")
  out.npn = huge(X.npn, method = "glasso")
  h <- huge.select(out.npn, criterion = 'ric')$opt.lambda
  out.op = huge(X.npn, lambda =h, method = "glasso",cov.output=TRUE)
  op.adj <- out.op$path[[1]]
  op.cov <- out.op$cov[[1]]
  adj.cov <- op.adj*op.cov
  dimnames(adj.cov) <- list(colnames(ret_gl),colnames(ret_gl))
  if(!identical(cols[cols == 0],numeric(0))){
  zeros.add <- matrix(0,length(all.ticker) - ncol(adj.cov), length(all.ticker) - ncol(adj.cov))
  adj.cov.add <- adiag(adj.cov,zeros.add )
  dim.name <- c(setdiff(all.ticker,exc.ticker), exc.ticker)
  dimnames(adj.cov.add) <- list(dim.name,dim.name)
  adj.cov <- adj.cov.add[all.ticker,all.ticker]
  }
  return(adj.cov)
}
  

plot.qgraph <- function(adj.cov, title, found = NULL){
  name.cov <- colnames(adj.cov)
  dimnames(adj.cov) <- list(1:ncol(adj.cov),1:ncol(adj.cov))
  if(!is.null(found)){
    st <- found %>% select(Ticker,Status) %>% column_to_rownames('Ticker')
    st$Status[st$Status=='Invest'] <- "#31688EFF"
    st$Status[st$Status=='Divest'] <- '#FDE725FF'
    colors <- unlist(st,use.names = F)
  }else{
    colors <- NULL
  }
  q <- qgraph(adj.cov,esize=8,title = title,label.scale = T,label.prop = 0.9, shape= "circle", border.width = 3, label.cex = 0.8, posCol= "darkgreen", negCol="darkred", layout="groups", vsize=8, colors = colors,
              legend.cex = 0.8,
              legend.mode = 'style2',
              nodeNames = name.cov, 
              font = 2,
              curveAll = T, 
              curveDefault = 0.5,
              title.cex = 1.7
              ) 
  return(q)
}


get.graph <- function(div_dynamic, return_mat, data.index, found = NULL){
  nameport <- unique(div_dynamic$PORTNAME)
  div_dynamic$PORTNAME <- factor(div_dynamic$PORTNAME,levels = unique(nameport))
  w_list <- div_dynamic %>% group_split(PORTNAME) %>% lapply(function(x) {x %>% dplyr::select(-PORTNAME) %>% multipywtoreturn(return_mat)})
  names(w_list) <- nameport
  sel.port <- lapply(w_list, function(x) x[data.index]) %>% unlist(recursive=FALSE)
  list.cov <- sel.port %>% lapply(sturcture.cov)
  return({
  par(mfrow=c(length(nameport),1))
  sapply(seq_along(list.cov), function(i) plot.qgraph(list.cov[[i]],names(sel.port)[i],found))
  })
}



map_index <- function(index_choice_input){
  index_country <-  (all.tick %>% dplyr::filter(Index == index_choice_input))$Country %>% unique()
  World$color <-  as.integer(World$name==index_country)
  World$color[World$color == 0] <- NA
  World$con <- World$name
  World$con[World$con != index_country] <- NA
  p <- tm_shape(World) +
    tm_borders(lwd = 1) +
    tm_text("con", size = 1) +
    tm_polygons("color", alpha = 0.7, legend.show = FALSE, border.col = , style="cont", n=2, group = "Countries", colorNA = 'gray') +
    tm_layout(bg.color = "skyblue", inner.margins = c(0, .02, .02, .02))
  
  tack <- unlist((all.tick %>% filter(Index == index_choice_input))$Ticker, use.names =  F)
  return(list(p,tack))
}


data(World)
all.tick <- read_csv("update_suggest.csv")
tidy.tick <- all.tick %>% select(Country, Index) %>% distinct(Country, Index, .keep_all = TRUE) %>% mutate(paste0(Index, " (",Country,")")) %>% `colnames<-`(c("Country", "Index", "CwithI"))
index_choice <- as.list(tidy.tick$Index) %>% `names<-` (c(tidy.tick$CwithI))

main_port <- c()
div_stock <- NULL
inv_stock <- NULL
rebalancedate <- NULL
ticker_rec <- "AAP"
Carbon <- c(29884383.57/21, 16488511.67/15, 6951694.12/24, 3572568.61/45, 2566298.00/13, 2539331.17/24, 1384162.29/41, 618047.76/48, 390973.88/24, 337598.75/39, 147601.10/38)
Sector <- c('Utilities','Energy','Basic Materials','Industrials','Communication Services','Consumer Defensive','Consumer Cyclical','Technology','Real Estate','Healthcare','Financial Services')
carbon_table <- as.data.frame(Carbon,Sector)

file1_Status <- NULL
file2_Historical <- NULL
file3_Weight <- NULL
file4_Schedule <- NULL
file5_Weight_Div <- NULL
file6_Div_Dynamic <- NULL
file7_Attribute <- NULL
file8_Return <- NULL
validcol <- c("red", "yellow", "aqua", "blue", "light-blue", "green", "navy", "teal", "olive", "lime", "orange", "fuchsia", "purple", "maroon", "black")
