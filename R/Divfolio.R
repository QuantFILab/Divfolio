#' @importFrom remotes install_github
#' @importFrom BatchGetSymbols BatchGetSymbols
#' @import rvest
#' @import shinydashboard
#' @import shiny
#' @import plotly
#' @import htmltab
#' @import shinycustomloader
#' @import shinydashboardPlus
#' @import waiter
#' @import shinyWidgets
#' @import dashboardthemes
#' @import quantmod
#' @import PerformanceAnalytics
#' @import reshape2
#' @import viridis
#' @import shinyjqui
#' @import DT
#' @import BatchGetSymbols
#' @import lubridate
#' @import robustbase
#' @import shinyBS
#' @import cluster
#' @import huge
#' @import qgraph
#' @import magic
#' @import shinypop
#' @import shinyFeedback

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

# academic_profile_box <- function(guser,title = NULL,subtitle= NULL,picture= NULL){
#   url <- paste0("https://scholar.google.com/citations?hl=en&user=",guser)
#   page <- read_html(url)
#   name <- ifelse(is.null(title),page %>% html_nodes(xpath = '//*[@id="gsc_prf_in"]') %>% html_text(),title)
#   subt <- ifelse(is.null(subtitle),page %>% html_nodes(xpath = '//*[@id="gsc_prf_i"]/div[2]') %>% html_text(),subtitle)
#   gbox <- userBox(
#     width = 6,
#     title = userDescription(
#       title = HTML(paste("<p style='color:white'>",name,"</p>")),
#       subtitle = HTML(paste("<p style='color:white'>",subt,"</p>")),
#       backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
#       image = ifelse(is.null(picture),paste0("https://scholar.googleusercontent.com/citations?view_op=view_photo&user=",guser,"&citpid=1"),picture),
#       type = 2
#     ),
#     status = "teal",
#     boxToolSize = "xl",
#     footer = "The footer here!"
#   )
#   return(gbox)
# }

# google_scholar_box <- function(guser){
#   url <- paste0("https://scholar.google.com/citations?hl=en&user=",guser)
#   page <- read_html(url)
#   name <- page %>% html_nodes(xpath = '//*[@id="gsc_prf_in"]') %>% html_text()
#   subt <- page %>% html_nodes(xpath = '//*[@id="gsc_prf_i"]/div[2]') %>% html_text()
#   gbox <- userBox(
#     width = 6,
#     title = userDescription(
#       title = HTML(paste("<p style='color:white'>",name,"</p>")),
#       subtitle = HTML(paste("<p style='color:white'>",subt,"</p>")),
#       backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
#       image = paste0("https://scholar.googleusercontent.com/citations?view_op=view_photo&user=",guser,"&citpid=1"),
#       type = 2
#     ),
#     status = "teal",
#     boxToolSize = "xl",
#     footer = "The footer here!"
#   )
#   return(gbox)
# }

check <- function(ticker){
  ticker <- toupper(ticker)
  profile <- paste0('https://finance.yahoo.com/quote/',ticker,'/profile?p=',ticker) %>% read_html()
  comn <- profile %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/h3') %>% html_text()
  return(ifelse(identical(comn, character(0)),0,1))
}



get_stock_profile <- function(ticker){
  re <- rep('-',14)
  ticker <- toupper(ticker)
  profile <- paste0('https://finance.yahoo.com/quote/',ticker,'/profile?p=',ticker) %>% read_html()
  comn <- profile %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/h3') %>% html_text()
  if(identical(comn, character(0))||(comn == "")||(is.na(comn))){re <- rep('-',14); re[1] <- 'Unavailable'; re[5] <- 'black'}else{
    table <- htmltab::htmltab(paste0('https://finance.yahoo.com/quote/',ticker,'?p=',ticker), which ='//*[@id="quote-summary"]/div[2]/table', header = 0)
    #check <- ifelse(identical(comn, character(0)),0,1
    sector <- profile %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[2]') %>% html_text()
    subsector <- profile %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[4]') %>% html_text()
    market <- profile %>% html_nodes(xpath = '//*[@id="quote-header-info"]/div[2]/div[1]/div[2]/span') %>% html_text()
    sust <- paste0('https://finance.yahoo.com/quote/',ticker,'/sustainability?p=',ticker) %>% read_html()
    egs <- rep("-",7)
    test_avi <- sust %>% html_nodes(xpath = '//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/h3/span')
    com_name <- sust %>% html_nodes(xpath = '//*[@id="quote-header-info"]/div[2]/div[1]/div[1]/h1') %>% html_text()
    egs[2] <- "red"
    if(length(test_avi) != 0){
      for(i in 3:6){
        egs[i] <- sust %>% html_nodes(xpath = paste0('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[',(i-2),']/div/div[2]/div[1]')) %>% html_text()
      }
      egs[1] <- sust %>% html_nodes(xpath = '//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[3]/div/span') %>% html_text()
      egs[7] <- sust %>% html_nodes(xpath ='//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[2]/div[2]/span/span') %>% html_text()
      if(egs[1] == "High"){
        egs[2] <- "red"
      }
      if(egs[1] == "Medium"){
        egs[2] <- "yellow"
      }
      if(egs[1] == "Low"){
        egs[2] <- "green"
      }
      if(egs[1] == "Negligible"){
        egs[2] <- "light-blue"
      }
      
    }
    re <- c(com_name,sector,subsector,egs,table$V2[1],table$V2[3],table$V2[6], market)
  }
  
  return(re)
}

get_esg_profile <- function(ticker){
  ticker <- toupper(ticker)
  profile <- paste0('https://finance.yahoo.com/quote/',ticker,'/profile?p=',ticker) %>% read_html()
  comn <- profile %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/h3') %>% html_text()
  sector <- profile %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[2]') %>% html_text()
  subsector <- profile %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[4]') %>% html_text()
  sust <- paste0('https://finance.yahoo.com/quote/',ticker,'/sustainability?p=',ticker) %>% read_html()
  egs <- rep("-",5)
  test_avi <- sust %>% html_nodes(xpath = '//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/h3/span')
  com_name <- sust %>% html_nodes(xpath = '//*[@id="quote-header-info"]/div[2]/div[1]/div[1]/h1') %>% html_text()
  if(length(test_avi) != 0){
    for(i in 2:5){
      egs[i] <- sust %>% html_nodes(xpath = paste0('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[',(i-1),']/div/div[2]/div[1]')) %>% html_text()
    }
    check <- sust %>% html_nodes(xpath = '//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[3]/div/span') 
    egs[1] <- ifelse(length(check) == 0, sust %>% html_nodes(xpath = ' //*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[2]/div[1]') %>% html_text(),
                     check %>% html_text())
  }
  Sys.sleep(1)
  return(c(com_name,ticker,sector,subsector,egs))
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

# corr_plot <- function(returnmat,rebdate,datesel){
#   inx <- which(rebdate == datesel)
#   selcor <- cor(returnmat[[inx]])
#   z <- melt(replace(selcor, lower.tri(selcor, TRUE), NA), na.rm = TRUE) 
#   z$Date <- rebdateadd[inx]
#   colnames(z) <- c('X', 'Y', 'Corr','Date')
#   p <- plot_ly(x=colnames(selcor), y= rev(rownames(selcor)), 
#                z = selcor[,rev(rownames(selcor))], 
#                type = "heatmap", 
#                colorscale= "Viridis",
#                showscale = T) %>%
#     layout(margin = list(l=120),title = "Correlation of Return of Assets in Portfolio",
#            legend = list(title=list(text='<b> Correlatio </b>')))
#   return(p)
# }

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

# plot_radar <- function(pertab){
#   last_row <- pertab[nrow(pertab),-c(1,ncol(pertab))]
#   tabnew <- pertab[-nrow(pertab),-c(1,ncol(pertab))]
#   radar <- bind_rows(apply(tabnew,2,max), apply(tabnew,2,min), last_row) %>% data.frame() %>% round(4) 
#   colnames(radar) <- sapply(1:ncol(radar), function(i) paste0(colnames(radar)[i]," = ",radar[3,i]) )
#   par(xpd = TRUE)
#   p <- radarchart(radar,
#              cglty = 1,       # Grid line type
#              cglcol = "gray", # Grid line color
#              pcol = 4,
#              title = "Overall Performance",
#              plwd = 3,        # Width for each line
#              plty = 1,        # Line type for each line
#              pfcol = rgb(0.1490196, 0.5098039, 0.5568627, 0.5))   # Color of the areas  
#   return(p)
# }

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
  
  #Combination of Efficient Portfolio is Invarant
  asig <- vartan - vargmv 
  csig  <- vargmv + 0.001
  
  alpcalp <- sqrt(csig*asig)/asig
  alp <- seq(-alpcalp,alpcalp,(2*alpcalp)/100)
  exeff <- (1-alp)*exgmv + (alp)*extan 
  sdeff <- sqrt((alp^2)*vartan + ((1-alp)^2)*vargmv  + 2*alp*(1-alp)*covgmvtan)
  meltplot <- data.frame(exeff,sdeff) 
  return(meltplot)
}

efficient_fontier <- function(x,y){
  
  #w,returnmat,covmat,rebdate
  rebdate <- unique(c(x$Date[1],y$Date))
  returnmat <- lapply(seq_along(rebdate)[-length(rebdate)], function(i) {rowtoday <- which(x$Date == as.Date(rebdate[i]));
  rowfuture <- which(x$Date == as.Date(rebdate[i+1])) - 1; x[(rowtoday):(rowfuture),-1]}) %>% lapply(function(m) {m[is.na(m)] <- 0; m}) %>% lapply(function(m) {ifelse(length(m[m == 0]) == 0, m <- m,m[m == 0] <- rnorm(length(m[m == 0]),0.0001,0.0001)); m}) 
  
  wmat <- lapply(1:nrow(y), function(i) as.matrix(returnmat[[i]])%*%t(as.matrix(y[i,-1]))) %>% lapply(function(m) {mm <- data.frame(mean(m),sd(m)) %>% `colnames<-`(c('Mu','SD')); mm}) %>% bind_rows()
  wmat$Date <- y$Date
  
  
  
  covmat <- lapply(returnmat, function(y) {cov(y)})
  invcov <- lapply(covmat, function(y) {solve(y,tol = 1E-100)}) 
  
  #Check Accuracy
  crit <- sapply(seq_along(covmat), function(i) sum(covmat[[i]]%*%invcov[[i]]))
  if(sum((crit >= (ncol(x)-1) - 1E-10)&&(crit <= (ncol(x)-1) + 1E-10)) != 1){
    covmat <- lapply(returnmat, function(y) {covr <- as.matrix(covOGK(y,sigmamu = s_mad)$cov); dimnames(covr) <- list(colnames(x)[-1],colnames(x)[-1]); covr})
    invcov <- lapply(covmat, function(y) {s <- as.matrix(solve(as.matrix(y),tol = 1E-100)); s[lower.tri(s)] <- t(s)[lower.tri(s)]; s} )}
  ef <- lapply(seq_along(invcov), function(i) {res <- ex_var_efficient(invcov[[i]],apply(returnmat[[i]] ,2,mean)); res$Date <- rebdate[i]; res}) %>% bind_rows()
  
  return(list(ef,wmat))
}


plot_efficient_fontier <- function(ef,w,wmat,seldate){
  #pertab is forward testing!!!
  wmat$Date <- as.character(wmat$Date)
  wmatsub <- subset(wmat, Date %in% seldate) %>% `colnames<-`(c('exeff','sdeff','Date'))
  efsub <- subset(ef, Date %in% seldate)
  
  p <- ggplot(efsub,aes(x = sdeff,y = exeff, colour = Date)) +
    geom_point() +
    scale_colour_viridis_d()+
    #scale_fill_viridis(discrete = FALSE) +
    geom_point(data = wmatsub, shape = 8, size =4) +
    #aes(shape = Date)
    #geom_point(aes(x =sqrt(vartan),y= extan), colour="black", size =4) +
    labs(x = "Standard Deviation",
         y = "Expected Return",
         colour = 'Portfolio'
         #shape = 'Portfolio'
    ) +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(vjust = 0.5, hjust=1))
  fig <- ggplotly(p)
  return(p)
}


#Table
#Not equal Ticker? Acceptable?

plot_fundamental <- function(w,found){
  w <- w[,c('Date',found$Ticker)]
  #char_factor <- found[, sapply(found, class) == 'character', drop=FALSE] 
  num_factor <- found[, sapply(found, class) %in% c('numeric','integer'), drop=FALSE]
  
  #coverage_char <- apply(char_factor, 2, function(x) (1- sum(is.na(x))/length(x))*100)
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
    #facet_grid(Factor~.,  scales = "free", space = "free", shrink = TRUE) +
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
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=Factor), alpha=0.6) +
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
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=Factor), alpha=0.6) +
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
          #legend.position= 'none',
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
  
  Invest_List <- found %>% filter(Status == 'Invest') %>% .$Ticker
  
  Divest_List <- found %>% filter(Status == 'Divest') %>% .$Ticker
  
  Bound <- unlist(Schedule$Bound,use.names =  FALSE)
  
  final_frame <- c()
  
  for(k in 1:nrow(w)){
    
    Invest <- w[,Invest_List][k,]
    Divest <- w[,Divest_List][k,]
    
    Long_Divest <- Divest[which(Divest >= 0)] 
    Short_Divest <- Divest[which(Divest < 0)]
    if(length(Short_Divest)==0){Short_Divest <- NULL}
    
    #Sum
    Long_Divest_Sum <- if (length(Long_Divest)==0) {0} else sum(Long_Divest)
    #Prevent Long only Port
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
    #scale_fill_manual(values = scales::viridis_pal()(10)[c(3,9)]) +
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
  
  #melt_frame_inv$Date <- as.Date(melt_frame_inv$Date)
  
  p <- ggplot(melt_frame_inv %>% arrange(desc(Weight)), aes(x = Date)) + 
    geom_bar(data = subset(melt_frame_inv, Position == "Long"),
             aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    geom_bar(data = subset(melt_frame_inv, Position == "Short"),
             aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    #scale_fill_manual(values = scales::viridis_pal()(10)[c(3,9)]) +
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


##################################Comparison-Div##########################

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


# plot_radar_div <- function(pertab,pertabdiv){
#   last_row <- pertab[nrow(pertab),-c(1,ncol(pertab))]
#   last_row_div <- pertabdiv[nrow(pertabdiv),-c(1,ncol(pertabdiv))]
#   tabnew <- rbind(pertab[-nrow(pertab),-c(1,ncol(pertab))],pertabdiv[-nrow(pertabdiv),-c(1,ncol(pertabdiv))])
#   radar <- bind_rows(apply(tabnew,2,max), apply(tabnew,2,min), last_row, last_row_div) %>% data.frame() %>% round(4) %>%
#     dplyr::select(-Return.cumulative,-MDD)
#   colnames(radar) <- sapply(1:ncol(radar), function(i) paste0(colnames(radar)[i]," = ",radar[3,i]," (Non) ",radar[4,i]," (Div)") )
#   
#   vc <- viridis_pal()(10)[c(2,9)]
#   
#   colors_border=c(vc[1], vc[2])
#   colors_in= alpha(colors_border,0.3)
#   
#   par(xpd = TRUE)
#   p <- radarchart(radar,
#                   cglty = 1,       # Grid line type
#                   cglcol = "gray", # Grid line color
#                   pcol=colors_border,
#                   title = "Overall Performance",
#                   plwd = 3,        # Width for each line
#                   plty = 1,        # Line type for each line
#                   pfcol = colors_in)   # Color of the areas  
#   legend(x=0.7, y=1, legend = c('Before-Divest','Divest'), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)
#   return(p)
# }

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
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=variable), alpha=0.6) +
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
  #Delete OVerall
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
  
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  #one col
  
  return(fig)
}


plot_fundamental_div <- function(w,w_div,found){
  w <- w[,c('Date',found$Ticker)]
  w_div <- w_div[,c('Date',found$Ticker)]
  w_all <- rbind(w,w_div)
  #char_factor <- found[, sapply(found, class) == 'character', drop=FALSE] 
  num_factor <- found[, sapply(found, class) %in% c('numeric','integer'), drop=FALSE]
  
  #coverage_char <- apply(char_factor, 2, function(x) (1- sum(is.na(x))/length(x))*100)
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
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=Factor), alpha=0.6) +
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
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=Factor), alpha=0.6) +
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

#Order of the list
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


#############################Optional###########################

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
    #scale_fill_manual(values = scales::viridis_pal()(10)[c(3,9)]) +
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
    #scale_fill_manual(values = scales::viridis_pal()(10)[c(3,9)]) +
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
  
  #fig1 <- ggplotly(p1, width = 1000, height=500)
  fig1 <- ggplotly(p1,width = 500, height = 700) 
  
  pertabm2 <- pertabm  %>% `colnames<-`(c('Date','Status','Risk','Value'))
  
  p2 <- ggplot(pertabm2, aes(x= Date,y= Value)) + 
    #geom_area(alpha=0.5, position = "identity") +
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
  #fig2 <- ggplotly(p2)
  
  
  #char_factor <- found[, sapply(found, class) == 'character', drop=FALSE] 
  num_factor <- found[, sapply(found, class) %in% c('numeric','integer'), drop=FALSE]
  
  #coverage_char <- apply(char_factor, 2, function(x) (1- sum(is.na(x))/length(x))*100)
  coverage_num <- apply(num_factor, 2, function(x) (1- sum(is.na(x))/length(x))*100)
  
  #posw <- lapply(list_port, function(x) {y <- x[-1,]; y[y < 0] <- 0; y})
  #negw <- lapply(list_port, function(x) {y <- x[-1,]; y[y  >= 0] <- 0; y})
  
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
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=Factor), alpha=0.6) +
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
  #Wddd
  meltneg$Score[is.nan(meltneg$Score)] <- 0
  meltneg$Port <- factor(meltneg$Port, levels = nameport)
  
  pneg <- ggplot(meltneg, aes(x= Port, y=Score, fill=Factor, group = Port)) +
    geom_boxplot(alpha=0.7) +
    geom_violin(width=1.4, alpha=0.3) +
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=Factor), alpha=0.6) +
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
  #tabfn$Port <- factor(tabfn$Port, levels = unique(div_dynamic$PORTNAME))
  tabfn <- spread(tabf,Factor,Score) %>% dplyr::arrange(Port,Position,Date) #dcast one column
  
  return(list(pertab,tabfn,fig1,fig2,fig,figpos,figneg))
}

###############

diff_summary_mul <- function(pertab,tabfn,ben.port,ly){
  #ben.port <- c('PEW')
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
      #perf
      ben.perf.dum <- list.perf[[ben.index[i]]][,sapply(list.perf[[ben.index[i]]], class) %in% c('numeric','integer')] 
      perf.dum <-list.perf[[j]][,sapply(list.perf[[j]], class) %in% c('numeric','integer')]
      re.perf <- 100*(perf.dum - ben.perf.dum)/ben.perf.dum
      re.perf[re.perf== 'NaN'] <- 0
      re.perf$Port <- rem.port[d]
      dif.perf <- append(dif.perf,list(re.perf))
      #att
      ben.att.dum <- list.att[[ben.index[i]]][,sapply(list.att[[ben.index[i]]], class) %in% c('numeric','integer')] 
      att.dum <-list.att[[j]][,sapply(list.att[[j]], class) %in% c('numeric','integer')]
      re.att <- 100*(att.dum - ben.att.dum)/ben.att.dum
      re.att[re.att== 'NaN'] <- 0
      #re.att$Port <- rem.port[d]
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
    #geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=Factor), alpha=0.6) +
    stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
    scale_fill_viridis(discrete = TRUE) +
    coord_cartesian(clip = "off", ylim = ylim.set) +
    #ylim(-40, 40) + 
    #facet_wrap(. ~ Port, scales="free_y", strip.position = 'right', ncol = 1) +
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

# Div_Dynamic <- file6_Div_Dynamic
# ben.sel <- 'benchmark'
# 
# Div_Dynamic %>% filter(PORTNAME %in% ben.sel) %>% group_split(PORTNAME)

#Table of Performance

#Box Plot of Performance

#Table of ESG

#Box Plot of ESG

#Bar Weights

#Excess Return

#################################Optional II: Clustering #######################

#



#Portfolio mean return
pmr <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else mean(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}

pmrcum <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else sum(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}

#Portfolio Risk
prisk <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else sd(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}

#Portfolio Sharpe Ratio
sharpe <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else (mean(portreturn[i:(i-tau+1)])-0)/sd(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}

#VaR
pVaR <- function(portreturn,tau,conf){
  x <- sapply(seq_along(portreturn),function(i) if (i < tau) NA else quantile(portreturn[i:(i-tau+1)], conf))
  return(x[!is.na(x)])
}

#Max draw-down

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
  #Melt
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










#################################Optional III: Graph Structure #######################




sturcture.cov <- function(return_mat){
  #ret_gl is recieve from another function
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
  #op.cor <- cov2cor(op.cov)
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
    #gst <- unlist(st,use.names = F)
    st$Status[st$Status=='Invest'] <- "#31688EFF"
    st$Status[st$Status=='Divest'] <- '#FDE725FF'
    colors <- unlist(st,use.names = F)
    
  }else{
    colors <- NULL
    #gst <- NULL
  }
  q <- qgraph(adj.cov,esize=8,title = title,label.scale = T,label.prop = 0.9, shape= "circle", border.width = 3, label.cex = 0.8, posCol= "darkgreen", negCol="darkred", layout="groups", vsize=8, colors = colors,
              legend.cex = 0.8, # scalar of the legend
              legend.mode = 'style2',
              nodeNames = name.cov, 
              font = 2,
              curveAll = T, # logical indicating if all edges should be curved
              curveDefault = 0.5,
              title.cex = 1.7
              #groups = gst
  ) 
  return(q)
}



get.graph <- function(div_dynamic, return_mat, data.index, found = NULL){
  nameport <- unique(div_dynamic$PORTNAME)
  div_dynamic$PORTNAME <- factor(div_dynamic$PORTNAME,levels = unique(nameport))
  w_list <- div_dynamic %>% group_split(PORTNAME) %>% lapply(function(x) {x %>% dplyr::select(-PORTNAME) %>% multipywtoreturn(return_mat)})
  names(w_list) <- nameport
  #date <- return_mat$Date[return_mat$Date >= unique(div_dynamic$Date)[1]]
  # title.date <- names(w_list[[1]])[data.index]
  # name.plot <- c()
  # for(i in title.date){
  #   for(j in nameport){
  #     name.plot <- c(name.plot, paste0(j," at ",i))
  # }
  # }
  
  sel.port <- lapply(w_list, function(x) x[data.index]) %>% unlist(recursive=FALSE)
  list.cov <- sel.port %>% lapply(sturcture.cov)
  
  return({
    #windows(width = 500, height = 1000)
    par(mfrow=c(length(nameport),1))
    sapply(seq_along(list.cov), function(i) plot.qgraph(list.cov[[i]],names(sel.port)[i],found))
  })
}










# eudismat <- unlist(lapply(seq_along(mat1), function(i) sqrt(sum((mat1[[i]] - mat2[[i]])^2))))
# 
# 
# euplot <- data.frame(list(x = seq_along(eudismat), dist = eudismat))
# p <- ggplot(data = euplot, aes(x = x, y = dist, group = 1)) +
#   geom_line(size= 0.5, position = "identity") +
#   geom_point(size=1.5, position = "identity")



#' Divfolio
#'
#' Run Divfolilo
#' @examples 
#' 
#' divfolio.run();
#' 
#' 
#' @export
divfolio_run <- function(){
  
  if(!require("shinypop")){
    remotes::install_github("dreamRs/shinypop")
  }
  library("shinypop")
  

  
  #################################Container#######################
  
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
  # n <- 10
  # barplot(1:n, col=scales::viridis_pal()(n))
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  
  #######################################Start Shiny###################################
  options(shiny.sanitize.errors = FALSE)
  
  ui <- dashboardPage(
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
    dashboardHeader(title = shinyDashboardLogo(
      theme = "blue_gradient",
      boldText = "DivFolio",
      mainText = "App",
      badgeText = "v. Beta"
    )),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome", tabName = "welcome", icon = icon("fa-solid fa-door-open")),
        menuItem("User Guide", tabName = "userguide", icon = icon("folder-plus")),
        menuItem("Divestment Plan", tabName = "div", icon = icon("tree")),
        menuItem("Our Team", tabName = "about_us", icon = icon("user-plus"))
      )
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "blue_gradient"
      ),
      withMathJax(),
      tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script >
            ")),
      useShinyFeedback(),
      use_noty(),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden;}",
                 ".shiny-output-error:after {visibility: hidden;}"
      ),
      
      tabItems(
        tabItem(tabName = "welcome",
                h2('Divfolio: Green Finance Wealth Management Software'),
                tags$li(HTML('Sustainablising portfolio by divestment')),
                tags$li(HTML('Comparing performance of portfolios before and after divestment')),
                tags$li(HTML('Comparing performance of portfolios in general purpose')),
                br(),
                HTML("Changes in the contemporary climate include both global warming and its effects on the planet's weather patterns. Climate change has occurred in the past, but the current rate of change is significantly more fast and is not attributable to natural causes. It is instead caused by the release of greenhouse gases, primarily carbon dioxide (CO2) and methane. The majority of these emissions are caused by the burning of fossil fuels for energy production. At the current warming level of 1.2 C (2.2 F), several of these effects can already be noticed. Additional warming will amplify these effects and may set off tipping points, such as the Greenland ice sheet melting. Under the Paris Agreement of 2015, governments pledged to limit global warming 'well below 2 C'. Despite the commitments made under the Agreement, global warming would still reach roughly 2.7 degrees Celsius (4.9 degrees Fahrenheit) by the end of the century. To limit warming to 1.5 C, emissions must be cut in half by 2030 and reach net-zero levels by 2050. <br><br>
                   Divestment and exclusion are approaches that limit investment in carbon-intensive assets or industries, based on an organization or its stakeholders guiding principles. Thus, individuals who seek to avoid carbon-intensive assets frequently favor this policy. These stakeholders say that divestment and exclusion increase the cost of capital for carbon-intensive operations, provide a market signal to enterprises engaged in these activities to encourage a more rapid transition, and ensure that investors are comfortable with the sources of their returns. <br><br>
                   Divfolio offers a comparison of the risk profiles, ESG scores, and customized attributes of portfolios, such as carbon intensity, before and after divestment based on the simulation using historical data as well as advanced options such as assessing stability of portfolio via clustering and correlation structure. The tool is useful for investigating the impact of divestment on portfolio performance in multidimensional views.<br><br>"),
                HTML('<script src="https://climateclock.world/widget-v2.js" async></script><climate-clock />'),
                
        ),
        
        #HTML('<embed src="https://www.esgtoday.com/category/esg-news/" style="width:500px; height: 500px;">')),
        
        tabItem(tabName = "userguide",
                tabBox(width = 12,
                       tabPanel(title = "Introduction",
                                tags$div(
                                  tags$ul(
                                    HTML("As awareness of the global warming problem, many countries, institutions, and agents are willing to move toward a more sustainable economy. Similarly to the financial and investment sector, sustainability trends are becoming more necessary. In the asset management industry, a debate has arisen over how to most effectively incentivize corporations and encourage private sector initiatives that will actively induce decision making to reduce environmental impact and drive change in positive mitigation strategies for reducing carbon emissions, the primary cause of climate change. These movements and activities are sometimes referred to as divestment strategies from fossil fuels. Investors and asset managers are urged to liquidate their holdings in companies classified as non-compliant or with poor performance on the environmental component of environmental social governance or ESG ratings, which reflect compliance with current best practices addressing carbon emissions. Not only does divestment deprive fossil fuel businesses of funding to promote change, but it is also a prudent business move for ESG reporting with long-term viability in the spotlight. If the cost of capital increases or refinancing becomes prohibitively expensive or simply unavailable, the valuations of carbon-intensive enterprises will be impacted, and they may become unviable. <br><br> 
                                       <b>Divfolio</b> is a software developed in R shiny environment that offers portfolio analytic related to divestment. The tool offers a comparison of the risk profiles, ESG scores, and customized attributes of portfolios before and after divestment based on the simulation using historical data as well as advanced options such as assessing stability of portfolio via clustering and correlation structure. The tool is useful for investigating the impact of divestment on portfolio performance in multidimensional views. The idea of the divestment methodology is given in our paper, <b>Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns</b>,  available on SSRN. The tool also benefits the general purpose of portfolio performance comparison, especially in ESG investing and sustainable investing. Users can customize the attributes of comparable portfolios by uploading prepared CSV files instead of generating portfolios on the tool."),
                                    a("See our paper here", href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4131449", target="_blank"),
                                    img(src = 'paperssrn.png',height="70%", width="70%")
                                  )
                                )
                       ),
                       tabPanel(title = "Workflow",
                                box(
                                  title = h4("Workflow"),   
                                  HTML("DivFolio App tool comprises two core components as will be detailed in the panels below. The first component involves a set of sequential STEPs that users undertake consisting of STEP 1 through to STEP 5, which must be performed sequentially. The second component involves a collection of three independent optional STEPs that can be performed individually. For the optional steps, so long as the user has the relevant data prepared in CSV format already, for instance as output from STEPS 1 to 5 in Component 1, then they can upload this directly into any of the STEPS in Component 2 instead of generating them again from the tool. The uploaded files are required to be in a specific format, described in the section <b>Types of Uploading Files, at the end of this page</b>. Uploading the files is recommended as it will speed up the process significantly and avoid blocking due to too frequent calling data via API.<br>"),
                                  img(src = 'daig.jpg',height="75%", width="75%", align = 'center'),
                                  tags$br(""),
                                  tags$li(HTML("<b>Step 1: Asset Performance Investigation</b> <br>
                                                This STEP is used for developing a list of potential portfolio assets. The user can view ESG data, accounting ratios, candlestick charts, and a summary of asset risk profiles in multiple periods. The tool allows users to investigate the performance of assets and add them to portfolio in STEP 2 that will display them. If a user already possesses a list of possible assets, Users can skip this step and go to STEP 2.")),
                                  tags$li(HTML("<b>Step 2: Selecting Divestable and Investable Assets</b> <br>
                                                This STEP is used for assigning the investment status to the selected assets, either 'invest' or 'divest'. Users can download the companies and ESG information from the tickers in STEP 1. The tool allows two modes of divestment asset selection: manually where the users selects assets to divest based on their expert opinion or automatically where a quantile ranking based method is used to select divestment assets by the quantile of their current ESG score. The comparison of the percentage of change in ESG scores or/and other attributes is given.")),
                                  tags$li(HTML("<b>Step 3: Batching Historical Data</b> <br>
                                                 This step is used for generating the time series of the daily price of assets. The tool will import the tickers in STEP 2 to receive the daily price data via API to calculate returns. The table of the return of each asset is given in this part as well as the distribution of the risk profiles in the comparable boxplots.")),
                                  tags$li(HTML("<b> Step 4: Constructing Investment Portfolio</b> <br>
                                                This step is used for generating the portfolio weights from the returns obtained in STEP 3 as well as to illustrate the portfolio performance. There are five options of portfolio weight available. The tool will calculate the weighted ESG scores or attributes and risk profiles of the portfolio over time.  Those results are displayed by the tables, the plot of time series, and distribution boxplots.")),
                                  tags$li(HTML("<b> Step 5: Divestment</b> <br>
                                                This STEP is used for generating the divestment schedule - sequence of limit of the investment weights for divestable assets at a given time. The tool will import the return and the portfolio weight  from STEP 3 and STEP 4 to generate investment weight of divested portfolio that the assets in divesting list from the portfolio according to the divestment schedule. The divestment strategy is to allocate capital from the divestable assets to the investable assets proportionally to their original weights. The full details of the divestment strategy can be found in the paper; :
Marupanthorn, Pasin and Sklibosios Nikitopoulos, Christina and Ofosu-Hene, Eric and Peters, Gareth and Richards, Kylie-Anne and Richards, Kylie-Anne, Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns (June 8, 2022). Available at SSRN: https://ssrn.com/abstract=4131449 or http://dx.doi.org/10.2139/ssrn.4131449. The comparison of performances to the non-divest portfolio will be given in the STEP 5.")),
                                  tags$li(HTML("<b> Optional I: Multiple Portfolios Comparison </b> <br>
                                                 This optional STEP is used for analysis and comparing the performance of many portfolios. The results in this STEP are similar to STEP 5 but in comparison to more than two portfolios. Generally, it can be used for comparing any portfolio, not limited to divestment.")),
                                  tags$li(HTML("<b> Optional II: Stability Analysis via Clustering </b> <br>
                                                 This optional STEP is used for investigating the stability of portfolio performance. We employ the clustering algorithm, namely Clustering Large Applications or CLARA, an approximate extension of k-medoids methods to achieve stability investigation. Portfolios that have similar relative behavior will belong to the same cluster over time. The features used in clustering were: return, cumulative return, volatility, sharpe ratio, value at risk, max drawdown, and sortino.")),
                                  tags$li(HTML("<b> Optional III: Graph Structure Correlation Analysis </b> <br>
                                                 This optional STEP is used for investigating the correlation structure of the assets in portfolios using graph LASSO. This function is suitable for studying the dynamic correlation structure of portfolios and comparison of their diversification as portfolio dynamically evolves, for instance as divestment is progressively performed on different sets of assets.")
                                  ),
                                  id = "mybox",
                                  collapsible = TRUE,
                                  closable = TRUE,
                                  width = 13
                                ),
                                
                                box(
                                  title = h4("Types of Uploading Files"),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F0</b>; List of tickers and, optional, their investment statuses. The file requires; <br>
                                               i) a compulsory column of the name 'Ticker', consisting of ticker of all potential assets, and <br>
                                               ii) an optional column of the name 'Status', containing the investment status either 'Invest' or 'Divest'.")),
                                  img(src = 'file0.png'),
                                  #tags$br(""),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F1</b>; List of tickers, their investment statuses, and some numerical attributes. The file requires; <br>
                                              i) a compulsory column of the name 'Ticker', consisting of ticker of all potential assets, and <br>
                                              ii) a compulsory column of the name 'Status', containing the investment status either 'Invest' or 'Divest', and <br>
                                              iii) at least one columns of any name that contain numerical attributes of the asset.")),
                                  img(src = 'file1.png'),
                                  #tags$br(""),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F2</b>; The table of assets return. The file requires; <br>
                                              i) a compulsory column of the name 'Date', consisting of dates in the form 'year-month-date', and <br>
                                              ii) other columns that are named by the tickers, containing the return of each asset over time.")), 
                                  img(src = 'file2.png'),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F3</b>; The table of portfolio weight. The file requires; <br>
                                              i) a compulsory column of the name 'Date', containing dates of portfolio rebalancing in the form 'year-month-date', and <br>
                                              ii) other columns that are named by the tickers, containing the investment weights of each asset over time.")),
                                  img(src = 'file3.png'),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F4</b>; The table of divestment schedule. The file requires; <br>
                                              i) a compulsory column of the name 'Date', containing dates of portfolio rebalancing in the form 'year-month-date', and <br>
                                              ii) a compulsory column of the name 'Bound', containing the sequence of limit of the investment weight of the divestable assets.")),
                                  img(src = 'file4.png'),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F5</b>; The table of portfolio weight with multiple portfolios. The file requires; <br>
                                              i) a compulsory column of the name 'Date', containing dates of portfolio rebalancing in the form 'year-month-date', and <br>
                                              ii) a compulsory column of the name 'PORTNAME', containing any name of portfolio as the label for separating data by portfolio type, and
                                              iii) other columns that are named by the tickers, containing the investment weights of each asset over time.")),
                                  img(src = 'file5.png'),
                                  hr(),
                                  tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F6</b>; List of tickers and their investment statuses. The file requires; <br>
                                               i) a compulsory column of the name 'Ticker', consisting of ticker of all potential assets, and <br>
                                               ii) compulsory column of the name 'Status', containing the investment status either 'Invest' or 'Divest'.")),
                                  img(src = 'file6.png'),
                                  hr(),
                                  id = "mybox",
                                  collapsible = TRUE,
                                  closable = TRUE,
                                  width = 13
                                )
                       ),
                       
                       tabPanel(title = "Packages and Dependency",
                                img(src = 'dependency.png',align = 'center'),
                                "The application was built under R version 4.1.2",
                                includeHTML('example.html')
                       )
                )
        ),
        
        tabItem(tabName = "div",
                tabBox(width = 12,
                       tabPanel(title = "Step 1",
                                fixedRow(
                                  column(1,
                                         dropdown(textInput("ticker", tags$span("Company's Ticker", bsButton("comtick", label = "", icon = icon("info"), style = "info", size = "extra-small")), "AAP"),
                                                  bsPopover(
                                                    id = "comtick",
                                                    title = "More information",
                                                    content = paste0(
                                                      "The ticker is required in ",
                                                      a("Yahoo Finance format", href = "https://finance.yahoo.com/", target="_blank")
                                                    ),
                                                    placement = "right",
                                                    trigger = "focus",
                                                    options = list(container = "body")),
                                                  
                                                  style = "unite", icon = icon("gear"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ),
                                                  actionBttn(
                                                    inputId = "submit",
                                                    label = "submit",
                                                    style = "pill", 
                                                    color = "success"
                                                  )),
                                         dropdown(textInput("ticker_sel", tags$span("Add assets to portfolio", bsButton("s1", label = "", icon = icon("info"), style = "info", size = "extra-small")), ticker_rec),
                                                  bsPopover(
                                                    id = "s1",
                                                    title = "More information",
                                                    content = "Please add one asset per time. Assets will be added to portfolio in Step 2",
                                                    placement = "right",
                                                    trigger = "focus",
                                                    options = list(container = "body")),
                                                  style = "unite", icon = icon("cart-plus"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ),
                                                  HTML("<b><h5>Assets in Portfolio</h5></b>"),
                                                  verbatimTextOutput("verb"),
                                                  actionBttn(
                                                    inputId = "add",
                                                    label = "add",
                                                    style = "pill", 
                                                    color = "success"
                                                  ))
                                  ),
                                  column(11,
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         h3("Step 1: Asset Performance Investigation"),
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         box(
                                           title = h4("User Guide"),
                                           "This phase is optional for users who wish to investigate performance of assets and add them to portfolio. 
                          The portfolio will be shown in STEP 2. If you already have list of potential assets, please go to STEP 2.",
                                           tags$br(""),
                                           "How to use this page?",
                                           tags$br(""),
                                           tags$li(HTML("Click <b>Gear icon</b> <i class='fa-solid fa-gear' style = 'color:#0072B2;'></i>, input a ticker, and click submit to get company's data")),
                                           tags$li(HTML("Click <b>Cart icon</b> <i class='fa-solid fa-cart-plus' style = 'color:#0072B2;'></i>, input a ticker, and click add to add asset to portfolio")),
                                           hr(),
                                           "The remarks for this STEP is listed below",
                                           tags$br(""),
                                           tags$li("The ticker is required in Yahoo Finance format. For example, the ticker of companies in the LSE does have .L after, in Tokyo have .T after, and in NYSE does not have the abbreviation after."),
                                           tags$li("The sustainability score ratings measure unmanaged risk on a 0-100 scale, with lower scores indicating less unmanaged ESG Risk."),
                                           tags$li("All charts are interactive, gernerated by 'plotly' package. Users can zoom, export and hide some plots."),
                                           tags$li("The table at the bottom of the page displays the average performances over the past week, month, three months, year, and entire time period."),
                                           tags$br(""),
                                           "* User Guide box is collapsible",
                                           id = "mybox",
                                           collapsible = TRUE,
                                           closable = TRUE,
                                           width = 13
                                         ),
                                         h3(textOutput('comp_name')),
                                         h6(textOutput('comp_market')),
                                         h6(textOutput('comp_sec')),
                                         h6(textOutput('comp_sum')),
                                         hr(),
                                         h5("Sustainability Score"),
                                         hr())),
                                
                                fixedRow(  
                                  column(1),
                                  column(11,
                                         fluidRow(
                                           withLoader(valueBoxOutput("lev_esg", width = 3), loader = 'loader4'),
                                           withLoader(valueBoxOutput("total_esg", width = 3), loader = 'loader4'),
                                           withLoader(valueBoxOutput("e_score", width = 2), loader = 'loader4'),
                                           withLoader(valueBoxOutput("s_score", width = 2), loader = 'loader4'),
                                           withLoader(valueBoxOutput("g_score", width = 2), loader = 'loader4'))
                                  ),
                                  column(12,   
                                         fixedRow(  
                                           column(1),
                                           column(11,
                                                  hr(),
                                                  h5("Historical Data"),
                                                  hr(),
                                                  plotlyOutput("candel")
                                           ))),
                                  column(12,   
                                         fixedRow(  
                                           column(1),
                                           column(11,
                                                  plotlyOutput("perf_fig")
                                           ))),
                                  column(12,
                                         fixedRow( 
                                           column(1),
                                           column(11,
                                                  #div(dataTableOutput("perf_table"), style = "font-size:80%")
                                                  dataTableOutput("perf_table")
                                           )))
                                )),
                       
                       tabPanel(title = "Step 2",
                                fixedRow(
                                  column(1,
                                         dropdown(fileInput("fileticker", "F0: CSV File of Ticker", 
                                                            accept = c(".csv")),
                                                  style = "unite", icon = icon("gear"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  )),
                                         dropdown(fileInput("file1", "F1: CSV File of Investment Status", 
                                                            accept = c(".csv")),
                                                  style = "unite", icon = icon("check"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ))),
                                  column(11,
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         h3("Step 2: Selecting Divestable and Investable Assets"),
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         box(
                                           title = h4("User Guide"),
                                           "This STEP is to determine which assets should be divested or invested. 
                           Users can also download the ESG data of assets in portfolio.",
                                           tags$br(""),
                                           "How to use this page?",
                                           tags$br(""),
                                           HTML("<b> 1. Already have data in CSV format </b>"),
                                           tags$li(HTML("Click <b>Gear icon</b> <i class='fa-solid fa-gear' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Ticker</b> with one column of the name 'Ticker', containing company's tickers. Another column of the name 'Status' with either 'Invest' or 'Divest' is an optional. This option helps users not to add one by one ticker in STEP 1. If you have <b>CSV File of Investment Status</b>, you do not need to upload this file.")),
                                           img(src = 'file0.png'),
                                           tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, upload <b>CSV File of Investment Status</b> with <br> i) a column of the name 'Ticker', containing company's tickers, and
                                        <br> ii) a column of the name 'Status', containing statuses; 'Divest' or 'Invest', and <br>
                                        iii) at least one numeric columns of any name <br>")),
                                           img(src = 'file1.png'),
                                           
                                           tags$br(""),
                                           HTML("<b>or 2. Download ESG data from a network </b>"),
                                           tags$li(HTML("Add potential assets in STEP 1 if you don,t upload <b>CSV File of Ticker</b>. The selected assets will appear in the part <b>Potential Assets</b>.")),
                                           tags$li(HTML("Drag and drop assets from the part <b>Potential Assets</b> in the part <b>Investable Assets</b> or <b>Divestable Assets</b>. The assets in the part <b>Potential Assets</b> will not be included in the portfolio.")),
                                           tags$li(HTML("Click <b>summary</b> button to get the data")),
                                           tags$br(""),
                                           hr(),
                                           "The remarks for this STEP is listed below",
                                           tags$br(""),
                                           tags$li(HTML("Assets in the part <b>Potential Assets</b> will not be included in the portfolio.")),
                                           tags$li(HTML("Assets in parts <b>Potential Assets</b>, <b>Investable Assets</b>, and <b>Divestable Assets</b> are exchangeable by draging and dropping.")),
                                           tags$li(HTML("Assign divestment status to asset by dropping in either <b>Investable Assets</b> or <b>Divestable Assets</b>.")),
                                           tags$li(HTML("Click <b>update summary</b> botton when you finish updating divestment status of the assets.")),
                                           tags$li(HTML("Select the option <b>Manage Divestment</b> if you want to assign divestment status by ranking the assets according to selected factor. The criteria is upper or lower quantile.")),
                                           tags$li(HTML("<b>Table of Attributes</b> shows the uploaded <b>CSV File of Investment Status</b> or the table of ESG scores where the data is downloaded from the network.")),
                                           tags$li(HTML("<b>Comparison of Attributes Before-After Divestment in Percentage</b> displays the score of attributes before and after divestment as a percentage of change under the assumption that all assets are equally valuable. The formula is given by <br> 
                                   $$R = \\frac{A_{after} - A_{before}}{A_{before}} \\times 100\\%,$$ <br> 
                                        where $A_{before}$ and $A_{after}$ is the attribute of the asset before and after divestment, respectively.")),
                                           tags$br(""),
                                           "* User Guide box is collapsible",
                                           id = "mybox",
                                           collapsible = TRUE,
                                           closable = TRUE,
                                           width = 13
                                         ),
                                         orderInput('inter', 'Potential Assets', items = NULL, item_class = 'info',placeholder = 'Add Ticker from Assets Section Panel', connect = c('source','divt'), width = "1000px"),
                                         hr(),
                                         orderInput('source', 'Investable Assets', items = NULL, item_class = 'info',placeholder = 'Drag Ticker here', connect = c('divt','inter'), width = "1000px"),
                                         hr(),
                                         orderInput('divt', 'Divestable Assets', items = NULL, item_class = 'danger',placeholder = 'Drag Ticker here', connect = c('source','inter'), width = "1000px"),
                                         hr(),
                                         actionBttn(
                                           inputId = "subesg",
                                           label = "summary",
                                           style = "pill", 
                                           color = "success",
                                           size = "xs"
                                         ),
                                         actionBttn(
                                           inputId = "subup",
                                           label = "update summary",
                                           style = "pill", 
                                           color = "success",
                                           size = "xs"
                                         ),
                                         hr(),
                                         awesomeCheckbox(
                                           inputId = "autosel",
                                           label = "Manage Divestment", 
                                           value = FALSE,
                                           status = "info"
                                         ),
                                         conditionalPanel(condition ="input.autosel == 1",
                                                          column(4,
                                                                 pickerInput(
                                                                   inputId = "selq",
                                                                   label = "Select Investment Criteria", 
                                                                   choices = NULL
                                                                 )),
                                                          column(4,
                                                                 textInput("perc", "Percentile (0 - 1)", 0.5)),
                                                          column(4,
                                                                 pickerInput(
                                                                   inputId = "seldec",
                                                                   label = "Top or Bottum", 
                                                                   choices = list('More than' = 1, 'Less than' = 0)
                                                                 )),
                                                          actionBttn(
                                                            inputId = "subman",
                                                            label = "update summary by quantile",
                                                            style = "pill", 
                                                            color = "success",
                                                            size = "xs"
                                                          )
                                         ),
                                         h4("Table of Attributes"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("ui_esg_table", width = "auto", height = "auto")),
                                         h4("Comparison of Attributes Before-After Divestment in Percentage"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("ui_esg_table_com", width = "auto", height = "auto"))
                                  ))
                                
                       ),
                       
                       tabPanel(title = "Step 3",
                                fixedRow(
                                  column(1,
                                         dropdown(dateInput("stdate", "From:", value = ymd(as.Date(Sys.time())) %m-% months(1)),
                                                  dateInput("enddate", "To:", value = as.Date(Sys.time())),
                                                  style = "unite", icon = icon("calendar"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ),
                                                  actionBttn(
                                                    inputId = "subtime",
                                                    label = "get historical data",
                                                    style = "pill", 
                                                    color = "success"
                                                  )),
                                         dropdown(fileInput("file2", "F2: CSV File of Historical Data", 
                                                            accept = c(".csv")),
                                                  style = "unite", icon = icon("check"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ))),
                                  column(11,
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         h3("Step 3: Batching Historical Data"),
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         
                                         box(
                                           title = h4("User Guide"),
                                           "This STEP is to download the return of assets in a portfolio. Note that the process in STEP 2 needs to be completed before running this STEP.",
                                           tags$br(""),
                                           "How to use this page?",
                                           tags$br(""),
                                           HTML("<b> 1. Already have data in CSV format </b>"),
                                           tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Historical</b> Data with one column of the name 'Date', containing date in form of 'year-month-day'. Other columns are named by the tickers and the values in cells are the asset returns.")),
                                           img(src = 'file2.png'),
                                           tags$br(""),
                                           HTML("<b>or 2. Download return data from a network </b>"),
                                           tags$li(HTML("Click <b>Calendar icon</b> <i class='fa-solid fa-calendar-days' style = 'color:#0072B2;'></i>, set time period to obtain the returns, and click <b>get historical data</b> button.")),
                                           hr(),
                                           "The remarks for this STEP is listed below",
                                           tags$br(""),
                                           tags$li(HTML("<b>Table of Historical Relative Return</b> shows the return uploaded from <b>CSV File of Historical Data</b> or the relative returns where the data is downloaded from the network. the ralative return is calculated by the adjust closing price by
                                    $$Re = \\frac{P_{t+1} - P_{t}}{P_{t}}$$ where $P_{t+1}$ and $P_{t}$ are the adjusted closing prices at time $t+1$ and $t$, respectively.")),
                                           tags$li(HTML("<b>Table of Assets Performance</b> displays the average of asset performances over the selected period.")),
                                           tags$li(HTML("<b>Distribution of Asset Returns</b> displays the distribution of the returns of the assets over the selected period. Users can select the picker option 
                                    <b>Arranging Plot by</b> to sort the assets according to the interesting performance.")),
                                           tags$br(""),
                                           "* User Guide box is collapsible",
                                           id = "mybox",
                                           collapsible = TRUE,
                                           closable = TRUE,
                                           width = 13
                                         ),
                                         
                                         h4("Table of Historical Relative Return"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("tab_return", width = "auto", height = "auto")),
                                         h4("Table of Assets Performance"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("reportreturn", width = "auto", height = "auto")),
                                         h4("Distribution of Asset Returns"),
                                         hr(),
                                         pickerInput(
                                           inputId = "rankby",
                                           label = "Arranging Plot by", 
                                           choices = NULL
                                         ),
                                         withLoader(plotlyOutput("boxasset"))
                                  )
                                )
                       ),
                       tabPanel(title = "Step 4",
                                fixedRow(
                                  column(1,
                                         dropdown(pickerInput(
                                           inputId = "type",
                                           label = "Select Portfolio Type", 
                                           choices = list("Passive Equal Weight" = "0", "Active Equal Weight" = "1",
                                                          "Global Minimum Variance" = "2", "Maximum Sharpe Ration" = "3",
                                                          "Principal Portfolio" = "4")),
                                           HTML('<h5><b>Limit Short-Selling Weight</b></h5>'),                            
                                           switchInput(
                                             inputId = "limselect"
                                           ),
                                           conditionalPanel(
                                             condition = "(input.limselect == 1)&(input.type == 2||input.type == 3||input.type == 4)",
                                             textInput("shortlim", "Short-Selling Limit", 0.3)
                                           ),
                                           textInput("reb", "Rebalancing Frequency [in day]", 20),
                                           textInput("tau", "Number of Days for Calculating Weights", 20),
                                           actionBttn(
                                             inputId = "subweight",
                                             label = "submit",
                                             style = "pill", 
                                             color = "success"
                                           ),
                                           style = "unite", icon = icon("chart-bar"),
                                           status = "primary", width = "300px",
                                           animate = animateOptions(
                                             enter = animations$fading_entrances$fadeInLeftBig,
                                             exit = animations$fading_exits$fadeOutRightBig
                                           )),
                                         dropdown(fileInput("file3", "F3: CSV File of Portfolio's Weight", 
                                                            accept = c(".csv")),
                                                  style = "unite", icon = icon("check"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ))
                                  ),
                                  column(11,
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         h3("Step 4: Constructing Investment Portfolio"),
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         
                                         box(
                                           title = h4("User Guide"),
                                           "This STEP is to construct an investment portfolio from the selected assets. The analysis of the performances of the portfolio will be given in the STEP. Note that the process in STEP 2 and STEP 3 need to be completed before running this STEP.",
                                           tags$br(""),
                                           "How to use this page?",
                                           tags$br(""),
                                           HTML("<b> 1. Already have data in CSV format </b>"),
                                           tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Portfolio's Weight</b> with one column of the name 'Date', containing rebalancing dates in form of 'year-month-day'. Other columns are named by the tickers and the values in cells are the asset weights which sum to one over row.")),
                                           img(src = 'file3.png'),
                                           tags$br(""),
                                           HTML("<b>or 2. Calculate portfolio weight using the tool</b>"),
                                           tags$li(HTML("Click <b>Chart icon</b> <i class='fa-solid fa-chart-bar' style = 'color:#0072B2;'></i>, select type of portfolio in <b>Select Portfolio Type</b>. There are five options for users; <br> 
                                      <p style='color:blue'>1. Passive Equal Weight</p> means giving each asset the same weight by $1/N$, where $N$ is the number of assets. This portfolio is long-only and the portfolio weight will not change over time. <br> 
                                      <p style='color:blue'>2. Active Equal Weight</p> means giving each asset proportional to its return. High-return assets gain more investment weight. This portfolio is long-only. <br> 
                                      <p style='color:blue'>3. Global Minimum Variance</p> means assigning investment weights to achieve the most stable return by the minimising variance of the portfolio's returns. This portfolio allows both long and short positions. <br> 
                                      <p style='color:blue'>4. Maximum Sharpe Ratio</p> means assigning investment weights to achieve the highest return per risk or Sharpe ratio. This portfolio allows both long and short positions. <br> 
                                      <p style='color:blue'>5. Principle Portfolio</p> means assigning investment weights based on the principal components analysis. This portfolio is risk-seeking as the investment weight will be gained when the asset is more risky. It allows both long and short positions.<br>
                                      Note here that the calculating process may be expensive for a large portfolio for the last three portfolios <br> <br>
                                      The details in deep of all portfolios can be found in our paper, <b>Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns</b> available on SSRN. <br> <br>")),
                                           tags$li(HTML("<b>Limit Short-Selling Weight</b> is the option to control leverage risk by setting the upper bound of the short-selling ratio. This option is available for portfolios that allow short selling. Switch on and set the limit to using this option.")),
                                           tags$li(HTML("<b>Rebalancing Frequency [in a day]</b> is the number of days before rebalancing portfolio. The number is no more than a number of days in the studying period.")),
                                           tags$li(HTML("<b>Number of Days for Calculating Weights</b> is days in the past used for calculating a covariance matrix. It is required for the last three portfolios as they are covariance-based. It is usually set to be equal to the rebalancing frequency.")),
                                           tags$li(HTML("Click <b>submit</b> button after filling all parameters to generate portofolio.")),
                                           hr(),
                                           "The remarks for this STEP is listed below",
                                           tags$br(""),
                                           tags$li(HTML("<b>Table of Rebalancing Portfolio Weight</b> shows the portfolio weights uploaded from <b>CSV File of Portfolio's Weight</b> or portfolio weights each rebalancing where the portfolio weights are generated from the tool.")),
                                           tags$li(HTML("<b>Time Series of Asset Weight</b> displays the plot of the individual asset over time. Users can observe the dynamic behavior of each asset across time.")),
                                           tags$li(HTML("<b>Assets Allocation</b> displays the charts of portfolio weights at given time, <b>Select Date</b>. Users can observe the attribute of each asset by using the picker <b>Select Factor</b>.")),
                                           tags$li(HTML("<b>Attribute Allocation in Portfolio</b> displays the plot of the attribute weighted by portfolio weights, $\\sum_{i = 1}^N w_iA_i$, where $N$ is the portfolio's asset count, $w_i$ is the asset's investment weight, and $A_i$ is the asset's attribute. It can be seen as the expected attribute. We calculate them for the short and long positions separately. Users can observe the behavior of the portfolio's expected attributable over time in this chart.")),
                                           tags$li(HTML("<b>Distribution of Attributes for Long Position</b> and <b>Distribution of Attributes for Short Position</b> show the boxplots of distribution of attributes separated by holding positions. The white dot represents the average value of the distribution." )),
                                           tags$li(HTML("<b>Table of Time Series of Attributes</b> displays the expected attribute of the portfolio over time separated by holding positions")),
                                           tags$li(HTML("<b>Distribution of Portfolio Risk Profiles</b> displays the boxplots of distribution of portfolio's performances over the studying period. The white dot represents the average value of the distribution")),
                                           tags$li(HTML("<b>Efficiency Frontier</b> displays the plot of portfolio standard deviation versus return. The picker has multiple options that allow user to observe many curves in a plot.")),
                                           tags$li(HTML("<b>Table of Risk Profiles</b> displays the table of risk profiles calculated backward before each rebalancing versus time.")),
                                           tags$li(HTML("<b>Time Series of Risk Profiles</b> displays the plot of risk profiles calculated backward before each rebalancing versus time.")),
                                           tags$br(""),
                                           "* User Guide box is collapsible",
                                           id = "mybox",
                                           collapsible = TRUE,
                                           closable = TRUE,
                                           width = 13
                                         ),
                                         
                                         h4("Table of Rebalancing Portfolio Weight"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("weight", width = "auto", height = "auto")),
                                         fixedRow(
                                           column(6,
                                                  h4("Time Series of Asset Weight"),
                                                  hr(),
                                                  pickerInput(
                                                    inputId = "selcom",
                                                    label = "Select Company", 
                                                    choices = colnames(file3_Weight)[-1]
                                                  ),
                                                  withLoader(plotlyOutput("plotw3", width = "auto", height = "auto"))),
                                           column(6,
                                                  h4("Assets Allocation"),
                                                  hr(),
                                                  fixedRow(
                                                    column(6,
                                                           pickerInput(
                                                             inputId = "selcat",
                                                             label = "Select Factor", 
                                                             choices = colnames(file1_Status)
                                                           )),
                                                    column(6,
                                                           pickerInput(
                                                             inputId = "seldate",
                                                             label = "Select Date", 
                                                             choices = unlist(file3_Weight$Date, use.names = F)
                                                           ))),
                                                  withLoader(plotlyOutput("plotw3port", width = "auto", height = "auto"))
                                           )),
                                         h4("Allocation of Attributes in Portfolio"),
                                         withLoader(plotlyOutput("plotw3F", width = "auto", height = "auto")),
                                         fixedRow(
                                           column(6,
                                                  h4("Distribution of Attributes for Long Position"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotw3Flong"))
                                           ),
                                           column(6,
                                                  h4("Distribution of Attributes for Short Position"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotw3Fshort")))
                                         ),
                                         h4("Table of Time Sereis of Attributes"),
                                         hr(),
                                         withLoader(DTOutput("plotw3Ftab", width = "auto", height = "auto")),
                                         fixedRow(
                                           column(6,
                                                  h4("Distribution of Portfolio Risk Profiles"),
                                                  hr(),
                                                  withLoader(plotlyOutput("radar"))
                                           ),
                                           column(6,
                                                  h4("Efficiency Frontier"),
                                                  hr(),
                                                  actionBttn(
                                                    inputId = "subeff",
                                                    label = "submit",
                                                    style = "pill", 
                                                    color = "success",
                                                    size = "xs"
                                                  ),
                                                  pickerInput(
                                                    inputId = "seldateeff",
                                                    label = "Select Dates", 
                                                    choices = file3_Weight[,1],
                                                    multiple = TRUE,
                                                    options = list(
                                                      `selected-text-format` = "count > 3")
                                                  ),
                                                  withLoader(plotlyOutput("eff", width = "auto", height = "auto")))
                                         ),
                                         h4("Table of Risk Profiles"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("plotw3risk", width = "auto", height = "auto")),
                                         h4("Time Series of Risk Profiles"),
                                         hr(),
                                         withLoader(plotlyOutput("plotw3riskplot", width = "auto", height = "auto"))
                                  )
                                )),
                       
                       tabPanel(title = "Step 5",
                                fixedRow(
                                  column(1,
                                         dropdown(
                                           pickerInput(
                                             inputId = "rate",
                                             label = "Select Rate of Divestment", 
                                             choices = list("Instant" = 0, "Linear" = 1,
                                                            "Hyperbolic" = 2)),
                                           conditionalPanel(
                                             condition = "(input.rate == 1)||(input.rate == 2)",
                                             pickerInput(
                                               inputId = "lastdate",
                                               label = "Select End Date of Divestment", 
                                               choices = NULL)
                                           ),
                                           
                                           conditionalPanel(
                                             condition = "input.rate == 1",
                                             withMathJax("$$D(t) = m\\times (t - t_{end})$$"),
                                             textInput("m", "Slope (m)", -0.1)
                                           ),
                                           conditionalPanel(
                                             condition = "input.rate == 2",
                                             withMathJax("$$D(t) = 1/t^a$$"),
                                             textInput("a", "Exponent (a)", 0.5)
                                           ),
                                           actionBttn(
                                             inputId = "subdiv",
                                             label = "submit",
                                             style = "pill", 
                                             color = "success"
                                           ),
                                           
                                           style = "unite", icon = icon("minus"),
                                           status = "primary", width = "300px",
                                           animate = animateOptions(
                                             enter = animations$fading_entrances$fadeInLeftBig,
                                             exit = animations$fading_exits$fadeOutRightBig)
                                         ),
                                         dropdown(fileInput("file4", "F4: CSV File of Divestment Schedule", 
                                                            accept = c(".csv")),
                                                  style = "unite", icon = icon("check"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ))
                                  ),
                                  column(11,
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         h3("Step 5: Divestment"),
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         
                                         box(
                                           title = h4("User Guide"),
                                           HTML("This STEP is to divest the assets in divesting list from the portfolio according to the divestment schedule - limit of the investment weights for divestable assets at a given time. The comparison of performances to the non-divest portfolio will be given in the STEP The divestment strategy is to allocate capital from the divestable assets to the investable assets proportionally to their original weights. The full details of the divestment strategy can be found in the paper, <b>Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns</b> available on SSRN. Note that the process in STEP 2, STEP 3, and STEP 4 need to be completed before running this STEP."),
                                           tags$br(""),
                                           "How to use this page?",
                                           tags$br(""),
                                           HTML("<b> 1. Already have data in CSV format </b>"),
                                           tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b> CSV File of Divestment Schedule</b> with two columns; the first column named 'Date', containing rebalancing dates in the form of 'year-month-day'. Another column of the name 'Bound', is a limit on the total investment weight of the divestable assets")),
                                           img(src = 'file4.png'),
                                           tags$br(""),
                                           HTML("<b>or 2. Calculate the divestment schedule using the tool</b>"),
                                           tags$li(HTML("Click <b>Minus icon</b> <i class='fa-solid fa-minus' style = 'color:#0072B2;'></i>, select type of portfolio in <b>Select Portfolio Type</b>. There are three options in <b>Select Rate of Divestment</b> for users; <br> 
                                      <p style='color:blue'>1. Instant</p> is to withdraw all divestable assets at the beginning. The governing equation is given by 
                                      $$D(t) = 0,$$ for all $t$, where $D(t)$ is the divestment schedule at time $t$.<br> 
                                      <p style='color:blue'>2. Linear</p> is to gradually withdraw divestable assets by the equally capital, $m$, each time step. The governing equation is given by 
                                      $$D(t) = m \\times (t -t_{end}),$$ where $t_{end}$ is the index of last date of divestment. <br> 
                                      <p style='color:blue'>3. Hyperbolic</p> is to fast withdraw divestable assets in the early time step based on a hyperbolic decreasing function with exponent $a$. The governing equation is given by $$D(t) = 1/t^a$$, where $a$ is the non-negative exponent. <br>")),
                                           tags$br(""),
                                           tags$li(HTML("<b>Select End Date of Divestment</b> is the option to set the last date of divestment. After this date, the sum of weights of all divestable assets is set to be zero.")),
                                           tags$li(HTML("Click <b>submit</b> button after filling all parameters to obtain the performance of the divested portfolio.")),
                                           hr(),
                                           "The remarks for this STEP is listed below",
                                           tags$br(""),
                                           tags$li(HTML("<b>Table of Divestment Schedule</b> shows the divestment schedule uploaded from <b>CSV File of Divestment Schedule</b> or the divestment schedule generated by the tool.")),
                                           tags$li(HTML("<b>Portfolio Weight After Divestment</b> shows the weights of the divested portfolio over time according to the divestment schedule.")),
                                           tags$li(HTML("<b>Sum of Weights of Divestable Assets</b> shows the chart of the sum of the weights of divestable assets over time. The weights will decrease over time, and the capital will be allocated to the investable assets.")),
                                           tags$li(HTML("<b>Sum of Allocated Weights and Investable Assets Weights</b> shows the chart of the sum of the weights allocated from the divestable and investable asset weights. ")),
                                           tags$li(HTML("<b>Time Series of Asset Weight</b> displays the plot of the individual asset over the time. Users can observe the dynamic behavior of each asset across the time.")),
                                           tags$li(HTML("<b>Assets Allocation</b> displays the charts of portfolio weights at given time, <b>Select Date</b>. Users can observe the attribute of each asset by using the picker <b>Select Factor</b>.")),
                                           tags$li(HTML("<b>Attribute Allocation in Portfolio</b> displays the plot of the attribute weighted by portfolio weights, $\\sum_{i = 1}^N w_iA_i$, where $N$ is the portfolio's asset count, $w_i$ is the asset's investment weight, and $A_i$ is the asset's attribute. It can be seen as the expected attribute. We calculate them for the short and long positions separately. Users can observe the behavior of the portfolio's expected attributable over time in this chart.")),
                                           tags$li(HTML("<b>Distribution of Attributes for Long Position</b> and <b>Distribution of Attributes for Short Position</b> show the boxplots of distribution of attributes separated by holding positions. The white dot represents the average value of the distribution." )),
                                           tags$li(HTML("<b>Table of Time Series of Attributes</b> displays the expected attribute of the portfolio over time separated by holding positions.")),
                                           tags$li(HTML("<b>Distribution of Portfolio Risk Profiles</b> displays the boxplots of distribution of portfolio's performances over the studying period. The white dot represents the average value of the distribution.")),
                                           tags$li(HTML("<b>Efficiency Frontier</b> displays the plot of portfolio standard deviation versus return. The picker has multiple options that allow user to observe many curves in one plot.")),
                                           tags$li(HTML("<b>Table of Risk Profiles</b> displays the table of risk profiles calculated backward before each rebalancing versus time.")),
                                           tags$li(HTML("<b>Time Series of Risk Profiles</b> displays the plot of risk profiles calculated backward before each rebalancing versus time.")),
                                           tags$li(HTML("<b>Distribution of Percentage of Relative Change between Non-divested and divested Portfolios</b> displays the boxplots of the distribution of the percentage of non-divested and divested Portfolios spread out. It is calculated by,
                                      $$PR = \\frac{AP_{divest} - AP_{non-divest}}{AP_{non-divest}} \\times 100\\%$$, where $AP_{non-divest}$ and $AP_{divest}$ are the attribute or risk profile of the portfolio before, and 
                                      after divestment, respectively")),
                                           tags$br(""),
                                           "* User Guide box is collapsible",
                                           id = "mybox",
                                           collapsible = TRUE,
                                           closable = TRUE,
                                           width = 13
                                         ),
                                         
                                         h4("Table of Divestment Schedule"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("plot4divtab", width = "auto", height = "auto")),
                                         h4("Portfolio Weight After Divestment"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("plot4divtabsum", width = "auto", height = "auto")),
                                         fixedRow(
                                           column(6,
                                                  h4("Sum of Weights of Divestable Assets"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotw3InDiv1", width = "auto", height = "auto")
                                                  )),
                                           column(6,
                                                  h4("Sum of Allocated Weights and Investable Assets Weights"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotw3InDiv2", width = "auto", height = "auto"))
                                           )),
                                         fixedRow(
                                           column(6,
                                                  h4("Time Series of Asset Weight"),
                                                  hr(),
                                                  pickerInput(
                                                    inputId = "selcomdiv",
                                                    label = "Select Company", 
                                                    choices = colnames(file5_Weight_Div)[-1]
                                                  ),
                                                  withLoader(plotlyOutput("plotw3div", width = "auto", height = "auto"))),
                                           column(6,
                                                  h4("Assets Allocation"),
                                                  hr(),
                                                  fixedRow(
                                                    column(6,
                                                           pickerInput(
                                                             inputId = "selcatdiv",
                                                             label = "Select Factor", 
                                                             choices = colnames(file1_Status)
                                                           )),
                                                    column(6,
                                                           pickerInput(
                                                             inputId = "seldatediv",
                                                             label = "Select Date", 
                                                             choices = unlist(file5_Weight_Div$Date, use.names = F)
                                                           ))),
                                                  withLoader(plotlyOutput("plotw3portdiv", width = "auto", height = "auto"))),
                                           h4("Attribute Allocation in Portfolio"),
                                           hr(),
                                           withLoader(plotlyOutput("plotw3Fdiv", width = "auto", height = "auto"))
                                         ),
                                         fixedRow(
                                           column(6,
                                                  h4("Distribution of Attributes for Long Position"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotw3Flongdiv"))
                                           ),
                                           column(6,
                                                  h4("Distribution of Attributes for Short Position"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotw3Fshortdiv")))
                                         ),
                                         withLoader(DTOutput("plotw3Ftabdiv", width = "auto", height = "auto")),
                                         fixedRow(
                                           column(6,
                                                  h4("Distribution of Portfolio Risk Profiles"),
                                                  hr(),
                                                  withLoader(plotlyOutput("radardiv"))
                                           ),
                                           column(6)
                                         ),
                                         h4("Table of Risk Profiles"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("plotw3riskdiv", width = "auto", height = "auto")),
                                         h4("Time Series of Risk Profiles"),
                                         hr(),
                                         withLoader(plotlyOutput("plotw3riskplotdiv", width = "auto", height = "auto")),
                                         h4("Table of Time Series of Risk Profiles"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("plotdiftable", width = "auto", height = "auto")),
                                         h4("Distribution of Percentage of Relative Change between Non-divested and Divested Portfolios"),
                                         hr(),
                                         textInput('ly.st5','Set Limit of y-axis',NA),
                                         actionBttn(
                                           inputId = "sub.ly.st5",
                                           label = "submit",
                                           style = "pill", 
                                           color = "success",
                                           size = "xs"
                                         ),
                                         withLoader(plotlyOutput("plotdifboxplot", width = "auto", height = "auto"))
                                         
                                  ))
                       ),
                       
                       tabPanel(title  = "Optional I",
                                fixedRow(
                                  column(1, 
                                         dropdown(fileInput("file6", "F5: CSV File of Weights Dynamic", 
                                                            accept = c(".csv")),
                                                  fileInput("file7", "F1: CSV File of Attributes", 
                                                            accept = c(".csv")),
                                                  fileInput("file8", "F2: CSV File of Returns", 
                                                            accept = c(".csv")),
                                                  style = "unite", icon = icon("check"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ),
                                                  actionBttn(
                                                    inputId = "subcomp",
                                                    label = "submit",
                                                    style = "pill", 
                                                    color = "success"
                                                  )
                                         )),
                                  column(11,
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         h3("Optional I: Multiple Portfolios Comparison"),
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         
                                         box(
                                           title = h4("User Guide"),
                                           "This optional STEP is used for analysis and comparing the performance of many portfolios. It works independently of other STEP can run the process without processing STEP 1 to STEP 5. This STEP accepts only CSV files, so users need to prepare the files before processing the optional STEP.",
                                           tags$br(""),
                                           "How to use this page?",
                                           tags$br(""),
                                           tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Weights Dynamic</b>. The file must consist of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset weights at given time, and <br>
                                    iii) one column of the name 'PORTNAME', containing the name or the label of each portfolio.")),
                                           img(src = 'file5.png'),
                                           tags$br(""),
                                           tags$li(HTML("Upload <b>CSV File of Attributes</b> that consists of; <br>
                                    i) one column of the name 'Ticker', containing the ticker of assets, and <br>
                                    ii) one column of the name 'Status', containing the status of the asset either 'Invest' or 'Divest', and <br>
                                    iii) at least one numeric column of any name.")),
                                           img(src = 'file1.png'),
                                           tags$br(""),
                                           tags$li(HTML("Upload <b>CSV File of Returns</b> that consists of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset returns at given time.")),
                                           img(src = 'file2.png'),
                                           tags$br(""),
                                           hr(),
                                           "The remarks for this STEP is listed below",
                                           tags$br(""),
                                           tags$li(HTML("<b>Table of Investment Weights</b> shows the portfolio weights uploaded from <b>CSV File of Weights Dynamic</b>.")),
                                           tags$li(HTML("<b>Table of Attributes</b> shows the attributes of assets uploaded from <b>CSV File of Attributes</b>.")),
                                           tags$li(HTML("<b>Table of Returns</b> shows the returns of asset uploaded from <b>CSV File of Returns</b>.")),
                                           tags$li(HTML("<b>Time Series of Asset Weight</b> displays the plot of the individual asset over time. Users can observe the dynamic behavior of each asset across time.")),
                                           tags$li(HTML("<b>Sum of Weights of Divestable Assets</b> shows the chart of the sum of the weights of divestable assets over time. The weights will decrease over time, and the capital will be allocated to the investable assets.")),
                                           tags$li(HTML("<b>Sum of Allocated Weights and Investable Assets Weights</b> shows the chart of the sum of the weights allocated from the divestable and investable asset weights. ")),
                                           tags$li(HTML("<b>Table of Time Sereis of Attributes</b> displays the attribute weighted by portfolio weights over time, $\\sum_{i = 1}^N w_iA_i$, where $N$ is the portfolio's asset count, $w_i$ is the asset's investment weight, and $A_i$ is the asset's attribute. It can be seen as the expected attribute. We calculate them for the short and long positions separately.")),
                                           tags$li(HTML("<b>Attribute Allocation in Portfolio</b> displays the plot of the attribute weighted by portfolio weights over time. Users can observe the behavior of the portfolio's expected attributable over time in this chart.")),
                                           tags$li(HTML("<b>Distribution of Attributes for Long Position</b> and <b>Distribution of Attributes for Short Position</b> show the boxplots of distribution of attributes separated by holding positions. The white dot represents the average value of the distribution." )),
                                           tags$li(HTML("<b>Table of Time Series of Risk Profiles</b> displays the risk profiles of the portfolio over time separated by holding positions.")),
                                           tags$li(HTML("<b>Distribution of Portfolio Risk Profiles</b> displays the boxplots of distribution of portfolio's performances over the studying period. The white dot represents the average value of the distribution.")),
                                           tags$li(HTML("<b>Time Series of Risk Profiles</b> displays the plot of risk profiles calculated backward before each rebalancing versus time.")),
                                           tags$li(HTML("<b>Percentage of Relative Change compared to Benchmarks</b> is the measure of the difference between the benchmark portfolios from others. The formula is given by
                       $$PR = \\frac{AP_{other} -AP_{benchmark}}{AP_{benchmark}} \\times 100\\%$$, where $AP_{benchmark}$ and $AP_{other}$ are the attribute or risk profile of the benchmark, and comparing portfolio, respectively <br>
                       <b>Select Benchmarks</b> is the option to set the benchmark portfolios. Users can select more than one benchmark. According to the order of the list in the picker, Portfolios after the benchmarks are compared to the nearest benchmark before them.")),
                                           tags$li(HTML("<b>Table of Percentage of Relative Change compared to Benchmarks</b> displays the percentages of the relative change of attributes and risk profiles of benchmarks and other portfolios.")),
                                           tags$li(HTML("<b>Distribution of Percentage of Relative Change compared to Benchmarks</b> displays the distribution of percentages of relative change of attributes and risk profiles of benchmarks and other portfolios. The white dot represents the average value of the distribution.")),
                                           tags$br(""),
                                           "* User Guide box is collapsible",
                                           id = "mybox",
                                           collapsible = TRUE,
                                           closable = TRUE,
                                           width = 13
                                         ),
                                         
                                         h4("Table of Investment Weights"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("div_dynamic", width = "auto", height = "auto")),
                                         h4("Table of Attributes"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("found", width = "auto", height = "auto")),
                                         h4("Table of Returns"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("hist_return", width = "auto", height = "auto")),
                                         
                                         h4("Time Series of Assets Weight"),
                                         hr(),
                                         pickerInput(
                                           inputId = "selcomcom",
                                           label = "Select Company", 
                                           choices = NULL
                                         ),
                                         withLoader(plotlyOutput("plotcom1", width = "auto", height = "auto")),
                                         fixedRow(
                                           column(6,
                                                  h4("Sum of Weights of Divestable Assets"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotcom2", width = "auto", height = "auto")
                                                  )),
                                           column(6,
                                                  h4("Sum of Weights Allocated to Investable Assets"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotcom3", width = "auto", height = "auto"))
                                           )),
                                         h4("Table of Time Sereis of Attributes"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("tab2", width = "auto", height = "auto")),
                                         h4("Attribute Allocation in Portfolio"),
                                         hr(),
                                         withLoader(plotlyOutput("plotcom6", width = "auto", height = "auto")),
                                         fixedRow(
                                           column(6,
                                                  h4("Distribution of Attributes for Long Position"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotcom7", width = "auto", height = "auto")
                                                  )),
                                           column(6,
                                                  h4("Distribution of Attributes for Short Position"),
                                                  hr(),
                                                  withLoader(plotlyOutput("plotcom8", width = "auto", height = "auto"))
                                           )),
                                         h4("Table of Time Series of Risk Profiles"),
                                         hr(),
                                         withLoader(DT::dataTableOutput("tab1", width = "auto", height = "auto")),
                                         h4("Distribution of Risk Profiles"),
                                         hr(),
                                         withLoader(plotlyOutput("plotcom4", width = "auto", height = "auto")),
                                         h4("Time Series of Risk Profiles"),
                                         hr(),
                                         withLoader(plotlyOutput("plotcom5", width = "auto", height = "auto")),
                                         h4("Percentage of Relative Change compared to Benchmarks"),
                                         hr(),       
                                         pickerInput(
                                           inputId = "selben.op1",
                                           label = tags$span("Select Benchmarks", bsButton("bent", label = "", icon = icon("info"), style = "info", size = "extra-small")), 
                                           choices = NULL,
                                           multiple = TRUE,
                                           options = list(
                                             `selected-text-format` = "count > 3")
                                         ),
                                         bsPopover(
                                           id = "bent",
                                           title = "More information",
                                           content = "Users can select more than one benchmark. The non-benchmark portfolios will be compared to the nearest benchmark before them. The names of portfolios will be arranged according to the input CSV file.",
                                           placement = "right",
                                           trigger = "focus",
                                           options = list(container = "body")),
                                         
                                         actionBttn(
                                           inputId = "submul",
                                           label = "submit",
                                           style = "pill", 
                                           color = "success",
                                           size = "xs"
                                         ),
                                         
                                         h4("Table of Percentage of Relative Change compared to Benchmarks"),
                                         hr(),
                                         DT::dataTableOutput("tabben.op1", width = "auto", height = "auto"),
                                         h4("Distribution of Percentage of Relative Change compared to Benchmarks"),
                                         hr(),
                                         textInput("ly.opt1", "Set Limit of y-asix", NA),
                                         plotlyOutput("plotben.op1", width = "auto", height = "auto")
                                  )
                                  
                                )
                       ),
                       tabPanel(title  = "Optional II",
                                fixedRow(
                                  column(1,
                                         dropdown(fileInput("file6.op2", "F5: CSV File of Weights Dynamic", 
                                                            accept = c(".csv")),
                                                  fileInput("file8.op2", "F2: CSV File of Returns", 
                                                            accept = c(".csv")),
                                                  style = "unite", icon = icon("check"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ),
                                                  textInput("tau.op2", "Number of Days in Time Window $\\tau$", 20),
                                                  textInput("sept.op2", "Number of Days in Cluster Analysis, $t$", 20),
                                                  actionBttn(
                                                    inputId = "subclust",
                                                    label = "submit",
                                                    style = "pill", 
                                                    color = "success"
                                                  )
                                         )
                                  ),
                                  column(11, 
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         h3("Optional II: Stability Analysis via Clustering"),
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         
                                         box(
                                           title = h4("User Guide"),
                                           HTML("This optional STEP is used for investigating the stability of portfolio performance.  We employ the clustering algorithm, namely Clustering Large Applications or CLARA, an approximate extension of k-medoids methods to achieve stability investigation. The CLARA is applied to cluster the aggregation time series of risk profiles with a time window of $\\tau$ days. The clustering algorithm is applied every $t$ day. Those parameters can be set in the tool. The optimal number of clusters will be determined by the clustering silhouette. Portfolios that have similar relative behavior will belong to the same cluster over time. It works independently of other STEP. They can run the process without processing STEP 1 to STEP 5. This STEP accepts only CSV files, so users need to prepare the files before processing the optional STEP."),
                                           tags$br(""),
                                           "How to use this page?",
                                           tags$br(""),
                                           tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Weights Dynamic</b>. The file must consist of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset weights at given time, and <br>
                                    iii) one column of the name 'PORTNAME', containing the name or the label of each portfolio.")),
                                           img(src = 'file5.png'),
                                           tags$br(""),
                                           tags$li(HTML("Upload <b>CSV File of Returns</b> that consists of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset returns at given time.")),
                                           img(src = 'file2.png'),
                                           tags$br(""),
                                           tags$li(HTML("<b>Number of Days in Time Window, $\\tau$ </b> is the number of days in the time window for calculating the aggregate time series of risk profiles.")),
                                           tags$li(HTML("<b>Number of Days in Cluster Analysis, $t$ </b> is the number of days for applying a clustering method. The algorithm is applied $\\lceil \\frac{\\text{number of days whole period}}{\\text{number of days in cluster analysis}}\\rceil$ times.")),
                                           hr(),
                                           "The remarks for this STEP is listed below",
                                           tags$br(""),
                                           tags$li(HTML("<b>Table of Investment Weights</b> shows the portfolio weights uploaded from <b>CSV File of Weights Dynamic</b>.")),
                                           tags$li(HTML("<b>Table of Returns</b> shows the returns of asset uploaded from <b>CSV File of Returns</b>.")),
                                           tags$li(HTML("<b>Table of Aggregate Time Series of Risk Profiles</b> displays the aggregate time series of risk profiles of portfolios versus time.")),
                                           tags$li(HTML("<b>Aggregate Time Series of Risk Profiles</b> displays the plots of the aggregate time series of risk profiles of portfolios versus time.")),
                                           tags$li(HTML("<b>Table of Clustering Silhouette</b> gives the silhouette information of clustering. The column name is the number of clusters.")),
                                           tags$li(HTML("<b>Clustering Results</b> displays the heatmaps of the clustering of all portfolios over time. The number inside the cells and color represents the label of the cluster.")),
                                           
                                           tags$br(""),
                                           "* User Guide box is collapsible",
                                           id = "mybox",
                                           collapsible = TRUE,
                                           closable = TRUE,
                                           width = 13
                                         ),
                                         
                                         h4("Table of Investment Weights"),
                                         hr(),
                                         DT::dataTableOutput("tab.op2", width = "auto", height = "auto"),
                                         h4("Table of Returns"),
                                         hr(),
                                         DT::dataTableOutput("tab.op2.2", width = "auto", height = "auto"),
                                         h4("Table of Aggregate Time Series of Risk Profiles"),
                                         hr(),
                                         DT::dataTableOutput("op2tab1", width = "auto", height = "auto"),
                                         h4("Time Series of Risk Profiles"),
                                         hr(),
                                         withLoader(plotlyOutput("op2graph", width = "auto", height = "auto")),
                                         h4("Table of Clustering Silhouette"),
                                         hr(),
                                         DT::dataTableOutput("op2tab2", width = "auto", height = "auto"),
                                         h4("Clustering Results"),
                                         hr(),
                                         withLoader(plotlyOutput("op2hm", width = "auto", height = "auto"))
                                  ))),
                       
                       tabPanel(title  = "Optional III",         
                                fixedRow(
                                  column(1,
                                         dropdown(fileInput("file6.op3", "F5: CSV File of Weights Dynamic", 
                                                            accept = c(".csv")),
                                                  fileInput("file7.op3", "F6: CSV File of Status Divestment, optional", 
                                                            accept = c(".csv")),
                                                  fileInput("file8.op3", "F2: CSV File of Returns", 
                                                            accept = c(".csv")),
                                                  style = "unite", icon = icon("check"),
                                                  status = "primary", width = "300px",
                                                  animate = animateOptions(
                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                    exit = animations$fading_exits$fadeOutRightBig
                                                  ))),
                                  column(11, 
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         h3("Optional III: Graph Structure Correlation Analysis"),
                                         hr(style = "border-top: 6px solid #0784BF;"),
                                         
                                         box(
                                           title = h4("User Guide"),
                                           HTML("This optional STEP is used for investigating the correlation structure of the assets in portfolios using graph LASSO. This function is suitable for studying the dynamic correlation structure of portfolios and comparison of their diversification as portfolio dynamically evolves, for instance as divestment is progressively performed on different sets of assets.
                          It works independently of other STEP users. They can run the process without processing STEP 1 to STEP 5. This STEP accepts only CSV files, so users need to prepare the files before processing the optional STEP."),
                                           tags$br(""),
                                           "How to use this page?",
                                           tags$br(""),
                                           tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, and upload <b>CSV File of Weights Dynamic</b>. The file must consist of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset weights at given time, and <br>
                                    iii) one column of the name 'PORTNAME', containing the name or the label of each portfolio.")),
                                           img(src = 'file5.png'),
                                           tags$br(""),
                                           tags$li(HTML("Upload <b>CSV File of Status Divestment</b> is optional that consists of; <br>
                                    i) one column of the name 'Ticker', containing the ticker of assets, and <br>
                                    ii) one column of the name 'Status', containing the status of the asset either 'Invest' or 'Divest'.")),
                                           img(src = 'file1.png'),
                                           tags$br(""),
                                           tags$li(HTML("Upload <b>CSV File of Returns</b> that consists of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset returns at given time.")),
                                           img(src = 'file2.png'),
                                           tags$br(""),
                                           hr(),
                                           "The remarks for this STEP is listed below",
                                           tags$br(""),
                                           tags$li(HTML("<b>Table of Investment Weights</b> shows the portfolio weights uploaded from <b>CSV File of Weights Dynamic</b>.")),
                                           tags$li(HTML("<b>Table of Divestment Status</b> shows the tickers and their investment status uploaded from <b>CSV File of Status Divestment</b>.")),
                                           tags$li(HTML("<b>Table of Returns</b> shows the returns of asset uploaded from <b>CSV File of Returns</b>.")),
                                           tags$li(HTML("<b>Covariance Portfolio Network Structure</b> displays the network obtained by the graph LASSO algorithm. The tool displays three timesteps for comparing structures. To process the algorithm, select a time in <b> Select Date</b> 
                                       and click the <b> submit</b> button. The correlation of the asset is represented by the link, and its strength is indicated by the thickness. The negative and positive correlations are colored in red and green, respectively. Nodes of the network represent the assets, 
                                       and their colors indicate investment status if a <b>CSV File of Status Divestment</b> was uploaded. The investable assets are colored blue and the divestable assets are colored yellow. The label of nodes are ordered clockwise and alphabetically order with the first node at the top-center position.
                                       It should be noted that even in a small portfolio, the process can be computationally expensive. The computation time varies by the number of assets and the number of portfolios.")),
                                           
                                           tags$br(""),
                                           "* User Guide box is collapsible",
                                           id = "mybox",
                                           collapsible = TRUE,
                                           closable = TRUE,
                                           width = 13
                                         ),
                                         h4("Table of Investment Weights"),
                                         hr(),
                                         DT::dataTableOutput("tab.op3", width = "auto", height = "auto"),
                                         h4("Table of Divestment Status"),
                                         hr(),
                                         DT::dataTableOutput("tab.op3.1", width = "auto", height = "auto"),
                                         h4("Table of Returns"),
                                         hr(),
                                         DT::dataTableOutput("tab.op3.2", width = "auto", height = "auto"),
                                         h4("Portfolio Network Structure of Covariance"),
                                         hr(),
                                         fixedRow(
                                           column(4,
                                                  pickerInput(
                                                    inputId = "gdate1",
                                                    label = "Select Date", 
                                                    choices = NULL),
                                                  actionBttn(
                                                    inputId = "subgdate1",
                                                    label = "submit",
                                                    style = "pill", 
                                                    color = "success",
                                                    size = 'xs'
                                                  ),
                                                  plotOutput('op3.1', height = "1000px")
                                           ),
                                           column(4,
                                                  pickerInput(
                                                    inputId = "gdate2",
                                                    label = "Select Date", 
                                                    choices = NULL),
                                                  actionBttn(
                                                    inputId = "subgdate2",
                                                    label = "submit",
                                                    style = "pill", 
                                                    color = "success",
                                                    size = 'xs'
                                                  ),
                                                  plotOutput('op3.2', height = "1000px")
                                           ),
                                           column(4,
                                                  pickerInput(
                                                    inputId = "gdate3",
                                                    label = "Select Date", 
                                                    choices = NULL),
                                                  actionBttn(
                                                    inputId = "subgdate3",
                                                    label = "submit",
                                                    style = "pill", 
                                                    color = "success",
                                                    size = 'xs'
                                                  ),
                                                  plotOutput('op3.3', height = "1000px")
                                           ))))),
                       
                )),
        
        
        tabItem(tabName = "about_us",
                fixedRow(
                  userBox(
                    width = 6,
                    title = userDescription(
                      title = HTML("<p style='color:white'>Christina Nikitopoulos Sklibosios</p>"),
                      subtitle = HTML("<p style='color:white'>Associate Professor Finance Discipline Group, <br> University of Technology Sydney </p>"),
                      backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                      image = "cns.jpg",
                      type = 2
                    ),
                    status = "teal",
                    boxToolSize = "xl",
                    collapsible = FALSE,
                    footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> Christina.Nikitopoulos@uts.edu.au<br><br>
                                <a href='https://www.linkedin.com/in/kylie-anne-richards/?originalSubdomain=au'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Kylie_Anne_Richards2'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=2060205'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://scholar.google.com.au/citations?user=nQ6iMnEAAAAJ&hl=en'><i class='fa-brands fa-google fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://profiles.uts.edu.au/Christina.Nikitopoulos'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                    #
                  ),
                  userBox(
                    width = 6,
                    title = userDescription(
                      title = HTML("<p style='color:white'>Eric Ofusu Hene</p>"),
                      subtitle = HTML("<p style='color:white'>Senior Lecturer in Accounting and Finance, <br> De Montfort University</p>"),
                      backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                      image = "eoh.jpg",
                      type = 2
                    ),
                    status = "teal",
                    boxToolSize = "xl",
                    collapsible = FALSE,
                    footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> eric.ofusu-hene@dmu.ac.uk<br><br>
                                <a href='https://www.linkedin.com/in/eric-dei-ofosu-hene-phd-526b3a96/?originalSubdomain=uk'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Eric-Ofosu-Hene'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=2756712'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://scholar.google.com/citations?user=YBJxo5gAAAAJ&hl=en'><i class='fa-brands fa-google fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.dmu.ac.uk/about-dmu/academic-staff/business-and-law/eric-dei-ofosu-hene/eric-dei-ofosu-hene.aspx'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                    
                  ),
                  
                  userBox(
                    width = 6,
                    title = userDescription(
                      title = HTML("<p style='color:white'>Gareth W. Peters</p>"),
                      subtitle = HTML("<p style='color:white'>Janet & Ian Duncan Endowed Chair of Actuarial Science, <br>
                                  Chair Professor of Statistics for Risk and Insurance, <br>
                                  University of California, Santa Barbara</p>"),
                      backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                      image = "gwp.png",
                      type = 2
                    ),
                    status = "teal",
                    boxToolSize = "xl",
                    collapsible = FALSE,
                    footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> garethpeters@ucsb.edu<br><br>
                                <a href='https://www.linkedin.com/in/prof-dr-gareth-w-peters-3928b4139/'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Gareth-Peters'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=2019678'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://scholar.google.co.jp/citations?user=lsb_nJoAAAAJ&hl=en'><i class='fa-brands fa-google fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.qrslab.com/'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                  ),
                  userBox(
                    width = 6,
                    title = userDescription(
                      title = HTML("<p style='color:white'>Kylie-Anne Richards</p>"),
                      subtitle = HTML("<p style='color:white'>Investment Practices, Fortlake <br>
                                                  Lecturer Finance Discipline Group, <br> University of Technology Sydney</p>"),
                      backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                      image = "kar.png",
                      type = 2
                    ),
                    status = "teal",
                    boxToolSize = "xl",
                    collapsible = FALSE,
                    footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> Kylie-Anne.Richards@uts.edu.au<br><br>
                                <a href='https://www.linkedin.com/in/kylie-anne-richards-9660765/?originalSubdomain=au'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Kylie_Anne_Richards2'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=2060205'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://profiles.uts.edu.au/Kylie-Anne.Richards'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                  ),
                  userBox(
                    width = 6,
                    title = userDescription(
                      title = HTML("<p style='color:white'>Pasin Marupanthorn</p>"),
                      subtitle = HTML("<p style='color:white'>Graduate Student in Actuarial Mathematics, <br> Heriot-Watt University and <br> The Maxwell Institute for Mathematical Sciences</p>"),
                      backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
                      image = "pm.jpg",
                      type = 2
                    ),
                    status = "teal",
                    boxToolSize = "xl",
                    collapsible = FALSE,
                    footer = HTML("<i class='fa-solid fa-envelope fa-xl' style = 'color:#0072B2;'></i> pm122@hw.ac.uk<br><br>
                                <a href='https://www.linkedin.com/in/pasin-marupanthorn/?originalSubdomain=uk'><i class='fa-brands fa-linkedin fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://www.researchgate.net/profile/Pasin-Maruphanton-2'><i class='fa-brands fa-researchgate fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=5286317'><i class='fa-solid fa-paperclip fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://scholar.google.com/citations?user=NcoXQYYAAAAJ&hl=en'><i class='fa-brands fa-google fa-xl' style = 'color:#0072B2;'></i></a>
                                              <a href= 'https://oporkabbb.wixsite.com/math'><i class='fa-solid fa-user fa-xl' style = 'color:#0072B2;'></i></a>")
                  )
                )
        )
      )
    )
  )
  
  #box(width = 7, solidheader = TRUE, check.names = FALSE, status = "primary", title = "Sustainable Score", 
  
  
  server <- function(input, output, session) {
    
    ########Panel A###############
    data_yahoo  <- eventReactive(input$submit, {get_data(input$ticker)})
    data_stock <- eventReactive(input$submit, {get_stock_profile(input$ticker)})
    candel_stock <- eventReactive(input$submit, {candlestick_plot(data_yahoo(),input$ticker)})
    perf_stock <- eventReactive(input$submit, {performance_plot(data_yahoo(),input$ticker)})
    re_ticker <- eventReactive(input$add, {
      main_port <<- unique(c(main_port,toupper(as.character(input$ticker_sel))))
      main_port})
    
    
    
    #####Render
    
    output$lev_esg <- renderValueBox({
      valueBox(
        value = tags$p(data_stock()[4], style = "font-size: 70%;"),
        subtitle = tags$p(data_stock()[10], style = "font-size: 70%;"),
        color = ifelse(data_stock()[5] %in% validcol,data_stock()[5],'black'),
        icon = icon('seedling')
      )})
    output$total_esg <- renderValueBox({
      valueBox(
        value = tags$p(data_stock()[6], style = "font-size: 70%;"),
        subtitle = tags$p("Total ESG Score ", style = "font-size: 70%;"),
        color = ifelse(data_stock()[5] %in% validcol,data_stock()[5],'black'),
        icon = icon('tree')
      )})#icon("thumbs-up", lib = "glyphicon")
    output$e_score<- renderValueBox({
      valueBox(
        value = tags$p(data_stock()[7], style = "font-size: 70%;"),
        subtitle = tags$p('Environment', style = "font-size: 70%;"),
        color = 'light-blue',
        icon = icon('solar-panel')
      )})
    output$s_score<- renderValueBox({
      valueBox(
        value = tags$p(data_stock()[8], style = "font-size: 70%;"),
        subtitle = tags$p('Social', style = "font-size: 70%;"),
        color = 'aqua',
        icon = icon('hashtag')
      )})
    output$g_score <- renderValueBox({
      valueBox(
        value = tags$p(data_stock()[9], style = "font-size: 70%;"),
        subtitle = tags$p('Governance', style = "font-size: 70%;"),
        color = 'blue',
        icon = icon('handshake')
      )})
    
    
    
    
    
    #######Panel B; File 1######### 
    table_esg <- eventReactive(input$subesg, {
      port <- unique(c(as.character(input$source),as.character(input$divt)));
      tab_esg <- sapply(seq_along(port), function(i) get_esg_profile(port[i])) %>% t() %>% data.frame() %>% `colnames<-`(c("Name","Ticker","Sector","Subsector","ESG Level","ESG","Enironment","Social","Governance"));
      for(i in 6:9){tab_esg[,i] <- as.numeric(tab_esg[,i])};
      tab_esg$Status <- ifelse((port %in% as.character(input$source)),"Invest","Divest");
      file1_Status <<- tab_esg;
      file1_Status
    })
    
    
    #######Panel B; File 2######### 
    
    
    
    
    #assets_allocation_weight <-  eventReactive(input$subcalport, {assets_allocation(batch()[[2]],file1_Status,as.character(input$portw))})
    
    
    observeEvent(input$subup, {
      if(is.null(file1_Status)){
        feedbackWarning(
          inputId = "subup",
          show = FALSE,
          text = "Upload File before")
      }else{
        port <- unique(c(as.character(input$source),as.character(input$divt)))
        port_up <- setdiff(port,file1_Status$Ticker)
        if(length(port_up) != 0){
          tab_esg <- sapply(seq_along(port_up), function(i) get_esg_profile(port_up[i])) %>% t() %>% data.frame() %>% `colnames<-`(c("Name","Ticker","Sector","Subsector","ESG Level","ESG","Enironment","Social","Governance"))
          for(i in 6:9){
            tab_esg[,i] <- as.numeric(tab_esg[,i])
          }
          tab_esg$Status <- ifelse((port_up %in% as.character(input$source)),"Invest","Divest")
          file1_Status <<- rbind(file1_Status,tab_esg)
        }else{
          dum <- rep(0,nrow(file1_Status))
          for(i in 1:length(file1_Status$Ticker)){
            dum[i] <-  ifelse((file1_Status$Ticker[i] %in% as.character(input$source)),"Invest","Divest")
          }
          file1_Status$Status <<- dum 
        }
        
        output$ui_esg_table <- DTtable(file1_Status)
      }
    })
    
    
    
    observeEvent(input$fileticker, {
      File <- input$fileticker
      validate(
        need(File != "", "No data has been uploaded")
      )
      tick <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      if(ncol(tick) == 1){
        tick$inv <- rep(1,nrow(tick))
      }else if(ncol(tick) == 2){
        tick <- tick
      }else{
        tick <- NULL
      }
      inv_asset <<- as.character(tick[,1][tick[,2] == 1])
      div_asset <<- as.character(tick[,1][tick[,2] == 0])
      updateOrderInput(session,inputId = "source", items =  inv_asset, item_class = 'info')
      updateOrderInput(session,inputId = "divt", items =  div_asset, item_class = 'info')
    })
    
    observeEvent(input$file1, {
      File <- input$file1
      validate(
        need(File != "", "No data has been uploaded")
      )
      f1 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      con1 <- (sum(sapply(f1, class) %in% c('interger', 'numeric')) == 0)
      con2 <- (sum(c("Status",'Ticker') %in% colnames(f1)) != 2)
      if(con1){
        showFeedbackWarning(
          inputId = "file1",
          text = "Require at least on column to be numeric")
      }else if(con2){
        showFeedbackWarning(
          inputId = "file1",
          text = "Missing columns of the names 'Ticker' and 'Status'" )
      }
      else if(FALSE %in% (unlist(f1$Status) %in% c('Invest', 'Divest'))){
        showFeedbackWarning(
          inputId = "file1",
          text = "Require Status to be 'Invest' or 'Divest'" )
      }else if(is.na(f1)){
        showFeedbackWarning(
          inputId = "file1",
          text = "Missing Data Detected" )
      } 
      else{
        file1_Status <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        output$ui_esg_table <- DTtable(file1_Status)
        inv_asset <<- (file1_Status %>% filter(Status == 'Invest'))$Ticker 
        div_asset <<- (file1_Status %>% filter(Status == 'Divest'))$Ticker
        updateOrderInput(session,inputId = "source", items =  inv_asset, item_class = 'info')
        updateOrderInput(session,inputId = "divt", items =  div_asset, item_class = 'info')
      }
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$file2, {
      File <- input$file2
      validate(
        need(File != "", "No data has been uploaded")
      )
      f2 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f2,c("Date"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file2",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file2",
          text = "Missing the column of the names 'Date'" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file2",
          text = "Missing Data Detected" )
      }else{
        file2_Historical <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        plottab <- file2_Historical
        output$tab_return <- DTtable(round_tab(plottab))
        output$reportreturn <-  DTtable(return_summary_update(file2_Historical))
      }
      
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$add, {
      updateOrderInput(session,inputId = "inter", items =  re_ticker(), item_class = 'info')
    })  
    
    observeEvent(input$reasset, {
      updateOrderInput(session,inputId = "inter", items =  NULL, item_class = 'info')
    })  
    
    batch <- eventReactive(input$subtime, {
      if(is.null(file1_Status)){
        noty(text = "Require Uploading File in Step 2 before", type = "warning")
      }else{
        
        data <- BatchGetSymbols(file1_Status$Ticker,
                                as.Date(input$stdate),
                                as.Date(input$enddate),
                                0.1)
        dataframe <- data[[2]] %>% dplyr::select(ticker,ref.date,ret.adjusted.prices) %>% drop_na() %>% dcast(ref.date ~ ticker) 
        colnames(dataframe)[which(names(dataframe) == "ref.date")] <- "Date"
        file2_Historical <<- dataframe
        file2_Historical}
    })
    
    output$tab_return <- DTtable(NULL);
    observeEvent(input$subtime,{
      validate(need(!is.null(file1_Status)," "))
      plottab <- batch();
      plottab[,2:ncol(plottab)] <- round(plottab[,2:ncol(plottab)],6);
      output$tab_return <- DTtable(plottab);
      output$reportreturn <-  DTtable(return_summary_update(batch()))})
    
    
    
    
    output$comp_name <- renderText(data_stock()[1])
    output$comp_market <- renderText(data_stock()[14])
    output$comp_sec <- renderText(paste0("Sector: ", data_stock()[2],", Subsector: ",data_stock()[3]))
    output$comp_sum <- renderText(paste0("Market Cap : ", data_stock()[11],", PE Ratio: ",data_stock()[12],", Forward Dividend & Yield: ",data_stock()[13]))
    output$perf_fig <- renderPlotly(perf_stock()[[2]])  
    output$perf_table <- DTtable(perf_stock()[[1]]) 
    output$verb <- renderText({re_ticker()})
    
    output$ui_esg_table <-  DTtable(table_esg())
    
    output$reportreturn <-  DTtable(return_summary(batch()[[2]]))
    
    output$tab_return <- DTtable(NULL)
    
    
    output$ui_esg_table_com <-  DTtable(NULL)
    
    observeEvent(list(input$file1, input$subesg, input$subup), {
      validate(
        need((!is.null(input$file1))||(input$subesg == TRUE)||(input$subup == TRUE), "Please select a weight")
      )
      output$ui_esg_table_com  <- DTtable(round(plotstart(file1_Status),2) %>% rownames_to_column('Status'));
      updatePickerInput(session, inputId = "selq", choices = file1_Status[, sapply(file1_Status, class) %in% c('numeric','integer'), drop=FALSE] %>% colnames())})
    
    observeEvent(input$subman, {
      if((as.numeric(input$perc) > 1)||(as.numeric(input$perc) < 0)||(is.na(as.numeric(input$perc)))){
        feedbackWarning(
          inputId = "perc",
          show = TRUE,
          text = "Please input number from 0 to 1"
        )}else if(is.null(file1_Status)){
          noty("Require Table of Attributes", type = 'warning')
        }else{
          file1_Status <<- auto_selection(file1_Status, input$selq, as.numeric(input$perc),input$seldec)
          output$ui_esg_table <- DTtable(file1_Status)
          output$ui_esg_table_com  <- DTtable(round(plotstart(file1_Status),2) %>% rownames_to_column('Status'))}})
    
    #,
    #####Render Radar Chart
    output$candel <- renderPlotly(candel_stock())
    
    
    #####Render Boxlot of Assset Returns
    
    observeEvent(input$file2,{
      updatePickerInput(session, "rankby", choices =list("Ticker","Return", "Volatility", "Sharpe", "MDD", "Sortino", "Return.cumulative", "VaR"))
    })
    observeEvent(input$subtime,{
      validate(need(!is.null(file1_Status)," "))
      updatePickerInput(session, "rankby", choices =list("Ticker","Return", "Volatility", "Sharpe", "MDD", "Sortino", "Return.cumulative", "VaR"))
    })
    
    
    output$boxasset <- renderPlotly(NULL)
    
    observeEvent(input$rankby, {
      validate(
        need(!is.null(input$rankby),"Please upload Historical Return")
      )
      output$boxasset <- renderPlotly(boxplot_assets(file2_Historical, as.character(input$rankby)))
    })
    
    
    ############################Panel B: File 3####################################
    
    observeEvent(input$file3, {
      File <- input$file3
      validate(
        need(File != "", "No data has been uploaded")
      )
      
      f3 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f3,c("Date"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file3",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file3",
          text = "Missing the column of the names 'Date'" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file3",
          text = "Missing Data Detected" )
      }else if(sum(sapply(f3[,-1],class) %in% c('integer','numeric')) != ncol(f3[,-1])){
        showFeedbackWarning(
          inputId = "file3",
          text = "Detected Non-numeric Colunms" )
      }else if(ncol(f3) <= 2){
        showFeedbackWarning(
          inputId = "file3",
          text = "Portfolio need more than one asset" )
      }else{
        file3_Weight <<- f3
        output$weight <- DTtable(file3_Weight)
      }
    }, ignoreNULL = FALSE)
    
    
    ###Calculate Weight
    get_weight <- eventReactive(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      if(is.null(file1_Status)||is.null(file2_Historical)){
        noty(text = "Require Uploading Files in Step 2 and 3 before", type = "warning")
      }else{
        if(is.null(input$file3)){
          Weightset <- allocation(file2_Historical,input$type,as.integer(input$tau),as.integer(input$reb),lim = as.integer(input$limselect)*as.numeric(input$shortlim), NULL)}else{
            Weightset <-  list(file3_Weight, multipywtoreturn(file3_Weight,file2_Historical),NULL,file3_Weight$Date)
          }
        file3_Weight <<- Weightset[[1]];
        file3_Weight[,2:ncol(file3_Weight)] <<- round(file3_Weight[,2:ncol(file3_Weight)],6);
        list(file3_Weight,Weightset[[2]],Weightset[[3]],Weightset[[4]])}})
    
    
    output$weight <- DTtable(NULL) #For render
    
    
    observeEvent(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      output$weight <- DTtable(get_weight()[[1]])
    }) #For update
    
    
    
    ###Render Asset Weight Plot (Dummy)
    output$plotw3 <- renderPlotly(NULL)  #For render
    
    ###Render Asset Weight Plot
    shiny_asset_weight_plot <- eventReactive(input$selcom, asset_weight_plot(get_weight()[[1]],as.character(input$selcom)))
    observeEvent(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      updatePickerInput(session, "selcom", choices = colnames(get_weight()[[1]])[-1])
    }) #For update choice
    observeEvent(input$selcom, output$plotw3 <- renderPlotly(shiny_asset_weight_plot())) #For update plot
    
    ###Render Fundamental (Dummy)
    output$plotw3F <- renderPlotly(NULL)  #For render
    output$plotw3Fshort <- renderPlotly(NULL)  #For render
    output$plotw3Flong <- renderPlotly(NULL)  #For render
    output$plotw3Ftab <- renderDT(NULL)  #For render
    
    
    ###Render Fundamental
    shiny_plot_fundamental <- eventReactive(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      plot_fundamental(get_weight()[[1]],file1_Status)})
    observeEvent(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      output$plotw3F <- renderPlotly(shiny_plot_fundamental()[[1]])
      output$plotw3Fshort <- renderPlotly(shiny_plot_fundamental()[[3]])
      output$plotw3Flong <- renderPlotly(shiny_plot_fundamental()[[2]])
      output$plotw3Ftab <- DTtable(shiny_plot_fundamental()[[4]])
    }) #For update plot
    
    
    ###Render Asset Weight Plot (Dummy)
    output$plotw3port <- renderPlotly(NULL)  #For render
    
    ###Render Asset Weight Plot
    shiny_plot_portfolio_weight <- eventReactive(list(input$subweight,input$file3), plot_portfolio_weight(get_weight()[[1]],file1_Status,as.character(input$selcat),as.Date(input$seldate)))
    observeEvent(list(input$subweight,input$file3), {
      output$plotw3Ftab <- DTtable(shiny_plot_fundamental()[[4]])
      updatePickerInput(session, "selcat", choices = colnames(file1_Status))
      updatePickerInput(session, "seldate", choices = as.character(unlist(get_weight()[[1]]$Date), use.names = FALSE))
    }) #For update choice
    observeEvent(input$selcat, output$plotw3pOort  <- renderPlotly(plot_portfolio_weight(get_weight()[[1]],file1_Status,as.character(input$selcat),as.Date(input$seldate)))) #For update plot
    observeEvent(input$seldate, output$plotw3port  <- renderPlotly(plot_portfolio_weight(get_weight()[[1]],file1_Status,as.character(input$selcat),as.Date(input$seldate)))) #For update plot
    
    
    ###Calculate Risk Profiles
    
    risk <- eventReactive(list(input$subweight,input$file3),{
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      all_performance_table(file2_Historical, file3_Weight)})
    
    ###Render Risk Profiles (Dummy)
    output$plotw3risk <- DTtable(NULL)  #For render
    
    ###Render Risk Profiles
    observeEvent(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      output$plotw3risk <- DTtable({tabr <- risk();
      tabr[,sapply(tabr, class) %in% c('numeric','integer')] <- round(tabr[,sapply(tabr, class) %in% c('numeric','integer')],6);
      tabr})}) #For update plot
    
    
    ###Render Risk Profiles Plot (Dummy)
    output$plotw3riskplot <- renderPlotly(NULL)  #For render
    
    ###Render Risk Profiles Plot
    shiny_plot_performance_table <- eventReactive(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      plot_performance_table(risk())})
    observeEvent(list(input$subweight,input$file3) , {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      output$plotw3riskplot <- renderPlotly(shiny_plot_performance_table())}) #For update plot
    
    
    ###Render Radar Plot (Dummy)
    output$radar <- renderPlotly(NULL)  #For render
    
    ###Render Risk Profiles Plot
    shiny_plot_radar <- eventReactive(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      plot_radar(risk())})
    
    observeEvent(list(input$subweight,input$file3) , {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      output$radar <- renderPlotly(plot_radar(risk()))}) #For update plot
    
    ###Render Efficiency Frontier (Dummy)
    output$eff <- renderPlotly(NULL)  #For render
    
    ###Render Efficiency Frontier
    shiny_efficient_fontier <- eventReactive(input$subeff, efficient_fontier(file2_Historical,file3_Weight))
    observeEvent(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      updatePickerInput(session, "seldateeff", choices = as.character(unlist(get_weight()[[1]]$Date), use.names = FALSE))}) #For update choice
    observeEvent(input$subeff , output$eff <- renderPlotly(plot_efficient_fontier(shiny_efficient_fontier()[[1]],file2_Historical,shiny_efficient_fontier()[[2]],input$seldateeff)))
    observeEvent(input$seldateff, output$eff  <- renderPlotly(plot_efficient_fontier(shiny_efficient_fontier()[[1]],file2_Historical,shiny_efficient_fontier()[[2]],input$seldateeff))) #For update plot
    
    
    ############################Panel B: File 4####################################
    
    observeEvent(input$file4, {
      File <- input$file4
      validate(
        need(File != "", "No data has been uploaded")
      )
      
      f4 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f4,c("Date","Bound"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file4",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file4",
          text = "Missing the column of the names 'Date' or 'Bound'" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file4",
          text = "Missing Data Detected" )
      }else{
        file4_Schedule  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      }
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$subdiv, {
      if((is.null(file1_Status))||(is.null(file2_Historical))||(is.null(file3_Weight))){
        noty(text ="Require Uploading Files in Step 2, 3 and 4 before", type = "warning")
      }else{
        file4_Schedule <<- div_schedule(as.integer(input$rate), c(as.numeric(input$m),as.numeric(input$a)),file3_Weight,as.Date(input$lastdate))}})
    
    
    ###Update End Date Selection
    observeEvent(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      updatePickerInput(session, "lastdate", choices = as.character(unlist(get_weight()[[1]]$Date)[-1], use.names = FALSE))}) #For update choice
    
    ###Render Divestment Schedule (Dummy)
    output$plot4divtab <- DTtable(NULL)  #For render
    
    ###Render Divestment Schedule
    
    observeEvent(list(input$subdiv,input$file4), {
      validate(need((!is.null(file3_Weight)),""))
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      output$plot4divtab <- DTtable({file4_Schedule})}) #For update plot
    
    ###Render Divestment Table (Dummy)
    output$plot4divtabsum <- DTtable(NULL)  #For render
    
    ###Render Divestment Table
    shiny_div_weight <- eventReactive(list(input$subdiv,input$file4),{ 
      validate(need((!is.null(file3_Weight)),""))
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      div_weight(file3_Weight, file4_Schedule,file1_Status)})
    
    observeEvent(list(input$subdiv,input$file4),{
      validate(need((!is.null(file3_Weight)),""))
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      output$plot4divtabsum <- DTtable({tab <- shiny_div_weight(); file5_Weight_Div <<- tab;
      tab[,2:ncol(tab)] <- round(tab[,2:ncol(tab)],6); tab})}) #For update plot
    
    
    
    
    #input$test1 | input$test2
    
    ###Render In-Div Sum (Dummy)
    output$plotw3InDiv1 <- renderPlotly(NULL)  #For render
    output$plotw3InDiv2 <- renderPlotly(NULL)  #For render
    
    ###Render In-Div Sum
    shiny_plot_div_Sch <- eventReactive(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      plot_div_Sch(file3_Weight,file1_Status,file4_Schedule, file5_Weight_Div)})
    observeEvent(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      output$plotw3InDiv1 <- renderPlotly(shiny_plot_div_Sch()[[1]])}) #For update plot
    observeEvent(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      output$plotw3InDiv2 <- renderPlotly(shiny_plot_div_Sch()[[2]])}) #For update plot
    
    
    
    ###
    ###Render Asset Weight Plot (Dummy)
    output$plotw3div <- renderPlotly(NULL)  #For render
    
    ###Render Asset Weight Plot
    shiny_asset_weight_plot_div <- eventReactive(input$selcomdiv, asset_weight_plot_div(file3_Weight,file5_Weight_Div,as.character(input$selcomdiv)))
    observeEvent(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      updatePickerInput(session, "selcomdiv", choices = colnames(shiny_div_weight())[-1])}) #For update choice
    observeEvent(input$selcomdiv, output$plotw3div <- renderPlotly(shiny_asset_weight_plot_div())) #For update plot
    
    ###Render Asset Weight Plot (Dummy)
    output$plotw3portdiv <- renderPlotly(NULL)  #For render
    
    ###Render Asset Weight Plot
    shiny_plot_portfolio_weight_div <- eventReactive(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      plot_portfolio_weight_div(file3_Weight,shiny_div_weight(),file1_Status,as.character(input$selcatdiv),as.Date(input$seldatediv))
    })
    observeEvent(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      updatePickerInput(session, "selcatdiv", choices = colnames(file1_Status))
      updatePickerInput(session, "seldatediv", choices = as.character(unlist(shiny_div_weight()$Date), use.names = FALSE))}) #For update choice
    
    observeEvent(input$selcatdiv, output$plotw3portdiv  <- renderPlotly(plot_portfolio_weight_div(file3_Weight, shiny_div_weight(),file1_Status,as.character(input$selcatdiv),as.Date(input$seldatediv)))) #For update plot
    observeEvent(input$seldatediv, output$plotw3portdiv  <- renderPlotly(plot_portfolio_weight_div(file3_Weight, shiny_div_weight(),file1_Status,as.character(input$selcatdiv),as.Date(input$seldatediv)))) #For update plot
    
    
    ###Calculate Risk Profiles
    
    riskdiv <- eventReactive(list(input$subdiv,input$file4),{
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      all_performance_table(file2_Historical, shiny_div_weight())})
    
    ###Render Risk Profiles (Dummy)
    output$plotw3riskdiv <- DTtable(NULL)  #For render
    
    ###Render Risk Profiles
    observeEvent(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      output$plotw3riskdiv <- DTtable({tabr <- riskdiv();
      tabr[,sapply(tabr, class) %in% c('numeric','integer')] <- round(tabr[,sapply(tabr, class) %in% c('numeric','integer')],6);
      tabr})}) #For update plot
    
    
    ###Render Risk Profiles Plot (Dummy)
    output$plotw3riskplotdiv <- renderPlotly(NULL)  #For render
    
    ###Render Risk Profiles Plot
    shiny_plot_performance_table_div <- eventReactive(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      plot_performance_table_div(risk(),riskdiv())})
    observeEvent(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      output$plotw3riskplotdiv <- renderPlotly(shiny_plot_performance_table_div())}) #For update plot
    
    ###Render Fundamental (Dummy)
    ###Render Asset Weight Plot
    shiny_asset_weight_plot <- eventReactive(input$selcom, asset_weight_plot(get_weight()[[1]],as.character(input$selcom)))
    observeEvent(list(input$subweight,input$file3), {
      validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
      updatePickerInput(session, "selcom", choices = colnames(get_weight()[[1]])[-1])}) #For update choice
    observeEvent(input$selcom, output$plotw3 <- renderPlotly(shiny_asset_weight_plot())) #For update plot
    
    ###Render Fundamental (Dummy)
    output$plotw3Fdiv <- renderPlotly(NULL)  #For render
    output$plotw3Fshortdiv <- renderPlotly(NULL)  #For render
    output$plotw3Flongdiv <- renderPlotly(NULL)  #For render
    output$plotw3Ftabdiv <- renderDT(NULL)  #For render
    
    ###Render Fundamental
    shiny_plot_fundamental_div <- eventReactive(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      plot_fundamental_div(file3_Weight, file5_Weight_Div, file1_Status)})
    observeEvent(list(input$subdiv,input$file4), 
                 {validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
                   output$plotw3Fdiv <- renderPlotly(shiny_plot_fundamental_div()[[1]])
                   output$plotw3Fshortdiv <- renderPlotly(shiny_plot_fundamental_div()[[3]])
                   output$plotw3Flongdiv <- renderPlotly(shiny_plot_fundamental_div()[[2]])
                   output$plotw3Ftabdiv <- DTtable(shiny_plot_fundamental_div()[[4]])
                 }) #For update plot
    
    
    
    
    
    ###Render Radar Plot (Dummy)
    output$radardiv <- renderPlotly(NULL)  #For render
    
    ###Render Risk Profiles Plot
    shiny_plot_radar_div <- eventReactive(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      plot_radar(risk(),riskdiv())})
    observeEvent(list(input$subdiv,input$file4), {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      output$radardiv <- renderPlotly(plot_radar_div(risk(),riskdiv()))}) #For update plot
    
    
    ###Render Different Table (Dummy)
    output$plotdiftable <- renderDT(NULL)  #For render
    
    ###Render Different Table (Dummy)
    output$plotdifboxplot <- renderPlotly(NULL)  #For render
    
    ###Render Different Table 
    shiny_diff_summary <- eventReactive(input$sub.ly.st5, {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      diff_summary(risk(),riskdiv(),shiny_plot_fundamental()[[4]],shiny_plot_fundamental_div()[[4]],as.numeric(input$ly.st5))})  
    observeEvent(input$sub.ly.st5, {
      validate(need((input$subdiv >= 1)||(!is.null(input$file4)),"data required to be uploaded"))
      output$plotdiftable <- DTtable(cbind(shiny_diff_summary()[[1]][,1],round(shiny_diff_summary()[[1]][,-1],6)))
      output$plotdifboxplot <- renderPlotly(shiny_diff_summary()[[2]])}) #For update plot
    
    
    # output$tablefile <- renderTable({
    #   File <- input$fileticker
    #   validate(
    #     need(File != "", "No data has been uploaded")
    #   )
    #   read.csv(File$datapath, header = TRUE, check.names = FALSE)
    # })
    
    
    ################################################################################
    ########################File 5 Optional#########################################
    
    output$div_dynamic <- DTtable(NULL)
    output$hist_return  <- DTtable(NULL)
    output$found  <- DTtable(NULL)
    
    observeEvent(input$file6, {
      File <- input$file6
      validate(
        need(File != "", "No data has been uploaded")
      )
      
      f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f6,c("Date","PORTNAME"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file6",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file6",
          text = "Missing the column of the names 'PORTNAME' or 'Date" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file6",
          text = "Missing Data Detected" )
      }else{
        file6_Div_Dynamic <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        output$div_dynamic <- DTtable(file6_Div_Dynamic)
      }
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$file7, {
      File <- input$file7
      validate(
        need(File != "", "No data has been uploaded")
      )
      #change name
      f7 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f7,c("Ticker","Status"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file7",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file7",
          text = "Missing the column of the names 'PORTNAME' or 'Date" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file7",
          text = "Missing Data Detected" )
      }else{
        file7_Attribute  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        output$found <- DTtable(file7_Attribute)
      }
    }, ignoreNULL = FALSE)
    
    observeEvent(input$file8, {
      File <- input$file8
      validate(
        need(File != "", "No data has been uploaded")
      )
      #change name
      
      f8 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f8,c("Date"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file8",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file8",
          text = "Missing the column of the name 'Date" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file8",
          text = "Missing Data Detected" )
      }else{
        file8_Return  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        output$hist_return  <- DTtable(round_tab(file8_Return))
      }
    }, ignoreNULL = FALSE)
    
    
    ###Render Different Table (Dummy)
    output$plotcom1 <- renderPlotly(NULL)  #For render
    
    
    ###Render Different Table 
    observeEvent(input$subcomp, updatePickerInput(session, "selcomcom", choices = colnames(file6_Div_Dynamic)[-c(1,ncol(file6_Div_Dynamic))])) #For update choice
    observeEvent(input$subcomp, output$plotcom1 <- renderPlotly(multicomp(file6_Div_Dynamic,as.character(input$selcomcom)))) #For update plot
    
    
    
    ###Render Different Table (Dummy)
    output$plotcom2 <- renderPlotly(NULL)  #For render
    output$plotcom3 <- renderPlotly(NULL)  #For render
    
    ###Render Different Table 
    shiny_plot_div_Sch_comp <- eventReactive(input$subcomp, plot_div_Sch_comp(file6_Div_Dynamic,file7_Attribute)) 
    observeEvent(input$subcomp, output$plotcom2 <- renderPlotly(shiny_plot_div_Sch_comp()[[1]])) #For update plot
    observeEvent(input$subcomp, output$plotcom3 <- renderPlotly(shiny_plot_div_Sch_comp()[[2]])) #For update plot
    
    
    
    ###Render Different Table (Dummy)
    output$plotcom4 <- renderPlotly(NULL)  #For render
    output$plotcom5 <- renderPlotly(NULL)  #For render
    output$plotcom6 <- renderPlotly(NULL)  #For render
    output$plotcom7 <- renderPlotly(NULL)  #For render
    output$plotcom8 <- renderPlotly(NULL)  #For render
    output$tab1 <- DTtable(NULL)
    output$tab2 <- DTtable(NULL)
    
    shiny_compareplot <- eventReactive(input$subcomp, compareplot(file8_Return, file6_Div_Dynamic,file7_Attribute)) 
    observeEvent(input$subcomp, output$plotcom4 <- renderPlotly(shiny_compareplot()[[3]])) #For update plot
    observeEvent(input$subcomp, output$plotcom5 <- renderPlotly(shiny_compareplot()[[4]]))
    observeEvent(input$subcomp, output$plotcom6 <- renderPlotly(shiny_compareplot()[[5]]))
    observeEvent(input$subcomp, output$plotcom7 <- renderPlotly(shiny_compareplot()[[6]]))
    observeEvent(input$subcomp, output$plotcom8 <- renderPlotly(shiny_compareplot()[[7]]))
    observeEvent(input$subcomp, output$tab1 <- DTtable(round_tab(shiny_compareplot()[[1]])))
    observeEvent(input$subcomp, output$tab2 <- DTtable(round_tab(shiny_compareplot()[[2]])))
    
    ####BoxPlot
    #output$tabben.op1 <- DTtable(NULL)
    #output$plotben.op1 <- renderPlotly(NULL)
    observeEvent(input$subcomp, updatePickerInput(session, inputId = 'selben.op1', choices = unique(file6_Div_Dynamic$PORTNAME)))
    shiny_diff_summary_mul <- eventReactive(input$submul, {diff_summary_mul(shiny_compareplot()[[1]],shiny_compareplot()[[2]],as.character(input$selben.op1), as.numeric(input$ly.opt1)) })
    observeEvent(input$submul, output$tabben.op1 <- DTtable(round_tab(shiny_diff_summary_mul()[[1]]))) #For update plot
    observeEvent(input$submul, output$plotben.op1 <- renderPlotly(shiny_diff_summary_mul()[[2]])) #For update plot
    
    ##############################Optional II#################################
    
    ####Input File
    #output$tab.op3 <- DTtable(NULL)
    
    observeEvent(input$file6.op2, {
      File <- input$file6.op2
      validate(
        need(File != "", "No data has been uploaded")
      )
      
      
      f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f6,c("Date","PORTNAME"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file6.op2",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file6.op2",
          text = "Missing the columns of the name 'Date' or 'PORTNAME'" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file6.op2",
          text = "Missing Data Detected" )
      }else{
        file6_Div_Dynamic <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        output$tab.op2 <- DTtable(round_tab(file6_Div_Dynamic))
      }
      
    }, ignoreNULL = FALSE)
    
    
    
    observeEvent(input$file8.op2, {
      File <- input$file8.op2
      validate(
        need(File != "", "No data has been uploaded")
      )
      #change name
      f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f6,c("Date"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file8.op2",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file8.op2",
          text = "Missing the column of the name 'Date" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file8.op2",
          text = "Missing Data Detected" )
      }else{
        file8_Return  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        output$tab.op2.2 <- DTtable(round_tab(file8_Return))
      }
      
    }, ignoreNULL = FALSE)
    
    
    ####
    shiny_clust.option <-  eventReactive(input$subclust, clust.option(file6_Div_Dynamic, file8_Return, as.numeric(input$tau.op2), as.numeric(input$sept.op2)))
    
    output$op2graph <- renderPlotly(NULL)
    output$op2hm <- renderPlotly(NULL)
    observeEvent(input$subclust, {
      output$op2graph <- renderPlotly(shiny_clust.option()[[3]])
      output$op2hm <- renderPlotly(shiny_clust.option()[[4]])
      output$op2tab1 <- renderDT(round_tab(shiny_clust.option()[[1]]))
      output$op2tab2 <- renderDT(round_tab(shiny_clust.option()[[2]]))
    }) #For update plot
    
    
    
    ##############################Optional III#################################
    
    ####Input File
    #output$tab.op3 <- DTtable(NULL)
    
    observeEvent(input$file6.op3, {
      File <- input$file6.op3
      validate(
        need(File != "", "No data has been uploaded")
      )
      
      f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f6,c("Date","PORTNAME"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file6.op3",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file6.op3",
          text = "Missing the columns of the name 'Date' or 'PORTNAME'" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file6.op3",
          text = "Missing Data Detected" )
      }else{
        file6_Div_Dynamic <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        output$tab.op3 <- DTtable(round_tab(file6_Div_Dynamic))
      }
      
      
      ####Update Picker
      updatePickerInput(session,inputId = "gdate1", choices = as.list(1:length(unique(file6_Div_Dynamic$Date))) %>% `names<-`(unique(file6_Div_Dynamic$Date)))
      updatePickerInput(session,inputId = "gdate2", choices = as.list(1:length(unique(file6_Div_Dynamic$Date))) %>% `names<-`(unique(file6_Div_Dynamic$Date)))
      updatePickerInput(session,inputId = "gdate3", choices = as.list(1:length(unique(file6_Div_Dynamic$Date))) %>% `names<-`(unique(file6_Div_Dynamic$Date)))
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$file7.op3, {
      File <- input$file7.op3
      validate(
        need(File != "", "No data has been uploaded")
      )
      
      f7 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f7,c("Ticker","Status"))
      
      if(cons[2]){
        showFeedbackWarning(
          inputId = "file7.op3",
          text = "Missing the columns of the name 'Ticker' or 'Status'" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file7.op3",
          text = "Missing Data Detected" )
      }else{
        file7_Attribute  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        output$tab.op3.1 <- DTtable(round_tab(file7_Attribute))
      }
      #change name
      
    }, ignoreNULL = FALSE)
    
    observeEvent(input$file8.op3, {
      File <- input$file8.op3
      validate(
        need(File != "", "No data has been uploaded")
      )
      f8 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      cons <- req.cons.file(f8,c("Date"))
      
      if(cons[1]){
        showFeedbackWarning(
          inputId = "file8.op3",
          text = "Require at least on column to be numeric")
      }else if(cons[2]){
        showFeedbackWarning(
          inputId = "file8.op3",
          text = "Missing the columns of the name 'Date'" )
      }else if(cons[3]){
        showFeedbackWarning(
          inputId = "file8.op3",
          text = "Missing Data Detected" )
      }else{
        file8_Return  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
        output$tab.op3.2 <- DTtable(round_tab(file8_Return))
      }
    }, ignoreNULL = FALSE)
    
    ####Plot Network
    #output$op3.1 <- renderPlot(NULL)
    #output$op3.2 <- renderPlot(NULL)
    #output$op3.3 <- renderPlot(NULL)
    shiny_get.graph.1 <-  eventReactive(input$subgdate1, get.graph(file6_Div_Dynamic, file8_Return, as.integer(input$gdate1), found = file7_Attribute))
    shiny_get.graph.2 <-  eventReactive(input$subgdate2, get.graph(file6_Div_Dynamic, file8_Return, as.integer(input$gdate2), found = file7_Attribute))
    shiny_get.graph.3 <-  eventReactive(input$subgdate3, get.graph(file6_Div_Dynamic, file8_Return, as.integer(input$gdate3), found = file7_Attribute))
    observeEvent(input$subgdate1, output$op3.1 <- renderPlot(shiny_get.graph.1())) #For update plot
    observeEvent(input$subgdate2, output$op3.2 <- renderPlot(shiny_get.graph.2())) #For update plot
    observeEvent(input$subgdate3, output$op3.3 <- renderPlot(shiny_get.graph.3())) #For update plot
    
  }
  
  shinyApp(ui, server, options = list(launch.browser = T))
}




