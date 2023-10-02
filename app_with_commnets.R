library(httr)
library(tidyverse)
library(rvest)
library(jsonlite)
library(plotly)
library(BatchGetSymbols)
library(shinydashboard)
library(shiny)
library(shinycustomloader)
library(shinydashboardPlus)
library(waiter)
library(shinyWidgets)
library(dashboardthemes)
library(quantmod)
library(PerformanceAnalytics)
library(reshape2)
library(viridis)
library(shinyjqui)
library(DT)
library(lubridate)
library(robustbase)
library(shinyBS) 
library(cluster)
library(huge)
library(qgraph)
library(magic)
library(shinyFeedback)
library(shinypop)
library(tmap)

#Note here that "shinypop" package need to be installed from Github
#remotes::install_github("dreamRs/shinypop", force =T)

######################################################################################################
########################################General Functions#############################################
######################################################################################################


# Initialize the function 'box' from the 'shinydashboard' package.
# Note: Several packages contain a 'box' function, so we are explicitly using the one from 'shinydashboard'.
box <- shinydashboard::box


# The `req.cons.file` function performs three different checks on the provided data frame `f`:
# 1. Whether it contains any numeric or integer type columns.
# 2. Whether it contains all the required column names specified in `req.colname`.
# 3. Whether it contains any NA values.
#
# Args:
#   f: A data frame that is being checked.
#   req.colname: A character vector of required column names.
#
# Returns:
#   A logical vector where each element represents the result of one of the three checks.
#   If all checks pass, it returns c(FALSE, FALSE, FALSE).
#
# Example Usage:
#   constraints <- req.cons.file(data_frame, required_column_names)
#
req.cons.file <- function(f,req.colname){
  con.num <- (sum(sapply(f, class) %in% c('interger', 'numeric')) == 0)
  con.colname <- (sum(req.colname %in% colnames(f)) != length(req.colname))
  con.na <- (sum(is.na(f)) != 0)
  return(c(con.num,con.colname,con.na))
}


# The `round_tab` function rounds all numeric and integer columns in a given data frame to 6 decimal places.
#
# Args:
#   x: A data frame whose numeric and integer columns need to be rounded.
#
# Returns:
#   The same data frame with all numeric and integer columns rounded to 6 decimal places.
#
# Example Usage:
#   rounded_table <- round_tab(original_table)
#
round_tab <- function(x){
  x[, sapply(x, class) %in% c('numeric','integer')] <- as.data.frame(apply(x[, sapply(x, class) %in% c('numeric','integer')],2,round,6), check.names = FALSE)
  return(x)
}


# Define the function 'everyother', which takes a vector 'x' as its input.
# The function works as follows:
# - If the length of 'x' is 16 or less, the function returns 'x' unchanged.
# - If the length of 'x' is more than 16, the function returns a subsequence of 'x', 
#   selecting elements at intervals of 'floor(length(x)/6)'.
# This function is useful when you want to reduce the number of elements in 'x' 
# for purposes such as visualization where displaying every element would be impractical.
everyother <- function(x){
  lx <- length(seq_along(x))
  if(lx <= 16){y <- x}else{y <- x[seq(1,lx,floor(lx/6))]}
  return(y)
}



# The `DTtable` function utilizes the DT package to render DataTables, 
# providing interactive tables with an array of features like search and pagination.
#
# Args:
#   tab: A data table object to be rendered.
#
# Returns:
#   An interactive table with the given tab data rendered via DT::renderDataTable.
#
# Example Usage:
#   interactive_table <- DTtable(data_table)
#
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


# The `sep_and_sum` function separates positive and negative values 
# in a given data frame and calculates the row sums of these separated values.
#
# Args:
#   data: A data frame containing numeric values to be separated and summed.
#
# Returns:
#   A new data frame with the Date, positive row sums, negative row sums, and PORTNAME from the original data.
#
# Example Usage:
#   separated_data <- sep_and_sum(original_data)
#
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


# The `multipywtoreturn` function calculates weighted returns for each rebalancing date in the input weights data frame,
# using corresponding return matrices, while accommodating for NA and zero values.
#
# Args:
#   w: A data frame representing weights, with ‘Date’ as one of the columns.
#   return_mat: A numeric matrix representing returns for different assets.
#
# Returns:
#   A list of weighted returns for each rebalancing date.
#
# Example Usage:
#   weighted_returns <- multipywtoreturn(weights_data, return_matrix)
#
multipywtoreturn <- function(w,return_mat){
  rebdate <- w$Date
  returnmat <- lapply(1:(length(rebdate)), function(i) {if(i < length(rebdate)){return_mat %>% dplyr::filter((Date >= rebdate[i]) & (Date < rebdate[i+1]))}else{return_mat %>% dplyr::filter(Date >= rebdate[i])}})
  returnmat <- returnmat %>% lapply(function(m) {m[is.na(m)] <- 0; ifelse(length(m[m == 0]) == 0, m <- m, m[m == 0] <- rnorm(length(m[m == 0]),0.0001,0.0001)); m})                                                                                    
  wm <- lapply(seq_along(rebdate), function(i) apply(returnmat[[i]][,-1],1, function(x) x*w[i,-1]) %>% bind_rows()) 
  names(wm) <- rebdate
  return(wm)
}


# The as.data.frame function call here is a workaround to ensure that data frames created are not subject to name 
# sanitization (conversion to syntactically valid names) which is typically done by the default as.data.frame method. 
# This is specifically important when column names are expected to be in a certain format, and any alteration to them 
# could result in errors or inaccuracies in subsequent operations or analyses.
#
as.data.frame <- function(x, ...) {
  base::as.data.frame(x, check.names = FALSE, ...)
}


######################################################################################################
########################################Functions for Step 1##########################################
######################################################################################################

# The 'key_detail' function takes a 'ticker' symbol as its input and constructs a URL to query Yahoo Finance for option data related to that 'ticker'.
# The 'ticker' symbol is converted to uppercase to ensure the correct URL format.
#
# The function uses 'tryCatch' to handle potential errors during the JSON request, returning 'NULL' if an error occurs.
# If the request is successful, the function extracts the 'quote' details from the 'optionChain' result and returns it as a list.
#
# Args:
#   ticker: A character string representing the ticker symbol of a stock.
#
# Returns:
#   A list containing the 'quote' details from the option chain result, or 'NULL' if an error occurs during the request.
key_detail <- function(ticker){
  url_req <- paste0("https://query1.finance.yahoo.com/v7/finance/options/",toupper(ticker))
  fin.res <- tryCatch(
    { res <- fromJSON(url_req)
    as.list(res$optionChain$result$quote)},
    error = function(cond){
      NULL  
    }
  )
  return(fin.res)
}


# The 'current_esg' function takes a stock 'ticker' symbol as its input and constructs a URL to query Yahoo Finance for the related ESG (Environmental, Social, and Governance) scores.
# The 'ticker' symbol is converted to uppercase to ensure the correct URL format.
#
# The function employs 'tryCatch' to manage potential errors during the JSON request, returning 'NULL' if any error occurs.
# If the request is successful, the function extracts the latest ('tail') environment, social, governance, and overall ESG scores from the 'esgChart' and returns them in a vector.
#
# Args:
#   ticker: A character string representing the ticker symbol of a stock.
#
# Returns:
#   A numeric vector containing the latest environmental, social, governance, and overall ESG scores, or 'NULL' if an error occurs during the request.
current_esg <- function(ticker){
  url_req <- paste0("https://query2.finance.yahoo.com/v1/finance/esgChart?symbol=",toupper(ticker))
  esgScores <- tryCatch(
    { res <- fromJSON(url_req)
    esgScores <- c(
      tail(res$esgChart$result$symbolSeries$environmentScore[[1]],1),
      tail(res$esgChart$result$symbolSeries$socialScore[[1]],1),
      tail(res$esgChart$result$symbolSeries$governanceScore[[1]],1),
      tail(res$esgChart$result$symbolSeries$esgScore[[1]],1)
    )},
    error = function(cond){NULL}
  )
  return(esgScores)
}


# The 'get_sector' function retrieves the sector information for a given 'ticker' from Yahoo Finance. 
# The ticker is converted to uppercase to ensure uniformity and accuracy in URL formation for data retrieval.
# The function reads the HTML of the Yahoo Finance profile page for the given 'ticker' and extracts the company name, sector, and subsector 
# using specific xpath queries to locate the required information on the webpage.
#
# Args:
#   ticker: A character string representing the ticker symbol of a stock.
#
# Returns:
#   A character vector containing:
#   - name: The name of the company.
#   - sector: The sector to which the company belongs.
#   - subsector: The subsector to which the company belongs.
#
# Example Usage:
#   apple_sector_info <- get_sector('AAPL')
#
# Note: 
#   This function relies on the structure of the Yahoo Finance webpage and may not work if the webpage's structure is altered.
get_sector <- function(ticker){
  url <- paste0("https://finance.yahoo.com/quote/",toupper(ticker),"/profile?p=",toupper(ticker))
  page <- read_html(url)
  return(
    c(name = page %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/h3') %>% html_text(),
      sector = page %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[2]') %>% html_text(),
      subsector = page %>% html_nodes(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[4]') %>% html_text()
    )
  )
}


# The 'get_stock_profile' function retrieves and compiles a profile for a given stock 'ticker'. 
# The ticker symbol is converted to uppercase to ensure uniformity and correct URL format for data retrieval.
# It uses various helper functions like 'key_detail', 'get_esg_summary', and 'get_sector' to obtain diverse information including company data, ESG scores, and sector details.
#
# The function performs several checks to handle cases where data is unavailable or NULL, and assigns default values accordingly.
# It collates retrieved information, including company name, ESG scores, market capitalization, and trailing PE, among others, into a vector 're'.
# Color codes are assigned based on the ESG risk level to provide a visual representation of risk.
#
# Args:
#   ticker: A character string representing the ticker symbol of a stock.
#
# Returns:
#   A character vector 're' containing consolidated stock profile information, or with 'Unavailable' and other default values for missing or error-prone data.
#
# Example of the returned vector 're':
#   re <- c(com_name, sector, subsector, esg, comp_data$marketCap, comp_data$trailingPE, comp_data$trailingAnnualDividendRate, market)
#
# 'esg' is a sub-vector containing ESG risk level, color representation, and the numeric ESG scores.
# The color is assigned based on risk level: 'red' for High, 'yellow' for Medium, 'green' for Low, and 'blue' for Negligible risk levels.
get_stock_profile <- function(ticker){
  re <- rep('-',14)
  ticker <- toupper(ticker)
  comp_data <- key_detail(ticker)
  esg_data <- get_esg_summary(ticker) 
  prof <-  get_sector(ticker) 
  comn <- comp_data$longName
  if(identical(comn, character(0))||(comn == "")||(is.na(comn))){re <- rep('-',14); re[1] <- 'Unavailable'; re[5] <- 'black'}else{
    if(is.null(prof)){
      sector <- NA
      subsector <- NA
    }else{
      sector <- prof[2]
      subsector <- prof[3]
    }
    market <- paste0(comp_data$exchange, " currency in ",comp_data$currency)
    esg <- rep("-",7)
    com_name <- paste0(comn," (", comp_data$symbol,")")
    esg[2] <- "black"
    if(!is.null(esg_data)){
      esg[3] <- as.numeric(esg_data$ESG)
      esg[4] <- as.numeric(esg_data$E)
      esg[5] <- as.numeric(esg_data$S)
      esg[6] <- as.numeric(esg_data$G)
      esg[1] <- esg_data$Lev
      esg[7] <- paste0(esg_data$Perc,"th percentile")
      if(esg[1] == "High"){esg[2] <- "red"}
      if(esg[1] == "Medium"){esg[2] <- "yellow"}
      if(esg[1] == "Low"){esg[2] <- "green"}
      if(esg[1] == "Negligible"){esg[2] <- "blue"}
    }else{esg <- rep("-",7)}
    re <- c(com_name,sector,subsector,esg,comp_data$marketCap,comp_data$trailingPE,comp_data$trailingAnnualDividendRate, market)
  }
  
  return(re)
}


# The 'get_data' function retrieves stock data for a specified 'ticker' symbol from Yahoo Finance.
# The ticker symbol is converted to uppercase to ensure the correct URL format.
# The 'getSymbols' function from quantmod package is used to obtain the stock data, 
# and it is then converted into a dataframe 'df' with appropriate column names.
#
# Args:
#   ticker: A character string representing the ticker symbol of a stock.
#
# Returns:
#   A dataframe 'df' containing historical stock data with the following columns:
#   - Date: The date of the stock data.
#   - Open: The opening price of the stock.
#   - High: The highest price of the stock during the trading session.
#   - Low: The lowest price of the stock during the trading session.
#   - Close: The closing price of the stock.
#   - Volume: The number of shares traded.
#   - Adjusted: The adjusted closing price of the stock.
#
# Example Usage:
#   apple_data <- get_data('AAPL')
get_data <-function(ticker){
  ticker <- toupper(ticker)
  tick_name <- getSymbols(ticker,src='yahoo')
  df <- data.frame(Date=index(get(tick_name)),coredata(get(tick_name)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  return(df)
}


# The 'get_esg_summary' function retrieves the Environmental, Social, and Governance (ESG) scores for a given 'ticker' from Yahoo Finance.
# The ticker is converted to uppercase to ensure the correct URL format for data retrieval.
# The function reads the HTML of the Yahoo Finance sustainability page for the given 'ticker' and extracts the ESG scores, their percentile, and risk level.
# Specific xpath queries are used to locate and extract the required information on the webpage.
# If an error occurs during the reading of the HTML page, the function returns an error message.
# Extracted scores and information are returned as a dataframe.
#
# Args:
#   ticker: A character string representing the ticker symbol of a stock.
#
# Returns:
#   A dataframe containing:
#   - ESG: Overall ESG score.
#   - E: Environmental score.
#   - S: Social score.
#   - G: Governance score.
#   - Perc: Percentile of the ESG score.
#   - Lev: ESG risk level.
#
# Example Usage:
#   apple_esg_summary <- get_esg_summary('AAPL')
#
# Note: 
#   This function relies on the structure of the Yahoo Finance sustainability webpage and may not work if the webpage's structure is altered.
get_esg_summary <- function(ticker) {
  url <- paste0("https://finance.yahoo.com/quote/", toupper(ticker), "/sustainability?p=", toupper(ticker))
  page <- tryCatch(read_html(url), error = function(e) return(NULL))
  if(is.null(page)) stop("Error reading the HTML page")
  extract_score <- function(xpath) {
    score <- page %>% 
      html_nodes(xpath=xpath) %>% 
      html_text() %>% 
      as.numeric()
    
    if(length(score) == 0 || is.na(score)) return(NA) 
    return(score)
  }
  esg.score <- extract_score('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[2]/div[1]')
  e.score <- extract_score('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[2]/div/div[2]/div[1]')
  s.score <- extract_score('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[3]/div/div[2]/div[1]')
  g.score <- extract_score('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[4]/div/div[2]/div[1]')
  perc <- page %>% 
    html_nodes(xpath='//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[2]/div[2]/span/span') %>% 
    html_text()
  perc <- as.numeric(gsub("\\D", "", perc))
  lev <- page %>% 
    html_nodes(xpath='//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[3]/div/span') %>% 
    html_text() 
  return(
    as.data.frame(list('ESG' = esg.score, 'E' = e.score, 'S' = s.score, 'G' = g.score, 'Perc' = perc, 'Lev' = lev))
  )
}



# The 'candlestick_plot' function creates a candlestick plot with Bollinger Bands and a bar plot for volume data for the given stock 'ticker'.
# The candlestick plot is used to represent open, high, low, close price data graphically and Bollinger Bands provide a relative definition of high and low prices.
# The ticker is converted to uppercase to ensure uniformity.
# Bollinger Bands are added to the dataset based on high, low, and close prices.
# Direction (increasing or decreasing) is determined based on closing and opening prices and added to the dataset.
# The candlestick plot, Bollinger Bands, Moving Average line, and Volume plot are layered sequentially using plotly syntax.
# Finally, layout adjustments and enhancements like legends, title, and rangeselectors are added.
#
# Args:
#   df: A dataframe containing, at minimum, Date, Open, High, Low, Close, and Volume columns for stock prices.
#   ticker: A character string representing the ticker symbol of a stock.
#
# Returns:
#   A plotly object containing layered candlestick plot, Bollinger Bands, Moving Average line, and Volume bar plot.
#
# Example Usage:
#   candlestick_plot(df = stock_df, ticker = 'AAPL')
#
# Note:
#   This function relies on the plotly and TTR packages for plotting and technical trading indicators respectively
candlestick_plot <- function(df,ticker){
  ticker <- toupper(ticker)
  bbands <- BBands(df[,c("High","Low","Close")])
  df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= df[1,1])
  for (i in 1:length(df[,1])) {
    if (df$Close[i] >= df$Open[i]) {
      df$direction[i] = 'Increasing'
    } else {
      df$direction[i] = 'Decreasing'
    }
  }
  fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                        open = ~Open, close = ~Close,
                        high = ~High, low = ~Low, name = ticker)
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
  fig2 <- df 
  fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = "Volume") 
  fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))
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
  fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
                 shareX = TRUE, titleY = TRUE)
  fig <- fig %>% layout(title = paste0(ticker," :",df[1,1]," - ",Sys.Date()),
                        xaxis = list(rangeselector = rs),
                        legend = list(orientation = 'h', x = 0.5, y = 1,
                                      xanchor = 'center', yref = 'paper',
                                      font = list(size = 10),
                                      bgcolor = 'transparent'))
  return(fig)
}


# The 'performance_plot' function is used to compute and plot the financial performance of a given asset over different time intervals.
# This function calculates various performance metrics such as Return, Volatility, Sharpe ratio, Maximum Drawdown (MDD), Sortino ratio, and Value at Risk (VaR) for the provided asset (‘ticker’) over the entire available time period as well as the last '1 week', '1 month', '3 months', and '1 year'.

# Args:
#   df: A dataframe containing the historical data of the asset. It must include a ‘Date’ column.
#   ticker: A string representing the ticker symbol of the asset to be analyzed.

# Returns:
#   A list containing two elements:
#     1. A dataframe displaying calculated performance metrics for the respective time periods.
#     2. A Plotly object (‘fig’) representing a color-coded plot displaying the calculated metrics over different time intervals.
#
# Details:
#   - The provided ‘df’ dataframe is first converted to an 'xts' object to facilitate time series analysis.
#   - The function calculates daily returns (‘Ret’) and subsequently computes the performance metrics.
#   - For plotting, the dataframe of performance metrics is melted and plotted using Plotly, where each metric is color-coded.
#   - The function returns both the calculated performance metrics and the corresponding plot.
#
# Example Usage:
#   performance_data_and_plot <- performance_plot(df, "AAPL")
#
# Note:
#   - The returned plot object can be visualized directly by calling it, and the performance dataframe can be accessed for numerical analysis.
#   - The ‘Time’ column in the returned dataframe indicates the time period over which the performance metrics are calculated.
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


######################################################################################################
########################################Functions for Step 2##########################################
######################################################################################################

# The 'plotstart' function calculates the average of numeric (and integer) columns in the input dataframe 'found' and 
# performs a comparative analysis before and after divestiture, with a focus on those rows where 'Status' is 'Invest'. 
# The function returns a dataframe showing the mean values of the numeric columns before divest, after divest, 
# and the relative change in percentage between the two states. 
# Additionally, the function calculates the total number of assets, the number of invested assets, and the relative proportion of invested assets 
# and appends these values to the resulting dataframe.
#
# Args:
#   found: A dataframe, which should contain at least one column named 'Status', and one or more numeric or integer columns.
#
# Returns:
#   A dataframe with the means of numeric and integer columns before and after divestiture, the relative change in percentage, and counts and proportion of invested assets.
#
# Example Usage:
#   plot_start_df <- plotstart(found_df)
#
# Note:
#   - It is expected that the input dataframe 'found' should have a column 'Status' with value 'Invest' indicating the rows to be considered for after divestiture calculations.
#   - The numeric and integer columns are dynamically identified based on the class of each column in the input dataframe.
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



# The 'auto_selection' function performs automatic asset selection based on a quantile criterion.
# It subsets the 'found' dataframe based on the 'fact' column and compares it against a quantile value 'q',
# derived from the 'crit' argument. Assets are then marked as 'Invest' or 'Divest' depending on whether they 
# meet the quantile criterion and the 'upper' argument.
#
# Args:
#   found: A dataframe containing asset information. Must contain a column 'Ticker' representing individual assets,
#          and should have the column represented by 'fact' argument to perform quantile analysis.
#   fact: A string representing the column name in 'found' dataframe on which quantile analysis is to be performed.
#   crit: A numeric value representing the quantile level to use in the analysis.
#   upper: A binary value (1 or 0); if 1, assets with 'fact' value above the quantile are marked as 'Invest', 
#          if 0, assets with 'fact' value below the quantile are marked as 'Invest'.
#
# Returns:
#   A dataframe 'foundup', which is the modified version of the input 'found' dataframe with an additional 
#   column 'Status', representing whether to 'Invest' or 'Divest' in each asset.
#
# Example Usage:
#   auto_selected_df <- auto_selection(found_df, 'fact_column', 0.75, 1)
#
# Note:
#   - The function will mark assets for investment or divestiture based on whether their 'fact' value is above or below the quantile value 'q', 
#     depending on the 'upper' argument.
#   - If the 'fact' column has NA values, they will be ignored in quantile calculation (na.rm = TRUE).
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


# `map_index` maps the countries associated with the selected index and returns the tickers related to the selected index.
#
# Args:
#   index_choice_input: A character string representing the chosen index.
#
# Returns:
#   A list containing a tm object (p) representing the map, and a vector (tack) containing the related tickers.
#
# Details:
#   - The function filters 'all.tick' data frame to find the countries and tickers associated with the chosen index.
#   - It then creates a map visualizing the identified countries using the 'tmap' package.
#   - Countries not associated with the index are represented in gray on the map.
#   - The identified tickers are returned as a separate object in the list.
#
# Example Usage:
#   mapped_index <- map_index("S&P500")
#   mapped_index$p # To display the created map.
#   mapped_index$tack # To retrieve the associated tickers.
#
# This function provides a visual representation of the geographical distribution of the chosen index and returns the relevant tickers, aiding in spatial and contextual understanding.
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


# `NA_heatmap` visualizes the presence and absence of NA values in the given data frame using a heatmap.
#
# Args:
#   df: Data frame whose NA values are to be visualized.
#
# Returns:
#   A plotly object representing the heatmap.
#
# Details:
#   - The function converts NA values to 0 and non-NA values to 1.
#   - It then uses the 'plot_ly' function to create a heatmap visualizing the presence and absence of NA values in the data frame.
#   - Cells representing NA values are colored differently from non-NA values for clear distinction.
#
# Example Usage:
#   na_heatmap_plot <- NA_heatmap(data_frame)
#   plotly::plot_ly(na_heatmap_plot) # To display the created interactive heatmap.
#
# This function aids in quickly identifying the pattern and distribution of missing values across different variables in a data frame.
NA_heatmap <- function(df){
  dfn <- df[,-1]
  dfn  <- dfn  %>%
    mutate_all(~ifelse(is.na(.), 0, 1))
  return(plot_ly(x = colnames(df[,-1]), y = df[,1], z = as.matrix(dfn), type = "heatmap",
                 colors = c("0" = "#440154FF", "1" = "#20A486FF")))
}


# `NA_handle` handles NA values in a given data frame based on the chosen method.
#
# Args:
#   df: Data frame containing the data.
#   method: Numeric value representing the method to handle NA values.
#           0 - Omit NA, 1 - Replace with 0, 2 - Linear Interpolation.
#
# Returns:
#   A cleaned data frame with handled NA values.
#
# Details:
#   - The function handles NA values based on the input method.
#   - If method is 0, it omits rows containing NA values.
#   - If method is 1, it replaces NA values with 0.
#   - If method is 2, it performs linear interpolation to estimate the NA values.
#
# Example Usage:
#   cleaned_data <- NA_handle(data_frame, method = 1)
#
# This function provides flexibility in handling missing values based on the requirement, ensuring data integrity for subsequent analysis.
NA_handle <- function(df, method){
  df <- column_to_rownames(df, "Date")
  if(method == 0){
    df_clean <- na.omit(df)
  }else if(method == 1) {
    df[is.na(df)] <- 0
    df_clean <- df
  }else{
    df_clean <- df
    for (col_name in names(df_clean)) {
      df_clean[[col_name]] <- na.approx(df[[col_name]], rule = 2)
    }
  }
  return(rownames_to_column(df_clean,'Date')) 
}



######################################################################################################
########################################Functions for Step 3##########################################
######################################################################################################

# The 'return_summary' function calculates the performance summary of various assets based on their adjusted return prices. It provides a snapshot of the performance metrics including Return, Volatility, Sharpe ratio, Maximum Drawdown (MDD), Sortino ratio, Cumulative Return, Value at Risk (VaR), and the number of days of data available for each asset.

# Args:
#   x: A dataframe with at least two columns: 'ticker' containing asset ticker symbols and 'ret.adjusted.prices' containing the adjusted return prices of the assets.

# Returns:
#   A dataframe containing the calculated performance summary for each asset in 'x'. The summary includes the Ticker symbol, average Return, Volatility, Sharpe ratio, MDD, Sortino ratio, Cumulative Return, VaR at 95%, the number of Days the data represents, and a placeholder column ‘Weight’.

# Details:
#   - The function first filters out rows with NA values in 'ret.adjusted.prices' and then groups the dataframe by 'ticker'.
#   - For each group (i.e., each asset), the function calculates the performance metrics mentioned above and summarizes them in a new dataframe.
#   - The calculated metrics are rounded off to six decimal places for precision and readability.
#   - An additional placeholder column 'Weight' is added to the resulting dataframe, which can be used later to assign asset weights if necessary.

# Example Usage:
#   asset_summary <- return_summary(asset_data)
#
# Note:
#   - It is essential to ensure that the input dataframe 'x' contains the required columns and that the 'ret.adjusted.prices' column is numeric.
#   - The resulting dataframe can be used for further analysis or visualization of asset performance and risk metrics.
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


# The 'return_summary_update' function calculates and summarizes various risk and return performance metrics for each asset in a given dataset, transforming the dataset as needed for further analysis. This function provides insights into each asset's performance, helping in effective decision-making in portfolio management and asset allocation.

# Args:
#   x: A dataframe with 'Date' as one of the columns and other columns representing different assets, containing their adjusted prices.

# Returns:
#   A dataframe containing a performance summary for each asset, with calculated metrics including average Return, Volatility, Sharpe ratio, Maximum Drawdown (MDD), Sortino ratio, Cumulative Return, Value at Risk (VaR), and the number of Days of data available for each asset.

# Details:
#   - The function begins by transforming the input dataframe 'x' using the 'melt' function to organize 'Date' and 'ticker' as identifier variables and 'ret.adjusted.prices' as the measured variable.
#   - Then, the function groups the melted dataframe by 'ticker' and computes various performance metrics for each asset.
#   - Subsequently, the function rounds off the calculated values to six decimal places for precision and creates an additional placeholder column named 'Weight'.
#   - This column can be used later for assigning weights to different assets based on investment strategies.

# Example Usage:
#   asset_summary_update <- return_summary_update(asset_data)
#
# Note:
#   - Ensure that the input dataframe 'x' has 'Date' as one of its columns and contains numerical values representing adjusted prices in other columns.
#   - The returned dataframe is suitable for further analysis, visualization, and decision-making in portfolio management and risk assessment.

# Working Mechanism:
#   1. Data Reshape: The function reshapes the input dataframe so that each row represents a unique Date-ticker combination, facilitating the calculation of performance metrics.
#   2. Metric Calculation: The function calculates several risk and return metrics for each asset, providing a comprehensive overview of asset performance.
#   3. Data Summarization: The resulting dataframe summarizes the performance metrics for each asset, serving as a base for further analysis and decision-making in investments.
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



# The 'boxplot_assets' function generates a comprehensive graphical representation of the return distribution for various assets. It creates a hybrid plot combining boxplot and violin plot features, offering a deeper insight into the return patterns and their dispersion.

# Args:
#   asset_return: A dataframe with 'Date' as one of the columns and other columns representing the return of different assets.
#   rankby: A string specifying the column name by which the assets should be ranked. E.g., "Return", "Volatility".

# Returns:
#   An interactive plotly figure with boxplots representing the interquartile range, median, and outliers of return for each asset and overlaid with a violin plot depicting the density of the return distribution, and points representing the mean return of each asset.

# Details:
#   - The function starts by melting the input dataframe 'asset_return' to get a long-format dataframe suitable for ggplot.
#   - It then creates an ordered factor for 'Ticker' based on the 'rankby' argument.
#   - The function employs ggplot to create a combined boxplot and violin plot for each asset with points representing the mean return.
#   - The resulting ggplot object is converted to an interactive plotly figure before being returned.

# Example Usage:
#   asset_boxplot <- boxplot_assets(asset_return, rankby="Return")

# Note:
#   - The 'asset_return' dataframe should have 'Date' as one of its columns, and it should be clean and correctly formatted.
#   - Users can interact with the resulting plotly figure to explore the return distribution details for each asset more thoroughly.

# Working Mechanism:
#   1. Data Transformation: The function transforms the input dataframe to a long format suitable for creating plots using ggplot.
#   2. Visualization Creation: The function creates a comprehensive graphical representation using boxplot, violin plot, and point plot to represent different statistical measures of asset returns.
#   3. Interactive Enhancement: By converting the ggplot object to a plotly figure, the function offers interactive capabilities allowing users to explore the graphical representation more thoroughly.
boxplot_assets <- function(asset_return, rankby){
  melt_asset <- melt(asset_return, id.var = "Date") %>% `colnames<-`(c("Date",'Ticker', 'Return'))
  returnsummary  <- return_summary_update(asset_return)
  ticker_arr <- as.vector((returnsummary %>% dplyr::arrange(get(rankby)))$Ticker)
  melt_asset$Ticker <- factor(melt_asset$Ticker, levels = ticker_arr)
  
  p <- ggplot(melt_asset, aes(x= Ticker, y=Return, fill=Ticker)) +
    geom_boxplot(alpha=0.7) +
    geom_violin(width=1.4, alpha=0.3) +
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



######################################################################################################
########################################Functions for Step 4##########################################
######################################################################################################


# The `box_constrain` function ensures that the absolute sum of negative weights (short positions) in a portfolio does not exceed a predefined limit ('Short_Limit'). This is often used in portfolio optimization processes where there are constraints on the short positions that can be taken in the portfolio.

# Args:
#   Weight: A numeric vector representing the weights of the assets in the portfolio. Negative values indicate short positions.
#   Short_Limit: A numeric value representing the maximum absolute sum of short positions allowed in the portfolio.

# Returns:
#   A numeric vector (`Weight_Box`), representing the adjusted weights of the assets in the portfolio, ensuring that the constraints on short positions are met.

# Details:
#   - The function calculates the absolute sum of the negative weights in the portfolio ('Crit').
#   - If 'Crit' exceeds the 'Short_Limit', the function proportionally scales down the short positions and scales up the long positions so that the overall constraint is satisfied.
#   - If 'Crit' is within the 'Short_Limit', the original weights are returned without any modification.

# Example Usage:
#   adjusted_weights <- box_constrain(original_weights, Short_Limit = 0.2)

# Note:
#   - This function is crucial when there are regulatory or risk management constraints on the amount of short positions a portfolio can hold.
#   - It is essential to ensure that 'Weight' is a properly formatted numeric vector, and 'Short_Limit' is a positive numeric value representing the limit on the absolute sum of short positions.

# Working Mechanism:
#   1. Constraint Check: The function begins by checking if the absolute sum of short positions ('Crit') exceeds the specified 'Short_Limit'.
#   2. Weight Adjustment: If 'Crit' is beyond the 'Short_Limit', the function recalibrates both short and long positions in the portfolio to comply with the constraint.
#   3. Return Adjusted Weights: The function then returns the adjusted weights ensuring that the portfolio abides by the short position constraints.
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


# The function aims to perform asset allocation based on the provided method type.
# It accepts asset return data, allocation type, and various other parameters
# and outputs a list containing asset weights, return matrices, covariance matrices, and rebalancing dates.

# Extract the initial, final, and rebalancing dates and calculate the return matrix for each rebalancing period.

# Calculate covariance and inverse covariance matrices of the asset returns for the rebalancing periods.
# Handle the case where the inverse covariance matrices are not computed correctly, recalculate using another method if needed.

# Depending on the type, calculate portfolio weights using different allocation strategies:
# type 0: Equally weighted portfolios.
# type 1: Portfolios based on the mean of asset returns.
# type 2: Minimum variance portfolios.
# type 3: Maximum Sharpe Ratio optimization.
# type 4: Portfolios based on the eigen decomposition of the covariance matrix.
# If a file with weights (filew) is provided, use it instead of calculating new weights.
# If lim != 0, adjust the weights using box_constrain function to handle short selling limits.
# Return a list containing asset weights, return matrices, covariance matrices, and rebalancing dates.
allocation <- function(x,type,tau,reb,lim = 0, filew){
  one <- as.matrix(rep(1,ncol(x)-1))
  first <- head(x[-(1:tau),],1)$Date
  last <- tail(x[-(1:tau),],1)$Date
  rebdate <- x[seq(reb,nrow(x),reb),]$Date
  returnmat <- lapply(seq_along(rebdate), function(i) {rowre <- which(x$Date == as.Date(rebdate[i])); x[(rowre-tau):(rowre-1),-1]}) %>%
    lapply(function(m) {m[is.na(m)] <- 0; m}) %>% lapply(function(m) {ifelse(length(m[m == 0]) == 0, m <- m,m[m == 0] <- rnorm(length(m[m == 0]),0.0001,0.0001)); m}) 
  
  covmat <- lapply(returnmat, function(y) cov(y))
  invcov <- lapply(covmat, function(y) {s <- as.matrix(solve(as.matrix(y),tol = 1E-1000)); s[lower.tri(s)] <- t(s)[lower.tri(s)]; s} ) 
  
  crit <- sapply(seq_along(covmat), function(i) sum(covmat[[i]]%*%invcov[[i]]))
  if(sum((crit >= (ncol(x)-1) - 1E-10)&&(crit <= (ncol(x)-1) + 1E-10)) != 1){
    covmat <- lapply(returnmat, function(y) {covr <- as.matrix(covOGK(y,sigmamu = s_mad)$cov); dimnames(covr) <- list(colnames(x)[-1],colnames(x)[-1]); covr})
    invcov <- lapply(covmat, function(y) {s <- as.matrix(solve(as.matrix(y),tol = 1E-1000)); s[lower.tri(s)] <- t(s)[lower.tri(s)]; s} )}
  
  
  if(is.null(filew)){
    switch(as.character(type),
           "0" = {w <- matrix(rep(1/ncol(x[,-1]),(ncol(x[,-1])*length(rebdate))),length(rebdate),ncol(x[,-1])) %>% `colnames<-`(colnames(x[,-1])) %>% as.data.frame(check.names = FALSE); covmat <- NULL},
           "1" = {w <- lapply(returnmat, function(y) {m <- apply(y,2,mean); m <- m+ifelse(min(m) >= 0,0,-min(m)+0.001); m/sum(m)}) %>% bind_rows() %>% as.data.frame(check.names = FALSE); covmat <- NULL},
           "2" = {w <- lapply(invcov, function(y) {numer <- t(one)%*%y;  numer/sum(numer)}) %>% lapply(as.data.frame, check.names = FALSE) %>% bind_rows()},
           "3" = {w <- sapply(seq_along(invcov), function(i) {mu <- matrix(apply(returnmat[[i]] ,2,mean)); numer <- t(mu)%*%invcov[[i]];
           numer/sum(numer)}) %>% t() %>% as.data.frame(check.names = FALSE) %>% `colnames<-`(colnames(x)[-1])},
           "4" = {w <- lapply(covmat, function(y) {ec <- eigen(y); as.matrix(apply(ec$vectors,2,function(n) n/sum(n)))%*%matrix(ec$values/sum(ec$values))}) %>%
             lapply(function(m) as.data.frame(t(m), check.names = FALSE)) %>% bind_rows() %>% `colnames<-`(colnames(x)[-1]) }
    )
    
    if(lim != 0){w <- apply(w,1,box_constrain, Short_Limit = lim) %>% t() %>% data.frame()}
    
    w <- cbind(rebdate,w) %>% as.data.frame(check.names = FALSE) %>% `colnames<-`(c('Date',colnames(w)))
  }else{
    w <- filew
    w[,2:ncol(w)] <- round(w[,2:ncol(w)],6)
  }
  
  return(list(w,returnmat,covmat,rebdate))
}


# The `sep_pos_neg` function is designed to separate the positive and negative values within a numeric vector, 'w', primarily used to distinguish between long (buy) and short (sell) positions within a portfolio of assets.

# Args:
#   w: A numeric vector representing the weights of the assets in the portfolio.

# Returns:
#   A list containing two numeric vectors:
#     - The first vector (`wp`) holds the original positive values of 'w', replacing negative values with 0.
#     - The second vector (`wn`) retains the original negative values of 'w', replacing positive values with 0.

# Details:
#   - This function is useful when dealing with portfolios having both long and short positions and there is a need to separately analyze or process the positive and negative components of the portfolio.
#   - By isolating positive and negative values, users can gain more nuanced insights into the portfolio’s composition and risk profile.

# Example Usage:
#   separated_weights <- sep_pos_neg(portfolio_weights)

# Note:
#   - Ensure that the input, 'w', is a well-structured numeric vector with valid numeric values representing asset weights.
#   - The output provides clear separation allowing for further nuanced analysis and processing of portfolio weights.

# Working Mechanism:
#   1. Positive Vector Creation: The function creates a copy of the input vector 'w' and replaces all negative values with 0, resulting in the 'wp' vector containing only positive values.
#   2. Negative Vector Creation: Another copy of 'w' is made, replacing all positive values with 0, producing the 'wn' vector containing only negative values.
#   3. Result Return: Finally, the function returns a list containing both 'wp' and 'wn' vectors, representing separated positive and negative values of the input vector.
sep_pos_neg <- function(w){
  wp <- w; wp[wp <= 0] <- 0
  wn <- w; wn[wn > 0] <- 0
  return(list(wp,wn))
}


# The `asset_weight_plot` function visualizes the temporal evolution of the investment weight of a selected asset within a portfolio. It produces an interactive plot representing the investment weight on different dates using both area and line geom, accentuating specific points with geom_point.

# Args:
#   w: A data frame containing at least two columns: 'Date' and the ticker symbol of the selected company ('selcom'). 'Date' represents the time point, and 'selcom' column represents the weight of the asset in the portfolio at that time.
#   selcom: A character string representing the ticker symbol of the selected company whose investment weight needs to be visualized.

# Returns:
#   An interactive plotly object (`fig`) representing the investment weight of the selected asset over time.

# Details:
#   - This function is crucial when tracking the investment weight of a specific asset in a portfolio over time, allowing for a clear and interactive visual representation of investment strategy and asset allocation.
#   - By focusing on one asset, users can get detailed insights into how the investment weight of that asset has changed, identifying trends, and analyzing investment decisions.

# Example Usage:
#   asset_plot <- asset_weight_plot(portfolio_weights, 'AAPL')

# Note:
#   - The input data frame 'w' should contain valid and properly formatted date and weight information.
#   - The function employs ggplot2 for initial plot creation and then converts it to a plotly object for interactivity.

# Working Mechanism:
#   1. Subset and Rename: The function subsets the data frame 'w' for the selected company and renames the columns appropriately for further processing.
#   2. Plot Creation: Using ggplot2, the function creates an area plot, layered with a line plot and point plot, each depicting the investment weight of the asset on different dates.
#   3. Interactive Conversion: The created ggplot object is then converted to an interactive plotly object, `fig`.
#   4. Result Return: The function returns the interactive plotly object `fig`, allowing users to interactively explore the investment weight of the selected asset over time.
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


# The `all_performance_measure` function calculates a series of performance measures for a given time series of returns, `xcol`, providing a comprehensive summary of the asset's or portfolio's risk and return profile. These measures can offer insights into the effectiveness of investment strategies and facilitate investment decision-making.

# Args:
#   xcol: A numeric vector representing the time series of returns for an asset or portfolio.

# Returns:
#   A data frame (`ftab`) containing the calculated performance measures, each as a column:
#     - Return: The average return of the time series.
#     - Volatility: The standard deviation of the time series representing risk.
#     - Sharpe: The Sharpe ratio, representing risk-adjusted return.
#     - MDD: The Maximum Drawdown, indicating the largest single drop from peak to bottom.
#     - Sortino: The Sortino ratio, focusing on downside risk.
#     - Return.cumulative: The cumulative return of the time series.
#     - VaR: The Value at Risk at 95% confidence level, using historical method.
#     - Days: The number of days (data points) in the time series.

# Details:
#   - This function is crucial for analyzing the performance of assets or portfolios, enabling investors and analysts to compare different investment options and strategies effectively.
#   - The variety of calculated measures provide a multifaceted view of performance, addressing different aspects of risk and return.

# Example Usage:
#   performance_summary <- all_performance_measure(asset_returns)

# Note:
#   - Ensure that `xcol` is a properly formatted numeric vector representing returns.
#   - This function relies on functions from the `PerformanceAnalytics` package such as `maxDrawdown` and `SortinoRatio`, so ensure this package is installed and loaded.

# Working Mechanism:
#   1. Calculation: For the given time series of returns, the function calculates several performance measures including average return, volatility, Sharpe ratio, maximum drawdown, Sortino ratio, cumulative return, and Value at Risk.
#   2. Compilation: The calculated measures are compiled into a single-row data frame (`ftab`) with appropriate column names.
#   3. Result Return: The function returns the compiled data frame, `ftab`, providing a succinct summary of the asset's or portfolio's performance.
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



# The `all_performance_table` function generates a comprehensive performance summary table for each rebalancing period and an overall summary. It combines the return matrix (`return_mat`) and the weight matrix (`wmat`) to calculate various performance measures for each period and then compiles them into a data frame.

# Args:
#   return_mat: A data frame or matrix where each column represents the time series of returns for different assets.
#   wmat: A weight matrix containing the weights assigned to each asset at different rebalancing dates.

# Returns:
#   A data frame (`pertab`) consisting of performance measures for each rebalancing period and an overall summary, with each row representing a different period (including 'Overall') and columns representing different performance measures.

# Details:
#   - This function is integral for portfolio analysis, offering a detailed view of how the portfolio has performed over different periods and overall, thus aiding in investment evaluation and decision-making.
#   - The generated table can serve as a ready reference for analysts and investors to understand portfolio behavior and make necessary adjustments.

# Example Usage:
#   performance_table <- all_performance_table(return_matrix, weight_matrix)

# Note:
#   - Properly formatted `return_mat` and `wmat` are crucial for accurate calculations.
#   - This function is dependent on the `all_performance_measure` and `multipywtoreturn` functions, so ensure they are defined and available in your environment.

# Working Mechanism:
#   1. Rebalancing Block Calculation: The function calculates the block of returns for each rebalancing period using `multipywtoreturn`, based on the input weight matrix (`wmat`) and return matrix (`return_mat`).
#   2. Performance Calculation: For each rebalancing block and the overall period, the function calculates various performance measures using `all_performance_measure`.
#   3. Table Compilation: The function compiles the calculated performance measures along with the corresponding rebalancing dates into a single data frame (`pertab`), which is then returned.
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


# The `plot_performance_table` function is designed to visually represent the portfolio's performance metrics over different rebalancing periods in a multi-facetted area plot. It facilitates easy and effective interpretation of how different risk and return measures have evolved over time.

# Args:
#   pertab: A data frame containing the portfolio's performance measures for each rebalancing period.

# Returns:
#   A Plotly object (`fig`), representing a multi-facetted area plot of different performance measures over time.

# Details:
#   - The function creates a series of facetted plots, each representing a different performance metric, helping in the comparative analysis of how each metric has varied over time.
#   - The performance data is melted to long format to facilitate ggplotting.
#   - The function uses ggplot2 and plotly for plotting, thus it is essential to have these packages installed and loaded.

# Example Usage:
#   performance_plot <- plot_performance_table(performance_table)

# Note:
#   - The input `pertab` must be a properly formatted data frame with performance measures and corresponding dates.
#   - The returned object is a Plotly figure, allowing for interactive exploration of the data.

# Working Mechanism:
#   1. Data Preparation: The function omits the last row of `pertab` and converts the 'Date' column to Date type and reshapes the data frame to a long format suitable for ggplotting.
#   2. Plotting: It creates facetted area plots using ggplot and then converts them to an interactive Plotly object (`fig`).
#   3. Return Plot: Finally, the function returns the created Plotly object, offering an interactive and detailed view of portfolio performance measures over different periods.
plot_performance_table <- function(pertab){
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
  
  
  return(fig)
}


# The `plot_radar` function provides a visual representation of the distribution of various risk and return measures of the portfolio over different rebalancing periods. It generates a series of boxplots, each representing a different performance measure, enabling easy and efficient comparative analysis of the portfolio's performance dynamics.

# Args:
#   pertab: A data frame containing various performance measures of the portfolio for different rebalancing periods.

# Returns:
#   A Plotly object (`fig`), depicting the boxplots of different performance measures, facilitating an interactive exploration of the portfolio's performance dynamics.

# Details:
#   - The function utilizes ggplot2 and plotly to create an interactive visualization of the distribution of various risk and return measures of the portfolio.
#   - It also includes violin plots and summary statistics for a comprehensive view.
#   - It is crucial to have the ggplot2 and plotly packages installed and loaded to use this function.

# Example Usage:
#   radar_plot <- plot_radar(performance_table)

# Note:
#   - The input, `pertab`, should be a properly structured data frame containing performance measures and corresponding dates.
#   - The returned object is a Plotly figure that allows for interactive and detailed exploration of the portfolio's risk and return measures.

# Working Mechanism:
#   1. Data Preparation: The function extracts relevant portions of the input data frame `pertab` and transforms the data to a long format suitable for ggplotting.
#   2. Plotting: The function then creates boxplots, violin plots, and summary statistics using ggplot2 and converts the plots to an interactive Plotly object (`fig`).
#   3. Return Plot: Finally, the Plotly object (`fig`) is returned, offering an interactive and in-depth view of the portfolio's risk and return measures over time.
plot_radar <- function(pertab){
  last_row <- pertab[nrow(pertab),-c(1,ncol(pertab))]
  tabnew <- pertab[-nrow(pertab),-ncol(pertab)]
  tabnewmelt <- melt(tabnew, id.vars = 'Date')
  
  p <- ggplot(tabnewmelt, aes(x= factor(0), y=value, fill=variable )) +
    geom_boxplot(alpha=0.7) +
    geom_violin(width=1.4, alpha=0.3) +
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


# The `ex_var_efficient` function computes the efficient frontier for a set of assets, a fundamental concept in portfolio theory, which represents the set of optimal portfolios that offer the highest expected return for a given level of risk.

# Args:
#   invc: A square symmetric matrix representing the inverse of the covariance matrix of asset returns.
#   mu: A numeric vector representing the expected returns of the assets.

# Returns:
#   A data frame (`meltplot`), where each row corresponds to a point on the efficient frontier, with columns for expected return (`exeff`) and standard deviation (`sdeff`).

# Details:
#   - The function calculates the expected return and variance for the Global Minimum Variance Portfolio (GMVP) and the Tangency Portfolio.
#   - It then computes the expected return and standard deviation for a range of portfolios along the efficient frontier.
#   - The efficient frontier is represented as a set of points, each with a unique combination of risk and return.

# Example Usage:
#   efficient_frontier <- ex_var_efficient(inverse_cov_matrix, expected_returns)

# Note:
#   - It is crucial that 'invc' is the inverse of a valid covariance matrix, and 'mu' is a properly formatted numeric vector representing expected returns.
#   - The returned object is a data frame that can be used to visualize the efficient frontier.

# Working Mechanism:
#   1. Parameter Calculation: The function calculates parameters like 'exgmv', 'vargmv', 'extan', 'vartan' and 'covgmvtan' which are required to compute the efficient frontier.
#   2. Portfolio Computation: Using the calculated parameters, the function computes the expected return and standard deviation for portfolios along the efficient frontier.
#   3. Return Results: The function returns a data frame representing the set of optimal portfolios along the efficient frontier.
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


# The `efficient_frontier` function computes the efficient frontier of portfolios based on historical return data for assets over specified rebalancing dates and given portfolio weights. The efficient frontier represents a set of portfolios that provide the highest expected return for a defined level of risk.

# Args:
#   x: A data frame representing historical return data with 'Date' column and asset return columns.
#   y: A data frame representing the portfolio weights at rebalancing dates with 'Date' column and weight columns for each asset.

# Returns:
#   A list containing two data frames:
#       - 'ef': The efficient frontier, where each row represents a point on the frontier with expected return, standard deviation, and corresponding 'Date'.
#       - 'wmat': Mean and standard deviation of portfolio returns at each rebalancing date.

# Details:
#   - The function utilizes the `ex_var_efficient` function to compute the efficient frontier for each sub-period between rebalancing dates.
#   - It constructs the covariance matrix and its inverse for each sub-period, based on the asset return data, and then calculates the efficient frontier using the expected return and covariance matrix.

# Example Usage:
#   results <- efficient_frontier(return_data, weight_data)

# Note:
#   - Ensure that 'x' and 'y' are properly formatted with 'Date' as a column, and have matching dates in order of appearance.
#   - It is crucial to have consistent and correct input data, as inaccuracies can lead to incorrect results in portfolio optimization.

# Working Mechanism:
#   1. Data Preparation: The function starts by determining the rebalancing dates and preparing the return data for each sub-period between them.
#   2. Portfolio Statistics Calculation: For each sub-period, it calculates the mean and standard deviation of portfolio returns based on given weights.
#   3. Covariance Matrix Construction: It constructs and inverts the covariance matrix for asset returns in each sub-period.
#   4. Efficient Frontier Calculation: Utilizing the `ex_var
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



# The `plot_efficient_frontier` function visualizes the efficient frontier and portfolio positions
# for selected dates. It provides a clear, concise, and interactive representation of risk-return trade-offs
# and aids in making informed investment decisions.

# Args:
#   ef: A data frame containing points on the efficient frontier with columns for expected return,
#       standard deviation, and date.
#   w: This parameter is not used within the function and may be considered for removal.
#   wmat: A data frame containing the expected return, standard deviation, and date for the portfolios
#         at the rebalance dates.
#   seldate: A character vector containing the selected dates for which the efficient frontier and the 
#            portfolios are to be plotted.

# Returns:
#   An interactive plotly object showcasing the efficient frontier along with the portfolios at the
#   selected dates. Efficient Frontier is plotted with points and portfolios are superimposed with 
#   a different shape.

# Details:
#   - The function first converts the ‘Date’ column of ‘wmat’ to character and subsets ‘ef’ and ‘wmat’
#     to include only the rows where ‘Date’ matches the selected dates `seldate`.
#   - It uses ggplot2 to create a scatter plot of the efficient frontier and overlays the portfolios 
#     on top, differentiating them by shape.
#   - Finally, it transforms the ggplot object to an interactive plotly object for better user interaction 
#     and visualization.

# Example Usage:
#   plot_obj = plot_efficient_frontier(ef, w, wmat, c('2023-01-01', '2023-02-01'))

# Note:
#   - The `seldate` parameter must contain valid dates present in the `ef` and `wmat` data frames.
#   - The `w` parameter is not utilized within the function and can be considered for removal in future iterations.

# Working Mechanism:
#   1. Data Subsetting: The function subsets the `ef` and `wmat` data frames based on the provided `seldate`.
#   2. Plot Construction: Constructs a ggplot object by plotting the efficient frontier points and superimposing 
#      portfolio positions with differentiated shapes.
#   3. Conversion to Interactive Plot: Transforms the static ggplot object into an interactive plotly object, 
#      allowing enhanced visualization and interaction.
plot_efficient_fontier <- function(ef,w,wmat,seldate){
  wmat$Date <- as.character(wmat$Date)
  wmatsub <- subset(wmat, Date %in% seldate) %>% `colnames<-`(c('exeff','sdeff','Date'))
  efsub <- subset(ef, Date %in% seldate) %>% `colnames<-`(c('exeff','sdeff','Date'))
  
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
  return(fig)
}


# The `plot_fundamental` function visualizes the impact of fundamental factors on portfolio positions
# over time, and assists in analyzing the exposure of the portfolio to different fundamental metrics.

# Args:
#   w: A data frame containing portfolio weights with 'Date' column and asset ticker columns.
#   found: A data frame containing fundamental data with 'Ticker' column and columns for different fundamental metrics.

# Returns:
#   A list containing four elements:
#     - The first element is an interactive plotly object representing the impact of each fundamental metric 
#       on portfolio positions for each date.
#     - The second and third elements are plotly objects representing the distribution of scores for long and short 
#       positions respectively, for each fundamental metric.
#     - The fourth element is a data frame consolidating the impact of each fundamental metric on portfolio positions, 
#       for each date, in long and short positions.

# Details:
#   - The function starts by subsetting the portfolio weight (w) and fundamental (found) data frames to include only 
#     common tickers. It then calculates the coverage for each numeric fundamental metric and divides the weights 
#     into positive (long) and negative (short) parts. It subsequently calculates the weighted average of each 
#     fundamental metric for long and short positions, for each date, and prepares the data for plotting.

# Example Usage:
#   result = plot_fundamental(w, found)

# Note:
#   - Ensure that the 'Ticker' column in the 'found' data frame and the asset ticker columns in the 'w' data frame 
#     have matching tickers.
#   - The function expects 'Date' to be a column in the 'w' data frame and 'Ticker' to be a column in the 'found' data frame.
#   - Ensure that 'found' data frame has fundamental data in numeric or integer format for accurate calculations.

# Working Mechanism:
#   1. Data Preparation: Subsets 'w' and 'found' to include only common tickers and calculates coverage for each 
#      numeric fundamental metric.
#   2. Calculation of Weighted Averages: Computes weighted averages of each fundamental metric for long and short 
#      positions, for each date.
#   3. Plot Construction: Constructs interactive plotly objects to represent the impact of each fundamental metric 
#      on portfolio positions and their distribution for long and short positions.
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
  poswa <- round(poswa,2) %>% `colnames<-`(as.character(w$Date)) %>% `rownames<-`(names(coverage_num))
  negwa <- NULL
  for(j in 1:ncol(num_factor)){
    negwa <- rbind(negwa,sapply(1:nrow(negw), function(i) {x <- num_factor[,j]; y <- negw[i,];
    x <- x[!is.na(x)]; y <- y[!is.na(x)]; sum(x*y)/sum(y)}))
  }
  negwa <- round(negwa,2) %>% `colnames<-`(as.character(w$Date)) %>% `rownames<-`(names(coverage_num))
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


# The `plot_portfolio_weight` function visualizes the portfolio weights along with 
# a selected fundamental metric on a specific date, providing insights into asset 
# allocation while considering the selected fundamental aspect. 

# Args:
#   w: A data frame representing portfolio weights, consisting of a 'Date' column 
#      and asset ticker columns.
#   found: A data frame representing the fundamental data, consisting of a 'Ticker' 
#      column and columns representing different fundamental metrics.
#   selcat: A string representing the selected fundamental metric (column name in 
#      'found' data frame) to be visualized.
#   seltime: A string representing the selected date (in 'w' data frame) for which 
#      the visualization should be created.

# Returns:
#   An interactive plotly object visualizing the portfolio weights and the 
#   selected fundamental metric for the chosen date.

# Details:
#   - The function starts by filtering the portfolio weight (w) data frame for 
#     the selected date and merges it with the fundamental (found) data frame 
#     to include the selected fundamental metric.
#   - It then creates a ggplot object and converts it to an interactive plotly 
#     object before returning it.

# Example Usage:
#   plot_object <- plot_portfolio_weight(w, found, 'PE_Ratio', '2023-01-01')

# Note:
#   - The 'Ticker' column in the 'found' data frame and the asset ticker columns 
#     in the 'w' data frame should have matching tickers.
#   - The 'Date' should be a column in the 'w' data frame, and 'Ticker' should be 
#     a column in the 'found' data frame.
#   - 'selcat' should be a valid column name in the 'found' data frame.
#   - 'seltime' should be a valid date in the 'w' data frame.

# Working Mechanism:
#   1. Data Preparation: Filters the 'w' data frame for the selected date and merges 
#      it with the 'found' data frame to include the selected fundamental metric.
#   2. Plot Construction: Constructs a ggplot object, which is then converted into 
#      an interactive plotly object, visualizing the portfolio weights and the selected 
#      fundamental metric for the chosen date.
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


######################################################################################################
########################################Functions for Step 5##########################################
######################################################################################################


# The `div_schedule` function generates a schedule based on specific types and parameters. 
# It is primarily designed to create different schedules depending on user requirements, 
# which may be useful in financial analysis or other scheduling needs.

# Args:
#   typesc: A character or numeric indicating the type of schedule, determining the calculation to be applied.
#   para: A vector of parameters used in schedule calculations.
#   w: A data frame that must include a 'Date' column, representing the range of dates for the schedule.
#   enddate: An optional character or Date object specifying the end date of the schedule.

# Returns:
#   A data frame with 'Date' and 'Bound' columns, where 'Date' corresponds to the dates from input data frame `w`,
#   and 'Bound' holds the calculated schedule values for each date.

# Details:
#   - If `typesc` is '0', a schedule consisting only of zeros is returned.
#   - If `typesc` is '1', a linear schedule is constructed using `para` and `enddate`, with 'Bound' values being zeros from `enddate` onward.
#   - If `typesc` is '2', a power-law-based schedule is formed, with 'Bound' values also being zeros from `enddate` onward.
#   - The function employs the `switch` statement to select the appropriate calculation method based on the `typesc` value.

# Example Usage:
#   div_schedule_df <- div_schedule("1", c(1), w, "2023-10-05")

# Note:
#   - Ensure that the 'Date' column in the 'w' data frame is in the correct Date format.
#   - Provide a recognizable date format or a Date object for the `enddate` parameter.
#   - Verify that `typesc` is one of the supported values ('0', '1', '2'), and the required `para` vector is appropriately supplied.

# Working Mechanism:
#   1. Date Conversion: Converts 'Date' in data frame `w` to Date type.
#   2. Schedule Computation: Depending on `typesc`, computes the 'Bound' values for each 'Date' in `w`.
#   3. Result Formation: Returns the formed data frame with 'Date' and corresponding 'Bound' values.
div_schedule <- function(typesc,para,w,enddate = NULL){
  Date <- as.Date(w$Date)
  switch(as.character(typesc),
         "0" = {sch <- rep(0,length(Date))},
         "1" = {indt <- which(Date == enddate); t <- (1:length(Date)); sch  <- para[1]*(t - indt); sch[indt:length(sch)] <- 0},
         "2" = {indt <- which(Date == enddate); t <- (1:length(Date)); sch  <- (t)^(-para[2]); sch[indt:length(sch)] <- 0}) 
  return(data.frame(Date,sch) %>% `colnames<-`(c('Date','Bound')))
}


# The `div_preview` function provides a visualization to preview divestment schedules over time 
# in relation to specified asset weights. The function is designed to offer insights into 
# the potential impact of divestments on asset positions.

# Args:
#   typesc: A character or numeric indicating the type of schedule, determining the calculation to be applied.
#   para: A vector of parameters used in schedule calculations.
#   enddate: An optional character or Date object specifying the end date of the schedule.
#   w: A data frame containing asset weights with 'Date' column and asset ticker columns.
#   found: A data frame containing asset information including 'Ticker' column and 'Status' column.

# Returns:
#   An interactive plotly object representing the sum of divestable weights over time with divestment schedule boundaries.

# Details:
#   - The function generates a schedule using `div_schedule` and computes the sum of divestable weights for 
#     long and short positions for each date.
#   - The resulting plot visualizes the sum of divestable weights along with the divestment schedule boundaries 
#     for a clearer understanding of divestment impacts.

# Example Usage:
#   preview_plot <- div_preview("1", c(1), "2023-10-05", w, found)

# Note:
#   - Ensure that the 'Date' column in the 'w' data frame is in the correct Date format, and 'Ticker' and 'Status' columns
#     are present in the 'found' data frame.
#   - Provide a recognizable date format or a Date object for the `enddate` parameter.
#   - Verify that `typesc` is one of the supported values ('0', '1', '2'), and the required `para` vector is appropriately supplied.

# Working Mechanism:
#   1. Schedule Generation: Calls `div_schedule` to generate a schedule based on `typesc`, `para`, and `enddate`.
#   2. Weight Aggregation: Computes the sum of divestable weights for long and short positions for each date.
#   3. Plot Construction: Constructs an interactive plotly object to visualize the sum of divestable weights and 
#      divestment schedule boundaries over time.
div_preview <- function(typesc, para, enddate = NULL, w, found){
  Schedule <- div_schedule(typesc,para,w,enddate)
  Bound <- unlist(Schedule$Bound,use.names =  FALSE)
  divasset <- (found %>% dplyr::filter(Status == "Divest"))$Ticker
  divw <- w[,divasset, drop = F]
  divestsum <- lapply(sep_pos_neg(divw),rowSums) %>% bind_cols() %>% as.data.frame(check.names = FALSE) %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short','Date')) %>% melt(id.vars = 'Date') %>% `colnames<-`(c('Date','Position','Weight'))
  divestsum$Bound <- c(Schedule$Bound,-(Schedule$Bound))
  
  p <- ggplot(divestsum, aes(x = Date)) + 
    geom_bar(data = subset(divestsum, Position == "Long"),
             aes(y = Weight, fill = Position), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    geom_bar(data = subset(divestsum, Position == "Short"),
             aes(y = Weight, fill = Position), stat = "identity", position = position_dodge(width = 0.4), color="black", width= 1.3) +
    #scale_fill_manual(values = scales::viridis_pal()(10)[c(3,9)]) +
    geom_hline(yintercept = 0,colour = "grey90") +
    geom_line(data = subset(divestsum, Position == "Long"),
              aes(y = Bound, group = 1), color="black", size = 1) +
    geom_line(data = subset(divestsum, Position == "Short"),
              aes(y = Bound, group = 1), color="black", size =  1) +
    labs(x = "Date",
         y = "Sum of Divestable Weights") +
    theme_bw() +
    #scale_x_discrete(breaks = everyother) +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="top")
  return(ggplotly(p))
}


# The `plot_indiv` function generates an interactive visual representation of asset weights over time, divided by status as "Invest" or "Divest".
# This function is crucial for visualizing and analyzing the temporal development of weights of individual assets in a portfolio.

# Args:
#   w: A data frame containing portfolio weights with 'Date' column and asset ticker columns.
#   found: A data frame containing asset information with 'Ticker' column and 'Status' column indicating whether to 'Invest' or 'Divest'.

# Returns:
#   An interactive plotly object showcasing the comparative weights of assets marked for investment and divestment over time.

# Details:
#   - The function segregates asset tickers based on their 'Status' from the 'found' data frame and calculates the sum of weights for 
#     long and short positions, for each date.
#   - A plotly bar graph is then constructed, showcasing the asset weights over time, and is facetted by 'Status' to distinguish between 
#     'Invest' and 'Divest' assets.

# Example Usage:
#   indiv_plot <- plot_indiv(w, found)

# Note:
#   - Ensure that the 'Ticker' and 'Status' columns are present in the 'found' data frame and the 'Date' and asset ticker columns in the 'w' data frame are correctly formatted.
#   - The function automatically identifies 'Invest' and 'Divest' statuses from the 'found' data frame, hence ensuring accurate 'Status' information is crucial.

# Working Mechanism:
#   1. Data Segregation: Identifies 'Invest' and 'Divest' assets from the 'found' data frame.
#   2. Weight Calculation: Computes the sum of weights for long and short positions for each 'Status' and 'Date'.
#   3. Plot Construction: Creates an interactive and facetted plotly object, visually representing the comparison of weights over time, segregated by 'Status'.
plot_indiv <- function(w,found){
  invasset <- (found %>% dplyr::filter(Status == "Invest"))$Ticker
  divsset <- (found %>% dplyr::filter(Status == "Divest"))$Ticker
  invw <- w[,invasset]
  divw <- w[,divsset]
  investsum <- lapply(sep_pos_neg(invw),rowSums) %>% bind_cols() %>% as.data.frame(check.names = FALSE) %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short')) %>% melt() %>% `colnames<-`(c('Date','Position','Weight'))
  divestsum <- lapply(sep_pos_neg(divw),rowSums) %>% bind_cols() %>% as.data.frame(check.names = FALSE) %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short')) %>% melt() %>% `colnames<-`(c('Date','Position','Weight'))
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


# The `div_weight` function is used to adjust and redistribute portfolio weights according to a specified schedule while considering investments and divestments, ensuring the portfolio adheres to specified constraints.

# Args:
#   w: A data frame representing the portfolio weights with a 'Date' column and asset ticker columns.
#   Schedule: A data frame representing the schedule containing the 'Bound' column which sets constraint levels for the weights.
#   found: A data frame containing asset information with 'Ticker' column and 'Status' column indicating whether to 'Invest' or 'Divest'.

# Returns:
#   A data frame containing adjusted weights for each asset for every date in the portfolio.

# Details:
#   - The function segregates assets into 'Invest' and 'Divest' based on the 'found' data frame.
#   - It adjusts the 'Divest' weights to ensure they do not exceed the 'Bound' constraints.
#   - Excess weights are then redistributed among the 'Invest' assets proportionally.
#   - If the divest sum does not exceed the bound, it returns the original weights for that date.

# Example Usage:
#   adjusted_weights <- div_weight(w, Schedule, found)

# Note:
#   - It is crucial that the input data frames 'w', 'Schedule', and 'found' are correctly formatted and contain the required columns.
#   - Ensure accurate 'Status' in the 'found' data frame as it dictates the weight adjustment process.

# Working Mechanism:
#   1. Asset Segregation: Identifies 'Invest' and 'Divest' assets from the 'found' data frame.
#   2. Weight Adjustment: Adjusts 'Divest' weights according to 'Bound' constraints and redistributes the excess weights among 'Invest' assets.
#   3. Weight Aggregation: Constructs the final data frame with adjusted weights for each date.

# This function is integral for managing portfolio weights, helping to maintain balance and adhere to investment strategies and constraints.
div_weight <- function(w,Schedule,found){
  Invest_List <- found %>% dplyr::filter(Status == 'Invest') %>% .$Ticker
  Divest_List <- found %>% dplyr::filter(Status == 'Divest') %>% .$Ticker
  Bound <- unlist(Schedule$Bound,use.names =  FALSE)
  final_frame <- c()
  for(k in 1:nrow(w)){
    Invest <- w[,Invest_List, drop = F][k,]
    Divest <- w[,Divest_List, drop = F][k, ,drop = F]
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
      if(length(Short_Divest_Scale) == 0){Short_Divest_Scale <- 0}
      List_of_Weight <- list(New_Invest,
                             Long_Divest_Scale, Short_Divest_Scale)
      Combine_Weight <- Filter(Negate(is.null), List_of_Weight) %>% 
        bind_cols()
      Weight_Divest <- Combine_Weight[,colnames(w)[-1]]
    }else{
      Weight_Divest <- w[k,-1, drop = F]
    }
    final_frame <- rbind(final_frame, Weight_Divest)
  }
  Date <- w$Date
  return(as.data.frame(cbind(Date,final_frame), check.names = FALSE))
}


# `plot_div_Sch_comp` function visualizes the sum of portfolio weights for assets that are marked for divestment and investment, respectively, across multiple portfolios.

# Args:
#   div_dynamic: A data frame containing portfolio weights for each asset and date, with a 'Date' column, asset ticker columns, and a 'PORTNAME' column indicating the name of the portfolio.
#   found: A data frame containing asset information with 'Ticker' column and 'Status' column indicating whether to 'Invest' or 'Divest'.

# Returns:
#   A list of two interactive plots created using ggplotly. 
#   The first plot visualizes the sum of divestment weights for each portfolio, and the second visualizes the sum of investment weights for each portfolio.

# Details:
#   - Assets are first segregated based on 'Invest' and 'Divest' statuses.
#   - Sum of weights is calculated for both positive (Long) and negative (Short) weights.
#   - Each plot uses bars to represent the sum of weights, with each bar's color corresponding to a different portfolio. 

# Example Usage:
#   plot_list <- plot_div_Sch_comp(div_dynamic, found)
#   plotly::subplot(plot_list[[1]], plot_list[[2]]) # To display both plots side by side using the 'subplot' function from the 'plotly' package.

# Note:
#   - The 'div_dynamic' and 'found' data frames should have the correct format.
#   - The function uses the `viridis` color palette for visualizing the different portfolios.

# This function offers a visual summary of how weights are distributed across portfolios for assets marked for divestment and investment, facilitating comparison and analysis of weight distribution.
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


# `asset_weight_plot_div` function visualizes the evolution of the weight of a selected asset in the portfolio, before and after divestment.

# Args:
#   w: A data frame containing the portfolio weights for each asset and date, with a 'Date' column and asset ticker columns, representing the portfolio before divestment.
#   w_div: A similar data frame as 'w', but representing the portfolio after divestment.
#   selcom: A character string representing the ticker of the asset whose weight evolution needs to be plotted.

# Returns:
#   An interactive plot created using ggplotly, displaying the evolution of the weight of the selected asset in the portfolio, both before and after divestment.

# Details:
#   - The function first extracts the relevant asset weights from the 'w' and 'w_div' data frames.
#   - Then, it combines these weights into a single data frame 'sel', with a new 'Status' column indicating 'Before-Divest' or 'Divest'.
#   - The plot is created using ggplot with filled area, lines, and points to represent the weight evolution, with different colors indicating the 'Status'.

# Example Usage:
#   asset_plot <- asset_weight_plot_div(w, w_div, 'AAPL')
#   plotly::plot_ly(asset_plot) # To display the created interactive plot.

# Note:
#   - The 'w', 'w_div', and 'selcom' should be provided with the correct format and values.
#   - The function uses the viridis color palette for differentiating between the 'Status'.

# This function helps in visually analyzing the impact of divestment on the weight of a selected asset in the portfolio over time, offering insights into how divestment actions have affected asset allocation.
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
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  fig <- ggplotly(p)
  return(fig)
}


# `plot_portfolio_weight_div` function visualizes portfolio weights for the assets at a selected date, both before and after divestment.

# Args:
#   w: A data frame containing the portfolio weights for each asset and date, with a 'Date' column and asset ticker columns, representing the portfolio before divestment.
#   w_div: A similar data frame as 'w', but representing the portfolio after divestment.
#   found: A data frame containing information about assets including their tickers and additional categorical/numerical information.
#   selcat: A character string specifying the column name in 'found' to be used for coloring the bars in the plot.
#   seltime: A character string representing the selected date to filter the portfolio weights for plotting.

# Returns:
#   An interactive plot created using ggplotly, displaying portfolio weights of assets at the selected date, colored by the selected category, both before and after divestment.

# Details:
#   - The function prepares the data for plotting by filtering and transforming the 'w' and 'w_div' data frames based on the selected date 'seltime'.
#   - The plots are created using ggplot with bars representing portfolio weights, colored by the selected category 'selcat', and differentiated by 'Status' (Before-Divest or Divest).

# Example Usage:
#   weight_plot_div <- plot_portfolio_weight_div(w, w_div, found, 'Sector', '2022-01-01')
#   plotly::plot_ly(weight_plot_div) # To display the created interactive plot.

# Note:
#   - The 'w', 'w_div', 'found', 'selcat', and 'seltime' should be provided with correct format and values.
#   - The function uses the viridis color palette for coloring the bars based on the selected category 'selcat'.

# This function aids in visually analyzing the allocation of assets in the portfolio at a selected date, and how it changes due to divestment, with the added context of additional asset information like Sector or Industry.
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
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  fig <- ggplotly(p) 
  return(fig)
}


# `plot_radar_div` visualizes the distribution of scores before and after divestment using boxplots and violin plots for each variable.

# Args:
#   pertab: Data frame containing scores for various variables before divestment.
#   pertabdiv: Data frame containing scores for various variables after divestment.

# Returns:
#   An interactive plot created using ggplotly, displaying boxplots and violin plots for each variable’s scores.

# Details:
#   - The function first melts the data frames to long format for easy plotting with ggplot.
#   - Boxplots, violin plots, and summary statistics are plotted for each variable in the dataset.
#   - Faceting is used to create individual plots for each variable.
#   - The viridis color palette is used for filling the plots.

# Example Usage:
#   radar_plot_div <- plot_radar_div(pertab, pertabdiv)
#   plotly::plot_ly(radar_plot_div) # To display the created interactive plot.

# This function provides a visual representation of how the distribution of scores for different variables changes due to divestment, allowing for easy comparison between the before and after states.
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

# `plot_performance_table_div` visualizes the evolution of the scores over time for each risk variable before and after divestment using area, line, and point plots.

# Args:
#   pertab: Data frame containing scores for various risks before divestment, with 'Date' as one of the columns.
#   pertabdiv: Data frame containing scores for various risks after divestment, with 'Date' as one of the columns.

# Returns:
#   An interactive plot created using ggplotly, displaying area, line, and point plots for each risk variable’s scores over time.

# Details:
#   - The function first merges the 'pertab' and 'pertabdiv' data frames for easy plotting with ggplot.
#   - Area, line, and point plots are plotted for each risk variable in the dataset.
#   - Faceting is used to create individual plots for each risk variable.
#   - The viridis color palette is used for filling the plots.

# Example Usage:
#   performance_plot_div <- plot_performance_table_div(pertab, pertabdiv)
#   plotly::plot_ly(performance_plot_div) # To display the created interactive plot.

# This function provides a visual representation of how the scores for different risk variables evolve over time, allowing for easy comparison between the portfolio's performance before and after divestment.
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


# `plot_fundamental_div` visualizes the distribution of scores before and after divestment using bar plots, box plots, and violin plots for each variable.
# 
# Args:
#   w: Data frame containing scores for various variables before divestment.
#   w_div: Data frame containing scores for various variables after divestment.
#   found: Data frame providing information about the common tickers between 'w' and 'w_div' data frames.
# 
# Returns:
#   A list containing ggplotly interactive plots (fig, figpos, figneg) and a data frame (tabfig) for detailed visual representation of scores.
# 
# Details:
#   - The function calculates the intersection of column names between 'w' and 'found$Ticker' to identify common tickers.
#   - It constructs new data frames ‘w’, ‘w_div’, and 'w_all' to compare scores before and after divestment.
#   - The function melts the constructed data frames to long format, allowing easy plotting with ggplot.
#   - The plots are created using ggplot and ggplotly to visualize the distribution of scores.
#   - The bar plots represent the distribution of positive and negative scores over different dates.
#   - Box plots and violin plots are created for detailed representation of scores distribution.
#   - Faceting is used to create individual plots for each variable with appropriate scaling.
#   - The viridis color palette is used for filling the plots.
#
# Example Usage:
#   fundamental_plot_div <- plot_fundamental_div(w, w_div, found)
#   plotly::plot_ly(fundamental_plot_div$fig) # To display the created interactive bar plot.
#   plotly::plot_ly(fundamental_plot_div$figpos) # To display the created interactive positive score box and violin plot.
#   plotly::plot_ly(fundamental_plot_div$figneg) # To display the created interactive negative score box and violin plot.
#
# This function provides a comprehensive visual representation of how the distribution of scores for different variables changes due to divestment, allowing for a detailed comparison between the before and after states.
plot_fundamental_div <- function(w,w_div,found){
  common <- intersect(colnames(w),found$Ticker)
  w <- w[,c('Date',common ), drop = F]
  w_div <- w_div[,c('Date',common), drop = F]
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
  poswa <- round(poswa,2) %>% `colnames<-`(as.character(rep(w$Date,2))) %>% `rownames<-`(names(coverage_num))
  meltpos <- rbind(cbind(melt(poswa[,1:nrow(w)]), 'Before-Divest') %>% `colnames<-`(c('Factor','Date','Score', 'Status')), cbind(melt(poswa[,(nrow(w)+1):(nrow(w_all))]),'Divest') %>% `colnames<-`(c('Factor','Date','Score', 'Status'))) 
  meltpos$Position <- 'Long'
  negwa <- NULL
  for(j in 1:ncol(num_factor)){
    negwa <- rbind(negwa,sapply(1:nrow(negw), function(i) {x <- num_factor[,j]; y <- negw[i,];
    x <- x[!is.na(x)]; y <- y[!is.na(x)]; sum(x*y)/sum(y)}))
  }
  negwa <- round(negwa,2) %>% `colnames<-`(as.character(rep(w$Date,2))) %>% `rownames<-`(names(coverage_num))
  meltneg <- rbind(cbind(melt(negwa[,1:nrow(w)]), 'Before-Divest') %>% `colnames<-`(c('Factor','Date','Score', 'Status')), cbind(melt(negwa[,(nrow(w)+1):(nrow(w_all))]),'Divest') %>% `colnames<-`(c('Factor','Date','Score', 'Status'))) 
  meltneg$Score <- gsub(NaN, 0, meltneg$Score)
  meltneg$Score <- as.numeric(meltneg$Score)
  meltneg$Position <- 'Short'
  meltplogt <- rbind(meltpos,meltneg)
  meltplogt$Status <- factor(meltplogt$Status, levels = c('Before-Divest','Divest'))
  p <- ggplot(meltplogt, aes(x = as.character(Date))) + 
    geom_bar(data = subset(meltplogt, Position == "Long"), 
             aes(y = Score, fill = Status), stat = "identity", position = position_dodge(), color="black") +
    geom_bar(data = subset(meltplogt, Position == "Short"), 
             aes(y = (-1)*Score, fill = Status), stat = "identity", position = position_dodge(), color="black") + 
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


# `diff_summary` visualizes the relative percentage change in given tables through boxplots and calculates the associated difference for each date and factor.
#
# Args:
#   pertab: Data frame containing the baseline table.
#   pertabdiv: Data frame containing the modified table.
#   tabfig: Another baseline table for comparison.
#   tabfigdiv: Corresponding modified table to tabfig for comparison.
#   ly: Numeric, limit for y-axis in the plot.
#
# Returns:
#   A list containing a data frame (tablechange) of percentage differences and a ggplotly object (fig) representing the visual plot.
#
# Details:
#   - The function computes relative percentage differences for comparable entities in provided tables.
#   - It generates a merged data frame ‘tablechange’ with all percentage differences.
#   - A boxplot is then constructed to visualize the distribution of percentage differences across dates and factors using ggplot and ggplotly.
#   - The viridis color palette is used for aesthetics in the plot.
#   - Mean of each factor is represented as a point in the boxplot.
#
# Example Usage:
#   diff_plot_summary <- diff_summary(pertab, pertabdiv, tabfig, tabfigdiv, ly)
#   plotly::plot_ly(diff_plot_summary$fig) # To display the created interactive box plot.
#
# This function offers an insightful visual representation of how different factors have changed relatively over given dates, aiding in quick comparative analysis.
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
    stat_summary(fun.y=mean, geom="point", shape= "*", size=0.8, color="white", fill="white") +
    scale_fill_viridis(discrete = TRUE) +
    coord_cartesian(clip = "off", ylim = ylim.set) +
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

# `plot_div_Sch` visualizes the sum of weights for long and short positions before and after divestment over specific dates using bar plots.
#
# Args:
#   w: Data frame containing scores for various variables before divestment.
#   found: Data frame providing information about the tickers found in 'w' data frame.
#   Schedule: Data frame containing schedule information with the Bound column.
#   div_w: Data frame containing scores for various variables after divestment.
#
# Returns:
#   A list containing two ggplotly interactive plots (fig1, fig2), representing the bar plots for divest and invest sums respectively.
#
# Details:
#   - The function calculates the sum of weights for specific positions (long/short) and statuses (before/divest) and visualizes them in bar plots.
#   - It creates two different plots, one for divest sums and one for invest sums.
#   - The plots are created using ggplot and ggplotly to visualize the distribution of weights.
#   - A line in the bar plot represents the bounds for long and short positions.
#   - The bars are colored based on the Status, and faceting is used to create individual plots for each date.
#
# Example Usage:
#   div_sch_plot <- plot_div_Sch(w, found, Schedule, div_w)
#   plotly::plot_ly(div_sch_plot$fig1) # To display the created interactive bar plot for divest sums.
#   plotly::plot_ly(div_sch_plot$fig2) # To display the created interactive bar plot for invest sums.
#
# This function provides a clear and comprehensive visual comparison of the weights' distribution before and after divestment, allowing for a detailed analysis of investment strategies over time.
plot_div_Sch <- function(w,found, Schedule, div_w){
  divasset <- (found %>% dplyr::filter(Status == "Divest"))$Ticker
  divw <- w[,divasset, drop = F]
  divestsum <- lapply(sep_pos_neg(divw),rowSums) %>% bind_cols() %>% as.data.frame(check.names = FALSE) %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short', 'Date')) %>% melt(id.vars = 'Date') %>% `colnames<-`(c('Date','Position','Weight'))
  divestsum$Status <- 'Before-Divest'
  divestsum$Bound <- c(Schedule$Bound,-(Schedule$Bound))
  invasset <- (found %>% dplyr::filter(Status == "Invest"))$Ticker
  invw <- w[,invasset]
  investsum <- lapply(sep_pos_neg(invw),rowSums) %>% bind_cols() %>% as.data.frame(check.names = FALSE) %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short','Date')) %>% melt(id.vars = "Date") %>% `colnames<-`(c('Date','Position','Weight'))
  investsum$Status <- 'Before-Divest'
  divwaf <- div_w[,divasset, drop = F]
  divestwafsum <- lapply(sep_pos_neg(divwaf),rowSums) %>% bind_cols() %>% as.data.frame(check.names = FALSE) %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short','Date')) %>% melt(id.vars = "Date") %>% `colnames<-`(c('Date','Position','Weight'))
  divestwafsum$Status <- 'Divest'
  invwaf <- div_w[,invasset, drop = F]
  investafsum <- lapply(sep_pos_neg(invwaf),rowSums) %>% bind_cols() %>% as.data.frame(check.names = FALSE) %>% cbind(w$Date) %>% `colnames<-`(c('Long','Short','Date')) %>% melt(id.vars = "Date") %>% `colnames<-`(c('Date','Position','Weight'))
  investafsum$Status <- 'Divest'
  melt_frame_div <- bind_rows(divestsum,divestwafsum)
  melt_frame_inv <- bind_rows(investsum,investafsum)
  melt_frame_div$Status <- factor(melt_frame_div$Status, levels = c('Before-Divest','Divest'))
  melt_frame_inv$Status <- factor(melt_frame_inv$Status, levels = c('Before-Divest','Divest'))
  p <- ggplot(melt_frame_div, aes(x = as.character(Date))) + 
    geom_bar(data = subset(melt_frame_div, Position == "Long"),
             aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(), color="black") +
    geom_bar(data = subset(melt_frame_div, Position == "Short"),
             aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(), color="black") +
    geom_hline(yintercept = 0,colour = "grey90") +
    geom_line(data = subset(divestsum, Position == "Long"),
              aes(y = Bound, group = 1), color="black", size = 0.7) +
    geom_line(data = subset(divestsum, Position == "Short"),
              aes(y = Bound, group = 1), color="black", size = 0.7) +
    labs(x = "Date",
         y = "Sum of Weights") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  fig1 <- ggplotly(p)
  p <- ggplot(melt_frame_inv %>% arrange(desc(Weight)), aes(x = Date)) + 
    geom_bar(data = subset(melt_frame_inv, Position == "Long"),
             aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(), color="black") +
    geom_bar(data = subset(melt_frame_inv, Position == "Short"),
             aes(y = Weight, fill =Status), stat = "identity", position = position_dodge(), color="black") +
    geom_hline(yintercept = 0,colour = "grey90") +
    labs(x = "Date",
         y = "Sum of Weights") +
    theme_bw() +
    theme(text = element_text(family = 'Fira Sans'),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  fig2 <- ggplotly(p)
  return(list(fig1,fig2))
}




######################################################################################################
########################################Functions for Option I########################################
######################################################################################################


# `multicomp` visualizes the weights of selected components in a portfolio over different dates using a bar plot.
#
# Args:
#   div_dynamic: A data frame containing the Date, selected component, and PORTNAME.
#   selcom: A string representing the selected component to be visualized.
#
# Returns:
#   A ggplotly object (fig) representing an interactive bar plot visualizing the weights of the selected components in a portfolio over different dates.
#
# Details:
#   - The function subsets the ‘div_dynamic’ data frame to include only Date, the selected component ‘selcom’, and PORTNAME.
#   - It then renames the columns to ‘Date’, ‘Weight’, and ‘Port’ respectively, and changes the ‘Port’ column to a factor variable with levels corresponding to the unique values of ‘Port’.
#   - A ggplot object is created, representing a bar plot of weights of the selected components in different portfolios over various dates.
#   - The ggplot object is then converted to a ggplotly object for interactive visualization.
#   - The plot’s aesthetics are enhanced by viridis color palette, and customization is applied to the x-axis text and overall theme.
#
# Example Usage:
#   multicomp_plot <- multicomp(div_dynamic, "selected_component")
#   plotly::plot_ly(multicomp_plot) # To display the created interactive bar plot.
#
# This function provides an interactive visual representation of how the weights of selected components in different portfolios change over time, aiding in the analysis of portfolio composition and strategy.
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


# `plot_div_Sch_comp` visualizes the sum of weights for both divested and invested assets over different dates using interactive bar plots.
#
# Args:
#   div_dynamic: A data frame containing the Date, Ticker symbols of assets, and PORTNAME.
#   found: A data frame providing information about the status of assets (whether they are divested or invested).
#
# Returns:
#   A list containing two ggplotly objects representing interactive bar plots for divested (fig1) and invested (fig2) assets.
#
# Details:
#   - The function filters out divested and invested assets from the ‘found’ data frame based on their ‘Status’.
#   - Separate data frames are created for divested and invested assets, where assets are grouped by ‘PORTNAME’ and the sum of weights is calculated for each group.
#   - ‘PORTNAME’ is converted to a factor variable with levels corresponding to the unique values in ‘div_dynamic$PORTNAME’.
#   - Two ggplot objects are created for divested and invested assets, representing the bar plots of the sum of weights over various dates, which are then converted to ggplotly objects for interactive visualization.
#   - The aesthetics of the plots are enhanced with the viridis color palette and customization is applied to the x-axis text and overall theme.
#
# Example Usage:
#   div_sch_comp_plots <- plot_div_Sch_comp(div_dynamic, found)
#   plotly::plot_ly(div_sch_comp_plots[[1]]) # To display the created interactive bar plot for divested assets.
#   plotly::plot_ly(div_sch_comp_plots[[2]]) # To display the created interactive bar plot for invested assets.
#
# This function provides comprehensive and interactive visual representations, enabling detailed analysis of the investment and divestment strategy’s impact on asset weights over time.
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



# `compareplot` function visualizes various facets of portfolio performance and composition over time.
# It generates several interactive plots, depicting aspects like historical returns, risk, 
# portfolio weights, and investment in various factors, providing insights into the portfolio’s dynamics.
#
# Args:
#   hist_return: A data frame containing historical returns information.
#   div_dynamic: A data frame with portfolio composition, including assets and their respective weights, over different time periods.
#   found: A data frame with additional relevant data and information related to the assets.
#
# Returns:
#   A list containing seven objects:
#     - pertab: A data frame consolidating the performance statistics of the portfolios.
#     - tabfn: A wide-format data frame presenting the Scores related to different factors.
#     - fig1, fig2, fig, figpos, figneg: ggplotly objects representing different aspects of portfolio dynamics as interactive plots.
#
# Details:
#   - The function segregates portfolio data from `div_dynamic` into different portfolios.
#   - It calculates performance metrics, weighted scores, and other relevant statistics for each portfolio and asset.
#   - The function transforms the data to long format for compatibility with ggplot2 and ggplotly.
#   - Multiple ggplot objects are created to represent different perspectives on portfolio dynamics.
#   - Aesthetic and theme customizations are applied to each plot for better visualization.
#   - The function wraps ggplot objects with ggplotly to make the plots interactive.
#
# Example Usage:
#   compareplot_results <- compareplot(hist_return, div_dynamic, found)
#   plotly::plot_ly(compareplot_results$fig1)  # To display the first created interactive plot.
#   plotly::plot_ly(compareplot_results$fig2)  # To display the second created interactive plot.
#   ...
#
# This function provides an extensive, multifaceted, and interactive visualization experience,
# enabling a detailed and comprehensive analysis of portfolio performance and composition over time.
compareplot <- function(hist_return,div_dynamic, found){
  list_port <- div_dynamic %>%  group_split(PORTNAME)
  nameport <- unique(div_dynamic$PORTNAME)
  pertab <- bind_rows(lapply(seq_along(list_port), function(i) all_performance_table(hist_return,list_port[[i]][,-ncol(list_port[[i]])])))
  pertab$Port <- factor(rep(nameport,unlist(lapply(list_port, function(x) nrow(x) + 1))), levels = nameport)
  pertabm <- melt(pertab) %>% filter(Date != 'Overall')
  p1 <- ggplot(pertabm, aes(x= Port, y=value, fill=variable, group = Port)) +
    geom_boxplot(alpha=0.7) +
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


# `diff_summary_mul` Function to compute and visualize the relative differences in performance
# and attributes between multiple portfolios and benchmarks. 
# It constructs detailed visualizations, outlining percentage of relative changes in different portfolios against the benchmark.
#
# Args:
#   pertab: A data frame containing performance-related information of the portfolios.
#   tabfn: A data frame containing attributes of the portfolios.
#   ben.port: A vector of portfolios considered as benchmarks.
#   ly: A parameter specifying the y-limit for the plots, may be NULL.
#
# Returns:
#   A list containing two objects:
#     - A data frame consolidating the relative differences in performance and attributes of the portfolios.
#     - A ggplotly object representing the relative changes in different facets of the portfolios.
#
# Details:
#   - The function prepares the input data, grouping it by Portfolios.
#   - It calculates the relative differences in performance and attributes for each portfolio compared to the benchmark.
#   - The function creates a box plot using ggplot2 and ggplotly, showing the relative changes in the portfolios, enabling users to discern the variances quickly.
#   - Aesthetic and theme customizations are applied to the plot for enhanced visualization.
#
# Example Usage:
#   diff_summary_results <- diff_summary_mul(pertab, tabfn, ben.port, ly)
#   plotly::plot_ly(diff_summary_results$fig)  # To display the created interactive plot.
#
# This function is highly valuable for gaining insights into how various portfolios are diverging or converging with the benchmarks over time, highlighting areas requiring attention or improvement.
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
      #perf
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




######################################################################################################
########################################Functions for Option II#######################################
######################################################################################################



# The `pmr` function calculates the moving average of portfolio returns over a specified time window (tau).
#
# Args:
#   portreturn: A numeric vector containing portfolio returns.
#   tau: A numeric value representing the time window over which the moving average is calculated.
#
# Returns:
#   A numeric vector containing the moving average of portfolio returns.
#
# Example Usage:
#   pmr_results <- pmr(portreturn, tau)
pmr <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else mean(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}


# The `pmrcum` function calculates the cumulative sum of portfolio returns over a specified time window (tau).
#
# Args:
#   portreturn: A numeric vector containing portfolio returns.
#   tau: A numeric value representing the time window over which the cumulative sum is calculated.
#
# Returns:
#   A numeric vector containing the cumulative sum of portfolio returns.
#
# Example Usage:
#   pmrcum_results <- pmrcum(portreturn, tau)
#
pmrcum <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else sum(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}


# The `prisk` function calculates the standard deviation (risk) of portfolio returns over a specified time window (tau).
#
# Args:
#   portreturn: A numeric vector containing portfolio returns.
#   tau: A numeric value representing the time window over which the standard deviation is calculated.
#
# Returns:
#   A numeric vector containing the standard deviation of portfolio returns.
#
# Example Usage:
#   prisk_results <- prisk(portreturn, tau)
prisk <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else sd(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}


# The `sharpe` function calculates the Sharpe ratio of portfolio returns over a specified time window (tau).
#
# Args:
#   portreturn: A numeric vector containing portfolio returns.
#   tau: A numeric value representing the time window over which the Sharpe ratio is calculated.
#
# Returns:
#   A numeric vector containing the Sharpe ratio of portfolio returns.
#
# Example Usage:
#   sharpe_results <- sharpe(portreturn, tau)
sharpe <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else (mean(portreturn[i:(i-tau+1)])-0)/sd(portreturn[i:(i-tau+1)]))
  return(x[!is.na(x)])
}


# The `pVaR` function calculates the Value at Risk (VaR) of portfolio returns over a specified time window (tau).
#
# Args:
#   portreturn: A numeric vector containing portfolio returns.
#   tau: A numeric value representing the time window over which VaR is calculated.
#   conf: A numeric value representing the confidence level for VaR calculation.
#
# Returns:
#   A numeric vector containing the Value at Risk of portfolio returns.
#
# Example Usage:
#   pVaR_results <- pVaR(portreturn, tau, conf)
#
pVaR <- function(portreturn,tau,conf){
  x <- sapply(seq_along(portreturn),function(i) if (i < tau) NA else quantile(portreturn[i:(i-tau+1)], conf))
  return(x[!is.na(x)])
}


# The `MDD` function calculates the Maximum Drawdown of portfolio returns over a specified time window (tau).
#
# Args:
#   portreturn: A numeric vector containing portfolio returns.
#   tau: A numeric value representing the time window over which Maximum Drawdown is calculated.
#
# Returns:
#   A numeric vector containing the Maximum Drawdown of portfolio returns.
#
# Example Usage:
#   MDD_results <- MDD(portreturn, tau)
#
MDD <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else (min(portreturn[i:(i-tau+1)])-max(portreturn[i:(i-tau+1)]))/(max(portreturn[i:(i-tau+1)])))
  return(x[!is.na(x)])
}


# The `SR` function calculates the Sortino ratio of portfolio returns over a specified time window (tau).
#
# Args:
#   portreturn: A numeric vector containing portfolio returns.
#   tau: A numeric value representing the time window over which the Sortino ratio is calculated.
#
# Returns:
#   A numeric vector containing the Sortino ratio of portfolio returns.
#
# Example Usage:
#   SR_results <- SR(portreturn, tau)
#
SR <- function(portreturn,tau){
  x <- sapply(seq_along(portreturn), function(i) if (i < tau) NA else (mean(portreturn[i:(i-tau+1)]) - 0)/sqrt(((1/(tau-1))*sum((portreturn[i:(i-tau+1)]-mean(portreturn[i:(i-tau+1)]))^2*((portreturn[i:(i-tau+1)]- 0) < 0)))))
  x <- x[!is.na(x)]
  x[abs(x) > 5] <- NA
  return(x[!is.na(x)])
}

# The `agg.perf` function aggregates various performance metrics for portfolio returns over a specified time window (tau).
#
# Args:
#   Port_Return: A numeric vector representing portfolio returns.
#   tau: A numeric value specifying the time window over which metrics are aggregated.
#   date: A vector of dates corresponding to the portfolio returns.
#
# Returns:
#   A data frame containing aggregated performance metrics including Return, Cumulative Return, Volatility, Sharpe, VaR, Max Drawdown, and Sortino, each associated with a specific date.
#
# Example Usage:
#   agg_perf_results <- agg.perf(Port_Return, tau, date)
#
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


# The `Annual_Best_Cluster` function identifies the optimal cluster each year based on Silhouette width, for given data (X), and a specified separation interval (sept).
#
# Args:
#   X: A data frame containing numeric values representing different variables.
#   sept: A numeric value specifying the separation interval to categorize data into different years.
#
# Returns:
#   A data frame containing the best cluster for each year, based on the maximum Silhouette width, along with Silhouette width values for each cluster.
#
# Example Usage:
#   annual_best_cluster_results <- Annual_Best_Cluster(X, sept)
#
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
  Final_Tab <- as.data.frame(Final_Tab, check.names = FALSE) %>%'rownames<-'(ben.date) %>%'colnames<-'(1:(ncol(X)-2))
  Final_Tab[Final_Tab == "NULL"] <- 0
  Max_Sil <- apply(Final_Tab,1,function(x) max(unlist(x)))
  Final_Tab$Best <- names(Final_Tab)[apply(Final_Tab, 1, function(i) which(i %in% Max_Sil)[1])]
  return(Final_Tab)
}


# The `Matrix_Cluster` function generates a data frame containing cluster assignments for given data (X), based on the best cluster of each year identified by the `Annual_Best_Cluster` function, and adds a performance column to it.
#
# Args:
#   X: A data frame containing numeric values representing different variables.
#   Perf: A numeric vector representing performance values corresponding to the data in X.
#   Best_Cluster_Year: A data frame containing the best cluster for each year as identified by the `Annual_Best_Cluster` function.
#   sept: A numeric value specifying the separation interval to categorize data into different years.
#
# Returns:
#   A data frame containing date, variable, value, and Performance, where 'variable' represents the variable name, and 'value' represents the cluster assignment.
#
# Example Usage:
#   matrix_cluster_results <- Matrix_Cluster(X, Perf, Best_Cluster_Year, sept)
#
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
  meltmap <- as.data.frame(Final_Tab, check.names = FALSE) %>% rownames_to_column('Date') %>% melt(id.vars='Date')
  meltmap$Performance <- Perf 
  return(meltmap)
}


# The `plot.hm` function visualizes matrix clusters and associated performance metrics, utilizing ggplotly for a responsive and interactive experience.
#
# Args:
#   each.perf: A list containing data frames representing different performance metrics.
#   best.clus: A list containing data frames representing the best clusters for each performance metric.
#   sept: A numeric value indicating the separation interval.
#
# Returns:
#   A ggplotly object showing matrix clusters in a heatmap format, faceted by performance metrics.
#
# Example Usage:
#   heatmap_plot <- plot.hm(each.perf, best.clus, sept)
#
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


# The `clust.option` function generates and visualizes a variety of cluster options for the given portfolio, along with associated performance metrics, providing insightful aggregations and visual representations of risk profiles over time.
#
# Args:
#   Div_Dynamic: A data frame containing dynamic diversification information.
#   Return: A data frame containing return values.
#   tau: A numeric value representing the time window over which metrics are aggregated.
#   sept: A numeric value representing the separation interval.
#
# Returns:
#   A list containing aggregated performance data frames, best cluster information, and ggplotly objects for visual representation of cluster options and risk profiles.
#
# Example Usage:
#   cluster_option_results <- clust.option(Div_Dynamic, Return, tau, sept)
#
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




######################################################################################################
########################################Functions for Option III######################################
######################################################################################################



# The `structure.cov` function calculates the covariance structure of the given return matrix, applying graphical LASSO for sparse inverse covariance estimation. It handles the case where columns sum to zero, providing an adjusted covariance matrix.
#
# Args:
#   return_mat: A numeric matrix containing return values for different assets.
#
# Returns:
#   A numeric matrix representing the adjusted covariance matrix after applying graphical LASSO.
#
# Example Usage:
#   adjusted_cov_matrix <- structure.cov(return_mat)
#
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


# The `plot.qgraph` function creates and visualizes a network graph from the provided adjacency covariance matrix using the `qgraph` package, allowing to display financial networks with customizable color coding based on investment status.
#
# Args:
#   adj.cov: A numeric matrix representing the adjusted covariance matrix.
#   title: A character string specifying the title of the graph.
#   found: An optional data frame containing information on the investment status of tickers (default is NULL).
#
# Returns:
#   A `qgraph` object representing the network graph visualization of the adjacency covariance matrix.
#
# Example Usage:
#   graph <- plot.qgraph(adj.cov, "Financial Network", found)
#
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


# The `get.graph` function calculates the portfolio weights, applies graphical LASSO on each portfolio to estimate sparse inverse covariance, and generates a series of network graphs to visualize financial relationships and investment statuses among various portfolios.
#
# Args:
#   div_dynamic: A data frame containing dynamic diversification information.
#   return_mat: A numeric matrix containing return values for different assets.
#   data.index: A numeric value representing the index of data to be selected.
#   found: An optional data frame containing information on the investment status of tickers (default is NULL).
#
# Returns:
#   A sequence of `qgraph` objects, each representing a network graph visualization for different portfolios.
#
# Example Usage:
#   portfolio_graphs <- get.graph(div_dynamic, return_mat, data.index, found)
#
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





######################################################################################################
########################################Global Variables##############################################
######################################################################################################



#world map including for the global indices
data(World) 
all.tick <- read_csv("https://github.com/QuantFILab/Divfolio/blob/main/Materials/update_suggest.csv?raw=true")
tidy.tick <- all.tick %>% select(Country, Index) %>% distinct(Country, Index, .keep_all = TRUE) %>% mutate(paste0(Index, " (",Country,")")) %>% `colnames<-`(c("Country", "Index", "CwithI"))
index_choice <- as.list(tidy.tick$Index) %>% `names<-` (c(tidy.tick$CwithI))

#setting empty variables
main_port <- c()
div_stock <- NULL
inv_stock <- NULL
rebalancedate <- NULL
ticker_rec <- "AAP"

#Generating Carbon Footprint Table
Carbon <- c(29884383.57/21, 16488511.67/15, 6951694.12/24, 3572568.61/45, 2566298.00/13, 2539331.17/24, 1384162.29/41, 618047.76/48, 390973.88/24, 337598.75/39, 147601.10/38)
Sector <- c('Utilities','Energy','Basic Materials','Industrials','Communication Services','Consumer Defensive','Consumer Cyclical','Technology','Real Estate','Healthcare','Financial Services')
carbon_table <- as.data.frame(Carbon,Sector, check.names = FALSE)

#Pre defining the file containers 
file1_Status <- NULL
file2_Historical <- NULL
file3_Weight <- NULL
file4_Schedule <- NULL
file5_Weight_Div <- NULL
file6_Div_Dynamic <- NULL
file7_Attribute <- NULL
file8_Return <- NULL

#setting colors
validcol <- c("red", "yellow", "aqua", "blue", "light-blue", "green", "navy", "teal", "olive", "lime", "orange", "fuchsia", "purple", "maroon", "black")

#hiding error from shiny
options(shiny.sanitize.errors = FALSE)


######################################################################################################
########################################Shinyapp UI##################################################
######################################################################################################


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
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
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
      
      tabItem(tabName = "userguide",
              tabBox(width = 12,
                     tabPanel(title = "Introduction",
                              tags$div(
                                tags$ul(
                                  HTML("As awareness of the global warming problem, many countries, institutions, and agents are willing to move toward a more sustainable economy. Similarly to the financial and investment sector, sustainability trends are becoming more necessary. In the asset management industry, a debate has arisen over how to most effectively incentivize corporations and encourage private sector initiatives that will actively induce decision making to reduce environmental impact and drive change in positive mitigation strategies for reducing carbon emissions, the primary cause of climate change. These movements and activities are sometimes referred to as divestment strategies from fossil fuels. Investors and asset managers are urged to liquidate their holdings in companies classified as non-compliant or with poor performance on the environmental component of environmental social governance or ESG ratings, which reflect compliance with current best practices addressing carbon emissions. Not only does divestment deprive fossil fuel businesses of funding to promote change, but it is also a prudent business move for ESG reporting with long-term viability in the spotlight. If the cost of capital increases or refinancing becomes prohibitively expensive or simply unavailable, the valuations of carbon-intensive enterprises will be impacted, and they may become unviable. <br><br> 
                                       <b>Divfolio</b> is a software developed in R shiny environment that offers portfolio analytic related to divestment. The tool offers a comparison of the risk profiles, ESG scores, and customized attributes of portfolios before and after divestment based on the simulation using historical data as well as advanced options such as assessing stability of portfolio via clustering and correlation structure. The tool is useful for investigating the impact of divestment on portfolio performance in multidimensional views. The idea of the divestment methodology is given in our paper, <b>Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns</b>,  available on SSRN. The tool also benefits the general purpose of portfolio performance comparison, especially in ESG investing and sustainable investing. Users can customize the attributes of comparable portfolios by uploading prepared CSV files instead of generating portfolios on the tool."),
                                  a("See our paper here", href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4131449", target="_blank"),
                                  img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/paperssrn.png?raw=true',height="70%", width="70%")
                                )
                              )
                     ),
                     tabPanel(title = "Workflow",
                              box(
                                title = h4("Workflow"),   
                                HTML("DivFolio App tool comprises two core components as will be detailed in the panels below. The first component involves a set of sequential STEPs that users undertake consisting of STEP 1 through to STEP 5, which must be performed sequentially. The second component involves a collection of three independent optional STEPs that can be performed individually. For the optional steps, so long as the user has the relevant data prepared in CSV format already, for instance as output from STEPS 1 to 5 in Component 1, then they can upload this directly into any of the STEPS in Component 2 instead of generating them again from the tool. The uploaded files are required to be in a specific format, described in the section <b>Types of Uploading Files, at the end of this page</b>. Uploading the files is recommended as it will speed up the process significantly and avoid blocking due to too frequent calling data via API.<br>"),
                                img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/daig.jpg?raw=true',height="75%", width="75%", align = 'center'),
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
                                tags$li(HTML("<b> Option I: Multiple Portfolios Comparison </b> <br>
                                                 This optional STEP is used for analysis and comparing the performance of many portfolios. The results in this STEP are similar to STEP 5 but in comparison to more than two portfolios. Generally, it can be used for comparing any portfolio, not limited to divestment.")),
                                tags$li(HTML("<b> Option II: Stability Analysis via Clustering </b> <br>
                                                 This optional STEP is used for investigating the stability of portfolio performance. We employ the clustering algorithm, namely Clustering Large Applications or CLARA, an approximate extension of k-medoids methods to achieve stability investigation. Portfolios that have similar relative behavior will belong to the same cluster over time. The features used in clustering were: return, cumulative return, volatility, sharpe ratio, value at risk, max drawdown, and sortino.")),
                                tags$li(HTML("<b> Option III: Graph Structure Correlation Analysis </b> <br>
                                                 This optional STEP is used for investigating the correlation structure of the assets in portfolios using graph LASSO. This function is suitable for studying the dynamic correlation structure of portfolios and comparison of their diversification as portfolio dynamically evolves, for instance as divestment is progressively performed on different sets of assets.")
                                ),
                                id = "mybox",
                                collapsible = TRUE,
                                collapsed = TRUE,
                                closable = TRUE,
                                width = 13
                              ),
                              
                              box(
                                title = h4("Types of Uploading Files"),
                                tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F0</b>; List of tickers and, optional, their investment statuses. The file requires; <br>
                                               i) a compulsory column of the name 'Ticker', consisting of ticker of all potential assets, and <br>
                                               ii) an optional column of the name 'Status', containing the investment status either 'Invest' or 'Divest'.")),
                                img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file0.png?raw=true'),
                                #tags$br(""),
                                hr(),
                                tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F1</b>; List of tickers, their investment statuses, and some numerical attributes. The file requires; <br>
                                              i) a compulsory column of the name 'Ticker', consisting of ticker of all potential assets, and <br>
                                              ii) a compulsory column of the name 'Status', containing the investment status either 'Invest' or 'Divest', and <br>
                                              iii) at least one columns of any name that contain numerical attributes of the asset.")),
                                img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file1.png?raw=true'),
                                #tags$br(""),
                                hr(),
                                tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F2</b>; The table of assets return. The file requires; <br>
                                              i) a compulsory column of the name 'Date', consisting of dates in the form 'year-month-date', and <br>
                                              ii) other columns that are named by the tickers, containing the return of each asset over time.")), 
                                img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file2.png?raw=true'),
                                hr(),
                                tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F3</b>; The table of portfolio weight. The file requires; <br>
                                              i) a compulsory column of the name 'Date', containing dates of portfolio rebalancing in the form 'year-month-date', and <br>
                                              ii) other columns that are named by the tickers, containing the investment weights of each asset over time.")),
                                img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file3.png?raw=true'),
                                hr(),
                                tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F4</b>; The table of divestment schedule. The file requires; <br>
                                              i) a compulsory column of the name 'Date', containing dates of portfolio rebalancing in the form 'year-month-date', and <br>
                                              ii) a compulsory column of the name 'Bound', containing the sequence of limit of the investment weight of the divestable assets.")),
                                img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file4.png?raw=true'),
                                hr(),
                                tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F5</b>; The table of portfolio weight with multiple portfolios. The file requires; <br>
                                              i) a compulsory column of the name 'Date', containing dates of portfolio rebalancing in the form 'year-month-date', and <br>
                                              ii) a compulsory column of the name 'PORTNAME', containing any name of portfolio as the label for separating data by portfolio type, and
                                              iii) other columns that are named by the tickers, containing the investment weights of each asset over time.")),
                                img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file5.png?raw=true'),
                                hr(),
                                tags$li(HTML("<i class='fa-solid fa-file fa-xl' style = 'color:#0072B2;'></i> <b>F6</b>; List of tickers and their investment statuses. The file requires; <br>
                                               i) a compulsory column of the name 'Ticker', consisting of ticker of all potential assets, and <br>
                                               ii) compulsory column of the name 'Status', containing the investment status either 'Invest' or 'Divest'.")),
                                img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file6.png?raw=true'),
                                hr(),
                                id = "mybox",
                                collapsible = TRUE,
                                collapsed = TRUE,
                                closable = TRUE,
                                width = 13
                              )
                     ),
                     
                     tabPanel(title = "Packages and Dependency",
                              img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/dependency.png?raw=true',align = 'center'),
                              "The application was built under R version 4.1.2",
                              includeHTML('https://github.com/QuantFILab/Divfolio/blob/main/Materials/example.html?raw=true')
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
                                                  color = "success",
                                                  size = "sm"
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
                                                  color = "success",
                                                  size ="sm"
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
                                         collapsed = TRUE,
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
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file0.png?raw=true'),
                                         tags$li(HTML("Click <b>Check icon</b> <i class='fa-solid fa-check' style = 'color:#0072B2;'></i>, upload <b>CSV File of Investment Status</b> with <br> i) a column of the name 'Ticker', containing company's tickers, and
                                        <br> ii) a column of the name 'Status', containing statuses; 'Divest' or 'Invest', and <br>
                                        iii) at least one numeric columns of any name <br>")),
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file1.png?raw=true'),
                                         
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
                                         collapsed = TRUE,
                                         closable = TRUE,
                                         width = 13
                                       ),
                                       box(
                                         title = h4("Seleacting Assets from World Major Indices"),
                                         pickerInput(
                                           inputId = "selindex",
                                           label = "Select Index", 
                                           choices = index_choice
                                         ),
                                         plotOutput("map"),
                                         actionBttn(
                                           inputId = "subindex",
                                           label = "submit index",
                                           style = "pill", 
                                           color = "success",
                                           size = "xs"
                                         ),
                                         id = "mybox2",
                                         collapsible = TRUE,
                                         collapsed = TRUE,
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
                                       h6("*Please remove assets with NA before operating STEP 4, otherwise some functions may not work properly"),
                                       actionBttn(
                                         inputId = "subremove",
                                         label = "remove NA rows",
                                         style = "pill", 
                                         color = "success",
                                         size = "xs"
                                       ),
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
                                       dropdown(dateInput("stdate", "From:", value = ymd(as.Date(Sys.time())) %m-% years(1)),
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
                                                  color = "success",
                                                  size ="sm"
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
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file2.png?raw=true'),
                                         tags$br(""),
                                         HTML("<b>or 2. Download return data from a network </b>"),
                                         tags$li(HTML("Click <b>Calendar icon</b> <i class='fa-solid fa-calendar-days' style = 'color:#0072B2;'></i>, set time period to obtain the returns, and click <b>get historical data</b> button.")),
                                         hr(),
                                         "The remarks for this STEP is listed below",
                                         tags$br(""),
                                         tags$li(HTML("<b>Table of Historical Relative Return</b> shows the return uploaded from <b>CSV File of Historical Data</b> or the relative returns where the data is downloaded from the network. the ralative return is calculated by the adjust closing price by
                                    $$Re = \\frac{P_{t+1} - P_{t}}{P_{t}}$$ where $P_{t+1}$ and $P_{t}$ are the adjusted closing prices at time $t+1$ and $t$, respectively.")),
                                         tags$li(HTML("<b>Table of Assets Performance</b> displays the average of asset performances over the selected period.")),
                                         tags$li(HTML("<b>Missing Data Handling</b> displays the average of missing returns over the selected period")),
                                         tags$li(HTML("<b>Distribution of Asset Returns</b> displays the distribution of the returns of the assets over the selected period. Users can select the picker option 
                                    <b>Arranging Plot by</b> to sort the assets according to the interesting performance.")),
                                         tags$br(""),
                                         "* User Guide box is collapsible",
                                         id = "mybox",
                                         collapsible = TRUE,
                                         collapsed = TRUE,
                                         closable = TRUE,
                                         width = 13
                                       ),
                                       
                                       h4("Table of Historical Relative Return"),
                                       hr(),
                                       withLoader(DT::dataTableOutput("tab_return", width = "auto", height = "auto")),
                                       h4("Table of Assets Performance"),
                                       hr(),
                                       withLoader(DT::dataTableOutput("reportreturn", width = "auto", height = "auto")),
                                       h4("Missing Data Handling"),
                                       hr(),
                                       h6("NA element is represented by 0 and Non-NA element is represented by 1"),
                                       h6("Please process to eliminate missing data before go to next step"),
                                       pickerInput(
                                         inputId = "selmethod",
                                         label = "Select NA handling method", 
                                         choices = list('Delete NA rows' = 0, 'Replace NA by 0' = 1, 'Linear Interpolation' = 2)
                                       ),
                                       actionBttn(
                                         inputId = "subnahandle",
                                         label = "NA clean",
                                         style = "pill", 
                                         color = "success",
                                         size = "xs"
                                       ),
                                       withLoader(plotlyOutput("naheatmap", width = "auto", height = "auto")),
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
                                         textInput("reb",  tags$span("Rebalancing Frequency [in day]", bsButton("iter", label = "", icon = icon("info"), style = "info", size = "extra-small")), 20),
                                         br(),
                                         textOutput(outputId = "textnum"),
                                         bsPopover(
                                           id = "iter",
                                           title = "Suggestion",
                                           content = "Number of rebalancing iterations = (Number of Days in Selected Period)/(Rebalancing Frequency). More number of iterations cost more computation time, espacially for a large portfolio. We suggest trying small number as no more than 10 iterations before running more number of iterations."
                                         ),
                                         br(),
                                         textInput("tau", "Number of Days for Calculating Weights", 20),
                                         actionBttn(
                                           inputId = "subweight",
                                           label = "submit",
                                           style = "pill", 
                                           color = "success",
                                           size = "sm"
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
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file3.png?raw=true'),
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
                                         collapsed = TRUE,
                                         closable = TRUE,
                                         width = 13
                                       ),
                                       progressBar(
                                         id = "pb",
                                         value = 0,
                                         total = 100,
                                         title = "Process does not start yet",
                                         display_pct = TRUE,
                                         status = "custom"
                                       ),
                                       tags$style(".progress-bar-custom {background-color: #25c484;}"),
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
                                           inputId = "subdivpreview",
                                           label = "preview",
                                           style = "pill", 
                                           color = "success",
                                           size ="sm"
                                         ),
                                         actionBttn(
                                           inputId = "subdiv",
                                           label = "submit",
                                           style = "pill", 
                                           color = "success",
                                           size ="sm"
                                         ),
                                         withLoader(plotlyOutput("plot4divpreview", width = "auto", height = "auto")),
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
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file4.png?raw=true'),
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
                                         collapsed = TRUE,
                                         closable = TRUE,
                                         width = 13
                                       ),
                                       
                                       h4("Table of Divestment Schedule"),
                                       hr(),
                                       progressBar(
                                         id = "pb2",
                                         value = 0,
                                         total = 100,
                                         title = "Process does not start yet",
                                         display_pct = TRUE,
                                         status = "custom"
                                       ),
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
                     
                     tabPanel(title  = "Option I",
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
                                                  color = "success",
                                                  size ="sm"
                                                )
                                       )),
                                column(11,
                                       hr(style = "border-top: 6px solid #0784BF;"),
                                       h3("Option I: Multiple Portfolios Comparison"),
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
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file5.png?raw=true'),
                                         tags$br(""),
                                         tags$li(HTML("Upload <b>CSV File of Attributes</b> that consists of; <br>
                                    i) one column of the name 'Ticker', containing the ticker of assets, and <br>
                                    ii) one column of the name 'Status', containing the status of the asset either 'Invest' or 'Divest', and <br>
                                    iii) at least one numeric column of any name.")),
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file1.png?raw=true'),
                                         tags$br(""),
                                         tags$li(HTML("Upload <b>CSV File of Returns</b> that consists of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset returns at given time.")),
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file2.png?raw=true'),
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
                                         collapsed = TRUE,
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
                     tabPanel(title  = "Option II",
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
                                                  color = "success",
                                                  size ="sm"
                                                )
                                       )
                                ),
                                column(11, 
                                       hr(style = "border-top: 6px solid #0784BF;"),
                                       h3("Option II: Stability Analysis via Clustering"),
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
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file5.png?raw=true'),
                                         tags$br(""),
                                         tags$li(HTML("Upload <b>CSV File of Returns</b> that consists of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset returns at given time.")),
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file2.png?raw=true'),
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
                                         collapsed = TRUE,
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
                     
                     tabPanel(title  = "Option III",         
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
                                       h3("Option III: Graph Structure Correlation Analysis"),
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
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file5.png?raw=true'),
                                         tags$br(""),
                                         tags$li(HTML("Upload <b>CSV File of Status Divestment</b> is optional that consists of; <br>
                                    i) one column of the name 'Ticker', containing the ticker of assets, and <br>
                                    ii) one column of the name 'Status', containing the status of the asset either 'Invest' or 'Divest'.")),
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file1.png?raw=true'),
                                         tags$br(""),
                                         tags$li(HTML("Upload <b>CSV File of Returns</b> that consists of; <br>
                                    i) one column of the name 'Date'containing date in form of 'year-month-day', and <br>
                                    ii) other columns are named by the tickers and the values in cells are the asset returns at given time.")),
                                         img(src = 'https://github.com/QuantFILab/Divfolio/blob/main/Materials/file2.png?raw=true'),
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
                                         collapsed = TRUE,
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
                                                progressBar(
                                                  id = "pbg1",
                                                  value = 0,
                                                  total = 100,
                                                  title = "Process does not start yet",
                                                  display_pct = TRUE,
                                                  status = "custom"), 
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
                                                progressBar(
                                                  id = "pbg2",
                                                  value = 0,
                                                  total = 100,
                                                  title = "Process does not start yet",
                                                  display_pct = TRUE,
                                                  status = "custom"
                                                ),
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
                                                progressBar(
                                                  id = "pbg3",
                                                  value = 0,
                                                  total = 100,
                                                  title = "Process does not start yet",
                                                  display_pct = TRUE,
                                                  status = "custom"
                                                ),
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
                    image = "https://github.com/QuantFILab/Divfolio/blob/main/Materials/cns.jpg?raw=true",
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
                    image = "https://github.com/QuantFILab/Divfolio/blob/main/Materials/eoh.jpg?raw=true",
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
                    image = "https://github.com/QuantFILab/Divfolio/blob/main/Materials/gwp.png?raw=true",
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
                    image = "https://github.com/QuantFILab/Divfolio/blob/main/Materials/kar.png?raw=true",
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
                    image = "https://github.com/QuantFILab/Divfolio/blob/main/Materials/pm.jpg?raw=true",
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


######################################################################################################
########################################Shinyapp Sever################################################
######################################################################################################



server <- function(input, output, session) {
  
  data_yahoo <- eventReactive(input$submit, {
    # Fetch yahoo data for the input ticker when submit button is pressed
    get_data(input$ticker)
  })
  
  data_stock <- eventReactive(input$submit, {
    # Fetch stock profile for the input ticker when submit button is pressed
    get_stock_profile(input$ticker)
  })
  
  candel_stock <- eventReactive(input$submit, {
    # Generate a candlestick plot for the fetched yahoo data when submit button is pressed
    candlestick_plot(data_yahoo(), input$ticker)
  })
  
  perf_stock <- eventReactive(input$submit, {
    # Generate a performance plot for the fetched yahoo data when submit button is pressed
    performance_plot(data_yahoo(), input$ticker)
  })
  
  re_ticker <- eventReactive(input$add, {
    # When the add button is pressed, update the main portfolio (main_port) with the selected ticker
    # '<<-' is used to modify the main_port in the global environment
    main_port <<- unique(c(main_port, toupper(as.character(input$ticker_sel))))
    main_port  # Return the updated main_port
  })
  
  
  # Render Level ESG Value Box
  output$lev_esg <- renderValueBox({
    valueBox(
      # Display the 4th element of data_stock() with font size 70%
      value = tags$p(data_stock()[4], style = "font-size: 70%;"),
      # Display the 10th element of data_stock() as a subtitle with font size 70%
      subtitle = tags$p(data_stock()[10], style = "font-size: 70%;"),
      # Set color based on the 5th element of data_stock() if it's in validcol, else black
      color = ifelse(data_stock()[5] %in% validcol, data_stock()[5], 'black'),
      # Use seedling as the icon
      icon = icon('seedling')
    )
  })
  
  # Render Total ESG Score Value Box
  output$total_esg <- renderValueBox({
    valueBox(
      # Display the 6th element of data_stock() with font size 70%
      value = tags$p(data_stock()[6], style = "font-size: 70%;"),
      # Display "Total ESG Score" as a subtitle with font size 70%
      subtitle = tags$p("Total ESG Score ", style = "font-size: 70%;"),
      # Set color based on the 5th element of data_stock() if it's in validcol, else black
      color = ifelse(data_stock()[5] %in% validcol, data_stock()[5], 'black'),
      # Use tree as the icon
      icon = icon('tree')
    )
  })
  
  # Render Environment Score Value Box
  output$e_score <- renderValueBox({
    valueBox(
      # Display the 7th element of data_stock() with font size 70%
      value = tags$p(data_stock()[7], style = "font-size: 70%;"),
      # Display 'Environment' as a subtitle with font size 70%
      subtitle = tags$p('Environment', style = "font-size: 70%;"),
      # Set color to light-blue
      color = 'light-blue',
      # Use solar-panel as the icon
      icon = icon('solar-panel')
    )
  })
  
  # Render Social Score Value Box
  output$s_score <- renderValueBox({
    valueBox(
      # Display the 8th element of data_stock() with font size 70%
      value = tags$p(data_stock()[8], style = "font-size: 70%;"),
      # Display 'Social' as a subtitle with font size 70%
      subtitle = tags$p('Social', style = "font-size: 70%;"),
      # Set color to aqua
      color = 'aqua',
      # Use hashtag as the icon
      icon = icon('hashtag')
    )
  })
  
  # Render Governance Score Value Box
  output$g_score <- renderValueBox({
    valueBox(
      # Display the 9th element of data_stock() with font size 70%
      value = tags$p(data_stock()[9], style = "font-size: 70%;"),
      # Display 'Governance' as a subtitle with font size 70%
      subtitle = tags$p('Governance', style = "font-size: 70%;"),
      # Set color to blue
      color = 'blue',
      # Use handshake as the icon
      icon = icon('handshake')
    )
  })
  
  
  # Define event reactive expression for table_esg, which is triggered by input$subesg
  table_esg <- eventReactive(input$subesg, {
    
    # Combine unique values from input$source and input$divt and store in port
    port <- unique(c(as.character(input$source), as.character(input$divt)));
    
    # For each element in port, fetch the ESG profile and store the results in tab_esg
    tab_esg <- sapply(seq_along(port), function(i) unlist(get_esg_profile(port[i]))) %>% t() %>% data.frame();
    
    # Convert specific columns of tab_esg to numeric
    tab_esg$ESG <- as.numeric(tab_esg$ESG)
    tab_esg$Environment <- as.numeric(tab_esg$Environment) # Fixed typo: Enironment to Environment
    tab_esg$Social <- as.numeric(tab_esg$Social)
    tab_esg$Governance <- as.numeric(tab_esg$Governance)
    
    # Assign Status based on whether port is in input$source
    tab_esg$Status <- ifelse((port %in% as.character(input$source)), "Invest", "Divest");
    
    # Update the global variable file1_Status
    file1_Status <<- tab_esg;
    
    # Update ui_esg_table with the data table
    output$ui_esg_table <- DTtable(file1_Status)
    
    # Return the value to be used by other reactive objects
    file1_Status
  })
  
  
  # Observe the event of input$subup and execute the code block when it triggers
  observeEvent(input$subup, {
    
    # If file1_Status is null, give a warning to upload a file first
    if (is.null(file1_Status)) {
      feedbackWarning(
        inputId = "subup",
        show = FALSE,
        text = "Upload File before"
      )
    } else {
      # Get unique values from input$source and input$divt and store in port
      port <- unique(c(as.character(input$source), as.character(input$divt)))
      
      # Get the difference between port and Ticker column of file1_Status
      port_up <- setdiff(port, file1_Status$Ticker)
      
      # If there are elements in port_up, get ESG profiles and update file1_Status
      if (length(port_up) != 0) {
        tab_esg <- sapply(seq_along(port_up), function(i) unlist(get_esg_profile(port_up[i]))) %>% t() %>% data.frame()
        
        # Converting specific columns to numeric
        tab_esg$ESG <- as.numeric(tab_esg$ESG)
        tab_esg$Environment <- as.numeric(tab_esg$Environment) # Fixed typo: Enironment to Environment
        tab_esg$Social <- as.numeric(tab_esg$Social)
        tab_esg$Governance <- as.numeric(tab_esg$Governance)
        
        # Assign Status based on whether port_up is in input$source
        tab_esg$Status <- ifelse((port_up %in% as.character(input$source)), "Invest", "Divest")
        
        # Update global variable file1_Status
        file1_Status <<- rbind(file1_Status, tab_esg) %>% subset(Ticker %in% port)
      } else {
        # If no elements in port_up, just update the Status column of file1_Status
        dum <- rep(0, nrow(file1_Status))
        for (i in 1:length(file1_Status$Ticker)) {
          dum[i] <- ifelse((file1_Status$Ticker[i] %in% as.character(input$source)), "Invest", "Divest")
        }
        
        # Update global variable file1_Status
        file1_Status$Status <<- dum
        file1_Status <<- file1_Status %>% subset(Ticker %in% port)
      }
      
      # Update ui_esg_table with the data table
      output$ui_esg_table <- DTtable(file1_Status)
    }
  })
  
  
  
  
  # Observe the event of input$fileticker and execute the code block when it triggers
  observeEvent(input$fileticker, {
    
    # Assign the uploaded file to File
    File <- input$fileticker
    
    # Validate the uploaded file, if no file has been uploaded, return a message "No data has been uploaded"
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    
    # Read the uploaded file and assign it to tick
    tick <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    
    # Check the number of columns in the uploaded file and perform corresponding actions
    if (ncol(tick) == 1) { # If the uploaded file has one column
      tick$inv <- rep(1, nrow(tick)) # Create a new column 'inv' and assign 1 to all rows
      
    } else if (ncol(tick) == 2) { # If the uploaded file has two columns
      tick <- tick # Keep tick as it is
      
    } else { # If the uploaded file has more than two columns
      tick <- NULL # Assign NULL to tick
    }
    
    # Assign values from the first column of tick where the second column is 1 to the global variable inv_asset
    inv_asset <<- as.character(tick[, 1][tick[, 2] == 1])
    
    # Assign values from the first column of tick where the second column is 0 to the global variable div_asset
    div_asset <<- as.character(tick[, 1][tick[, 2] == 0])
    
    # Update the 'source' OrderInput with the values in inv_asset and assign 'info' class to the items
    updateOrderInput(session, inputId = "source", items =  inv_asset, item_class = 'info')
    
    # Update the 'divt' OrderInput with the values in div_asset and assign 'info' class to the items
    updateOrderInput(session, inputId = "divt", items =  div_asset, item_class = 'info')
  })
  
  
  observeEvent(input$file1, {
    # Assign the uploaded file to File
    File <- input$file1
    
    # Validate the uploaded file, if no file has been uploaded, return a message "No data has been uploaded"
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    
    # Read the uploaded file and assign it to global variable f1
    f1 <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    
    # Check if all columns are of character type and if columns 'Ticker' and 'Status' exist in the dataframe
    con1 <- (sum(sapply(f1, class) %in% c('integer', 'numeric')) == 0) # Corrected 'interger' to 'integer'
    con2 <- (sum(c("Status", 'Ticker') %in% colnames(f1)) != 2)
    
    # Display warnings for different conditions not being met
    if(con1){
      # Display warning if there is no numeric or integer column
      showFeedbackWarning(
        inputId = "file1",
        text = "Require at least one column to be numeric") # Corrected 'on' to 'one'
    }else if(con2){
      # Display warning if columns 'Ticker' and 'Status' are missing
      showFeedbackWarning(
        inputId = "file1",
        text = "Missing columns of the names 'Ticker' and 'Status'")
    }else if(FALSE %in% (unlist(f1$Status) %in% c('Invest', 'Divest'))){
      # Display warning if the Status column has values other than 'Invest' or 'Divest'
      showFeedbackWarning(
        inputId = "file1",
        text = "Require Status to be 'Invest' or 'Divest'")
    }else if(sum(is.na(f1)) != 0){
      # Display warning if there are missing values in the dataframe
      showFeedbackWarning(
        inputId = "file1",
        text = "Missing Data Detected")
    }else{
      # Update the UI elements and global variables if all conditions are met
      file1_Status <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      output$ui_esg_table <- DTtable(file1_Status)
      inv_asset <<- (file1_Status %>% dplyr::filter(Status == 'Invest'))$Ticker 
      div_asset <<- (file1_Status %>% dplyr::filter(Status == 'Divest'))$Ticker
      updateOrderInput(session, inputId = "source", items =  inv_asset, item_class = 'info')
      updateOrderInput(session, inputId = "divt", items =  div_asset, item_class = 'info')
    }
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$file2, {
    # Assign the uploaded file to File
    File <- input$file2
    
    # Validate the uploaded file, if no file has been uploaded, return a message "No data has been uploaded"
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    
    # Read the uploaded file and assign it to the local variable f2
    f2 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    
    # Check some conditions using a user-defined function `req.cons.file`, assumed to return a list of logicals
    cons <- req.cons.file(f2, c("Date"))
    
    # Display warnings for different conditions not being met
    if(cons[1]){
      # Display warning if there is no numeric or date column
      showFeedbackWarning(
        inputId = "file2",
        text = "Require at least one column to be numeric or date") # Assumed condition corrected
    }else if(cons[2]){
      # Display warning if the 'Date' column is missing
      showFeedbackWarning(
        inputId = "file2",
        text = "Missing the column of the names 'Date'")
    }else if(cons[3]){
      # Display warning if there are missing values in the dataframe
      showFeedbackWarning(
        inputId = "file2",
        text = "Missing Data Detected")
    }else{
      # Update the UI elements and global variables if all conditions are met
      file2_Historical <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      plottab <- file2_Historical
      output$tab_return <- DTtable(round_tab(plottab))
      output$reportreturn <- DTtable(return_summary_update(file2_Historical))
      output$naheatmap <- renderPlotly(NA_heatmap(file2_Historical))
    }
    
  }, ignoreNULL = FALSE)
  
  
  
  observeEvent(input$add, {
    # Upon clicking a UI element with input ID 'add', the following code will execute:
    # The items in the 'OrderInput' UI element with inputId = 'inter' are updated with the values returned by re_ticker() function, and the class of the item is set to 'info'.
    updateOrderInput(session, inputId = "inter", items = re_ticker(), item_class = 'info')
  })
  
  observeEvent(input$reasset, {
    # Upon clicking a UI element with input ID 'reasset', the following code will execute:
    # The items in the 'OrderInput' UI element with inputId = 'inter' are updated to NULL, and the class of the item is set to 'info'.
    updateOrderInput(session, inputId = "inter", items = NULL, item_class = 'info')
  })
  
  
  batch <- eventReactive(input$subtime, {
    if (is.null(file1_Status)) {
      # If file1_Status is NULL, a notification with a warning message is displayed.
      noty(text = "Require Uploading File in Step 2 before", type = "warning")
    } else {
      # If file1_Status is not NULL, fetches financial data for the tickers in file1_Status$Ticker for the specified date range.
      data <- BatchGetSymbols(file1_Status$Ticker, as.Date(input$stdate), as.Date(input$enddate), 0.1)
      # Processes the obtained data and assigns it to the file2_Historical variable.
      dataframe <- data[[2]] %>% dplyr::select(ticker, ref.date, ret.adjusted.prices) %>% drop_na() %>% dcast(ref.date ~ ticker)
      colnames(dataframe)[which(names(dataframe) == "ref.date")] <- "Date"
      file2_Historical <<- dataframe
      file2_Historical
    }
  })
  
  
  output$tab_return <- DTtable(NULL)
  output$ui_esg_table_com <- DTtable(NULL)
  
  observeEvent(input$subtime, {
    # Validate that file1_Status is not null
    shiny::validate(need(!is.null(file1_Status), " "))
    
    # Prepare and round data from batch() reactive expression
    plottab <- batch()
    plottab[, 2:ncol(plottab)] <- round(plottab[, 2:ncol(plottab)], 6)
    
    # Update the outputs based on the prepared data
    output$tab_return <- DTtable(plottab)
    output$reportreturn <- DTtable(return_summary_update(batch()))
    output$naheatmap <- renderPlotly(NA_heatmap(file2_Historical))
  })
  
  output$comp_name <- renderText(data_stock()[1])
  output$comp_market <- renderText(data_stock()[14])
  output$comp_sec <- renderText(paste0("Sector: ", data_stock()[2], ", Subsector: ", data_stock()[3]))
  output$comp_sum <- renderText(paste0("Market Cap: ", data_stock()[11], ", PE Ratio: ", data_stock()[12], ", Forward Dividend & Yield: ", data_stock()[13]))
  
  output$perf_fig <- renderPlotly(perf_stock()[[2]])
  output$perf_table <- DTtable(perf_stock()[[1]])
  
  output$verb <- renderText({ re_ticker() })
  output$ui_esg_table <- DTtable(table_esg())
  output$reportreturn <- DTtable(return_summary(batch()[[2]]))
  
  output$tab_return <- DTtable(NULL)
  output$ui_esg_table_com <- DTtable(NULL)
  
  
  # This observer watches for changes in either of the following:
  # 1. File uploaded in `input$file1`
  # 2. The action button `input$subesg` is pressed
  # 3. The action button `input$subup` is pressed
  observeEvent(list(input$file1, input$subesg, input$subup), {
    # Validation step: If no file is uploaded AND both buttons are not pressed, it will display a validation error asking the user to select a weight.
    shiny::validate(
      need((!is.null(input$file1))||(input$subesg == TRUE)||(input$subup == TRUE), "Please select a weight")
    )
    # If validation is passed, this will update the output table `ui_esg_table_com` 
    # with the rounded values from the `plotstart` function applied to the `file1_Status` data frame,
    # and the row names are added as a column named 'Status'.
    output$ui_esg_table_com  <- DTtable(round(plotstart(file1_Status),2) %>% rownames_to_column('Status'))
    
    # Additionally, it will update the choices available in the "selq" PickerInput, based on the columns of `file1_Status`
    # that are either numeric or integer types.
    updatePickerInput(session, inputId = "selq", choices = file1_Status[, sapply(file1_Status, class) %in% c('numeric','integer'), drop=FALSE] %>% colnames())
  })
  
  
  # ------------------------------------
  # Function: observeEvent for input$subman
  # Purpose:  This function is triggered by the 'subman' event and manages user input to perform 
  #           auto-selection based on the given conditions. It updates tables and other UI elements 
  #           based on the new selection.
  # Input:    The function observes 'input$subman' and responds to changes.
  # Output:   The function updates UI elements like 'ui_esg_table', 'ui_esg_table_com', 'source', and 'divt'.
  # ------------------------------------
  
  observeEvent(input$subman, {
    # Check if input$perc is numeric and within the range of [0, 1].
    if((as.numeric(input$perc) > 1) || (as.numeric(input$perc) < 0) || (is.na(as.numeric(input$perc)))){
      # Provide feedback if the entered value is not in the acceptable range.
      feedbackWarning(
        inputId = "perc",
        show = TRUE,
        text = "Please input number from 0 to 1"
      )
    } else if(is.null(file1_Status)){
      # Provide feedback if the 'file1_Status' table is not available.
      noty("Require Table of Attributes", type = 'warning')
    } else {
      # Perform auto-selection and update the 'file1_Status' table.
      file1_Status <<- auto_selection(file1_Status, input$selq, as.numeric(input$perc), input$seldec)
      
      # Update the 'ui_esg_table' and 'ui_esg_table_com' with new values from 'file1_Status'.
      output$ui_esg_table <- DTtable(file1_Status)
      output$ui_esg_table_com <- DTtable(round(plotstart(file1_Status),2) %>% rownames_to_column('Status'))
      
      # Update the 'source' and 'divt' OrderInputs with new tickers from 'file1_Status'.
      updateOrderInput(session, inputId = "source", items = (file1_Status %>% dplyr::filter(Status == "Invest") %>% drop_na())$Ticker, item_class = 'info')
      updateOrderInput(session, inputId = "divt", items =  (file1_Status %>% dplyr::filter(Status == "Divest") %>% drop_na())$Ticker, item_class = 'info')
    }
  })
  
  
  
  # ------------------------------------
  # Function: observeEvent for input$subremove
  # Purpose:  This function is executed when 'subremove' event is triggered, 
  #           mainly dealing with removal of rows with NA values from 'file1_Status' 
  #           and updating several UI components with new values.
  # Input:    The function observes 'input$subremove' and responds to changes.
  # Output:   It updates several UI elements like 'ui_esg_table', 'ui_esg_table_com', 'source', and 'divt'.
  # ------------------------------------
  
  observeEvent(input$subremove, {
    # Check if the 'file1_Status' table is available.
    if(is.null(file1_Status)){
      # Warn the user if 'file1_Status' table is not available.
      noty("Require Table of Attributes", type = 'warning')
    } else {
      # Remove rows with NA values from 'file1_Status' table and update it.
      file1_Status <<- na.omit(file1_Status)
      
      # Update the 'ui_esg_table' and 'ui_esg_table_com' with new values from 'file1_Status'.
      output$ui_esg_table <- DTtable(file1_Status)
      output$ui_esg_table_com <- DTtable(round(plotstart(file1_Status),2) %>% rownames_to_column('Status'))
      
      # Update the 'source' and 'divt' OrderInputs with new tickers from 'file1_Status'.
      updateOrderInput(session, inputId = "source", items = (file1_Status %>% dplyr::filter(Status == "Invest") %>% drop_na())$Ticker, item_class = 'info')
      updateOrderInput(session, inputId = "divt", items =  (file1_Status %>% dplyr::filter(Status == "Divest") %>% drop_na())$Ticker, item_class = 'info')
    }
  })
  
  
  output$candel <- renderPlotly(candel_stock())
  
  observeEvent(input$file2,{
    updatePickerInput(session, "rankby", choices =list("Ticker","Return", "Volatility", "Sharpe", "MDD", "Sortino", "Return.cumulative", "VaR"))
  })
  observeEvent(input$subtime,{
    shiny::validate(need(!is.null(file1_Status)," "))
    updatePickerInput(session, "rankby", choices =list("Ticker","Return", "Volatility", "Sharpe", "MDD", "Sortino", "Return.cumulative", "VaR"))
  })
  
  
  output$boxasset <- renderPlotly(NULL)
  
  observeEvent(input$rankby, {
    shiny::validate(
      need(!is.null(input$rankby),"Please upload Historical Return")
    )
    output$boxasset <- renderPlotly(boxplot_assets(file2_Historical, as.character(input$rankby)))
  })
  
  
  observeEvent(input$file3, {
    File <- input$file3
    shiny::validate(
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
  
  
  # Define a reactive expression 'get_weight' that depends on 'input$subweight' and 'input$file3'
  get_weight <- eventReactive(list(input$subweight,input$file3), {
    
    # Validate the presence of 'input$subweight' or 'input$file3', displaying an error message if neither is present.
    shiny::validate(need((input$subweight >= 1) || (!is.null(input$file3)), "Data is needed"))
    
    # Check whether 'file1_Status' and 'file2_Historical' are NULL, displaying a warning message if either is.
    if(is.null(file1_Status) || is.null(file2_Historical)){
      noty(text = "Require Uploading Files in Step 2 and 3 before", type = "warning")
    } else {
      
      # Check if 'input$file3' is NULL
      if(is.null(input$file3)){
        
        # Update the progress bar with the specified values and title if 'input$file3' is NULL.
        updateProgressBar(id = 'pb', value = 0, total = 100, title = "Processing")
        
        # Assign the values of various input elements and file data to local variables.
        x <- file2_Historical
        type <- input$type
        tau <- as.integer(input$tau)
        reb <- as.integer(input$reb)
        lim <- as.integer(input$limselect) * as.numeric(input$shortlim)
        filew <- NULL
        one <- as.matrix(rep(1, ncol(x)-1))
        
        # Compute 'rebdate', a sequence of dates at which rebalancing occurs.
        rebdate <- x[seq(reb, nrow(x), reb),]$Date
        
        # Compute 'returnmat', a list of matrices representing return data for each rebalancing period.
        # Perform various transformations on the 'returnmat' data to handle NA and zero values.
        returnmat <- lapply(seq_along(rebdate), function(i) {
          rowre <- which(x$Date == as.Date(rebdate[i]))
          x[(rowre - tau):(rowre - 1), -1]
        }) %>% lapply(function(m) {m[is.na(m)] <- 0; m}) %>% lapply(function(m) {
          ifelse(length(m[m == 0]) == 0, m <- m, m[m == 0] <- rnorm(length(m[m == 0]), 0.0001, 0.0001))
          m
        })
        
        # Compute 'covmat' and 'invcov', lists of covariance matrices and their inverses for each period in 'returnmat'.
        # Perform various transformations on the covariance matrices to handle numerical instability and update the progress bar accordingly.
        covmat <- vector("list", length = length(returnmat))
        invcov <- vector("list", length = length(returnmat))
        for(j in 1:length(returnmat)) {
          covmat[[j]] <- cov(returnmat[[j]])
          if(log10(kappa(covmat[[j]])) >= 5) {
            covmat[[j]] <- as.matrix(covOGK(returnmat[[j]], sigmamu = s_mad)$cov)
            dimnames(covmat[[j]]) <- list(colnames(x)[-1], colnames(x)[-1])
            s <- as.matrix(solve(as.matrix(covmat[[j]], tol = 1E-1000)))
            s[lower.tri(s)] <- t(s)[lower.tri(s)]
            invcov[[j]] <- s
          } else {
            s <- as.matrix(solve(as.matrix(covmat[[j]], tol = 1E-1000)))
            s[lower.tri(s)] <- t(s)[lower.tri(s)]
            invcov[[j]] <- s
          }
          title <- ifelse(j == length(returnmat), "Done", "Calculating, please wait")
          updateProgressBar(id = 'pb', value = j, total = length(returnmat), title = title)
        }
        
        # Based on the user-selected 'type', compute the weights 'w' for each asset in each rebalancing period.
        # Perform various transformations on 'w' based on the user-selected limit 'lim' and column names.
        # If 'filew' is not NULL, use it as the value of 'w' instead.
        if(is.null(filew)) {
          # Computing 'w' using a switch-case structure based on the value of 'type'.
          # ... (Refer to original code for specific calculations.)
          
          if(lim != 0) {w <- apply(w, 1, box_constrain, Short_Limit = lim) %>% t() %>% data.frame(check.names = FALSE)}
          
          w <- cbind(rebdate, w) %>% as.data.frame(check.names = FALSE) %>% `colnames<-`(c('Date', colnames(w)))
        } else {
          w <- filew
          w[, 2:ncol(w)] <- round(w[, 2:ncol(w)], 6)
        }
        
        # Compute 'Weightset', a list containing the computed weights 'w' and additional information related to returns and covariance matrices.
        Weightset <- list(w, returnmat, covmat, rebdate)
      } else {
        # If 'input$file3' is not NULL, compute 'Weightset' using previously computed weights 'file3_Weight' and additional information.
        Weightset <- list(file3_Weight, multipywtoreturn(file3_Weight, file2_Historical), NULL, file3_Weight$Date)
      }
      
      # Assign the first element of 'Weightset' to 'file3_Weight' in the global environment and round its numerical values to six decimal places.
      file3_Weight <<- Weightset[[1]]
      file3_Weight[, 2:ncol(file3_Weight)] <<- round(file3_Weight[, 2:ncol(file3_Weight)], 6)
      
      # Return 'Weightset'.
      list(file3_Weight, Weightset[[2]], Weightset[[3]], Weightset[[4]])
    }
  })
  
  
  # Observe 'input$reb' and update 'output$textnum' to display the number of rebalancing iterations.
  observeEvent(input$reb, {
    output$textnum <- renderText({
      ifelse(is.null(input$reb), 
             "Select Rebalancing Frequency", 
             paste0("Number of Rebalancing Iterations is now ", ceiling(nrow(file2_Historical) / as.integer(input$reb))))
    })
  })
  
  # Initially set 'output$weight' to NULL.
  output$weight <- DTtable(NULL)
  
  
  # This code sets up an observer which listens for changes to either 'input$subweight' or 'input$file3'.
  observeEvent(list(input$subweight, input$file3), {
    
    # It validates if either 'input$subweight' is greater than or equal to 1 or 'input$file3' is not NULL.
    # If neither condition is met, it will display "Data is needed" message.
    shiny::validate(need((input$subweight >= 1) || (!is.null(input$file3)), "Data is needed"))
    
    # If the conditions are met, it populates 'output$weight' with the first element of 'get_weight()' 
    # rendered as a DataTable (presumably 'DTtable' is a custom function for rendering DataTables).
    output$weight <- DTtable(get_weight()[[1]])
  })
  
  # It initializes 'output$plotw3' with a NULL Plotly plot. 
  # This is likely a placeholder, and 'output$plotw3' is probably redefined elsewhere in the app.
  output$plotw3 <- renderPlotly(NULL)
  
  
  # Defining an event reactive object named shiny_asset_weight_plot
  # It will execute asset_weight_plot() function every time input$selcom changes
  shiny_asset_weight_plot <- eventReactive(input$selcom, asset_weight_plot(get_weight()[[1]],as.character(input$selcom)))
  
  # Observing changes in either input$subweight or input$file3 and then validating data
  # Upon successful validation, the "selcom" PickerInput choices are updated with new column names 
  # from the first list element returned by get_weight() excluding the first column
  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    updatePickerInput(session, "selcom", choices = colnames(get_weight()[[1]])[-1])
  })
  
  # Observing changes to input$selcom to render and update the plotly plot, output$plotw3
  # using the current state of shiny_asset_weight_plot eventReactive object
  observeEvent(input$selcom, output$plotw3 <- renderPlotly(shiny_asset_weight_plot())) 
  
  # Initializing several output Plotly plots and a DataTable to NULL
  output$plotw3F <- renderPlotly(NULL)  
  output$plotw3Fshort <- renderPlotly(NULL)  
  output$plotw3Flong <- renderPlotly(NULL)  
  output$plotw3Ftab <- renderDT(NULL)  
  
  # Creating another eventReactive object shiny_plot_fundamental
  # It gets triggered when either input$subweight or input$file3 changes,
  # and after successful validation, it executes the plot_fundamental() function
  shiny_plot_fundamental <- eventReactive(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    plot_fundamental(get_weight()[[1]],file1_Status)
  })
  
  
  # Observing changes in either input$subweight or input$file3
  # On event, it validates the inputs and then renders several Plotly plots and a DataTable
  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    
    # Rendering different Plotly plots using various elements from shiny_plot_fundamental eventReactive object
    output$plotw3F <- renderPlotly(shiny_plot_fundamental()[[1]])
    output$plotw3Fshort <- renderPlotly(shiny_plot_fundamental()[[3]])
    output$plotw3Flong <- renderPlotly(shiny_plot_fundamental()[[2]])
    
    # Rendering a DataTable using the fourth element from shiny_plot_fundamental eventReactive object
    output$plotw3Ftab <- DTtable(shiny_plot_fundamental()[[4]])
  })
  
  # Initializing another Plotly plot to NULL
  output$plotw3port <- renderPlotly(NULL)  
  
  # Creating a new eventReactive object shiny_plot_portfolio_weight 
  # It is triggered when either input$subweight or input$file3 changes and 
  # Executes the plot_portfolio_weight() function
  shiny_plot_portfolio_weight <- eventReactive(list(input$subweight,input$file3), plot_portfolio_weight(get_weight()[[1]],file1_Status,as.character(input$selcat),as.Date(input$seldate)))
  
  # Observing changes in either input$subweight or input$file3
  # On event, it updates several PickerInputs and renders a DataTable
  observeEvent(list(input$subweight,input$file3), {
    output$plotw3Ftab <- DTtable(shiny_plot_fundamental()[[4]])
    
    # Updating the choices of the PickerInputs "selcat" and "seldate"
    updatePickerInput(session, "selcat", choices = colnames(file1_Status))
    updatePickerInput(session, "seldate", choices = as.character(unlist(get_weight()[[1]]$Date), use.names = FALSE))
  })
  
  # Observing changes to input$selcat and input$seldate to render and update the plotly plot, output$plotw3port
  observeEvent(input$selcat, output$plotw3port  <- renderPlotly(shiny_plot_portfolio_weight())) 
  observeEvent(input$seldate, output$plotw3port  <- renderPlotly(shiny_plot_portfolio_weight())) 
  
  # Creating another eventReactive object named risk
  # It gets triggered when either input$subweight or input$file3 changes, 
  # and after successful validation, it executes the all_performance_table() function
  risk <- eventReactive(list(input$subweight,input$file3),{
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    all_performance_table(file2_Historical, file3_Weight)
  })
  
  # Initializing a DataTable output to NULL
  output$plotw3risk <- DTtable(NULL) 
  
  
  # Observing changes in either input$subweight or input$file3.
  # On the event, it validates the inputs and then renders a DataTable
  # Adjusting numerical values to display up to 6 decimal places in the rendered DataTable
  observeEvent(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    output$plotw3risk <- DTtable({
      tabr <- risk();
      tabr[,sapply(tabr, class) %in% c('numeric','integer')] <- round(tabr[,sapply(tabr, class) %in% c('numeric','integer')],6);
      tabr
    })
  })
  
  # Initializing another Plotly plot to NULL
  output$plotw3riskplot <- renderPlotly(NULL) 
  
  # Creating a new eventReactive object shiny_plot_performance_table
  # It is triggered when either input$subweight or input$file3 changes and executes the plot_performance_table() function
  shiny_plot_performance_table <- eventReactive(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    plot_performance_table(risk())
  })
  
  # Observing changes in either input$subweight or input$file3
  # On event, it renders a Plotly plot after validating the inputs
  observeEvent(list(input$subweight,input$file3) , {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    output$plotw3riskplot <- renderPlotly(shiny_plot_performance_table()) #For updating the plot
  })
  
  # Initializing another Plotly plot to NULL
  output$radar <- renderPlotly(NULL)
  
  # Creating another eventReactive object shiny_plot_radar triggered by changes in input$subweight or input$file3
  # Executes the plot_radar() function upon activation
  shiny_plot_radar <- eventReactive(list(input$subweight,input$file3), {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    plot_radar(risk())
  })
  
  # Rendering the Plotly radar plot upon changes in either input$subweight or input$file3 after validation
  observeEvent(list(input$subweight,input$file3) , {
    shiny::validate(need((input$subweight >= 1)||(!is.null(input$file3)), "Data is needed"))
    output$radar <- renderPlotly(shiny_plot_radar()) #For updating the plot
  })
  
  # Initializing another Plotly output to NULL
  output$eff <- renderPlotly(NULL)  
  
  # Creating eventReactive object shiny_efficient_frontier triggered by changes in input$subeff
  # Executes the efficient_frontier() function upon activation
  shiny_efficient_frontier <- eventReactive(input$subeff, efficient_frontier(file2_Historical, file3_Weight))
  
  # Observing changes in either input$subweight or input$file3, validating, and updating the choices of the PickerInput "seldateeff"
  observeEvent(list(input$subweight, input$file3), {
    shiny::validate(need((input$subweight >= 1) || (!is.null(input$file3)), "Data is needed"))
    updatePickerInput(session, "seldateeff", choices = as.character(unlist(get_weight()[[1]]$Date), use.names = FALSE))
  })
  
  # Rendering and updating the efficient frontier plot when either input$subeff or input$seldateeff changes
  observeEvent(input$subeff, output$eff <- renderPlotly(plot_efficient_frontier(shiny_efficient_frontier()[[1]], file2_Historical, shiny_efficient_frontier()[[2]], input$seldateeff)))
  observeEvent(input$seldateeff, output$eff <- renderPlotly(plot_efficient_frontier(shiny_efficient_frontier()[[1]], file2_Historical, shiny_efficient_frontier()[[2]], input$seldateeff))) 
  
  
  
  # Observe if a file is uploaded through input$file4
  # Once a file is uploaded, it performs validation and reads the CSV file, checking for specific conditions and columns
  observeEvent(input$file4, {
    File <- input$file4
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    
    f4 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    cons <- req.cons.file(f4, c("Date", "Bound"))
    
    # Displaying different warning messages depending on the validation results
    # The file is read into file4_Schedule only if it meets all the conditions
    if(cons[1]){
      showFeedbackWarning(inputId = "file4", text = "Require at least one column to be numeric")
    } else if(cons[2]){
      showFeedbackWarning(inputId = "file4", text = "Missing the column of the names 'Date' or 'Bound'")
    } else if(cons[3]){
      showFeedbackWarning(inputId = "file4", text = "Missing Data Detected")
    } else{
      file4_Schedule  <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    }
  }, ignoreNULL = FALSE)
  
  # Observes the click event of input$subdiv
  # Ensures that files are uploaded in step 2, 3, and 4 before proceeding
  observeEvent(input$subdiv, {
    if((is.null(file1_Status)) || (is.null(file2_Historical)) || (is.null(file3_Weight))){
      noty(text = "Require Uploading Files in Step 2, 3, and 4 before", type = "warning")
    } else{
      file4_Schedule <<- div_schedule(as.integer(input$rate), c(as.numeric(input$m), as.numeric(input$a)), file3_Weight, as.Date(input$lastdate))
    }
  })
  
  # Observes changes in input$subweight or input$file3 and updates the PickerInput "lastdate" with the dates from get_weight()[[1]]$Date
  observeEvent(list(input$subweight, input$file3), {
    shiny::validate(need((input$subweight >= 1) || (!is.null(input$file3)), "Data is needed"))
    updatePickerInput(session, "lastdate", choices = as.character(unlist(get_weight()[[1]]$Date)[-1], use.names = FALSE)) #For updating the choice
  })
  
  # Initializing a DataTable to NULL
  output$plot4divtab <- DTtable(NULL)
  
  # Observes changes in either input$subdiv or input$file4
  # Renders the DataTable with file4_Schedule after performing validations
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((!is.null(file3_Weight)), ""))
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$plot4divtab <- DTtable({file4_Schedule})
  })
  
  
  # Initializing DataTables to NULL
  output$plot4divtabsum <- DTtable(NULL)
  output$plot4divpreview <- DTtable(NULL)
  
  # Rendering a Plotly plot for 'plot4divpreview' when 'input$subdivpreview' is triggered, after performing necessary validations
  observeEvent(input$subdivpreview, {
    shiny::validate(need((!is.null(file3_Weight)), ""))
    shiny::validate(need((input$subdivpreview >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$plot4divpreview <- renderPlotly(div_preview(as.integer(input$rate), c(as.numeric(input$m), as.numeric(input$a)), as.Date(input$lastdate), file3_Weight, file1_Status))
  })
  
  # Creating a reactive event that triggers with changes in 'input$subdiv' or 'input$file4', and it updates the progress bar and returns the result of 'div_weight' function
  shiny_div_weight <- eventReactive(list(input$subdiv, input$file4), {
    shiny::validate(need((!is.null(file3_Weight)), ""))
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    updateProgressBar(id = 'pb2', value = 0, total = 100, title = "Processing")
    div_weight(w = file3_Weight, Schedule = file4_Schedule, found = file1_Status)
  })
  
  # Rendering a DataTable 'plot4divtabsum' with the result of 'shiny_div_weight', and storing the result in 'file5_Weight_Div'
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((!is.null(file3_Weight)), ""))
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$plot4divtabsum <- DTtable({
      tab <- shiny_div_weight()
      file5_Weight_Div <<- tab
      tab[, 2:ncol(tab)] <- round(tab[, 2:ncol(tab)], 6)
      tab
    })
  })
  
  # Initializing Plotly plots to NULL
  output$plotw3InDiv1 <- renderPlotly(NULL)
  output$plotw3InDiv2 <- renderPlotly(NULL)
  
  # Creating an eventReactive element 'shiny_plot_div_Sch' that returns the plot data when 'input$subdiv' or 'input$file4' is changed, after performing necessary validations
  shiny_plot_div_Sch <- eventReactive(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    plot_div_Sch(file3_Weight, file1_Status, file4_Schedule, file5_Weight_Div)
  })
  
  # Rendering Plotly plots 'plotw3InDiv1' and 'plotw3InDiv2' with the results from 'shiny_plot_div_Sch' when 'input$subdiv' or 'input$file4' is changed
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$plotw3InDiv1 <- renderPlotly(shiny_plot_div_Sch()[[1]])
  })
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$plotw3InDiv2 <- renderPlotly(shiny_plot_div_Sch()[[2]])
  })
  
  # Initializing a Plotly plot to NULL
  output$plotw3div <- renderPlotly(NULL)
  
  
  # Creating an eventReactive object that triggers when `input$selcomdiv` changes
  # It calls the `asset_weight_plot_div` function with the required parameters
  shiny_asset_weight_plot_div <- eventReactive(input$selcomdiv, 
                                               asset_weight_plot_div(file3_Weight, file5_Weight_Div, as.character(input$selcomdiv)))
  
  # Observing for changes in `input$subdiv` or `input$file4`
  # Updates the `selcomdiv` PickerInput choices with column names from `shiny_div_weight()`, excluding the first one
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    updatePickerInput(session, "selcomdiv", choices = colnames(shiny_div_weight())[-1])
  })
  
  # Observing for changes in `input$selcomdiv`
  # Renders a Plotly plot using the result of `shiny_asset_weight_plot_div`
  observeEvent(input$selcomdiv, output$plotw3div <- renderPlotly(shiny_asset_weight_plot_div()))
  
  # Initializing a Plotly plot to `NULL`
  output$plotw3portdiv <- renderPlotly(NULL)
  
  # Creating an eventReactive object that triggers when `input$subdiv` or `input$file4` changes
  # Calls the `plot_portfolio_weight_div` function with the required parameters
  shiny_plot_portfolio_weight_div <- eventReactive(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    plot_portfolio_weight_div(file3_Weight, shiny_div_weight(), file1_Status, as.character(input$selcatdiv), as.Date(input$seldatediv))
  })
  
  # Observing for changes in `input$subdiv` or `input$file4`
  # Updates the `selcatdiv` and `seldatediv` PickerInput with the respective choices
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    updatePickerInput(session, "selcatdiv", choices = colnames(file1_Status))
    updatePickerInput(session, "seldatediv", choices = as.character(unlist(shiny_div_weight()$Date), use.names = FALSE))
  })
  
  # Observing for changes in `input$selcatdiv` or `input$seldatediv`
  # Renders a Plotly plot using the result of `plot_portfolio_weight_div` function
  observeEvent(input$selcatdiv, output$plotw3portdiv <- renderPlotly(plot_portfolio_weight_div(file3_Weight, shiny_div_weight(), file1_Status, as.character(input$selcatdiv), as.Date(input$seldatediv))))
  observeEvent(input$seldatediv, output$plotw3portdiv <- renderPlotly(plot_portfolio_weight_div(file3_Weight, shiny_div_weight(), file1_Status, as.character(input$selcatdiv), as.Date(input$seldatediv))))
  
  # Creating an eventReactive object that triggers when `input$subdiv` or `input$file4` changes
  # Calls the `all_performance_table` function with the required parameters
  riskdiv <- eventReactive(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    all_performance_table(file2_Historical, shiny_div_weight())
  })
  
  # Initializing a DataTable to `NULL`
  output$plotw3riskdiv <- DTtable(NULL)
  
  # Observing for changes in `input$subdiv` or `input$file4`
  # Renders a DataTable using the result of `riskdiv`, rounding numeric and integer columns to 6 decimal places
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$plotw3riskdiv <- DTtable({
      tabr <- riskdiv()
      tabr[, sapply(tabr, class) %in% c('numeric', 'integer')] <- round(tabr[, sapply(tabr, class) %in% c('numeric', 'integer')], 6)
      tabr
    })
  })
  
  # Initializing a Plotly plot to `NULL`
  output$plotw3riskplotdiv <- renderPlotly(NULL)
  
  
  # Creating an eventReactive object, triggered when `input$subdiv` or `input$file4` changes
  # Calls the `plot_performance_table_div` function with the results of `risk()` and `riskdiv()` as parameters
  shiny_plot_performance_table_div <- eventReactive(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    plot_performance_table_div(risk(), riskdiv())
  })
  
  # Observing for changes in `input$subdiv` or `input$file4`
  # Renders a Plotly plot using the result of `shiny_plot_performance_table_div` and assigning it to `output$plotw3riskplotdiv`
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$plotw3riskplotdiv <- renderPlotly(shiny_plot_performance_table_div())
  })
  
  # Creating an eventReactive object, triggered when `input$selcom` changes
  # Calls the `asset_weight_plot` function with the required parameters
  shiny_asset_weight_plot <- eventReactive(input$selcom, asset_weight_plot(get_weight()[[1]], as.character(input$selcom)))
  
  # Observing for changes in `input$subweight` or `input$file3`
  # Updates the `selcom` PickerInput choices with column names from `get_weight()[[1]]`, excluding the first one
  observeEvent(list(input$subweight, input$file3), {
    shiny::validate(need((input$subweight >= 1) || (!is.null(input$file3)), "Data is needed"))
    updatePickerInput(session, "selcom", choices = colnames(get_weight()[[1]])[-1])
  })
  
  # Observing for changes in `input$selcom`
  # Renders a Plotly plot using the result of `shiny_asset_weight_plot`
  observeEvent(input$selcom, output$plotw3 <- renderPlotly(shiny_asset_weight_plot()))
  
  # Initializing multiple Plotly plots and a DataTable to `NULL`
  output$plotw3Fdiv <- renderPlotly(NULL)
  output$plotw3Fshortdiv <- renderPlotly(NULL)
  output$plotw3Flongdiv <- renderPlotly(NULL)
  output$plotw3Ftabdiv <- renderDT(NULL)
  
  # Creating an eventReactive object, triggered when `input$subdiv` or `input$file4` changes
  # Calls the `plot_fundamental_div` function with the required parameters
  shiny_plot_fundamental_div <- eventReactive(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    plot_fundamental_div(file3_Weight, file5_Weight_Div, file1_Status)
  })
  
  # Observing for changes in `input$subdiv` or `input$file4`
  # Rendering multiple Plotly plots and a DataTable using the result of `shiny_plot_fundamental_div`
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$plotw3Fdiv <- renderPlotly(shiny_plot_fundamental_div()[[1]]) # Rendering the first plot from the list returned by shiny_plot_fundamental_div
    output$plotw3Fshortdiv <- renderPlotly(shiny_plot_fundamental_div()[[3]]) # Rendering the third plot from the list returned by shiny_plot_fundamental_div
    output$plotw3Flongdiv <- renderPlotly(shiny_plot_fundamental_div()[[2]]) # Rendering the second plot from the list returned by shiny_plot_fundamental_div
    output$plotw3Ftabdiv <- DTtable(shiny_plot_fundamental_div()[[4]]) # Rendering the fourth DataTable from the list returned by shiny_plot_fundamental_div
  })
  
  # Initializing a Plotly plot to NULL
  output$radardiv <- renderPlotly(NULL)
  
  # Creating an eventReactive object, triggered when `input$subdiv` or `input$file4` changes
  # Calls the `plot_radar` function with the required parameters
  shiny_plot_radar_div <- eventReactive(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    plot_radar(risk(), riskdiv())
  })
  
  # Observing for changes in `input$subdiv` or `input$file4`
  # Renders a Plotly plot using the result of `shiny_plot_radar_div`
  observeEvent(list(input$subdiv, input$file4), {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$radardiv <- renderPlotly(shiny_plot_radar_div()) # For update plot
  })
  
  # Initializing a DataTable and a Plotly plot to NULL
  output$plotdiftable <- renderDT(NULL)
  output$plotdifboxplot <- renderPlotly(NULL)
  
  # Creating an eventReactive object, triggered when `input$sub.ly.st5` changes
  # Calls the `diff_summary` function with the required parameters
  shiny_diff_summary <- eventReactive(input$sub.ly.st5, {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    diff_summary(risk(), riskdiv(), shiny_plot_fundamental()[[4]], shiny_plot_fundamental_div()[[4]], as.numeric(input$ly.st5))
  })
  
  # Observing for changes in `input$sub.ly.st5`
  # Renders a DataTable and a Plotly plot using the result of `shiny_diff_summary`
  observeEvent(input$sub.ly.st5, {
    shiny::validate(need((input$subdiv >= 1) || (!is.null(input$file4)), "data required to be uploaded"))
    output$plotdiftable <- DTtable(cbind(Date = shiny_diff_summary()[[1]][, 1], round(shiny_diff_summary()[[1]][, -1], 6))) # For update DataTable
    output$plotdifboxplot <- renderPlotly(shiny_diff_summary()[[2]]) # For update plot
  })
  
  
  # Initializing DataTable to NULL for 'div_dynamic', 'hist_return', and 'found'
  output$div_dynamic <- DTtable(NULL)
  output$hist_return <- DTtable(NULL)
  output$found <- DTtable(NULL)
  
  # Observing for any event (or change) in 'input$file6'
  observeEvent(input$file6, {
    
    File <- input$file6  # Getting the uploaded file
    
    shiny::validate(
      need(File != "", "No data has been uploaded") # Validating that a file has been uploaded
    )
    
    f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)  # Reading the uploaded CSV file
    
    cons <- req.cons.file(f6, c("Date", "PORTNAME"))  # Custom function call, possibly to validate the existence of "Date" and "PORTNAME" columns and maybe other conditions
    
    # Check if the first condition in 'cons' is TRUE and show a warning feedback message if so
    if(cons[1]){
      showFeedbackWarning(
        inputId = "file6",
        text = "Require at least one column to be numeric")
      
      # If the first condition is FALSE, check if the second condition in 'cons' is TRUE and show a different warning feedback message if so  
    } else if(cons[2]){
      showFeedbackWarning(
        inputId = "file6",
        text = "Missing the column of the names 'PORTNAME' or 'Date")
      
      # If the first two conditions are FALSE, check if the third condition in 'cons' is TRUE and show another warning feedback message if so  
    } else if(cons[3]){
      showFeedbackWarning(
        inputId = "file6",
        text = "Missing Data Detected")
      
      # If none of the conditions in 'cons' are TRUE, read the file and assign it to 'file6_Div_Dynamic' and render it as a DataTable 'div_dynamic'   
    } else {
      file6_Div_Dynamic <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      output$div_dynamic <- DTtable(file6_Div_Dynamic)
    }
    
  }, ignoreNULL = FALSE)  # 'ignoreNULL' is set to FALSE, meaning this observeEvent will trigger even if 'input$file6' is NULL when the app is first started
  
  
  # Observe for any event (or change) in 'input$file7'
  observeEvent(input$file7, {
    File <- input$file7  # Getting the uploaded file
    
    # Validating that a file has been uploaded
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    
    f7 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)  # Reading the uploaded CSV file
    cons <- req.cons.file(f7, c("Ticker", "Status"))  # Custom function call, possibly validating the existence of "Ticker" and "Status" columns and maybe other conditions
    
    # Checking and displaying appropriate warnings based on the conditions in 'cons'
    # Displaying DataTable 'found' if no warnings
    # Similar structure followed for 'input$file8'
    
    # ... [rest of the code here is similar to the previous comment, with different variable names and text messages]
    
  }, ignoreNULL = FALSE)  # This observeEvent will trigger even if 'input$file7' is NULL when the app is first started
  
  # Observe for any event (or change) in 'input$file8'
  observeEvent(input$file8, {
    # Similar validation, reading, condition-checking, and DataTable displaying structure as above, just for a different input file
    # ... [Code here is structured similarly as above, just with different variable names and text messages]
  }, ignoreNULL = FALSE)
  
  # Initializing Plotly plots to NULL
  output$plotcom1 <- renderPlotly(NULL)
  
  # Observing for any event (or change) in 'input$subcomp' and updating the PickerInput 'selcomcom' choices and rendering a Plotly plot 'plotcom1'
  observeEvent(input$subcomp, updatePickerInput(session, "selcomcom", choices = colnames(file6_Div_Dynamic)[-c(1, ncol(file6_Div_Dynamic))]))  # For update choice
  observeEvent(input$subcomp, output$plotcom1 <- renderPlotly(multicomp(file6_Div_Dynamic, as.character(input$selcomcom))))  # For update plot
  
  # Initializing more Plotly plots to NULL
  output$plotcom2 <- renderPlotly(NULL)
  output$plotcom3 <- renderPlotly(NULL)
  
  # Defining an event reactive expression 'shiny_plot_div_Sch_comp' that triggers on change in 'input$subcomp' and returns a plot
  shiny_plot_div_Sch_comp <- eventReactive(input$subcomp, plot_div_Sch_comp(file6_Div_Dynamic, file7_Attribute))
  
  # Observing for any event (or change) in 'input$subcomp' and rendering Plotly plots 'plotcom2' and 'plotcom3'
  observeEvent(input$subcomp, output$plotcom2 <- renderPlotly(shiny_plot_div_Sch_comp()[[1]]))  # For update plot
  observeEvent(input$subcomp, output$plotcom3 <- renderPlotly(shiny_plot_div_Sch_comp()[[2]]))  # For update plot
  
  
  # Initializing several Plotly plots and DataTables to NULL
  output$plotcom4 <- renderPlotly(NULL)
  output$plotcom5 <- renderPlotly(NULL)
  output$plotcom6 <- renderPlotly(NULL)
  output$plotcom7 <- renderPlotly(NULL)
  output$plotcom8 <- renderPlotly(NULL)
  output$tab1 <- DTtable(NULL)
  output$tab2 <- DTtable(NULL)
  
  # Creating an eventReactive object 'shiny_compareplot' that gets executed when 'input$subcomp' changes
  shiny_compareplot <- eventReactive(input$subcomp, compareplot(file8_Return, file6_Div_Dynamic, file7_Attribute))
  
  # Observing 'input$subcomp' and updating various plotly plots and DataTables accordingly
  # For each observeEvent, 'shiny_compareplot' gets re-executed and respective plots and tables get updated
  observeEvent(input$subcomp, {
    output$plotcom4 <- renderPlotly(shiny_compareplot()[[3]])  # Updating respective Plotly plot
    # ... Similar code for other plots and tables
  })
  
  # Observing 'input$subcomp' and updating PickerInput 'selben.op1'
  observeEvent(input$subcomp, updatePickerInput(session, inputId = 'selben.op1', choices = unique(file6_Div_Dynamic$PORTNAME)))
  
  # Defining eventReactive for 'input$submul' that executes a function and returns results based on the current state of inputs
  shiny_diff_summary_mul <- eventReactive(input$submul, {
    diff_summary_mul(shiny_compareplot()[[1]], shiny_compareplot()[[2]], as.character(input$selben.op1), as.numeric(input$ly.opt1))
  })
  
  # Observing 'input$submul' and updating DataTable and Plotly plot accordingly
  observeEvent(input$submul, {
    output$tabben.op1 <- DTtable(round_tab(shiny_diff_summary_mul()[[1]]))  # Updating DataTable
    output$plotben.op1 <- renderPlotly(shiny_diff_summary_mul()[[2]])  # Updating Plotly plot
  })
  
  # Observing 'input$file6.op2' for uploaded file and performing validation, file reading, and DataTable displaying
  observeEvent(input$file6.op2, {
    File <- input$file6.op2  # Getting the uploaded file
    
    # Validating that a file has been uploaded
    shiny::validate(
      need(File != "", "No data has been uploaded")
    )
    
    f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)  # Reading the uploaded CSV file
    cons <- req.cons.file(f6, c("Date", "PORTNAME"))  # Custom function call, possibly validating the existence of "Date" and "PORTNAME" columns and maybe other conditions
    
    # Displaying appropriate warnings or DataTable based on the conditions in 'cons'
    if(cons[1]) {
      showFeedbackWarning(inputId = "file6.op2", text = "Require at least one column to be numeric")
    } else if(cons[2]) {
      showFeedbackWarning(inputId = "file6.op2", text = "Missing the columns of the name 'Date' or 'PORTNAME'")
    } else if(cons[3]) {
      showFeedbackWarning(inputId = "file6.op2", text = "Missing Data Detected")
    } else {
      file6_Div_Dynamic <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)  # Reading the file again and updating global variable
      output$tab.op2 <- DTtable(round_tab(file6_Div_Dynamic))  # Updating DataTable
    }
  }, ignoreNULL = FALSE)  # This observeEvent will trigger even if 'input$file6.op2' is NULL when the app is first started
  
  
  
  
  # Observe when a file is uploaded to 'input$file8.op2'
  observeEvent(input$file8.op2, {
    # Get the uploaded file
    File <- input$file8.op2
    
    # Validate whether a file has been uploaded
    shiny::validate(need(File != "", "No data has been uploaded"))
    
    # Read the uploaded file as a CSV
    f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    
    # Perform some custom validation on the uploaded file
    cons <- req.cons.file(f6, c("Date"))
    
    # Show feedback warnings based on the result of the custom validation
    if (cons[1]) {
      showFeedbackWarning(inputId = "file8.op2", text = "Require at least one column to be numeric")
    } else if (cons[2]) {
      showFeedbackWarning(inputId = "file8.op2", text = "Missing the column of the name 'Date'")
    } else if (cons[3]) {
      showFeedbackWarning(inputId = "file8.op2", text = "Missing Data Detected")
    } else {
      # If the uploaded file passes all validations, read the file again and update the global variable and DataTable
      file8_Return <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      output$tab.op2.2 <- DTtable(round_tab(file8_Return))
    }
  }, ignoreNULL = FALSE)  # Trigger this observer even when 'input$file8.op2' is initially NULL
  
  # Create an eventReactive object 'shiny_clust.option' that executes when 'input$subclust' is triggered
  shiny_clust.option <- eventReactive(input$subclust, {
    clust.option(file6_Div_Dynamic, file8_Return, as.numeric(input$tau.op2), as.numeric(input$sept.op2))
  })
  
  # Initialize some Plotly plots to NULL
  output$op2graph <- renderPlotly(NULL)
  output$op2hm <- renderPlotly(NULL)
  
  # Observe when 'input$subclust' is triggered and update Plotly plots and DataTables accordingly
  observeEvent(input$subclust, {
    output$op2graph <- renderPlotly(shiny_clust.option()[[3]])
    output$op2hm <- renderPlotly(shiny_clust.option()[[4]])
    output$op2tab1 <- renderDT(round_tab(shiny_clust.option()[[1]]))
    output$op2tab2 <- renderDT(round_tab(shiny_clust.option()[[2]]))
  })
  
  
  # Setup an observer to react when a file is uploaded in the Shiny app to 'input$file6.op3'
  observeEvent(input$file6.op3, {
    # Store the uploaded file into 'File' variable
    File <- input$file6.op3
    
    # Validate if a file has been uploaded; if not, notify the user
    shiny::validate(need(File != "", "No data has been uploaded"))
    
    # Read the uploaded file into a dataframe 'f6'
    f6 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    
    # Check the conditions/validations defined in 'req.cons.file' function on the dataframe 'f6'
    cons <- req.cons.file(f6, c("Date","PORTNAME"))
    
    # If the first condition in 'cons' is TRUE, show a warning message notifying that at least one numeric column is required
    if(cons[1]){
      showFeedbackWarning(inputId = "file6.op3", text = "Require at least on column to be numeric")
    }
    # If the second condition in 'cons' is TRUE, show a warning message notifying that the columns 'Date' or 'PORTNAME' are missing
    else if(cons[2]){
      showFeedbackWarning(inputId = "file6.op3", text = "Missing the columns of the name 'Date' or 'PORTNAME'")
    }
    # If the third condition in 'cons' is TRUE, show a warning message notifying that there is missing data detected in the uploaded file
    else if(cons[3]){
      showFeedbackWarning(inputId = "file6.op3", text = "Missing Data Detected")
    }
    # If none of the above conditions are TRUE, re-read the uploaded file and store it in a global variable 'file6_Div_Dynamic'
    # Then, display the contents of 'file6_Div_Dynamic' in a DataTable in the Shiny app
    else{
      file6_Div_Dynamic <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      output$tab.op3 <- DTtable(round_tab(file6_Div_Dynamic))
    }
    
    # Update the choices available in the picker inputs 'gdate1', 'gdate2', and 'gdate3' in the Shiny app
    # based on the unique 'Date' values found in 'file6_Div_Dynamic'
    updatePickerInput(session, inputId = "gdate1", choices = as.list(1:length(unique(file6_Div_Dynamic$Date))) %>% `names<-`(unique(file6_Div_Dynamic$Date)))
    updatePickerInput(session, inputId = "gdate2", choices = as.list(1:length(unique(file6_Div_Dynamic$Date))) %>% `names<-`(unique(file6_Div_Dynamic$Date)))
    updatePickerInput(session, inputId = "gdate3", choices = as.list(1:length(unique(file6_Div_Dynamic$Date))) %>% `names<-`(unique(file6_Div_Dynamic$Date)))
  }, ignoreNULL = FALSE) # Execute the observer even if 'input$file6.op3' is initially NULL
  
  
  
  # Observes the 'input$file7.op3' for any event (file upload) in Shiny app.
  observeEvent(input$file7.op3, {
    # Stores the uploaded file information in 'File'.
    File <- input$file7.op3
    
    # Validates if any file has been uploaded; if not, notifies the user.
    shiny::validate(need(File != "", "No data has been uploaded"))
    
    # Reads the uploaded file into dataframe 'f7' without modifying the column names.
    f7 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    
    # Applies 'req.cons.file' function to check the existence of 'Ticker' and 'Status' columns in 'f7' dataframe.
    cons <- req.cons.file(f7, c("Ticker","Status"))
    
    # If 'Ticker' or 'Status' columns are missing, show a warning message to the user.
    if(cons[2]){
      showFeedbackWarning(inputId = "file7.op3", text = "Missing the columns of the name 'Ticker' or 'Status'")
    }
    # If there is any missing data in the uploaded file, show a warning message to the user.
    else if(cons[3]){
      showFeedbackWarning(inputId = "file7.op3", text = "Missing Data Detected")
    }
    # If there are no issues with the uploaded file, read it again and store it globally in 'file7_Attribute',
    # and display its contents in a DataTable in the Shiny app.
    else{
      file7_Attribute <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      output$tab.op3.1 <- DTtable(round_tab(file7_Attribute))
    }
    
  }, ignoreNULL = FALSE) # Execute the observer even if 'input$file7.op3' is initially NULL.
  
  
  # Observes 'input$file8.op3' for any event (usually file upload) in the Shiny app.
  observeEvent(input$file8.op3, {
    # Stores the uploaded file information in 'File'.
    File <- input$file8.op3
    
    # Validates if any file has been uploaded; if not, notifies the user.
    shiny::validate(need(File != "", "No data has been uploaded"))
    
    # Reads the uploaded file into dataframe 'f8' without modifying the column names.
    f8 <- read.csv(File$datapath, header = TRUE, check.names = FALSE)
    
    # Applies 'req.cons.file' function to check the existence of 'Date' column in 'f8' dataframe and numeric type constraint.
    cons <- req.cons.file(f8,c("Date"))
    
    # If the uploaded file does not contain at least one numeric column, show a warning message to the user.
    if(cons[1]){
      showFeedbackWarning(inputId = "file8.op3", text = "Require at least one column to be numeric")
    }
    # If 'Date' column is missing, show a warning message to the user.
    else if(cons[2]){
      showFeedbackWarning(inputId = "file8.op3", text = "Missing the columns of the name 'Date'")
    }
    # If there is any missing data in the uploaded file, show a warning message to the user.
    else if(cons[3]){
      showFeedbackWarning(inputId = "file8.op3", text = "Missing Data Detected")
    }
    # If there are no issues with the uploaded file, read it again and store it globally in 'file8_Return',
    # and display its contents in a DataTable in the Shiny app.
    else{
      file8_Return <<- read.csv(File$datapath, header = TRUE, check.names = FALSE)
      output$tab.op3.2 <- DTtable(round_tab(file8_Return))
    }
    
  }, ignoreNULL = FALSE) # Execute the observer even if 'input$file8.op3' is initially NULL.
  
  
  # Define a reactive event when 'input$subgdate1' is triggered.
  shiny_get.graph.1 <- eventReactive(input$subgdate1, {
    # Initialize the progress bar with id 'pbg1'.
    updateProgressBar(id = 'pbg1', value = 0, total = 100, title = "Processing")
    
    # Store relevant data frames in local variables.
    div_dynamic <- file6_Div_Dynamic
    return_mat <- file8_Return
    
    # Convert 'input$gdate1' to integer and store it in 'data.index'.
    data.index <- as.integer(input$gdate1)
    
    # Store 'file7_Attribute' in 'found'.
    found <- file7_Attribute
    
    # Extract unique 'PORTNAME' and store them in 'nameport'.
    nameport <- unique(div_dynamic$PORTNAME)
    
    # Convert 'PORTNAME' to factor with levels being the unique 'PORTNAME's.
    div_dynamic$PORTNAME <- factor(div_dynamic$PORTNAME, levels = unique(nameport))
    
    # Perform several transformations and calculations on 'div_dynamic' and store the result in 'w_list'.
    w_list <- div_dynamic %>% 
      group_split(PORTNAME) %>% 
      lapply(function(x) {x %>% dplyr::select(-PORTNAME) %>% multipywtoreturn(return_mat)})
    names(w_list) <- nameport
    
    # Extract the 'data.index'th element from each list in 'w_list' and store them in 'sel.port'.
    sel.port <- lapply(w_list, function(x) x[data.index]) %>% unlist(recursive=FALSE)
    
    # Apply 'sturcture.cov' to each list in 'sel.port' and store the result in 'list.cov'.
    list.cov <- sel.port %>% lapply(sturcture.cov)
    
    # Set up the plotting layout with number of rows being the length of 'nameport'.
    par(mfrow=c(length(nameport),1))
    
    # Loop over each list in 'list.cov' and plot them using 'plot.qgraph', and update the progress bar after each plot.
    for(i in seq_along(list.cov)){
      plot.qgraph(list.cov[[i]], names(sel.port)[i], found)
      title <- ifelse(i == length(list.cov), "Done", "Calculating, please wait")
      updateProgressBar(id = 'pbg1', value = i, total = length(list.cov), title = title)
    }
  })
  
  # Similar reactive events for 'input$subgdate2' and 'input$subgdate3'.
  # The logic inside these reactive events is similar to the one described above.
  shiny_get.graph.2 <- eventReactive(input$subgdate2, {
    # Similar processes as shiny_get.graph.1 are carried out with respective changes in variable names and input.
    updateProgressBar(id = 'pbg2', value = 0, total = 100, title = "Processing")
    div_dynamic <- file6_Div_Dynamic
    return_mat <- file8_Return 
    data.index <- as.integer(input$gdate2)
    found <- file7_Attribute
    nameport <- unique(div_dynamic$PORTNAME)
    div_dynamic$PORTNAME <- factor(div_dynamic$PORTNAME, levels = unique(nameport))
    w_list <- div_dynamic %>% group_split(PORTNAME) %>% lapply(function(x) {x %>% dplyr::select(-PORTNAME) %>% multipywtoreturn(return_mat)})
    names(w_list) <- nameport
    sel.port <- lapply(w_list, function(x) x[data.index]) %>% unlist(recursive=FALSE)
    list.cov <- sel.port %>% lapply(sturcture.cov)
    par(mfrow=c(length(nameport),1))
    for(i in seq_along(list.cov)){
      plot.qgraph(list.cov[[i]], names(sel.port)[i], found)
      title <- ifelse(i == length(list.cov), "Done", "Calculating, please wait")
      updateProgressBar(id = 'pbg2', value = i, total = length(list.cov), title = title)
    }
  })
  
  shiny_get.graph.3 <- eventReactive(input$subgdate3, {
    # Similar processes as shiny_get.graph.1 are carried out with respective changes in variable names and input.
    updateProgressBar(id = 'pbg3', value = 0, total = 100, title = "Processing")
    div_dynamic <- file6_Div_Dynamic
    return_mat <- file8_Return 
    data.index <- as.integer(input$gdate3)
    found <- file7_Attribute
    nameport <- unique(div_dynamic$PORTNAME)
    div_dynamic$PORTNAME <- factor(div_dynamic$PORTNAME, levels = unique(nameport))
    w_list <- div_dynamic %>% group_split(PORTNAME) %>% lapply(function(x) {x %>% dplyr::select(-PORTNAME) %>% multipywtoreturn(return_mat)})
    names(w_list) <- nameport
    sel.port <- lapply(w_list, function(x) x[data.index]) %>% unlist(recursive=FALSE)
    list.cov <- sel.port %>% lapply(sturcture.cov)
    par(mfrow=c(length(nameport),1))
    for(i in seq_along(list.cov)){
      plot.qgraph(list.cov[[i]], names(sel.port)[i], found)
      title <- ifelse(i == length(list.cov), "Done", "Calculating, please wait")
      updateProgressBar(id = 'pbg3', value = i, total = length(list.cov), title = title)
    }
  })
  
  # Observe if 'input$subgdate1' is triggered and update 'output$op3.1' with the rendered plot from 'shiny_get.graph.1'.
  observeEvent(input$subgdate1, output$op3.1 <- renderPlot(shiny_get.graph.1())) #For update plot
  
  # Similar to above, but for 'input$subgdate2' and 'output$op3.2' with 'shiny_get.graph.2'.
  observeEvent(input$subgdate2, output$op3.2 <- renderPlot(shiny_get.graph.2())) #For update plot
  
  # Similar to above, but for 'input$subgdate3' and 'output$op3.3' with 'shiny_get.graph.3'.
  observeEvent(input$subgdate3, output$op3.3 <- renderPlot(shiny_get.graph.3())) #For update plot
  
  # Defines a reactive event when 'input$selindex' is triggered and maps the index.
  shiny_index_map <- eventReactive(input$selindex, map_index(input$selindex))
  
  # Observe if 'input$selindex' is triggered and update 'output$map' with the first element from 'shiny_index_map'.
  observeEvent(input$selindex, output$map <- renderPlot(shiny_index_map()[[1]]))
  
  # Observe if 'input$subindex' is triggered and update the order input 'source' with the second element from 'shiny_index_map'.
  observeEvent(input$subindex, {
    updateOrderInput(session, inputId = "source", items = (shiny_index_map()[[2]]), item_class = 'info')
  })
  
  # Observe if 'input$subnahandle' is triggered and perform several operations and updates if 'file1_Status' is not NULL.
  observeEvent(input$subnahandle, {
    shiny::validate(need(!is.null(file1_Status), " "))
    file2_Historical <<- NA_handle(file2_Historical, as.integer(input$selmethod))
    output$tab_return <- DTtable(file2_Historical)
    output$reportreturn <- DTtable(return_summary_update(file2_Historical))
    output$naheatmap <- renderPlotly(NA_heatmap(file2_Historical))
  })
  
  # Initially set 'output$naheatmap' to NULL.
  output$naheatmap <- renderPlotly(NULL)
  
  
}

######################################################################################################
########################################Run APP#######################################################
######################################################################################################
shinyApp(ui, server, options = list(launch.browser = T))