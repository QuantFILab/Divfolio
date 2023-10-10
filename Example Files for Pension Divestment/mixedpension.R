#R script for Mixed Pension Funds divestment study

library(tidyverse)
library(xtable)
library(viridis)
library(gridExtra)
path <- "https://raw.githubusercontent.com/QuantFILab/Divfolio/main/Example%20Files%20for%20Pension%20Divestment"
####################################################################################################################################
############################################Functions##############################################################################
####################################################################################################################################

#Function for calculating the return for all time points 
#Input:  Files F3 and F2
#Output: Return of rebalancing portfolio all time points
return.line <- function(F3,F2){
  rebdate <- F3$Date
  returnmat <- lapply(1:(length(rebdate)), 
                      function(i){
                        if(i < length(rebdate)){
                          F2 %>% dplyr::filter((as.Date(Date) >= as.Date(rebdate[i])) & (as.Date(Date) < as.Date(rebdate[i+1])))
                          }else{
                            F2 %>% dplyr::filter(as.Date(Date) >= as.Date(rebdate[i]))}
                        }
                      )
  ret <- lapply(seq_along(rebdate), 
                function(i) apply(returnmat[[i]][,-1],1, function(x) x*F3[i,-1]) %>% bind_rows()) %>% bind_rows()
  ret.final <- data.frame(
    Date = F2$Date[as.Date(F2$Date) >= as.Date(head(F3$Date,1))], 
    Return = rowSums(ret), check.names = F) 
  return(ret.final)
}

#Optimal Funding Weight for Mixed Pension
w.funding <-function(funded, unfunded, risk.averse){
  funded.num <- column_to_rownames(funded, 'Date') %>% unlist()
  unfunded.num <- column_to_rownames(unfunded, 'Date') %>% unlist()
  mu.r <- mean(funded.num)
  mu.g <- mean(unfunded.num)
  var.g <- var(unfunded.num)
  cov.rg <- cov(unfunded.num, funded.num)
  var.rmg <- var.r + var.g - 2*cov.rg
  wf <- (mu.r - mu.g + risk.averse*(var.g - cov.rg))/(risk.averse*var.rmg)
  return(wf)
}

######################################################################################################################################
#########################################Data Preparation Codes############################################################################
######################################################################################################################################


us.ret <- read.csv(paste0(path,"/F2US.csv"), check.names = F) %>% `rownames<-`(NULL)
uk.ret <- read.csv(paste0(path,"/F2UK.csv"), check.names = F) %>% `rownames<-`(NULL)

us.w.ben <- read.csv(paste0(path,"/F3US.csv"), check.names = F) %>% `rownames<-`(NULL)
uk.w.ben <- read.csv(paste0(path,"/F3UK.csv"), check.names = F) %>% `rownames<-`(NULL)

us.w.inst <- read.csv(paste0(path,"/F4USInst.csv"), check.names = F) %>% `rownames<-`(NULL)
uk.w.inst <- read.csv(paste0(path,"/F4UKInst.csv"), check.names = F) %>% `rownames<-`(NULL)

us.w.slow <- read.csv(paste0(path,"/F4USSlow.csv"), check.names = F) %>% `rownames<-`(NULL)
uk.w.slow <- read.csv(paste0(path,"/F4UKSlow.csv"), check.names = F) %>% `rownames<-`(NULL)


us.ben <- return.line(us.w.ben, us.ret)
us.inst <- return.line(us.w.inst, us.ret)
us.slow <- return.line(us.w.slow, us.ret)
us.port <- merge(us.ben, us.inst, by = 'Date')
us.port <- merge(us.port, us.slow, by = 'Date') 
colnames(us.port) <- c('Date',"USBen","USInst","USSlow")

uk.ben <- return.line(uk.w.ben, uk.ret)
uk.inst <- return.line(uk.w.inst, uk.ret)
uk.slow <- return.line(uk.w.slow, uk.ret)
uk.port <- merge(uk.ben, uk.inst, by = 'Date')
uk.port <- merge(uk.port, uk.slow, by = 'Date') 
colnames(uk.port) <- c('Date',"UKBen","UKInst","UKSlow")

gdp <- read.csv(paste0(path,"/gdpfin.csv")) %>% `rownames<-`(NULL)
gdp.port <- merge(gdp, us.port)
gdp.port <- merge(gdp.port, uk.port)
write.csv(gdp.port, paste0(path,"/gdpandport.csv"), row.names = F)



######################################################################################################################################
#########################################Calculation Codes############################################################################
######################################################################################################################################

gdp.port.fund <- read.csv(paste0(path,"/gdpandport.csv"))

#Summary Mean Variance, Covariance and Correlation 
gdp.port.fund <- gdp.port.fund[,c(1,2,3,4,6,5,7,9,8)]
round(apply(gdp.port.fund[,-1], 2, mean)*100,2)
round(apply(gdp.port.fund[,-1]*100, 2, var),2)
round(cov(gdp.port.fund[,-1]*100)[1:2,-(1:2)],2)
round(cor(gdp.port.fund[,-1]*100)[1:2,-(1:2)],2)


#Summary Founding wight Risk Aversion
risk.av <- c(0.001,0.002,0.004)
us.tab <- data.frame(c("PEW","Slow","Instant"))
for(j in risk.av){
  w.fund.us <- c()
  mix.ret.us <- c()
  for(i in 4:6){
    w.fund <- w.funding(gdp.port[,c(1,i)], gdp.port[,c(1,2)], j)
    mix.ret <- w.fund*gdp.port[,i] + (1-w.fund)*gdp.port[,2] 
    w.fund.us <- c(w.fund.us,  w.fund)
    mix.ret.us <- cbind(mix.ret.us, mix.ret)
  }
  us.names <- c("USBen","USSlow","USInst")
  colnames(mix.ret.us) <- us.names 
  names(w.fund.us) <- us.names 
  df.tab <- data.frame(w = round(w.fund.us*100,2),
                       mu = round(apply(mix.ret.us*100, 2, mean),2),
                       var = round(apply(mix.ret.us*100, 2, var),2),
                       sharpe = round(apply(mix.ret.us*100, 2, function(x) mean(x)/sd(x)),2)
                       )  
  us.tab <- data.frame(us.tab,df.tab)
}

xtable(us.tab)

 
risk.av <- c(0.001,0.002,0.004)
uk.tab <- data.frame(c("PEW","Slow","Instant"))

for(j in risk.av){
  w.fund.uk <- c()
  mix.ret.uk <- c()
  for(i in 7:9){
    w.fund <- w.funding(gdp.port[,c(1,i)], gdp.port[,c(1,3)], j)
    mix.ret <- w.fund*gdp.port[,i] + (1-w.fund)*gdp.port[,3] 
    w.fund.uk <- c(w.fund.uk,  w.fund)
    mix.ret.uk <- cbind(mix.ret.uk, mix.ret)
  }
  uk.names <- c("UKBen","UKSlow","UKInst")
  colnames(mix.ret.uk) <- uk.names
  names(w.fund.uk) <- uk.names 
df.tab <- data.frame(w = round(w.fund.uk*100,2),
                     mu = round(apply(mix.ret.uk*100, 2, mean),2),
                     var = round(apply(mix.ret.uk*100, 2, var),2),
                     sharpe = round(apply(mix.ret.uk*100, 2, function(x) mean(x)/sd(x)),2)
)  
uk.tab <- data.frame(uk.tab,df.tab)
}

xtable(uk.tab)


#Plots of Risk reversion VS return

risk.av <- c()
for(i in 1:500){
  risk.av[i] <- 0.0005*2*i
}

us.tab <- c()
for(j in risk.av){
  w.fund.us <- c()
  mix.ret.us <- c()
  for(i in 4:6){
    w.fund <- w.funding(gdp.port[,c(1,i)], gdp.port[,c(1,2)], j)
    mix.ret <- w.fund*gdp.port[,i] + (1-w.fund)*gdp.port[,2] 
    w.fund.us <- c(w.fund.us,  w.fund)
    mix.ret.us <- cbind(mix.ret.us, mix.ret)
  }
  df.tab <- data.frame(w = round(w.fund.us*100,2),
                       mu = round(apply(mix.ret.us*100, 2, mean),2),
                       var = round(apply(mix.ret.us*100, 2, var),2),
                       sharpe = round(apply(mix.ret.us*100, 2, function(x) mean(x)/sd(x)),2),
                       Port = c("PEW","Slow","Inst"),
                       risk = rep(j,3)
  )  
  us.tab <- rbind(us.tab,df.tab)
}


us.tab$Port <- factor(us.tab$Port, levels = c("PEW","Slow","Inst"))

us.plot <- ggplot(data = us.tab, aes(x = log(risk), y = sharpe, group = Port, color = Port)) +
  geom_point(size = 3, shape = 19, alpha = 0.7) +  # larger points with some transparency
  geom_line(size = 1.05, linetype = "solid") +  # solid line
  scale_color_viridis(discrete = TRUE, name = "Portfolio") +  # set a legend title
  theme_minimal() +
  labs(
    x = "Log of Risk Aversion", 
    y = "Sharpe Ratio",
    title = "Impact of Risk Aversion on Sharpe Ratio",  # adding a title
    subtitle = "US Mixed Pension Porfolio"  # and a subtitle
  ) +
  theme(
    text = element_text(size = 12),  # adjust text size
    axis.text = element_text(face = "bold", size = 12),  # make axis labels bold and slightly larger
    panel.grid.minor = element_blank(),  # remove minor grid
    panel.grid.major = element_line(color = "grey90"),  # lighten the major grid lines
    legend.position = "bottom"  # move legend to the bottom
  )


us.tab <- c()
for(j in risk.av){
  w.fund.us <- c()
  mix.ret.us <- c()
  for(i in 4:6){
    w.fund <- w.funding(gdp.port[,c(1,i)], gdp.port[,c(1,2)], j)
    mix.ret <- w.fund*gdp.port[,i] + (1-w.fund)*gdp.port[,2] 
    w.fund.us <- c(w.fund.us,  w.fund)
    mix.ret.us <- cbind(mix.ret.us, mix.ret)
  }
  df.tab <- data.frame(w = round(w.fund.us*100,2),
                       mu = round(apply(mix.ret.us*100, 2, mean),2),
                       var = round(apply(mix.ret.us*100, 2, var),2),
                       sharpe = round(apply(mix.ret.us*100, 2, function(x) mean(x)/sd(x)),2),
                       Port = c("PEW","Slow","Inst"),
                       risk = rep(j,3)
  )  
  us.tab <- rbind(us.tab,df.tab)
}


us.tab$Port <- factor(us.tab$Port, levels = c("PEW","Slow","Inst"))

us.plot <- ggplot(data = us.tab, aes(x = log(risk), y = sharpe, group = Port, color = Port)) +
  geom_point(size = 3, shape = 19, alpha = 0.7) +  # larger points with some transparency
  geom_line(size = 1.05, linetype = "solid") +  # solid line
  scale_color_viridis(discrete = TRUE, name = "Portfolio") +  # set a legend title
  theme_minimal() +
  labs(
    x = "Log of Risk Aversion", 
    y = "Sharpe Ratio",
    title = "Impact of Risk Aversion on Sharpe Ratio",  # adding a title
    subtitle = "US Mixed Pension Porfolio"  # and a subtitle
  ) +
  theme(
    text = element_text(size = 12),  # adjust text size
    axis.text = element_text(face = "bold", size = 12),  # make axis labels bold and slightly larger
    panel.grid.minor = element_blank(),  # remove minor grid
    panel.grid.major = element_line(color = "grey90"),  # lighten the major grid lines
    legend.position = "bottom"  # move legend to the bottom
  )


uk.tab <- data.frame()  
for(j in risk.av){
  w.fund.uk <- c()
  mix.ret.uk <- c()
  for(i in 7:9){
    w.fund <- w.funding(gdp.port[,c(1,i)], gdp.port[,c(1,3)], j)
    mix.ret <- w.fund*gdp.port[,i] + (1-w.fund)*gdp.port[,3] 
    w.fund.uk <- c(w.fund.uk, w.fund)
    mix.ret.uk <- cbind(mix.ret.uk, mix.ret)
  }
  df.tab <- data.frame(w = round(w.fund.uk*100,2),
                       mu = round(apply(mix.ret.uk*100, 2, mean),2),
                       var = round(apply(mix.ret.uk*100, 2, var),2),
                       sharpe = round(apply(mix.ret.uk*100, 2, function(x) mean(x)/sd(x)),2),
                       Port = c("PEW","Slow","Inst"),
                       risk = rep(j,3)
  )  
  uk.tab <- rbind(uk.tab, df.tab)
}

uk.tab$Port <- factor(uk.tab$Port, levels = c("PEW","Slow","Inst"))

uk.plot <- ggplot(data = uk.tab, aes(x = log(risk), y = sharpe, group = Port, color = Port)) +
  geom_point(size = 3, shape = 19, alpha = 0.7) +  # larger points with some transparency
  geom_line(size = 1.05, linetype = "solid") +  # solid line
  scale_color_viridis(discrete = TRUE, name = "Portfolio") +  # set a legend title
  theme_minimal() +
  labs(
    x = "Log of Risk Aversion", 
    y = "Sharpe Ratio",
    title = "Impact of Risk Aversion on Sharpe Ratio",  # title remains the same
    subtitle = "UK Mixed Pension Portfolio"  # updated subtitle
  ) +
  theme(
    text = element_text(size = 12),  # adjust text size
    axis.text = element_text(face = "bold", size = 12),  # make axis labels bold and slightly larger
    panel.grid.minor = element_blank(),  # remove minor grid
    panel.grid.major = element_line(color = "grey90"),  # lighten the major grid lines
    legend.position = "bottom"  # move legend to the bottom
  )

fig.pen <- grid.arrange(us.plot, uk.plot, 
          ncol = 2, nrow = 1)

ggsave(filename = "riskvssh.eps",plot = fig.pen, device = "eps")
