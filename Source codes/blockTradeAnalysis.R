
#wsi <- read.csv("Desktop/data.csv")

#stockcode, start.date, end.date,

blockTradeAnalysis <- function(shares, VMA5, discount){
    trading.days <- ceiling(shares / VMA5);
    total.volume <- 0
    if(trading.days < 0 | trading.days > 5){
        print("number of days out of range");
    }
    deal.price <- wsi$close[1] * discount[trading.days]
    print(deal.price)
    
    for(i in 2: nrow(data)){
        if(wsi$close[i] > deal.price){
            total.volume <- total.volume + wsi$volume[i]
        }
    }
    
    if(total.volume > shares){
        return(0)
    }else{
        return(-1)
    }
    #interval <- (unclass(as.POSIXct(startData))[[1]]
}

#1 consecutive trading days
blockTradeAnalysisMulti <- function(wsi, wsd, deal.amt, trading.days = 1, impact.cost = 1, exp = 0){
    discount <- seq(0.9, 1, 0.005)
    total.days <- length(wsd$DATETIME)
    #trading.days <- ceiling(deal.amt/VMA5)
    #trading.days<-2
    win <- matrix(nrow = (total.days-trading.days+1), ncol = length(discount))
    colnames(win) <- discount
    rownames(win) <- wsd$DATETIME[1:(total.days-trading.days+1)]
    
    volume <- matrix(nrow = (total.days-trading.days+1), ncol = length(discount))
    colnames(volume) <- discount
    rownames(volume) <- wsd$DATETIME[1:(total.days-trading.days+1)]
    
    profit <- matrix(nrow = (total.days-trading.days+1), ncol = length(discount))
    colnames(profit) <- discount
    rownames(profit) <- wsd$DATETIME[1:(total.days-trading.days+1)]
    
    for(i in 1: (total.days-trading.days+1)){
        deal.price <- wsd$PRE_CLOSE[i] * discount * impact.cost ^ exp
        total.volume <- rep(0, length(deal.price))
        total.profit <- rep(0, length(deal.price))
        #could be revised, select each days data using which(substr(data2$time,1,7) == data2$date[i])
        #plan B for indexing: for(j in ((i-1)*242 + 1) :((i-1)*242 + 242)){
        #index <- c()
        #for(k in 1: trading.days){
        #    index <- c(index, which(substr(wsi$time,1,7) == wsd$DATETIME[i+k-1]))
            #}
        index <- seq(242*(i-1)+1,242*(i+trading.days-1))
        for(j in index){
            #Add impact cost here! re-calculate per day. add an day count
            sub.exp <- exp + floor((j-(242*(i-1)+1))/242)
            post.close <- wsi$close[j] * impact.cost ^ sub.exp
            
            temp <- post.close > deal.price
            total.volume[which(temp == TRUE)] <- total.volume[which(temp == TRUE)] + wsi$volume[j]
            total.profit[which(temp == TRUE)] <- total.profit[which(temp == TRUE)] + wsi$volume[j] * (post.close - deal.price[which(temp == TRUE)])
        }
        
        win[i,] <- total.volume > deal.amt
        volume[i, ] <- total.volume
        #profit[i, ] <- total.profit 
        profit[i, which(win[i,] == TRUE)] <- total.profit[which(win[i,] == TRUE)] / total.volume[which(win[i,] == TRUE)] * deal.amt
    }
    
    winning.percentage <- c()
    for(p in 1: ncol(win)){
        winning.percentage <- c(winning.percentage, length(which(win[,p] == TRUE))/nrow(win))
    }
    
    profit.mean <- apply(profit, 2, mean)
    
    result <- list(winning.percentage = winning.percentage, profit.mean = profit.mean, win.matrix = win, volume.matrix = volume, profit.matrix = profit)
    return(result)
}

#2. trading strategies with intervals, automatic allocation

#loopholes, trading days dividing
blockTradeAnalysisInterval <- function(code, start.date, end.date, deal.amt, trading.days = 1, interval.counts, interval.days, impact.cost = 1){
    w_wsi_data<-w.wsi(code,"close,volume,amt",paste(start.date, "09:00:00"),paste(end.date, "15:00:00"))
    w_wsd_data<-w.wsd(code,"pre_close",start.date,end.date,"Fill=Previous")
    wsi <- w_wsi_data$Data
    wsd <- w_wsd_data$Data
  
    discount <- seq(0.9, 1, 0.005)
    #trading.days <- ceiling(deal.amt/VMA5)
    total.days <- length(wsd$DATETIME)

    if(interval.counts >= trading.days | interval.counts < 1){
        print("The number of intervals invalid.")
        return
    }
    
    #history.span <- total.days - interval.counts * interval.days
    deal.span <- trading.days + interval.counts * interval.days
    trading.starts <- generate.start.dates(trading.days, interval.counts, interval.days)
    sub.trading.days<- generate.subtrading.days(trading.days, interval.counts)
    
    #initializing result set
    result <- list()
    result$win.matrix <- matrix(TRUE, nrow = total.days-deal.span+1, ncol = length(discount))
    result$volume.matrix <- matrix(0, nrow = total.days-deal.span+1, ncol = length(discount))
    result$winning.percentage <- c()
    result$profit.matrix <- matrix(0, nrow = total.days-deal.span+1, ncol = length(discount))
    
    for(i in 1:length(trading.starts)){
        start <- trading.starts[i]
        end <- total.days - deal.span + start + sub.trading.days[i] - 1
        temp1 <- wsi[((start - 1) * 242 + 1):(end * 242), ]
        temp2 <- wsd[start:end, ]
        
        exp <- 0
        if(i > 1){
            exp = sum(sub.trading.days[1:(i-1)])
        }
        
        sub.results <- blockTradeAnalysisMulti(temp1, temp2, deal.amt[i], sub.trading.days[i], impact.cost, exp)
        
        result$win.matrix <- result$win.matrix & sub.results$win.matrix
        result$volume.matrix <- result$volume.matrix + sub.results$volume.matrix
        result$profit.matrix <- result$profit.matrix + sub.results$profit.matrix
    }
    
    for(p in 1: ncol(result$win.matrix)){
        result$winning.percentage <- c(result$winning.percentage, length(which(result$win.matrix[,p] == TRUE))/nrow(result$win.matrix))
    }
    result$profit.mean <- apply(result$profit.matrix, 2, mean, na.rm = TRUE)

    return(result)
}

#trading with designated intervals
#trading.schedule: (T,T,X,T,T)
blockTradeAnalysisInterval2 <- function(code, start.date, end.date, deal.amt, trading.days = 1, trading.schedule, impact.cost = 1){
  w_wsi_data<-w.wsi(code,"close,volume,amt",paste(start.date, "09:00:00"),paste(end.date, "15:00:00"))
  w_wsd_data<-w.wsd(code,"pre_close",start.date,end.date,"Fill=Previous")
  wsi <- w_wsi_data$Data
  wsd <- w_wsd_data$Data
  
  discount <- seq(0.9, 1, 0.005)
  #trading.days <- ceiling(deal.amt/VMA5)
  total.days <- length(wsd$DATETIME)
  
  deal.span <- length(trading.schedule)
  schedule.details <- schedule.detail(trading.schedule)
  trading.starts <- schedule.details$trading.starts
  sub.trading.days<- schedule.details$sub.trading.days
  
  #initializing result set
  result <- list()
  result$win.matrix <- matrix(TRUE, nrow = total.days-deal.span+1, ncol = length(discount))
  result$volume.matrix <- matrix(0, nrow = total.days-deal.span+1, ncol = length(discount))
  result$winning.percentage <- c()
  result$profit.matrix <- matrix(0, nrow = total.days-deal.span+1, ncol = length(discount))
  
  for(i in 1:length(trading.starts)){
    start <- trading.starts[i]
    end <- total.days - deal.span + start + sub.trading.days[i] - 1
    temp1 <- wsi[((start - 1) * 242 + 1):(end * 242), ]
    temp2 <- wsd[start:end, ]
    
    exp <- 0
    if(i > 1){
      exp = sum(sub.trading.days[1:(i-1)])
    }
    
    sub.results <- blockTradeAnalysisMulti(temp1, temp2, deal.amt[i], sub.trading.days[i], impact.cost, exp)
    
    result$win.matrix <- result$win.matrix & sub.results$win.matrix
    result$volume.matrix <- result$volume.matrix + sub.results$volume.matrix
    result$profit.matrix <- result$profit.matrix + sub.results$profit.matrix
  }
  
  for(p in 1: ncol(result$win.matrix)){
    result$winning.percentage <- c(result$winning.percentage, length(which(result$win.matrix[,p] == TRUE))/nrow(result$win.matrix))
  }
  result$profit.mean <- apply(result$profit.matrix, 2, mean, na.rm = TRUE)
  
  return(result)
}


#Need to be revised generate.subtrading.days(4,2) :[1] 2 2 0

generate.start.dates <- function(trading.days, interval.counts, interval.days){
    single.trading.days <- ceiling(trading.days/(interval.counts+1))
    start.days <- seq(1, trading.days + interval.counts * interval.days, single.trading.days + interval.days)
    return(start.days)
}

generate.subtrading.days <- function(trading.days,interval.counts){
    result <- rep(ceiling(trading.days/(interval.counts+1)),interval.counts)
    result <- c(result, trading.days-ceiling(trading.days/(interval.counts+1))*interval.counts)
    return(result)
}

imapct.cost <- function(pre.amt, close.price){
    
}

schedule.detail <- function(trading.schedule){
  #if(length(which(trading.schedule == 'S')) != trading.days){
  #  print("unmatch trading days and trading schedule. Please check")
  #  errorCode = -1
  #}
  
  if(trading.schedule[1] != 'T' | trading.schedule[length(trading.schedule)] != 'T'){
    print("Trading schedule must start as well as end with selling.")
    return(errorCode = -2)
  }
  
  if(length(subset(trading.schedule, trading.schedule != 'T' & trading.schedule != 'X')) > 0){
    print("Trading schedule must contain 'T' or 'X' only.")
    return(errorCode = -3)
  }  
  
  sub <- 1
  trading.starts <- c(1)
  sub.trading.days <- c()
  
  for(i in 2:length(trading.schedule)){
    if(trading.schedule[i] == 'T'){
      sub <- sub + 1
    }
    if(trading.schedule[i] == 'X' & trading.schedule[i-1] == 'T'){
      sub.trading.days <- c(sub.trading.days, sub)
      sub <- 0
    }
    if(trading.schedule[i] == 'X' & trading.schedule[i+1] == 'T'){
      trading.starts <- c(trading.starts, i + 1)
    }
  }
  
  last.std <- length(trading.schedule) - trading.starts[length(trading.starts)] + 1
  sub.trading.days <- c(sub.trading.days, last.std)
  return(list(errorCode = 0, trading.starts = trading.starts, sub.trading.days = sub.trading.days))
}
