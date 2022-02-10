
app.hist.limit <-function(ds=ds1, datevar='date', casevar='cases'){
  surv.ds1 <- surv.ds.convert(ds, datevar=datevar, casevar=casevar)
  
  shinyApp(
  ui=fluidPage(
    
    
    sliderInput("week.test", "Current Week:",
                min=273, max=nrow(ds), value=273),
    selectInput("set.alpha", "Alpha",
                choices=list(0.01,0.025, 0.05, 0.1), selected=0.025),
    selectInput("set.b", "Number of historical years",
                choices=list(2,3,4,5,6), selected=c(5)),
    selectInput("set.m", "Number of surrounding 4-week months to use",
                choices=list(1,2), selected=c(1)),
    plotOutput("periodPlot")
  ),
  server=function(input, output){
    output$periodPlot = renderPlot({
      
      b=as.numeric(input$set.b)
      m=as.numeric(input$set.m)
      freq=52
      alpha=as.numeric(input$set.alpha)
      all.agg<-rollapply(surv.ds1$observed,4,mean, align='right', fill=NA)
      
      #########time point
      
      
      lag.units <- (1:b)*53
      if(m==1){
        all.prev <- c(lag.units, lag.units-4, lag.units+4) # the blocks include weeks, intervals end at 49, 53,57: 
      }else if(m==2){
        all.prev <- c(lag.units, lag.units-4,lag.units-8, lag.units+4, lag.units+8) # the blocks include weeks, intervals end at 49, 53,57: 
        
      }
      
      #(49,48,47,46)   # N-53  (53,52,51,50) # N-53+4 54,55,56,57
      
      prev.times <- sort(input$week.test - all.prev) #53 weeks ago is same period from prev year, then 1 on each side
      
      hist.mean <- mean(all.agg[prev.times])
      hist.sd <- sd(all.agg[prev.times])
      upCI <- hist.mean + qnorm(1 - alpha/2)*hist.sd*sqrt(1 + 1/length(prev.times))
      #Generate plot    
      
      alarm <-  all.agg[input$week.test] > upCI
      
      col.alarms<-c(rep(2, times=(input$week.test-1)) , (alarm+3), rep(1, times=length(surv.ds1$observed)-input$week.test ) )
      
      trans.white<-rgb(1,1,1,alpha=0.8)
      
      
      
      plot.obs <- surv.ds1$observed
      plot.obs[col.alarms==1] <- NA
      
      all.agg[col.alarms==1] <- NA
      observed=surv.ds1$observed
      
      par(mfrow=c(2,1))
      plot(ds[,datevar],plot.obs ,type='l',bty='l', ylab='Cases', xlab='Week', col='black', main='Weekly counts')
      
      #########time point
      
      #Generate plot    
      col.vec<-rep(1, times=length(observed))
      col.vec[c(input$week.test) ]<-2 + alarm*2
      col.vec[prev.times]<-3
      col.select<-c('white', 'blue', 'black','red')
      plot.upCI <- c(rep(NA, times=(input$week.test-1)) , upCI)
      plot(ds[,datevar],all.agg, type='p', pch=16, col=col.select[col.vec], bty='l', main='Black=comparison periods, purple=threshold')
      points(ds[(1:length(plot.upCI)),datevar],plot.upCI, type='p', cex=3,col='purple', pch="-")
      points(ds[,datevar],all.agg ,type='l', col='gray')
      
    },width = "auto", height = "auto")
  }
)
}
