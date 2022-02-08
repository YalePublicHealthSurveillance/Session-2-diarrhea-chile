FarringtonApp <-function(ds=ds1, datevar='date', casevar='cases'){
  surv.ds1 <- surv.ds.convert(ds, datevar=datevar, casevar=casevar)
  
shinyApp(
  ui=fluidPage(
    
    
    sliderInput("week.test", "Current Week:",
                min=273, max=nrow(ds), value=273, step=1),
    selectInput("set.alpha", "Alpha",
                choices=list(0.01,0.025, 0.05, 0.1), selected=0.05),
    selectInput("set.b", "Number of historical years (default 5)",
                choices=list(2,3,4,5,6), selected=c(5)),
    selectInput("set.w", "Number of surrounding weeks on either side to use from historical period (default 3)",
                choices=list(1,2,3,4), selected=c(3)),
    checkboxInput("reweight", "Downweight past epidemics?:",
                  value=TRUE),
    plotOutput("periodPlot")
  ),
  server=function(input, output){
    output$periodPlot = renderPlot({
      
      freq=52
      b=as.numeric(input$set.b)
      w=as.numeric(input$set.w)
      
      prev.yr.wk <- (1:b)*53
      lag.units<- array(NA, dim=c(2,nrow=length(prev.yr.wk), ncol= w ))
      
      for(i in 1:w){
        lag.units[1,,i] <- prev.yr.wk + i
        lag.units[2,,i] <- prev.yr.wk - i
        
      }
      lag.units <- c(prev.yr.wk,as.vector(lag.units))
      
      lag.units<-  sort(lag.units)
      
      train.points <- input$week.test - lag.units
      
      mod1<-algo.farrington(surv.ds1, #dataset to use
                            control=list(range=c(input$week.test),
                                         b=b, #How many years of historical data to use
                                         w=w, #Number of weeks before and after current week to include in                                 model fitting
                                         reweight=input$reweight, #Do you want to downweight past epidemics?
                                         plot=FALSE,
                                         alpha=as.numeric(input$set.alpha)
                            ))
      
      col.vec<-rep(1, times=length(ds[,casevar]))
      col.vec[c(input$week.test) ]<-2 + mod1$alarm*2
      col.vec[train.points]<-3
      col.select<-c('white', 'blue', 'black','red')
      
      cols<-c('gray', 'black', 'red')
      plot.obs <- ds[,casevar]
      plot.obs[(input$week.test+1) : length(plot.obs)] <- NA
      plot.upperci <- c(rep(NA, times=(input$week.test-1)) , mod1$upperbound)
      
      plot(ds[,datevar],plot.obs , pch=16 , bty='l', ylab='Cases', xlab='Week',  col=col.select[col.vec], xlim=c(min(ds[,datevar]),max(ds[,datevar]) ))
      points(ds[1:length(plot.upperci),datevar],plot.upperci, type='p', col='purple',cex=3, pch="-")
      points(ds[,datevar],plot.obs ,type='l', col='gray')
      
      title('Farrington. Cases vs threshold: alarms are RED')
      
    },width = "auto", height = "auto")
  }
)
}