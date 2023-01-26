surv.ds.convert <- function(ds, datevar='date', casevar='cases'){
  datevec <- pull(ds, datevar)
  casevec <- pull(ds, casevar)
  year.start <- min(year(datevec))
  week.start <- min(week(datevec)[year(datevec)==year.start])
  
  SurvObj <- create.disProg(
    week = 1:nrow(ds), #Index of observations
    observed = casevec ,  #cases
    state=matrix(0, nrow=nrow(ds), ncol=1), #just 0s
    start = c(year.start, week.start)) #start date; 1st week of 1990
  return(SurvObj)
}