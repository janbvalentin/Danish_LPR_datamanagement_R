
library(data.table)
library(lubridate)
library(dplyr)


risk_set_sampling <- function(data,casevar,id,start,end,ncontrols=4,replacement=TRUE,catvars=NULL,contvars=NULL,contvars_tol=NULL) {
  
  # TODO:
  # Check id is unique
  # check contvars and contvars_tol are same length
  
  matched <- data %>% filter({{casevar}})
  allcontrols <- data #%>% filter(!{{casevar}})
  ncases <- nrow(matched)
  allcontrols$newcase <- FALSE
  matched$newcase <- TRUE
  matched$mid <- 1:ncases
  matched$drop <- FALSE
  
  wab <- data.table(wa=contvars,wb=contvars_tol)
  
  print(ncases)
  
  for (j in 1:ncases) {
    
    evalstring <- "potcontrols <- allcontrols %>% filter("
    evalstring <- paste0(evalstring,"(matched$",end,"[j] - matched$",start,"[j]) < (",end," - ",start,")")
    for (v in catvars) {
      evalstring <- paste0(evalstring," & matched$",v,"[j] == ",v)
    }
    for (wi in 1:nrow(wab)) {
      w <- unlist(wab[wi,])
      evalstring <- paste0(evalstring," & matched$",w[1],"[j] > ",w[1],"-",w[2])
      evalstring <- paste0(evalstring," & matched$",w[1],"[j] < ",w[1],"+",w[2])
    }
    evalstring <- paste0(evalstring,")")
    eval(parse(text=evalstring))
    
    if (nrow(potcontrols)<ncontrols) {
      matched$drop[j] <- TRUE
      next
    }
    randompick <- sample(c(rep(FALSE,nrow(potcontrols)-ncontrols),rep(TRUE,ncontrols)))
    picked <- potcontrols[randompick,] %>% mutate(mid = matched$mid[j], drop = FALSE)
    matched <- rbind(matched,picked)
    
    if (!replacement) {
      rmv <- (picked %>% select({{id}}) %>% mutate(rmvid = {{id}}))$rmvid
      allcontrols <- allcontrols %>% filter(!{{id}} %in% rmv)
    }
    
    if (j %% 20 == 0) { cat(j) } else { cat(".") }
  }
  
  return(matched %>% filter(!drop))
}




matched_nrep <- risk_set_sampling(dt,
                                  casevar=case,
                                  id=Person_IDs,
                                  start="Start_Date",
                                  end="End_Date",
                                  replacement=FALSE,
                                  catvars=c("Sex"),
                                  contvars=c("Age","Start_Date"),
                                  contvars_tol=c(2,30))


