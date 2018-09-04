#' Converts raw scores to project benefit scores
#'
#' \code{authors_clean} This function takes the output from `references_read()` and cleans the author information.
#'
#' @param data a dataframe with 29 columns. The first is the project name. The next 28 are the metrics. With a seperate column for metric 6a and 6b. Each proposals is a row. 
#'


convert_to_benefit_score <- function(data){
  load("gomamnportfolio/data/scores_to_utility.Rdata")
  s2u <- scores_to_utility
  
  utility <- data.frame(project = data$project,
                        metric1=NA,  metric2=NA,
                        metric3=NA,  metric4=NA,
                        metric5=NA,  metric6a=NA,
                        metric6b=NA, metric7=NA,
                        metric8=NA,  metric9=NA,
                        metric10=NA, metric11=NA,
                        metric12=NA, metric13=NA,
                        metric14=NA, metric15=NA,
                        metric16=NA, metric17=NA,
                        metric18=NA, metric19=NA,
                        metric20=NA, metric21=NA,
                        metric22=NA, metric23=NA,
                        metric24=NA, metric25=NA,
                        metric26=NA, metric27=NA)
  
  for(metric in 1:28){
    for(i in 1:nrow(utility)){
      utility[i,(metric+1)] <-  s2u[s2u$"utility"==data[i,(metric+1)],
                                    (metric+1)]
    }}
  
  utility$metric6a <- utility$metric6a + utility$metric6b
  
  drops <- c("metric6b")
  
  utility <- utility[ , !(names(utility) %in% drops)]
  
  colnames(utility)[7] <- "metric6"
  
  load("gomamnportfolio/data/weights.Rdata")
  
  weightedscores <- data.frame(matrix(nrow=nrow(utility), ncol=ncol(utility)))  
  
  colnames(weightedscores) <- colnames(utility)
  
  weightedscores$project <- utility$project
  
  for(i in 1:27){
    weightedscores[,(1+i)] <- utility[,(i+1)]*weights[1,i]
  }
  
  benefitscores <- data.frame(project=utility$project,
                              benefit_score = rowSums(weightedscores[,2:28]))
  
  outputs <- list()
  
  outputs[["scores"]] <- data
  outputs[["utilities"]] <- utility
  outputs[["weighted_scores"]] <- weightedscores
  outputs[["benefit_scores"]] <- benefitscores
  
  return(outputs)
  
} 