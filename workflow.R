

library(lpSolveAPI)


### read in a csv file with 29 columns. the first is the project name, the rest are each performance metric. With seperate columns for metric 6a and metric 6b

scores <- read.csv("~/../Dropbox/GoMAMN/technical_report/SDM_tool_excel_version/example_scores.csv")

# load in the table with all the converstions from scores to utilities
load("gomamnportfolio/data/scores_to_utility.Rdata")

#rename the scores to utilities table
s2u <- scores_to_utility

#create an empty dataframe to store our utilities in
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
  
  # loop over each metric
  for(metric in 1:28){
  # loop over each project [row]
  for(i in 1:nrow(utility)){
  utility[i,(metric+1)] <-  s2u[s2u$"utility"==data[i,(metric+1)],
                                (metric+1)]
  }}
  
  # add together the scores from metric 6a and metric 6b
  utility$metric6a <- utility$metric6a + utility$metric6b
  
  # define the name of the column we want to drop
  drops <- c("metric6b")
  
  # drop column for metric 6b
  utility <- utility[ , !(names(utility) %in% drops)]
  
  # renname the column for metric6a to metric6
  colnames(utility)[7] <- "metric6"
    
  # load in the weights for each performance metric
  load("gomamnportfolio/data/weights.Rdata")
  
  # create an empty dataframe to score the weighted scores 
  weightedscores <- data.frame(matrix(nrow=nrow(utility), ncol=ncol(utility)))  
  
  # set the column names for the weighted scores object
  colnames(weightedscores) <- colnames(utility)
  
  # transfer the project names
  weightedscores$project <- utility$project

  # multiply the utilities by the weights
  for(i in 1:27){
    weightedscores[,(1+i)] <- utility[,(i+1)]*weights[1,i]
  }
  
  # create a new dataframe, with the names of the projects and the benefit scores
proj_benscore <- data.frame(project=utility$project,
                      benefit_score = rowSums(weightedscores[,2:28]))

# add a column with the project costs  
proj_benscore$cost <- c(10,100,230,400,550,670,780,13,75,134,154 ) * 10000
# add any columns of constraints you want to include
proj_benscore$texas <- c(0,1,1,1,0,0,0,1,1,0,1)

# set your max value [budget]
value <- 1000000

# determine how many projects there are 
nitems = nrow(proj_benscore)

# make a new linear program model object
lprec <- make.lp(0, ncol=nitems)
# set the type of optimization 
set.type(lprec, columns=seq.int(nitems), type="binary")
# add as many or few constraints as you want 
add.constraint(lprec, proj_benscore$cost, "<=", value) 
add.constraint(lprec, proj_benscore$texas, ">=", 2)
#set objective coefficients
set.objfn(lprec, rep(proj_benscore$benefit_score))
#set objective direction
lp.control(lprec,sense='max')
# SOLVE IT
solve(lprec)

# which projects are selected?
projbs[get.variables(lprec)==1,]


