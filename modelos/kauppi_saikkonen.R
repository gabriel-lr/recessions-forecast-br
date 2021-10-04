# Probit Coding

test.df <- df.train |> 
  select(dummy, swap)

sigmoid <- function(x){
  1/(1+exp(-x))
}

neg_log_likelihood <- function(par, data, y, include_alpha = T){
  x = data[,names(data) != y]
  y_data = data[,y]
  
  # 1. Calculate theta
  if(include_alpha){
    
    # Multiply each value by their parameter
    multiplied =  mapply("*",x,par[2:length(par)])
    
    # We sum for each observation and add alpha
    theta =  rowSums(multiplied) + par[1]
  }else{
    theta =  rowSums(mapply("*",x,par))
  }
  
  # 2. Calculate p
  p = sigmoid(theta)
  #p = exp(theta) / (1 + exp(theta))
  
  # 3. Calculate -log likelihood
  val = -sum(y_data * log(p) + (1-y_data)*log(1-p)) 
  
  return(val)
}

library(optimx)

opt = optimx(
  par = c(0,1),
  fn = neg_log_likelihood,
  data = test.df,
  y = "dummy",
  include_alpha = T,
  control = list(trace = 0, all.methods = TRUE)
)


##-- Tentativa ---##
# Estudar documentação gets//EWS