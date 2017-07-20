calc_sm <- function(act, pred){ 
  sm <- 200 * abs(act - pred) / (abs(act) + abs(pred))# normal formula
  sm <- ifelse(is.na(act), NA,sm)                     # omit if act is NA
  sm <- ifelse(is.na(pred) & !is.na(act), 200,sm)     # max error if pred is NA and act is available
  sm <- ifelse(pred==0 & act==0, 0,sm)                # perfect (arbitrary 0/0=0)
  return (sm) 
}  