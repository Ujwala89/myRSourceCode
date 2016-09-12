hospitalNameFunction <- function(dat,num) {
  
  if (num == "worst") {
    n <- nrow(dat)
  } else {
    n <- num
  }
  
  dat[n,"Hospital"] 
}