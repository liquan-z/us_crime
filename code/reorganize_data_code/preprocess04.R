# change line 12
preprocess_before_2004 <- function(df){
  # delete all NA rows
  row.all.na <- apply(df, 1, function(x){all(is.na(x))})
  df <- df[!row.all.na,]
  # fill in 100,000 in per 100,000 inhabitants
  for (i in 1:nrow(df)) {
    if (df$Area[i]=='Rate per 100,000 inhabitants') df$Population[i]='100000'
  }
  # check number of NA in population row, if larger 52, then find the place 
  # where the next line of NA is equal to 1
  
  if (sum(is.na(df$Population))>52) {
    for (i in 1:(nrow(df)-1)){
      if (is.na(df$Population[i])==T & df$Area[i+1]=='Area actually reporting') {
        df$Population[i]='0'
      }
    }
  }
  # remove rows include None
  df <- subset(df,(is.na(Population)==TRUE) | (Population!='None'))
  return(df)
}


