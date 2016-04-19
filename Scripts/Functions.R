f.Recode3 <- function(var) {
  # construct recode string
  ## MAY EXPECT CLASS CHAR
  # vector of unique level names
  levs <- sort(unique(var))
  # build vector of matching recode values, excluding unsure responses (caught by else further downstream)
  vals <- integer()
  levs <- levs[!'']
  for(i in 1:length(levs)) {
    if(levs[i] %in% c('Got better', 'It got better'))               {vals[i] <-  1}
    if(levs[i] %in% c('Stayed the same', 'It stayed the same'))     {vals[i] <-  0}
    if(levs[i] %in% c('Got worse', 'It got worse'))                 {vals[i] <- -1}
    if(levs[i] %in% c("I don't know/Unsure")) {levs[i] <- NULL}
  }
  # format the string chunk with single quotes, equal signs, and semicolons
  chunk.i <- cbind('\'', levs, '\'', '=', vals, ';')
  # add else NA clause in place of idk/unsure
  chunk.o <- character()
  for(i in 1:nrow(chunk.i)) {
    chunk.o <- rbind(chunk.o, paste0(chunk.i[i,], collapse = ""))
  }
  chunk.o <- append(chunk.o, 'else = NA;')
  # pass string to car::recode
  library(car)
  return recode(var, paste(chunk.o, collapse= ""))
}