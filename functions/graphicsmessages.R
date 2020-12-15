getMsg <- function(m, lang) {
  data = read.csv(file = "languages/graphicsmessages.csv", sep=";", header = T, fileEncoding = "UTF-8")

  x <- subset(data, data["msg"] == m)

  if(lang == "pt-br") {
    resul = x$pt 
  }else {
    resul = x$en 
  }
  
  return(resul)
}