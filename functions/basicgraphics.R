#Create the pattern name to save the images
file.name.pattern <- function(name = "no_name", campus = "no_campus", period = "no_period", extension  =".pdf"){
  a <- "images/"
  b <- paste(period, campus, name, sep = "_")
  n <- paste(a, b, extension, sep = "")
  
  return(n)
}

#Get Basic configuration Theme
get.graph.theme.config <- function(){
  # Configuration of Theme for graphics
  mytheme <- theme_bw() +
    theme(
      axis.title = element_text(size = rel(1), lineheight=.9, face="bold.italic", colour="black"),
      panel.border = element_blank(),
      legend.position="top",
      text = element_text(size=12),
      axis.line = element_line(colour="black"),)
  
  return(mytheme)
}

#plot a graph with two axes of time series
plot.xy <- function(  dt,
                      x, 
                      y,
                      ymsg = "", 
                      xmsg = "", 
                      vline = F,
                      vline.intercept = '',
                      vlinecolor = "black",
                      vlinesize = 1.6, 
                      vlinetype = "dashed",
                      lcolors = c(),
                      ltype = c(),
                      show.points = F,
                      show.lines = T,
                      pshape = c(),
                      psize = 3,
                      pcolour = "black",
                      lsize = 1.4,
                      save = F,
                      save.name = "no_name", 
                      extension  = saved.name.pattern$extension,
                      ggwidth = graphics.size.pattern$width, 
                      ggheight = graphics.size.pattern$height, 
                      ggunits = graphics.size.pattern$units,
                      save.campus = saved.name.pattern$campus, 
                      save.period = saved.name.pattern$period,
                      legendlabels = c()){

  g <- ggplot(dt, aes(x = x, y = y))
  
  if(show.lines) {
    g <- g + geom_line(aes(color = "l", linetype = "l"), size = lsize)
    g <- g + scale_color_manual(values = lcolors, labels=legendlabels)
  }

  if(show.points) {
    g <- g + geom_point(aes(shape = "l"), size = psize)
    g <- g + scale_shape_manual(values = pshape, labels=legendlabels)
  }
  g <- g + scale_linetype_manual(values = ltype, labels=legendlabels)
  g <- g + get.graph.theme.config()
  g <- g + theme(legend.title=element_blank())
  g <- g + ylab(ymsg) 
  g <- g + xlab(xmsg)
  
  
  if(vline){
    g <- g + geom_vline(xintercept = vline.intercept, color = vlinecolor, size = vlinesize, linetype = vlinetype)  
  }
  
  if(save) {
    name <- file.name.pattern(save.name, save.campus, save.period)
    ggsave(name, width = ggwidth, height = ggheight, units = ggunits)
  }
  
  return (g)
}

plot.ts.forecast.XY <- function(mts, 
                                fcast,  
                                ymsg = "", 
                                xmsg = "",
                                save = F,
                                save.name = "no_name",
                                lsize = 1.4,
                                ltype = c("solid","twodash","dotted","dotted","dotted"),
                                lcolor = c("#1C1C1C", "#696969","#696969", "#696969","#696969"),
                                save.campus = saved.name.pattern$campus, 
                                save.period = saved.name.pattern$period,
                                extension  = saved.name.pattern$extension,
                                ggwidth = graphics.size.pattern$width, 
                                ggheight = graphics.size.pattern$height, 
                                ggunits = graphics.size.pattern$units,
                                nlegends = c('', '', '', '', ''))
{
  time.plot <- as.vector(time(mts))
  time.forecast.mean <- as.vector(time(fcast$mean))
  df1 = data.frame(time = time.plot, M = as.numeric(mts), isin = nlegends[1])
  df2 = data.frame(time = time.plot, M = as.numeric(fcast$fitted), isin = nlegends[2])
  df3 <- data.frame(time = time.forecast.mean, M = as.numeric(fcast$mean), isin = nlegends[3])
  df4 <- data.frame(time = time.forecast.mean, M = as.numeric(fcast$upper[,2]), isin = nlegends[4])
  df5 <- data.frame(time = time.forecast.mean, M = as.numeric(fcast$lower[,2]), isin = nlegends[5])
  df <- rbind(df1, df2, df3, df4, df5)
  
  gen <- ggplot(df, aes(x = time, y = M, group = isin)) + 
    geom_line(size = lsize,aes(color = isin, linetype = isin)) 
  gen <- gen + scale_colour_manual(values=lcolor) 
  gen <- gen + scale_linetype_manual(values = ltype)
  gen <- gen + get.graph.theme.config()
  gen <- gen + ylab(ymsg) 
  gen <- gen + xlab(xmsg)
  gen <- gen + theme(legend.title = element_blank())
  
  if(save) {
    name <- file.name.pattern(save.name, save.campus, save.period)
    ggsave(name, width = ggwidth, height = ggheight, units = ggunits)
  }
  
  return(gen)
}


#plot a graph with two axes of time series
plot.ts.xy <- function( dts = list(), 
                        ymsg = "", 
                        xmsg = "", 
                        vline = F,
                        vline.intercept = '',
                        vlinecolor = "black",
                        vlinesize = 1.6, 
                        vlinetype = "dashed",
                        hline = F,
                        hline.intercept = '',
                        hlinecolor = "black",
                        hlinesize = 1.6, 
                        hlinetype = "dashed",
                        lcolors = c(),
                        ltype = c(),
                        show.points = F,
                        pshape = c(),
                        psize = 3,
                        pcolour = "black",
                        lsize = 1.4,
                        save = F,
                        save.name = "no_name", 
                        extension  = saved.name.pattern$extension,
                        ggwidth = graphics.size.pattern$width, 
                        ggheight = graphics.size.pattern$height, 
                        ggunits = graphics.size.pattern$units,
                        save.campus = saved.name.pattern$campus, 
                        save.period = saved.name.pattern$period,
                        legendlabels = c()){
  df <- data.frame()
  number <- 0
  
  for (i in names(dts)) {
    number <- number + 1
    df.prov <- data.frame("x" = as.vector(time(dts[[i]])), 
                          "y" = as.numeric(dts[[i]]), 
                          "l" = rep(as.character(number), length(dts[[i]])))
    df <-rbind(df, df.prov)
  }
  
  g <- ggplot(df, aes(x = x, y = y))
  g <- g + geom_line(aes(color = l, linetype = l), size = lsize)
  g <- g + scale_color_manual(values = lcolors, labels=legendlabels)
  if(show.points) {
    g <- g + geom_point(aes(shape = l), size = psize)
    g <- g + scale_shape_manual(values = pshape, labels=legendlabels)
  }
  g <- g + scale_linetype_manual(values = ltype, labels=legendlabels)
  g <- g + get.graph.theme.config()
  g <- g + theme(legend.title=element_blank())
  g <- g + ylab(ymsg) 
  g <- g + xlab(xmsg)
  
  
  if(vline){
    g <- g + geom_vline(xintercept = vline.intercept, color = vlinecolor, size = vlinesize, linetype = vlinetype)  
  }
  
  if(hline){
    g <- g + geom_hline(yintercept = hline.intercept, color = hlinecolor, size = hlinesize, linetype = hlinetype)  
  }
  
  if(save) {
    name <- file.name.pattern(save.name, save.campus, save.period)
    ggsave(name, width = ggwidth, height = ggheight, units = ggunits)
  }
  
  return (g)
}

#plot a sazonality graph 
plot.ts.seasonality.XY <- function(main.dts, 
                                   others.dts = c(),
                                   ymsg = "", 
                                   xmsg = "", 
                                   show.points = F, 
                                   lcolors = c("black"),
                                   lsize = 1.4,
                                   p.size = 2.5,
                                   pshape = 21,
                                   pfill="white",
                                   pscale.start = 0.9,
                                   pscale.end = 0,
                                   gg.season.labels = "",
                                   save = F,
                                   save.name = "no_name", 
                                   save.campus = saved.name.pattern$campus, 
                                   save.period = saved.name.pattern$period,
                                   extension  = saved.name.pattern$extension,
                                   ggwidth = graphics.size.pattern$width, 
                                   ggheight = graphics.size.pattern$height, 
                                   ggunits = graphics.size.pattern$units){
  gen <- ggseasonplot(main.dts, main="",  season.labels = gg.season.labels) 
  gen <- gen + geom_line(size=lsize)
  gen <- gen + get.graph.theme.config()
  gen <- gen + geom_point(size=p.size, shape=pshape, fill=pfill) 
  gen <- gen + scale_color_grey(start=pscale.start, end=pscale.end, name="")
  
  gen <- gen + ylab(ymsg) 
  gen <- gen + xlab(xmsg)
  
  if(save) {
    name <- file.name.pattern(save.name, save.campus, save.period)
    ggsave(name, width = ggwidth, height = ggheight, units = ggunits)
  }
  
  return(gen)
}

#Plot boxplot
plot.boxplot.XY <- function(dts, 
                            ymsg = "", 
                            xmsg = "",
                            save.name = "no_name", 
                            save = F,
                            hasJitter = F,
                            jitter.shape = 16,
                            pos_jitter = 0.2,
                            out.color = "black",
                            out.shape = 8,
                            out.size = 3,
                            extension  = saved.name.pattern$extension,
                            ggwidth = graphics.size.pattern$width, 
                            ggheight = graphics.size.pattern$height, 
                            ggunits = graphics.size.pattern$units,
                            save.campus = saved.name.pattern$campus, 
                            save.period = saved.name.pattern$period){
  df <- data.frame()
  
  if(is.null(dim(dts))) {
      df <- data.frame("y" = as.numeric(dts), 
                       "x" = rep('a', length(dts)))
  } else {
    for (i in colnames(dts)) {
      df.prov <- data.frame("y" = as.numeric(dts[,i]), 
                            "x" = rep(i, length(dts[,i])))
      df <-rbind(df, df.prov)
    }
  }
  
  gen <- ggplot(df, aes(x, y) )  
  gen <- gen + geom_boxplot(outlier.color= out.color, outlier.size= out.size, outlier.shape = out.shape) 
  gen <- gen + get.graph.theme.config()
  gen <- gen + theme(legend.title=element_blank())
  gen <- gen + ylab(ymsg) 
  gen <- gen + xlab(xmsg)

  if(hasJitter) {
    gen <- geom_jitter(shape=jitter.shape, position=position_jitter(pos_jitter))
  }
  
  if(save) {
    name <- file.name.pattern(save.name, save.campus, save.period)
    ggsave(name, width = ggwidth, height = ggheight, units = ggunits)
  }
  
  return(gen)
} 

plot.macde.compare <- function(main.dts, 
                               fcasted,
                               ftest,
                               lsize = 1.4,
                               psize = 3,
                               xmsg = "",
                               ymsg = "",
                               slinetype = c("dotted", "solid"),
                               lynecolor = "#808080",
                               shapecolor = "black",
                               save = F,
                               save.name = "no_name", 
                               legendlabels = c(),
                               save.campus = saved.name.pattern$campus, 
                               save.period = saved.name.pattern$period,
                               extension  = saved.name.pattern$extension,
                               ggwidth = graphics.size.pattern$width, 
                               ggheight = graphics.size.pattern$height, 
                               ggunits = graphics.size.pattern$units){
  nrep <- length(main.dts) / frequency(main.dts)
  myd <-     data.frame(y = rep(as.numeric(fcasted), nrep), x = rep(1:12, nrep))
  myftest <- data.frame(y = rep(as.numeric(ftest),   nrep), x = rep(1:12, nrep))
  data <- data.frame(y = as.numeric(main.dts), 
                     year = trunc(round(time(main.dts),8)), 
                     cycle = as.numeric(cycle(main.dts)))
  gen <- ggplot(data, aes(x = factor(cycle)))
  gen <- gen + geom_point(aes(y = y, shape = factor(year)), colour = shapecolor, size= psize)
  gen <- gen + geom_line(aes(y = myd$y, x = myd$x, linetype = slinetype[1]), color= lynecolor, size = lsize)
  gen <- gen + geom_line(aes(y = myftest$y, x = myftest$x, linetype = slinetype[2]), color= lynecolor, size = lsize)
  gen <- gen + scale_linetype(labels = legendlabels)
  gen <- gen + get.graph.theme.config()
  gen <- gen + labs(x = xmsg, y =  ymsg, shape = NULL, linetype = NULL)
  
  if(save) {
    name <- file.name.pattern(save.name, save.campus, save.period)
    ggsave(name, width = ggwidth, height = ggheight, units = ggunits)
  }
  
  return(gen)
}

plot.compare.value <- function(dt,
                               fill.name = "Possui Geração",
                               fill.labels = c("Não", "Sim"),
                               fill.colors = c("#696969", "#1C1C1C"),
                               xmsg = "ano",
                               ymsg = "",
                               save = F,
                               save.name = "no_name",
                               save.campus = saved.name.pattern$campus, 
                               save.period = saved.name.pattern$period,
                               extension  = saved.name.pattern$extension,
                               ggwidth = graphics.size.pattern$width, 
                               ggheight = graphics.size.pattern$height, 
                               ggunits = graphics.size.pattern$units){
  
  gen <- ggplot(dt, aes(fill=generation, y=kw, x=year)) + 
    geom_bar(size = 1.5, position="dodge", stat="identity",width = .8) + 
    scale_fill_manual(name= fill.name, labels = fill.labels, values = fill.colors) + 
    facet_wrap(name~campus) + xlab(xmsg) + ylab(ymsg) + get.graph.theme.config()
  
  if(save) {
    name <- file.name.pattern(save.name, save.campus, save.period)
    ggsave(name, width = ggwidth, height = ggheight, units = ggunits)
  }
  
  
  return(gen)
}

