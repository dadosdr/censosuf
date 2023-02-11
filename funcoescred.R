## ## ## ## ## ## ## ## ## 
# Função para mapa
## ## ## ## ## ## ## ## ## 
plotmapa = function(..., base, var, titulo, estados, cores, tit.size)  {
  {
    library(tmap)
    tm_shape(base) + tm_fill(var,
                                 # breaks = c(0,22642931 , 595701218, Inf),
                                 style = "kmeans", # kmeans, sd, fisher, jenks, hclust, bclust, quantile
                                 breaks = c(0,Inf),
                                 # breaks=c(0,quantile(paste(base,"$",var,"[",base,"$",var, ">0]"), na.rm=TRUE)), textNA = "Zero",
                                 colorNA = "gray",
                                 legend.hist = F,
                                 palette=cores)+
      tm_compass(position=c("left", "bottom"))+
      tm_layout(frame = FALSE, main.title = titulo, title.position = c('right', 'top'),
                main.title.size = tit.size)+
      tm_borders("black", lwd=.1, alpha = .1)+
      tm_scale_bar(width = 0.22, position=c("left", "bottom"),lwd = .6)+
      tm_legend(position=c("right", "bottom"),
                compass.type="arrow", 
                legend.outside = T,
                legend.format = list(text.separator= "a", 
                                     text.or.more="ou maior",
                                     text.less.than="menor que"
                                     # ,fun=function(x) formatC(x, 
                                                              # big.mark = ".", 
                                                              # decimal.mark = ",", 
                                                              # digits=2, 
                                                              # format="d"
                                                              # )
                ))+
      tm_shape(estados)+
      tm_borders("black", lwd=1)
    
  }}

plotmapab = function(..., base, var, titulo, estados, cores, Breaks)  {
  {
    
# tab = table(ifelse(base$var2<=1,"0 a 1", 
#                    ifelse(base$var2<=2,"1 a 2", "2 ou maior")))
    
    library(tmap)
    tm_shape(base) + tm_fill(var, 
                             # breaks = c(0,22642931 , 595701218, Inf),
                             #style = "kmeans", # kmeans, sd, fisher, jenks, hclust, bclust, quantile
                             breaks = Breaks,
                             # breaks=c(0,quantile(paste(base,"$",var,"[",base,"$",var, ">0]"), na.rm=TRUE)), textNA = "Zero",
                             colorNA = "gray",
                             legend.hist = F,
                             palette=cores)+
      tm_compass(position=c("right", "bottom"), size = 2)+
      tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_layout(frame = TRUE, main.title = titulo, 
                title.position = c('left', 'top'),
                main.title.size = .9, 
                outer.margins=c(.0,0,.0,0))+
      tm_borders("black", lwd=.1, alpha = .3)+
      # tm_scale_bar(width = 0.22, position=c("left", "bottom"),lwd = .6)+
      tm_legend(position=c("left", "bottom"),
                compass.type="8star",
                # legend.outside = F,
                legend.format = list(text.separator= "a",
                                     text.or.more="ou maior",
                                     text.less.than="menor que"
                                     # ,fun=function(x) formatC(x, 
                                     # big.mark = ".", 
                                     # decimal.mark = ",", 
                                     # digits=2, 
                                     # format="d"
                                     # )
                ))+

      tm_shape(estados)+
      tm_borders("black", lwd=1)
    
  }}

plotmapak = function(..., base, var, titulo, estados, cores, Style = c("kmeans","sd","jenks","hclust","bclust"))  {
  {
    
    # tab = table(ifelse(base$var2<=1,"0 a 1", 
    #                    ifelse(base$var2<=2,"1 a 2", "2 ou maior")))
    
    library(tmap)
    tm_shape(base) + tm_fill(var, 
                             # breaks = c(0,22642931 , 595701218, Inf),
                             style = Style, #"kmeans", # kmeans, sd, fisher, jenks, hclust, bclust, quantile
                             #breaks = Breaks,
                             # breaks=c(0,quantile(paste(base,"$",var,"[",base,"$",var, ">0]"), na.rm=TRUE)), textNA = "Zero",
                             colorNA = "gray",
                             legend.hist = F,
                             palette=cores)+
      tm_compass(position=c("right", "bottom"))+
      tm_layout(frame = FALSE, main.title = titulo, asp=0, title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .3)+
      tm_scale_bar(width = 0.22, position=c("right", "bottom"),lwd = .2)+
      tm_legend(position=c("left", "bottom"),
                # compass.type="arrow",
                compass.type="8star",
                # legend.outside = F,
                legend.format = list(text.separator= "a",
                                     text.or.more="ou maior",
                                     text.less.than="menor que"
                                     ,fun=function(x) formatC(x,
                                     big.mark = ".",
                                     decimal.mark = ",",
                                      digits=2,
                                     format="fg"
                                     )
                ))+
      
      tm_shape(estados)+
      tm_borders("black", lwd=1)
    
  }}

plotmapakn2 = function(..., base, 
                      var, 
                      pop,
                      Id,
                      titulo, 
                      estados, 
                      cores, Style = c("kmeans","sd","jenks","hclust","bclust"))  {
  {
    
    # tab = table(ifelse(base$var2<=1,"0 a 1", 
    #                    ifelse(base$var2<=2,"1 a 2", "2 ou maior")))
    
    library(tmap)
    
    tm_shape(base) + tm_fill("var", 
                             # breaks = c(0,22642931 , 595701218, Inf),
                             style = Style, #"kmeans", # kmeans, sd, fisher, jenks, hclust, bclust, quantile
                             #breaks = Breaks,
                             # breaks=c(0,quantile(paste(base,"$",var,"[",base,"$",var, ">0]"), na.rm=TRUE)), textNA = "Zero",
                             colorNA = "gray",
                             legend.hist = F,
                             popup.vars = pop,
                             id = Id,
                             palette=cores)+
      # tm_text(label) + 
      tm_compass(position=c("right", "bottom"))+
      tm_layout(frame = FALSE, main.title = titulo, asp=0, title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .3)+
      tm_scale_bar(width = 0.22, position=c("right", "bottom"),lwd = .2)+
      tm_legend(position=c("left", "bottom"),
                # compass.type="arrow",
                compass.type="8star",
                # legend.outside = F,
                legend.format = list(text.separator= "a",
                                     text.or.more="ou maior",
                                     text.less.than="menor que"
                                     ,fun=function(x) formatC(x,
                                                              big.mark = ".",
                                                              decimal.mark = ",",
                                                              digits=2,
                                                              format="fg"
                                     )
                ))+
      
      tm_shape(estados)+
      tm_borders("black", lwd=1)
    
  }}

plotmapakn = function(..., base, 
                      var, 
                      Id,
                      titulo, 
                      label,
                      estados, 
                      cores, Style = c("kmeans","sd","jenks","hclust","bclust"))  {
  {
    
    # tab = table(ifelse(base$var2<=1,"0 a 1", 
    #                    ifelse(base$var2<=2,"1 a 2", "2 ou maior")))
    
    library(tmap)
    
    tm_shape(base) + tm_fill(var, 
                             # breaks = c(0,22642931 , 595701218, Inf),
                             style = Style, #"kmeans", # kmeans, sd, fisher, jenks, hclust, bclust, quantile
                             #breaks = Breaks,
                             # breaks=c(0,quantile(paste(base,"$",var,"[",base,"$",var, ">0]"), na.rm=TRUE)), textNA = "Zero",
                             colorNA = "gray",
                             legend.hist = F,
                             id = Id,
                             palette=cores)+
      tm_text(label) + 
      tm_compass(position=c("right", "bottom"))+
      tm_layout(frame = FALSE, main.title = titulo, asp=0, title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .3)+
      tm_scale_bar(width = 0.22, position=c("right", "bottom"),lwd = .2)+
      tm_legend(position=c("left", "bottom"),
                # compass.type="arrow",
                compass.type="8star",
                # legend.outside = F,
                legend.format = list(text.separator= "a",
                                     text.or.more="ou maior",
                                     text.less.than="menor que"
                                     ,fun=function(x) formatC(x,
                                                              big.mark = ".",
                                                              decimal.mark = ",",
                                                              digits=2,
                                                              format="fg"
                                     )
                ))+
      
      tm_shape(estados)+
      tm_borders("black", lwd=1)
    
  }}

## ## ## ## ## ## ## ## ## ## 
# função para o I e Moran
## ## ## ## ## ## ## ## ## ## 
func = function(var, x)  {
  for(i in 1:x)  
  {
    model = moran.test(var, spdep::nb2listw(
      spdep::knn2nb(spdep::knearneigh(
        sp::SpatialPoints(coords), k=i)), style="W"))
    pval = model[[2]][[1]]
    est = model$statistic
    Imoran = model[[3]][[1]]
    varianc = model[[3]][[3]]
    df1 = data.frame(`Imoran-Knn` = format(Imoran, digits=3),
                     `p-value` = pval,
                     # pvalue = (format(round(pval,3),digits=4, scientific = F)), 
                     estSd = est,
                     variance = varianc)
    rownames(df1) <- i
    print(df1)
  } 
}

## ## ## ## ## ## ## ## ## ## ## ## 
## função para o bimoran
## ## ## ## ## ## ## ## ## ## ## ## 
bimoran = function(dep, indep, y, x)  {
  # for(i in 1:indep)
  { form = ((dep))~((indep))
  mylabel = bquote("Moran": .(format(coef(lm(form))[[2]], scientific=F, digits = 3)))
  mylabel2 = bquote("p": .(format(summary(lm(form))$coefficients[[8]], digits = 3)))
  mylabel3 = paste0(mylabel,"(",mylabel2,")")
  
  print(paste(
    plot(form,  main = mylabel, ylab = y, xlab=x
         # xlim = c(mean(dep)-sd(dep),sd(dep)+mean(dep)),
         # ylim = c(mean(indep)-sd(indep),sd(indep)+mean(indep))
    ),
    abline(coef(lm(form))[1], coef(lm(form))[2], col='black', lwd = 3),
    # text(min(indep)*1.1, max(dep)*1, labels = mylabel,cex=1.),
    # text(min(indep)*1.2, max(dep)*1, labels = mylabel2,cex=1.),
    abline(v = mean(indep), h = mean(dep), lwd = .5, lty = 4),
    axis(1, cex.axis=.3),
    axis(2, cex.axis=.3)
  ))}}



morantab = function(base, var)  {
  library(broom)
  library(spdep)
  library(spdep)
  library(spatialreg)
  library(rgdal)
  library(sphet)
  library(tseries)
  library(readxl)
  library(splm)
  library(rgeoda)
  library(geobr)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(tmap)
  {
  base.spdep = as(base, "Spatial") 
  set.seed(123)
  listav = spdep::poly2nb(base.spdep, queen=TRUE)
  W = spdep::nb2listw(listav, glist=NULL, style="W",  zero.policy = TRUE)
  
  #queen segunda ordem
  # W2 <- nblag(listav, maxlag = 2)
  W2cuml <- nblag_cumul(nblag(listav, maxlag = 2))
  W2 <- nb2listw(nblag_cumul(nblag(poly2nb(base.spdep, queen=FALSE),maxlag = 5)), style="W", glist=NULL,  zero.policy = TRUE)
  
  W3cuml <- nblag_cumul(nblag(listav, maxlag = 3))
  W3 <- nb2listw(nblag_cumul(nblag(poly2nb(base.spdep, queen=FALSE),maxlag = 3)), style="W", glist=NULL,  zero.policy = TRUE)
  # W2cumlnb <- nblag_cumul(W2)
  
  #Matriz de pesos tipo distância
  
  coords = sp::coordinates(base.spdep)
  Wdist = spdep::dnearneigh(coords,0,1,longlat=FALSE)
  
  #Matriz de K vizinhos mais próximos (knn)
  
  pontos_mun = sp::SpatialPoints(coords)
  knn = spdep::knearneigh(pontos_mun, k=1) #k=1, k=2, ...
  knn_nb = spdep::knn2nb(knn)
  Wknn = spdep::nb2listw(knn_nb, style="W")
  Wknn2 = spdep::nb2listw(
    spdep::knn2nb(spdep::knearneigh(
      sp::SpatialPoints(coords), k=2)), style="W")
  Wknn3 = spdep::nb2listw(
    spdep::knn2nb(spdep::knearneigh(
      sp::SpatialPoints(coords), k=3)), style="W")
  Wknn4 = spdep::nb2listw(
    spdep::knn2nb(spdep::knearneigh(
      sp::SpatialPoints(coords), k=4)), style="W")
  
  tabw = rbind(
    cbind(Var = "Variável", tipo=c("WQueen","WQueen2","Wknn","Wknn2","Wknn3","Wknn4"),
          rbind(
            broom::tidy(moran.test(var, W, zero.policy = TRUE, na.action = na.omit)),
            broom::tidy(moran.test(var, W2, zero.policy = TRUE, na.action = na.omit)),
            broom::tidy(moran.test(var, Wknn, zero.policy = TRUE, na.action = na.omit)),
            broom::tidy(moran.test(var, Wknn2, zero.policy = TRUE, na.action = na.omit)),
            broom::tidy(moran.test(var, Wknn3, zero.policy = TRUE, na.action = na.omit)),
            broom::tidy(moran.test(var, Wknn4, zero.policy = TRUE, na.action = na.omit))
          )))
  colnames(tabw) = c("Variável","Matriz","Moran I", "Exp.","Variancia","Stat.","p","","")
  print(tabw)
}}


## ## ## ## ## ## ## 
# MAPA LISA
## ## ## ## ## ## ## 

lisamapasknn1 = function(..., basegeoda, var, estados, titulo)  {
  {
    
    library(rgeoda)
    library(ggplot2)
    library(spdep)
    library(spatialreg)
    library(rgdal)
    library(sphet)
    library(tseries)
    library(readxl)
    library(splm)
    library(rgeoda)
    library(geobr)
    library(sf)
    library(dplyr)
    library(ggplot2)
    library(tmap)
    
    # Análise LISA (pacote rgeoda)
    # create weights object
    queen_w <- rgeoda::queen_weights(basegeoda
                                     # ,order = 1
                                     # ,include_lower_order = T
    )
    queen_w2 <- rgeoda::queen_weights(basegeoda
                                      ,order = 2
                                     ,include_lower_order = T
    )
    
    # Rook contiguity weights using the sf objec
    rook_w <- rook_weights(basegeoda)
    # summary(rook_w)
    
    # knn
    knn1_w <- knn_weights(basegeoda, 1)
    knn2_w <- knn_weights(basegeoda, 2)
    knn2_w <- knn_weights(basegeoda, 3)
 
    # calcular MORAN local pelo rgeoda
    lisa <- rgeoda::local_moran(knn1_w, basegeoda[var], permutations = 999, significance_cutoff = 0.05) 
    
    # processar resultados queen
    basegeoda$clusterlq <- as.factor(lisa$GetClusterIndicators())
    levels(basegeoda$clusterlq) <- lisa$GetLabels()
    cluster1 = basegeoda$clusterlq
    ## plotando LISA
    moran_lbls <- lisa_labels(lisa) # extraindo os dados
    moran_colors <- setNames(lisa_colors(lisa), moran_lbls)
    
    # pacote tmap
    library(tmap)
    
    
    
    mapalisa = tm_shape(basegeoda) + tm_fill("clusterlq", palette=moran_colors,legend.show = FALSE)+
      tm_layout(frame = FALSE, main.title = titulo, title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .3)+
      # tm_compass(position=c("right", "top"), size = .5)+
      # tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom")
        #         #compass.type="8star",
        #         legend.outside=F,
        #         legend.format = list(text.separator= "a", 
        #                              text.or.more="ou maior",
        #                              text.less.than="menor que")
        )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda$clusterlq))), " (", as.list(table(basegeoda$clusterlq)),")"),
                                   col=moran_colors,
                                   title = "Lisa cluster")+
      tm_shape(estados)+
      tm_borders("black", lwd=1)
    
    
    # LISA significância
    
    basegeoda$lisa_p = lisa$p_vals
    basegeoda$lisapcol = as.factor(sapply(basegeoda$lisa_p, function(x){
      if (x <= 0.001) return("p<=0,001")
      else if (x <= 0.01) return("p<=0,01")
      else if (x <= 0.05) return ("p<=0,05")
      else return("Nao sign.")
    }))
    lisap1 = basegeoda$lisa_p
    lisapc = basegeoda$lisapcol
    
    mapalisasig = tm_shape(basegeoda) + tm_fill("lisapcol", 
                                                legend.show = FALSE,
                                                title = "LISA Sig.",
                                                palette = c("#eeeeee","#348124", "#53c53c", "#84f576"))+
      tm_layout(frame = FALSE, main.title = " ", title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .3)+
      tm_compass(position=c("right", "top"), size = .99)+
      tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom"),
                compass.type="8star"
                # legend.outside=F,
                # legend.format = list(text.separator= "a", 
                #                      text.or.more="ou maior",
                #                      text.less.than="menor que")
        )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda$lisapcol))), " (", as.list(table(basegeoda$lisapcol)),")"),
                    col=c("#eeeeee","#348124", "#53c53c", "#84f576"),
                    title = "Lisa cluster")+
      tm_shape(estados)+
      tm_borders("black", lwd=1)
    
    # mylabel = bquote("Moran": .(format(coef(lm(form))[[2]], scientific=F, digits = 3)))
    # mylabel2 = bquote("p": .(format(summary(lm(form))$coefficients[[8]], digits = 3)))
    # mylabel3 = paste0(mylabel,"(",mylabel2,")")    
    # 
    
    
    tmap_arrange(mapalisa, mapalisasig, widths = c(.5, .5))    
    
    
  }}

lisamapasQueen = function(..., basegeoda, var, estados, titulo)  {
  {
    
    library(rgeoda)
    library(ggplot2)
    library(spdep)
    library(spatialreg)
    library(rgdal)
    library(sphet)
    library(tseries)
    library(readxl)
    library(splm)
    library(rgeoda)
    library(geobr)
    library(sf)
    library(dplyr)
    library(ggplot2)
    library(tmap)
    
    # Análise LISA (pacote rgeoda)
    # create weights object
    queen_w <- rgeoda::queen_weights(basegeoda
                                     # ,order = 1
                                     # ,include_lower_order = T
    )
    queen_w2 <- rgeoda::queen_weights(basegeoda
                                      ,order = 2
                                      ,include_lower_order = T
    )
    
    # Rook contiguity weights using the sf objec
    rook_w <- rook_weights(basegeoda)
    # summary(rook_w)
    
    # knn
    knn1_w <- knn_weights(basegeoda, 1)
    knn2_w <- knn_weights(basegeoda, 2)
    knn2_w <- knn_weights(basegeoda, 3)
    
    # calcular MORAN local pelo rgeoda
    lisa <- rgeoda::local_moran(queen_w, basegeoda[var], permutations = 999, significance_cutoff = 0.05) 
    
    # processar resultados queen
    basegeoda$clusterlq <- as.factor(lisa$GetClusterIndicators())
    levels(basegeoda$clusterlq) <- lisa$GetLabels()
    cluster1 = basegeoda$clusterlq
    ## plotando LISA
    moran_lbls <- lisa_labels(lisa) # extraindo os dados
    moran_colors <- setNames(lisa_colors(lisa), moran_lbls)
    
    # pacote tmap
    library(tmap)
    
    
    
    mapalisa = tm_shape(basegeoda) + tm_fill("clusterlq", palette=moran_colors,legend.show = FALSE)+
      tm_layout(frame = FALSE, main.title = titulo, outer.margins=c(.0,0,.0,0), title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .3)+
      # tm_compass(position=c("right", "top"), size = .5)+
      # tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom")
        #         #compass.type="8star",
        #         legend.outside=F,
        #         legend.format = list(text.separator= "a", 
        #                              text.or.more="ou maior",
        #                              text.less.than="menor que")
      )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda$clusterlq))), " (", as.list(table(basegeoda$clusterlq)),")"),
                    col=moran_colors,
                    title = "Lisa cluster")+
      tm_shape(estados)+
      tm_borders("black", lwd=1)
    
    
    # LISA significância
    
    basegeoda$lisa_p = lisa$p_vals
    basegeoda$lisapcol = as.factor(sapply(basegeoda$lisa_p, function(x){
      if (x <= 0.001) return("p<=0,001")
      else if (x <= 0.01) return("p<=0,01")
      else if (x <= 0.05) return ("p<=0,05")
      else return("Nao sign.")
    }))
    lisap1 = basegeoda$lisa_p
    lisapc = basegeoda$lisapcol
    
    mapalisasig = tm_shape(basegeoda) + tm_fill("lisapcol", 
                                                legend.show = FALSE,
                                                title = "LISA Sig.",
                                                palette = c("#eeeeee","#348124", "#53c53c", "#84f576"))+
      tm_layout(frame = FALSE, main.title = " ",outer.margins=c(.0,0,.0,0), title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .3)+
      tm_compass(position=c("right", "bottom"), size = 2)+
      tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom"),
        compass.type="8star"
        # legend.outside=F,
        # legend.format = list(text.separator= "a", 
        #                      text.or.more="ou maior",
        #                      text.less.than="menor que")
      )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda$lisapcol))), " (", as.list(table(basegeoda$lisapcol)),")"),
                    col=c("#eeeeee","#348124", "#53c53c", "#84f576"),
                    title = "Lisa cluster")+
      tm_shape(estados)+
      tm_borders("black", lwd=1)
    
    # mylabel = bquote("Moran": .(format(coef(lm(form))[[2]], scientific=F, digits = 3)))
    # mylabel2 = bquote("p": .(format(summary(lm(form))$coefficients[[8]], digits = 3)))
    # mylabel3 = paste0(mylabel,"(",mylabel2,")")    
    # 
    
    
    tmap_arrange(mapalisa, mapalisasig, widths = c(.5, .5))    
    
    
  }}

lisamapasknn1c = function(..., basegeoda1, basegeoda2, basegeoda3, basegeoda4, var, estados, titulo1, titulo2, titulo3, titulo4)  {
  {
    
    library(rgeoda)
    library(ggplot2)
    library(spdep)
    library(spatialreg)
    library(rgdal)
    library(sphet)
    library(tseries)
    library(readxl)
    library(splm)
    library(rgeoda)
    library(geobr)
    library(sf)
    library(dplyr)
    library(ggplot2)
    library(tmap)
    
    # Análise LISA (pacote rgeoda)
    # create weights object
    queen_w <- rgeoda::queen_weights(basegeoda2)
    queen_w2 <- rgeoda::queen_weights(basegeoda2,order = 2,include_lower_order = T)
    # Rook contiguity weights using the sf objec
    rook_w <- rook_weights(basegeoda2)
    # knn
    knn1_w <- knn_weights(basegeoda2, 1)
    knn2_w <- knn_weights(basegeoda2, 2)
    knn2_w <- knn_weights(basegeoda2, 3)
    # calcular MORAN local pelo rgeoda ######  1
    lisa1 <- rgeoda::local_moran(knn1_w, basegeoda1[var], permutations = 999, significance_cutoff = 0.05) 
    # processar resultados queen
    basegeoda1$clusterlq <- as.factor(lisa1$GetClusterIndicators())
    levels(basegeoda1$clusterlq) <- lisa1$GetLabels()
    cluster1 = basegeoda1$clusterlq
    ## plotando LISA
    moran_lbls1 <- lisa_labels(lisa1) # extraindo os dados
    moran_colors1 <- setNames(lisa_colors(lisa1), moran_lbls1)
    # calcular MORAN local pelo rgeoda ###### 2
    lisa2 <- rgeoda::local_moran(knn1_w, basegeoda2[var], permutations = 999, significance_cutoff = 0.05) 
    # processar resultados queen
    basegeoda2$clusterlq <- as.factor(lisa2$GetClusterIndicators())
    levels(basegeoda2$clusterlq) <- lisa2$GetLabels()
    cluster1 = basegeoda2$clusterlq
    ## plotando LISA
    moran_lbls2 <- lisa_labels(lisa2) # extraindo os dados
    moran_colors2 <- setNames(lisa_colors(lisa2), moran_lbls2)
    lisa3 <- rgeoda::local_moran(knn1_w, basegeoda3[var], permutations = 999, significance_cutoff = 0.05) 
    # processar resultados queen
    basegeoda3$clusterlq <- as.factor(lisa3$GetClusterIndicators())
    levels(basegeoda3$clusterlq) <- lisa3$GetLabels()
    cluster3 = basegeoda3$clusterlq
    ## plotando LISA
    moran_lbls3 <- lisa_labels(lisa3) # extraindo os dados
    moran_colors3 <- setNames(lisa_colors(lisa3), moran_lbls3)
    lisa4 <- rgeoda::local_moran(knn1_w, basegeoda4[var], permutations = 999, significance_cutoff = 0.05) 
    # processar resultados queen
    basegeoda4$clusterlq <- as.factor(lisa4$GetClusterIndicators())
    levels(basegeoda4$clusterlq) <- lisa3$GetLabels()
    cluster4 = basegeoda4$clusterlq
    ## plotando LISA
    moran_lbls4 <- lisa_labels(lisa4) # extraindo os dados
    moran_colors4 <- setNames(lisa_colors(lisa4), moran_lbls4)
    # pacote tmap
    library(tmap)
    
    
    
    mapalisa1 = tm_shape(basegeoda1) + tm_fill("clusterlq", palette=moran_colors1,legend.show = FALSE)+
      tm_layout(frame = TRUE, main.title = titulo1, title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .1)+
      # tm_compass(position=c("right", "top"), size = .5)+
      # tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom")
        #         #compass.type="8star",
        #         legend.outside=F,
        #         legend.format = list(text.separator= "a", 
        #                              text.or.more="ou maior",
        #                              text.less.than="menor que")
      )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda1$clusterlq))), " (", as.list(table(basegeoda1$clusterlq)),")"),
                    col=moran_colors1,
                    title = "Lisa cluster")+
      tm_shape(geom.st)+
      tm_borders("black", lwd=1)
    
    
    mapalisa2 = tm_shape(basegeoda2) + tm_fill("clusterlq", palette=moran_colors2,legend.show = FALSE)+
      tm_layout(frame = TRUE, main.title = titulo2, title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .1)+
      # tm_compass(position=c("right", "top"), size = .5)+
      # tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom")
        #         #compass.type="8star",
        #         legend.outside=F,
        #         legend.format = list(text.separator= "a", 
        #                              text.or.more="ou maior",
        #                              text.less.than="menor que")
      )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda2$clusterlq))), " (", as.list(table(basegeoda2$clusterlq)),")"),
                    col=moran_colors2,
                    title = "Lisa cluster")+
      tm_shape(geom.st)+
      tm_borders("black", lwd=1)
    
    mapalisa3 = tm_shape(basegeoda3) + tm_fill("clusterlq", palette=moran_colors3,legend.show = FALSE)+
      tm_layout(frame = TRUE, main.title = titulo3, title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .1)+
      # tm_compass(position=c("right", "top"), size = .5)+
      # tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom")
        #         #compass.type="8star",
        #         legend.outside=F,
        #         legend.format = list(text.separator= "a", 
        #                              text.or.more="ou maior",
        #                              text.less.than="menor que")
      )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda3$clusterlq))), " (", as.list(table(basegeoda3$clusterlq)),")"),
                    col=moran_colors3,
                    title = "Lisa cluster")+
      tm_shape(geom.st)+
      tm_borders("black", lwd=1)
    
    mapalisa4 = tm_shape(basegeoda4) + tm_fill("clusterlq", palette=moran_colors4,legend.show = FALSE)+
      tm_layout(frame = TRUE, main.title = titulo4, title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .1)+
      # tm_compass(position=c("right", "top"), size = .5)+
      # tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom")
        #         #compass.type="8star",
        #         legend.outside=F,
        #         legend.format = list(text.separator= "a", 
        #                              text.or.more="ou maior",
        #                              text.less.than="menor que")
      )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda4$clusterlq))), " (", as.list(table(basegeoda4$clusterlq)),")"),
                    col=moran_colors4,
                    title = "Lisa cluster")+
      tm_shape(geom.st)+
      tm_borders("black", lwd=1)
    
    tmap_arrange(mapalisa1, mapalisa2, mapalisa3, mapalisa4, widths = c(.5, .5), nrow = 2)    
    
    
  }}


lisamapasknn1c2 = function(..., basegeoda1, basegeoda2, var, estados, titulo1, titulo2)  {
  {
    
    library(rgeoda)
    library(ggplot2)
    library(spdep)
    library(spatialreg)
    library(rgdal)
    library(sphet)
    library(tseries)
    library(readxl)
    library(splm)
    library(rgeoda)
    library(geobr)
    library(sf)
    library(dplyr)
    library(ggplot2)
    library(tmap)
    
    # Análise LISA (pacote rgeoda)
    # create weights object
    queen_w <- rgeoda::queen_weights(basegeoda2)
    queen_w2 <- rgeoda::queen_weights(basegeoda2,order = 2,include_lower_order = T)
    # Rook contiguity weights using the sf objec
    rook_w <- rook_weights(basegeoda2)
    # knn
    knn1_w <- knn_weights(basegeoda2, 1)
    knn2_w <- knn_weights(basegeoda2, 2)
    knn2_w <- knn_weights(basegeoda2, 3)
    # calcular MORAN local pelo rgeoda ######  1
    lisa1 <- rgeoda::local_moran(knn1_w, basegeoda1[var], permutations = 999, significance_cutoff = 0.05) 
    # processar resultados queen
    basegeoda1$clusterlq <- as.factor(lisa1$GetClusterIndicators())
    levels(basegeoda1$clusterlq) <- lisa1$GetLabels()
    cluster1 = basegeoda1$clusterlq
    ## plotando LISA
    moran_lbls1 <- lisa_labels(lisa1) # extraindo os dados
    moran_colors1 <- setNames(lisa_colors(lisa1), moran_lbls1)
    # calcular MORAN local pelo rgeoda ###### 2
    lisa2 <- rgeoda::local_moran(knn1_w, basegeoda2[var], permutations = 999, significance_cutoff = 0.05) 
    # processar resultados queen
    basegeoda2$clusterlq <- as.factor(lisa2$GetClusterIndicators())
    levels(basegeoda2$clusterlq) <- lisa2$GetLabels()
    cluster1 = basegeoda2$clusterlq
    ## plotando LISA
    moran_lbls2 <- lisa_labels(lisa2) # extraindo os dados
    moran_colors2 <- setNames(lisa_colors(lisa2), moran_lbls2)

    # pacote tmap
    library(tmap)
    
    
    
    mapalisa1 = tm_shape(basegeoda1) + tm_fill("clusterlq", palette=moran_colors1,legend.show = FALSE)+
      tm_layout(frame = TRUE, title = titulo1, title.position = c('left', 'top'))+
      tm_borders("black", lwd=.1, alpha = .1)+
      # tm_compass(position=c("right", "top"), size = .5)+
      # tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom")
        #         #compass.type="8star",
        #         legend.outside=F,
        #         legend.format = list(text.separator= "a", 
        #                              text.or.more="ou maior",
        #                              text.less.than="menor que")
      )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda1$clusterlq))), " (", as.list(table(basegeoda1$clusterlq)),")"),
                    col=moran_colors1,
                    title = "Lisa cluster")+
      tm_shape(geom.st)+
      tm_borders("black", lwd=1)
    
    
    mapalisa2 = tm_shape(basegeoda2) + tm_fill("clusterlq", palette=moran_colors2,legend.show = FALSE)+
      tm_layout(frame = TRUE, title = titulo2, title.position = c('left', 'top'))+
      tm_borders("black", lwd=.1, alpha = .1)+
      # tm_compass(position=c("right", "top"), size = .5)+
      # tm_scale_bar(position=c("right", "bottom"), size = .5)+
      tm_legend(
        position=c("left", "bottom")
        #         #compass.type="8star",
        #         legend.outside=F,
        #         legend.format = list(text.separator= "a", 
        #                              text.or.more="ou maior",
        #                              text.less.than="menor que")
      )+
      tm_add_legend(type = "fill",
                    labels = paste0(as.list(names(table(basegeoda2$clusterlq))), " (", as.list(table(basegeoda2$clusterlq)),")"),
                    col=moran_colors2,
                    title = "Lisa cluster")+
      tm_shape(geom.st)+
      tm_borders("black", lwd=1)
    

    
    tmap_arrange(mapalisa1, mapalisa2,  widths = c(.5, .5),   ncol = 2, nrow = 1 )    
    
    
  }}


regressoesPOLS.EF.RE = function(..., baseplm, formula, efeito.fixo, tipo, titulo)  {
  {
library(plm)    
regpols=lm(
  formula,
  data=baseplm)
### POLS Robusto

# robsepols <- sqrt(diag(vcovHC(regpols, type = "HC2")))
### POLS Cluster
regpols1 = plm(
  # as.formula(step1),
  formula,
  # update.formula(as.formula(step1), ~. + ano),
  data=baseplm, model="pooling")
robsepols = lmtest::coeftest(regpols, vcov=vcovHC(regpols1,type="HC2"))
regpolsc = lmtest::coeftest(regpols1, vcov=vcovHC(regpols1, type="HC2", cluster="group"))

### FE
regfe = plm(
  formula, data=baseplm, 
  index = c("codmicro", "ano"), 
  model="within"
  , effect = c(efeito.fixo)
)

# robsefe <- sqrt(diag(vcovHC(regfe, type = "HC2")))
robsefe = lmtest::coeftest(regfe, vcov=vcovHC(regfe,type="HC2"))
### FE Cluster
regfec = lmtest::coeftest(regfe, vcov=vcovHC(regfe,type="HC2",cluster="group"))

### RE 
regre = plm(
  formula,
  # update.formula(as.formula(step1), ~. + ano),
  data=baseplm, model="random", effect = c(efeito.fixo))
### RE Robusto
# robsere <- sqrt(diag(vcovHC(regre, type = "HC2")))
robsere = lmtest::coeftest(regre, vcov=vcovHC(regfe,type="HC2"))
### RE Cluster
regrec = lmtest::coeftest(regre, vcov=vcovHC(regre,type="HC2",cluster="group"))

library(texreg) 

  
chow = format(pFtest(regfe, regpols1)$p.value,digits=5)
hausmann = format(phtest(regfe, regre)$p.value,digits=5)
lm = format(plmtest(regpols1, type = "bp")$p.value,digits=5)

tipo(caption = titulo, caption.above = TRUE,
     fontsize="normalsize", float.pos = "H",
     no.margin=TRUE, single.row = FALSE,  include.adjrs = FALSE,
     list(MQO = regpols,
          "MQO-ROB" = robsepols,
          # "POLS-CLUS" = regpolsc,
          EF = regfe,
          "EF-ROB" = robsefe,
          # "FE-CLUS" = regfec ,
          EA = regre,
          "EA-ROB"=robsere
          # ,"RE-CLUS" = regrec
), 
custom.gof.rows = list("P-value Chow POLSxFE" = c(chow, "","","","",""),
                       "P-value Hausmann FExRE" = c(hausmann, "","","","",""),
                       "P-value LM POLSxRE" = c(lm, "","","","","")))

# stargazer::stargazer(regpols, regpols, regpolsc,
#                      title = "Regressões POLS",
#                      dep.var.caption = "Variável dependente:",
#                      type = type,
#                      header=FALSE,
#                      model.names = FALSE,
#                      column.labels = c("POLS", "POLS-ROB", "POLS-VCE"),
#                      se = list(NULL, robsepols, NULL)),
# 
# 
# 
# stargazer::stargazer(regfe, regfe, regfec,
#                      se=list(NULL, robsefe, NULL), 
#                      type=type,
#                      title = "Regressões estimadas FE",
#                      dep.var.caption = "Variável dependente:",
#                      model.names = FALSE,
#                      header=FALSE,
#                      column.labels = c("FE", "FE-ROB", "FE-VCE"),
#                      add.lines = list(c("Efeito Fixo", efeito.fixo,efeito.fixo,efeito.fixo ))),
# 
# 
# 
# stargazer::stargazer(regre, regre, regrec,
#                      title = "Regressões estimadas RE",
#                      dep.var.caption = "Variável dependente:",
#                      type = type,
#                      model.names = FALSE,
#                      header=FALSE,
#                      column.labels = c("RE", "RE-ROB", "RE-ROB-VCE"),
#                      se = list(NULL, robsere, NULL))


# library(plm)


# print(
# ## chow (POLS x EF) H0: pols melhor
# pFtest(regfe, regpols1)
# )
# 
# print(
# ## Teste de Hausman (FE x RE) H0: RE melhor
# phtest(regfe, regre)
# )
# 
# print(
# ## Teste BP de componentes não observados H0: POLS melhor
# plmtest(regpols1, type = "bp")
# )

  }}



tabreg = function(..., reg){
  data.frame(Coef=names(reg$coefficients),
                  "reg" = paste(round(reg$coefficients,4),
                                ifelse(abs(reg$coefficients/sqrt(diag(vcov(reg))))>2.576,"***",
                                       ifelse(abs(reg$coefficients/sqrt(diag(vcov(reg))))>2.323,"**",
                                              ifelse(abs(reg$coefficients/sqrt(diag(vcov(reg))))>1.96,"*","")))))

}




