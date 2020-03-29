#my_functions.R


# LIBS --------------------------------------------------------------------

library(tidyverse)
library(rlang)
library(splitstackshape)
library(cowplot)
theme_set(theme_cowplot())


# FUNCTIONS ---------------------------------------------------------------


#function to plot pca density
plot_pc_density = function(pca_df, pc, group){
  #set up labels
  percentVar = attr(pca_df, "percentVar")[as.numeric(sub('PC', '', pc))]
  xlab = paste0(paste0(paste0(pc), ": "), 
                round(percentVar * 100), "%")
  pca_df %>% 
    ggplot(aes_string(x=pc, fill=group)) +
    geom_density(alpha = 0.75) +
    theme(legend.position = 'none',
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank()) +
    labs(subtitle=group,
         x=xlab)
}

#function to plot pca scatterplot
scatterplot_pca = function(df,
                           group_col = 'treatment',
                           pc1 = 1,
                           pc2 = 2,
                           subtitle = "",
                           size = 2,
                           legend_title=NULL,
                           x_invert=1,
                           legend_position = 'right',
                           fix_coords = FALSE,
                           alpha=0.1){
  #select PCs to plot and nvert X if needed
  plt_df = tibble(x = df[,paste('PC', pc1, sep = '')]*x_invert,
                  y = df[,paste('PC', pc2, sep = '')],
                  col = df[,group_col])
  #pull out the percent variances
  percentVar = attr(df, "percentVar")[c(pc1,pc2)]
  #build axis labs
  xlab = paste0(paste0(paste0("PC", pc1), ": "), 
                round(percentVar[pc1] * 100), "%")
  ylab = paste0(paste0(paste0("PC", pc2), ": "), round(percentVar[pc2] * 100), "%")
  g = plt_df %>% 
    ggplot(aes(x=x,
               y=y,
               color=col)) + 
    geom_point(size = size,
               alpha = alpha) +
    labs(x = xlab,
         y = ylab,
         subtitle=subtitle,
         color=legend_title) +
    theme(legend.position=legend_position,
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank())
  if (fix_coords){
    g = g + coord_fixed() 
  }
  return(g)
}


#function to get pearson correlation
get_pearson_cor = function(dat, xcol, ycol){
  dat=data.frame(dat)
  bad = c(NA, NaN, Inf, -Inf)
  x=dat[,xcol]
  y=dat[,ycol]
  rem = (x %in% bad) | (y %in% bad)
  totBad = sum(rem)
  if (totBad>0){
    print(paste('removing', totBad, 'rows with NA/NaN/Inf'))
  }
  dat=dat[!rem,]
  lm1=lm(dat[,ycol]~dat[,xcol])
  r2 = round(summary(lm1)$r.squared, digits=2)
  pearsonCor=cor(x=dat[,xcol],
                 y=dat[,ycol])
  r=round(pearsonCor, digits=2)
  return(r)
}


#same as r2 version but for pearson correlation 
plot_scatter_pearsonCor_annotated = function(dat, xcol, ycol, xlab, ylab, ALPHA=0.1, ylim=FALSE){
  dat=data.frame(dat)
  bad = c(NA, NaN, Inf, -Inf)
  x=dat[,xcol]
  y=dat[,ycol]
  rem = (x %in% bad) | (y %in% bad)
  totBad = sum(rem)
  if (totBad>0){
    print(paste('removing', totBad, 'rows with NA/NaN/Inf'))
  }
  dat=dat[!rem,]
  lm1=lm(dat[,ycol]~dat[,xcol])
  r2 = round(summary(lm1)$r.squared, digits=2)
  pearsonCor=cor(x=dat[,xcol],
                 y=dat[,ycol])
  r=round(pearsonCor, digits=2)
  print(summary(lm1))
  plt = dat %>% 
    ggplot(aes_string(x=xcol, y=ycol)) +
    geom_point(alpha = ALPHA) +
    labs(x=xlab,
         y=ylab)
  pbuild = ggplot_build(plt)
  yrange = pbuild$layout$panel_params[[1]]$y.range
  xrange = pbuild$layout$panel_params[[1]]$x.range
  if (length(ylim)>1){
    print('setting y limits')
    plt=plt + lims(y=ylim) +
      annotate("text", x = xrange[1], y = ylim[2],
               label = paste('italic(r) ==', r), parse=TRUE, color='black',
               hjust=0)
  } else {
    plt=plt +
      annotate("text", x = xrange[1], y = yrange[2],
               label = paste('italic(r) ==', r), parse=TRUE, color='black',
               hjust=0)
  }
  return(plt)
}




#function to get a plotting datafrom from an roc object build with pROC::roc
get_roc_plt_df = function(roc_object){
  roc_df = tibble(cutoff=roc_object$thresholds,
                      sensitivity = roc$sensitivities,
                      specificity = roc$specificities,
                      num = rep_len(1:100, length.out=length(roc_object$thresholds))) %>% 
    filter(num==1) %>% 
    select(-num) %>% 
    mutate(distance = abs(sensitivity-specificity))
}


#plot sensitivity and specificity against cutoff from an roc object build with pROC::roc
plot_sens_spec = function(roc_object){
  roc_df = get_roc_plt_df(roc_object)
  intersect = roc_df %>% 
    filter(distance==min(distance)) %>% 
    pull(cutoff)
  plt=roc_df %>% 
    select(-distance) %>% 
    pivot_longer(-cutoff,
                 names_to = 'metric',
                 values_to = 'value') %>% 
    ggplot(aes(x=cutoff,
               y=value,
               color=metric)) +
    geom_line() +
    geom_vline(xintercept = intersect, lty=2) +
    labs(title='sensitivity & specificity',
         subtitle = round(intersect, digits=3))
  return(list('plot' = plt,
              'int' = intersect))
}


#plot sensitivity and specificity against cutoff from an roc dataframe from get_roc_plt_df()
plot_roc_curve = function(roc_object){
  roc_df = get_roc_plt_df(roc_object)
  auc = round(roc_object$auc, digits=3)
  roc_df %>% 
    ggplot(aes(y=sensitivity,
               x=1-specificity)) +
    geom_line() +
    labs(title='ROC',
         subtitle=paste('AUC =', auc)) +
    geom_line(aes(x=sensitivity,
                  y=sensitivity),
              color='grey')
}


# #function to build a confusion matrix with a given cutoff
# make_confusion = function(probabilities, actual, cut){
#   call = if_else(probabilities >=cut,
#                  'yes',
#                  'no') %>% 
#     factor(levels=c('yes', 'no'))
#   ref = factor(actual,
#                levels=c('yes', 'no'))
#   
#   confusionMatrix(data=call,
#                   reference = ref,
#                   positive='yes')
# }



#plot density
plot_density = function(pwr){
  dplt = sat_df %>% 
    mutate(satisfaction = satis^pwr) %>% 
    ggplot(aes(x=satisfaction, fill=leaving)) +
    geom_density(alpha =0.6)
  
  int_x = ggplot_build(dplt)$data[[1]] %>% 
    data.frame() %>% 
    select(fill, x, y) %>% 
    pivot_wider(names_from = fill,
                values_from = y) %>% 
    data.frame() %>% 
    set_names(c('x', 'y1', 'y2')) %>% 
    mutate(dist = abs(y1-y2)) %>% 
    filter(dist == min(dist)) %>% 
    pull(x)
  
  wline = dplt +
    geom_vline(xintercept = int_x, lty=2)
  plot(wline)
  
  return(list('plt'=wline,
              'int'=int_x))
}
