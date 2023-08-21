


# Loading the libraries ----
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(ggcorrplot)

data("mtcars")


dim(mtcars)
colnames(mtcars
         )
str(mtcars)

correlations <- cor(mtcars)
p.values.correlations <- cor_pmat(mtcars)


  ggcorrplot(correlations, 
             method='square', 
             # hc.order = TRUE, 
             outline.col = "white",
             type='upper',
             sig.level = 0.05,
             insig = 'blank',
             colors = c("dodgerblue3", "white", "darkorange"),
             p.mat = p.values.correlations) +
  #my_theme() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x=NULL, y=NULL) 

# Correlation plots ---- 


mtcars <- mtcars %>%  select_if(is.numeric) 

p.values.correlations <- as.data.frame(p.values.correlations)  

p.values.correlations.sel <-
  p.values.correlations %>% 
  mutate(another = rownames(.)) %>% 
  pivot_longer(cols = -another,
               values_to = 'pvalue',
               names_to = 'first') %>% 
  filter(pvalue !=0) %>% 
  filter(pvalue <=.05) %>% 
  mutate(combined = str_c(first, another, sep = '_')) %>% 
  mutate(sorted_words = sapply(strsplit(combined, "_"), 
                               function(x) paste(sort(x), collapse = "_"))) %>% 
  distinct(sorted_words, .keep_all = T) %>% 
  select(-combined, -sorted_words)

my_plot <- function(source, df) {
  x_var <- paste0(p.values.correlations.sel.item[1,2])
  y_var <- paste0(p.values.correlations.sel.item[1,1])
  ggplot(data = df, aes(x=.data[[x_var]], y=.data[[y_var]]))+
    geom_smooth(aes(x=.data[[x_var]], y=.data[[y_var]]), method = 'lm', se=FALSE, color='darkgray', formula = y ~ x)+
    geom_point(size=3, color='orange') +
    theme_light()+
    #my_theme()+
    theme(
          legend.position = 'right',
          legend.title = element_blank()) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    scale_x_continuous(expand = c(0.1, 0.1)) +
    labs(title = NULL,
         subtitle = y_var, y=NULL) +
    stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "*`,`~")),
             label.y= Inf, label.x = Inf, vjust = 1, hjust = 1.1, size = 2.5)
}

plots.corr <- list()
for (i in (1:nrow(p.values.correlations.sel))){
  p.values.correlations.sel.item <- p.values.correlations.sel[i,]
  
  plot_i <- 
  my_plot(source = p.values.correlations.sel.item, df = mtcars)
  
  plots.corr[[paste0(unique(p.values.correlations.sel.item$another), '_',
              unique(p.values.correlations.sel.item$first))]] <- plot_i
}


lapply(plots.corr, function(x) ggsave(plot = x, filename = paste0('./Plots/Correlations/',
                                                                  names(plots.corr),
                                                                  '.png')))


for (i in (1:nrow(p.mat.sel))){
  p.mat.sel5 <- p.mat.sel[i,]
  my_plot(source = p.mat.sel5, df = data)
  ggsave(plot = last_plot(),
         filename = paste0('./Plots/Correlations/',
                           unique(p.mat.sel5$another), '_',
                           unique(p.mat.sel5$first),
                           '.png'),
         width= 90,
         height = 60,
         unit='mm')
}


# corrplot <<- 
#   ggcorrplot(data3, 
#              method='square', 
#              # hc.order = TRUE, 
#              outline.col = "white",
#              type='upper',
#              sig.level = 0.05,
#              insig = 'blank',
#              colors = c("dodgerblue3", "white", "darkorange"),
#              p.mat = p.mat) +
#     #my_theme() +
#     theme(axis.text.x = element_text(angle=45, hjust=1)) +
#     labs(x=NULL, y=NULL) 
# 
# if (y == 'yes'){
# 
# ggsave(filename = paste0('./Plots/Correlations/', unique(data$Cells),'/',unique(data$Cells), '_correlogram.png'),
#        width = 160, height = 160, units = 'mm')
# }
# }

