# Loading the libraries ----
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)

# Load the data
data("mtcars")

# Select only numeric columns. In this example some of the columns were removed from the analysis
mtcars <- mtcars %>%  select_if(is.numeric) %>% 
  select(-c(cyl, vs, am, gear, carb))

# Create correlations matrix and the p.value matrix using the ggcorrplot package
correlations <- cor(mtcars)
p.values.correlations <- cor_pmat(mtcars)

# Create the correlogram
correlogram <- 
ggcorrplot(correlations, 
             method='square', 
             outline.col = "white",
             type='upper',
             sig.level = 0.05,
             insig = 'blank',
             colors = c("dodgerblue3", "white", "darkorange"),
             p.mat = p.values.correlations) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x=NULL, y=NULL) 

correlogram
# Correlation plots ---- 

# Change the p.value matrix into data frame
p.values.correlations <- as.data.frame(p.values.correlations)  

# Create a long dataframe with the p.values 
p.values.correlations.sel <-
  p.values.correlations %>% 
  mutate(another = rownames(.)) %>% 
  pivot_longer(cols = -another,
               values_to = 'pvalue',
               names_to = 'first') %>% 
  filter(pvalue !=0) %>% 
  filter(pvalue <=.05) %>% # Filter only p values <=0.05
  mutate(combined = str_c(first, another, sep = '_')) %>% 
  mutate(sorted_words = sapply(strsplit(combined, "_"), 
                               function(x) paste(sort(x), collapse = "_"))) %>% 
  distinct(sorted_words, .keep_all = T) %>% 
  select(-combined, -sorted_words)

# Function to create regression plots
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
             label.y= Inf, label.x = Inf, vjust = 1, hjust = 1.1, size = 3.5)
}

# Creating a list of plots
plots.corr <- list()
for (i in (1:nrow(p.values.correlations.sel))){
  p.values.correlations.sel.item <- p.values.correlations.sel[i,]
  
  plot_i <- 
  my_plot(source = p.values.correlations.sel.item, df = mtcars)
  
  plots.corr[[paste0(unique(p.values.correlations.sel.item$another), '_',
              unique(p.values.correlations.sel.item$first))]] <- plot_i
}

# Saving the plots to the disc.
plot.names <- names(plots.corr)
lapply(seq_along(plots.corr), function(x) {
  plot <- plots.corr[[x]]
  name <- plot.names[x]
  
  ggsave(plot = plot, 
         filename = paste0('./Plots/Correlations/',name, '.png'),
         width = 80,
         height = 80, units = 'mm')
})

# Arranging the plots
regressions <- 
ggarrange(plotlist = plots.corr)

regressions