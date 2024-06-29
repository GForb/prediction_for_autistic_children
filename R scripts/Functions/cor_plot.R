cor_plot <- function(cor_mat){
  ggcorrplot::ggcorrplot(cor_mat, type = "lower", outline.col = "white", lab = TRUE,  ggtheme = ggplot2::theme_gray,
                         colors = c("#6D9EC1", "white", "#E46726") )
}