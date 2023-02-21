library(magick)
library(ggplot2)

save_ggplots <- function(ggplot_list, directory, prefix = "plot") {
  # create the directory if it doesn't exist
  if (!dir.exists(directory)) dir.create(directory)
  
  # loop over the ggplot list and save each plot with a unique filename
  for (i in seq_along(ggplot_list)) {
    if (is.ggplot(ggplot_list[[i]])) {
      # if the element is a ggplot object, save it with a unique filename
      plot <- ggplot_list[[i]]
      plot <- plot +
        ggplot2::theme(title = element_text()) +
        ggplot2::ggtitle(paste0("Wave:", i))+
      filename <- paste0(prefix, i, ".png")
      filepath <- file.path(directory, filename)
      ggsave(filepath, plot = plot, device = "png")
    } else if (is.list(ggplot_list[[i]]) && all(sapply(ggplot_list[[i]], is.ggplot))) {
      # if the element is a list of ggplot objects, save each plot with a unique filename
      for (j in seq_along(ggplot_list[[i]])) {
        plot <- ggplot_list[[i]][[j]]
        plot <- plot +
          ggplot2::theme(title = element_text()) +
          ggplot2::ggtitle(paste0("Wave:", i)) 
        filename <- paste0(prefix, i, "-", j, ".png")
        filepath <- file.path(directory, filename)
        ggsave(filepath, plot = plot, device = "png")
      }
    } else {
      # if the element is neither a ggplot object nor a list of ggplot objects, raise an error
      stop(paste0("Element ", i, " of the input list is not a ggplot object or a list of ggplot objects."))
    }
  }
}

create_gif <- function(plotlist){
  dir_out <- file.path(outputs, "Plots/tmp")
  save_ggplots(plotlist, dir_out)
  imgs <- list.files(dir_out, full.names = T)
  img_list <- lapply(imgs, magick::image_read)
  img_joined <- magick::image_join(img_list)
  img_animated <- magick::image_animate(img_joined, fps = 0.5)
  do.call(file.remove, list(imgs))
  return(img_animated)
}

f <- create_gif(y)
f


