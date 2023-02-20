library(magick)

create_gif <- function(plotlist){
  dir_out <- file.path(outputs, "Plots/tmp")
  for(c in 1:length(plotlist)) {
    print(c)
    fp <- file.path(dir_out, paste0("rc", c))
    ggplot2::ggsave(plot = plotlist[[c]], filename = fp, device = "png")
  }
  # imgs <- list.files(dir_out, full.names = T)
  # return(imgs)
  # img_list <- lapply(imgs, magick::image_read)
  # img_joined <- magick::image_join(img_list)
  # img_animated <- magick::image_animate(img_joined, fps = 0.5)
  # do.call(file.remove, imgs)
  # return(img_animated)
}

f <- create_gif(y)
