source(here::here("R scripts/Functions/gif.R"))
#making a gif
gif_plots <- make_raincloudplot_wave(lsac_data, 7, "sdq_tot_p", "Total SDQ", "springgreen")
gif <- create_gif(gif_plots)
image_write(image = gif, path = file.path(outputs, "lsac_gif.gif"))