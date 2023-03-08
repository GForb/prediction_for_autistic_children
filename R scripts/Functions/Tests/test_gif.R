source(here::here("R scripts/Functions/gif.R"))
#making a gif

load(file.path(derived_data, "lsac.Rdata"))


gif_plots <- make_raincloudplot_wave(lsac_data, 7, "sdq_tot_p", "Total SDQ", "springgreen")
gif <- create_gif(gif_plots)
image_write(image = gif, path = file.path(outputs, "lsac_gif.gif"))



gif_plots2 <- make_raincloudplot_wave(lsac_data, 7, "sdq_tot_p", "Total SDQ", "springgreen", var_metadata = var_metadata)
gif2 <- create_gif(gif_plots2)
image_write(image = gif2, path = file.path(outputs, "lsac_gif2.gif"))