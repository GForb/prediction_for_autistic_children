# Creating global variables with the path to data and outputs folder.

For this project, data and outputs are saved in a shared folder named "Data and Outputs". For data governance reasons access to this folder is restricted. This follows the method outlined in the folder fun package documents, although we do not use the folder fun package https://cran.r-project.org/web/packages/folderfun/vignettes/intro.html.

Before running any code an environment variable named `pfac_path` must be set with the path to the data. To do this edit your .Renviron file to include the following:

`pfac_path = "*path to data*"`

To edit the .Renviron file, load the "usethis" package and use the  the commnand `usethis::edit_r_environ()`.

One the environment variable is set up then the R script set_paths can be run to create r objects with the needed paths.