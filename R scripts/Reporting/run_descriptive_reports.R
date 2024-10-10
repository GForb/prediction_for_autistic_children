
# Vineland

descriptive_template <- here::here("Rmarkdown/descriptive_report_template.rmd")
## Pooled.

create_doc(
  dataset = "pooled_vabs", 
  template = descriptive_template,
  outcome = "vabs")

## EDX
create_doc(dataset = "edx_vabs", template = descriptive_template, outcome = "vabs")

## EpiTED
create_doc(
  dataset = "epited", 
  template = descriptive_template,
  outcome = "vabs")

## ELENA
create_doc(
  dataset = "elena_vabs", 
  template = descriptive_template,
  outcome = "vabs")


## Pathways
create_doc(
  dataset = "pathways_vabs", 
  template = descriptive_template,
  outcome = "vabs")



# CBCL


create_doc(
  dataset = "pooled_cbcl", 
  template = descriptive_template,
  outcome = "cbcl")


# SDQ

create_doc(
  dataset = "edx_cbcl", 
  template = descriptive_template,
  outcome = "cbcl")



# ELENA
create_doc(
  dataset = "elena_cbcl", 
  template = descriptive_template,
  outcome = "cbcl")

# ELENA
create_doc(
  dataset = "pathways_cbcl", 
  template = descriptive_template,
  outcome = "cbcl")


# ToGo
create_doc(
  dataset = "togo1", 
  template = descriptive_template,
  outcome = "cbcl")

create_doc(
  dataset = "togo2", 
  template = descriptive_template,
  outcome = "cbcl")

# low n!!!!!

# SDQ
create_doc(
  dataset = "k_families", 
  template = descriptive_template,
  outcome = "sdq")

create_doc(
  dataset = "quest", 
  template = descriptive_template,
  outcome = "sdq")

