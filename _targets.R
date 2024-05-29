
library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    'accelerometry',
    'arrow',
    'cowplot',
    'dataRetrieval',
    'exactextractr',
    'GGally', # Needed for `ggcorr()`
    'httr',
    'magick', # Needed for 7_Disseminate and `cowplot::draw_image()`
    'MESS',
    'nhdplusTools', # Need to have a specific version. TODO, update once its on CRAN https://github.com/DOI-USGS/nhdplusTools/issues/365#ref-commit-6b18b97
    'paws.storage', # Needed to download NWM streamflow
    'pdp',
    'qs',
    'randomForest',
    'Rarr', # Needed to download NWM streamflow; devtools::install_github(repo = 'grimbough/Rarr')
    'raster',
    'sbtools',
    'scico',
    'sf',
    'tidytext',
    'tidyverse',
    'units',
    'usmap',
    'yaml',
    'zip'
  ), 
  format =  'qs',
  workspace_on_error = TRUE
)

source('1_Download.R')
source('2_Prepare.R')
source('3_Filter.R')
source('4_EpisodicSalinization.R')
source('5_DefineCharacteristics.R')
# source('6_PredictClass.R') # TODO: UNCOMMENT WHEN THIS PHASE IS ADDED!
source('7_Disseminate.R')

select <- dplyr::select # The raster pkg keeps overriding this one so make sure this is correct

c(p1_targets, p2_targets, p3_targets,
  p4_targets, p5_targets, #p6_targets, # TODO: UNCOMMENT WHEN THIS PHASE IS ADDED!
  p7_targets)
