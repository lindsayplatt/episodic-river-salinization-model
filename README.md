# episodic-river-salinization-model
Modeling code in a targets pipeline for understanding characteristics of rivers that experience episodic salinization from winter road salting events.

This repository contains reproducible code for downloading, processing, and modeling data related to river salinization dynamics. Using [The Turing Ways's definitions](https://the-turing-way.netlify.app/reproducible-research/overview/overview-definitions), this code and analysis are intended to be *fully reproducible* and could be *somewhat replicable* with different states and/or dates. 

## Associated publications and resources

The code supports the analysis for Lindsay Platt's ([@lindsayplatt](https://github.com/lindsa%5D(https://github.com/lindsayplatt))) Master's Thesis, *Basin characteristics modulate signatures of river salinization*.

The code and data will be published in Zenodo.

TODO: MORE DETAILS TBD

## Running the code 

This repository is setup as an automated pipeline using the [`targets` R package](https://books.ropensci.org/targets/) in order to orchestrate a complex, modular workflow where dependency tracking determines which components need to be built. As written, this pipeline will need about 2.5 hours to build and will need to have an internet connection.

The pipeline is broken into 6 different phases:

* `1_Download` contains all the code that pulls from NWIS, ScienceBase, NHD+, etc. It will require an internet connection. It also sets the spatial and temporal limits for the analysis.
* `2_Prepare` this phase is doing all of the heavy lifting to process data into a useable state for both our time series data (prefixed with `p2_ts`) and our static attributes data (prefixed with `p2_attr`).
* `3_Filter` applies all of our criteria to remove sites that are not up to our minimum standards and/or are sites with unexpected features (tidal, in a really high agricultural area, don't have an associated NHD+ catchment area, etc).
* `4_EpisodicSalinization` applies an algorithm that has been used to identify storms by finding steep peaks in a hydrograph to the specific conductance time series in order to identify winter storms where road salts are washed into streams and cause sharp peaks (similar to storm hydrographs). In the end, this phase identifies sites with specific conductance data that exhibit this episodic behavior.
* `5_BaseflowSalinization` is doing two different things: applying a baseflow separation algorithm to extract only the days in the specific conductance time series that occurred on a baseflow day, and then using a seasonal Mann-Kendall to evaluate whether the baseflow specific conductance is experiencing any sort of trend. In the end, this returns a table with each site and what the salinization trend was for the baseflow days (either `positive`, `none`, or `negative`).
* `6_DefineCharacteristics` uses the information gathered in `4_EpisodicSalinization` and `5_BaseflowSalinization` to categorize the sites based on whether they exhibit episodic salinization and/or they have positive trends in baseflow specific conductance. Then, it applies random forest models to these categorizations with the collection of static attributes prepared and filtered in `2_Prepare` and `3_Filter` to define the attributes and values that are important for determining a site's category.

### Pipeline setup

Run the following command to make sure you have all the necessary packages before trying to build the pipeline.

# TODO: mac install issues.

``` r
install.packages(c(
    'targets', 
    'tarchetypes',
    'accelerometry',
    'arrow',
    'cowplot',
    'dataRetrieval',
    'EnvStats',
    'exactextractr',
    'FlowScreen',
    'GGally', 
    'httr',
    'MESS',
    'nhdplusTools',
    'pdp',
    'qs',
    'randomForest',
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
))
```

### Pipeline build

To build this pipeline (after running the setup section), you should 

1. Open the `run_pipeline.R` script.
1. Click on the `Background Jobs` tab in RStudio (next to `Console` and `Terminal`).
1. Choose `Start Background Job` and make sure the `run_pipeline.R` script is selected.
1. Accept the defaults and click `Start` to kick off the pipeline build.

This will build the pipeline in the background, so that your RStudio session can still be used as the job is running.

### Pipeline outputs

Many of the pipeline's artifacts are "object targets" but there are some files created. As of 2/29/2024, the best way to see how the pipeline and analysis ran is to open the files stored in  `6_DefineCharacteristics/log/`. This will only have built if all the other pipeline steps were successfully run.

TODO: Add more info about pipeline output.
