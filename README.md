<img src="www/images/TU_logo_large.png" alt="TU logo" width="200" align="right"/>

# Markov Model - Shiny Example
This repository contains the code for creating a shiny app for running simple
Markov Models.

## Table of Contents

1. [Overview](#overview)
2. [Using the Repository](#using)
3. [Repository Structure](#structure)
4. [Contributors](#contributors)

## Overview
This repository contains the code to create a **Shiny App** to demonstrate how
Markov Models are constructed and how to run simple models. Markov models are 
used widely in health economics and epidemiology to understand how patients can 
move between different health states. Comparing different models with different 
transition probabilities can enable comparisons of employing different healthcare
solutions.

The Markov Model App is available for demonstration [here](https://nhs-tu-andy-wilson.shinyapps.io/markov_example/).

## Using the Repository
To run the model locally the repository can be cloned:
```bash

git clone https://github.com/NHS-Transformation-Unit/markov_example.git
cd markov_example

```
Dependencies should be installed using `renv.lock` but please review the 
required packages in the `app.R` script.

The app can then be run using the following command in R:
```r
library(shiny)
runApp("app.R")
```

## Structure

The folder structure of the repository is shown below:

```plaintext

├───R
  └───markov_model.R
├───renv
  ├───.gitignore
  ├───activate.R
  └───settings.json
├───www
  ├───images
  └───themes
├───README.md
├───renv.lock
├───.gitignore
├───markov_example.Rproj
├───.Rprofile
├───app.R
├───LICENSE

```
### `R`
This folder contains the `markov_model.R` script. This has the necessary 
functions for running the markov model.

### `renv`
Contains necessary files for `renv` for reproducibility and collaboration.

### `www`
Contains the images and theme files used within the app. The `images` include 
diagrams of markov models and screenshots of the app. The `themes` include TU
`.css` themes and ggplot themes for the app.

### `app.R`
This is the shiny app that is deployed [here](https://nhs-tu-andy-wilson.shinyapps.io/markov_example/).

## Contributors
This repository has been created and developed by:
- [Andy Wilson](https://github.com/ASW-Analyst)
