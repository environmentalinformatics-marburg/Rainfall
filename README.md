# Rainfall
Functions for training, prediction and validation of an MSG based optical rainfall retrieval

Main functions include
1. Handling of MSG images
  1. \code{link{getChannels}}
2. Calculation of predictor variables from MSG images
  1. Texture variables \code{link{textureVariables}} and \code{link{glcmPerPatch}}
  2. Shape of cloud variables \code{link{geometryVariables}}
3. Feature selection
4. Model training
5. Model prediction
6. Model validation
7. Visualisations



Install the package with 
install_github("environmentalinformatics-marburg/Rainfall")
