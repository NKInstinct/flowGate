flowGate: Interactive Manual Gating
================
Andrew Wight & Harvey Cantor

Dana-Farber Research Institute, Boston, MA

This repository contains the flowGate software package for the R 
programming language. flowGate is meant to be used within the flow 
cytometry analysis framework provided by the Bioconductor project and 
based on the flowCore and (especially) flowWorkspace packages.

flowGate is a simple package that adds an interactive GUI to gating flow
cytometry data. This way, you can physically draw rectangle, span, 
quadrant, and polygon gates on your flow data, once it is read into a 
GatingSet object using the flowWorkspace package. Please see the vignette 
accompanying this software package for a step-by-step guide to using 
flowGate. This vignette was written assuming that the reader has some 
familiarity with the R programming language, but does not assume that the 
reader has ever worked with cytometry data in R before. Therefore, the 
vignette includes a step-by-step guide to performing the necessary data 
reading steps implemented in flowCore and flowWorkspace before gating, as 
well as an introduction to plotting flow cytometry data using ggcyto.

flowGate is an ongoing project, so any feedback and suggestions are 
appreciated. In particular, we hope that the accompanying vignette can 
act as a clear and concise guide to beginning with flow cytometry analysis 
in R in general, so suggestions on where to clarify or correct the
vignette are always welcome.
