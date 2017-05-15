# PDE-Project
PDE Project for MATH 452

This repo contains the code I used for this project. The purpose of the project is to investigate how a wood log dries.
The Model assumes no radial diffusion. Dirichlet boundary condition.

## What does each file do?
These are all the code files I have in the my working directory, and some of them are not used. The code is not well documented. I just needed to finish the project ASAP.

The main code files are:

### FiniteSumFull.R
Partial sum of analytical solution for full wood log. Numerical method with reacTran package.
### FullParallel.R
The parallel processing code for unsplit wood moisture diffusion. Numerical method with reacTran package.
### ParallelSplit.R
The parallel processing code for half-split wood moisture diffusion. Numerical method with reacTran package.
### ParallelQuarter.R
The parallel processing code for quarter-split wood moisture diffusion. Numerical method with reacTran package.
### Plotlywave.R
Using the data genreated by FullParalel.R, visualize the moisture change in a unsplit log. Ripple pattern. Did not actually use plot.ly in the end.
### comparisonplot.R
With all data generated, plot different splits.
### dimensionvariant.R
Vary the dimensions of the logs and generate data.
### sizecomparison.R
With data generated, plot different sizes.

## Notes on settings
I saved all the files on desktop "~/Desktop/PDE\ Project"
The 2d numerical methods generate huge files like 2GB each. Run with caution.
