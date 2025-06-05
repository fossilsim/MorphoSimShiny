## MorphoSimShiny
Shiny app using the morphosim package

To use the app open R (or Rstudio) (>= v.4.4.2) and run

### Quick Start
```
devtools::install_github("https://github.com/fossilsim/morphosim")
devtools::install_github("https://github.com/fossilsim/MorphoSimShiny")
MorphoSimShiny::launchMorphoSimShiny()
```
Make sure you have `devtools` and `Rtools` packages installed. 

### User's Guide

#### Step 1
Familiarize yourself with all the different control panels.
Would you like to (A) generate a new tree or (B) do you have a tree (a newick string with branch lengths in units of time) along which you'd like to simulate morphological traits?

#### Step 2
- (A) Select the number of extant taxa you want in you tree and choose the rates for speciation and extinction ("TREE MODEL")
- (B) Paste your newick string into "PROVIDE A TREE"

#### Step 3
Select the rate of morphological change ("CLOCK MODEL"), how many different possible states and traits should be simulated, and under which morphological model the data should be simualted. If no box under "MORPOLOGICAL MODEL" is ticked, the standard MK model will be used. If everything is set up, hit the "START SIMULATION" button.

#### Step 4
Use the "PLOTTING" panel to visualize different traits along the tree. You can use "fix tree" to keep the current tree topology and just re-simulate the character evolution. If a newick string is provided, this is not neccesary. The "Simulation details" print the newick string of the tree simulated last.

#### Step 5
Once you have simulated morphological data you can remove some of the information to better match what is commonly available from fossil data.
Data is removed from the matrix according to a given probability. 

