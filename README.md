# MorphSim Shiny Tutorial

## Introduction

**MorphSim Shiny** provides an interactive and simplified interface for simulating morphological character evolution along phylogenetic trees based on the **MorphSim** package. Users can adjust parameters controlling tree building, fossil and extant sampling, clock rates, and substitution model settings. A visualization pane allows tracking of character state changes throughout evolutionary history.

This tutorial explains how to install the app and set parameters. No prior experience with phylogenetic simulation is required.

---

## I. Installing and Launching the App

To install the app, you will need the **devtools** package:

```r
install.packages("devtools")
devtools::install_github("fossilsim/MorphSimShiny")  # dependencies install automatically
MorphSimShiny::launchMorphSimShiny()
```

## II. Model Parameters
### i. Tree Model

The Tree and Sampling menus control how the phylogenetic tree is generated and how it is observed.
| Tree parameter              | Description                                                                                                                |
| --------------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| **Number of species (N)**   | The birth–death simulation runs until N simultaneously extant lineages are present.                                        |
| **Speciation rate (λ)**     | Rate at which lineages split. Higher values produce younger, more highly branched trees.                                   |
| **Extinction rate (μ)**     | Rate at which lineages go extinct. While μ > λ is mathematically possible, this option is disabled here due to known bugs. |


| Sampling parameter             | Description                                                                                                                                                |
| ------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Fossil sampling rate**       | A Poisson process describing fossil recovery. Higher values produce more sampled fossils. Enabling **Show Samples** displays fossil samples as diamonds on branches and extant samples are green circles on the tips of tree. |
| **Extant sampling proportion** | Proportion of present-day taxa that are observed.                                                                                                          |


### ii. Clock Model

The current app supports a strict molecular clock, meaning all branches evolve at the same rate. Higher values lead to more character state changes across the tree.
Although the underlying MorphSim package supports relaxed clocks, the Shiny app restricts users to a strict clock for simplicity.

### iii. Substitution Model

These settings determine how morphological characters evolve along the tree:

| Setting                            | Description                                                                                             |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------- |
| **Number of partitions**           | Number of independent subsets of characters.                                                            |
| **Number of traits per partition** | How many characters are simulated within each partition.                                                |
| **Number of states**               | Number of discrete states for traits in each partition e.g., 2 (binary), 3, 4....                       |
| **Morphological model**            | The substitution model used for character change. The standard **mk** model is extendable by simulating only varying traits (+V) and introducing site dependent rate heterogeneity (+G, fixed to 4 rates in the app). |

## III. Visualization and Output
### i. Plotting

The app visualizes morphological evolution directly on the tree:
- The starting state is shown at the root.
- Every character state change is annotated on the branch where it occurs.

Showing all traits at once would overcrowd the plot, so the Plotting menu allows selecting one character at a time.
In the character matrix beside the tree, the selected character is highlighted in black, while all others appear in gray.

Above the plot there are additional boxes which can be ticked:
|Box                         | Description                                                                                                                         |
|--------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
|**Fix tree**                | Keeps the current tree topology for the next simulation (in case you wish to just change **clock** or **substitution** parameters). |
|**Show fossils**            | Prints the sampled fossils as diamonds on the branches.                                                                             |
|**Show reconstructed tree** | Highlights the reconstructable topology given the incompleteness of sampling.                                                       |

### ii. Colorblindness Support

Character states are displayed using color.
If the default palette is not accessible, the Colorblindness menu offers alternatives designed for:
- Protanopia (Red-blind)
- Deuteranopia (Green-blind)
- Tritanopia (Blue-blind)

If another palette would be helpful —or if one of these is insufficient— please contact the development team.
