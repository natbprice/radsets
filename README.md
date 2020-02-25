
<!-- README.md is generated from README.Rmd. Please edit that file -->

# radsets <img src="man/figures/logo.svg" align="right" alt="" width="120" />

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/natbprice/radsets.svg?branch=master)](https://travis-ci.org/natbprice/radsets)
[![Coverage
status](https://codecov.io/gh/natbprice/radsets/branch/master/graph/badge.svg)](https://codecov.io/github/natbprice/radsets?branch=master)
[![License: CC0-1.0](https://img.shields.io/badge/License-CC0%201.0-lightgrey.svg)](http://creativecommons.org/publicdomain/zero/1.0/)

**Package Website:** <https://natbprice.github.io/radsets/>

**Source Code:** <https://github.com/natbprice/radsets>

### Problem

The **radsets** package is a solution to the problem of how to visualize
large overlapping sets. In particular, when analyzing data with many
overlapping sets it is difficult to draw insight from a Venn diagram.
Moreover, it may be impossible to draw a Venn diagram that accurately
reflects the size of set intersections.

The solution implemented in the **radsets** package is a based on Radial
Sets diagrams (see references). Radial Sets diagrams are an interactive,
network-based visualization for the analysis of ovarlapping sets.

### MovieLens Example

The [MovieLens dataset](https://grouplens.org/datasets/movielens/20m/)
includes 19 genre attributes for 27,000 movies.

#### Radial Sets Diagram

A Radial Sets diagram created with the **radsets** package is shown
below.

The width of the links indicate the percent overlap (normalized by
union) between each movie genre. The link color indicates the relative
difference between the observed overlap and the expected overlap
assuming marginal independence. The link color palette is diverging blue
to red with red indicating overlaps that are larger than expected.

The size of the sections on the circumference are drawn to scale based
on the size of each set (i.e., the number of items in each set
regardless of overlap). However, since the sets overlap the section
sizes can not be interpreted as a donut chart or pie chart. The set
locations on the circumference have been optimized to place similar sets
closer together, thereby minimizing length of thickest links.

The bars in each sector indicate the number of items unique to each set,
shared with one other set, shared with two other sets, or shared with
three or more other sets.

<img src= "./man/figures/README-examplePlot-1.svg">

In the diagram below, we focus on the crime genre. The links are now
directional originating from the crime set and link thickness indicates
the percent of crime movies that belong to the other genres.

<img src= "./man/figures/README-examplePlot2-1.svg">

#### Venn Euler Diagram

Radial sets are useful for visualizing relationships among variables
which have many features in common. When the number of variables is
small, simple venn diagrams can be useful. However, when the number of
pairwise interactions is high, identifying patterns may be too difficult
using simple visualizations. Moreover, it may not be possible to draw a
venn diagam that accurately represents the overlap between sets. Here we
use the [venneuler](https://cran.r-project.org/package=venneuler)
package to illustrate the problem with using Venn diagrams when we are
interested in many overlapping sets.

<img src= "./man/figures/README-simpleVennDiag-1.svg">

<img src= "./man/figures/README-complexVennDiag-1.svg">

## Acknowledgements

Radial Sets diagrams were originally proposed by Alsallakh, Aigner,
Miksch, and Hauser. The **radsets** package is unaffiliated with these
researchers and the visualizations produced differ slightly from the
original design. This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall pthe fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

## References

Alsallakh, B., Aigner, W., Miksch, S., & Hauser, H. (2013). Radial sets:
Interactive visual analysis of large overlapping sets. IEEE Transactions
on Visualization and Computer Graphics, 19(12), 2496–2505. Retrieved
from <http://ieeexplore.ieee.org/abstract/document/6634104/>
