---
title: 'radsets: An R package for interactive, network-based visualizations of overlapping set'
tags:
  - R
  - radial sets diagrams
  - overlapping sets
  - visualization
  - data analytics
authors:
  - name: Nathaniel B. Price
    orcid: 0000-0002-6450-617X
    affiliation: "1, 2"
    - name: Christopher Chizinski
    orcid: 
    affiliation: "1"
  - name: Jessica L. Burnett
    orcid: 0000-0002-6450-617X
    affiliation: "1, 3"
affiliations:
 - name: School of Natural Resources, University of Nebraska
   index: 1
 - name: ICF
   index: 2
 - name: Science Analytics and Synthesis, Core Science Systems, U.S. Geological Survey
   index: 3
date: 24 November 2019
bibliography: paper.bib
---

# Summary
The `radsets` R package is a solution to the visualizing large, overlapping sets. For example, it is difficult to draw insgihts from Venn diagrams when when analyzing data with many overlapping sets. Moreover, it may be impossible to draw a Venn diagram which accurately reflects the _size_ of set intersections. Here, we implement a solution based based on Radial Sets Diagrams [@alsallakh2013radial].  


![Figure 1. Example of a Radial Sets Diagram using the MovieLens Data [@harper2016movielens].](fig1.svg)

Radial Sets Diagrams are an interactive, network-based visualization for the analysis of ovarlapping sets [@alsallakh2013radial]. In Radial Sets Diagrams, the width of the links among nodes (i.e. units of interest) indicate the percent overlap (normalized by union) between (Figure 1). In this visualization, the link color indicates the relative difference between the observed overlap and the expected overlap assuming marginal independence. The link color palette is diverging blue to red with red indicating overlaps that are larger than expected. The size of the sections on the circumference are drawn to scale based on the size of each set (i.e., the number of items in each set regardless of overlap). The set locations along the circumference have been optimized for visualization to place similar sets closer together, thereby minimizing length of the thickest links. The bars in each sector indicate the number of items unique to each set, shared with one other set, shared with two other sets, or shared with three or more other sets. 


# Acknowledgements
Radial Sets diagrams were originally proposed by Alsallakh, Aigner, Miksch, and Hauser. The `radsets` package is unaffiliated with these researchers and the visualizations produced differ slightly from the original design. This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. This draft manuscript is distributed solely for purposes of scientific peer review. Its content is deliberative and predecisional, so it must not be disclosed or released by reviewers. Because the manuscript has not yet been approved for publication by the U.S. Geological Survey (USGS), it does not represent any official USGS finding or policy. The software associated with this manuscript has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.


# References
