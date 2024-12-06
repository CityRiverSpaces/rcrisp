---
affiliations:
- index: 1
  name: Delft University of Technology, Julianalaan 134, 2628 BL Delft,
    The Netherlands
- index: 2
  name: Netherlands eScience Center, Science Park 140, 1098 XG
    Amsterdam, The Netherlands
authors:
- affiliation: 1
  corresponding: true
  equal-contrib: true
  name: Claudiu Forgaci
  orcid: 0000-0003-3218-5102
- affiliation: 2
  equal-contrib: true
  name: Francesco Nattino
  orcid: 0000-0003-3286-0139
bibliography: paper.bib
date: 2017-08-13
tags:
- urban rivers
- urban morphology
- delineation
title: "CRiSp: An R package for urban river space delineation"
toc-title: Table of contents
---

# Summary

# Introduction

Urban rivers and the urban areas surrounding them, hereinafter referred
to as urban river spaces (URS), have come to the forefront of
sustainable urban transformations worldwide. This trend is not only due
to pounding water-related disturbances such as floods or water scarcity,
but also because the vital role of rivers in supporting social and
ecological processes is increasingly acknowledged. As researchers of the
urban environment engage into understanding current URS transformations,
they face the challenge of capturing the complexities of environmental,
social, and ecological phenomena in an integrated way [@prominski2023].
Moreover, considering the global extent of the phenomenon, they lack
established frames of comparison that may reveal local specificities and
cross-case similarities. An essential part of the challenge is how urban
areas surrounding rivers are delineated---that is, how their boundaries
are drawn for analytical or intervention purposes---, as the resulting
spatial units can considerably impact decisions made for urban
riverspace transformations. More often than not, URS boundaries are
determined without a systematic and reproducible delineation method,
wherever suitable spatial data is available, based on census tracts,
using rivers as boundaries, using neutral (square or circular) cut-outs,
at different levels of aggregation (city, district, neighbourhood), or
areas of study specific to a local urban development project. None of
these approaches to delineation are suitable for a reliable integrated
understanding of URSs, as they are to a large extent arbitrary. An
integrated understanding of URS requires spatial morphological units of
analysis that reflect the combined "footprints" of environmental, social
and ecological processes [@marcus2016]. This is a case of the modifiable
areal unit problem (MAUP) [@openshaw1984] with a considerable impact on
decisions taken in environmental, mobility, ecological, and public space
planning and design.

# Statement of need

CRiSp addresses a diverse, growing and cross-disciplinary research
community concerned with understanding URSs in an integrated and
scalable way. Building on a previously developed delineation method
[@Forgaci2018], CRiSp automates the process of delineation, while
leveraging local and global spatial-temporal big data (e.g.,
OpenStreetMap and Global DEM data) increasingly available for riverspace
analysis at large [@forgaci2020].

CRiSp is a software for automated and scalable delineation of urban
river spaces with spatial-temporal big data. CRiSp provides specialized
functionality in addition to more general morphometric tools like the
Python packages momepy \[@fleischmann2019; @fleischmann2022 and OSMnx
[@boeing2024] and the R package sfnetworks [@vandermeer2024].

CRiSp is intended to open new research avenues, such as integrated local
spatial analyses and global cross-case analyses, which have been so far
hindered by data- or workflow-related difficulties. However, the
usability and effectiveness of CRiSp in addressing the needs, challenges
and various use cases of researchers and practitioners is yet to be
tested and the software needs to be adapted according to such input.

# Algorithms

-   costDist

-   shortest path

-   COINS

# Related software

# Software package overview

Modules...

-   corridor

-   delineate

-   network

-   osmdata

-   utils

-   valley

# Acknowledgements

# Author contributions

# References {#references .unnumbered}
