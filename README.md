# Code to "On the efficiency of biosphere–Atmosphere ammonia exchange processes over a Scots pine forests and its link to ecosystem activity"
This github repository contains the code to reproduce the analysis of "On the Efficiency of Biosphere–Atmosphere Ammonia Exchange Processes over a Scots Pine Forests and Its Link to Ecosystem Activity", as presented in [Melman et al. (2026)](https://doi.org/10.1016/j.agrformet.2026.111358). 

## Overview
This repository contains all code necessary to reproduce the analysis and results presented in the publication: "On the Efficiency of Biosphere–Atmosphere Ammonia Exchange Processes over a Scots Pine Forests and Its Link to Ecosystem Activity." The project focuses on analyzing NH3 flux data as collected over a temperate Scots pine forest in the Netherlands between 24-Aug-2023 and 04-Sep-2024, with special interest in different exchange mechanisms and links to the ecosystem carbon uptake. The analysis includes statistical analyses and figure generation as described in the manuscript.

The repository is organized to promote transparency and reproducibility, and includes:
- A main script (`main-loobos.R`) that installs the required packages, downloads data, and sets project information and plot settings.
- Code to generate all figures and tables in the paper (`analysis/`). In `figure-1.R`, the flux footprint is calculated with the method and code (`functions/calc-footprint-FFP-climatology.R`) from [Kljun et al. (2015)](https://doi.org/10.5194/gmd-8-3695-2015).
- Code to gap-fill NH₃ fluxes. Note that this is included for reproducibility only; the gap-filled output is suitable for calculating annual deposition load, but **not** for detailed process studies.

## Data
Data is downloaded from https://zenodo.org/records/20666694. Please check if a new version of the data is available before running the code.  

## Licensing
This code is licensed under the **European Union Public Licence (EUPL) v1.2**.

- Full license text: `LICENSE`
- Copyright holder: RIVM (Rijksinstituut voor Volksgezondheid en Milieu)

When redistributing or creating derivative works, follow the obligations in the EUPL v1.2 text included in this repository.

## Citation
When using this repository, please cite: Melman, E. A., Wintjen, P., Zhang, J., Rutledge-Jonker, S., Hensen, A., Felter, K., van der Molen, M. K., Snellen, H., de Boer, J., Eijkelboom, M., Haaima, M., van der Hoff, R., van Mansom, H., Voorneveld, M., Vilà-Guerau de Arellano, J., Wichink Kruit, R. J., & van Zanten, M. C. (2026). On the efficiency of biosphere–atmosphere ammonia exchange processes over a Scots pine forest and its link to ecosystem activity. Agricultural and Forest Meteorology, 389, 111358. https://doi.org/10.1016/j.agrformet.2026.111358

## References
- **Kljun**, N., Calanca, P., Rotach, M. W., & Schmid, H. P. (2015). A simple two-dimensional parameterisation for flux footprint prediction (FFP). Geoscientific Model Development, 8(11), 3695–3713. https://doi.org/10.5194/gmd-8-3695-2015
- **Melman**, E. A., Wintjen, P., Zhang, J., Rutledge-Jonker, S., Hensen, A., Felter, K., van der Molen, M. K., Snellen, H., de Boer, J., Eijkelboom, M., Haaima, M., van der Hoff, R., van Mansom, H., Voorneveld, M., Vilà-Guerau de Arellano, J., Wichink Kruit, R. J., & van Zanten, M. C. (2026). On the efficiency of biosphere–atmosphere ammonia exchange processes over a Scots pine forest and its link to ecosystem activity. Agricultural and Forest Meteorology, 389, 111358. https://doi.org/10.1016/j.agrformet.2026.111358
