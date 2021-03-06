
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Replication-Material-Environmental-Inequality-and-Residential-Sorting

This repository provides all materials for replication of results in

> Rüttenauer T. and Best H. (2021). “Environmental Inequality and
> Residential Sorting in Germany: A Spatial Time-Series Analysis of the
> Demographic Consequences of Industrial Sites”, *Demography*,
> Forthcoming.

Preprint on my homepage:
<https://ruettenauer.github.io/files/RuettenauerBest_Demographics.pdf>

Date: 2021-10-13

## Set up

The code for replication of the results requires the following folders:
“01\_Script”, “02\_Data”, “03\_Output”. All R Scripts are required in
folder “01\_Script”, all data are required in folder “02\_Data”.

To reproduce the results of the paper, the scripts need to be executed
in order.

The following packages are necessary for reproduction of main results:

``` r
install.packages("plyr")
install.packages("dplyr")
install.packages("zoo")
install.packages("rgeos")
install.packages("spdep")
install.packages("raster")
install.packages("sf")
install.packages("ggplot2")
install.packages("scales")
install.packages("tmap")
install.packages("tmaptools")
install.packages("texreg")
install.packages("stargazer")
install.packages("xtable")
install.packages("grid")
install.packages("gridExtra")
install.packages("plm")
install.packages("feisr")
```

### Scripts:

-   *00\_Read-EPER*: Reads and combines different sets of information
    from the E-PRTR database.

-   *00b\_Read-IOER*: Reads IOER tiffs and combines them to a spatial
    dataframe.

-   *01\_EPER-ToxWeights\_v16*: Adds USEtox toxicity weights to E-PRTR
    emissions

-   *02\_Match-Pollution*: Validates E-PRTR facilities against IOER land
    use and calculates the number of facilities in each municpality and
    year

-   *03\_Match-INKAR*: Adds annual information from INKAR to the
    municipality data

-   *04\_Construct-Ranks*: Constructs spatial lags and ranks for
    relevant variables. Produces Figure2.

-   *05\_Descriptives*: Produces descriptive statistics (Figure1 and
    TableA1).

-   *06\_Main-Analysis*: Performs main analyses of the paper. Produces
    Figure3, Figure4, Table1, Table2, Table3.

## Data:

All data for this paper are freely available and accessible online. To
reproduce the results, the following data are required in the folder
“02\_Data”

-   *E-PRTR\_database\_v16\_csv*: Folder containing E-PRTR database
    (v16) exports in csv format. Available here:
    <https://www.eea.europa.eu/data-and-maps/data/member-states-reporting-art-7-under-the-european-pollutant-release-and-transfer-register-e-prtr-regulation-18/european-pollutant-release-and-transfer-register-e-prtr-data-base/eprtr_v9_csv.zip/view>

-   *ioer*: Folder containing tiff exports of the IOER monitor
    (<https://www.ioer-monitor.de/>) on percent industrial land use of
    the area, named “ind\_area\_\[year\].tiff”. The files are created by
    connecting to to WCS service via QGIS
    (<https://monitor.ioer.de/monitor_api/user?id=S05RG&service=wms>),
    then exporting the single layers (1000m) for each year to tiff.

-   *inkar*: Folder containing csv exports of the INKAR database
    (<https://www.inkar.de/>). Data was exported by a) selecting all
    municipalities (“Gemeindeverbaende”), b) selecting the respective
    indicators, c) selecting all available time periods, d) exporting to
    csv. To prevent errors, it may be necessary to not download all
    indicators at once, but downloading step by step. The script
    “00\_Read-EPER.R” reads all files named
    "INKAR\_Gemeindeverbaende\*", reshapes and combines them. The data
    was exported as of 2019-10-14. The following indicators are
    required:

    -   einwohnerdichte (Population density)
    -   einwohnerunter6jahre (percent below 6 years)
    -   einwohnervon6bisunter18jahren (percent from 6 to 17 years)
    -   einwohner65jahreundlter (percent from 65 and older)
    -   einkommensteuer (income tax revenue per capita)
    -   gewerbesteuer (trade tax revenue per capita)
    -   arbeitslosigkeit (percent unemployed)
    -   anteilarbeitsloseauslnder (percent foreigners of the unemployed)
    -   frauenanteil (percent women)

-   *USEtox\_2.12*: Folder containing the USEtov (v2.12) toxicity
    weights: <https://usetox.org/model/download>

-   *vg250\_ebenen\_2017*: Folder containing municipality shapes as of
    31.12.2017 in UTM32 (vg250\_2017-12-31.utm32s.shape.ebenen.zip):
    <https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/2017/>

-   *vg1000-ew\_2017-12-31.utm32s.shape.ebenen*: Folder containing
    federal states shapes as of 31.12.2017 in UTM32 (only needed for
    maps):
    <https://daten.gdz.bkg.bund.de/produkte/vg/vg1000-ew_ebenen_1231/2017/>

-   *Gem-Gemverb\_Makro.csv* and *Gemverb\_Makro.csv*: tables exported
    from “Referenz Gemeinden, Kreise, NUTS” of INKAR municipality
    information: <https://www.inkar.de/>

Plugging together the original data, unfortunately, involves several
manual steps. If you have problems finding or exporting the required
data as described above, please get in touch. I am happy to provide the
original data as used in this study.

## System and version information

Platform: Windows 10 (x86\_64)

Version: R version 4.0.4
