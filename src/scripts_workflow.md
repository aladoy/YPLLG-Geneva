**SCRIPTS WORKFLOW**

1. Mortality data cleaning: `YPLLGgeneva-Cleaning.py`
2. Mortality data wrangling: `YPLLGgeneva-Wrangling.py`
3. Spatialise GIREC's income: `YPLLGgeneva-AddInfoGIREC.py`
4. Geocode mortality data at the address level: `YPLLGgeneva-Geocoding.py`
5. Comparison with cantonal statistics: `YPLLGgeneva-CompareWithCantonalStatistics.py`
6. Compute local spatial autocorrelation statistics: `YPLLGgeneva-SpatialAnalysis_RandomSample.R`
7. Create maps for local spatial autocorrelation results: `YPLLGgeneva-Maps.R`
8. Statistical analysis of the results: `YPLLGgeneva-Descriptive_Stats.py` & `YPLLGgeneva-StatsForManuscript.R`
9. Code for Tables 1 & 2 in the manuscript: `YPLLGgeneva-SummarizeResults.R`


**Functions**

`YPLLGgeneva-FunctionSpatialAutocorrelation.R`\
`YPLLGgeneva-FunctionJoinIncome.R`
