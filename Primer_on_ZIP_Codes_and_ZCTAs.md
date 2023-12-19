# ZIP Codes and Zip Code Tabulation Areas (ZCTA)

Many US-based public health analyses are conducted at the ZIP Code level, but this administrative unit is difficult to work with from a geospatial perspective.

## What are ZIP Codes?

Zoning Improvement Plan Codes (better known as ZIP Codes) are 5- and 9-digit identifiers defined by the US Postal Service (USPS) to delineate delivery routes of postal mail. There are several important things to know about these identifiers:

+ ZIP Codes are *not* designed as areal units
+ ZIP Codes are *not* static in time -- they are subject to continuous change based on changes in population distributions, delivery routes, or mail volume [1]. 
+ ZIP Codes can span multiple cities/towns, counties, or even states -- they do not adhere to municipal boundaries
+ ZIP Codes were not designed to be reflective of populations or demographics
	+ Some ZIP Codes refer to a business campus, Post Office Boxes ("PO Box"), or other location that is not reflective of where populations live
+ USPS does not publish authoritative shapefiles delineating ZIP Codes -- shapefiles of polygons approximating ZIP Codes are available from third parties, but these are neither endorsed nor validated by USPS.

## What are Zip Code Tabulation Areas (ZCTA)?

Zip Code Tabulation Areas (ZCTA) are defined by the US Census Bureau to approximate ZIP Code areas. There are several important things to know about these identifiers:

+ ZCTAs *are* designed as areal units
+ ZCTAs are *relatively* static in time -- they are substantively updated at every Decennial Census (e.g., 2000, 2010, 2020)
+ ZCTAs can also span multiple cities/towns, counties, or states
+ ZCTAs are defined based on the majority of ZIP Codes that are present in households within a particular *census block*, which is the smallest administrative unit used by the US Census Bureau.
+ Although ZCTAs and ZIP Codes often match up fairly well, there can be substantial differences between them
	+ ZIP Codes that do not include residences, such as PO Boxes, will not have an accompanying ZCTA, since the Census Bureau products are meant to reflect where people live

## Using ZIP Codes and ZCTAs in Research

Health data is sometimes available only at the level of the patient's home address ZIP Code, which necessitates the need to align exposure and covariate data to the ZIP Code as well. Users have two options to achieve this:

1. **Use a third-party shapefile**, such as that provided annually from Esri Business Analyst, for approximating the geographic extent of ZIP Codes. Gridded exposure data can then be extracted at these boundaries for use in the analysis.
	+ Pros: Most direct comparison between health and exposure data
	+ Cons: ZIP Code shapefiles are not freely available to the public or validated by USPS; demographic and population data are not available at ZIP Code geographies; ZIP Code shapefiles change very frequently
2. **Use a ZIP-Code-to-ZCTA crosswalk** to assign an accompanying ZCTA to each patient's home address ZCTA. Shapefiles of ZCTAs provided by the Census Bureau are then used to extract exposure data, and users can easily align with Census data provided at ZCTA geographies
	+ Pros: Relatively stable geographies (shapefiles substantively change only once per decade); data are freely and publicly available; demographic and population data are easily accessible at this resolution
	+ Cons: Some exposure-measurement error in assigning ZCTA-level information to ZIP Code-level health information, since they are not exactly the same

### References

[1] 
