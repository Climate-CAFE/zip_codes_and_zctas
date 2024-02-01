# ZIP Codes and Zip Code Tabulation Areas (ZCTA)

Many US-based public health analyses are conducted at the ZIP Code level, but this administrative unit is difficult to work with from a geospatial perspective.

## What are ZIP Codes?

Zoning Improvement Plan Codes (better known as ZIP Codes) are 5- and 9-digit identifiers defined by the US Postal Service (USPS) to delineate delivery routes of postal mail. There are several important things to know about these identifiers:

+ ZIP Codes are *not* designed as areal units
+ ZIP Codes are *not* static in time (i.e., they are subject to change) (CRS 2022). 
+ ZIP Codes can span multiple cities/towns, counties, or even states -- they do not adhere to municipal boundaries (CRS 2022)
+ ZIP Codes were not designed to be reflective of populations or demographics
	+ Some ZIP Codes refer to a business campus, Post Office Boxes ("PO Box"), or other location that is not reflective of where populations live
+ USPS does not publish authoritative shapefiles delineating ZIP Codes -- shapefiles of polygons approximating ZIP Codes are available from third parties, but these are neither endorsed nor validated by USPS.

## What are Zip Code Tabulation Areas (ZCTA)?

Zip Code Tabulation Areas (ZCTA) are defined by the US Census Bureau to approximate ZIP Code areas. Census Bureau (2023) identifies several important features about these identifiers:

+ ZCTAs *are* designed as areal units
+ ZCTAs are *relatively* static in time -- they are substantively updated only at the Decennial Censuses (e.g., 2000, 2010, 2020)
+ ZCTAs can also span multiple cities/towns, counties, or states
+ ZCTAs are defined based on the majority of ZIP Codes that are represented by household addresses within a particular *census block*, which is the smallest administrative unit used by the US Census Bureau
+ Although ZCTAs and ZIP Codes often match up fairly well, there can be differences between them
	+ ZIP Codes that do not include residences, such as PO Boxes or corporate addresses, _may_ have an accompanying ZCTA

## Using ZIP Codes and ZCTAs in Research

Health data is sometimes available only at the level of the patient's home address ZIP Code, which necessitates the need to align exposure and covariate data to the ZIP Code as well. Users have two options to achieve this:

1. **Use a third-party shapefile**, such as that provided annually from Esri Business Analyst (Esri 2023), for approximating the geographic extent of ZIP Codes. Gridded exposure data can then be extracted at these boundaries for use in the analysis.
	+ Pros: Most direct comparison between health and exposure data
	+ Cons: ZIP Code shapefiles are not freely available to the public or validated by USPS; demographic and population data are not available at ZIP Code geographies; ZIP Code shapefiles change frequently
2. **Use a ZIP-Code-to-ZCTA crosswalk**, such as those provided by UDS Mapper (2023), to assign an accompanying ZCTA to each patient's home address ZCTA. Shapefiles of ZCTAs provided by the Census Bureau are then used to extract exposure data, and users can easily align with Census data provided at ZCTA geographies
	+ Pros: Relatively stable geographies (shapefiles substantively change only once per decade); data are freely and publicly available; demographic and population data are easily accessible at this resolution
	+ Cons: Some exposure-measurement error in assigning ZCTA-level information to ZIP Code-level health information, since they are not exactly the same

### References

+ Census Bureau, United States. 2023. "ZIP Code Tabulation Areas (ZCTAs)." Last updated on 10 August. https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html
+ Congressional Research Service (CRS). 2022. "Postal Primer: ZIP Codes and Boundary Review Process." Published on 14 June. https://crsreports.congress.gov/product/pdf/IF/IF12132/2 
+ Esri. 2023. "USA ZIP Code Areas." Last updated on 20 December. https://www.arcgis.com/home/item.html?id=8d2012a2016e484dafaac0451f9aea24#!
+ UDS Mapper. 2023. "ZIP Code to ZCTA Crosswalk." https://udsmapper.org/zip-code-to-zcta-crosswalk/
