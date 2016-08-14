# tablextr

Tablextr is an R package to extract data from web pages. Apart from the regular
tabular data extraction, tablextr performs additional cleaning and inference:

  * taking care of "colspan" attributes,
  * guessing the table headers based on tags' names and attributes,
  * merging tables with the same schema scattered along the web page.

## Usage

```R
library(tablextr)
aircrafts <- extractTables("http://www.flugzeuginfo.net/table_accodes_en.php")
cssProperties <- extractTables("http://www.w3schools.com/cssref/")
```

## Future work

  * column type inference
  * extracting data from subpages
  * "rowspan" attributes
