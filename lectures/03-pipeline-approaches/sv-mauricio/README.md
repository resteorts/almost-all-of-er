
sv-mauricio.csv
---------------
This is derived from `UNTRC_original_and_corrected.csv` which was provided to 
us by Mauricio Sadinle. It has been subject to some pre-processing, 
specifically:
* attribute names were translated from Spanish to English
* `dept` and `muni` were derived from `geocode`
* records with missing `firstname` and `lastname` were removed
* attribute values of zero for `year`, `month`, `day` and `muni` were set to 
NA
* `firstname` and `lastname` were standardized (see 
`standardize_UNTRCcorrected.r`)
In addition, ground truth identifiers have been added for a subset of the 
records: those in the departments of Cuscatlan and Ahuachapan. Of the 5395 
records, 735 have a ground truth identifier.

known-firstname.csv
--------------------
Derived from `ElSalvadorUNTRCGivenNamesPhonetic.txt` as provided by Mauricio 
Sadinle.


known-lastname.csv
------------------
Derived from `ElSalvadorUNTRCLastNamesPhonetic.txt` as provided by Mauricio 
Sadinle.


matches.csv and matches.ods
---------------------------
A list of possible matching record pairs in the departments of Cuscatlan and 
Ahuachapan (7 and 1). These were identified manually by Neil and include 
a match confidence score. Any record pairs from Cuscatlan and Ahuachapan not 
present in this list are assumed to be non-matches with certainty.
