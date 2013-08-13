# Ovation Field Data Importer for R

Ovation is the powerful data management service engineered specifically for scientists that liberates research through organization of multiple data formats and sources, the ability to link raw data with analysis and the freedom to safely share all of this with colleagues and collaborators.

The Ovation Field Data Importer for R provides data import/export functions for field data as collected by the Inouye Lab.


## Requirements

* Java 1.6+
* R 2.15.0+
* Rovation

## Usage

    library(fielddata)
    InitRovation()
    
    context <- NewDataContext('<email>')
    
    # Import CSV data
    ImportLegacyCSV(<csv file path>, context, <protocol uri>, <experiment uri>, timezone)
    
    # Retrieve data as an R DataFrame
    exp <- context$getObjectWithURI('<experiment uri>')
    epochs <- CollectEpochs(exp)
    df <- CollectLegacyMeasurements(epochs)
    
## License
THe Ovation Field Data Importer for R is provided under the terms of the GPLv3 license (see LICENSE.txt)

The Ovation Field Data Importer for R is copyright (c) 2013 Physion LLC.

