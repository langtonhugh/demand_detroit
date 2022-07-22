These materials replicate a [recent paper](https://www.tandfonline.com/doi/full/10.1080/15614263.2022.2102494#.YtmYstBTq3I.twitter) published in _Police Practice and Research_ which examined the scale and composition of emergency calls for police service in Detroit, United States.

**Steps for reproduction in R**

- Download or clone the entire GitHub repository to your local machine.
- Download the [open calls for service data](https://data.detroitmi.gov/datasets/detroitmi::911-calls-for-service/about) and save it in the _data_ folder as a csv, naming it 'detroit_16_21.csv'.
- Download the [open shapefile](https://data.detroitmi.gov/datasets/current-city-of-detroit-neighborhoods/explore?location=42.352721%2C-83.099208%2C11.13) of Detroit neighborhood boundaries. This should be unzipped and the files saved into the _data_ folder.
- The two scripts in the _script_ folder can be used to replicate the paper's findings. Both scripts are almost identical: one for the raw counts/total deployment time, one for the time spent on scene.
- Running these scripts within the R project (demand_viz.Rproj) will produce and save the relevant tables and visuals referred to in the paper. 

**General**

- The download links above can be used to explore the data in your software (e.g., Excel, QGIS).
- The graphics reported in the paper can be viewed in the _visuals_ folder.
- Findings and reference tables (e.g., categorization summary) can be found in the _results_ folder.

**Contact**

If you have any questions about running the scripts or the paper itself, please get in touch.


