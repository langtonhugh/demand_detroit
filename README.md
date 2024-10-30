These materials reproduce the findings from a [paper](https://www.tandfonline.com/doi/full/10.1080/15614263.2022.2102494#.YtmYstBTq3I.twitter) published in _Police Practice and Research_ which examined the scale and composition of emergency calls for police service in Detroit, United States.

**Steps for reproduction in R**

- Download or clone this GitHub repository to your local machine.
- Download the data files from the corresponding OSF repository and save them in the `data` folder[^1].
- Open the `demand_viz.Rproj` project file. This will launch RStudio.
- Package versions are managed by [renv](https://rstudio.github.io/renv/articles/renv.html). Execute `renv::restore()` to install the required packages in the virtual environment.
- The two scripts in the _scripts_ folder can be used to reproduce the paper's findings. Both scripts are almost identical: one for the raw counts/total deployment time, one for the time spent on scene[^2].
- Running these scripts will generate and save tables and figures in the `results` and `visuals` folders, respectively.
- Render `results_summary.Rmd` to summarize these findings in a single document. 

**Contact**

If you have any questions about running the scripts or the paper itself, please [get in touch](www.samlangton.info).

[^1]: Since publication, the calls for service data used in the paper were removed from the Detroit Police Department open data portal and replaced with comparable -- but differently structured -- data files for individual years. Hence, we the raw data used for analysis has been uploaded to OSF. For ease, the same has been done for the Detroit shapefiles.

[^2]: As evident in the commit history, minor changes were made to the code after publication to remedy errors caused by a lack of package versioning. Using [renv](https://rstudio.github.io/renv/articles/renv.html) should avoid these problems going forward.
