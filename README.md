**About**

These materials reproduce the findings from [a journal paper](https://www.tandfonline.com/doi/full/10.1080/15614263.2022.2102494#.YtmYstBTq3I.twitter) published in _Police Practice and Research_ which explored police deployment time in response to emergency calls for service using data from Detroit, United States.

**Steps for reproduction in R**

- Download or clone this GitHub repository to your local machine.
- Download the data files from the corresponding [OSF repository](https://osf.io/vr58u/) and save them in the `data` folder[^1].
- Open the `demand_viz.Rproj` project file. This will launch RStudio.
- Package versions are managed by [renv](https://rstudio.github.io/renv/articles/renv.html). Execute `renv::restore()` to install the required packages in the virtual environment. You might have to install [lorenzgini](https://github.com/wsteenbeek/lorenzgini) manually from GitHub -- see comments within the scripts.
- The two scripts in the _scripts_ folder can be used to reproduce the paper's findings. Both scripts are almost identical: one for the raw counts/total deployment time, one for the time spent on scene[^2].
- Running these scripts will generate and save tables and figures in the `results` and `visuals` folders, respectively.
- Render `results_summary.Rmd` to summarize these findings in a single document. 

**Contact**

If you have any questions about running the scripts or the paper itself, please [get in touch](www.samlangton.info).

[^1]: Since publication, the calls for service data used in the paper were removed from the Detroit Police Department open data portal and replaced with comparable -- but differently structured -- data files for individual years. Hence, the raw data used for analysis has been uploaded to OSF. For ease, the same has been done for the geo data.

[^2]: As evident in the commit history, minor changes were made to the code after publication to remedy new code errors caused by a lack of package versioning. Since then, [renv](https://rstudio.github.io/renv/articles/renv.html) has been used and it should avoid these problems going forward.
