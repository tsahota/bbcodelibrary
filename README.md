# bbcodelibrary
## Interaction with [QCP code library at Bitbucket](https://bitbucket.org/qcpadmin/qcpcodelibrary/) with the REST API

#### New projects
* Browse the code library
* Preview script and copy into new projects
#### Existing projects
* Check for updated versions of copied scripts
* Submit new scripts to the code library

## Installation

Make sure [tidyproject](github.com/tsahota/tidyproject) is running.

Install the bbcodelibrary package:

```R
install.packages("devtool")
devtools::install_github("rgray1/bbcodelibrary")
library(bbcodelibrary)
```
## Tutorial

Make a [tidyproject](github.com/tsahota/tidyproject):

```R
make_project("path/to/directory")
```
Open the newly created Rstudio project with File -> Open Project. **Warning: do not use setwd() to open tidyprojects.**

You should see a new directory structure.

List the scripts available in the code library:

```R
ls_scripts()
```

See more information about scripts in the library:

```R
code_library(fields=c("Description", "Author"))
```

See information about specific scripts:

```R
info_scripts("AUC.R", fields=c("Author", "Keywords"))
```

Preview a specific script:

```R
preview_script("AUC.R")
```
Copy a script into your local tidyproject:

```R
copy_script("AUC.R")
```

Check if a locally copied script has since been updated in the code library:

```R
check_for_updates("AUC.R")
```

Submit a new/modified script to the code library:

```R
submit_script("new_script.R")
```



