print(.libPaths())
if (!require('markdown')) install.packages('markdown', repos = 'http://cran.r-project.org')
if (!require('R2SWF')) install.packages('R2SWF', repos = 'http://cran.r-project.org')
print(.packages(TRUE))
