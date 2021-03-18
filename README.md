# manifold-lle

***

### Code base

The code base for SSLLE is stored in ``1_sslle_implementation``.
In order to compute an SSLLE embedding, simply use the ``perform_sslle`` 
function defined in ``fun_perform_sslle.R``.
The easiest way to set up all required packages and source file is by simply 
running ``0_run_setup.R`` first.

***

### Seminar

All additional code concerning analyses and visualization is stored under 
``0_seminar/1_code``.

``0_utils`` contains source files (general utility functions and functions 
for visualization purposes) required to run the scripts besides the core SSLLE
implementation.

``1_scripts`` contains code required to produce the sensitivity analysis 
(``1_run_sensitivity_analysis.R``) and corresponding visualization 
(``2_run_visualization_experiments.R``).
``3_run_figures.R`` produces all figures used for report and presentation.
Prior to running these files, ``0_run_setup.R`` must be executed to set up 
all packages and source the required function files.

Lastly, ``2_data`` contains several temporary data files produced by the above 
scripts as well as the raw input data needed to build the *world data set*.

***

### So, n order to...

... compute an SSLLE embedding, simply use ``perform_sslle`` from 
``1_sslle_implementation``.

... reproduce all analyses and figures of the seminar report/presentation, 
execute all scripts in ``0_seminar/1_code/1_scripts``in the given order.