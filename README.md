# manifold-lle

***

### Code base

The code base is stored in ``2_code``.

``0_functions`` contains source files required to run the scripts, sorted into 
general utility functions (``0_utils``) and functions for the SSLLE embedding 
(``1_embedding``).
The actual SSLLE function is defined in ``fun_perform_sslle.R`` and sources some 
of the other utilities.

``1_scripts`` contains code required to produce the sensitivity analysis 
(``1_run_sensitivity_analysis.R``) and corresponding visualization 
(``2_run_visualization_experiments.R``).
``3_run_figures.R`` produces all figures used for report and presentation.
Prior to running these files, ``0_run_setup.R`` must be executed to set up 
all packages and source the required function files.

Lastly, ``2_data`` contains several temporary data files produced by the above 
scripts.

***

### In order to...

... compute an SSLLE embedding, simply use the ``perform_sslle`` function 
defined in ``fun_perform_sslle.R``

... reproduce all analyses and figures of the seminar report/presentation, 
execute all scripts in ``1_scripts`` in the given order.