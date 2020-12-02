# Neonatal outcomes during the COVID-19 pandemic

Authors: Felix Richter (felix.richter@icahn.mssm.edu), Nidhi/Shoshana/Sam, Sarah U. Morton, Katherine Guttmann, Benjamin Glicksberg

Manuscript google doc:
https://docs.google.com/document/d/1ZjJ51xik4TN6vsAq72jO13gJZnYDrqWSDPG_t3vfX0k/edit#

## Overview of files

- Background: papers relevant to manuscript
- Code
  - Preemie_meta.R: Sam's meta-analysis
  - Premie_graphs.R: Shoshana's trend graphs
  - ga_bw_stats_plots.R: boxplots and stats for gestational age and birthweight
  - plotting_mortality_NICU_admits.R: mortality, NICU admission, and length of stay analyses
  - preterm_counts.ipynb: jupyter notebook used to prepare counts for plotting_mortality_NICU_admits.R and ga_bw_stats_plots.R
- Data: contains aggregated counts of NICU trends. Identifiable NICU data are secured on HIPAA-compliant storage and available from Felix if IRB-approved. SQL commands and initial data cleaning scripts are available on request.
- Figures: main and supplemental figures in a single powerpoint
- Supplement: final excel table containing counts

## Data processing pipeline

1. **General neonatal data cleaning:** (all files in /data_drive/felix_richter/clarity_preprocessing/)
    - prod_sql/: pulls relevant Clarity data
    - prod_preprocess/merge_data.py: calls class from pat_db.py and helper functions from joint_functions.py to create a merged dataframe that serves as the starting point for all neonatal projects
2. **Prematurity data cleaning:** /data_drive/felix_richter/manuscript_preterm_code/preterm_df_prep.ipynb: initial data cleaning
    - Not uploaded to github bc jupyter notebooks display MRNs
3. **DiD count prep:** rdd_did_counts_prep.ipynb: prepare data for diff in diff analysis
