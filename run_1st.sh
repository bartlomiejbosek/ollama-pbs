#!/bin/bash

# Define variables
DATA_FILE="~/00.NewsHeadlines/01.1.Telstra_AS51TELE_df_ts.xlsx"
START_DATE="2019-05-22"
END_DATE="2019-06-13"

# Change to the working directory

# Function to run R script
run_r_script() {
    Rscript -e "$1"
}

# Run the first R script
run_r_script "
source('~/01.Script/2024.09.10.GenAI_vct_hip.R');
run_genAI_vct_hip(
  file_path = '$DATA_FILE',
  from = '$START_DATE',
  to = '$END_DATE',
  company_name = 'Commonwealth Bank'
);"
