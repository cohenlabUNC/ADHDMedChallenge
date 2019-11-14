#!/bin/bash


#SBATCH --output=/proj/cohenlab/projects/ADHDMedChal/ADHDMC_2019_Process/data_GLM/GLM_L1_output/out.%J
#SBATCH --mem=20000
#SBATCH --time=04:00:00
#SBATCH --job-name="GNGregL1"
#SBATCH --ntasks=7
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=macmitch@email.unc.edu

feat /proj/cohenlab/projects/ADHDMedChal/ADHDMC_2019_Process/data_GLM/sub-1048_ses-01_gng_run-01.fsf
feat /proj/cohenlab/projects/ADHDMedChal/ADHDMC_2019_Process/data_GLM/sub-1048_ses-01_gng_run-02.fsf
feat /proj/cohenlab/projects/ADHDMedChal/ADHDMC_2019_Process/data_GLM/sub-1081_ses-02_gng_run-01.fsf
feat /proj/cohenlab/projects/ADHDMedChal/ADHDMC_2019_Process/data_GLM/sub-1083_ses-02_gng_run-02.fsf


