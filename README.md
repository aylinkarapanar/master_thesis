# Master Thesis

## Project Structure

```
MASTER_THESIS/
├── data/                                        # Simulated data (auto-generated or from HF) [gitignored]
│   └── {seed}/
│       ├── {n_participant}_{n_items}_{prevalence}_data.csv
│       └── {n_participant}_{n_items}_{prevalence}_participants.csv
├── figures/                                     # Generated plots and figures
├── JAGS_models/                                 # JAGS model definition files
│   ├── JAGS_bias1.txt                           
│   ├── JAGS_bias2.txt                           
│   ├── JAGS_external.txt                        
│   ├── JAGS_guess.txt                           
│   ├── JAGS_hierarchical.txt                    
│   ├── JAGS_integrative.txt                     
│   ├── JAGS_internal.txt                        
│   └── JAGS_sequential.txt                      
├── parameter_estimates/                         # Parameter estimates [gitignored]
│   ├── product_space/                           # From Bayes Factor / Product Space
│   │   └── warnings/                            # Rhat and ESS warnings
│   └── non_hierarchical/                        # From WAIC and PSIS-LOO
│       ├── PSIS-LOO/                            # Per-model parameter estimates (LOO)
│       ├── WAIC/                                # Per-model parameter estimates (WAIC)
│       └── warnings/                            # Rhat and ESS warnings
├── model_assignments/                           # Model assignments per participant [gitignored]
│   ├── bf_ps/                                   # Assignments from Bayes Factor estimation using Product Space
│   │   └── prob_strat/                          # Posterior strategy probabilities
│   ├── PSIS-LOO/                                # Assignments from PSIS-LOO
│   │   └── PSIS-LOO_df/                         # Raw LOOIC values per model
│   ├── WAIC/                                    # Assignments from WAIC
│   │   └── WAIC_df/                             # Raw WAIC values per model
│   └── hbi/                                     # Assignments from HBI
│       └── prob_strat/                          # Responsibility matrices
├── hbi_output/                                  # Full HBI output (pickles) [gitignored]
│   └── {seed}/{output_base}/
│       ├── cbm_m{k}.pkl                         # Individual CBM fits per model
│       ├── hbi.pkl                              # Full HBI fit
│       └── {output_base}_full_output.pkl        # Full output object
├── metrics/                                     # Classification performance metrics
│   └── metrics_all.csv                          # Combined metrics across all methods
├── runtime/                                     # Runtime logs
│   ├── runtime_bf_ps.csv                        # BF/PS runtimes
│   ├── runtime_waic_loo.csv                     # WAIC and LOO runtimes
│   └── runtime_hbi.csv                          # HBI runtimes
├── traceplots/                                  # MCMC traceplots [gitignored]
│   ├── loo/                                     # Traceplots from LOO refits
│   └── waic/                                    # Traceplots from WAIC refits
├── data_simulation.R                            # Data simulation script
├── main.R                                       # Main R file to run all the scripts
├── metrics.R                                    # Classification metrics computation
├── parameter.R                                  # Analysis of parameter estimations
├── BF_PS.R                                      # Fitting of BHMM and its model assignment 
├── responsibility.py                            # HBI model fitting (Python)
├── requirements.txt                             # Python dependencies
├── visualisation.R                              # Visualisation script
└── WAIC_and_PSIS.R                              # Fitting of non-hierarhical models and their model assignments
```

> **Note:** Folders marked [gitignored] are not included in the repository because they
> contain large files. They are created automatically when the scripts are run.

---

## Requirements

### R
R 4.1+ is required (developed and tested on R 4.5.1). Install R via https://www.r-project.org/

R packages are managed automatically, no manual installation is required.

### Python

Python 3.10+ is required (developed and tested on Python 3.13.2). Install python via the https://www.python.org/downloads/
Python dependencies are listed in `requirements.txt` and managed via `uv`.

Install `uv` if you don't have it yet:

**macOS/Linux:**
```bash
curl -Ls https://astral.sh/uv/install.sh | sh
```

**Windows:**
```powershell
powershell -c "irm https://astral.sh/uv/install.ps1 | iex"
```

---

## Setup

### 1. Clone the repository
```bash
git clone https://github.com/aylinkarapanar/master_thesis.git
cd master_thesis
```

### 2. Create the virtual environment with uv (for responsibility.py)

**macOS/Linux:**
```bash
uv venv
source .venv/bin/activate
```

**Windows:**
```bash
uv venv
.venv\Scripts\activate
```

### 3. Install Python dependencies
```bash
uv pip install -r requirements.txt
```

### 4. Deactivate the virtual environment when done
```bash
deactivate
```

---

## Data

The `data/` directory is not included in the repository. You can obtain the data in one of two ways:

### Option 1 — Generate the data
The data is automatically generated by running `main.R` (see [How to Run](#how-to-run)).
The `data_simulation.R` script will create and populate the `data/` directory.

### Option 2 — Download from Hugging Face
Pre-generated data is available on Hugging Face [add the link when it is public]

Download and place the files in the `data/` directory before running the analysis:

---

## How to Run

All R scripts are sourced from `main.R`, so the entire analysis can be
run by executing a single command in R:

```r
source("main.R")
```
or from the terminal:

```bash
Rscript main.R 
```

R package dependencies are handled automatically at the start of each
script, no manual installation needed.

For the HBI model fitting, make sure the virtual environment is activated
first, then pass either a file or a directory containing data files:

```bash
# Run on a single file
python responsibility.py ./data/{seed}/{file}_data.csv

# Run on an entire directory
python responsibility.py ./data/
```

