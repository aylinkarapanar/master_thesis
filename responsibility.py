import numpy as np
import pandas as pd
import pickle
from pathlib import Path
from scipy.special import expit
from cbm.individual_fit import individual_fit
from cbm.optimization import Config
from cbm.hbi import hbi_main
import sys
import time
# from concurrent.futures import ProcessPoolExecutor, as_completed
# import os
import cpuinfo


# Define the loglikelihood
def bernoulli_loglik(resp, theta, eps=1e-12):
    # Protect against theta = 0 and theta = 1
    theta = np.clip(theta, eps, 1 - eps)
    return np.sum(resp * np.log(theta) + (1 - resp) * np.log(1 - theta))


# Define the models
def model_internal(parameters, data):
    diff, cue, resp = data
    b0, bint = parameters
    theta = expit(b0 + bint * diff)
    return bernoulli_loglik(resp, theta)


def model_external(parameters, data):
    diff, cue, resp = data
    (bext,) = parameters
    theta = expit(bext * cue)
    return bernoulli_loglik(resp, theta)


def model_sequential(parameters, data):
    diff, cue, resp = data
    b0, bint, bext, z = parameters

    Mcenter = -(b0 / (bint if abs(bint) > 1e-16 else 1e-16))

    use_internal = (diff < Mcenter - z) | (diff > Mcenter + z)

    theta_int = expit(b0 + bint * diff)
    theta_ext = expit(bext * cue)
    theta = np.where(use_internal, theta_int, theta_ext)

    return bernoulli_loglik(resp, theta)


def model_integrative(parameters, data):
    diff, cue, resp = data
    b0, bint, bext = parameters
    theta = expit(b0 + bint * diff + bext * cue)
    return bernoulli_loglik(resp, theta)


def model_guessing(parameters, data):
    diff, cue, resp = data
    (guess,) = parameters
    return bernoulli_loglik(resp, guess)


def model_bias1(parameters, data):
    diff, cue, resp = data
    (bias1,) = parameters
    theta = np.full(len(resp), bias1)
    return bernoulli_loglik(resp, theta)


def model_bias2(parameters, data):
    diff, cue, resp = data
    (bias2,) = parameters
    theta = np.full(len(resp), bias2)
    return bernoulli_loglik(resp, theta)


MODELS = [
    model_internal,
    model_external,
    model_sequential,
    model_integrative,
    model_guessing,
    model_bias1,
    model_bias2,
]

MODEL_NAMES = [
    "Internal",
    "External",
    "Sequential",
    "Integrative",
    "Guessing",
    "Bias D1",
    "Bias D2",
]

PRIOR_MEANS = [
    np.array([0.0, 3.0]),
    np.array([3.0]),
    np.array([0.0, 3.0, 3.0, 0.85]),
    np.array([0.0, 3.0, 3.0]),
    np.array([0.5]),
    np.array([0.1]),
    np.array([0.9]),
]

PRIOR_VARIANCE = [
    np.array([1.0, 1.0]),            
    np.array([1.0]),                 
    np.array([1.0, 1.0, 1.0, (1/6)**2]), 
    np.array([1.0, 1.0, 1.0]),      
    np.array([0.01]),                
    np.array([0.01]),                
    np.array([0.01]),               
]

# Define parameter limits (truncations)
configs = [
    Config(
        d=2, hard_bounds=np.array([[-np.inf, -np.inf], [np.inf, np.inf]]), verbose=False
    ),
    Config(d=1, hard_bounds=np.array([[1.0], [np.inf]]), verbose=False),
    Config(
        d=4,
        hard_bounds=np.array(
            [[-np.inf, -np.inf, 1.0, 0.4], [np.inf, np.inf, np.inf, 1.33]]
        ),
        verbose=False,
    ),
    Config(
        d=3,
        hard_bounds=np.array([[-np.inf, -np.inf, 1.0], [np.inf, np.inf, np.inf]]),
        verbose=False,
    ),
    Config(d=1, hard_bounds=np.array([[0.4], [0.6]]), verbose=False),
    Config(d=1, hard_bounds=np.array([[0.0], [0.15]]), verbose=False),
    Config(d=1, hard_bounds=np.array([[0.85], [1.0]]), verbose=False),
]


def make_output_base(data_file):
    p = Path(data_file)
    if "empirical" in p.stem:
        return "empirical", p.stem   # seed_name="empirical", base=filename stem
    return p.parts[1], Path(*p.parts[2:]).name.replace("_data.csv", "")


def parse_metadata(data_file):
    p = Path(data_file)
    if "empirical" in p.stem:
        return None, None, "empirical", "empirical"
    seed = int(p.parts[1])
    name = p.stem.replace("_data", "")
    parts = name.split("_")
    return int(parts[0]), int(parts[1]), parts[2], seed


def run_hbi_for_file(data_file):
    seed_file, output_base = make_output_base(data_file)
    n_participant, n_items, prevalence_type, seed = parse_metadata(data_file)

    PARAM_DIR = Path(f"./results_data/parameter_estimates/hbi/{seed_file}")
    ASSIGN_DIR = Path(f"./results_data/model_assignments/hbi/{seed_file}")
    PROB_DIR   = Path(f"./results_data/model_assignments/hbi/prob_strat/{seed_file}")
    OUT_DIR    = Path(f"./results_data/hbi_output/{seed_file}/{output_base}")

    for d in [PARAM_DIR, ASSIGN_DIR, PROB_DIR, OUT_DIR]:
        d.mkdir(parents=True, exist_ok=True)

    n_participant, n_items, prevalence_type, seed = parse_metadata(data_file)
    df = pd.read_csv(data_file)

    all_data = []
    for subj_id in sorted(df["ID"].unique()):
        subj_df = df[df["ID"] == subj_id]
        diff = subj_df["diff"].values
        cue = subj_df["cue"].values
        resp = subj_df["resp"].values
        all_data.append((diff, cue, resp))

    OUT_DIR = Path(f"./hbi_output/{seed_file}/{output_base}")
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    cbm_paths = []

    start_time = time.time()


    for k, (model, prior_mean, prior_var, name, config) in enumerate(
        zip(MODELS, PRIOR_MEANS, PRIOR_VARIANCE, MODEL_NAMES, configs), start=1
    ):
        cbm_k = individual_fit(
            all_data, model, prior_mean, prior_var, config=config
        )
        path_k = OUT_DIR / f"cbm_m{k}.pkl"
        with open(path_k, "wb") as f:
            pickle.dump(cbm_k, f)
        cbm_paths.append(str(path_k))

    cbm = hbi_main(
        all_data,
        MODELS,
        cbm_paths,
        fname=str(OUT_DIR / "hbi.pkl"),
        config={"verbose": 0, "save_prog": False},
    )

    end_time = time.time()

    params = cbm.output.parameters
    prob_strat = pd.DataFrame(cbm.output.responsibility, columns=MODEL_NAMES)
    assigned_model_idx = np.argmax(cbm.output.responsibility, axis=1)
    strat_mat = pd.DataFrame(
        {
            "ID": np.arange(1, len(assigned_model_idx) + 1),
            "strat_labels": assigned_model_idx + 1,
        }
    )

    for i, arr in enumerate(params):
        pd.DataFrame(arr).to_csv(
            PARAM_DIR / f"{output_base}_params_{i+1}.csv", index=False
        )

    strat_mat.to_csv(
        ASSIGN_DIR / f"{output_base}_strategy_assignments.csv", index=False
    )
    prob_strat.to_csv(PROB_DIR / f"{output_base}_prob_strat.csv", index=False)

    with open(OUT_DIR / f"{output_base}_full_output.pkl", "wb") as f:
        pickle.dump(cbm.output, f)

    print(f"Saved outputs for {output_base}")

    return {
        "data_file": data_file,
        "n_participant": n_participant,
        "n_items": n_items,
        "prevalence_type": prevalence_type,
        "seed": seed,
        "total_runtime": end_time - start_time,
    }


def main(data_files, runs_file="runs.csv"):

    if Path(runs_file).exists():
        runs = pd.read_csv(runs_file)
        for col in runs.columns:
            if col != "file":
                runs[col] = runs[col].astype("boolean")
    else:
        print("Creating runs file for tracking")

        runs = pd.DataFrame({
        "file": pd.Series(dtype="string"),
        "posterior_prob": pd.Series(dtype="boolean"),
        "waic": pd.Series(dtype="boolean"),
        "loo": pd.Series(dtype="boolean"),
        "hbi": pd.Series(dtype="boolean"),
        })

        runs.to_csv(runs_file, index=False)


    # Filter files that still need to run
    files_to_run = []
    for f in data_files:
        f_clean = "./" + str(Path(f))
        already_done = ((runs["file"] == f_clean) & (runs["hbi"] == True)).any()
        if already_done:
            print(f"Skipping {f}, already done.")
        else:
            files_to_run.append(f)

    # Uncomment to run on multiple cores (it runs different datafiles on different cores, recommended for multiple datafiles)
    # Note: the runtime calculation won't be accurate, due to cueing

    # Use all cores or adjust (if concurrently the computer is going to be used, set n_cores to available cores - 2 or 3)
    
    # n_cores = os.cpu_count() - 2
    # machine_name = cpuinfo.get_cpu_info()['brand_raw']

    # log_rows = []

    # print(f"Running {len(files_to_run)} file(s) on {n_cores} core(s)")

    # with ProcessPoolExecutor(max_workers=n_cores) as executor:
    #     futures = [executor.submit(run_hbi_for_file, f) for f in files_to_run]

    #     for future in as_completed(futures):
    #         try:
    #             res = future.result()
    #         except Exception as e:
    #             print(f"Worker failed: {e}")
    #             continue
            
    #         f = res["data_file"]

    #         print(f"Finished {f} in {res['total_runtime']:.2f}s")

    #         res["no_of_cores"] = n_cores
    #         res["name"] = machine_name

    #         log_rows.append(
    #             {
    #                 "n_participant": res["n_participant"],
    #                 "n_items": int(res["n_items"] / 3),
    #                 "prevalence_type": str(res["prevalence_type"]),
    #                 "seed": res["seed"],
    #                 "total_runtime": res["total_runtime"],
    #                 "no_of_cores": n_cores,
    #                 "name": str(machine_name),
    #             }
    #         )

    #         f_clean = "./" + str(Path(f))
            
    #         # Update runs.csv
    #         if f_clean in runs["file"].values:
    #             runs.loc[runs["file"] == f_clean, "hbi"] = True
    #         else:
    #             runs = pd.concat(
    #                 [
    #                     runs,
    #                     pd.DataFrame(
    #                         [
    #                             {
    #                                 "file": f_clean,
    #                                 "posterior_prob": pd.NA,
    #                                 "waic": pd.NA,
    #                                 "loo": pd.NA,
    #                                 "hbi": True,
    #                             }
    #                         ]
    #                     ),
    #                 ],
    #                 ignore_index=True,
    #             )
    #         runs.to_csv(runs_file, index=False)
    #         print(f"Updated {runs_file}")
    #         log_file = "./runtime/runtime_hbi.csv"

    #         new_row_df = pd.DataFrame([log_rows[-1]])

    #         if Path(log_file).exists():
    #             existing = pd.read_csv(log_file)
    #             new_row_df = pd.concat([existing, new_row_df], ignore_index=True)

    #         new_row_df.to_csv(log_file, index=False)

    #         print(f"Saved runtime log to {log_file}")

    n_cores      = 1
    machine_name = cpuinfo.get_cpu_info()['brand_raw']
    log_rows     = []

    print(f"Running {len(files_to_run)} file(s) sequentially")

    for f in files_to_run:
        try:
            res = run_hbi_for_file(f)
        except Exception as e:
            print(f"Failed {f}: {e}")
            continue

        print(f"Finished {f} in {res['total_runtime']:.2f}s")

        log_rows.append({
            "n_participant":  res["n_participant"],
            "n_items":        int(res["n_items"] / 3) if res["n_items"] else None,
            "prevalence_type": str(res["prevalence_type"]),
            "seed":           res["seed"],
            "total_runtime":  res["total_runtime"],
            "no_of_cores":    n_cores,
            "name":           str(machine_name),
        })

        f_clean = "./" + str(Path(f))

        if f_clean in runs["file"].values:
            runs.loc[runs["file"] == f_clean, "hbi"] = True
        else:
            runs = pd.concat([
                runs,
                pd.DataFrame([{
                    "file": f_clean,
                    "posterior_prob": pd.NA,
                    "waic": pd.NA,
                    "loo": pd.NA,
                    "hbi": True,
                }])
            ], ignore_index=True)

        runs.to_csv(runs_file, index=False)
        print(f"Updated {runs_file}")

        log_file    = "./runtime/runtime_hbi.csv"
        new_row_df  = pd.DataFrame([log_rows[-1]])

        if Path(log_file).exists():
            new_row_df = pd.concat([pd.read_csv(log_file), new_row_df], ignore_index=True)

        new_row_df.to_csv(log_file, index=False)
        print(f"Saved runtime log to {log_file}")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python responsibility.py <file_or_directory> [...]")
        sys.exit(1)

    data_files = []

    for arg in sys.argv[1:]:
        p = Path(arg)

        if p.is_file():
            data_files.append(str(p))

        elif p.is_dir():
            files = sorted(p.rglob("*data.csv")) + sorted(p.rglob("*data_merged.csv"))
            files = sorted(set(files))
            if not files:
                print(f"No matching files in {arg}")
            data_files.extend([str(f) for f in files])

        else:
            print(f"Invalid path: {arg}")

    main(data_files)
