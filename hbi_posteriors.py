import numpy as np
import pandas as pd
import pickle
import sys
from pathlib import Path
from scipy import linalg
from scipy.stats import multivariate_normal

PARAM_NAMES = {
    1: ["b0", "bint"],
    2: ["bext"],
    3: ["b0", "bint", "bext", "z"],
    4: ["b0", "bint", "bext"],
    5: ["guess"],
    6: ["bias1"],
    7: ["bias2"],
}

N_SAMPLES  = 1000
BASE_SEED  = 1234
HBI_DIR    = Path("./results_data/hbi_output")
SAMPLE_DIR = Path("./results_data/parameter_estimates/hbi")

def make_output_base(data_file):
    p = Path(data_file)
    return p.parts[1], Path(*p.parts[2:]).name.replace("_data.csv", "")


def check_and_invert(H, label=""):
    dets = np.linalg.det(H)                      
    bad  = np.where(np.abs(dets) < 1e-10)[0]
    if len(bad) > 0:
        print(f"[{label}] Singular Hessian for subjects: {bad + 1}")
    else:
        print(f"[{label}] All Hessians invertible")
    return np.linalg.inv(H) 


def sample_posterior(P, Hinv, model_number):
    param_names = PARAM_NAMES[model_number]
    cols = {}

    for i in range(P.shape[0]):
        samples = multivariate_normal.rvs(
            mean=P[i], cov=Hinv[i],
            size=N_SAMPLES, random_state=BASE_SEED + i
        )
        samples = np.atleast_2d(samples)
        if samples.shape[0] != N_SAMPLES:
            samples = samples.T

        for j, pname in enumerate(param_names):
            cols[f"{pname}[{i + 1}]"] = samples[:, j]

    return pd.DataFrame(cols)


def generate_posteriors_for_dataset(seed_name, output_base):
    dataset_dir = HBI_DIR / seed_name / output_base
    out_dir     = SAMPLE_DIR / seed_name / "samples"
    out_dir.mkdir(parents=True, exist_ok=True)

    for k in PARAM_NAMES:
        pkl_path = dataset_dir / f"cbm_m{k}.pkl"
        if not pkl_path.exists():
            print(f"Missing: {pkl_path}")
            continue

        with open(pkl_path, "rb") as f:
            cbm_k = pickle.load(f)

        P = np.array(cbm_k.output.parameters)
        H = np.array(cbm_k.math.hessian)

        label = f"{output_base} model {k}"
        Hinv  = check_and_invert(H, label=label)
        df    = sample_posterior(P, Hinv, model_number=k)

        out_path = out_dir / f"{output_base}_posterior_{k}.csv"
        df.to_csv(out_path, index=False)
        print(f"Saved: {out_path} shape={df.shape}")


def main(targets):
    for target in targets:
        p = Path(target)

        if p.is_file() and p.suffix == ".csv":
            seed_name, output_base = make_output_base(str(p))
            print(f"\nDataset: {output_base}  seed: {seed_name}")
            generate_posteriors_for_dataset(seed_name, output_base)

        elif p.is_dir():
            for seed_dir in sorted(HBI_DIR.iterdir()):
                if not seed_dir.is_dir():
                    continue
                for dataset_dir in sorted(seed_dir.iterdir()):
                    if not dataset_dir.is_dir():
                        continue
                    print(f"\nDataset: {dataset_dir.name}  seed: {seed_dir.name}")
                    generate_posteriors_for_dataset(seed_dir.name, dataset_dir.name)

        else:
            print(f"Invalid path: {target}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python generate_hbi_posteriors.py <data_file_or_directory> [...]")
        sys.exit(1)

    main(sys.argv[1:])