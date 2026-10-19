# Hindcast analysis — original-script structure

This archive contains the six original R scripts with minimal path-related edits. The analytical procedures, order, exploratory sections, and forecast-horizon formulas have **not** been redesigned.

- `setwd()` statements have been removed.
- Machine-specific `C:/`, `X:/`, and `/media/...` paths were replaced by relative `file.path()` calls.
- Run scripts from a suitable project working directory with data organized under `data/` or edit the directory variables at the top.
- These scripts are **not yet a validated end-to-end reproduction package**. Some need intermediate MFCL outputs and prepared input files.
- In `yft_hindcast.R`, MFCL writes temporary files to the working directory. Run from a dedicated location and install `mfclo64` on PATH.
- Existing additional plots, original MASE horizon definition, and analytical code are retained intentionally for fidelity to the original scripts.
- Do not upload restricted fisheries input data.

The code has not been run against the original data.
