# Resume the national US fit

<!-- checkpoint status -->
## Saved checkpoints

| Model | Saved stages | Complete model |
| --- | ---: | --- |
| Presidential | 3 of 3 | [Ready](data/output/us-2026-national-b0bb47d/pres_fit.rds) |
| House | 0 of 3 | Pending |
<!-- end checkpoint status -->

Run from the VoteFrame repository root with the same code as commit
`b0bb47d`. The checkpoint directory contains the R libraries from this
Codespace. If reopening on a different R version or operating system,
install vglmer 1.0.8, gKRLS 1.0.5, SQUAREM, and knitr for that system.

The fitted-model files, R library, and logs live on this Codespace's
workspace disk and are ignored by Git. The restart and prediction scripts
are included in the repository. Reopen the same Codespace to use them after stopping it. A deleted
Codespace or a new clone will not have these files. Check that the model's
full RDS appears as Ready above before relying on that model to restart.

The Codespace was configured with a 30-minute idle timeout when checked.
Closing the laptop leaves it running until that timeout or the Codespace
lifetime limit. If it stops during House, the completed presidential fit
is already on the workspace disk and can be reused. [GitHub Codespaces
lifecycle](https://docs.github.com/en/codespaces/about-codespaces/understanding-the-codespace-lifecycle)

Before starting another run, inspect the [run log](data/output/us-2026-national-b0bb47d/run.log).
At 15:56 UTC on November 24, 2026, all presidential stages had converged
and the full presidential fit was verified. House No Vote had finished
iteration 2 and was still running. No House stage had been saved yet.
If the original process is still advancing, let it continue.
Otherwise run the following. The script skips each completed model when
its full RDS file is present. If interrupted during House, it restarts
House using the vignette seed and keeps the presidential fit.

```sh
run_dir="$PWD/data/output/us-2026-national-b0bb47d"
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 R_GC_MEM_GROW=0 \
  R_LIBS="$run_dir/library" VOTEFRAME_US_OUTPUT_DIR="$run_dir" \
  Rscript "$run_dir/resume.R" > "$run_dir/resume.log" 2>&1
```

After both model RDS files exist, run the pre-rake benchmark comparisons
with 125 draws per state and DC:

```sh
run_dir="$PWD/data/output/us-2026-national-b0bb47d"
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  R_LIBS="$run_dir/library" VOTEFRAME_US_OUTPUT_DIR="$run_dir" \
  Rscript "$run_dir/predict.R" > "$run_dir/predict.log" 2>&1
```
