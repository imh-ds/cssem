# G11 implementation plan: categorical structural outcomes

Spec: `docs/superpowers/specs/2026-09-22-g11-categorical-structural-outcomes-design.md`

## Milestones

1. **Declaration and validation** — add response-family constructors, persist
   normalized family metadata on structure objects, and document/export the
   API. Add red tests for family mapping and invalid declarations. Commit.
2. **Family-aware structural estimator** — dispatch linear structural fits to
   logistic GLM or ordinal proportional odds, validate locked response values,
   and reject unsupported nonlinear/EIV/constraint combinations. Add red/green
   estimator tests. Commit.
3. **Cross-validation and result schema** — use categorical log loss in
   structural CV; retain probability diagnostics and mark shadow gaps/EIV
   correction unavailable with explicit status. Add tests. Commit.
4. **Prediction and assessment** — add probability/class prediction modes and
   categorical assessment metrics while preserving Gaussian defaults. Add
   tests. Commit.
5. **Documentation and verification** — update `docs/gaps.md` with status and
   commit IDs, run focused and package checks, and commit documentation.
6. **Categorical inference and calibration** — report binomial/ordinal
   coefficients in likelihood units with model-based standard errors and Wald
   intervals; add optional seeded pairs-bootstrap intervals for marginal
   contrasts; test RNG restoration, reproducibility, failure reporting, and
   fixed-seed coefficient/contrast coverage before updating G11's scope and
   validation record. Commit the implementation and documentation separately.

## Constraints

- Keep Gaussian behavior byte-for-byte compatible where practical.
- Do not infer a categorical family from measurement type; it must be declared.
- Do not apply linear EIV corrections or coefficient-product mediation to
  categorical responses.
- Commit after each meaningful milestone.

## Test commands

- Focused: `Rscript -e "testthat::test_file('tests/testthat/test-structural-outcomes.R')"`
- Regression: `Rscript -e "testthat::test_dir('tests/testthat', reporter='summary')"`
- Package check where available: `R CMD check --no-manual .`
