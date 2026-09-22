# G8 Group Comparison and Measurement Invariance Design

**Date:** 2026-09-21

**Goal:** Add a common-anchor group comparison workflow that reports measurement parameter differences and structural path contrasts without treating separately fitted latent scales as comparable.

## Context and scope

The current package fits one pooled cross-fitted measurement encoder and one associational structural model. It has no public group argument, invariance diagnostic, or path-difference test. Users can fit each group separately, but those fits may use different latent locations and scales, so direct comparisons are not identified by the current API.

G8 will provide a useful, scale-safe diagnostic layer around the existing fit. The pooled encoder remains the sole score anchor. Group-specific item parameters are refit conditional on those locked pooled scores; they are diagnostics for item functioning on the common scale, not a new joint latent-variable estimator. Structural paths are refit within groups from the same pooled locked scores and compared by a reproducible permutation procedure.

The implementation will not claim full lavaan-style multi-group equality-constrained MML, ordinal threshold invariance, or a calibrated DIF likelihood-ratio test. Those remain a later extension requiring a joint estimation design. The returned object will expose this limitation in its status and metadata.

## Public interfaces

### `measurement_invariance(fit, group, reference = NULL, min_group_size = 20L, level = .95, tolerance = 0.10)`

`fit` is a `fit_states` object. `group` is either a column name in the original input data or a vector aligned to the original input rows. The function resolves listwise row filtering through `fit$row_ids`, rejects missing group labels and fewer than two groups, and retains the declared group ordering. `reference` selects one group; otherwise the first observed group is used.

The result has class `cssem_measurement_invariance` and contains:

- `groups`: group counts, minimum-size status, and a clear small-group warning when any group is below `min_group_size`;
- `constructs`: pooled locked-score means, SDs, and pairwise reference differences;
- `item_parameters`: one row per group/item/parameter, using the existing graded-response discrimination/threshold and continuous intercept/slope/residual-SD definitions;
- `item_contrasts`: reference-group contrasts with absolute differences and tolerance flags for discrimination/slope and threshold/intercept changes;
- `status`: `"diagnostic_common_anchor"`, never a conventional metric/scalar invariance label;
- `limitations`: text explaining that item parameters are conditional diagnostics and that formal constrained multi-group estimation is unavailable.

Manifest constructs are included in group score summaries but receive explicit unavailable item-parameter rows. Ordinal categories are matched to the pooled encoder's declared levels; unseen categories and non-finite values are reported as unavailable rather than silently recoded. Group-specific item fits use the pooled locked construct score as the conditioning variable, with graceful unavailable rows for sparse or failed fits.

### `group_comparison(association, group, reference = NULL, permutations = 999L, seed = 1L, min_group_size = 20L, level = .95)`

`association` is a `cssem_association` object. The function resolves groups through `association$fit`, uses the association's locked-score rows, and fits each declared structural outcome with the selected shape per predictor inside each group. Linear and product edges return scalar estimates; nonlinear edges return an explicit unavailable reason because a single slope is not a shape-invariant contrast. Pairwise reference differences use a fixed group-label permutation test (two-sided p-values) when `permutations > 0`; zero disables inference and returns the raw differences.

The result has class `cssem_group_comparison` and contains `group_effects`, `contrasts`, `permutation_settings`, `groups`, `status = "associational_group_contrast"`, and limitations. Permutations preserve the observed group sizes and recompute the same declared edge estimator, so no independently fitted measurement scales enter the test.

## Validation and failure handling

- Group labels must be finite/non-missing after alignment and must identify at least two groups.
- Every requested group must contain enough complete rows for the declared edge; otherwise that estimate is `NA` with an availability reason.
- A group below `min_group_size` is retained but marked `small_group = TRUE` and produces a warning. This avoids hiding the requested comparison while preventing a small group from reading as calibrated evidence.
- Reference groups must be present and unique. Pairwise contrasts are always directed `group - reference`.
- Permutation seeds are isolated through the package's existing seed-preservation helper; repeated calls with the same seed and settings are identical.

## Testing contract

Tests will cover: group-column and vector alignment; pooled-score scale preservation; ordinal threshold and discrimination shifts; continuous slope/intercept shifts; manifest availability; sparse-group warnings; path contrasts and reproducible permutation p-values; unsupported nonlinear path estimates; and invalid/missing group inputs.

## Future extension boundary

The implementation leaves formal configural/metric/scalar model comparison, jointly estimated shared/free ordinal thresholds and discriminations, and calibrated ordinal DIF tests as future work. `gaps.md` will remain `Partial (core workflow implemented)` until that joint estimator exists.
