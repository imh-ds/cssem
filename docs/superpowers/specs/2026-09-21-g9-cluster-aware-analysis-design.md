# G9 Cluster-Aware Analysis and Repeated Observations Design

**Date:** 2026-09-21

**Goal:** Add a unit-aware data and resampling contract for repeated observations
without presenting cluster bootstrap as multilevel SEM.

## Context and scope

The package currently treats rows as independent in its default measurement
splits, outer validation, and bootstrap helper. `make_splits(method = "group")`
can keep a supplied label in one partition, but the resulting independent-unit
counts are not carried through fitted objects, missing-data accounting, or
bootstrap results. `bootstrap_model()` samples rows with replacement, so it can
split a respondent or subject across resampled measurement folds and does not
identify the number of independent units behind an interval.

G9 has two deliberately separate scopes. The P2 scope adds cluster/subject-aware
splitting, cluster bootstrap resampling, row-to-unit provenance, and explicit
unit counts. The P3 scope remains a later methodological project for within/between
latent states, longitudinal measurement alignment, growth/random effects, and
survey-weighted estimation. The P2 result must never be described as a multilevel
or longitudinal SEM estimator.

## Public interfaces

### `fit_states(..., cluster = NULL)`

`cluster` is a column name in the original input data or a vector aligned to its
rows. When supplied, `fit_states()` resolves the labels before missing-data
filtering, retains the original row IDs, and either creates a grouped measurement
split or verifies that an explicit split never places one cluster in multiple
folds. Missing, non-finite, or zero-length labels fail before fitting. The fitted
object retains the aligned cluster IDs, a unit summary, the number of rows and
independent units, and the resolved design settings so `update()` and bootstrap
refits cannot silently drop the unit contract.

### `make_splits(..., method = "group", group = ...)`

The existing grouped split remains the public partition constructor. Its returned
object gains independent-unit counts and per-partition row/unit counts. Grouped
measurement and outer partitions must be checked from the labels, not inferred
from row counts. A supplied explicit integer assignment without group metadata is
accepted only when the caller also supplies cluster labels and validation confirms
that each cluster has one assignment.

### `bootstrap_model(..., resample = c("row", "cluster"), cluster = NULL)`

`resample = "row"` preserves the current behavior and is the default for fits
without cluster metadata. `resample = "cluster"` samples independent cluster
labels with replacement, concatenates every row belonging to each sampled label,
and retains draw-level unit IDs so duplicated clusters are not mistaken for new
source subjects. Measurement refits reuse a valid grouped split for the sampled
units. The result records the resampling method, original and resampled unit
counts, rows per sampled unit, and failure reasons; deterministic replicate seeds
and caller RNG preservation remain unchanged.

### `sample_accounting()` and outer validation

`sample_accounting()` gains independent-unit counts and a unit-level table for
fits, associations, and derived effects. `validate_outer()` accepts the same
cluster vector (or reads it from grouped split provenance), passes the training
cluster IDs into each refit, verifies that train/test partitions contain whole
units, and reports unit counts in partition provenance.

### Unsupported survey designs

The P2 contract does not accept probability weights, strata, finite-population
corrections, replicate weights, or design-based standard errors. The public
entry points must fail with a stable, explanatory message if unsupported design
metadata is supplied through the design validation path. Existing
`respondent_weighting = "information"` remains explicitly posterior-information
weighting and must not be relabeled as a survey weight.

## Validation and failure handling

- A cluster label must be non-missing, finite when numeric, and aligned to the
  original input rows.
- Listwise filtering subsets cluster labels by original row ID; it never
  re-encodes or reorders units.
- A cluster may not occur in more than one measurement fold or outer train/test
  partition. Violations fail before fitting or prediction metrics are reported.
- Singleton and highly unbalanced clusters remain valid but their row/unit
  counts and effective independent-unit denominator are visible in summaries.
- A cluster bootstrap must preserve whole clusters, retain duplicated draw IDs,
  and report when too few distinct units remain for a requested grouped fit.
- Unsupported survey-design metadata fails explicitly; it must not be silently
  treated as row weights or posterior-information weights.
- Status and limitation text use `cluster_resampling_only` (or an equivalent
  explicit label) and state that within/between effects, growth, and causal
  multilevel interpretation are unavailable.

## Testing contract

Tests will cover: column/vector cluster alignment; listwise filtering; automatic
and explicit grouped measurement splits; rejection of cluster leakage; grouped
outer-validation provenance; unequal cluster sizes and singleton clusters;
reproducible cluster bootstrap draws and worker-count invariance; measurement
refit behavior; unit-level sample accounting; unsupported survey metadata; and
explicit non-multilevel status/limitations.

## Future extension boundary

P3 requires a separate design before implementation: a within/between latent
state decomposition, cluster-level likelihood or estimating equations, temporal
measurement alignment, growth/random effects, and design-weighted inference. No
P2 API may expose a `multilevel = TRUE` switch or imply that grouped splits and
cluster bootstrap identify those estimands.
