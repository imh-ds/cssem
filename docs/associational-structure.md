# Associational structural layer

associate() takes a fitted measurement object and a theory-declared
cssem_structure, then models the locked construct states. Structural
relationships are associational by default. The declared graph and its
predictive results do not by themselves identify causal direction. Causal
effects and both associational and causal indirect-effect estimands are
separate public workflows with distinct assumptions; see the
[method specification](method-spec.md).

## Declare a model and select shapes

Declare outcomes with specify_structure() formulas. Unwrapped predictors use
the "auto" policy. Formula markers let a theory specify the candidate shape
for an edge:

    structure <- specify_structure(
      Quality ~ auto_monotone(Trust),
      Loyalty ~ linear(Trust) + Quality,
      order = c("Trust", "Quality", "Loyalty")
    )

auto compares linear, monotone-increasing, monotone-decreasing, and
natural-spline forms (df = 3, 4 by default). auto_monotone() compares the
linear and monotone forms; linear(), monotone_increasing(),
monotone_decreasing(), and smooth() restrict the declared candidates.
These are formula syntax markers recognized by specify_structure(), not
standalone callable functions. At most one nonlinear edge is retained per
outcome. Curvature must pass the selector's robust test and predictive-gain
criteria. Repeated cross-validation reports stability; when an eligible
monotone curve is predictively indistinguishable from a more flexible shape,
the selector prefers the simpler monotone curve. These rules control a limited
shape search; they are not confirmatory tests of causal effects.

An interaction must be written explicitly with formula A:B syntax. The
selector does not discover undeclared interactions, feedback loops, or latent
interactions, and shape markers cannot wrap an interaction. Gaussian outcomes
are the default. Explicit binomial and ordinal outcomes are available, with
linear main effects only; categorical outcomes do not support EIV correction,
information weighting, or constrained paths.

For eligible Gaussian linear, monotone, and product terms, associate() can
report a reliability-based errors-in-variables correction. It corrects
predictor measurement error with a stabilized covariance adjustment; the
reliability floor or covariance shrinkage may affect the result, so correction
is not an accuracy guarantee. Smooth terms are not corrected. Optional
corrected-effect bootstrap intervals condition on the selected shapes.
respondent_weighting = "information" remains experimental and is off by
default.

## Shadow benchmarks

Declare an ordering when theory supplies a temporal order:

    structure <- specify_structure(
      Quality ~ auto_monotone(Trust),
      Loyalty ~ linear(Trust) + Quality,
      order = c("Trust", "Quality", "Loyalty")
    )

With the default shadow_scope = "both", outcomes receive cross-validated
shallow-tree benchmarks:

| Benchmark | Eligible predictors | Substantive question |
| --- | --- | --- |
| temporal | Locked constructs earlier than the outcome in order | Given the declared ordering, how does the theory model predict relative to earlier constructs? |
| unrestricted | Other same-wave locked constructs | How much predictive information lies outside the declared directional model? |

For Trust → Quality → Loyalty, Loyalty is excluded from the temporal Quality
shadow because it follows Quality in the declared order. It remains available
to the unrestricted Quality shadow. A better unrestricted benchmark may
reflect downstream association, feedback, common causes, or measurement
overlap; it does not establish that the theory arrow is reversed.

The reported specification gap is theory cross-validated performance minus
shadow cross-validated performance. Positive values favor the declared theory
model over that benchmark; negative values favor the benchmark and can flag
omitted predictors, interactions, nonlinear behavior, or same-wave dependence.
They do not automatically replace the declared theory.

    association <- associate(fit, structure)
    effect_card(association, "Loyalty")
    effect_ledger(association)
    specification_gap(association, "temporal")
    specification_gap(association, "unrestricted")

effect_ledger() reports selected shapes, repeated-CV stability, predictive
contribution when each edge is removed, and the shadow gaps. It is an evidence
profile, not a causal verdict or a calibrated confidence interval.

## Contrasts and constrained paths

parameter_table() assigns stable IDs to structural edges and derived effects,
including separate :naive and :corrected edge aliases. Use contrast_spec() for
arithmetic over those IDs:

    spec <- contrast_spec(list(
      difference = "edge:Loyalty~Trust:naive - edge:Loyalty~Quality:naive",
      product = "edge:Quality~Trust:naive * edge:Loyalty~Quality:naive"
    ))
    contrast(association, spec, reps = 999, seed = 42)

All referenced terms share each row or cluster bootstrap draw. Fixed-selection
intervals condition on the selected shapes. selection = "repeat" reruns the
shape decision and retains per-replicate shape signatures. Unavailable
corrected terms and invalid ratios remain NA with an availability reason.

For a prespecified linear locked-score restriction, declare equality groups or
fixed coefficients with cssem_constraint() and pass the object to
associate(). The estimator is deterministic pooled constrained least squares
and reports rank and conditioning diagnostics. Constraints reject EIV
correction, information weighting, interactions, and nonlinear selected edges;
they do not turn this associational layer into a likelihood SEM.

Use compare_outer() for paired held-out comparison of two completed
validate_outer() results. Both results must retain identical partitions,
observation rows, score bases, target availability, and outer_test metric
scope. compare_models() reuses one cssem_splits object for two theories.
When construct names differ, pass an explicit named model-B-to-model-A
alignment map only when indicators and observed target bases are genuinely
common. The comparison reports predictive metric deltas and partition
bootstrap intervals; it does not expose AIC/BIC, likelihood-ratio tests, or
global SEM fit statistics.
