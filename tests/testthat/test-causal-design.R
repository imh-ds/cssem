test_that("causal design audits a declared backdoor adjustment set", {
  edges <- data.frame(
    from = c("C", "C", "X"),
    to = c("X", "Y", "Y"),
    type = "causal"
  )
  design <- causal_design(
    edges, treatment = "X", outcome = "Y", adjust = "C",
    assumptions = list(
      consistency = "assumed",
      no_unmeasured_confounding = "assumed",
      positivity = "assumed",
      measurement_validity = "assumed",
      temporal_order = "assumed"
    )
  )

  audit <- validate_causal_design(design)

  expect_s3_class(design, "cssem_causal_design")
  expect_true(audit$valid)
  expect_true(audit$causal_admissible)
  expect_true(audit$checks$passed[audit$checks$check == "backdoor_adjustment"])
  expect_identical(audit$graph_semantics, "causal_dag_with_separate_associations")
  expect_false(any(audit$assumptions$verification == "verified"))
})

test_that("causal design rejects unblocked backdoors and post-treatment adjustments", {
  confounded <- causal_design(
    data.frame(from = c("U", "U", "X"), to = c("X", "Y", "Y")),
    treatment = "X", outcome = "Y", adjust = character(),
    assumptions = list(no_unmeasured_confounding = "assumed")
  )
  confounded_audit <- validate_causal_design(confounded)
  expect_true(confounded_audit$valid)
  expect_false(confounded_audit$causal_admissible)
  expect_false(confounded_audit$checks$passed[
    confounded_audit$checks$check == "backdoor_adjustment"])

  post_treatment <- causal_design(
    data.frame(from = c("X", "M", "X"), to = c("M", "Y", "Y")),
    treatment = "X", outcome = "Y", adjust = "M",
    assumptions = list(no_unmeasured_confounding = "assumed")
  )
  audit <- validate_causal_design(post_treatment)
  expect_false(audit$causal_admissible)
  expect_false(audit$checks$passed[
    audit$checks$check == "no_post_treatment_adjustment"])

  collider <- causal_design(
    data.frame(from = c("U", "U", "V", "V", "X"),
      to = c("X", "K", "K", "Y", "Y")),
    treatment = "X", outcome = "Y", adjust = "K",
    assumptions = list(no_unmeasured_confounding = "assumed")
  )
  collider_audit <- validate_causal_design(collider)
  expect_true(collider_audit$checks$passed[
    collider_audit$checks$check == "no_post_treatment_adjustment"])
  expect_false(collider_audit$checks$passed[
    collider_audit$checks$check == "backdoor_adjustment"])
})

test_that("associational edges do not create causal backdoor claims", {
  design <- causal_design(
    data.frame(
      from = c("C", "C", "X"),
      to = c("X", "Y", "Y"),
      type = c("associational", "associational", "causal")
    ),
    treatment = "X", outcome = "Y", adjust = character(),
    assumptions = list(no_unmeasured_confounding = "assumed")
  )

  audit <- validate_causal_design(design)

  expect_true(audit$checks$passed[audit$checks$check == "backdoor_adjustment"])
  expect_false(audit$causal_admissible)
  expect_true(any(audit$checks$check == "no_unmeasured_confounding_assumption"))
  expect_false(audit$assumptions$passed[audit$assumptions$assumption == "consistency"])
})

test_that("causal design requires an acyclic causal graph and explicit assumption states", {
  expect_error(causal_design(
    data.frame(from = c("X", "Y"), to = c("Y", "X")),
    treatment = "X", outcome = "Y"
  ), "acyclic")

  expect_error(causal_design(
    data.frame(from = "X", to = "Y"), treatment = "X", outcome = "Y",
    assumptions = list(positivity = "verified")
  ), "assumption status")

  design <- causal_design(
    data.frame(from = "X", to = "Y"), treatment = "X", outcome = "Y"
  )
  audit <- validate_causal_design(design)
  expect_true(audit$valid)
  expect_false(audit$causal_admissible)
  expect_true(all(audit$assumptions$status == "not_assessed"))
})

test_that("mediation audits require blocked mediator-outcome backdoors", {
  # Regression: the mediation audit checked only the treatment-outcome
  # backdoor, so a declared U -> M, U -> Y confounder left unadjusted still
  # produced a causal-admissible design.
  assumed <- stats::setNames(rep(list("assumed"), 6), c("consistency",
    "no_unmeasured_confounding", "positivity", "measurement_validity",
    "temporal_order", "no_exposure_induced_mediator_outcome_confounding"))
  edges <- data.frame(from = c("C", "C", "C", "X", "X", "M", "U", "U"),
    to = c("X", "M", "Y", "M", "Y", "Y", "M", "Y"))
  confounded <- validate_causal_design(causal_design(edges, "X", "Y", adjust = "C",
    assumptions = assumed), estimand = "mediation")
  expect_false(confounded$checks$passed[confounded$checks$check == "mediator_outcome_backdoor"])
  expect_false(confounded$causal_admissible)
  expect_true(validate_causal_design(causal_design(edges, "X", "Y", adjust = "C",
    assumptions = assumed))$causal_admissible)

  blocked <- validate_causal_design(causal_design(edges, "X", "Y", adjust = c("C", "U"),
    assumptions = assumed), estimand = "mediation")
  expect_true(blocked$causal_admissible)

  serial <- data.frame(from = c("C", "C", "X", "M1", "M2", "X"), to = c("X", "Y", "M1", "M2", "Y", "Y"))
  expect_true(validate_causal_design(causal_design(serial, "X", "Y", adjust = "C",
    assumptions = assumed), estimand = "mediation")$causal_admissible)
})
