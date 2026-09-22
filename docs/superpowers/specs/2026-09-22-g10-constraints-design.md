# G10 Constraints Design

`cssem_constraint()` is a deliberately narrow declaration for equality and
fixed-value restrictions on selected linear structural coefficients. It applies
after shape selection to locked construct scores and solves the resulting
deterministic pooled least-squares system. It does not add a covariance-SEM
likelihood or global fit test.

The declaration accepts `equal`, a list of edge groups such as
`list(c("Y~X", "Z~X"))`, and `fixed`, a named numeric vector such as
`c("Y~X" = 0)`. Edge names use the declared `outcome~predictor` spelling.
Duplicate edges, conflicting equality groups, unknown edges, non-finite fixed
values, and rank-deficient systems fail explicitly.

The implementation is limited to selected `linear` edges on locked scores. It
rejects interaction and nonlinear selected shapes, explicit EIV overrides or
EIV bootstrap requests, information weighting, and measurement-level equality
constraints. Output retains the unconstrained shape-selection ledger and adds
constraint declarations, KKT rank/conditioning diagnostics, and a clear
locked-score associational limitation.
