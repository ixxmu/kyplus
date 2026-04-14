# AGENTS.md

This file defines how coding agents should operate in this repository.

## Scope
- Applies to the whole repository rooted at `kyplus/`.
- Follow explicit user instructions first; then follow this file.

## Project profile
- Language: R package (with Rcpp components).
- Key directories:
  - `R/`: package source functions.
  - `src/`: C++ and generated Rcpp glue.
  - `tests/testthat/`: testthat tests.
  - `vignettes/`: user-facing tutorials.
  - `.github/workflows/`: CI workflows.

## Required workflow for code changes
1. Read relevant files before editing.
2. Make surgical changes only for the requested scope.
3. Prefer editing existing files over creating new ones.
4. Run focused validation for touched areas.
5. Report exactly what was validated and what was not run.

## Repository-specific guardrails
- Do **not** introduce package-load side effects that mutate user environment (e.g., writing to `~/.R/*`, global `Sys.setenv()` changes in `.onLoad()`).
- Avoid `setwd()` in package runtime functions; prefer path arguments and explicit return values.
- Keep tests under `tests/testthat/` so `test_check("kyplus")` consistently discovers them.
- Do not commit build artifacts (`*.o`, `*.so`, local history files like `.Rhistory`).
- If touching `src/` or exported interfaces, ensure Rcpp-generated files are consistent and tests still pass.

## Validation commands (prefer narrow first)
- Targeted tests:
  - `devtools::test(filter = "<name>")`
- Full tests:
  - `devtools::test()`
- Package checks:
  - `devtools::check()`

## Change quality bar
- No unrelated refactors.
- No speculative abstractions.
- Keep behavior backward-compatible unless user requested a behavior change.
- Document meaningful behavior changes in user-facing docs when relevant (`README.md`, vignette, or NEWS).

## PR / review checklist
- [ ] Scope matches request.
- [ ] Tests added/updated for changed behavior.
- [ ] No new global side effects.
- [ ] No committed binary/build artifacts.
- [ ] Validation evidence included in summary.
