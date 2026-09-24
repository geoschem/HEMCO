# Security Policy

## Supported Versions

HEMCO does not maintain long-term-support branches. Security fixes are only provided for the most recently released version, listed in `CHANGELOG.md`.

## Reporting a Vulnerability

If you believe you have found a security vulnerability in this repository — for example, a supply-chain issue in a GitHub Actions workflow, or an issue in a run-directory/data-download script (`run/download_data.py`, `run/createRunDir.sh`, etc.) that could lead to unintended code execution — please report it privately using GitHub's **[Report a vulnerability](https://github.com/geoschem/hemco/security/advisories/new)** feature (Security tab) rather than opening a public issue.

The threat class that matters most here is **arbitrary code execution when reading a data/config file**. HEMCO and its helper scripts read many files that users download or share: `HEMCO_Config.rc`, `HEMCO_Diagn.rc`, and the other `.rc` configuration files; `run/download_data.yml` (read by `run/download_data.py`); and NetCDF emissions inventories. If any of these can be crafted to make HEMCO or a script run code or shell commands, please report it.

If the issue is specific to a model that couples to HEMCO (GEOS-Chem, GCClassic, GCHP) or to a sibling submodule (Cloud-J, HETP) rather than to this repository, please report it in that repository instead.

This project is maintained by the **GEOS-Chem Support Team (GCST)** on a best-effort basis, so there is no guaranteed response SLA, but we will acknowledge reports as promptly as we can and work with you on a fix and coordinated disclosure.

## Out of Scope

Scientific-correctness bugs, numerical issues, and general "how do I..." questions are **not** security reports. Please use the normal channels described in `SUPPORT.md` and `CONTRIBUTING.md` ([GitHub issues](https://github.com/geoschem/geos-chem/issues/new/choose)) for those instead.
