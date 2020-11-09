---
name: HEMCO version release checklist
about: Use this template to create a checklist for a new HEMCO version release.
title: "[RELEASE-CHECKLIST]"
labels: 'checklist'
assignees: ''

---

# Checklist for releasing HEMCO X.Y.0

## Tests
<!--- Make sure that HEMCO compiles and runs properly within the following model contexts. -->
<!--- Place an `x` in each box that applies: -->
- [ ] Validated that HEMCO works in GEOS-Chem "Classic"
- [ ] Validated that HEMCO works in GCHPctm
- [ ] Validated that HEMCO works in GEOS
- [ ] Validated that HEMCO works in WRF-GC
- [ ] Validated that HEMCO works in CESM
- [ ] Validated that HEMCO works with the standalone interface

## Github repositories
<!--- Place an `x` in each box that applies: -->
- [ ] Push tags for this version https://github.com/geoschem/HEMCO
- [ ] Publish new release(s) on Github
- [ ] Create DOI with Zenodo.org

## Data
<!--- Place an `x` in each box that applies: -->
- [ ] Document updated data directories for current version on wiki
- [ ] Copy updated data files to Compute Canada (this should be done as part of the GEOS-Chem release process anyway)

## Wiki and website updates
<!--- Place an `x` in each box that applies: -->
- [ ] Update [The HEMCO Users Guide](http://wiki.geos-chem.org/The_HEMCO_Users_Guide)
- [ ] Update [Main page of the GEOS-Chem wiki](http://wiki.geos-chem.org/) with the HEMCO version number

## YouTube tutorials
<!--- Place an `x` in each box that applies: -->
- [ ] TBD

## Announcement
<!--- Place an `x` in each box that applies: -->
- [ ] Send email to user list announcing release