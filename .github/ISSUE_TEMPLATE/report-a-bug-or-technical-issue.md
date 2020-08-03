---
name: Report a bug or technical issue
about: Use this template to report bugs and technical issues encountered while using HEMCO.
title: "[BUG/ISSUE]"
labels: bug
assignees: ''

---

# Report a HEMCO bug or technical issue
<!--- STOP!  BEFORE YOU SUBMIT THIS ISSUE, PLEASE READ THE FOLLOWING: -->
<!--- -->
<!--- 1. If this is the first time you are submitting a HEMCO issue via Github, we recommend that you first view -->
<!--- our tutorial videos at this link: https://www.youtube.com/c/geoschem -->
<!--- --->
<!--- 2. Only post bug reports in this issue.
<!--- To report ask a general HEMCO question, start a discussion, or add a feature request, please use this link: -->
<!--- https://github.com/geoschem/HEMCO/issues/new/choose -->
<!--- --->
<!--- 3. Contact the GEOS-Chem Working Groups directly for assistance with scientific questions --->
<!--- Please keep in mind that the GEOS-Chem Support Team] focuses primarily on software development and user support -->
<!--- rather than on scientific research. If your question is more scientific in nature (e.g. "What happens if I change -->
<!--- this reaction rate from X to Y?", or "Is emissions inventory A better than inventory B?", etc.), then we recommend -->
<!--- that you contact the relevant GEOS-Chem Working Group for assistance. -->
<!--- -->
<!--- 4. Check to see if your issue has already been resolved -->
<!--- Before submitting a HEMCO bug/issue report, please take a moment to check -->
<!--- if a solution for your issue has already been reported in this issue tracker. -->
<!--- -->
<!--- 5. Try to resolve the issue yourself -->
<!--- Compile HEMCO with debugging options and turn on VERBOSE:3 and WARNINGS:3 -->
<!--- in the HEMCO Configuration file.  This may provide some insight into the issue. -->

## Describe the bug:

### Expected behavior: ###
<!--- Include a clear and concise description of what you expected to happen. -->

### Actual behavior: ###
<!--- Include a clear and concise description of the bug or issue that you have encountered. -->

### Steps to reproduce:g:
<!--- Include the steps that must be done in order to reproduce the observed behavior:

**Compilation commands**
<!--- Please list all the steps you did to to compile GEOS-Chem in the spaces marked by `__`. -->
1.  __
2.  __
3.  __

**Run commands**
<!--- Please list all the steps you did to run GEOS-Chem in the spaces marked by `__`. -->
1.  __
2.  __
3.  __

### Error messages
<!--- Please cut and paste any error message output where it says `add text here`. --->
```
add text here
```

## Required information:

### Your HEMCO version and runtime environment:
<!--- Please supply the requested information in the spaces marked by `__`. -->
 - HEMCO version: __
 - Compiler version: __
 - netCDF version: __
 - netCDF-Fortran version (if applicable): __
 - Did you run on a computational cluster, on the AWS cloud: __
   - If you ran on the AWS cloud, please specify the Amazon Machine Image (AMI) ID: __
 - Are you using GEOS-Chem "out of the box" (i.e. unmodified): __
   - If you have modified GEOS-Chem, please list what was changed: __

### Input and log files to attach
<!--- Please supply the requested information in the spaces marked by `__` -->
<!--- You can drag and drop files to this window and Github will upload them to this issue. --->
<!--- NOTE: Any text files (*.F90, *.rc, input.geos, log files) must have the `.txt` suffix --->
<!--- or Github will not be able to display them. --->
 - lastbuild: __
 - HEMCO.log: __
 - HEMCO_Config.rc: __
 - HEMCO_sa_Config.rc: __ (for HEMCO standalone)
 - HEMCO_sa_Grid.rc: __ (for HEMCO standalone)
 - HEMCO_sa_Time.rc: __ (for HEMCO standalone)
 - HEMCO_sa_Spec.rc: __ (for HEMCO standalone)
 - slurm.out or any other error messages from your scheduler: __
 - Any other error messages: __

### Additional context
<!--- Include any other context about the problem here. -->
