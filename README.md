<!-- badges: start -->
[![funding](https://img.shields.io/static/v1?label=published+through&message=LIFE+RIPARIAS&labelColor=00a58d&color=ffffff)](https://www.riparias.be/)
[![update-data](https://github.com/riparias/rato-occurrences/actions/workflows/update-data.yaml/badge.svg)](https://github.com/riparias/rato-occurrences/actions/workflows/update-data.yaml)
[![run-tests](https://github.com/riparias/rato-occurrences/actions/workflows/run-tests.yaml/badge.svg)](https://github.com/riparias/rato-occurrences/actions/workflows/run-tests.yaml)
<!-- badges: end -->

# Management actions by RATO in East Flanders, Belgium

## Rationale

This repository contains the functionality to standardize the management actions by [RATO vzw](https://oost-vlaanderen.be/wonen-en-leven/natuur-en-milieu/overlastsoorten/rattenbestrijding-.html) to a Darwin Core occurrence dataset that can be harvested by a GBIF IPT.

## Workflow

### [Update data](https://github.com/riparias/rato-occurrences/actions/workflows/update-data.yaml) GitHub Action

1. Triggers every Monday (or manually).
2. Get the latest data using [src/get_data.R](src/get_data.R) and creates an issue on failure.
3. On success, writes the output to [interim data](data/interim) and stops if no changes were detected.
4. Transforms the data to Darwin Core using [src/dwc_mapping.R](src/dwc_mapping.R) and creates an issue on failure.
5. On success, writes the output to [processed data](data/processed).
6. Runs tests using [test/test-dwc_mapping.R](test/test-dwc_mapping.R) and creates an issue on failure.
7. On success, creates a PR with the changes.
8. Merges PR.

### [INBO IPT](https://ipt.inbo.be/resource?r=rato-occurrences)

1. Periodically reads data from this repository and auto-publish.

## Published dataset

* [Dataset on the IPT](https://ipt.inbo.be/resource?r=rato-occurrences)
* [Dataset on GBIF](https://doi.org/10.15468/fw2rbx)

## Repo structure

The repository structure is based on [Cookiecutter Data Science](http://drivendata.github.io/cookiecutter-data-science/) and the [Checklist recipe](https://github.com/trias-project/checklist-recipe). Files and directories indicated with `GENERATED` should not be edited manually.

```
├── README.md
├── LICENSE
├── rato-occurrences.Rproj
├── DESCRIPTION: R dependencies
├── .gitignore
│
├── .github
│   ├── PULL_REQUEST_TEMPLATE_AUTO.md : PR template used by get-data.yaml
│   └── workflows
│       ├── update-data.yaml
│       └── run-tests.yaml
│
├── src
│   ├── update_data.R
│   └── dwc_mapping.R
│
├── tests
│   └── test-dwc_mapping.R
│
└── data
    ├── reference: Reference data to be used in mapping
    ├── interim: GENERATED
    └── processed: GENERATED
```
