<!-- badges: start -->
[![funding](https://img.shields.io/static/v1?label=published+through&message=LIFE+RIPARIAS&labelColor=00a58d&color=ffffff)](https://www.riparias.be/)
[![update-data](https://github.com/riparias/rato-occurrences/actions/workflows/update-data.yaml/badge.svg)](https://github.com/riparias/rato-occurrences/actions/workflows/update-data.yaml)
[![run-tests](https://github.com/riparias/rato-occurrences/actions/workflows/run-tests.yaml/badge.svg)](https://github.com/riparias/rato-occurrences/actions/workflows/run-tests.yaml)
<!-- badges: end -->

# RATO - Daily operations commissioned by the province East Flanders, Belgium

## Rationale

This repository contains the functionality to standardize the daily operations by [RATO vzw](https://oost-vlaanderen.be/wonen-en-leven/natuur-en-milieu/overlastsoorten/rattenbestrijding-.html) to a Darwin Core occurrence dataset that can be harvested by a GBIF IPT.

## Workflow

### [Update data](https://github.com/riparias/rato-occurrences/actions/workflows/update-data.yaml) GitHub Action

1. Triggers every month (or manually).
2. [src/get_data.R](src/get_data.R): Gets the latest data from RATO and write as [interim data](data/interim).
3. [src/dwc_mapping.R](src/dwc_mapping.R): Maps the data to Darwin Core and write as [processed data](data/processed).
4. Creates a PR with the changes.

### [Run tests](https://github.com/riparias/rato-occurrences/actions/workflows/run-tests.yaml) GitHub Action

1. Triggers on a PR (or manually).
1. [test/test-dwc_mapping.R](test/test-dwc_mapping.R): Tests the Darwin Core mapping.
2. Comments on the PR with test results.

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
