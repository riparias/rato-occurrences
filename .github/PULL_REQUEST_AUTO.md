This pull request was created by the **GitHub Action** [Update data](https://github.com/riparias/rato-occurrences/actions/workflows/update-data.yaml).

This workflow:

1. Runs every month
2. Gets the latest data (output `data/interim/confirmed_observations.csv`).
3. Maps data to Darwin Core (output `data/processed/occurrence.csv`)

Todo:

1. Verify the number of changed records in `confirmed_observations.csv` and `occurrence.csv` are the same.
2. Verify that tests did not fail. Fix the code locally if they do and rerun.
3. Accept this PR.
4. Merge this PR.

Next steps:

The IPT will automatically use `data/processed/occurrence.csv` from `main` in its next auto-publication.
