# Create archive files to read from until data from 2021 and 2022 is restored

# In November 2023, RATO removed data from 2021 and 2022 from the source. The
# intention exists to restore this data by March 2024. In the meantime a local
# archive of this raw data was stored in the repo and is used instead.

tables_to_archive <-
  readr::read_csv("data/raw/rato_data.csv") %>%
  mutate(bewerkt_year = lubridate::year(Laatst_Bewerkt_Datum)) %>%
  filter(bewerkt_year < 2023) %>%
  group_by(bewerkt_year) %>%
  nest() %>%
  pull(data)

for (table in tables_to_archive) {
  readr::write_csv(table, here::here(
    "data",
    "raw",
    glue::glue(
      "{dataset_year}_archive_rato_data.csv",
      dataset_year = unique(lubridate::year(table$Laatst_Bewerkt_Datum))
    )
  ))
}
