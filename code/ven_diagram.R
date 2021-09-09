# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Script to generate Venndiagram
# Claudio Zanettini, NYC
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Libraries and import data ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(here)
library(tidyverse)
library(purrr)
library(janitor)
library(ggvenn)
library(ggVennDiagram)
theme_set(theme_bw())

to_import <- list.files(here("data", "tab_vendiagram"), pattern = "csv", full.names = TRUE)

tabs <- lapply(to_import, read.csv)
names(tabs) <- sub("\\.csv", "", list.files(here("data", "tab_vendiagram"), pattern = "csv"))

tabs$rna <- rename(tabs$rna, Country = Center)
tabs$rna <- rename(tabs$rna, PPCG_Donor_Id = PPCG_Donor_ID)
tabs$wgs <- rename(tabs$wgs, PPCG_Donor_Id = PPCG_Donor_ID)

df_all <- map_dfr(tabs, function(dat){
                      dat[, c("Country", "PPCG_Donor_Id")] %>%
                              clean_names() %>%
                              distinct(ppcg_donor_id, .keep_all = TRUE)
                },
                .id = "assay")




# Clean
df_all_donors <-
  df_all %>%
  unite("assay_country", c("assay", "country"), remove = FALSE) %>%
  filter(country != "") %>%
  filter(ppcg_donor_id != "") %>%
  mutate(country =  recode( country,
                            "USA/WCM" = "USA",
                            "Danmark" = "Denmark"
  ))



length(unique(df_all_donors$ppcg_donor_id)) # 2046


ls_donors_assay <- split.data.frame(df_all_donors, df_all_donors$assay)


glob_assay <- lapply(ls_donors_assay, function(dat) dat[["ppcg_donor_id"]])

png(here("figs_tabs", "ven_diagram.png"))
ggvenn(glob_assay, text_size = 4)
dev.off()

# @@@@@@@@@@@@@@@@@@
# More Details -----
# @@@@@@@@@@@@@@@@@@


# Donors By Country and assay
cbb_palette <- c("white", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


df_all_donors %>%
  group_by(country, assay) %>%
  summarise(n = n(), .groups = "drop_last") %>%

  ggplot(aes(n, country, fill = country)) +
      geom_col(col = "black") +
      facet_wrap(vars(assay)) +
      geom_text(aes(x = n/2, label = n),  size = 3, hjust = 0.5, col = "black") +
      scale_fill_manual(values = cbb_palette) +

      labs(
        x = "Unique Donors",
        y = NULL
      ) +
      theme(legend.position = "none")


ggsave(filename = here("figs_tabs", "donors_country.png"), height = 5, width = 10)

# @@@@@@@@@@@@@@@@@@
# More Details -----
# @@@@@@@@@@@@@@@@@@


ls_donors_assay_d <- lapply(ls_donors_assay, function(x) x %>%
         select(ppcg_donor_id, country) %>%
         pull(ppcg_donor_id))

unique(Reduce(function(...) intersect(...), ls_donors_assay_d))















