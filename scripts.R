library(tidyverse)
library(paletteer)
library(showtext)

showtext_auto()
showtext_opts(dpi = 300)


#--------Get DC data from clinicaltrials.gov---------

dc_all <- readr::read_csv("data/dc_clinical.csv")  %>% 
  janitor::clean_names() %>% 
  select(nct_number, study_status, conditions, study_type)

colorblind_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                        "#D55E00", "#CC79A7", "#999999", "#882255", "#44AA99")

alternative_palette <- c("#8E0152", "#F1B6DA", "#B3CDE3", "#A1D76A", "#762A83", 
                         "#5AAE61", "#D9F0D3", "#E66101", "#B2182B", "#FDDBC7")

# --------Cancer keywords---------
keywords <- "Carcinoma|Malignant|Melanoma|Cancer|Glioblastoma|Melanoma|Metastasis|
Glioma|Neoplasms|Neoplasm|Lymphoma|Mesothelioma|Myeloma|Tumor|Tumors|Sarcoma|
Adenocarcinoma|Leukemia|Malignancies|Malignancy|Pancreatic|Adenocarcinoma of the Prostate|
Gliomas|Angiosarcoma|NSCLC|Neoplasia"

#------Completed studies--------------------
dc_complete <- dc_all %>% 
  filter(study_status == "COMPLETED") %>% 
  filter(!str_detect(conditions, "\\|"))

# Add conditions_1 to identify tumors from others
dc_complete <- dc_complete %>% 
  mutate(conditions_1 = ifelse(str_detect(conditions, keywords), "Cancer", "Others"))

# Tumors v non-tumors
dc_complete_cancer <- dc_complete %>%
  filter(str_detect(conditions, keywords))

dc_complete_others <- dc_complete %>%
  filter(!str_detect(conditions, keywords))

conditions_complete <- data.frame(
  conditions = c("Cancer (70.4 %)", "Others (29.6%)"),
  values = c(238, 100)
)

# Calculate the percentages and cumulative percentages
conditions_complete <- conditions_complete %>%
  mutate(percentage = values / sum(values) * 100) %>%
  arrange(desc(conditions))

# Create the donut chart
complete_plot <- conditions_complete %>% 
  ggplot(aes(x = 2, y = percentage, fill = conditions)) +
  geom_bar(stat = "identity", width = 1, color = "white")  +
  coord_polar(theta = "y") +
  xlim(0.1, 2.5) +
  scale_fill_manual(values = colorblind_palette) +
  theme_void() +
  labs(fill = NULL) +
  theme(legend.position = "none")

ggsave("images/completed_studies.png", complete_plot)

# Tumors in detail
tumor_complete <- subset(dc_complete, conditions_1 == "Cancer") %>% 
  mutate(type = case_when(
    str_detect(conditions, "Breast") ~ "Breast",
    str_detect(conditions, "Melanoma") ~ "Melanoma",
    str_detect(conditions, "Glioblastoma|Gliomas") ~ "Glioblastoma",
    str_detect(conditions, "Leukemia") ~ "Leukemia",
    str_detect(conditions, "Colon") ~ "Colon",
    str_detect(conditions, "Cervical") ~ "Cervical",
    str_detect(conditions, "Liver") ~ "Liver",
    str_detect(conditions, "Prostate") ~ "Prostate",
    str_detect(conditions, "Solid") ~ "Solid",
    str_detect(conditions, "Renal") ~ "Renal",
    str_detect(conditions, "Gastric|Gastrointestinal") ~ "Gastrointestinal",
    str_detect(conditions, "Ovarian") ~ "Ovarian",
    str_detect(conditions, "Colorectal") ~ "Colorectal",
    str_detect(conditions, "Pancreatic") ~ "Pancreatic",
    str_detect(conditions, "Myeloma") ~ "Myeloma",
    str_detect(conditions, "Lymphoma") ~ "Lymphoma",
    str_detect(conditions, "Lung") ~ "Lung",
    str_detect(conditions, "Hematological|Blood|Hematologic") ~ "Blood",
    str_detect(conditions, "Mesothelioma") ~ "Mesothelioma",
    str_detect(conditions, "Brain") ~ "Brain",
    str_detect(conditions, "Sarcoma") ~ "Sarcoma",
    str_detect(conditions, "Kidney") ~ "Kidney",
    str_detect(conditions, "Bladder") ~ "Bladder",
    str_detect(conditions, "Dendritic Cell") ~ "Dendritic Cell",
    str_detect(conditions, "Kidney") ~ "Kidney",
    str_detect(conditions, "Kidney") ~ "Kidney",
    TRUE ~ conditions  # This sets conditions if neither condition is met
  ))

tumor_complete_counted <- tumor_complete %>% 
  count(type) %>% 
  filter(n>4) %>% 
  filter(!(type %in% c("Colorectal", "Renal"))) %>% 
  arrange(desc(n)) %>% 
  add_row(type = "Others", n = 1) %>% 
  mutate(percentage = round(n / sum(n) * 100, 1))

tumor_complete_counted$type <- factor(
  tumor_complete_counted$type,
  levels = c("Melanoma", "Prostate", "Breast", "Glioblastoma", "Lung",
             "Leukemia", "Myeloma", "Brain", "Pancreatic", "Others"),
  labels = c("Melanoma (34.1 %)", "Prostate (18.5 %)", "Breast (8.1 %)",
             "Glioblastoma (8.1 %)", "Lung (8.1 %)", "Leukemia (6.7 %)",
             "Myeloma (6.7 %)", "Brain (4.4 %)", "Pancreatic (4.4 %)", 
             "Others (0.7 %)"))

tumor_complete_plot <- tumor_complete_counted %>% 
  ggplot(aes(2, percentage, fill = type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = colorblind_palette) +
  theme_void() +
  labs(fill = "Cancer")# +
  #theme(legend.position = "none")

# Non tumors in detail
others_complete <- subset(dc_complete, conditions_1 == "Others") %>% 
  mutate(type = case_when(
    str_detect(conditions, "Diabetes|diabetes") ~ "Diabetes",
    str_detect(conditions, "HIV") ~ "HIV",
    str_detect(conditions, "Peanut Allergy|Graft-Versus-Host Disease|Immune Disease|
               Rheumatic|Arthritis|Graves Disease|Multiple Sclerosis|
               Immunity|Rheumatoid Arthritis|Graft Versus Host Disease|
               Living Donor Liver Transplantation") ~ "Autoimmune Disease",
    TRUE ~ conditions  # This sets conditions if neither condition is met
  )) %>% 
  filter(conditions != "Healthy")

others_complete_counted <- others_complete %>% 
  count(type) %>% 
  filter(n>2) %>% 
  add_row(type = "Others", n = 0.5) %>% 
  mutate(percentage = round(n / sum(n) * 100, 1))

others_complete_counted$type <- factor(
  others_complete_counted$type,
  levels = c("Autoimmune Disease","HIV", "Diabetes", "Asthma", "Others"),
  labels = c("Autoimmune (40.9 %)", "HIV (36.1 %)", 
             "Diabetes (11.3 %)", "Asthma (9.9 %) ", "Others (1.8 %)")
)

others_complete_plot <- others_complete_counted %>% 
  ggplot(aes(2, percentage, fill = type)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
  scale_fill_manual(values = colorblind_palette) +
    theme_void() +
    labs(fill = "Others")


# Ongoing studies-------------------
dc_ongoing <- dc_all %>% 
  filter(study_status != "COMPLETED")

# Add conditions_1 to identify tumors from others
dc_ongoing <- dc_ongoing %>% 
  mutate(conditions_1 = ifelse(str_detect(conditions, keywords), "Cancer", "Others"))

dc_ongoing_cancer <- dc_ongoing %>%
  filter(str_detect(conditions, keywords))

dc_ongoing_others <- dc_ongoing %>%
  filter(!str_detect(conditions, keywords))

conditions_ongoing <- data.frame(
  conditions = c("Cancer (66.6 %)", "Others (33.4 %)"),
  values = c(108, 54)
)

# Calculate the percentages and cumulative percentages
conditions_ongoing <- conditions_ongoing %>%
  mutate(percentage = values / sum(values) * 100) %>%
  arrange(desc(conditions))

# Create the donut chart
ongoing_plot <- conditions_ongoing %>% 
  ggplot(aes(x = 2, y = percentage, fill = conditions)) +
  geom_bar(stat = "identity", width = 1, color = "white")  +
  coord_polar(theta = "y") +
  xlim(0.1, 2.5) +
  scale_fill_manual(values = colorblind_palette) +
  theme_void() +
  labs(fill = NULL)

# Tumors in detail (ongoing)
tumor_ongoing <- subset(dc_ongoing, conditions_1 == "Cancer") %>% 
  mutate(type = case_when(
    str_detect(conditions, "Breast") ~ "Breast",
    str_detect(conditions, "Melanoma|Skin") ~ "Melanoma",
    str_detect(conditions, "Glioblastoma|Gliomas|Glioma") ~ "Glioblastoma",
    str_detect(conditions, "Leukemia") ~ "Leukemia",
    str_detect(conditions, "Colon") ~ "Colon",
    str_detect(conditions, "Cervical") ~ "Cervical",
    str_detect(conditions, "Liver|Hepatocellular") ~ "Liver",
    str_detect(conditions, "Prostate") ~ "Prostate",
    str_detect(conditions, "Solid") ~ "Solid",
    str_detect(conditions, "Renal") ~ "Renal",
    str_detect(conditions, "Gastric|Gastrointestinal") ~ "Gastrointestinal",
    str_detect(conditions, "Ovarian") ~ "Ovarian",
    str_detect(conditions, "Colorectal") ~ "Colorectal",
    str_detect(conditions, "Pancreatic") ~ "Pancreatic",
    str_detect(conditions, "Myeloma") ~ "Myeloma",
    str_detect(conditions, "Lymphoma") ~ "Lymphoma",
    str_detect(conditions, "Lung") ~ "Lung",
    str_detect(conditions, "Hematological|Blood|Hematologic") ~ "Blood",
    str_detect(conditions, "Mesothelioma") ~ "Mesothelioma",
    str_detect(conditions, "Brain") ~ "Brain",
    str_detect(conditions, "Sarcoma") ~ "Sarcoma",
    str_detect(conditions, "Kidney") ~ "Kidney",
    str_detect(conditions, "Bladder") ~ "Bladder",
    str_detect(conditions, "Dendritic Cell") ~ "Dendritic Cell",
    str_detect(conditions, "Kidney") ~ "Kidney",
    str_detect(conditions, "Kidney") ~ "Kidney",
    TRUE ~ conditions  # This sets conditions if neither condition is met
  ))

tumor_ongoing_counted <- tumor_ongoing %>% 
  count(type) %>% 
  filter(n>4) %>% 
  # filter(!(type %in% c("Colorectal", "Renal"))) %>% 
  arrange(desc(n)) %>% 
  add_row(type = "Others", n = 1) %>% 
  mutate(percentage = round(n / sum(n) * 100, 1))

tumor_ongoing_counted$type <- factor(
  tumor_ongoing_counted$type,
  levels = c("Breast", "Leukemia", "Melanoma", "Glioblastoma", "Solid",
             "Ovarian", "Lung", "Dendritic Cell", "Prostate", "Others"),
  labels = c("Breast (19.6 %)", "Leukemia (15.2 %)", "Melanoma (14.1 %)",
             "Glioblastoma (10.9 %)", "Solid (10.9 %)", "Ovarian (9.8 %)",
             "Lung (7.6 %)", "Dendritic Cell (5.0 %)", "Prostate (5.0 %)", 
             "Others (1.1 %)"))

tumor_ongoing_plot <- tumor_ongoing_counted %>% 
  ggplot(aes(2, percentage, fill = type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = alternative_palette) +
  theme_void() +
  labs(fill = "Cancer")

# Non tumors in detail
others_ongoing <- subset(dc_ongoing, conditions_1 == "Others") %>% 
  mutate(type = case_when(
    str_detect(conditions, "Diabetes|diabetes") ~ "Diabetes",
    str_detect(conditions, "HIV") ~ "HIV",
    str_detect(conditions, "Peanut Allergy|Graft-Versus-Host Disease|Immune Disease|
               Rheumatic|Arthritis|Graves Disease|Multiple Sclerosis|
               Immunity|Rheumatoid Arthritis|Graft Versus Host Disease|
               Living Donor Liver Transplantation|Transplant|Graft-versus-host|
               Immune-related Adverse Event|Immunotherapy") ~ "Autoimmune Disease",
    str_detect(conditions, "Covid-19") ~ "Covid-19",
    str_detect(conditions, "Asthma") ~ "Asthma",
    str_detect(conditions, "Anemia") ~ "Anemia",
    str_detect(conditions, "Psoriasis") ~ "Psoriasis",
    str_detect(conditions, "Horton Disease") ~ "Horton Disease",
    TRUE ~ conditions  # This sets conditions if neither condition is met
  )) %>% 
  filter(!(conditions %in% c("Healthy","GBM", "High Grade Glioma|Diffuse Intrinsic Pontine Glioma", "Gliomas",
                             "WHO Grade III Gliomas|WHO Grade IV Gliomas", "Diffuse Hemispheric Glioma, H3 G34-Mutant",
                             "Angiosarcoma", "Diffuse Hemispheric Glioma, H3 G34-Mutant",
                             "Diffuse Intrinsic Pontine Glioma (DIPG)|Brain Stem Glioma",
                             "Neuroblastoma|Diffuse Intrinsic Pontine Glioma" )))
others_ongoing$type
others_ongoing_counted <- others_ongoing %>% 
  count(type) %>% 
  filter(n>1) %>% 
  add_row(type = "Others", n = 0.5) %>% 
  mutate(percentage = round(n / sum(n) * 100, 1))

others_ongoing_counted$type <- factor(
  others_ongoing_counted$type,
  levels = c("Autoimmune Disease","HIV", "Asthma", "Anemia", "Covid-19", "Psoriasis", "Others"),
  labels = c("Autoimmune (46.2 %)", "HIV (10.3 %)","Asthma (10.3 %)", 
             "Anemia (10.3 %)", "Covid-19 (10.3 %)", "Psoriasis (10.3 %)", "Others (2.6 %)")
)

others_ongoing_plot <- others_ongoing_counted %>% 
  ggplot(aes(2, percentage, fill = type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = alternative_palette) +
  theme_void() +
  labs(fill = "Others")

