###########################################
#
#
#                 Tor A EM graphs
#
#
#
# R Version: 4.5.2
# Author : Guillaume Chero
# Date : September 2026
##########################################


# 1. Install packages ==================================================================

lapply(c("tidyverse", "ggplot2", "dplyr", "cowplot", "icesTAF", "glue"), library, character.only = TRUE)



# 2. Import DATA ==================================================================

if(!exists("D1")){D1 <- data.table::fread(here("data","D1_wgbyc.csv"))}
if(!exists("D2")){D2 <- data.table::fread(here("data","D2_wgbyc.csv"))}
if(!exists("D3")){D3 <- data.table::fread(here("data","D3_wgbyc.csv"))}


# 3. Parameters ==================================================================

path_export = "./2026/outputs/"


# 4. Make plot ==================================================================

## 4.1 Pivot the info ====
NofIncidents <- D3 %>%
  group_by(year, monitoringMethod, classname) %>%
  summarize(
    sum_incidents = sum(incidentsWithPingers, na.rm = TRUE) + sum(incidentsWithoutPingers, na.rm = TRUE),
  )


## 4.2 Use only Electronic Monitoring ====
Aves <- NofIncidents %>%
  filter(classname == "Aves")
Aves <- Aves %>%
  group_by(year, monitoringMethod) %>%
  summarise(sum_incidents = sum(sum_incidents)) %>%
  mutate(total_incidents_per_year = sum(sum_incidents, na.rm = TRUE)) %>%
  mutate(percentage = (sum_incidents / total_incidents_per_year) * 100)
percent_aves <- Aves$percentage[Aves$year == 2025 & Aves$monitoringMethod == "EM"]

Mammalia <- NofIncidents %>%
  filter(classname == "Mammalia")
Mammalia <- Mammalia %>%
  group_by(year, monitoringMethod) %>%
  summarise(sum_incidents = sum(sum_incidents)) %>%
  mutate(total_incidents_per_year = sum(sum_incidents, na.rm = TRUE)) %>%
  mutate(percentage = (sum_incidents / total_incidents_per_year) * 100)
percent_mammalia <- Mammalia$percentage[Mammalia$year == 2025 & Mammalia$monitoringMethod == "EM"]

glue("In France, only data collected on marine mammals - mainly cetaceans - can be analyzed. EM data from other ETP 
     species such as seabirds, elasmobranchs and reptiles cannot be considered as data analysis on these taxa cannot
     be performed without vessel crews’ approval (Figure 1.12). Along with at-sea observers, this method represents the
     largest source of bycatch records reported (Figure 1.9), with {percent_mammalia} % of reported mammal and {percent_aves} % of seabird bycatch 
     incidents recorded with EM in 2025. This technology enables continuous monitoring providing high coverage and an
     increased likelihood of recording rare bycatch events of ETP species.")


## 4.3  Trips per country per year  ====

#Use only Electronic Monitoring
EM_monitoringeffort <- D2 %>%
  filter(monitoringMethod == "EM")
## Pivot the info
EM_Grouped <- EM_monitoringeffort %>%
  group_by(year, country) %>%
  summarize(total_tripsOb = sum(tripsOb, na.rm = TRUE),
            total_daysAtSeaOb = sum(daysAtSeaOb, na.rm = TRUE))

## 4.4 PLOT EM total trips observed =======
A <- ggplot(EM_Grouped, aes(x = year, y = total_daysAtSeaOb, fill = country)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = unique(EM_Grouped$year)) +
  labs(fill = "Country") +
  xlab('') +
  ylab("Total DAS observed using EM")

B <- ggplot(EM_Grouped, aes(x = year, y = total_tripsOb, fill = country)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = unique(EM_Grouped$year)) +
  labs(fill = "Country") +
  xlab('') +
  ylab("Total trips observed using EM")

AB <- plot_grid(A + theme(legend.position = "none"),
                B + theme(legend.position = "right"),
                rel_widths = c(1,1.2), 
                nrow = 1)

taf.png(glue(path_export, "em_total_das.png"), width = 800, height = 1000, res = 200)
print(A)
dev.off()


### 4.5 N of incidents per year per taxa, stack countries ====

#Use only Electronic Monitoring
EM_taxa_bycatch <- D3 %>%
  filter(monitoringMethod == "EM")

# Drop na values and drop teleostei (Because only 3 incidents! Check this later if needed!)
EM_taxa_bycatch <- EM_taxa_bycatch %>%
  filter(!is.na(classname) & classname != "Teleostei")

## Pivot the info
EM_Taxa_Grouped <- EM_taxa_bycatch %>%
  group_by(year, country, classname) %>%
  summarize(
    sum_incidents = sum(incidentsWithPingers, na.rm = TRUE) + sum(incidentsWithoutPingers, na.rm = TRUE),
    sum_individuals = sum(individualsWithPingers, na.rm = TRUE) + sum(individualsWithoutPingers, na.rm = TRUE)
  )

#Plot individuals per taxa
plt_individuals <- ggplot(EM_Taxa_Grouped, aes(x = year, y = sum_individuals, fill = country)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = unique(EM_Taxa_Grouped$year)) +
  labs(fill = "Country") +
  ylab("Total number of individuals") +
  xlab('') +
  facet_wrap(~classname, scales = "free")
  #scale_x_continuous(limits = c(2018.5, max(EM_Taxa_Grouped$year+0.5))) #this is just to make all x-axes the same

taf.png(glue(path_export, "em_plt_individuals.png"))
print(plt_individuals)
dev.off()

#Plot incidents per taxa
plt_incidents <- ggplot(EM_Taxa_Grouped, aes(x = year, y = sum_incidents, fill = country)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = unique(EM_Taxa_Grouped$year)) +
  labs(fill = "Country") +
  ylab("Total number of incidents") +
  xlab('') +
  facet_wrap(~classname, scales = "free")
  #scale_x_continuous(limits = c(2018.5, max(EM_Taxa_Grouped$year+0.5))) #this is just to make all x-axes the same

taf.png(glue(path_export, "em_plt_incidents.png"))
print(plt_incidents)
dev.off()







