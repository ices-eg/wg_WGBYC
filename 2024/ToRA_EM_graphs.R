library(ggplot2)
library(dplyr)


#### calculate percentages for text ####

#"Along with at-sea observers, this method yet represents the source of a majority of bycatch 
#records reported (Figure 5), with 40% of reported mammal and 30% of seabird bycatch incidents recorded with EM.

## Pivot the info
NofIncidents <- D3 %>%
  group_by(year, monitoringMethod, classname) %>%
  summarize(
    sum_incidents = sum(incidentsWithPingers, na.rm = TRUE) + sum(incidentsWithoutPingers, na.rm = TRUE),
  )

#Use only Electronic Monitoring
Aves <- NofIncidents %>%
  filter(classname == "Aves")

Aves <- Aves %>%
  group_by(year, monitoringMethod) %>%
  summarise(sum_incidents = sum(sum_incidents)) %>%
  mutate(total_incidents_per_year = sum(sum_incidents, na.rm = TRUE)) %>%
  mutate(percentage = (sum_incidents / total_incidents_per_year) * 100)

#Use only Electronic Monitoring
Mammalia <- NofIncidents %>%
  filter(classname == "Mammalia")

Mammalia <- Mammalia %>%
  group_by(year, monitoringMethod) %>%
  summarise(sum_incidents = sum(sum_incidents)) %>%
  mutate(total_incidents_per_year = sum(sum_incidents, na.rm = TRUE)) %>%
  mutate(percentage = (sum_incidents / total_incidents_per_year) * 100)

Mammalia

#### Trips per country per year ####

#Use only Electronic Monitoring
EM_monitoringeffort <- D2 %>%
  filter(monitoringMethod == "EM")
## Pivot the info
EM_Grouped <- EM_monitoringeffort %>%
  group_by(year, country) %>%
  summarize(total_tripsOb = sum(tripsOb, na.rm = TRUE))

# PLOT EM total trips observed
plt <- ggplot(EM_Grouped, aes(x = year, y = total_tripsOb, fill = country)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(fill = "Country") +
  ylab("Total trips observed using EM")

print(plt)

#### N of incidents per year per taxa, stack countries ####

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
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(fill = "Country") +
  ylab("Total number of individuals") +
  facet_wrap(~classname, scales = "free") +
  scale_x_continuous(limits = c(2018.5, max(EM_Taxa_Grouped$year+0.5))) #this is just to make all x-axes the same

print(plt_individuals)

#Plot incidents per taxa
plt_incidents <- ggplot(EM_Taxa_Grouped, aes(x = year, y = sum_incidents, fill = country)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(fill = "Country") +
  ylab("Total number of incidents") +
  facet_wrap(~classname, scales = "free") +
  scale_x_continuous(limits = c(2018.5, max(EM_Taxa_Grouped$year+0.5))) #this is just to make all x-axes the same

print(plt_incidents)







