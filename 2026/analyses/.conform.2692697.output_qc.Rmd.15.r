list_table <- list(
    #  ID |    Figure   | Descritpion
    plt_fishing_effort = c("Figure 1.3", ""),
    plt_monitoring_effort = c("Figure 1.4", ""),
    plt_monitoring_coverage = c("Figure 1.5", ""),
    data_toPlot_allPETS_plot = c("Figure 1.6", ""),
    plt_monitoring_method = c("Figure 1.7", ""),
    plt_monitoring_program_country = c("Figure 1.8", ""),
    plt_incident_programm = c("Figure 1.9", ""),
    plt_monitoring_method_country = c("Figure 1.10", ""),
    plt_em_total_das_trip = c("Figure 1.11", ""),
    plt_em_plt_incidents = c("Figure 1.12", "")
)


#a fct to shorten ecor for ggplot2
fctshoreco <- function(a) {
    sub("and the", "and the\n", a)
}
