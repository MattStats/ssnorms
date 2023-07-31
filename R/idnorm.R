idnorm <- function(data = athletes_trials, columnName = definition.result, metrics, activity, rounding = 2, identification, Date1 = min(data$Date), Date2 = max(data$Date), Unit = definition.unit) {
  stopifnot("AthleteId is incorrect" = any(data$id == identification), "This sport is not available in this dataset" = any(data$sport == activity))
  dplyr::group_by(data, sport, {{columnName}}, definition.unit) %>%
    dplyr::filter(sport == activity, {{columnName}} %in% metrics,
                  dplyr::between(Date, as.Date(Date1), as.Date(Date2))) %>%
    dplyr::summarise(id, value, Percentile = rank(value)/ length(value),
                     Zscore = (value - mean(value))/(sd(value)), Date) %>%
    dplyr::arrange(desc(Date))  %>%
    dplyr::filter(id == identification)  %>%
    dplyr::mutate_if(is.numeric, round, digits = rounding)  %>%
    kableExtra::kable(caption = "Percentile Norms")  %>%
    kableExtra::column_spec(1:8, border_left = T, border_right = T)  %>%
    kableExtra::kable_styling()
}
