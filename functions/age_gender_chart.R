age_gender_chart <- function(data,
                             labels = FALSE) {
  age_gender_chart_data <- data |>
    dplyr::select(AGE_BAND,
                  PATIENT_GENDER,
                  TOTAL_IDENTIFIED_PATIENTS) |>
    tidyr::complete(PATIENT_GENDER,
                    AGE_BAND,
                    fill = list(TOTAL_IDENTIFIED_PATIENTS = 0))
  
  categories = c(unique(age_gender_chart_data$AGE_BAND))
  
  max <-
    max(age_gender_chart_data$TOTAL_IDENTIFIED_PATIENTS)
  min <-
    max(age_gender_chart_data$TOTAL_IDENTIFIED_PATIENTS) * -1
  
  male <- age_gender_chart_data |>
    dplyr::filter(PATIENT_GENDER == "Male")
  
  female <- age_gender_chart_data |>
    dplyr::filter(PATIENT_GENDER == "Female") |>
    dplyr::mutate(TOTAL_IDENTIFIED_PATIENTS = 0 - TOTAL_IDENTIFIED_PATIENTS)
  
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = 'bar') |>
    hc_chart(style = list(fontFamily = "Arial")) |>
    highcharter::hc_xAxis(
      list(
        title = list(text = "Age group"),
        categories = categories,
        reversed = FALSE,
        labels = list(step = 1)
      ),
      list(
        categories = categories,
        opposite = TRUE,
        reversed = FALSE,
        linkedTo = 0,
        labels = list(step = 1)
      )
    ) |>
    highcharter::hc_tooltip(
      shared = FALSE,
      formatter = JS(
        "function () {
                   return this.point.category + '<br/>' +
                   '<b>' + this.series.name + '</b> ' +
                   Highcharts.numberFormat(Math.abs(this.point.y), 0);}"
      )
    ) |>
    highcharter::hc_yAxis(
      title = list(text = "Identified patients"),
      max = max,
      min = min,
      labels = list(
        formatter = JS(
          'function () {
               result = Math.abs(this.value);
               if (result >= 1000000) {result = result / 1000000;
                                                            return Math.round(result.toPrecision(3)) + "M"}
                                  else if (result >= 1000) {result = result / 1000;
                                                              return Math.round(result.toPrecision(3)) + "K"}
               return result;
             }'
        )
      )
    ) |>
    highcharter::hc_plotOptions(series = list(stacking = 'normal')) |>
    highcharter::hc_series(
      list(
        dataLabels = list(
          enabled = labels,
          inside = FALSE,
          color = '#ed8b00',
          fontFamily = "Arial",
          formatter = JS(
            'function () {
                                  result = this.y;
                                  if (result >= 1000000) {result = result / 1000000;
                                                            return Math.round(result.toPrecision(3)) + "M"}
                                  else if (result >= 1000) {result = result / 1000;
                                                              return Math.round(result.toPrecision(3)) + "K"}
                                  return result;
                                  }'
          )
        ),
        color = "#ed8b00",
        fontFamily = "Arial",
        name = 'Male',
        data = c(male$TOTAL_IDENTIFIED_PATIENTS)
      ),
      list(
        dataLabels = list(
          enabled = labels,
          inside = FALSE,
          color = '#005eb8',
          fontFamily = "Arial",
          formatter = JS(
            'function () {
                                  result = this.y * -1;
                                  if (result >= 1000000) {result = result / 1000000;
                                                            return result.toPrecision(3) + "M"}
                                  else if (result >= 1000) {result = result / 1000;
                                                              return result.toPrecision(3) + "K"}
                                  return result;
                                  }'
          )
        ),
        color = "#005eb8",
        name = 'Female',
        fontFamily = "Arial",
        data = c(female$TOTAL_IDENTIFIED_PATIENTS)
      )
    ) |>
    highcharter::hc_legend(reversed = T)
  
  return(hc)
  
}