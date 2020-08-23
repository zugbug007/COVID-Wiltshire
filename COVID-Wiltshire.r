library(data.table)
library(ggplot2)
data_url <- "https://c19downloads.azureedge.net/downloads/csv/coronavirus-cases_latest.csv"
raw_data <- fread(data_url, check.names = TRUE)
wiltshire_data <- raw_data[
  Area.name == "Wiltshire" &
    Area.type == "Upper tier local authority",,
  ][,Specimen.date := as.Date(Specimen.date)
    ][,c("Specimen.date","Daily.lab.confirmed.cases")][
      order(Specimen.date)
      ]
wiltshire_data <- merge(wiltshire_data,
                     data.table(Specimen.date = seq(
                       min(wiltshire_data[,Specimen.date]),
                       max(wiltshire_data[,Specimen.date]),
                       by = "1 day"
                     )), all = TRUE, by = "Specimen.date")
setkey(wiltshire_data, Specimen.date)
setnafill(wiltshire_data, type = "const", fill = 0,
          cols = c("Daily.lab.confirmed.cases"))
wiltshire_data[,roll_mean := frollmean(Daily.lab.confirmed.cases, n = 7, align = "right")]
m_wiltshire_data <- melt(wiltshire_data, id.vars="Specimen.date",
                      measure.vars = c("Daily.lab.confirmed.cases","roll_mean"))
Wiltshire_plot <- ggplot(m_wiltshire_data, aes(x = Specimen.date, y = value, fill = variable, color = variable))+
  geom_bar(data = subset(m_wiltshire_data, variable == "Daily.lab.confirmed.cases"),
           stat = "identity") +
  geom_line(data = subset(m_wiltshire_data, variable == "roll_mean")) +
  labs(x="Specimen Date", y="Number of Confirmed Cases",
       fill = "", color = "") +
  scale_fill_manual(values = c("#ff0000","#000000"),
                    labels = c("Wiltshire # Daily Confirmed cases",
                               "7 day average")) +
  scale_color_manual(values = c("#ff0000","#000000"),
                     labels = c("Wiltshire # Daily Confirmed cases",
                                "7 day average")) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  theme_bw() %+replace% theme(legend.position = "top",
                              legend.justification = "left")
ggsave(filename = "Wiltshire_COVID.png", Wiltshire_plot, width = 10, height = 6)