#' @title theme_pub
#'
#' @description A theme for plots in Rmd
#' @examples ggplot(data=municipios_tlax, aes(x=municipio, y=cuenta)) +
#'  geom_bar(stat="identity") +
#'  labs(title="Número de beneficiarios por municipio", fill="Número de beneficiarios",y="Beneficiarios únicos") +
#'  scale_y_continuous(labels = comma,breaks = pretty_breaks(n = 6)) +
#'  theme_pub()
theme_pub <- function(base_size=12, font=NA){
  txt <- element_text(size = base_size+2, colour = "black", face = "plain")
  bold_txt <- element_text(size = base_size+2, colour = "black", face = "bold")

  theme_classic(base_size = base_size, base_family = font) +

  theme(plot.title = element_text(size = 30, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(angle = 45, size=18, hjust = 1),
        axis.text.y = element_text(size=22))
}

#' @title quant_labels
#'
#' @description Make quantile divisions of a data to plot
#' @param variable data.frame. Te data we need to divide
#' @param no_classes Int. The number of quantile divisions.
#' @description A theme for plots in Rmd
#' @examples municipios_nal$cuenta_q <- quant_labels(municipios_nal$cuenta)
#' @export
quant_labels <- function(variable, no_classes=6){
  quantiles <- quantile(variable,
                        probs = seq(0, 1, length.out = no_classes + 1),na.rm = TRUE)
  labels <- c()
  for(idx in 1:length(quantiles)){labels <- c(labels, paste0(round(quantiles[idx], 2)," – ", round(quantiles[idx + 1], 2))) }
  labels <- labels[1:length(labels)-1]
  variable_q <- cut(variable, breaks = quantiles,labels = labels, include.lowest = T)
  return(variable_q)
}
