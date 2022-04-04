#'  Descriptive figures demonstrating the predictive capabilities of EVI.
#'
#' @param graph Type of graph to be plotted
#' @param ln If the data would be transformed through the natural logarithm befor plotting.
#'
#'
#' @examples
#' evi.plot(graph=c("cases",ln=T))
#'
#' @export

evi.plot=function(graph=c("cases","pv_plus","pv_minus"), ln=T) {

  library(ggplot2)
  library(cowplot)

  EVI_output$cases_1=EVI_output$Cases*EVI_output$Index
  EVI_output$cases_1[EVI_output$cases_1 == 0] <- NA
  EVI_output$cases_0=EVI_output$Cases*(1-EVI_output$Index)
  EVI_output$cases_0[EVI_output$cases_0 == 0] <- NA

  EVI_output$pvn=EVI_output$pvn*(1-EVI_output$Index)
  EVI_output$pvn[EVI_output$pvn == 0] <- NA
  EVI_output$pvs=EVI_output$pvs*EVI_output$Index
  EVI_output$pvs[EVI_output$pvs == 0] <- NA

  if (graph=="cases" && ln==F) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=(Cases), color=Index>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      theme(legend.position = "none")+
      labs(y = "Cases", x="Days")
      #ggtitle(, subtitle = subt)
  }

  if (graph=="cases" && ln==T) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=log(Cases), color=Index>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      theme(legend.position = "none")+
      labs(y = "ln(Cases)", x="Days")
    #ggtitle(, subtitle = subt)
  }

  if (graph=="pv_plus" && ln==F) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=(cases_1), col=pvs), size=0.5)+
      geom_point(aes(y=(cases_0)), col="grey69", size=0.5)+
      labs(y = "Cases", x="")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "PPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
  }

  if (graph=="pv_plus" && ln==T) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=log(cases_1), col=pvs), size=0.5)+
      geom_point(aes(y=log(cases_0)), col="grey69", size=0.5)+
      labs(y = "ln(Cases)", x="")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "PPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
  }

  if (graph=="pv_minus" && ln==F) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=(cases_0), col=pvn), size=0.5)+
      geom_point(aes(y=(cases_1)), col="grey69", size=0.5)+
      labs(y = "Cases")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "NPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
  }


  if (graph=="pv_minus" && ln==T) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=log(cases_0), col=pvn), size=0.5)+
      geom_point(aes(y=log(cases_1)), col="grey69", size=0.5)+
      labs(y = "ln(Cases)")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "NPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
  }

  print(sp3)

  }












