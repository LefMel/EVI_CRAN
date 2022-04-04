#' Epidemic Volatility Index Graphs
#' 
#' This function  produces  plots of the time series data with the EVI predictions.
#'
#' @return 
#' Three types of plots are generated:
#' (i) A plot of the confirmed cases with red dots corresponding to time points that an early warning was issued and grey dots corresponding to time points without an early warning indication.
#' (ii) A plot of the confirmed cases with colored dots corresponding to time points with an early warning. Color intensity is increasing with higher positive predictive value (PPV).
#' (iii) A plot of the confirmed cases with colored dots corresponding to time points without an early warning. Color intensity is increasing with higher negative predictive value (NPV).
#'
#' An EVI_output is required as input, derived from the \code{\link[EVI:deviant]{deviant()}} function.
#'
#' @param EVI_output output of the \code{\link[EVI:deviant]{deviant()}} function
#' @param graph Type of graph to be plotted. Options: "EVI", "PPV", "NPV". "EVI" (the default) is giving a plot of the confirmed cases, with red dots corresponding to time points that an early warning was issued and grey dots corresponding to time points without an early warning indication. "PPV" is giving a plot of the confirmed cases with colored dots corresponding to time points with an early warning. Color intensity is increasing with higher PPV. "NPV" is giving a plot of the confirmed cases with colored dots corresponding to time points without an early warning. Color intensity is increasing with higher NPV.
#' @param ln  TRUE or FALSE; If TRUE (the default) the output of the graph will be presented on the logarithmic scale. IF FALSE the output data will be presented on the original scale.
#' @param type By default, points are plotted on EVI graphs. In cases where, changes are very sudden or data sparsely available, type="l" introduces lines on top of points for the "EVI" type of graph.
#'
#' @examples
#'      \dontrun{
#'         # Epidemic Volatility Index (EVI) Explained:
#' 	       vignette('EVI', package='EVI')
#' 	       
#'	       # For information on how to cite EVI:
#'	       citation('EVI')
#'	    }
#'
#' @export
#'
#' @import ggplot2
#' @import cowplot
#'
#' @references
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}


evi.graphs <- function(EVI_output,graph=c("EVI"), ln=TRUE, type="p") {
  
  
  if (!exists("EVI_output"))
    stop("Please run the deviant function first")
  
  #EVI_output=temp
  EVI_output$cases_1=as.numeric(EVI_output$Cases*EVI_output$Index)
  EVI_output$cases_1[EVI_output$cases_1 == 0] <- NA
  EVI_output$cases_0=as.numeric(EVI_output$Cases*(1-EVI_output$Index))
  EVI_output$cases_0[EVI_output$cases_0 == 0] <- NA

  EVI_output$npv=as.numeric(EVI_output$npv*(1-EVI_output$Index))
  EVI_output$npv[EVI_output$npv == 0] <- NA
  EVI_output$ppv=as.numeric(EVI_output$ppv*EVI_output$Index)
  EVI_output$ppv[EVI_output$ppv == 0] <- NA
  EVI_output$variable<-"x"

  if (graph=="EVI" && ln==FALSE) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
      geom_point(aes_string(y=("Cases"), color="Index>0"), size=0.5),
      scale_color_manual(values=c("grey69", "red3")),
      theme(legend.position = "none"),
      labs(y = "Cases", x="Days"),
    if (type=="l")  geom_path(aes_string(y="Cases",colour="factor(Index>0)"))
    )
      }

  if (graph=="EVI" && ln==TRUE) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
      geom_point(aes_string(y="log(Cases)", color="Index>0"), size=0.5),
      scale_color_manual(values=c("grey69", "red3")),
      theme(legend.position = "none"),
      labs(y = "ln(Cases)", x="Days"),
      if (type=="l")  geom_path(aes_string(y="log(Cases)",colour="factor(Index>0)"))
      )
     }

  if (graph=="PPV" && ln==FALSE) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
      geom_point(aes_string(y="(cases_1)", col="ppv"), size=0.5),
      geom_point(aes_string(y="(cases_0)"), col="grey69", size=0.5),
      labs(y = "Cases", x=""),
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1)),
      labs(color= "PPV"),
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
      )
      }

  if (graph=="PPV" && ln==TRUE) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
      geom_point(aes_string(y="log(cases_1)", col="ppv"), size=0.5),
      geom_point(aes_string(y="log(cases_0)"), col="grey69", size=0.5),
      labs(y = "ln(Cases)", x=""),
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1)),
      labs(color= "PPV"),
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
      )
    }

  if (graph=="NPV" && ln==FALSE) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
      geom_point(aes_string(y="(cases_0)", col="npv"), size=0.5),
      geom_point(aes_string(y="(cases_1)"), col="grey69", size=0.5),
      labs(y = "Cases"),
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1)),
      labs(color= "NPV"),
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
      )
  }


  if (graph=="NPV" && ln==TRUE) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
      geom_point(aes_string(y="log(cases_0)", col="npv"), size=0.5),
      geom_point(aes_string(y="log(cases_1)"), col="grey69", size=0.5),
      labs(y = "ln(Cases)"),
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1)),
      labs(color= "NPV"),
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
      )
  }

  print(sp3)

  }












