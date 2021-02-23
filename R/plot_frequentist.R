#' @title Create plots of estimated versus initial values of the parameters
#'
#' @param init_values Data.frame containing initial values of the parameters
#' @param est_values Data.frame containing estimated values of the parameters
#' @param crit Vector containing the minimum value of the criterion for each repetition of the minimization
#' @param lb Vector containing the lower bounds of the estimated parameters
#' @param ub Vector containing the upper bounds of the estimated parameters
#' @param bubble Logical indicating if bubbles of size proportional to the minimum
#' values of the criterion should be plot (TRUE, default value) or not (FALSE).
#'
#' @return A named list containing one plot per parameter
#'
#' @details The number of the repetition that leads to the minimal value of the
#' criterion over all repetitions is written in white (if bubble is TRUE) or in red
#' (if bubble is false) while the other ones are written in black.
#'
#' @import ggplot2
#'
#' @keywords internal
#'
plot_estimVSinit <- function(init_values, est_values, crit, lb, ub, bubble=TRUE) {

  nb_params <- ncol(init_values)
  param_names <-  colnames(init_values)
  nb_rep <- nrow(init_values)
  ind_min_crit <- which.min(crit)

  tmp<-rbind(lb,ub,est_values,init_values)
  tmp[tmp==Inf | tmp==-Inf]<-NA
  minvalue<-apply(tmp,2,min,na.rm=TRUE); maxvalue<-apply(tmp,2,max,na.rm=TRUE)
  minvalue<-minvalue-0.05*(maxvalue-minvalue); maxvalue<-maxvalue+0.05*(maxvalue-minvalue)

  p <- list()

  for (param_name in param_names) {

    df <- data.frame(init_values=init_values[,param_name],est_values=est_values[,param_name],crit=crit)
    row.names(df) = paste0(seq(1:nb_rep))

    if (bubble) {
      tmp_aes <- aes(x=init_values, y=est_values, size = crit)
      color_best_rep <- "white"
    } else {
      tmp_aes <- aes(x=init_values, y=est_values)
      color_best_rep <- "red"
    }

    p[[param_name]] <- ggplot(df, tmp_aes) +
      labs(title=paste0("Estimated vs Initial values of ",param_name," for the different repetitions"),
           y = paste("Estimated value for", param_name),
           x = paste("Initial value for", param_name),
           fill = "Criterion")

    if (bubble) {
      p[[param_name]]  <- p[[param_name]] +  geom_point(alpha=0.5, color="red")
    }

    p[[param_name]]  <- p[[param_name]] +
      geom_text(
        label=rownames(df),
        nudge_x = 0, nudge_y = 0,
        check_overlap = T,
        show.legend = F,
        size = 4) +
      geom_text(data=df[ind_min_crit,],
                label=rownames(df[ind_min_crit,]),
                nudge_x = 0, nudge_y = 0,
                check_overlap = T,
                show.legend = F,
                size = 4, color=color_best_rep) +
      xlim(minvalue[param_name],maxvalue[param_name]) + ylim(minvalue[param_name],maxvalue[param_name])

    if (bubble) {
      if (length(unique(crit))>1) {
        p[[param_name]]  <- p[[param_name]] +  scale_size_binned(range = c(2, 20), name="Final Value of \n minimized criteria")
      } else {
        p[[param_name]]  <- p[[param_name]] +  scale_size(name="Final Value of \n minimized criteria")
      }
    }

  }

  return(p)

}
