#' helpers
#'
#' @param data data frame, with "genotype" as column names, and variables defined in "var_more" and "var_less"
#' @param var_more character vector that must match with column names of data. It contains variables to be maximized. One of `rhot_table` or `var_more` and `var_less` must be non NULL.
#' @param var_less character vector that must match with column names of data. It contains variables to be minimized One of `rhot_table` or `var_more` and `var_less` must be non NULL.
#' @param rhot_table a data frame with columns "trait", "direction", "opti_val" and "weight". Direction must be one of "min", "max" or "opti". In that case, the "opti_val" column should be filled for this line.
#' @param SI selection intensity in percentage of number of unique genotypes in the table. Default to 15, must be between 0 and 100.
#' @param mineval parameter of `mgidi` function for the number of factors to retain. Default to 1.
#' @param weights weigths to apply to variables (if not provided by "rhot_table"), optional. If defined, it must be between 0 and 1 and defined for all variables.
#' @param avg_NA logical to indicate if missing data would be replaced by population average to avoid the exclusion of genotypes in MGIDI.
#'
#' @description This function computes the MGIDI algorithm to select genotypes based on a selection index.
#'
#' @return a list with two elements: "res_mgidi" the MGIDI output and "data_mean", the averaged data per genotype for all selected variables.
#' @import dplyr

calc_mgidi <- function(data, var_more=NULL, var_less=NULL,
                       rhot_table=NULL, SI=15,
                       mineval=1, weights=NULL,
                       avg_NA=FALSE){
  #print("in calc_mgidi function")
  `%>%` <- magrittr::`%>%`


  if(!is.null(rhot_table)){
    rhot_table <- janitor::remove_empty(rhot_table, which = c("rows","cols"))
    vmore <- rhot_table$trait[rhot_table$direction == "max"]
    vless <- rhot_table$trait[rhot_table$direction %in% c("min","opti")]
  } else {
    vmore <- var_more
    vless <- var_less
  }
  # print(paste0("var more: ", vmore))
  # print(paste0("var less: ", vless))
  stopifnot(all(c("genotype",vmore,vless) %in% colnames(data)),
            SI > 0, SI<100)

  ## STEP 1: reformat data
  ## set at long format, select columns
  dt_long <- data %>% dplyr::select(c(genotype,vmore, vless)) %>%
    tidyr::pivot_longer(cols=c(vmore, vless))

  ## group and average trait values by genotype
  dt_mean_long <- dt_long %>%
    dplyr::group_by(genotype,name) %>%
    dplyr::summarise(Mean=mean(value, na.rm=TRUE))

  ## re-set at wide format
  dt_mean_wide <- dt_mean_long %>%
    tidyr::pivot_wider(values_from=Mean)

  dt_mean_wide <- as.data.frame(dt_mean_wide)
  if(avg_NA){
    dt_mean_wide[,c(vmore,vless)] <- apply(dt_mean_wide[,c(vmore,vless)],2,
                                           function(x){
                                             tidyr::replace_na(x, replace=mean(x, na.rm=TRUE))
                                           }
                                           )
  }
  rownames(dt_mean_wide) <- dt_mean_wide$genotype
  dt_mean_wide <- dt_mean_wide[,c(vmore,vless),drop=FALSE]
  dat_mgidi <- na.omit(dt_mean_wide)

  ## STEP 2 Use rhandsontable as input if provided
  if(!is.null(rhot_table)){
    ideotype <- plyr::mapvalues(rhot_table$direction,
                                from=c("min","max","opti"),
                                to=c("l","h","l"),
                                warn_missing = FALSE)
    names(ideotype) <- rhot_table$trait
    # print(ideotype)
    weights <- rhot_table$weight
    if(is.null(weights)) weights <- rep(1,nrow(rhot_table))
    names(weights) <- rhot_table$trait

    ## for optimum values, calculate difference between each observation and optimum
    for(tr in rhot_table$trait[rhot_table$direction == "opti"]){
      dat_mgidi[[tr]] <- abs(dat_mgidi[[tr]] - rhot_table$opti_val[rhot_table$trait == tr])
    }
  } else {
    ideotype=c(rep("h",length(var_more)),
               rep("l",length(var_less)))
    if(is.null(weights)) weights <- rep(1,length(ideotype))
  }


  # print(colnames(dat_mgidi))
  # print(ideotype[colnames(dat_mgidi)])
  # print(weights[colnames(dat_mgidi)])
  ## STEP 3 apply mgidi
  res_mgidi <- metan::mgidi(.data=dat_mgidi,
                            use_data="pheno",
                            SI=SI, # selection intensity
                            mineval=mineval, # value for number of factors retained
                            ideotype=ideotype[colnames(dat_mgidi)],
                            weights=weights[colnames(dat_mgidi)],
                            use="pairwise.complete.obs", verbose=FALSE)
  if(length(res_mgidi) > 1){
    write("mgidi applied with success", stderr())
  }
  res_mgidi$MGIDI$MGIDI <- round(res_mgidi$MGIDI$MGIDI,2)
  dt_mean_wide$Genotype <- rownames(dt_mean_wide)
  dt_mean_wide <- relocate(dt_mean_wide,Genotype)

  res_mgidi$sel_dif[,c("Xo","Xs","SD", "SDperc","goal")] <-
    apply(res_mgidi$sel_dif[,c("Xo","Xs","SD", "SDperc","goal")], 2, round, 2)

  return(list(res_mgidi=res_mgidi,
              data_mean=dt_mean_wide)
  )

}


