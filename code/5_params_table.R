# Summarize the parameters in a table

# load pkgs -----
library(r4ss)
library(dplyr)
# Read in the ctl -----
# note that both CAPAM_platoons_template_current and
# CAPAM_platoon_template_initF were used, but the only differences between the
# ctl file are the CV for the platoons, and the use of init F params in the init
# F one (for baseline) and no init F params for one way trip.
dat <- SS_readdat(file.path("CAPAM_platoons_template_current", "platoon_data_template.ss"))
ctl <- SS_readctl(file.path("CAPAM_platoons_template_current", "platoons_control.ss"),
                  use_datlist = TRUE, datlist = dat)

# Pull together a table of the params -----

get_params <- function(table_name, ctl_file = ctl) {
    if(is.null(ctl_file[[table_name]])) {
        return(NULL)
    }
    tmp_tbl <- ctl_file[[table_name]][, c("INIT", "PHASE"), drop = FALSE]
    tmp_tbl$name <- row.names(tmp_tbl)
    row.names(tmp_tbl) <- NULL
    tmp_tbl$estimated_or_fixed <- ifelse(tmp_tbl$PHASE > 0, "estimated", "fixed")
    tmp_tbl$init <- tmp_tbl$INIT
    tmp_tbl <- tmp_tbl[, c("name", "estimated_or_fixed", "init"), drop = FALSE]
    tmp_tbl
}


tbl_names <- c("MG_parms", "SR_parms", "Q_parms", "size_selex_parms",
 "age_selex_parms")

tbl <- lapply(tbl_names, get_params)
tbl <- do.call(rbind, tbl)

# print out as markdown and .csv -----
write.csv(tbl, "parameter_table.csv")

knitr::kable(tbl, format = "markdown")

# output

# |name                      |estimated_or_fixed |        init|
# |:-------------------------|:------------------|-----------:|
# |NatM_p_1_Fem_GP_1         |fixed              |   0.1500000|
# |L_at_Amin_Fem_GP_1        |estimated          |  20.0000000|
# |L_at_Amax_Fem_GP_1        |estimated          | 100.0000000|
# |VonBert_K_Fem_GP_1        |estimated          |   0.1000000|
# |CV_young_Fem_GP_1         |estimated          |   0.1000000|
# |CV_old_Fem_GP_1           |estimated          |   0.2500000|
# |Wtlen_1_Fem_GP_1          |fixed              |   0.0000217|
# |Wtlen_2_Fem_GP_1          |fixed              |   2.8600000|
# |Mat50%_Fem_GP_1           |fixed              |  55.0000000|
# |Mat_slope_Fem_GP_1        |fixed              |  -0.2500000|
# |Eggs/kg_inter_Fem_GP_1    |fixed              |   1.0000000|
# |Eggs/kg_slope_wt_Fem_GP_1 |fixed              |   0.0000000|
# |CohortGrowDev             |fixed              |   1.0000000|
# |FracFemale_GP_1           |fixed              |   0.5000000|
# |SR_LN(R0)                 |estimated          |  10.0000000|
# |SR_BH_steep               |fixed              |   1.0000000|
# |SR_sigmaR                 |fixed              |   0.5000000|
# |SR_regime                 |fixed              |   0.0000000|
# |SR_autocorr               |fixed              |   0.0000000|
# |LnQ_base_fishery(1)       |fixed              |  -6.9798000|
# |SizeSel_P_1_fishery(1)    |estimated          |  40.0000000|
# |SizeSel_P_2_fishery(1)    |estimated          |   1.0000000|

