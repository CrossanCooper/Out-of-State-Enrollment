use "C:\Users\ryanh\Dropbox\admissions_project\data\share_ins.dta", replace

reghdfe N_dest N_origin, absorb(d_state) vce(robust) noconstant
reghdfe N_dest N_origin, absorb(d_state grad_y) cluster(d_state) noconstant

ivreghdfe N_dest (N_origin = adm_rate), absorb(d_state) robust
ivreghdfe N_dest (N_origin = adm_rate), absorb(d_state) robust cluster(d_state)

ivreghdfe diff_log_d_share (o_share = adm_rate), absorb(d_state) robust
ivreghdfe diff_log_d_share (o_share = adm_rate), absorb(d_state) robust cluster(d_state)
ivreghdfe diff_log_d_share grad_y (o_share = adm_rate), absorb(d_state) robust cluster(d_state)