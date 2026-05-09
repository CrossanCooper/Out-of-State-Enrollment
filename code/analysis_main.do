cd "C:\Users\ryanh\Dropbox\admissions_project\data"

use "C:\Users\ryanh\Dropbox\admissions_project\data\join_bal.dta", replace

* Declare choice-set structure
cmset user_id code

* Estimate conditional logit models
* Can cluster SEs by origin state if we wish, using vce(cluster o_state)

* No heterogeneity
cmclogit choice home_state o_share, vce(cluster o_state)
matrix vcov0 = e(V)
esttab mat(vcov0) using vcov_mod0.csv, replace mlab(none)

* No heterogeneity, linear trend
cmclogit choice home_state o_share t_AL, vce(cluster o_state)
matrix vcov1 = e(V)
esttab mat(vcov1) using vcov_mod1.csv, replace mlab(none)

* No trends
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins, vce(cluster o_state)
matrix vcov2 = e(V)
esttab mat(vcov2) using vcov_mod2.csv, replace mlab(none)

* Linear trends
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins t_AL_oos t_AL_ins, vce(cluster o_state)
matrix vcov3 = e(V)
esttab mat(vcov3) using vcov_mod3.csv, replace mlab(none)

* Non-parametric trends
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins i.t_AL_oos i.t_AL_ins, vce(cluster o_state)
matrix vcov4 = e(V)
esttab mat(vcov4) using vcov_mod4.csv, replace mlab(none)

* Non-parametric trends
* With controls, and excluding the Covid year
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins i.t_AL_oos i.t_AL_ins ur net_rate if grad_y != 2020, vce(cluster o_state)
matrix vcov5 = e(V)
esttab mat(vcov5) using vcov_mod5.csv, replace mlab(none)

* Heterogeneity

* By major
cmclogit choice home_state_oos home_state_ins o_share_oos_bus o_share_oos_mkt o_share_oos_fin o_share_oos_eng o_share_oos_acc o_share_oos_ed o_share_oos_nurse o_share_oos_econ o_share_oos_stem o_share_oos_other o_share_ins_bus o_share_ins_mkt o_share_ins_fin o_share_ins_eng o_share_ins_acc o_share_ins_ed o_share_ins_nurse o_share_ins_econ o_share_ins_stem o_share_ins_other i.t_AL_oos i.t_AL_ins, vce(cluster o_state)
matrix vcov6 = e(V)
esttab mat(vcov6) using vcov_mod4_major.csv, replace mlab(none)
