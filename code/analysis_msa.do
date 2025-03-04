cd "C:\Users\ryanh\Dropbox\admissions_project\data"

use "C:\Users\ryanh\Dropbox\admissions_project\data\join_bal_msa.dta", replace

* Declare choice-set structure
cmset user_id code

* Estimate conditional logit models
* Can cluster SEs by origin state if we wish, using vce(cluster o_state)

* No trends
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins o_share_other_oos o_share_other_ins, basealternative("alt004") vce(cluster o_state)
matrix vcov1 = e(V)
esttab mat(vcov1) using vcov_mod4_msa.csv, replace mlab(none)

* Linear trends
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins o_share_other_oos o_share_other_ins t_AL_oos t_AL_ins, basealternative("alt004") vce(cluster o_state)
matrix vcov2 = e(V)
esttab mat(vcov2) using vcov_mod6_msa.csv, replace mlab(none)

* Non-parametric trends
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins o_share_other_oos o_share_other_ins i.t_AL_oos i.t_AL_ins, basealternative("alt004") vce(cluster o_state)
matrix vcov3 = e(V)
esttab mat(vcov3) using vcov_mod8_msa.csv, replace mlab(none)

* Try adding unemployment and net migration controls, omitting 2020
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins o_share_other_oos o_share_other_ins ur net_rate i.t_AL_oos i.t_AL_ins if grad_y != 2020, basealternative("alt004") vce(cluster o_state)
matrix vcov4 = e(V)
esttab mat(vcov4) using vcov_mod9_msa.csv, replace mlab(none)

* Get average marginal effects
margins, at(o_share_ins=generate(o_share_ins)) at(o_share_ins=generate(o_share_ins+1))
alternative(new_york_city_metropolitan_area) contrast(atcontrast(r) nowald effects)

* Need to make sure these probabilities are conditioning on in-state students and not
* giving mechanical zero effects on the OOS students (where o_share_ins is zero)