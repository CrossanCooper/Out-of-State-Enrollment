use "C:\Users\ryanh\Dropbox\admissions_project\data\join_bal_msa.dta"

* Declare choice-set structure
cmset user_id alternative

* Estimate conditional logit models
* Can cluster SEs by origin state if we wish, using vce(cluster o_state)

* No trends
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins o_share_other_oos o_share_other_ins, vce(robust)
* Linear trends
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins o_share_other_oos o_share_other_ins t_AL_oos t_AL_ins, vce(robust)
* Non-parametric trends
cmclogit choice home_state_oos home_state_ins o_share_oos o_share_ins o_share_other_oos o_share_other_ins i.t_AL_oos i.t_AL_ins, vce(robust)

* Get average marginal effects
margins, at(o_share_ins=generate(o_share_ins)) at(o_share_ins=generate(o_share_ins+1))
alternative(new_york_city_metropolitan_area) contrast(atcontrast(r) nowald effects)

* Need to make sure these probabilities are conditioning on in-state students and not
* giving mechanical zero effects on the OOS students (where o_share_ins is zero)