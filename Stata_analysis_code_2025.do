*load data

use "investor-manager-relationship.dta"

*General overview - Table 1
tab gender if line_type == "storing_questionnaire" 
tab econ if line_type == "storing_questionnaire" 
tab history if line_type == "investor_decision" 

tab history treatment if line_type == "investor_decision" 
tab gender treatment if line_type == "storing_questionnaire", chi2 
tab gender treatment if line_type == "storing_questionnaire"&role=="investor", chi2 
tab econ treatment if line_type == "storing_questionnaire", chi2
tab econ treatment if line_type == "storing_questionnaire" & role=="investor", chi2

*Transfer Decisions, figure A1 - Footnote 12
preserve
collapse (mean) binary_transfer_decision if line_type == "transfer_decision", by(round)
twoway line binary_transfer_decision round, ylabel(0(0.2)1)
restore

*Are they equally distibuted? - Footnote 12
tab choice round if line_type == "transfer_decision", chi2
*Only due to round 3
tab choice round if line_type == "transfer_decision" & round!=3, chi2

*Look for a trend - Footnote 12
preserve
drop if line_type != "transfer_decision"
xtset ppnummer_int round
xtlogit binary_transfer_decision round if role=="manager"& treatment=="Transfer"
restore

**Investor decisions over time
*Plot of switching decision in the different treatments over rounds - Figure 4
preserve 
drop if line_type!="investor_decision"
collapse (mean) investor_decision round (first) treatment, by(treatment_round_type) 
#delimit ;
graph twoway (line investor_decision round if treatment=="Control", sort(round) lwidth(medthick) xlabel(1 (1) 8) ylabel(0 (20) 100) ) 
(line investor_decision round if treatment=="History", sort(round) lwidth(medthick) lpattern(dash)) 
(line investor_decision round if treatment=="Transfer", sort(round) lwidth(medthick) lpattern(longdash)),
legend(label(1 Control) label(2 History) label(3 Transfer) rows(1)) ytitle("Switching Rate, %") xtitle("Round") graphregion(color(white)) plotregion(fcolor(white));
#delimit cr
restore

*Plot of switching decision in all treatments over rounds (not used)
preserve
drop if line_type!="investor_decision"
collapse (mean) investor_decision (first) treatment, by(round) 
#delimit ;
graph twoway (line investor_decision round, sort(round) lwidth(medthick) xlabel(1 (1) 8) ylabel(0 (20) 100) ) ,
legend(label(1 Control) label(2 History) label(3 Transfer) rows(1)) ytitle("Average Switching Rate, %") xtitle("Round") graphregion(color(white)) plotregion(fcolor(white));
#delimit cr
restore

*Are they equally distibuted? - Footnote 13
tab choice round if line_type == "investor_decision", chi2

*Regression of time trend for switching - Footnote 14
preserve
drop if line_type != "investor_decision"
xtset ppnummer_int round
xtlogit binary_inv_decision round
xtlogit binary_inv_decision round if treatment=="Transfer"
restore

*Plot of transfer decision in all treatments over rounds - Figure A.1
preserve 
drop if line_type!="transfer_decision"
collapse (mean) full_transfer_decision round (first) treatment, by(treatment_round_type) 
#delimit ;
graph twoway (
line full_transfer_decision round if treatment=="Transfer", sort(round) lwidth(medthick) xlabel(1 (1) 8) ylabel(0 (20) 100) ),
legend(label(1 Transfer) rows(1)) ytitle("Transfer Rate, %") xtitle("Round") graphregion(color(white)) plotregion(fcolor(white));
#delimit cr
restore

*Main investor decision overview - Figure 5
graph drop _all 
graph bar (mean) full_inv_decision if line_type=="investor_decision", over(experience) title("(a) Experience") blabel(bar, format(%2.1f) position(inside) color(white)) name(by_experience) ytitle("Average Switching Rate, %") graphregion(color(white)) plotregion(fcolor(white))
graph bar (mean) full_inv_decision if line_type=="investor_decision", over(treatment) title("(b) Treatment") blabel(bar, format(%2.1f) position(inside) color(white)) name(by_treatment) ytitle("Average Switching Rate, %") graphregion(color(white)) plotregion(fcolor(white))
graph bar (mean) full_inv_decision if line_type=="investor_decision", over(experience) over(treatment) title("(c) Experience/Treatment") blabel(bar, format(%2.1f) position(inside) color(white)) name(by_re_tr) ytitle("Average Switching Rate, %") graphregion(color(white)) plotregion(fcolor(white))
graph bar (mean) full_inv_decision if line_type=="investor_decision", over(bin_outcome_pre_dec_spread) over(treatment) title("(d) Most Recent Result/Treatment") blabel(bar, format(%2.1f) position(inside) color(white)) name(by_prev_outcome) ytitle("Average Switching Rate, %") graphregion(color(white)) plotregion(fcolor(white))
graph combine by_experience by_treatment by_re_tr by_prev_outcome, scheme(s1mono) ycommon
gr_edit plotregion1.graph3.grpaxis.style.editstyle majorstyle(alternate(yes)) editcopy
gr_edit plotregion1.graph4.grpaxis.style.editstyle majorstyle(alternate(yes)) editcopy 

*Investor decision statistics
*Hypothesis 1, Footnote 15
logit binary_inv_decision experience if line_type == "investor_decision", vce(cluster ppnummer_int)
logit binary_inv_decision experience if line_type == "investor_decision" & round ==1, vce(cluster ppnummer_int)

*Differences between treatments
logit binary_inv_decision treatment_int if line_type == "investor_decision" & treatment != "Transfer", vce(cluster ppnummer_int)
logit binary_inv_decision treatment_int if line_type == "investor_decision" & treatment != "Transfer" & round == 1, vce(cluster ppnummer_int)
logit binary_inv_decision treatment_int if line_type == "investor_decision" & treatment != "History", vce(cluster ppnummer_int)
logit binary_inv_decision treatment_int if line_type == "investor_decision" & treatment != "History" & round == 1, vce(cluster ppnummer_int)

*Difference by experience within treatment
logit binary_inv_decision experience if line_type == "investor_decision" & treatment == "Control", vce(cluster ppnummer_int)
logit binary_inv_decision experience if line_type == "investor_decision" & treatment == "Control" & round == 1, vce(cluster ppnummer_int)
logit binary_inv_decision experience if line_type == "investor_decision" & treatment == "History", vce(cluster ppnummer_int)
logit binary_inv_decision experience if line_type == "investor_decision" & treatment == "History" & round == 1, vce(cluster ppnummer_int)
logit binary_inv_decision experience if line_type == "investor_decision" & treatment == "Transfer", vce(cluster ppnummer_int)
logit binary_inv_decision experience if line_type == "investor_decision" & treatment == "Transfer" & round == 1, vce(cluster ppnummer_int)

*Differences by most recent result (not reported in the paper)
logit binary_inv_decision bin_outcome_pre_dec_spread  if line_type == "investor_decision" & treatment=="Control", vce(cluster ppnummer_int)
logit binary_inv_decision bin_outcome_pre_dec_spread  if line_type == "investor_decision" & treatment=="Control" & round == 1, vce(cluster ppnummer_int)
logit binary_inv_decision bin_outcome_pre_dec_spread  if line_type == "investor_decision" & treatment=="History", vce(cluster ppnummer_int)
logit binary_inv_decision bin_outcome_pre_dec_spread  if line_type == "investor_decision" & treatment=="History" & round == 1, vce(cluster ppnummer_int)
logit binary_inv_decision bin_outcome_pre_dec_spread  if line_type == "investor_decision" & treatment=="Transfer", vce(cluster ppnummer_int)
logit binary_inv_decision bin_outcome_pre_dec_spread  if line_type == "investor_decision" & treatment=="Transfer" & round == 1, vce(cluster ppnummer_int)

*Comparison between projects
*EV-10 vs EV+20
logit binary_inv_decision dilemma_order if (dilemma_order == 1 | dilemma_order == 5) & line_type == "investor_decision", vce(cluster ppnummer_int)
logit binary_inv_decision dilemma_order if (dilemma_order == 1 | dilemma_order == 5) & line_type == "investor_decision" & round == 1, vce(cluster ppnummer_int)
*SD-10 vs SD+20
logit binary_inv_decision dilemma_order if (dilemma_order == 6 | dilemma_order == 8) & line_type == "investor_decision", vce(cluster ppnummer_int)
logit binary_inv_decision dilemma_order if (dilemma_order == 6 | dilemma_order == 8) & line_type == "investor_decision" & round == 1, vce(cluster ppnummer_int)

*Investor Decisions with Different Dilemma Types - Figure 6
graph drop _all 
graph bar(mean) full_inv_decision if line_type=="investor_decision", over(dilemma_type, sort(dilemma_order)) over(dilemma_category, sort(dilemma_order)) nofill blabel(bar, format(%2.1f) position(inside) color(white)) ytitle("Average Switching Rate, %") title("Total") graphregion(color(white)) plotregion(fcolor(white)) name(dilemma_types_all)
graph bar(mean) full_inv_decision if line_type=="investor_decision"&treatment=="Control", over(dilemma_type, sort(dilemma_order)) over(dilemma_category, sort(dilemma_order)) nofill blabel(bar, format(%2.1f) position(inside) color(white)) ytitle("Average Switching Rate, %") title("Control") graphregion(color(white)) plotregion(fcolor(white)) name(dilemma_types_control)
graph bar(mean) full_inv_decision if line_type=="investor_decision"&treatment=="History", over(dilemma_type, sort(dilemma_order)) over(dilemma_category, sort(dilemma_order)) nofill blabel(bar, format(%2.1f) position(inside) color(white)) ytitle("Average Switching Rate, %") title("History") graphregion(color(white)) plotregion(fcolor(white)) name(dilemma_types_history)
graph bar(mean) full_inv_decision if line_type=="investor_decision"&treatment=="Transfer", over(dilemma_type, sort(dilemma_order)) over(dilemma_category, sort(dilemma_order)) nofill blabel(bar, format(%2.1f) position(inside) color(white)) ytitle("Average Switching Rate, %") title("Transfer") graphregion(color(white)) plotregion(fcolor(white)) name(dilemma_types_transfer)
gr combine dilemma_types_all dilemma_types_control dilemma_types_history dilemma_types_transfer, title("Project Switching by Dilemma Type") scheme(s1mono) ycommon
gr_edit plotregion1.graph1.plotregion1.barlabels[1].style.editstyle size(vsmall) editcopy
gr_edit plotregion1.graph1.grpaxis.style.editstyle majorstyle(alternate(yes)) editcopy
gr_edit plotregion1.graph1.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph1.grpaxis.edit_tick 1 4.44444 `"Equal"', custom tickset(major) editstyle(tickstyle(show_labels(no)) )
gr_edit plotregion1.graph2.plotregion1.barlabels[1].style.editstyle size(vsmall) editcopy
gr_edit plotregion1.graph2.grpaxis.style.editstyle majorstyle(alternate(yes)) editcopy
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 1 4.44444 `"Equal"', custom tickset(major) editstyle(tickstyle(show_labels(no)) )
gr_edit plotregion1.graph3.plotregion1.barlabels[1].style.editstyle size(vsmall) editcopy
gr_edit plotregion1.graph3.grpaxis.style.editstyle majorstyle(alternate(yes)) editcopy
gr_edit plotregion1.graph3.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph3.grpaxis.edit_tick 1 4.44444 `"Equal"', custom tickset(major) editstyle(tickstyle(show_labels(no)) )
gr_edit plotregion1.graph4.plotregion1.barlabels[1].style.editstyle size(vsmall) editcopy
gr_edit plotregion1.graph4.grpaxis.style.editstyle majorstyle(alternate(yes)) editcopy
gr_edit plotregion1.graph4.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph4.grpaxis.edit_tick 1 4.44444 `"Equal"', custom tickset(major) editstyle(tickstyle(show_labels(no)) )

*The combination of regressions that is in the paper - Table 2
preserve
drop if line_type != "investor_decision"
xtset ppnummer_int round
eststo clear
eststo: xtlogit binary_inv_decision i.treatment_int##i.experience if line_type=="investor_decision"
eststo: xtlogit binary_inv_decision i.experience bin_outcome_pre_dec_spread EV_dif SD_dif if line_type=="investor_decision"
eststo: xtlogit binary_inv_decision i.treatment_int##i.experience bin_outcome_pre_dec_spread EV_dif SD_dif if line_type=="investor_decision"
eststo: xtlogit binary_inv_decision i.treatment_int##i.experience bin_outcome_pre_dec_spread EV_dif SD_dif round gender_dummy age_expand econ_dummy choice_number if line_type=="investor_decision"
*Command used for hypothesis 2 and footnote 18
margins r.experience, over(r.treatment_int) predict(pu0)
*Final regression
eststo: xtlogit binary_inv_decision b2.treatment_int##i.experience bin_outcome_pre_dec_spread EV_dif SD_dif round gender_dummy age_expand econ_dummy choice_number if line_type=="investor_decision"
restore

*Same regressions, Probit - Table A.2
preserve
drop if line_type != "investor_decision"
xtset ppnummer_int round
eststo clear
eststo: xtprobit binary_inv_decision i.treatment_int##i.experience if line_type=="investor_decision"
eststo: xtprobit binary_inv_decision i.experience bin_outcome_pre_dec_spread EV_dif SD_dif if line_type=="investor_decision"
eststo: xtprobit binary_inv_decision i.treatment_int##i.experience bin_outcome_pre_dec_spread EV_dif SD_dif if line_type=="investor_decision"
eststo: xtprobit binary_inv_decision i.treatment_int##i.experience bin_outcome_pre_dec_spread EV_dif SD_dif round gender_dummy age_expand econ_dummy choice_number if line_type=="investor_decision"
eststo: xtprobit binary_inv_decision b2.treatment_int##i.experience bin_outcome_pre_dec_spread EV_dif SD_dif round gender_dummy age_expand econ_dummy choice_number if line_type=="investor_decision"
restore

***Decision time section starts here***
*Some simple statistics: - Figure 7
sort treatment
by treatment: summarize decision_time if line_type == "investor_decision"
*Not used: transfer decision time
summarize decision_time if line_type == "transfer_decision"

*Treatment comparison - Footnote 20
clttest decision_time if line_type == "investor_decision" & treatment != "Control" & role=="investor", cluster(ppnummer_int ) by(treatment_int)
ttest decision_time if line_type == "investor_decision" & treatment != "Control" & role=="investor" & round==1, by(treatment)
clttest decision_time if line_type == "investor_decision" & treatment != "History" & role=="investor", cluster(ppnummer_int ) by(treatment_int)
ttest decision_time if line_type == "investor_decision" & treatment != "History" & role=="investor" & round==1, by(treatment)
clttest decision_time if line_type == "investor_decision" & treatment != "Transfer" & role=="investor", cluster(ppnummer_int ) by(treatment_int)
ttest decision_time if line_type == "investor_decision" & treatment != "Transfer" & role=="investor" & round==1, by(treatment)

*Density estimates of decision times - Figure A.2
#delimit ;
graph twoway (kdensity decision_time if treatment=="Control" & line_type == "investor_decision" & decision_time<=50, bwidth(1) lwidth(medthick))
(kdensity decision_time if treatment=="History" & line_type == "investor_decision" & decision_time<=50, bwidth(1) lwidth(medthick) lpattern(dash)) 
(kdensity decision_time if treatment=="Transfer" & line_type == "investor_decision" & decision_time<=50, bwidth(1) lwidth(medthick) lpattern(longdash)), 
 legend(label(1 Control) label(2 History) label(3 Transfer) rows(1)) ytitle("Density") xtitle("Investor decision time in seconds") graphregion(color(white)) plotregion(fcolor(white)) ;
#delimit cr

*Plots of the different treatments over rounds - Figure A.3
preserve
drop if line_type!="investor_decision"
collapse (mean) decision_time round (first) treatment, by(treatment_round_type) 
#delimit ;
graph twoway (line decision_time round if treatment=="Control", sort(round) lwidth(medthick) xlabel(1 (1) 8) ylabel(0 (5) 35) ) 
(line decision_time round if treatment=="History", sort(round) lwidth(medthick) lpattern(dash)) 
(line decision_time round if treatment=="Transfer", sort(round) lwidth(medthick) lpattern(longdash)),
legend(label(1 Control) label(2 History) label(3 Transfer) rows(1)) ytitle("Decision Times by Round") xtitle("Round") graphregion(color(white)) plotregion(fcolor(white));
#delimit cr
restore

*The following graph separates decision times by the investor decision and experience in the different treatments - Figure A.4
graph drop _all 
graph bar (mean) decision_time if line_type=="investor_decision" & treatment=="Control" &experience==1, over(binary_inv_decision) blabel(bar, format(%3.1f) position(inside) color(white)) ytitle("Time in seconds") graphregion(color(white)) plotregion(fcolor(white)) title("Control, Positive Experience") name(dec_time_rel_cont)
graph bar (mean) decision_time if line_type=="investor_decision" & treatment=="Control" &experience==0, over(binary_inv_decision) blabel(bar, format(%3.1f) position(inside) color(white)) ytitle("Time in seconds") graphregion(color(white)) plotregion(fcolor(white)) title("Control, Negative Experience") name(dec_time_nrel_cont)
graph bar (mean) decision_time if line_type=="investor_decision" & treatment=="History" &experience==1, over(binary_inv_decision) blabel(bar, format(%3.1f) position(inside) color(white)) ytitle("Time in seconds") graphregion(color(white)) plotregion(fcolor(white)) title("History, Positive Experience") name(dec_time_rel_hist)
graph bar (mean) decision_time if line_type=="investor_decision" & treatment=="History" &experience==0, over(binary_inv_decision) blabel(bar, format(%3.1f) position(inside) color(white)) ytitle("Time in seconds") graphregion(color(white)) plotregion(fcolor(white)) title("History, Negative Experience") name(dec_time_nrel_hist)
graph bar (mean) decision_time if line_type=="investor_decision" & treatment=="Transfer" &experience==1, over(binary_inv_decision) blabel(bar, format(%3.1f) position(inside) color(white)) ytitle("Time in seconds") graphregion(color(white)) plotregion(fcolor(white))  title("Transfer, Positive Experience")name(dec_time_rel_tran)
graph bar (mean) decision_time if line_type=="investor_decision" & treatment=="Transfer" &experience==0, over(binary_inv_decision) blabel(bar, format(%3.1f) position(inside) color(white)) ytitle("Time in seconds") graphregion(color(white)) plotregion(fcolor(white))  title("Transfer, Negative Experience")name(dec_time_nrel_tran)
gr combine dec_time_rel_cont dec_time_nrel_cont dec_time_rel_hist dec_time_nrel_hist dec_time_rel_tran  dec_time_nrel_tran,  cols(2) ysize(7) scheme(s1mono) ycommon xcommon
gr_edit plotregion1.graph1.scaleaxis.title.xoffset = -1
gr_edit plotregion1.graph2.scaleaxis.title.xoffset = -1
gr_edit plotregion1.graph3.scaleaxis.title.xoffset = -1
gr_edit plotregion1.graph4.scaleaxis.title.xoffset = -1
gr_edit plotregion1.graph5.scaleaxis.title.xoffset = -1
gr_edit plotregion1.graph6.scaleaxis.title.xoffset = -1

*Decision time regressions - Table A.3
preserve
drop if line_type != "investor_decision"
xtset ppnummer_int round
eststo clear
eststo: xtreg decision_time i.treatment_int##i.experience if line_type=="investor_decision"
eststo: xtreg decision_time i.experience binary_inv_decision bin_outcome_pre_dec_spread abs_EV_dif abs_SD_dif if line_type=="investor_decision"
eststo: xtreg decision_time i.treatment_int##i.experience binary_inv_decision bin_outcome_pre_dec_spread abs_EV_dif abs_SD_dif if line_type=="investor_decision"
eststo: xtreg decision_time i.treatment_int##i.experience binary_inv_decision bin_outcome_pre_dec_spread abs_EV_dif abs_SD_dif round gender_dummy age_expand econ_dummy choice_number if line_type=="investor_decision"
esttab using example.tex, label title(Regression Investor Decision) stats(N_clust r2_p) replace
restore

*Experience/investor decision interaction - Footnote 21
*using both social treatments
regress decision_time i.experience##binary_inv_decision bin_outcome_pre_dec_spread EV_dif SD_dif round gender_dummy age_expand econ_dummy choice_number if line_type=="investor_decision" & treatment!="Control", vce(cluster ppnummer_full)
*individual treatments
regress decision_time i.experience##binary_inv_decision bin_outcome_pre_dec_spread EV_dif SD_dif round gender_dummy age_expand econ_dummy choice_number if line_type=="investor_decision" & treatment=="Control", vce(cluster ppnummer_full)
regress decision_time i.experience##binary_inv_decision bin_outcome_pre_dec_spread EV_dif SD_dif round gender_dummy age_expand econ_dummy choice_number if line_type=="investor_decision" & treatment=="History", vce(cluster ppnummer_full)
regress decision_time i.experience##binary_inv_decision bin_outcome_pre_dec_spread EV_dif SD_dif round gender_dummy age_expand econ_dummy choice_number if line_type=="investor_decision" & treatment=="Transfer", vce(cluster ppnummer_full)

****Questionnaire data****
*Graphs separately by treatment - Figure 8
graph drop _all
histogram emotion if treatment=="History" & line_type == "storing_questionnaire", discrete percent addlabels name(emotion_hist) xsc(r(1 5)) xlabel (1 (1) 5) graphregion(color(white)) plotregion(fcolor(white)) color(navy) xtitle("Positive Emotion Intensity") title("History Treatment")
histogram obligation if treatment=="History" & line_type == "storing_questionnaire", discrete percent addlabels name(obligation_hist) xsc(r(1 5)) xlabel (1 (1) 5)  graphregion( color(white) ) plotregion(  fcolor(white) ) color(navy) xtitle("Obligation Intensity")
histogram reaction if treatment=="History" & line_type == "storing_questionnaire", discrete percent addlabels name(reaction_hist) xsc(r(1 5)) xlabel (1 (1) 5)  graphregion( color(white) ) plotregion(  fcolor(white) ) color(navy) xtitle("Likelihood to Stay")
histogram emotion if treatment=="Transfer" & line_type == "storing_questionnaire", discrete percent addlabels name(emotion_tran) xsc(r(1 5)) xlabel (1 (1) 5) graphregion( color(white) ) plotregion(  fcolor(white) ) color(navy) xtitle("Positive Emotion Intensity") title("Transfer Treatment")
histogram obligation if treatment=="Transfer" & line_type == "storing_questionnaire", discrete percent addlabels name(obligation_tran) xsc(r(1 5)) xlabel (1 (1) 5)  graphregion( color(white) ) plotregion(  fcolor(white) ) color(navy) xtitle("Obligation Intensity")
histogram reaction if treatment=="Transfer" & line_type == "storing_questionnaire", discrete percent addlabels name(reaction_tran) xsc(r(1 5)) xlabel (1 (1) 5)  graphregion( color(white) ) plotregion(  fcolor(white) ) color(navy) xtitle("Likelihood to Stay")
graph combine emotion_hist emotion_tran obligation_hist obligation_tran reaction_hist reaction_tran, ycommon xcommon cols(2) ysize(7) scheme(s1mono)
gr_edit plotregion1.graph2.title.yoffset = 1
gr_edit plotregion1.graph1.title.yoffset = 1

*Questionnaire correlations - Table A.4
pwcorr emotion obligation reaction if line_type == "storing_questionnaire"&treatment!="Control", sig star(.1)
pwcorr emotion obligation reaction if line_type == "storing_questionnaire"&treatment=="History", sig star(.1)
pwcorr emotion obligation reaction if line_type == "storing_questionnaire"&treatment=="Transfer", sig star(.1)

*Questionnaire regressions - Table A.5
preserve
drop if line_type != "investor_decision"
xtset ppnummer_int round
eststo clear
eststo: xtlogit binary_inv_decision i.treatment_int c.emotion##i.experience bin_outcome_pre_dec_spread EV_dif SD_dif if line_type=="investor_decision"
eststo: xtlogit binary_inv_decision i.treatment_int c.obligation##i.experience bin_outcome_pre_dec_spread EV_dif SD_dif if line_type=="investor_decision"
eststo: xtlogit binary_inv_decision i.treatment_int c.reaction##i.experience  bin_outcome_pre_dec_spread EV_dif SD_dif if line_type=="investor_decision"
esttab using example.tex, label title(Regression Investor Decision) stats(N_clust r2_p) replace
restore
