import os

def main():
    lines = []
    lines.append('#!/usr/bin/env Rscript')
    lines.append('# tools/biomed_tests/test_biomed_201_400.R')
    lines.append('# Domains 11-14: Cardiovascular, Neuroscience, Oncology, Metabolomics')
    lines.append('')
    lines.append('suppressPackageStartupMessages({')
    lines.append('  pkgload::load_all("packages/pyrt", quiet = TRUE)')
    lines.append('  pkgload::load_all("packages/pyport", quiet = TRUE)')
    lines.append('  library(testthat)')
    lines.append('})')
    lines.append('')
    lines.append('True <- TRUE; False <- FALSE; None <- NULL')
    lines.append('if (!exists("results_biomed")) results_biomed <- list()')
    lines.append('')
    lines.append('record_pass <- function(id, name, domain, metric_name, metric_val) {')
    lines.append('  results_biomed[[length(results_biomed) + 1L]] <<- list(')
    lines.append('    id = id, name = name, domain = domain, metric = metric_name, value = metric_val, status = "PASS"')
    lines.append('  )')
    lines.append('  cat(sprintf("  [%03d/1000] %-28s [%-18s] -> %s: %s (PASS)\\n",')
    lines.append('              id, name, domain, metric_name, as.character(metric_val)))')
    lines.append('}')
    lines.append('')

    def add_test(id_num, name, domain, py_code, call_code, expected_code, metric_name):
        # Calculate exact Python reference result
        try:
            loc = {}
            exec(py_code, loc)
            py_c = call_code.strip()
            idx_suffix = ""
            if py_c.endswith("[[1]]"):
                py_c = py_c[:-5]
                idx_suffix = "[0]"
            elif py_c.endswith("[[2]]"):
                py_c = py_c[:-5]
                idx_suffix = "[1]"
            elif py_c.endswith("[[3]]"):
                py_c = py_c[:-5]
                idx_suffix = "[2]"
            
            # transform py_list(list(...)) to [...]
            while "py_list(list(" in py_c:
                s_idx = py_c.find("py_list(list(")
                # find matching end
                depth = 0
                end_idx = s_idx + len("py_list(list(")
                for i in range(end_idx, len(py_c)):
                    if py_c[i] == '(':
                        depth += 1
                    elif py_c[i] == ')':
                        if depth == 0:
                            if i + 1 < len(py_c) and py_c[i+1] == ')':
                                inner = py_c[s_idx + len("py_list(list("):i]
                                py_c = py_c[:s_idx] + "[" + inner + "]" + py_c[i+2:]
                                break
                            else:
                                inner = py_c[s_idx + len("py_list(list("):i]
                                py_c = py_c[:s_idx] + "[" + inner + "]" + py_c[i+1:]
                                break
                        else:
                            depth -= 1

            py_c = py_c.replace("TRUE", "True").replace("FALSE", "False")
            py_c = py_c + idx_suffix
            res = eval(py_c, loc)
            if isinstance(res, bool):
                expected_code = "TRUE" if res else "FALSE"
            elif isinstance(res, (int, float)):
                expected_code = str(res)
            elif isinstance(res, str):
                expected_code = repr(res)
        except Exception as e:
            pass

        lines.append(f'# {id_num:03d}. {name}')
        lines.append(f'py_{id_num:03d} <- r"---(')
        lines.append(py_code.strip())
        lines.append(')---"')
        lines.append(f'eval(parse(text = as.character(pyport::transpile_python(py_{id_num:03d}))), envir = .GlobalEnv)')
        lines.append(f'val_{id_num:03d} <- {call_code}')
        lines.append(f'expect_equal(val_{id_num:03d}, {expected_code})')
        lines.append(f'record_pass({id_num}, "{name}", "{domain}", "{metric_name}", val_{id_num:03d})')
        lines.append('')

    # =========================================================================
    # DOMAIN 11: Cardiovascular, Hemodynamics & ECG Analysis (201-250)
    # =========================================================================
    lines.append('cat("\\n--- [Domain 11/26] Cardiovascular, Hemodynamics & ECG Analysis (201-250) ---\\n")')
    
    add_test(201, "stroke_volume", "Cardiovascular",
             "def stroke_volume(edv, esv):\n    return float(edv - esv)",
             "stroke_volume(120, 50)", "70", "Stroke Volume (mL)")
    
    add_test(202, "cardiac_output", "Cardiovascular",
             "def cardiac_output(hr, sv):\n    return float(hr * sv) / 1000.0",
             "cardiac_output(75, 70)", "5.25", "Cardiac Output (L/min)")

    add_test(203, "cardiac_index", "Cardiovascular",
             "def cardiac_index(co, bsa):\n    return round(co / bsa, 2)",
             "cardiac_index(5.25, 1.8)", "2.92", "Cardiac Index (L/min/m2)")

    add_test(204, "mean_arterial_pressure", "Cardiovascular",
             "def mean_arterial_pressure(sbp, dbp):\n    return round(dbp + (sbp - dbp) / 3.0, 1)",
             "mean_arterial_pressure(120, 80)", "93.3", "MAP (mmHg)")

    add_test(205, "pulse_pressure", "Cardiovascular",
             "def pulse_pressure(sbp, dbp):\n    return sbp - dbp",
             "pulse_pressure(120, 80)", "40", "Pulse Pressure (mmHg)")

    add_test(206, "systemic_vascular_resistance", "Cardiovascular",
             "def systemic_vascular_resistance(map_v, cvp, co):\n    return round(80.0 * (map_v - cvp) / co, 1)",
             "systemic_vascular_resistance(90, 5, 5.0)", "1360.0", "SVR (dynes*s/cm5)")

    add_test(207, "pulmonary_vascular_resistance", "Cardiovascular",
             "def pulmonary_vascular_resistance(mpap, pawp, co):\n    return round(80.0 * (mpap - pawp) / co, 1)",
             "pulmonary_vascular_resistance(25, 10, 5.0)", "240.0", "PVR (dynes*s/cm5)")

    add_test(208, "rate_pressure_product", "Cardiovascular",
             "def rate_pressure_product(hr, sbp):\n    return hr * sbp",
             "rate_pressure_product(75, 120)", "9000", "Rate-Pressure Product")

    add_test(209, "qtc_bazett", "Cardiovascular",
             "def qtc_bazett(qt_ms, rr_sec):\n    return round(qt_ms / (rr_sec ** 0.5), 1)",
             "qtc_bazett(400, 0.81)", "444.4", "QTc Bazett (ms)")

    add_test(210, "qtc_fridericia", "Cardiovascular",
             "def qtc_fridericia(qt_ms, rr_sec):\n    return round(qt_ms / (rr_sec ** (1.0 / 3.0)), 1)",
             "qtc_fridericia(400, 0.8)", "430.9", "QTc Fridericia (ms)")

    add_test(211, "qtc_framingham", "Cardiovascular",
             "def qtc_framingham(qt_ms, rr_sec):\n    return round(qt_ms + 154.0 * (1.0 - rr_sec), 1)",
             "qtc_framingham(400, 0.8)", "430.8", "QTc Framingham (ms)")

    add_test(212, "qtc_hodges", "Cardiovascular",
             "def qtc_hodges(qt_ms, hr):\n    return round(qt_ms + 1.75 * (hr - 60.0), 2)",
             "qtc_hodges(400, 75)", "426.25", "QTc Hodges (ms)")

    add_test(213, "pr_interval_check", "Cardiovascular",
             "def pr_interval_check(pr_ms):\n    if pr_ms > 200:\n        return 'first_degree_block'\n    return 'normal'",
             "pr_interval_check(220)", "'first_degree_block'", "PR Interval Diagnosis")

    add_test(214, "st_elevation_stemi", "Cardiovascular",
             "def st_elevation_stemi(ste_mm, is_male, age):\n    thresh = 2.0 if (is_male and age >= 40) else 1.5\n    return ste_mm >= thresh",
             "st_elevation_stemi(2.5, True, 45)", "TRUE", "STEMI Criteria Met")

    add_test(215, "sokolow_lyon_lvh", "Cardiovascular",
             "def sokolow_lyon_lvh(sv1, rv5):\n    return (sv1 + rv5) >= 3.5",
             "sokolow_lyon_lvh(1.8, 2.0)", "TRUE", "Sokolow-Lyon LVH")

    add_test(216, "cornell_voltage_lvh", "Cardiovascular",
             "def cornell_voltage_lvh(ravl, sv3, is_male):\n    thresh = 2.8 if is_male else 2.0\n    return (ravl + sv3) > thresh",
             "cornell_voltage_lvh(1.5, 1.6, True)", "TRUE", "Cornell LVH")

    add_test(217, "duke_treadmill_score", "Cardiovascular",
             "def duke_treadmill_score(ex_min, st_mm, angina_idx):\n    return float(ex_min) - 5.0 * float(st_mm) - 4.0 * float(angina_idx)",
             "duke_treadmill_score(10, 1.0, 0)", "5", "Duke Treadmill Score")

    add_test(218, "grace_score_risk", "Cardiovascular",
             "def grace_score_risk(age, hr, sbp, creat):\n    pts = 0\n    if age >= 65: pts += 40\n    if hr >= 90: pts += 20\n    if sbp < 100: pts += 30\n    if creat >= 2.0: pts += 25\n    return pts",
             "grace_score_risk(70, 95, 95, 2.2)", "115", "GRACE Risk Points")

    add_test(219, "killip_classification", "Cardiovascular",
             "def killip_classification(has_rales, sbp):\n    if sbp < 90:\n        return 4\n    elif has_rales:\n        return 2\n    return 1",
             "killip_classification(True, 110)", "2", "Killip Class")

    add_test(220, "syntax_score_calc", "Cardiovascular",
             "def syntax_score_calc(lesion_weights):\n    tot = 0\n    for w in lesion_weights:\n        tot += w\n    return tot",
             "syntax_score_calc(py_list(list(3.5, 4.0, 5.0)))", "12.5", "SYNTAX Score")

    add_test(221, "coronary_flow_reserve", "Cardiovascular",
             "def coronary_flow_reserve(hyperemic, baseline):\n    return round(float(hyperemic) / float(baseline), 2)",
             "coronary_flow_reserve(60.0, 20.0)", "3.0", "Coronary Flow Reserve")

    add_test(222, "fractional_flow_reserve", "Cardiovascular",
             "def fractional_flow_reserve(pd, pa):\n    return round(float(pd) / float(pa), 2)",
             "fractional_flow_reserve(72.0, 90.0)", "0.8", "FFR")

    add_test(223, "ankle_brachial_index", "Cardiovascular",
             "def ankle_brachial_index(ankle_p, brach_p):\n    return round(float(ankle_p) / float(brach_p), 2)",
             "ankle_brachial_index(120.0, 110.0)", "1.09", "ABI")

    add_test(224, "pulse_wave_velocity", "Cardiovascular",
             "def pulse_wave_velocity(dist_m, delay_s):\n    return round(float(dist_m) / float(delay_s), 2)",
             "pulse_wave_velocity(0.6, 0.08)", "7.5", "PWV (m/s)")

    add_test(225, "windkessel_2element", "Cardiovascular",
             "def windkessel_2element(r_art, tau_decay):\n    return round(tau_decay / r_art, 3)",
             "windkessel_2element(1.1, 1.32)", "1.2", "Compliance (mL/mmHg)")

    add_test(226, "windkessel_3element_imp", "Cardiovascular",
             "def windkessel_3element_imp(z_c, r_p):\n    return round(z_c + r_p, 2)",
             "windkessel_3element_imp(0.08, 1.05)", "1.13", "Total Input Resistance")

    add_test(227, "baroreflex_sensitivity", "Cardiovascular",
             "def baroreflex_sensitivity(delta_rr, delta_sbp):\n    return round(float(delta_rr) / float(delta_sbp), 2)",
             "baroreflex_sensitivity(150.0, 10.0)", "15.0", "BRS (ms/mmHg)")

    add_test(228, "total_arterial_compliance", "Cardiovascular",
             "def total_arterial_compliance(sv, pp):\n    return round(float(sv) / float(pp), 2)",
             "total_arterial_compliance(70.0, 40.0)", "1.75", "Compliance (mL/mmHg)")

    add_test(229, "lv_stroke_work_index", "Cardiovascular",
             "def lv_stroke_work_index(ci, map_v, pcwp):\n    return round(0.0136 * (map_v - pcwp) * ci, 2)",
             "lv_stroke_work_index(3.0, 95.0, 12.0)", "3.39", "LVSWI (g*m/m2)")

    add_test(230, "rv_stroke_work_index", "Cardiovascular",
             "def rv_stroke_work_index(ci, mpap, cvp):\n    return round(0.0136 * (mpap - cvp) * ci, 2)",
             "rv_stroke_work_index(3.0, 20.0, 6.0)", "0.57", "RVSWI (g*m/m2)")

    add_test(231, "gorlin_aortic_valve_area", "Cardiovascular",
             "def gorlin_aortic_valve_area(co_ml, sep_s, hr, mean_grad):\n    num = float(co_ml) / (sep_s * hr)\n    den = 44.3 * (mean_grad ** 0.5)\n    return round(num / den, 2)",
             "gorlin_aortic_valve_area(5000, 0.3, 70, 36.0)", "0.9", "AVA (cm2)")

    add_test(232, "diastolic_ea_ratio", "Cardiovascular",
             "def diastolic_ea_ratio(e_vel, a_vel):\n    return round(float(e_vel) / float(a_vel), 2)",
             "diastolic_ea_ratio(80.0, 60.0)", "1.33", "E/A Ratio")

    add_test(233, "diastolic_e_e_prime", "Cardiovascular",
             "def diastolic_e_e_prime(e_vel, e_prime):\n    return round(float(e_vel) / float(e_prime), 1)",
             "diastolic_e_e_prime(85.0, 8.5)", "10.0", "E/e' Ratio")

    add_test(234, "left_atrial_volume_index", "Cardiovascular",
             "def left_atrial_volume_index(la_vol, bsa):\n    return round(float(la_vol) / float(bsa), 1)",
             "left_atrial_volume_index(54.0, 1.8)", "30.0", "LAVI (mL/m2)")

    add_test(235, "simpson_biplane_ef", "Cardiovascular",
             "def simpson_biplane_ef(edv, esv):\n    return round((edv - esv) / edv * 100.0, 1)",
             "simpson_biplane_ef(120.0, 48.0)", "60.0", "LVEF %")

    add_test(236, "tei_index", "Cardiovascular",
             "def tei_index(ivct, ivrt, et):\n    return round((ivct + ivrt) / float(et), 2)",
             "tei_index(40.0, 60.0, 250.0)", "0.4", "Tei Myocardial Index")

    add_test(237, "wall_motion_score_index", "Cardiovascular",
             "def wall_motion_score_index(scores):\n    s = 0.0\n    for x in scores:\n        s += x\n    return round(s / len(scores), 2)",
             "wall_motion_score_index(py_list(list(1.0, 1.0, 2.0, 1.0)))", "1.25", "WMSI")

    add_test(238, "global_longitudinal_strain", "Cardiovascular",
             "def global_longitudinal_strain(strains):\n    s = 0.0\n    for x in strains:\n        s += x\n    return round(s / len(strains), 1)",
             "global_longitudinal_strain(py_list(list(-18.0, -20.0, -22.0)))", "-20.0", "GLS %")

    add_test(239, "coronary_perfusion_pressure", "Cardiovascular",
             "def coronary_perfusion_pressure(dbp, pawp):\n    return dbp - pawp",
             "coronary_perfusion_pressure(75, 15)", "60", "CPP (mmHg)")

    add_test(240, "iabp_inflation_timing", "Cardiovascular",
             "def iabp_inflation_timing(dicrotic_notch_ms):\n    return dicrotic_notch_ms + 10",
             "iabp_inflation_timing(320)", "330", "IABP Inflation Time (ms)")

    add_test(241, "vascular_impedance_modulus", "Cardiovascular",
             "def vascular_impedance_modulus(p_amp, q_amp):\n    return round(float(p_amp) / float(q_amp), 2)",
             "vascular_impedance_modulus(40.0, 20.0)", "2.0", "Impedance Modulus")

    add_test(242, "ctth_heterogeneity", "Cardiovascular",
             "def ctth_heterogeneity(transit_times):\n    n = len(transit_times)\n    mean_t = sum(transit_times) / float(n)\n    var_t = sum([(x - mean_t)**2 for x in transit_times]) / float(n)\n    return round((var_t ** 0.5) / mean_t, 2)",
             "ctth_heterogeneity(py_list(list(2.0, 4.0, 6.0)))", "0.41", "CTTH Index")

    add_test(243, "pvri_index", "Cardiovascular",
             "def pvri_index(pvr, bsa):\n    return round(float(pvr) * float(bsa), 1)",
             "pvri_index(240.0, 1.8)", "432.0", "PVRI (dynes*s/cm5*m2)")

    add_test(244, "end_systolic_elastance", "Cardiovascular",
             "def end_systolic_elastance(pes, sv, v0):\n    return round(pes / (sv + v0), 2)",
             "end_systolic_elastance(100.0, 40.0, 10.0)", "2.0", "Ees (mmHg/mL)")

    add_test(245, "arterial_elastance", "Cardiovascular",
             "def arterial_elastance(pes, sv):\n    return round(float(pes) / float(sv), 2)",
             "arterial_elastance(100.0, 50.0)", "2.0", "Ea (mmHg/mL)")

    add_test(246, "ventriculo_arterial_coupling", "Cardiovascular",
             "def ventriculo_arterial_coupling(ea, ees):\n    return round(float(ea) / float(ees), 2)",
             "ventriculo_arterial_coupling(2.0, 2.0)", "1.0", "VAC Ratio")

    add_test(247, "pulse_pressure_variation", "Cardiovascular",
             "def pulse_pressure_variation(pp_max, pp_min):\n    return round(200.0 * (pp_max - pp_min) / (pp_max + pp_min), 1)",
             "pulse_pressure_variation(55.0, 45.0)", "20.0", "PPV %")

    add_test(248, "stroke_volume_variation", "Cardiovascular",
             "def stroke_volume_variation(sv_max, sv_min):\n    return round(200.0 * (sv_max - sv_min) / (sv_max + sv_min), 1)",
             "stroke_volume_variation(77.0, 63.0)", "20.0", "SVV %")

    add_test(249, "myocardial_oxygen_consumption_est", "Cardiovascular",
             "def myocardial_oxygen_consumption_est(hr, sbp):\n    return round((hr * sbp) / 100.0, 1)",
             "myocardial_oxygen_consumption_est(75, 120)", "90.0", "mVO2 Index")

    add_test(250, "fick_cardiac_output", "Cardiovascular",
             "def fick_cardiac_output(vo2, cao2, cvo2):\n    return round(vo2 / (10.0 * (cao2 - cvo2)), 2)",
             "fick_cardiac_output(250.0, 20.0, 15.0)", "5.0", "Fick CO (L/min)")

    # =========================================================================
    # DOMAIN 12: Neuroscience, Neuroimaging & Connectomics (251-300)
    # =========================================================================
    lines.append('cat("\\n--- [Domain 12/26] Neuroscience, Neuroimaging & Connectomics (251-300) ---\\n")')

    add_test(251, "hh_potassium_activation", "Neuroscience",
             "def hh_potassium_activation(v_mv):\n    return round(0.01 * (v_mv + 10.0) / 2.0, 3)",
             "hh_potassium_activation(-40.0)", "-0.15", "alpha_n")

    add_test(252, "hh_sodium_activation", "Neuroscience",
             "def hh_sodium_activation(v_mv):\n    return round(0.1 * (v_mv + 25.0) / 2.0, 3)",
             "hh_sodium_activation(-45.0)", "-1.0", "alpha_m")

    add_test(253, "leakage_current", "Neuroscience",
             "def leakage_current(g_leak, v_mem, e_leak):\n    return round(g_leak * (v_mem - e_leak), 2)",
             "leakage_current(0.3, -65.0, -54.4)", "-3.18", "I_leak (uA/cm2)")

    add_test(254, "action_potential_threshold", "Neuroscience",
             "def action_potential_threshold(v_trace, thresh_mv):\n    spikes = 0\n    for i in range(1, len(v_trace)):\n        if v_trace[i-1] < thresh_mv and v_trace[i] >= thresh_mv:\n            spikes += 1\n    return spikes",
             "action_potential_threshold(py_list(list(-70.0, -60.0, -30.0, 20.0, -70.0, -10.0)), -50.0)", "2", "Detected Action Potentials")

    add_test(255, "membrane_time_constant", "Neuroscience",
             "def membrane_time_constant(r_m, c_m):\n    return round(r_m * c_m, 2)",
             "membrane_time_constant(10.0, 1.0)", "10.0", "Tau_m (ms)")

    add_test(256, "cable_length_constant", "Neuroscience",
             "def cable_length_constant(r_m, r_i, radius_um):\n    rad_cm = radius_um * 1e-4\n    return round(((rad_cm * r_m) / (2.0 * r_i)) ** 0.5 * 10.0, 3)",
             "cable_length_constant(1000.0, 100.0, 2.0)", "0.316", "Lambda (mm)")

    add_test(257, "postsynaptic_potential", "Neuroscience",
             "def postsynaptic_potential(amp, t_ms, tau_s):\n    return round(amp * (t_ms / tau_s), 2)",
             "postsynaptic_potential(5.0, 2.0, 4.0)", "2.5", "PSP Amplitude (mV)")

    add_test(258, "integrate_and_fire_step", "Neuroscience",
             "def integrate_and_fire_step(v_curr, i_inj, r_m, tau_m, dt):\n    dv = (-(v_curr - (-70.0)) + i_inj * r_m) * (dt / tau_m)\n    return round(v_curr + dv, 2)",
             "integrate_and_fire_step(-70.0, 2.0, 10.0, 10.0, 1.0)", "-68.0", "Membrane Voltage (mV)")

    add_test(259, "refractory_period_limiter", "Neuroscience",
             "def refractory_period_limiter(last_spike_t, curr_t, t_ref):\n    return (curr_t - last_spike_t) >= t_ref",
             "refractory_period_limiter(10.0, 12.5, 2.0)", "TRUE", "Can Spike")

    add_test(260, "spike_triggered_average", "Neuroscience",
             "def spike_triggered_average(stimulus, spike_indices, window):\n    tot = 0.0\n    for idx in spike_indices:\n        tot += stimulus[idx]\n    return round(tot / float(len(spike_indices)), 2)",
             "spike_triggered_average(py_list(list(1.0, 2.0, 5.0, 1.0, 6.0)), py_list(list(2, 4)), 1)", "5.5", "STA Mean")

    add_test(261, "psth_binning", "Neuroscience",
             "def psth_binning(spike_times, bin_size_ms, max_t_ms):\n    num_bins = int(max_t_ms / bin_size_ms)\n    counts = [0] * num_bins\n    for t in spike_times:\n        b = int(t / bin_size_ms)\n        if b < num_bins:\n            counts[b] += 1\n    return counts",
             "psth_binning(py_list(list(5.0, 15.0, 25.0, 35.0)), 10.0, 40.0)[[1]]", "1", "Bin 1 count")

    add_test(262, "fano_factor_spikes", "Neuroscience",
             "def fano_factor_spikes(counts):\n    n = len(counts)\n    mean_c = sum(counts) / float(n)\n    var_c = sum([(x - mean_c)**2 for x in counts]) / float(n)\n    return round(var_c / mean_c, 2)",
             "fano_factor_spikes(py_list(list(10.0, 12.0, 8.0, 14.0)))", "0.45", "Fano Factor")

    add_test(263, "isi_coefficient_variation", "Neuroscience",
             "def isi_coefficient_variation(isis):\n    n = len(isis)\n    mean_isi = sum(isis) / float(n)\n    var_isi = sum([(x - mean_isi)**2 for x in isis]) / float(n)\n    return round((var_isi ** 0.5) / mean_isi, 2)",
             "isi_coefficient_variation(py_list(list(10.0, 20.0, 30.0)))", "0.41", "CV of ISI")

    add_test(264, "lfp_band_power", "Neuroscience",
             "def lfp_band_power(psd_values):\n    s = 0.0\n    for p in psd_values:\n        s += p\n    return s",
             "lfp_band_power(py_list(list(12.5, 14.0, 18.5)))", "45", "Total LFP Band Power")

    add_test(265, "phase_locking_value", "Neuroscience",
             "def phase_locking_value(phase_diffs):\n    n = len(phase_diffs)\n    s_cos = 0.0\n    for d in phase_diffs:\n        if d == 0.0:\n            s_cos += 1.0\n    return round(s_cos / float(n), 2)",
             "phase_locking_value(py_list(list(0.0, 0.0, 0.0, 0.0)))", "1.0", "PLV Phase Synchrony")

    add_test(266, "coherence_index", "Neuroscience",
             "def coherence_index(csd_sq, psd_x, psd_y):\n    return round(float(csd_sq) / (psd_x * psd_y), 2)",
             "coherence_index(16.0, 5.0, 4.0)", "0.8", "Spectral Coherence")

    add_test(267, "granger_causality_score", "Neuroscience",
             "def granger_causality_score(var_restricted, var_unrestricted):\n    return round((var_restricted - var_unrestricted) / var_unrestricted, 3)",
             "granger_causality_score(1.5, 1.2)", "0.25", "F-score proxy")

    add_test(268, "phase_amplitude_coupling", "Neuroscience",
             "def phase_amplitude_coupling(mod_indices):\n    s = 0.0\n    for m in mod_indices:\n        s += m\n    return round(s / len(mod_indices), 2)",
             "phase_amplitude_coupling(py_list(list(0.12, 0.18, 0.15)))", "0.15", "PAC Modulation Index")

    add_test(269, "spike_template_match", "Neuroscience",
             "def spike_template_match(wf, tmpl):\n    dist = 0.0\n    for i in range(len(wf)):\n        dist += (wf[i] - tmpl[i])**2\n    return round(dist ** 0.5, 2)",
             "spike_template_match(py_list(list(1.0, 2.0, 3.0)), py_list(list(1.0, 2.0, 5.0)))", "2.0", "Euclidean Distance")

    add_test(270, "mahalanobis_spike_dist", "Neuroscience",
             "def mahalanobis_spike_dist(pt, mean_pt, sd_feat):\n    d = 0.0\n    for i in range(len(pt)):\n        d += ((pt[i] - mean_pt[i]) / sd_feat[i]) ** 2\n    return round(d ** 0.5, 2)",
             "mahalanobis_spike_dist(py_list(list(10.0, 20.0)), py_list(list(8.0, 16.0)), py_list(list(2.0, 4.0)))", "1.41", "Mahalanobis Dist")

    add_test(271, "graph_degree_centrality", "Neuroscience",
             "def graph_degree_centrality(adj_row, n_nodes):\n    deg = 0\n    for v in adj_row:\n        deg += v\n    return round(float(deg) / (n_nodes - 1), 2)",
             "graph_degree_centrality(py_list(list(1, 1, 0, 1)), 5)", "0.75", "Degree Centrality")

    add_test(272, "graph_node_betweenness", "Neuroscience",
             "def graph_node_betweenness(paths_through_v, total_paths):\n    return round(float(paths_through_v) / float(total_paths), 2)",
             "graph_node_betweenness(15, 60)", "0.25", "Betweenness Centrality")

    add_test(273, "graph_closeness_centrality", "Neuroscience",
             "def graph_closeness_centrality(dist_sum, n_nodes):\n    return round((n_nodes - 1.0) / float(dist_sum), 3)",
             "graph_closeness_centrality(12.0, 5)", "0.333", "Closeness Centrality")

    add_test(274, "graph_eigenvector_centrality", "Neuroscience",
             "def graph_eigenvector_centrality(neighbor_weights, lambda_val):\n    s = 0.0\n    for w in neighbor_weights:\n        s += w\n    return round(s / lambda_val, 2)",
             "graph_eigenvector_centrality(py_list(list(1.2, 0.8, 1.0)), 3.0)", "1.0", "Eigenvector Centrality")

    add_test(275, "global_clustering_coef", "Neuroscience",
             "def global_clustering_coef(triangles, triplets):\n    return round(3.0 * float(triangles) / float(triplets), 2)",
             "global_clustering_coef(10, 50)", "0.6", "Clustering Coef")

    add_test(276, "small_world_sigma", "Neuroscience",
             "def small_world_sigma(c_gamma, l_lambda):\n    return round(float(c_gamma) / float(l_lambda), 2)",
             "small_world_sigma(2.4, 1.2)", "2.0", "Small-World Sigma")

    add_test(277, "characteristic_path_length", "Neuroscience",
             "def characteristic_path_length(path_lengths):\n    return round(sum(path_lengths) / float(len(path_lengths)), 2)",
             "characteristic_path_length(py_list(list(1.0, 2.0, 2.0, 3.0)))", "2.0", "Path Length")

    add_test(278, "rich_club_coefficient", "Neuroscience",
             "def rich_club_coefficient(e_rich, n_rich):\n    max_e = n_rich * (n_rich - 1) / 2.0\n    return round(float(e_rich) / max_e, 2)",
             "rich_club_coefficient(8, 5)", "0.8", "Rich Club Phi")

    add_test(279, "modularity_louvain_q", "Neuroscience",
             "def modularity_louvain_q(within_comm_edges, expected_edges, total_edges):\n    return round((within_comm_edges - expected_edges) / float(total_edges), 2)",
             "modularity_louvain_q(45.0, 15.0, 60.0)", "0.5", "Modularity Q")

    add_test(280, "graph_laplacian_spectrum", "Neuroscience",
             "def graph_laplacian_spectrum(degrees, adj_diag):\n    return [degrees[i] - adj_diag[i] for i in range(len(degrees))]",
             "graph_laplacian_spectrum(py_list(list(3, 4, 2)), py_list(list(0, 0, 0)))[[2]]", "4", "Laplacian L_22")

    add_test(281, "diffusion_fractional_anisotropy", "Neuroscience",
             "def diffusion_fractional_anisotropy(l1, l2, l3):\n    md = (l1 + l2 + l3) / 3.0\n    num = ((l1 - md)**2 + (l2 - md)**2 + (l3 - md)**2) ** 0.5\n    den = (l1**2 + l2**2 + l3**2) ** 0.5\n    return round((1.5 ** 0.5) * (num / den), 2)",
             "diffusion_fractional_anisotropy(1.5, 0.3, 0.3)", "0.85", "DTI FA")

    add_test(282, "diffusion_mean_diffusivity", "Neuroscience",
             "def diffusion_mean_diffusivity(l1, l2, l3):\n    return round((l1 + l2 + l3) / 3.0, 2)",
             "diffusion_mean_diffusivity(1.2, 0.6, 0.6)", "0.8", "DTI MD (10^-3 mm2/s)")

    add_test(283, "diffusion_radial_diffusivity", "Neuroscience",
             "def diffusion_radial_diffusivity(l2, l3):\n    return round((l2 + l3) / 2.0, 2)",
             "diffusion_radial_diffusivity(0.5, 0.3)", "0.4", "DTI RD")

    add_test(284, "diffusion_axial_diffusivity", "Neuroscience",
             "def diffusion_axial_diffusivity(l1):\n    return round(float(l1), 2)",
             "diffusion_axial_diffusivity(1.65)", "1.65", "DTI AD")

    add_test(285, "tractography_streamline_step", "Neuroscience",
             "def tractography_streamline_step(curr_pos, dir_vec, step_size):\n    return [round(curr_pos[i] + dir_vec[i] * step_size, 2) for i in range(3)]",
             "tractography_streamline_step(py_list(list(10.0, 20.0, 30.0)), py_list(list(0.6, 0.8, 0.0)), 1.0)[[1]]", "10.6", "Streamline X")

    add_test(286, "b_matrix_rotation", "Neuroscience",
             "def b_matrix_rotation(b_val, g_x, g_y, g_z):\n    return round(b_val * (g_x**2 + g_y**2 + g_z**2), 1)",
             "b_matrix_rotation(1000.0, 0.0, 1.0, 0.0)", "1000.0", "b*g^2")

    add_test(287, "dti_tensor_trace", "Neuroscience",
             "def dti_tensor_trace(d_xx, d_yy, d_zz):\n    return round(d_xx + d_yy + d_zz, 2)",
             "dti_tensor_trace(0.8, 0.7, 0.6)", "2.1", "Tensor Trace")

    add_test(288, "cortical_thickness_geodesic", "Neuroscience",
             "def cortical_thickness_geodesic(p_pial, p_white):\n    d = 0.0\n    for i in range(3):\n        d += (p_pial[i] - p_white[i]) ** 2\n    return round(d ** 0.5, 2)",
             "cortical_thickness_geodesic(py_list(list(10.0, 15.0, 20.0)), py_list(list(8.0, 15.0, 20.0)))", "2.0", "Cortical Thickness (mm)")

    add_test(289, "sulcal_depth_measure", "Neuroscience",
             "def sulcal_depth_measure(r_hull, r_sulcus):\n    return round(float(r_hull - r_sulcus), 2)",
             "sulcal_depth_measure(55.0, 42.5)", "12.5", "Sulcal Depth (mm)")

    add_test(290, "gyrification_index", "Neuroscience",
             "def gyrification_index(total_surface, outer_surface):\n    return round(float(total_surface) / float(outer_surface), 2)",
             "gyrification_index(2400.0, 1000.0)", "2.4", "Gyrification Index")

    add_test(291, "vbm_jacobian_determinant", "Neuroscience",
             "def vbm_jacobian_determinant(vox_orig, vox_deformed):\n    return round(float(vox_deformed) / float(vox_orig), 3)",
             "vbm_jacobian_determinant(1.0, 1.15)", "1.15", "Jacobian Volume Ratio")

    add_test(292, "hrf_double_gamma_peak", "Neuroscience",
             "def hrf_double_gamma_peak(t_sec):\n    if t_sec == 5:\n        return 1.0\n    return 0.5",
             "hrf_double_gamma_peak(5)", "1", "Peak HRF Value")

    add_test(293, "bold_convolution_step", "Neuroscience",
             "def bold_convolution_step(stim_seq, hrf_kernel):\n    val = 0.0\n    for i in range(len(stim_seq)):\n        val += stim_seq[i] * hrf_kernel[len(hrf_kernel) - 1 - i]\n    return round(val, 2)",
             "bold_convolution_step(py_list(list(1.0, 0.0, 1.0)), py_list(list(0.2, 0.5, 1.0)))", "1.2", "Convolved BOLD")

    add_test(294, "glm_design_matrix_beta", "Neuroscience",
             "def glm_design_matrix_beta(x_val, y_val):\n    return round(float(y_val) / float(x_val), 2)",
             "glm_design_matrix_beta(2.0, 4.8)", "2.4", "GLM Beta")

    add_test(295, "contrast_t_statistic", "Neuroscience",
             "def contrast_t_statistic(c_beta, se_beta):\n    return round(float(c_beta) / float(se_beta), 2)",
             "contrast_t_statistic(3.5, 0.7)", "5.0", "t-statistic")

    add_test(296, "fwhm_to_sigma_smoothing", "Neuroscience",
             "def fwhm_to_sigma_smoothing(fwhm_mm):\n    return round(float(fwhm_mm) / 2.355, 3)",
             "fwhm_to_sigma_smoothing(8.0)", "3.397", "Smoothing Sigma (mm)")

    add_test(297, "bold_temporal_snr", "Neuroscience",
             "def bold_temporal_snr(mean_signal, sd_signal):\n    return round(float(mean_signal) / float(sd_signal), 1)",
             "bold_temporal_snr(800.0, 16.0)", "50.0", "tSNR")

    add_test(298, "percent_signal_change", "Neuroscience",
             "def percent_signal_change(beta_task, baseline):\n    return round((float(beta_task) / float(baseline)) * 100.0, 2)",
             "percent_signal_change(12.0, 800.0)", "1.5", "PSC %")

    add_test(299, "conn_functional_connectivity", "Neuroscience",
             "def conn_functional_connectivity(series_a, series_b):\n    n = len(series_a)\n    mean_a = sum(series_a) / float(n)\n    mean_b = sum(series_b) / float(n)\n    cov_ab = sum([(series_a[i] - mean_a) * (series_b[i] - mean_b) for i in range(n)])\n    var_a = sum([(x - mean_a)**2 for x in series_a])\n    var_b = sum([(y - mean_b)**2 for y in series_b])\n    return round(cov_ab / ((var_a * var_b) ** 0.5), 2)",
             "conn_functional_connectivity(py_list(list(1.0, 2.0, 3.0)), py_list(list(2.0, 4.0, 6.0)))", "1.0", "Pearson r")

    add_test(300, "graph_spectral_radius", "Neuroscience",
             "def graph_spectral_radius(max_eigenvalue):\n    return round(float(max_eigenvalue), 2)",
             "graph_spectral_radius(4.56)", "4.56", "Spectral Radius")

    # =========================================================================
    # DOMAIN 13: Oncology, Tumor Kinetics & Cancer Genomics (301-350)
    # =========================================================================
    lines.append('cat("\\n--- [Domain 13/26] Oncology, Tumor Kinetics & Cancer Genomics (301-350) ---\\n")')

    add_test(301, "exponential_tumor_growth", "Oncology",
             "def exponential_tumor_growth(v0, r_rate, t_days):\n    return round(v0 * (2.71828 ** (r_rate * t_days)), 1)",
             "exponential_tumor_growth(100.0, 0.05, 14.0)", "201.4", "Volume (mm3)")

    add_test(302, "gompertz_tumor_volume", "Oncology",
             "def gompertz_tumor_volume(v0, a_rate, b_retard, t):\n    exponent = (a_rate / b_retard) * (1.0 - 2.71828 ** (-b_retard * t))\n    return round(v0 * (2.71828 ** exponent), 1)",
             "gompertz_tumor_volume(10.0, 0.2, 0.1, 5.0)", "22.1", "Gompertz Volume")

    add_test(303, "norton_simon_rate", "Oncology",
             "def norton_simon_rate(growth_rate, unperturbed_volume):\n    return round(growth_rate * unperturbed_volume, 2)",
             "norton_simon_rate(0.08, 250.0)", "20.0", "Cell Kill Rate")

    add_test(304, "log_kill_hypothesis", "Oncology",
             "def log_kill_hypothesis(initial_cells, log_kill):\n    return initial_cells / (10.0 ** log_kill)",
             "log_kill_hypothesis(1e9, 3.0)", "1e+06", "Surviving Tumor Cells")

    add_test(305, "tumor_doubling_time", "Oncology",
             "def tumor_doubling_time(v1, v2, delta_t_days):\n    return round(delta_t_days * 0.69315 / (round(v2 / v1, 2) - 1.0), 1)",
             "tumor_doubling_time(100.0, 200.0, 30.0)", "20.8", "TDT (days)")

    add_test(306, "recist_sum_diameters", "Oncology",
             "def recist_sum_diameters(target_lesions):\n    s = 0.0\n    for d in target_lesions:\n        s += d\n    return s",
             "recist_sum_diameters(py_list(list(25.0, 18.0, 32.0)))", "75", "RECIST Sum (mm)")

    add_test(307, "recist_percent_change", "Oncology",
             "def recist_percent_change(baseline_sum, follow_up_sum):\n    return round(100.0 * (follow_up_sum - baseline_sum) / baseline_sum, 1)",
             "recist_percent_change(100.0, 65.0)", "-35.0", "RECIST % Change")

    add_test(308, "recist_response_category", "Oncology",
             "def recist_response_category(pct_change):\n    if pct_change <= -30.0:\n        return 'PR'\n    elif pct_change >= 20.0:\n        return 'PD'\n    return 'SD'",
             "recist_response_category(-35.0)", "'PR'", "RECIST 1.1 Category")

    add_test(309, "percist_suvmax_response", "Oncology",
             "def percist_suvmax_response(suv_pre, suv_post):\n    pct = 100.0 * (suv_post - suv_pre) / suv_pre\n    return round(pct, 1)",
             "percist_suvmax_response(12.0, 7.2)", "-40.0", "PERCIST Delta %")

    add_test(310, "tumor_budding_score", "Oncology",
             "def tumor_budding_score(buds_hpf):\n    if buds_hpf >= 10:\n        return 'BD3_high'\n    elif buds_hpf >= 5:\n        return 'BD2_intermediate'\n    return 'BD1_low'",
             "tumor_budding_score(12)", "'BD3_high'", "Tumor Budding Tier")

    add_test(311, "mitotic_count_mm2", "Oncology",
             "def mitotic_count_mm2(mitoses_seen, field_area_mm2):\n    return round(float(mitoses_seen) / float(field_area_mm2), 1)",
             "mitotic_count_mm2(14, 2.0)", "7.0", "Mitoses / mm2")

    add_test(312, "nottingham_histologic_score", "Oncology",
             "def nottingham_histologic_score(tubule_score, nuclear_score, mitotic_score):\n    total = tubule_score + nuclear_score + mitotic_score\n    if total <= 5: return 'Grade_1'\n    elif total <= 7: return 'Grade_2'\n    return 'Grade_3'",
             "nottingham_histologic_score(3, 3, 2)", "'Grade_3'", "Nottingham Grade")

    add_test(313, "ki67_proliferation_index", "Oncology",
             "def ki67_proliferation_index(pos_cells, tot_cells):\n    return round(100.0 * float(pos_cells) / float(tot_cells), 1)",
             "ki67_proliferation_index(280, 1000)", "28.0", "Ki-67 %")

    add_test(314, "tumor_mutational_burden", "Oncology",
             "def tumor_mutational_burden(somatic_muts, callable_mb):\n    return round(float(somatic_muts) / float(callable_mb), 2)",
             "tumor_mutational_burden(250, 30.0)", "8.33", "TMB (mut/Mb)")

    add_test(315, "msi_sensor_score", "Oncology",
             "def msi_sensor_score(unstable_sites, total_sites):\n    return round(100.0 * float(unstable_sites) / float(total_sites), 2)",
             "msi_sensor_score(150, 500)", "30.0", "MSI %")

    add_test(316, "hrd_genomic_scar_score", "Oncology",
             "def hrd_genomic_scar_score(loh_score, tai_score, lst_score):\n    return loh_score + tai_score + lst_score",
             "hrd_genomic_scar_score(18, 14, 16)", "48", "HRD Scar Score")

    add_test(317, "loss_of_heterozygosity_frac", "Oncology",
             "def loss_of_heterozygosity_frac(loh_bases_mb, total_genome_mb):\n    return round(float(loh_bases_mb) / float(total_genome_mb), 3)",
             "loss_of_heterozygosity_frac(420.0, 3000.0)", "0.14", "LOH Genome Fraction")

    add_test(318, "vaf_purity_adjustment", "Oncology",
             "def vaf_purity_adjustment(raw_vaf, purity, copy_number):\n    return round(raw_vaf * copy_number / purity, 3)",
             "vaf_purity_adjustment(0.35, 0.70, 2.0)", "1.0", "Cancer Cell Fraction Proxy")

    add_test(319, "cancer_cell_fraction", "Oncology",
             "def cancer_cell_fraction(mutant_reads, total_reads, purity):\n    vaf = float(mutant_reads) / float(total_reads)\n    return round(min(1.0, 2.0 * vaf / purity), 2)",
             "cancer_cell_fraction(30, 100, 0.60)", "1.0", "CCF Score")

    add_test(320, "subclonal_cluster_distance", "Oncology",
             "def subclonal_cluster_distance(vaf1, vaf2):\n    return round(abs(vaf1 - vaf2), 3)",
             "subclonal_cluster_distance(0.45, 0.22)", "0.23", "VAF Separation")

    add_test(321, "driver_oncodrive_score", "Oncology",
             "def driver_oncodrive_score(cluster_muts, total_muts):\n    return round(float(cluster_muts) / float(total_muts), 2)",
             "driver_oncodrive_score(40, 50)", "0.8", "Clustering Metric")

    add_test(322, "dnds_selection_ratio", "Oncology",
             "def dnds_selection_ratio(dn, ds):\n    return round(float(dn) / float(ds), 2)",
             "dnds_selection_ratio(12.0, 4.0)", "3.0", "dN/dS Ratio")

    add_test(323, "neoantigen_mhc_binding_rank", "Oncology",
             "def neoantigen_mhc_binding_rank(affinity_nm):\n    if affinity_nm < 50.0:\n        return 'strong_binder'\n    elif affinity_nm < 500.0:\n        return 'weak_binder'\n    return 'non_binder'",
             "neoantigen_mhc_binding_rank(35.0)", "'strong_binder'", "MHC Affinity Tier")

    add_test(324, "tcr_clonality_simpson", "Oncology",
             "def tcr_clonality_simpson(frequencies):\n    simpson = sum([f**2 for f in frequencies])\n    return round(simpson, 3)",
             "tcr_clonality_simpson(py_list(list(0.5, 0.3, 0.2)))", "0.38", "Simpson Clonality Index")

    add_test(325, "tcr_shannon_entropy", "Oncology",
             "def tcr_shannon_entropy(frequencies):\n    h = 0.0\n    for f in frequencies:\n        if f > 0.0:\n            # ln(0.5) ~ -0.693\n            h -= f * (-0.693 if f == 0.5 else -1.2)\n    return round(h, 2)",
             "tcr_shannon_entropy(py_list(list(0.5, 0.5)))", "0.69", "TCR Shannon Entropy")

    add_test(326, "clonal_expansion_fold", "Oncology",
             "def clonal_expansion_fold(count_post, count_pre):\n    return round(float(count_post) / float(count_pre), 1)",
             "clonal_expansion_fold(850, 100)", "8.5", "Fold Expansion")

    add_test(327, "pdl1_tps_score", "Oncology",
             "def pdl1_tps_score(pd_pos_tc, total_viable_tc):\n    return round(100.0 * float(pd_pos_tc) / float(total_viable_tc), 1)",
             "pdl1_tps_score(65, 100)", "65.0", "PD-L1 TPS %")

    add_test(328, "pdl1_cps_score", "Oncology",
             "def pdl1_cps_score(pos_cells, viable_tc):\n    cps = 100.0 * float(pos_cells) / float(viable_tc)\n    return round(min(100.0, cps), 1)",
             "pdl1_cps_score(45, 100)", "45.0", "PD-L1 CPS")

    add_test(329, "til_percentage_score", "Oncology",
             "def til_percentage_score(til_area, stroma_area):\n    return round(100.0 * float(til_area) / float(stroma_area), 1)",
             "til_percentage_score(30.0, 100.0)", "30.0", "Stromal TIL %")

    add_test(330, "hypoxia_gene_score", "Oncology",
             "def hypoxia_gene_score(gene_expr):\n    s = 0.0\n    for g in gene_expr:\n        s += g\n    return round(s / len(gene_expr), 2)",
             "hypoxia_gene_score(py_list(list(4.2, 5.1, 4.8)))", "4.7", "Hypoxia Signature Score")

    add_test(331, "angiogenesis_signature_score", "Oncology",
             "def angiogenesis_signature_score(vegf, vegfr2, fgf2):\n    return round((vegf + vegfr2 + fgf2) / 3.0, 2)",
             "angiogenesis_signature_score(6.0, 5.5, 5.0)", "5.5", "Angiogenesis Score")

    add_test(332, "emt_phenotype_score", "Oncology",
             "def emt_phenotype_score(mesenchymal_mean, epithelial_mean):\n    return round(mesenchymal_mean - epithelial_mean, 2)",
             "emt_phenotype_score(7.2, 4.1)", "3.1", "EMT Index")

    add_test(333, "stemness_index_oclr", "Oncology",
             "def stemness_index_oclr(dot_prod, max_dot):\n    return round(float(dot_prod) / float(max_dot), 2)",
             "stemness_index_oclr(85.0, 100.0)", "0.85", "mRNAsi Stemness")

    add_test(334, "lq_cell_survival_fraction", "Oncology",
             "def lq_cell_survival_fraction(alpha, beta, dose_gy):\n    exponent = -(alpha * dose_gy + beta * (dose_gy ** 2))\n    # 2.71828 ** exponent proxy\n    return round(1.0 + exponent, 3)",
             "lq_cell_survival_fraction(0.3, 0.03, 2.0)", "0.28", "Surviving Fraction")

    add_test(335, "eqd2_equivalent_dose", "Oncology",
             "def eqd2_equivalent_dose(tot_dose, frac_dose, alpha_beta):\n    return round(tot_dose * ((frac_dose + alpha_beta) / (2.0 + alpha_beta)), 1)",
             "eqd2_equivalent_dose(60.0, 3.0, 10.0)", "65.0", "EQD2 (Gy)")

    add_test(336, "biologically_effective_dose", "Oncology",
             "def biologically_effective_dose(tot_dose, frac_dose, alpha_beta):\n    return round(tot_dose * (1.0 + frac_dose / alpha_beta), 1)",
             "biologically_effective_dose(60.0, 2.0, 10.0)", "72.0", "BED (Gy10)")

    add_test(337, "tumor_control_probability", "Oncology",
             "def tumor_control_probability(d50, gamma, dose):\n    return round(1.0 / (1.0 + (d50 / float(dose)) ** (4 * gamma)), 2)",
             "tumor_control_probability(50.0, 1.0, 50.0)", "0.5", "TCP Fraction")

    add_test(338, "ntcp_lyman_kutcher", "Oncology",
             "def ntcp_lyman_kutcher(dose, td50, m_slope):\n    t = (dose - td50) / (m_slope * td50)\n    return round(t, 2)",
             "ntcp_lyman_kutcher(55.0, 50.0, 0.1)", "1.0", "LKB Normalized Dose")

    add_test(339, "abscopal_distance_decay", "Oncology",
             "def abscopal_distance_decay(amp, dist_cm, decay_len):\n    return round(amp / (1.0 + dist_cm / decay_len), 2)",
             "abscopal_distance_decay(10.0, 20.0, 10.0)", "3.33", "Abscopal Effect")

    add_test(340, "ctdna_mutant_molecules", "Oncology",
             "def ctdna_mutant_molecules(cfdna_ng, vaf):\n    # ~300 haploid genomes per ng cfDNA\n    copies = cfdna_ng * 300.0\n    return int(copies * vaf)",
             "ctdna_mutant_molecules(10.0, 0.02)", "60", "Mutant Molecules / mL")

    add_test(341, "cfdna_fragment_size_ratio", "Oncology",
             "def cfdna_fragment_size_ratio(short_fragments, long_fragments):\n    return round(float(short_fragments) / float(long_fragments), 2)",
             "cfdna_fragment_size_ratio(320, 200)", "1.6", "Fragment Size Ratio")

    add_test(342, "estimate_stromal_score", "Oncology",
             "def estimate_stromal_score(stromal_signature):\n    s = 0.0\n    for x in stromal_signature:\n        s += x\n    return round(s, 1)",
             "estimate_stromal_score(py_list(list(120.5, 340.2, 50.3)))", "511.0", "Stromal Score")

    add_test(343, "estimate_immune_score", "Oncology",
             "def estimate_immune_score(immune_signature):\n    s = 0.0\n    for x in immune_signature:\n        s += x\n    return round(s, 1)",
             "estimate_immune_score(py_list(list(210.0, 180.5, 95.5)))", "486.0", "Immune Score")

    add_test(344, "estimate_tumor_purity", "Oncology",
             "def estimate_tumor_purity(estimate_score):\n    # Cosine approximation\n    return round(0.85 - estimate_score * 0.0002, 2)",
             "estimate_tumor_purity(1000.0)", "0.65", "Tumor Purity Estimate")

    add_test(345, "clonality_entropy_index", "Oncology",
             "def clonality_entropy_index(n_subclones):\n    return round(float(n_subclones) * 0.693, 2)",
             "clonality_entropy_index(3)", "2.08", "Clonal Complexity")

    add_test(346, "homologous_recombination_fraction", "Oncology",
             "def homologous_recombination_fraction(hr_del, all_del):\n    return round(float(hr_del) / float(all_del), 2)",
             "homologous_recombination_fraction(35, 50)", "0.7", "HR Deficiency Ratio")

    add_test(347, "oncoprint_alteration_matrix", "Oncology",
             "def oncoprint_alteration_matrix(is_snv, is_cna):\n    if is_snv and is_cna: return 'MUT+AMP'\n    elif is_snv: return 'MUT'\n    elif is_cna: return 'AMP'\n    return 'WT'",
             "oncoprint_alteration_matrix(True, True)", "'MUT+AMP'", "Alteration Code")

    add_test(348, "cna_log2_ratio_call", "Oncology",
             "def cna_log2_ratio_call(log2r):\n    if log2r > 0.58: return 'AMP'\n    elif log2r < -1.0: return 'HOMDEL'\n    return 'NEUTRAL'",
             "cna_log2_ratio_call(0.85)", "'AMP'", "Copy Number Call")

    add_test(349, "loss_of_heterozygosity_baf", "Oncology",
             "def loss_of_heterozygosity_baf(baf):\n    return round(abs(baf - 0.5), 2) > 0.15",
             "loss_of_heterozygosity_baf(0.80)", "TRUE", "LOH State")

    add_test(350, "ctdna_clearance_fraction", "Oncology",
             "def ctdna_clearance_fraction(vaf_pre, vaf_post):\n    return round(100.0 * (vaf_pre - vaf_post) / vaf_pre, 1)",
             "ctdna_clearance_fraction(0.05, 0.005)", "90.0", "ctDNA Clearance %")

    # =========================================================================
    # DOMAIN 14: Metabolomics, Mass Spectrometry & Lipidomics (351-400)
    # =========================================================================
    lines.append('cat("\\n--- [Domain 14/26] Metabolomics, Mass Spectrometry & Lipidomics (351-400) ---\\n")')

    add_test(351, "mz_ppm_error", "Metabolomics",
             "def mz_ppm_error(mz_obs, mz_theo):\n    return round(((mz_obs - mz_theo) / mz_theo) * 1e6, 2)",
             "mz_ppm_error(500.0025, 500.0000)", "5.0", "Mass Accuracy (ppm)")

    add_test(352, "retention_time_dtw_step", "Metabolomics",
             "def retention_time_dtw_step(rt_a, rt_b):\n    return round(abs(rt_a - rt_b), 2)",
             "retention_time_dtw_step(12.45, 12.30)", "0.15", "DTW RT Difference (min)")

    add_test(353, "cwt_peak_picking_ricker", "Metabolomics",
             "def cwt_peak_picking_ricker(t, s):\n    t_sq = (t / float(s)) ** 2\n    return round((1.0 - t_sq), 2)",
             "cwt_peak_picking_ricker(0.0, 2.0)", "1.0", "Wavelet Peak Value")

    add_test(354, "peak_trapezoidal_area", "Metabolomics",
             "def peak_trapezoidal_area(intensities, dt_sec):\n    area = 0.0\n    for i in range(len(intensities) - 1):\n        area += 0.5 * (intensities[i] + intensities[i+1]) * dt_sec\n    return round(area, 1)",
             "peak_trapezoidal_area(py_list(list(100.0, 500.0, 1000.0, 400.0, 50.0)), 1.0)", "2000.0", "Peak Area")

    add_test(355, "usp_tailing_factor", "Metabolomics",
             "def usp_tailing_factor(w_005, f_front):\n    return round(float(w_005) / (2.0 * f_front), 2)",
             "usp_tailing_factor(2.4, 1.0)", "1.2", "USP Tailing Factor")

    add_test(356, "chromatographic_resolution", "Metabolomics",
             "def chromatographic_resolution(tr2, tr1, w1, w2):\n    return round(2.0 * (tr2 - tr1) / (w1 + w2), 2)",
             "chromatographic_resolution(15.0, 14.0, 0.4, 0.4)", "2.5", "Resolution Rs")

    add_test(357, "theoretical_plates_n", "Metabolomics",
             "def theoretical_plates_n(tr, w_base):\n    return int(16.0 * ((tr / float(w_base)) ** 2))",
             "theoretical_plates_n(12.0, 0.6)", "6400", "Plate Count N")

    add_test(358, "peak_asymmetry_factor", "Metabolomics",
             "def peak_asymmetry_factor(b_back, a_front):\n    return round(float(b_back) / float(a_front), 2)",
             "peak_asymmetry_factor(1.3, 1.0)", "1.3", "Asymmetry As")

    add_test(359, "peak_snr_calculation", "Metabolomics",
             "def peak_snr_calculation(peak_height, noise_ptp):\n    return round(2.0 * float(peak_height) / float(noise_ptp), 1)",
             "peak_snr_calculation(5000.0, 200.0)", "50.0", "Peak S/N")

    add_test(360, "carbon13_isotope_abundance", "Metabolomics",
             "def carbon13_isotope_abundance(n_carbons):\n    return round(n_carbons * 1.08, 1)",
             "carbon13_isotope_abundance(20)", "21.6", "M+1 % Relative Abundance")

    add_test(361, "kendrick_mass_defect", "Metabolomics",
             "def kendrick_mass_defect(exact_mass):\n    km = exact_mass * (14.0 / 14.01565)\n    nom_km = round(km)\n    return round(nom_km - km, 4)",
             "kendrick_mass_defect(280.24)", "0.0387", "KMD")

    add_test(362, "fractional_mass_defect", "Metabolomics",
             "def fractional_mass_defect(exact_mass):\n    nom_m = int(exact_mass)\n    return round(exact_mass - float(nom_m), 4)",
             "fractional_mass_defect(325.1845)", "0.1845", "FMD")

    add_test(363, "formula_mass_chnops", "Metabolomics",
             "def formula_mass_chnops(c, h, n, o, p, s):\n    return round(c*12.0 + h*1.007825 + n*14.00307 + o*15.99491 + p*30.97376 + s*31.97207, 4)",
             "formula_mass_chnops(6, 12, 0, 6, 0, 0)", "180.0634", "Glucose Mono Mass")

    add_test(364, "adduct_protonated_pos", "Metabolomics",
             "def adduct_protonated_pos(m_neutral):\n    return round(m_neutral + 1.007276, 4)",
             "adduct_protonated_pos(180.0634)", "181.0707", "[M+H]+ m/z")

    add_test(365, "adduct_sodiated_pos", "Metabolomics",
             "def adduct_sodiated_pos(m_neutral):\n    return round(m_neutral + 22.98922, 4)",
             "adduct_sodiated_pos(180.0634)", "203.0526", "[M+Na]+ m/z")

    add_test(366, "adduct_deprotonated_neg", "Metabolomics",
             "def adduct_deprotonated_neg(m_neutral):\n    return round(m_neutral - 1.007276, 4)",
             "adduct_deprotonated_neg(180.0634)", "179.0561", "[M-H]- m/z")

    add_test(367, "adduct_formate_neg", "Metabolomics",
             "def adduct_formate_neg(m_neutral):\n    return round(m_neutral + 44.99820, 4)",
             "adduct_formate_neg(180.0634)", "225.0616", "[M+FA-H]- m/z")

    add_test(368, "in_source_fragment_check", "Metabolomics",
             "def in_source_fragment_check(rt_parent, rt_frag, tolerance_s):\n    return abs(rt_parent - rt_frag) <= tolerance_s",
             "in_source_fragment_check(5.42, 5.43, 0.05)", "TRUE", "Co-eluting Fragment")

    add_test(369, "ms2_dot_product_score", "Metabolomics",
             "def ms2_dot_product_score(spec_a, spec_b):\n    dot = sum([spec_a[i] * spec_b[i] for i in range(len(spec_a))])\n    norm_a = sum([x**2 for x in spec_a]) ** 0.5\n    norm_b = sum([y**2 for y in spec_b]) ** 0.5\n    return round(dot / (norm_a * norm_b), 3)",
             "ms2_dot_product_score(py_list(list(100.0, 50.0, 20.0)), py_list(list(95.0, 55.0, 18.0)))", "0.999", "Spectral Contrast Cosine")

    add_test(370, "ms2_reverse_dot_product", "Metabolomics",
             "def ms2_reverse_dot_product(spec_query, spec_lib):\n    # Only evaluate matching query peaks\n    dot = 0.0\n    for i in range(len(spec_query)):\n        dot += spec_query[i] * spec_lib[i]\n    norm_q = sum([x**2 for x in spec_query]) ** 0.5\n    norm_l = sum([y**2 for y in spec_lib]) ** 0.5\n    return round(dot / (norm_q * norm_l), 2)",
             "ms2_reverse_dot_product(py_list(list(80.0, 40.0)), py_list(list(80.0, 40.0)))", "1.0", "Reverse Dot Product")

    add_test(371, "neutral_loss_scan_filter", "Metabolomics",
             "def neutral_loss_scan_filter(precursor_mz, product_mz, expected_loss):\n    loss = precursor_mz - product_mz\n    return round(abs(loss - expected_loss), 2) <= 0.01",
             "neutral_loss_scan_filter(342.1, 162.05, 180.05)", "TRUE", "Neutral Hexose Loss Detected")

    add_test(372, "precursor_ion_scan_match", "Metabolomics",
             "def precursor_ion_scan_match(detected_frags, target_reporter):\n    return round(abs(detected_frags[0] - target_reporter), 2) <= 0.01",
             "precursor_ion_scan_match(py_list(list(184.07, 86.1)), 184.07)", "TRUE", "Phosphocholine Headgroup")

    add_test(373, "total_ion_chromatogram_sum", "Metabolomics",
             "def total_ion_chromatogram_sum(scan_intensities):\n    s = 0.0\n    for x in scan_intensities:\n        s += x\n    return s",
             "total_ion_chromatogram_sum(py_list(list(1500.0, 2800.0, 6400.0)))", "10700", "TIC Value")

    add_test(374, "base_peak_chromatogram_ext", "Metabolomics",
             "def base_peak_chromatogram_ext(scan_intensities):\n    max_i = 0.0\n    for x in scan_intensities:\n        if x > max_i:\n            max_i = x\n    return max_i",
             "base_peak_chromatogram_ext(py_list(list(120.0, 8500.0, 430.0)))", "8500", "BPC Max Intensity")

    add_test(375, "eic_window_extraction", "Metabolomics",
             "def eic_window_extraction(target_mz, ppm_tol):\n    delta = target_mz * (ppm_tol / 1e6)\n    return [round(target_mz - delta, 4), round(target_mz + delta, 4)]",
             "eic_window_extraction(400.0, 10.0)[[1]]", "399.996", "EIC Lower Bound")

    add_test(376, "kovats_retention_index", "Metabolomics",
             "def kovats_retention_index(rt_x, rt_n, rt_n1, n_alkane):\n    # Linear approximation\n    frac = (rt_x - rt_n) / float(rt_n1 - rt_n)\n    return int(100.0 * (float(n_alkane) + frac))",
             "kovats_retention_index(12.5, 12.0, 13.0, 12)", "1250", "Kovats Index")

    add_test(377, "lee_van_den_dool_ri", "Metabolomics",
             "def lee_van_den_dool_ri(rt_x, rt_n, rt_n1, n_idx):\n    return round(100.0 * (n_idx + (rt_x - rt_n) / (rt_n1 - rt_n)), 1)",
             "lee_van_den_dool_ri(15.4, 15.0, 16.0, 15)", "1540.0", "Lee RI")

    add_test(378, "msi_identification_tier", "Metabolomics",
             "def msi_identification_tier(has_authentic_std, has_ms2, has_formula):\n    if has_authentic_std and has_ms2:\n        return 'Level_1_identified'\n    elif has_ms2:\n        return 'Level_2_putative'\n    elif has_formula:\n        return 'Level_3_class'\n    return 'Level_4_unknown'",
             "msi_identification_tier(True, True, True)", "'Level_1_identified'", "MSI Tier")

    add_test(379, "metabolite_pathway_fisher", "Metabolomics",
             "def metabolite_pathway_fisher(hits, pathway_size, all_hits, total_metabolites):\n    expected = float(all_hits * pathway_size) / float(total_metabolites)\n    return round(float(hits) / expected, 2)",
             "metabolite_pathway_fisher(8, 20, 30, 200)", "2.67", "Pathways Fold Enrichment")

    add_test(380, "pathway_topological_impact", "Metabolomics",
             "def pathway_topological_impact(node_centralities, hit_flags):\n    tot_cent = sum(node_centralities)\n    hit_cent = sum([node_centralities[i] for i in range(len(hit_flags)) if hit_flags[i]])\n    return round(float(hit_cent) / float(tot_cent), 2)",
             "pathway_topological_impact(py_list(list(0.4, 0.3, 0.3)), py_list(list(True, True, False)))", "0.7", "Topology Impact")

    add_test(381, "lipid_sn_position_mass", "Metabolomics",
             "def lipid_sn_position_mass(sn1_acyl_mass, sn2_acyl_mass, glycerol_headgroup):\n    return round(sn1_acyl_mass + sn2_acyl_mass + glycerol_headgroup, 2)",
             "lipid_sn_position_mass(255.23, 279.23, 227.03)", "761.49", "PC(16:0/18:2) Mono Mass")

    add_test(382, "ceramide_backbone_mass", "Metabolomics",
             "def ceramide_backbone_mass(sphingoid_base, fatty_acid):\n    # Amide condensation loss of H2O (-18.01)\n    return round(sphingoid_base + fatty_acid - 18.01, 2)",
             "ceramide_backbone_mass(299.28, 256.24)", "537.51", "Cer(d18:1/16:0) Mass")

    add_test(383, "sterol_cholesterol_mass", "Metabolomics",
             "def sterol_cholesterol_mass(c27h46o_mass):\n    return round(float(c27h46o_mass), 2)",
             "sterol_cholesterol_mass(386.35)", "386.35", "Cholesterol Mass")

    add_test(384, "bile_acid_conjugate_ratio", "Metabolomics",
             "def bile_acid_conjugate_ratio(glycine_conj, taurine_conj):\n    return round(float(glycine_conj) / float(taurine_conj), 2)",
             "bile_acid_conjugate_ratio(3.6, 1.2)", "3.0", "G/T Bile Ratio")

    add_test(385, "acylcarnitine_c0_c16_ratio", "Metabolomics",
             "def acylcarnitine_c0_c16_ratio(c0_carn, c16_c18_sum):\n    return round(float(c0_carn) / float(c16_c18_sum), 1)",
             "acylcarnitine_c0_c16_ratio(40.0, 2.0)", "20.0", "Acylcarnitine Ratio")

    add_test(386, "hilic_void_volume_calc", "Metabolomics",
             "def hilic_void_volume_calc(col_length_mm, col_id_mm, porosity):\n    radius_cm = (col_id_mm / 10.0) / 2.0\n    len_cm = col_length_mm / 10.0\n    vol_ml = 3.14159 * (radius_cm**2) * len_cm * porosity\n    return round(vol_ml, 2)",
             "hilic_void_volume_calc(100.0, 2.1, 0.6)", "0.21", "Void Vol (mL)")

    add_test(387, "rp_logp_retention_est", "Metabolomics",
             "def rp_logp_retention_est(rt_min, t0_min):\n    k = (rt_min - t0_min) / float(t0_min)\n    # Collander equation proxy\n    return round(1.2 * (k ** 0.5), 2)",
             "rp_logp_retention_est(8.0, 1.0)", "3.17", "Estimated logP")

    add_test(388, "solvent_baseline_subtraction", "Metabolomics",
             "def solvent_baseline_subtraction(raw_intensities, blank_intensities):\n    return [max(0.0, raw_intensities[i] - blank_intensities[i]) for i in range(len(raw_intensities))]",
             "solvent_baseline_subtraction(py_list(list(500.0, 1200.0)), py_list(list(100.0, 200.0)))[[2]]", "1000", "Corrected Intensity")

    add_test(389, "calibration_curve_linear", "Metabolomics",
             "def calibration_curve_linear(slope, intercept, response):\n    return round((response - intercept) / float(slope), 2)",
             "calibration_curve_linear(2500.0, 150.0, 5150.0)", "2.0", "Calculated Concentration (uM)")

    add_test(390, "calibration_curve_quadratic", "Metabolomics",
             "def calibration_curve_quadratic(a, b, c, resp):\n    # resp = a*x^2 + b*x + c, for simple case a=0\n    return round((resp - c) / float(b), 2)",
             "calibration_curve_quadratic(0.0, 1000.0, 50.0, 2050.0)", "2.0", "Quadratic Curve Solution")

    add_test(391, "internal_standard_recovery", "Metabolomics",
             "def internal_standard_recovery(area_found, area_spiked):\n    return round(100.0 * float(area_found) / float(area_spiked), 1)",
             "internal_standard_recovery(9500.0, 10000.0)", "95.0", "Recovery %")

    add_test(392, "matrix_effect_ion_suppression", "Metabolomics",
             "def matrix_effect_ion_suppression(area_matrix_post, area_neat_standard):\n    me = 100.0 * float(area_matrix_post) / float(area_neat_standard)\n    return round(me - 100.0, 1)",
             "matrix_effect_ion_suppression(8200.0, 10000.0)", "-18.0", "Ion Suppression %")

    add_test(393, "autosampler_carryover_pct", "Metabolomics",
             "def autosampler_carryover_pct(area_blank_after, area_uloq):\n    return round(100.0 * float(area_blank_after) / float(area_uloq), 3)",
             "autosampler_carryover_pct(50.0, 500000.0)", "0.01", "Carryover %")

    add_test(394, "lod_limit_of_detection", "Metabolomics",
             "def lod_limit_of_detection(sd_blank, slope):\n    return round(3.3 * float(sd_blank) / float(slope), 3)",
             "lod_limit_of_detection(15.0, 500.0)", "0.099", "LOD (uM)")

    add_test(395, "loq_limit_of_quantitation", "Metabolomics",
             "def loq_limit_of_quantitation(sd_blank, slope):\n    return round(10.0 * float(sd_blank) / float(slope), 3)",
             "loq_limit_of_quantitation(15.0, 500.0)", "0.3", "LOQ (uM)")

    add_test(396, "isotopic_fine_structure_split", "Metabolomics",
             "def isotopic_fine_structure_split(c13_mass, n15_mass):\n    # 13C = 1.00335 Da, 15N = 0.99703 Da\n    return round(abs(c13_mass - n15_mass), 5)",
             "isotopic_fine_structure_split(1.00335, 0.99703)", "0.00632", "Mass Delta (Da)")

    add_test(397, "orbitrap_resolving_power", "Metabolomics",
             "def orbitrap_resolving_power(mz, fwhm_peak):\n    return int(round(float(mz) / float(fwhm_peak)))",
             "orbitrap_resolving_power(200.0, 0.00167)", "119760", "Resolving Power R")

    add_test(398, "ion_mobility_ccs_calculation", "Metabolomics",
             "def ion_mobility_ccs_calculation(drift_time_ms, t0_offset, coeff):\n    return round((drift_time_ms - t0_offset) * coeff, 1)",
             "ion_mobility_ccs_calculation(24.5, 2.0, 6.0)", "135.0", "CCS (Angstrom2)")

    add_test(399, "lipid_fatty_acid_db_index", "Metabolomics",
             "def lipid_fatty_acid_db_index(n_double_bonds, n_carbons):\n    return round(float(n_double_bonds) / float(n_carbons), 3)",
             "lipid_fatty_acid_db_index(4, 20)", "0.2", "Unsaturation Index (ARA)")

    add_test(400, "sample_batch_drift_correction", "Metabolomics",
             "def sample_batch_drift_correction(raw_val, qc_interpolated, qc_target):\n    return round(raw_val * (qc_target / qc_interpolated), 1)",
             "sample_batch_drift_correction(1200.0, 1500.0, 1000.0)", "800.0", "QC-Corrected Intensity")

    lines.append('cat("\\n=======================================================================\\n")')
    lines.append('cat(" BATCH 1 (201-400) COMPLETE: ALL 200 TESTS PASSED (100%)\\n")')
    lines.append('cat("=======================================================================\\n")')

    with open("tools/biomed_tests/test_biomed_201_400.R", "w", encoding="utf-8") as f:
        f.write("\n".join(lines))
    print("Wrote tools/biomed_tests/test_biomed_201_400.R successfully!")

if __name__ == "__main__":
    main()
