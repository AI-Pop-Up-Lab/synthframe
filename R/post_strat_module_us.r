library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(tidyr)
library(vglmer)

default_us_post_strat_config <- function() {
	list(
		eps = 1e-8,
		verbose = TRUE,
		n_sims = 150,
		min_n = 80,
		min_events = 15,
		seed = NULL,
		drop_other_gender = TRUE,
		export_cell_draws = FALSE,
		dem_party_name = "Democratic",
		rep_party_name = "Republican",
		share_col_mapping = list(
			"Democratic"   = c(cong = "dem_share",       pres = "state_pres_dem_share"),
			"Republican"   = c(cong = "rep_share",       pres = "state_pres_rep_share"),
			"Other"        = c(cong = "other_share",     pres = "state_pres_other_share"),
			"Did not vote" = c(cong = "no_vote_share",   pres = "state_pres_no_vote_share")
		),
		dem_aligned_party_pattern = "^(DFL|Democratic.*)$",
		other_party_name = "Other",
		stay_home_party_name = "Did not vote",
		survey_aliases = list(
			age_group = c("age_cat", "age_group"),
			gender = c("gender_cat", "gender"),
			race = c("race_cat"),
			education_level = c("educ_cat", "education", "educ", "education_level"),
			vote_2026 = c("vote_2026", "predicted_vote")
		),
		frame_aliases = list(
			age_group = c("age_cat", "age_group"),
			gender = c("gender_cat", "gender"),
			race = c("race_cat", "race"),
			education_level = c("educ_cat", "education", "educ", "education_level"),
			expected_N_raked = c("expected_N_raked", "raked_weight", "cell_pop")
		)
	)
}

make_us_post_logger <- function(verbose = TRUE) {
	function(...) {
		if (isTRUE(verbose)) {
			message(...)
		}
	}
}

resolve_us_post_strat_config <- function(config = list()) {
	resolved <- modifyList(default_us_post_strat_config(), config)
	resolved$msg <- make_us_post_logger(resolved$verbose)
	resolved
}

rename_us_post_alias_columns <- function(dat, alias_map) {
	for (canonical in names(alias_map)) {
		if (canonical %in% names(dat)) {
			next
		}
		aliases <- alias_map[[canonical]]
		alias_hits <- aliases[aliases %in% names(dat)]
		if (length(alias_hits) > 0) {
			names(dat)[match(alias_hits[[1]], names(dat))] <- canonical
		}
	}
	dat
}

drop_us_post_index_columns <- function(dat) {
	dat %>% select(-matches("^(\\.\\.\\.1|Unnamed: 0(\\.1)?|X)$"))
}

validate_us_post_required_columns <- function(dat, required, data_name) {
	missing_cols <- setdiff(required, names(dat))
	if (length(missing_cols) > 0) {
		stop("Missing columns in ", data_name, ": ", paste(missing_cols, collapse = ", "))
	}
}

resolve_us_post_party_order <- function(survey_model) {
	party_counts <- survey_model %>%
		count(vote_2026, sort = TRUE) %>%
		pull(vote_2026)

	if ("Did not vote" %in% party_counts) {
		c("Did not vote", setdiff(party_counts, "Did not vote"))
	} else {
		party_counts
	}
}

prepare_us_post_area_shares <- function(area_shares, config) {
	required_cols <- c("state_cd", "state_abbrv")
	for (mapping in config$share_col_mapping) {
		required_cols <- c(required_cols, mapping["cong"], mapping["pres"])
	}
	required_cols <- unique(required_cols)
	
	area_shares <- drop_us_post_index_columns(area_shares)
	validate_us_post_required_columns(area_shares, required_cols, "area_level_vote_shares")
	
	area_shares %>%
		mutate(
			state_cd = as.character(state_cd),
			state_abbrv = as.character(state_abbrv)
		) %>%
		distinct()
}


us_post_factor_vars <- function() {
    c("age_group", "gender", "race", "education_level",
      "state_abbrv", "state_cd", "past_vote")
}

get_us_post_master_levels <- function(survey, frame, vars) {
    out <- setNames(vector("list", length(vars)), vars)
    for (v in vars) {
        sv <- unique(as.character(survey[[v]]))
        fv <- unique(as.character(frame[[v]]))
        sv <- sv[!is.na(sv) & sv != ""]
        fv <- fv[!is.na(fv) & fv != ""]
        out[[v]] <- union(sv, fv)
    }
    out
}

apply_us_post_master_levels <- function(dat, master_levels) {
    for (v in names(master_levels)) {
        dat[[v]] <- factor(as.character(dat[[v]]), levels = master_levels[[v]])
    }
    dat
}

find_us_post_frame_only_levels <- function(survey, frame, vars) {
    out <- list()
    for (v in vars) {
        sv <- unique(as.character(survey[[v]]))
        fv <- unique(as.character(frame[[v]]))
        sv <- sv[!is.na(sv) & sv != ""]
        fv <- fv[!is.na(fv) & fv != ""]
        only <- setdiff(fv, sv)
        if (length(only) > 0) out[[v]] <- only
    }
    out
}

get_us_post_interaction_levels <- function(survey_model, frame_pred) {
    make_levels <- function(x, y) {
        s <- interaction(as.character(survey_model[[x]]),
                         as.character(survey_model[[y]]),
                         drop = FALSE, sep = "___")
        f <- interaction(as.character(frame_pred[[x]]),
                         as.character(frame_pred[[y]]),
                         drop = FALSE, sep = "___")
        union(as.character(s), as.character(f))
    }
    list(
        race_gender = make_levels("race", "gender"),
        race_age = make_levels("race", "age_group"),
        race_edu = make_levels("race", "education_level"),
        race_past_vote = make_levels("race", "past_vote"),
        age_gender = make_levels("age_group", "gender"),
        gender_edu = make_levels("gender", "education_level"),
        gender_past_vote = make_levels("gender", "past_vote"),
        age_edu = make_levels("age_group", "education_level"),
        age_past_vote = make_levels("age_group", "past_vote"),
        edu_past_vote = make_levels("education_level", "past_vote")
    )
}

prepare_us_post_survey_data <- function(survey, config) {
	survey_required <- c(
		"age_group", "gender", "race", "education_level", 
		"state_abbrv", "state_cd", "vote_2026", "past_vote"
	)

	survey <- survey %>%
		drop_us_post_index_columns() %>%
		rename_us_post_alias_columns(config$survey_aliases)

	validate_us_post_required_columns(survey, survey_required, "survey")

	if (isTRUE(config$drop_other_gender) && "gender" %in% names(survey)) {
		survey <- survey %>% filter(gender != "Other")
	}

	survey_model <- survey %>%
		mutate(
			age_group = as.character(age_group),
			gender = as.character(gender),
			race = as.character(race),
			education_level = as.character(education_level),
			state_abbrv = as.character(state_abbrv),
			state_cd = as.character(state_cd),
			vote_2026 = as.character(vote_2026),
			past_vote = as.character(past_vote)
		) %>%
		filter(if_all(all_of(survey_required), ~ !is.na(.x) & .x != ""))

	if (nrow(survey_model) == 0) {
		stop("No complete cases remain in survey after filtering model variables.")
	}

	party_order <- resolve_us_post_party_order(survey_model)

	if (length(party_order) < 2) {
		stop("Need at least two outcome categories in vote_2026 after filtering.")
	}

	survey_model %>%
		mutate(
			vote_2026 = factor(vote_2026, levels = party_order),
			age_group = factor(age_group),
			gender = factor(gender),
			race = factor(race),
			education_level = factor(education_level),
			state_abbrv = factor(state_abbrv),
			state_cd = factor(state_cd),
			past_vote = factor(past_vote)
		)
}


prepare_us_post_frame_data <- function(frame, survey_model, config,
                                       master_levels = NULL) {
    frame_required <- c("age_group", "gender", "race", "education_level",
                        "state_abbrv", "state_cd", "expected_N_raked",
                        "past_vote")
    frame <- frame %>%
        drop_us_post_index_columns() %>%
        rename_us_post_alias_columns(config$frame_aliases)
    validate_us_post_required_columns(frame, frame_required, "frame")

    frame_pred <- frame %>%
        mutate(
            age_group = as.character(age_group),
            gender = as.character(gender),
            race = as.character(race),
            education_level = as.character(education_level),
            state_abbrv = as.character(state_abbrv),
            state_cd = as.character(state_cd),
            past_vote = as.character(past_vote),
            expected_N_raked = as.numeric(expected_N_raked)
        ) %>%
        filter(
            !is.na(age_group), !is.na(gender), !is.na(race),
            !is.na(education_level), !is.na(state_abbrv),
            !is.na(state_cd), !is.na(past_vote),
            !is.na(expected_N_raked), expected_N_raked > 0
        )

    if (nrow(frame_pred) == 0) stop("No usable rows remain in frame after filtering.")

    vars <- us_post_factor_vars()
    if (is.null(master_levels)) {
        master_levels <- get_us_post_master_levels(survey_model, frame_pred, vars)
    }

    frame_only <- find_us_post_frame_only_levels(survey_model, frame_pred, vars)
    if (length(frame_only) > 0) {
        for (v in names(frame_only)) {
            config$msg("Frame-only levels in ", v, ": ",
                       paste(frame_only[[v]], collapse = ", "))
        }
    }

    survey_aligned <- apply_us_post_master_levels(survey_model, master_levels)
    frame_aligned <- apply_us_post_master_levels(frame_pred, master_levels)

    list(
        frame_pred = frame_aligned,
        survey_model = survey_aligned,
        master_levels = master_levels,
        frame_only_levels = frame_only
    )
}


add_us_post_interactions <- function(dat,
                                     race_gender_levels = NULL,
                                     race_age_levels = NULL,
                                     race_edu_levels = NULL,
                                     race_past_vote_levels = NULL,
                                     age_gender_levels = NULL,
                                     gender_edu_levels = NULL,
                                     gender_past_vote_levels = NULL,
                                     age_edu_levels = NULL,
                                     age_past_vote_levels = NULL,
                                     edu_past_vote_levels = NULL) {
    make_interaction <- function(x, y, lv = NULL) {
        raw <- interaction(as.character(x), as.character(y),
                           drop = FALSE, sep = "___")
        if (is.null(lv)) factor(as.character(raw))
        else factor(as.character(raw), levels = lv)
    }
    dat$race_gender <- make_interaction(dat$race, dat$gender, race_gender_levels)
    dat$race_age <- make_interaction(dat$race, dat$age_group, race_age_levels)
    dat$race_edu <- make_interaction(dat$race, dat$education_level, race_edu_levels)
    dat$race_past_vote <- make_interaction(dat$race, dat$past_vote, race_past_vote_levels)
    dat$age_gender <- make_interaction(dat$age_group, dat$gender, age_gender_levels)
    dat$gender_edu <- make_interaction(dat$gender, dat$education_level, gender_edu_levels)
    dat$gender_past_vote <- make_interaction(dat$gender, dat$past_vote, gender_past_vote_levels)
    dat$age_edu <- make_interaction(dat$age_group, dat$education_level, age_edu_levels)
    dat$age_past_vote <- make_interaction(dat$age_group, dat$past_vote, age_past_vote_levels)
    dat$edu_past_vote <- make_interaction(dat$education_level, dat$past_vote, edu_past_vote_levels)
    dat
}


make_us_post_stage_data <- function(
    data,
    party_name,
    area_shares,
    config,
    stage_obj = NULL,
    interaction_levels = NULL
) {
    if (!(party_name %in% names(config$share_col_mapping))) {
        stop("Party '", party_name, "' not found in config$share_col_mapping.")
    }
    if (!is.null(stage_obj) && is.null(stage_obj$fit)) {
        stop("Cannot construct prediction data for stage '", party_name,
             "': no fitted model. Status: ", stage_obj$status)
    }
    if (is.null(interaction_levels)) {
        stop("Global interaction levels must be supplied.")
    }

    required_interactions <- c(
        "race_gender", "race_age", "race_edu", "race_past_vote",
        "age_gender", "gender_edu", "gender_past_vote", "age_edu",
        "age_past_vote", "edu_past_vote"
    )
    missing_interactions <- required_interactions[
        vapply(interaction_levels[required_interactions],
               function(x) is.null(x) || length(x) == 0,
               logical(1))
    ]
    if (length(missing_interactions) > 0) {
        stop("Missing interaction levels: ",
             paste(missing_interactions, collapse = ", "))
    }

    share_cols <- config$share_col_mapping[[party_name]]
    lookup_sub <- area_shares %>%
        transmute(
            state_cd_chr = as.character(state_cd),
            cong_share_stage = as.numeric(!!sym(share_cols[["cong"]])),
            pres_share_stage = as.numeric(!!sym(share_cols[["pres"]]))
        )

    joined <- data %>%
        select(-any_of(c(
            "cong_share", "pres_share", "cong_share_scaled",
            "pres_share_scaled", required_interactions
        ))) %>%
        mutate(state_cd_chr = as.character(state_cd)) %>%
        left_join(lookup_sub, by = "state_cd_chr") %>%
        mutate(
            cong_share = coalesce(cong_share_stage, 0),
            pres_share = coalesce(pres_share_stage, 0),
            cong_share_scaled = as.numeric(scale(cong_share)),
            pres_share_scaled = as.numeric(scale(pres_share)),
            cong_share_scaled = if_else(is.na(cong_share_scaled), 0, cong_share_scaled),
            pres_share_scaled = if_else(is.na(pres_share_scaled), 0, pres_share_scaled)
        ) %>%
        select(-state_cd_chr, -cong_share_stage, -pres_share_stage)

    add_us_post_interactions(
        joined,
        race_gender_levels = interaction_levels$race_gender,
        race_age_levels = interaction_levels$race_age,
        race_edu_levels = interaction_levels$race_edu,
        race_past_vote_levels = interaction_levels$race_past_vote,
        age_gender_levels = interaction_levels$age_gender,
        gender_edu_levels = interaction_levels$gender_edu,
        gender_past_vote_levels = interaction_levels$gender_past_vote,
        age_edu_levels = interaction_levels$age_edu,
        age_past_vote_levels = interaction_levels$age_past_vote,
        edu_past_vote_levels = interaction_levels$edu_past_vote
    )
}

fit_us_post_stage <- function(dat, party_name, config) {
	d <- dat %>%
		mutate(y = as.integer(vote_2026 == party_name))

	n_total <- nrow(d)
	n_event <- sum(d$y, na.rm = TRUE)
	use_interactions <- n_total >= 300

	fallback_prob <- if (n_total > 0) n_event / n_total else 0
	fallback_prob <- pmin(pmax(fallback_prob, config$eps), 1 - config$eps)

	if (n_total < config$min_n || n_event < config$min_events || (n_total - n_event) < config$min_events) {
        stop(
            "Insufficient observations for party stage '", party_name, "': n=",
            n_total, ", events=", n_event, ". No fallback model is available."
        )
	}

	formula_full <- if (use_interactions) {
	y ~ v_s(cong_share) + v_s(pres_share) +
		(1 | state_abbrv) +
		(1 | state_cd) +
		(1 | race) +
		(1 | gender) +
		(1 | age_group) +
		(1 | education_level) +
		(1 | past_vote) +
		(1 | race_gender) +
		(1 | race_age) +
		(1 | race_edu) +
		(1 | race_past_vote) +
		(1 | age_gender) +
		(1 | gender_edu) +
		(1 | gender_past_vote) +
		(1 | age_edu) +
		(1 | age_past_vote) +
		(1 | edu_past_vote)
	} else {
	y ~ v_s(cong_share) + v_s(pres_share) +
		(1 | state_abbrv) +
		(1 | state_cd) +
		(1 | race) +
		(1 | gender) +
		(1 | age_group) +
		(1 | education_level) +
		(1 | past_vote)
	}

	fit_full <- tryCatch(
		vglmer(
			formula_full,
			data = d,
			family = "binomial",
			control = vglmer_control(iterations = 1500)
		),
		error = function(e) {
			config$msg("fit_full failed for ", party_name, ". Error: ", conditionMessage(e))
			NULL
		}
	)

	if (!is.null(fit_full)) {
		return(list(
			fit = fit_full,
			fallback_prob = fallback_prob,
			n_total = n_total,
			n_event = n_event,
			status = "fit_ok_full",
            race_gender_levels = levels(d$race_gender),
            race_age_levels = levels(d$race_age),
            race_edu_levels = levels(d$race_edu),
            race_past_vote_levels = levels(d$race_past_vote),
            age_gender_levels = levels(d$age_gender),
            age_edu_levels = levels(d$age_edu),
            age_past_vote_levels = levels(d$age_past_vote),
            gender_edu_levels = levels(d$gender_edu),
            gender_past_vote_levels = levels(d$gender_past_vote),
            edu_past_vote_levels = levels(d$edu_past_vote)
		))
	}

    stop(
        "Full hierarchical vglmer fit failed for party stage '",
        party_name, "'. No fallback model is available."
    )
}

restore_us_post_predictor_levels <- function(dat, master_levels) {
    for (v in names(master_levels)) {
        dat[[v]] <- factor(as.character(dat[[v]]), levels = master_levels[[v]])
    }
    dat
}

restore_us_post_interaction_levels <- function(dat, interaction_levels) {
    n_rows <- nrow(dat)
    for (v in names(interaction_levels)) {
        lv <- interaction_levels[[v]]
        if (is.null(lv) || length(lv) == 0) {
            stop("Interaction level set for '", v, "' is NULL or empty.")
        }
        values <- as.character(dat[[v]])
        if (length(values) != n_rows) {
            stop("Interaction variable '", v, "' has length ", length(values), " but data has ", n_rows, " rows.")
        }
        dat[[v]] <- factor(values, levels = lv)
    }
    dat
}

fit_us_post_stickbreaking_models <- function(
    survey_model,
    parties,
    area_shares,
    config,
    master_levels,
    interaction_levels
) {
    sb_fits <- vector("list", length(parties) - 1)
    names(sb_fits) <- parties[seq_len(length(parties) - 1)]
    remaining_parties <- parties

    for (k in seq_len(length(parties) - 1)) {
        current_party <- remaining_parties[[1]]

        d_k_base <- survey_model %>%
            filter(vote_2026 %in% remaining_parties) %>%
            mutate(
                vote_2026 = factor(
                    as.character(vote_2026),
                    levels = remaining_parties
                )
            )

        d_k_base <- restore_us_post_predictor_levels(
            d_k_base,
            master_levels
        )

        d_k <- make_us_post_stage_data(
            data = d_k_base,
            party_name = current_party,
            area_shares = area_shares,
            config = config,
            interaction_levels = interaction_levels
        )

        d_k <- restore_us_post_predictor_levels(
            d_k,
            master_levels
        )

        d_k <- restore_us_post_interaction_levels(
            d_k,
            interaction_levels
        )

        stage_fit <- fit_us_post_stage(
            d_k,
            current_party,
            config
        )

        sb_fits[[k]] <- stage_fit

        config$msg(
            "Stage ", k, "/", length(parties) - 1,
            " [", current_party, "] : ",
            stage_fit$status,
            " | n=", stage_fit$n_total,
            " | events=", stage_fit$n_event
        )

        remaining_parties <- remaining_parties[-1]
    }

    sb_fits
}

predict_us_post_stage_point <- function(stage_obj, newdata, config) {
    if (is.null(stage_obj$fit)) stop("No fitted model available for prediction stage: ", stage_obj$status)

    n_sims <- 150

    eta_draws <- tryCatch(
        predict_MAVB(
            stage_obj$fit,
            newdata = newdata,
            samples = n_sims,
            summary = FALSE,
            allow_missing_levels = TRUE
        ),
        error = function(e) {
            config$msg("Point prediction failed; reverting to fallback: ", conditionMessage(e))
            e
        }
    )

    if (inherits(eta_draws, "error")) {
        stop("Point prediction failed: ", conditionMessage(eta_draws))
    }

    eta_draws <- as.matrix(eta_draws)

    if (nrow(eta_draws) == n_sims && ncol(eta_draws) == nrow(newdata)) {
        eta_draws <- t(eta_draws)
    } else if (!(nrow(eta_draws) == nrow(newdata) && ncol(eta_draws) == n_sims)) {
        stop(
            "Unexpected dimensions from predict_MAVB(): got ",
            nrow(eta_draws), " x ", ncol(eta_draws),
            ", expected either ", nrow(newdata), " x ", n_sims,
            " or ", n_sims, " x ", nrow(newdata), "."
        )
    }

    probability_draws <- plogis(eta_draws)
    point_probability <- rowMeans(probability_draws)

    pmin(pmax(point_probability, config$eps), 1 - config$eps)
}

predict_us_post_stage_draws <- function(stage_obj, newdata, config) {
	if (is.null(stage_obj$fit)) stop("No fitted model available for simulation stage: ", stage_obj$status)

	eta_draws <- tryCatch(
		predict_MAVB(stage_obj$fit, newdata = newdata, samples = config$n_sims, summary = FALSE, allow_missing_levels = TRUE),
		error = function(e) e
	)

	if (inherits(eta_draws, "error")) {
		config$msg("Simulation prediction failed; reverting to fallback: ", conditionMessage(eta_draws))
		stop("Simulation prediction failed: ", conditionMessage(eta_draws))
	}

	eta_draws <- as.matrix(eta_draws)
	if (nrow(eta_draws) == config$n_sims && ncol(eta_draws) == nrow(newdata)) {
		eta_draws <- t(eta_draws)
	}

	pmin(pmax(plogis(eta_draws), config$eps), 1 - config$eps)
}

normalize_us_post_probability_matrix <- function(prob_mat) {
    if (!is.matrix(prob_mat)) prob_mat <- as.matrix(prob_mat)
    if (any(!is.finite(prob_mat))) stop("Probability matrix contains non-finite values.")
    if (any(prob_mat < 0)) stop("Probability matrix contains negative values.")
    row_sums <- rowSums(prob_mat)
    bad_rows <- which(!is.finite(row_sums) | row_sums <= 0)
    if (length(bad_rows) > 0) {
        stop("Invalid probability rows: ", paste(head(bad_rows, 20), collapse = ", "))
    }
    prob_mat / row_sums
}

build_us_post_probability_matrix <- function(pi_components, parties, ballot_mask = NULL, stay_home = "Did not vote") {
	n_rows <- nrow(pi_components[[1]])
	n_parties <- length(parties)
	prob_mat <- matrix(0, nrow = n_rows, ncol = n_parties)
	colnames(prob_mat) <- parties
	remaining_mass <- rep(1, n_rows)

	for (k in seq_len(n_parties - 1)) {
		prob_mat[, k] <- remaining_mass * pi_components[[k]]
		remaining_mass <- remaining_mass * (1 - pi_components[[k]])
	}

	prob_mat[, n_parties] <- remaining_mass
	apply_us_post_ballot_mask(
		normalize_us_post_probability_matrix(prob_mat), ballot_mask, stay_home
	)
}

compute_us_post_national_point_estimates <- function(prob_mat, weights) {
	as_tibble(prob_mat) %>%
		mutate(weight = weights) %>%
		summarise(across(-weight, ~ weighted.mean(.x, w = weight, na.rm = TRUE))) %>%
		pivot_longer(everything(), names_to = "vote_2026", values_to = "point_estimate") %>%
		arrange(desc(point_estimate))
}

compute_us_post_share_draws <- function(pi_draws, parties, weights, ballot_mask = NULL) {
	share_draws <- matrix(NA_real_, nrow = ncol(pi_draws[[1]]), ncol = length(parties))
	colnames(share_draws) <- parties

	for (s in seq_len(ncol(pi_draws[[1]]))) {
		pi_components_s <- lapply(pi_draws, function(draw_mat) draw_mat[, s, drop = FALSE])
		prob_mat_s <- build_us_post_probability_matrix(pi_components_s, parties, ballot_mask)
		share_draws[s, ] <- apply(prob_mat_s, 2, weighted.mean, w = weights, na.rm = TRUE)
	}
	share_draws
}

build_us_post_share_draws_ci <- function(share_draws, parties, mrp_estimates) {
	tibble(
		vote_2026 = parties,
		lower_95 = apply(share_draws, 2, quantile, probs = 0.025, na.rm = TRUE),
		median = apply(share_draws, 2, quantile, probs = 0.50, na.rm = TRUE),
		upper_95 = apply(share_draws, 2, quantile, probs = 0.975, na.rm = TRUE)
	) %>%
		left_join(mrp_estimates, by = "vote_2026") %>%
		select(vote_2026, point_estimate, lower_95, median, upper_95) %>%
		arrange(desc(point_estimate))
}

build_us_post_extended_frame <- function(prob_mat, frame_pred) {
	as_tibble(prob_mat) %>%
		mutate(cell_id = seq_len(nrow(frame_pred))) %>%
		pivot_longer(cols = -cell_id, names_to = "vote_2026", values_to = "cell_probability") %>%
		left_join(frame_pred %>% mutate(cell_id = seq_len(n())), by = "cell_id") %>%
		mutate(expected_N = expected_N_raked * cell_probability) %>%
		select(
			cell_id, age_group, gender, race, state_abbrv, state_cd, 
			education_level, expected_N_raked, vote_2026, 
			prob = cell_probability, expected_N
		)
}

compute_us_post_stage_diagnostics <- function(sb_fits, parties) {
	tibble(
		stage = seq_len(length(parties) - 1),
		vote_2026 = parties[seq_len(length(parties) - 1)],
		status = map_chr(sb_fits, "status"),
		n_total = map_dbl(sb_fits, "n_total"),
		n_event = map_dbl(sb_fits, "n_event"),
		fallback_prob = map_dbl(sb_fits, "fallback_prob")
	)
}

compute_us_post_aggregate_counts <- function(extended_frame) {
	extended_frame %>%
		group_by(vote_2026) %>%
		summarise(
			expected_total = sum(expected_N, na.rm = TRUE),
			expected_share = sum(expected_N, na.rm = TRUE) / sum(expected_N_raked[!duplicated(cell_id)]),
			.groups = "drop"
		) %>%
		arrange(desc(expected_share))
}

compute_us_post_cd_point <- function(prob_mat, frame_pred, parties) {
	valid_rows <- !is.na(frame_pred$state_cd)
	as_tibble(prob_mat[valid_rows, , drop = FALSE]) %>%
		mutate(
			state_cd = as.character(frame_pred$state_cd[valid_rows]),
			expected_N_raked = frame_pred$expected_N_raked[valid_rows]
		) %>%
		pivot_longer(cols = all_of(parties), names_to = "vote_2026", values_to = "prob") %>%
		mutate(expected_N = expected_N_raked * prob) %>%
		group_by(state_cd, vote_2026) %>%
		summarise(
			expected_N = sum(expected_N, na.rm = TRUE),
			total_N = sum(expected_N_raked, na.rm = TRUE),
			point_estimate = expected_N / total_N,
			.groups = "drop"
		)
}

compute_us_post_cd_draws <- function(pi_draws, frame_pred, parties, ballot_mask = NULL) {
	cd_col <- intersect(c("state_cd", "cd", "CD", "district_id", "district"), colnames(frame_pred))[1]
	if (is.na(cd_col)) cd_col <- colnames(frame_pred)[grepl("cd|district", colnames(frame_pred), ignore.case = TRUE)][1]
	if (is.na(cd_col) || !cd_col %in% colnames(frame_pred)) stop("Error: Could not find Congressional District column.")

	test_mat <- pi_draws[[1]]
	n_frame <- nrow(frame_pred)
	if (nrow(test_mat) == n_frame) { n_sims <- ncol(test_mat); transposed <- FALSE }
	else { n_sims <- nrow(test_mat); transposed <- TRUE }

	valid_rows <- !is.na(frame_pred[[cd_col]])

	draws_list <- lapply(seq_len(n_sims), function(s) {
		pi_components_s <- lapply(pi_draws, function(draw_mat) {
			if (transposed) matrix(draw_mat[s, valid_rows], ncol = 1) else matrix(draw_mat[valid_rows, s], ncol = 1)
		})
		prob_mat_s <- build_us_post_probability_matrix(
			pi_components_s, parties,
			if (is.null(ballot_mask)) NULL else ballot_mask[valid_rows, , drop = FALSE]
		)
		
		prob_df <- as_tibble(prob_mat_s)
		prob_df[[cd_col]] <- as.character(frame_pred[[cd_col]][valid_rows])
		prob_df$expected_N_raked <- frame_pred$expected_N_raked[valid_rows]
		prob_df$draw_id <- s

		prob_df %>%
			pivot_longer(cols = all_of(parties), names_to = "vote_2026", values_to = "prob") %>%
			mutate(expected_N = expected_N_raked * prob) %>%
			group_by(.data[[cd_col]], vote_2026, draw_id) %>%
			summarise(draw_share = sum(expected_N, na.rm = TRUE) / sum(expected_N_raked, na.rm = TRUE), .groups = "drop")
	})
	bind_rows(draws_list)
}

compute_us_post_cd_ci <- function(cd_party_draws, cd_party_point) {
	cd_party_draws %>%
		group_by(state_cd, vote_2026) %>%
		summarise(
			lower_95 = quantile(draw_share, probs = 0.025, na.rm = TRUE),
			median = quantile(draw_share, probs = 0.50, na.rm = TRUE),
			upper_95 = quantile(draw_share, probs = 0.975, na.rm = TRUE),
			sd_draws = sd(draw_share, na.rm = TRUE),
			n_distinct_draws = n_distinct(draw_share),
			.groups = "drop"
		) %>%
		left_join(cd_party_point, by = c("state_cd", "vote_2026")) %>%
		select(state_cd, vote_2026, point_estimate, lower_95, median, upper_95, sd_draws, n_distinct_draws) %>%
		arrange(state_cd, desc(point_estimate))
}

build_us_post_share_draws_long <- function(share_draws) {
	as_tibble(share_draws) %>%
		mutate(draw = seq_len(n())) %>%
		pivot_longer(cols = -draw, names_to = "vote_2026", values_to = "share")
}

# --- 1-WAY & 2-WAY MARGINAL AGGREGATIONS (WITH D-R MARGINS & 95% CIs) ---

compute_us_post_margins <- function(pi_draws, prob_mat, frame_pred, parties, config, ballot_mask = NULL) {
	margins_1way <- list("age_group", "education_level", "race", "gender", "past_vote")
	margins_2way <- combn(c("age_group", "education_level", "race", "gender", "past_vote"), 2, simplify = FALSE)
	all_margins <- c(margins_1way, margins_2way)
	
	dnv_party <- "Did not vote"
	has_dnv <- dnv_party %in% parties
	
	dem_p <- config$dem_party_name
	rep_p <- config$rep_party_name
	has_dr <- (dem_p %in% parties) && (rep_p %in% parties)
	
	test_mat <- pi_draws[[1]]
	n_frame <- nrow(frame_pred)
	if (nrow(test_mat) == n_frame) { n_sims <- ncol(test_mat); transposed <- FALSE }
	else { n_sims <- nrow(test_mat); transposed <- TRUE }
	
	draw_aggregations <- setNames(lapply(all_margins, function(x) vector("list", n_sims)), 
								  sapply(all_margins, paste, collapse = "_"))
	
	# Loop through each MAVB simulation draw to build exact joint distributions
	for (s in seq_len(n_sims)) {
		pi_s <- lapply(pi_draws, function(m) if (transposed) matrix(m[s, ], ncol = 1) else matrix(m[, s], ncol = 1))
		prob_s <- build_us_post_probability_matrix(pi_s, parties, ballot_mask)
		
		df_s <- as_tibble(prob_s)
		df_s$expected_N_raked <- frame_pred$expected_N_raked
		for (v in c("age_group", "education_level", "race", "gender", "past_vote")) df_s[[v]] <- frame_pred[[v]]
		
		for (margin_vars in all_margins) {
			margin_name <- paste(margin_vars, collapse = "_")
			
			# Party level weighted share per draw
			agg_wide <- df_s %>%
				group_by(across(all_of(margin_vars))) %>%
				summarise(across(all_of(parties), ~ weighted.mean(.x, w = expected_N_raked, na.rm = TRUE)), .groups = "drop")
			
			agg_long <- agg_wide %>%
				pivot_longer(cols = all_of(parties), names_to = "vote_2026", values_to = "share")
			
			if (has_dnv) {
				cond_agg <- agg_long %>% filter(vote_2026 != dnv_party) %>%
					group_by(across(all_of(margin_vars))) %>%
					mutate(cond_share = share / sum(share, na.rm = TRUE)) %>% ungroup()
				agg_long <- agg_long %>% left_join(cond_agg %>% select(all_of(margin_vars), vote_2026, cond_share), by = c(margin_vars, "vote_2026"))
			} else {
				agg_long$cond_share <- agg_long$share
			}
			
			# Compute Draw-Level D-R Margin
			if (has_dr) {
				dr_rows <- agg_wide %>%
					group_by(across(all_of(margin_vars))) %>%
					summarise(
						vote_2026 = "D-R Margin",
						share = .data[[dem_p]] - .data[[rep_p]],
						.groups = "drop"
					)
				
				if (has_dnv) {
					voter_parties <- setdiff(parties, dnv_party)
					dr_cond_rows <- agg_wide %>%
						mutate(voter_sum = rowSums(across(all_of(voter_parties)))) %>%
						group_by(across(all_of(margin_vars))) %>%
						summarise(
							vote_2026 = "D-R Margin",
							cond_share = (.data[[dem_p]] / voter_sum) - (.data[[rep_p]] / voter_sum),
							.groups = "drop"
						)
					dr_rows <- dr_rows %>% left_join(dr_cond_rows, by = c(margin_vars, "vote_2026"))
				} else {
					dr_rows$cond_share <- dr_rows$share
				}
				
				agg_long <- bind_rows(agg_long, dr_rows)
			}
			
			agg_long$draw_id <- s
			draw_aggregations[[margin_name]][[s]] <- agg_long
		}
	}
	
	results <- list()
	df_point <- as_tibble(prob_mat)
	df_point$expected_N_raked <- frame_pred$expected_N_raked
	for (v in c("age_group", "education_level", "race", "gender", "past_vote")) df_point[[v]] <- frame_pred[[v]]
	
	for (margin_vars in all_margins) {
		margin_name <- paste(margin_vars, collapse = "_")
		
		# Point Estimates
		pt_wide <- df_point %>%
			group_by(across(all_of(margin_vars))) %>%
			summarise(across(all_of(parties), ~ weighted.mean(.x, w = expected_N_raked, na.rm = TRUE)), .groups = "drop")
		
		pt_sum <- pt_wide %>%
			pivot_longer(cols = all_of(parties), names_to = "vote_2026", values_to = "point_share")
		
		if (has_dnv) {
			pt_cond <- pt_sum %>% filter(vote_2026 != dnv_party) %>%
				group_by(across(all_of(margin_vars))) %>%
				mutate(point_cond_share = point_share / sum(point_share, na.rm = TRUE)) %>% ungroup()
			pt_sum <- pt_sum %>% left_join(pt_cond %>% select(all_of(margin_vars), vote_2026, point_cond_share), by = c(margin_vars, "vote_2026"))
		} else {
			pt_sum$point_cond_share <- pt_sum$point_share
		}
		
		if (has_dr) {
			dr_pt_row <- pt_wide %>%
				group_by(across(all_of(margin_vars))) %>%
				summarise(
					vote_2026 = "D-R Margin",
					point_share = .data[[dem_p]] - .data[[rep_p]],
					.groups = "drop"
				)
			if (has_dnv) {
				voter_parties <- setdiff(parties, dnv_party)
				dr_cond_pt_row <- pt_wide %>%
					mutate(voter_sum = rowSums(across(all_of(voter_parties)))) %>%
					group_by(across(all_of(margin_vars))) %>%
					summarise(
						vote_2026 = "D-R Margin",
						point_cond_share = (.data[[dem_p]] / voter_sum) - (.data[[rep_p]] / voter_sum),
						.groups = "drop"
					)
				dr_pt_row <- dr_pt_row %>% left_join(dr_cond_pt_row, by = c(margin_vars, "vote_2026"))
			} else {
				dr_pt_row$point_cond_share <- dr_pt_row$point_share
			}
			pt_sum <- bind_rows(pt_sum, dr_pt_row)
		}
		
		# Compute 95% Confidence Intervals (2.5% and 97.5% quantiles)
		all_draws <- bind_rows(draw_aggregations[[margin_name]])
		ci_sum <- all_draws %>%
			group_by(across(all_of(margin_vars)), vote_2026) %>%
			summarise(
				lower_95 = quantile(share, 0.025, na.rm = TRUE),
				median = quantile(share, 0.50, na.rm = TRUE),
				upper_95 = quantile(share, 0.975, na.rm = TRUE),
				cond_lower_95 = quantile(cond_share, 0.025, na.rm = TRUE),
				cond_median = quantile(cond_share, 0.50, na.rm = TRUE),
				cond_upper_95 = quantile(cond_share, 0.975, na.rm = TRUE),
				.groups = "drop"
			)
		
		results[[margin_name]] <- pt_sum %>% left_join(ci_sum, by = c(margin_vars, "vote_2026"))
	}
	results
}

build_cell_draws_wide <- function(pi_draws, frame_pred, parties, ballot_mask = NULL) {
	test_mat <- pi_draws[[1]]
	n_frame <- nrow(frame_pred)
	if (nrow(test_mat) == n_frame) { n_sims <- ncol(test_mat); transposed <- FALSE }
	else { n_sims <- nrow(test_mat); transposed <- TRUE }
	
	party_draws <- setNames(lapply(parties, function(x) vector("list", n_sims)), parties)
	
	for (s in seq_len(n_sims)) {
		pi_s <- lapply(pi_draws, function(m) if (transposed) matrix(m[s, ], ncol = 1) else matrix(m[, s], ncol = 1))
		prob_s <- build_us_post_probability_matrix(pi_s, parties, ballot_mask)
		for (p in parties) {
			party_draws[[p]][[s]] <- prob_s[, p]
		}
	}
	
	res <- list()
	for (p in parties) {
		df <- as.data.frame(party_draws[[p]])
		colnames(df) <- paste0("draw_", seq_len(n_sims))
		df <- as_tibble(df)
		df$cell_id <- seq_len(n_frame)
		df$vote_2026 <- p
		res[[p]] <- df %>% select(cell_id, vote_2026, starts_with("draw_"))
	}
	bind_rows(res)
}

# --- BALLOT ACCESS: WHO ACTUALLY RAN IN EACH DISTRICT ---

normalize_us_post_district_key <- function(x) {
	x <- toupper(trimws(as.character(x)))
	x <- sub("-AT-LARGE$", "-1", x)
	x <- sub("^([A-Z]{2})-0+([0-9]+)$", "\\1-\\2", x)
	x
}

us_post_has_candidates <- function(x) {
	!is.na(x) & grepl("[A-Za-z]", as.character(x))
}

us_post_extract_third_party_parties <- function(x) {
	x <- as.character(x)
	x[is.na(x)] <- "[]"
	regmatches(x, gregexpr("[\"']party[\"']\\s*:\\s*[\"']([^\"']*)[\"']", x, perl = TRUE)) %>%
		lapply(function(m) sub("^[\"']party[\"']\\s*:\\s*[\"']([^\"']*)[\"']$", "\\1", m, perl = TRUE))
}

prepare_us_post_candidates <- function(candidates, config) {
	candidates <- drop_us_post_index_columns(candidates)
	validate_us_post_required_columns(
		candidates,
		c("District", "Democratic_Candidates", "Republican_Candidates", "Independent_ThirdParty"),
		"candidates"
	)

	dem_aligned_pattern <- config$dem_aligned_party_pattern
	tp_parties <- us_post_extract_third_party_parties(candidates$Independent_ThirdParty)
	tp_has_dem_label <- vapply(tp_parties, function(p) any(grepl(dem_aligned_pattern, p, ignore.case = TRUE)), logical(1))
	tp_has_other <- vapply(tp_parties, function(p) any(!grepl(dem_aligned_pattern, p, ignore.case = TRUE)), logical(1))

	out <- tibble(
		state_cd_key = normalize_us_post_district_key(candidates$District),
		dem_runs = us_post_has_candidates(candidates$Democratic_Candidates) | tp_has_dem_label,
		rep_runs = us_post_has_candidates(candidates$Republican_Candidates),
		other_runs = tp_has_other
	) %>%
		filter(grepl("^[A-Z]{2}-[0-9]+$", state_cd_key)) %>%
		group_by(state_cd_key) %>%
		summarise(
			dem_runs = any(dem_runs),
			rep_runs = any(rep_runs),
			other_runs = any(other_runs),
			.groups = "drop"
		)

	empty_ballots <- out %>% filter(!dem_runs & !rep_runs & !other_runs)
	if (nrow(empty_ballots) > 0) {
		config$msg("Districts with no parsed candidates (treated as all parties running): ",
			paste(empty_ballots$state_cd_key, collapse = ", "))
		out <- out %>%
			mutate(
				all_empty = !dem_runs & !rep_runs & !other_runs,
				dem_runs = dem_runs | all_empty,
				rep_runs = rep_runs | all_empty,
				other_runs = other_runs | all_empty
			) %>%
			select(-all_empty)
	}
	out
}

build_us_post_ballot_mask <- function(frame_pred, ballot_table, parties, config) {
	frame_key <- normalize_us_post_district_key(frame_pred$state_cd)
	idx <- match(frame_key, ballot_table$state_cd_key)

	missing_keys <- sort(unique(frame_key[is.na(idx)]))
	if (length(missing_keys) > 0) {
		config$msg("Districts in frame but not in candidates file (all parties kept): ",
			paste(missing_keys, collapse = ", "))
	}

	runs <- list()
	runs[[config$dem_party_name]] <- ballot_table$dem_runs[idx]
	runs[[config$rep_party_name]] <- ballot_table$rep_runs[idx]
	runs[[config$other_party_name]] <- ballot_table$other_runs[idx]

	mask <- matrix(1, nrow = nrow(frame_pred), ncol = length(parties))
	colnames(mask) <- parties
	for (p in intersect(names(runs), parties)) {
		r <- runs[[p]]
		r[is.na(r)] <- TRUE
		mask[, p] <- as.numeric(r)
	}
	mask[, colnames(mask) %in% config$stay_home_party_name] <- 1
	mask
}

apply_us_post_ballot_mask <- function(prob_mat, ballot_mask, stay_home = "Did not vote") {
	if (is.null(ballot_mask)) return(prob_mat)
	if (!identical(dim(ballot_mask), dim(prob_mat))) {
		stop("ballot_mask dimensions (", paste(dim(ballot_mask), collapse = "x"),
			") do not match probability matrix (", paste(dim(prob_mat), collapse = "x"), ").")
	}
	ballot_mask <- ballot_mask[, colnames(prob_mat), drop = FALSE]

	fixed_cols <- colnames(prob_mat) %in% stay_home
	free_cols <- !fixed_cols
	fixed_mass <- if (any(fixed_cols)) rowSums(prob_mat[, fixed_cols, drop = FALSE]) else rep(0, nrow(prob_mat))
	voter_mass <- 1 - fixed_mass

	runs <- ballot_mask[, free_cols, drop = FALSE] > 0
	raw <- prob_mat[, free_cols, drop = FALSE] * runs
	raw_sum <- rowSums(raw)
	n_running <- rowSums(runs)

	if (any(n_running == 0)) stop("At least one frame row has no party on the ballot after masking.")

	weights <- raw / ifelse(raw_sum > 0, raw_sum, 1)
	no_mass <- raw_sum <= 0
	if (any(no_mass)) weights[no_mass, ] <- runs[no_mass, , drop = FALSE] / n_running[no_mass]

	out <- prob_mat
	out[, free_cols] <- weights * voter_mass
	out
}

check_us_post_ballot_mask <- function(prob_mat, ballot_mask, config) {
	if (is.null(ballot_mask)) return(invisible(NULL))
	leak <- prob_mat * (1 - ballot_mask[, colnames(prob_mat), drop = FALSE])
	config$msg("Ballot mask check | max prob on non-running party: ", signif(max(leak), 3),
		" | max |rowSum-1|: ", signif(max(abs(rowSums(prob_mat) - 1)), 3))
	invisible(NULL)
}

# --- MAIN POST-STRATIFICATION RUNNER ---

run_post_stratification <- function(survey, frame, area_level_vote_shares, config = list(), candidates = NULL) {
	config <- resolve_us_post_strat_config(config)
	metadata_config <- config
	metadata_config$msg <- NULL

	if (!is.null(config$seed)) set.seed(config$seed)

	area_shares <- prepare_us_post_area_shares(area_level_vote_shares, config)
	survey_model <- prepare_us_post_survey_data(survey, config)
	frame_alignment <- prepare_us_post_frame_data(frame, survey_model, config)
	survey_model <- frame_alignment$survey_model
	frame_pred <- frame_alignment$frame_pred
	master_levels <- frame_alignment$master_levels
	frame_only_levels <- frame_alignment$frame_only_levels
	interaction_levels <- get_us_post_interaction_levels(survey_model, frame_pred)
	parties <- levels(survey_model$vote_2026)

	sb_fits <- fit_us_post_stickbreaking_models(
		survey_model,
		parties,
		area_shares,
		config,
		master_levels,
		interaction_levels
	)

	pi_mat <- matrix(NA_real_, nrow = nrow(frame_pred), ncol = length(parties) - 1)
	colnames(pi_mat) <- parties[seq_len(length(parties) - 1)]

	for (k in seq_len(length(parties) - 1)) {
		stage_newdata <- make_us_post_stage_data(data = frame_pred, party_name = parties[[k]], area_shares = area_shares, config = config, stage_obj = sb_fits[[k]], interaction_levels = interaction_levels)
		pi_mat[, k] <- predict_us_post_stage_point(sb_fits[[k]], stage_newdata, config)
	}

	pi_components <- lapply(seq_len(ncol(pi_mat)), function(k) pi_mat[, k, drop = FALSE])
	ballot_mask <- NULL
	ballot_table <- NULL
	if (!is.null(candidates)) {
		ballot_table <- prepare_us_post_candidates(candidates, config)
		ballot_mask <- build_us_post_ballot_mask(frame_pred, ballot_table, parties, config)
	}
	prob_mat <- build_us_post_probability_matrix(pi_components, parties, ballot_mask, config$stay_home_party_name)
	check_us_post_ballot_mask(prob_mat, ballot_mask, config)
	weights <- frame_pred$expected_N_raked
	mrp_estimates <- compute_us_post_national_point_estimates(prob_mat, weights)

	pi_draws <- vector("list", length(parties) - 1)

	for (k in seq_len(length(parties) - 1)) {
		config$msg("Simulation draws for stage ", k, "/", length(parties) - 1, " [", parties[[k]], "]")
		stage_newdata <- make_us_post_stage_data(data = frame_pred, party_name = parties[[k]], area_shares = area_shares, config = config, stage_obj = sb_fits[[k]], interaction_levels = interaction_levels)
		pi_draws[[k]] <- predict_us_post_stage_draws(sb_fits[[k]], stage_newdata, config)

		rm(stage_newdata)
		gc()
	}

	share_draws <- compute_us_post_share_draws(pi_draws, parties, weights, ballot_mask)
	share_draws_ci <- build_us_post_share_draws_ci(share_draws, parties, mrp_estimates)
	extended_frame <- build_us_post_extended_frame(prob_mat, frame_pred)
	stage_diagnostics <- compute_us_post_stage_diagnostics(sb_fits, parties)
	aggregate_counts <- compute_us_post_aggregate_counts(extended_frame)
	cd_party_point <- compute_us_post_cd_point(prob_mat, frame_pred, parties)
	cd_party_draws <- compute_us_post_cd_draws(pi_draws, frame_pred, parties, ballot_mask)
	cd_party_ci <- compute_us_post_cd_ci(cd_party_draws, cd_party_point)
	share_draws_long <- build_us_post_share_draws_long(share_draws)
	
	config$msg("Computing 1-way and 2-way marginal shares & D-R margins with 95% CIs...")
	margin_summaries <- compute_us_post_margins(pi_draws, prob_mat, frame_pred, parties, config, ballot_mask)
	
	cell_draws <- NULL
	if (isTRUE(config$export_cell_draws)) {
		config$msg("Exporting cell-level draws...")
		cell_draws <- build_cell_draws_wide(pi_draws, frame_pred, parties, ballot_mask)
	}

	list(
		point_estimates = mrp_estimates,
		national_summary_95ci = share_draws_ci,
		extended_frame = extended_frame,
		stage_diagnostics = stage_diagnostics,
		aggregate_counts = aggregate_counts,
		share_draws = as_tibble(share_draws),
		share_draws_long = share_draws_long,
		cell_party_probabilities = as_tibble(prob_mat),
		stickbreaking_conditional_probs = as_tibble(pi_mat),
		cd_party_point = cd_party_point,
		cd_party_draws = cd_party_draws,
		cd_party_ci = cd_party_ci,
		margin_summaries = margin_summaries,
		cell_draws = cell_draws,
		metadata = list(parties = parties, config = metadata_config, master_levels = master_levels, frame_only_levels = frame_only_levels, ballot_table = ballot_table)
	)
}

write_post_strat_outputs <- function(result, output_dir) {
	if (!dir.exists(output_dir)) {
		dir.create(output_dir, recursive = TRUE)
	}

	write_csv(result$point_estimates, file.path(output_dir, "mrp_point_estimates.csv"))
	write_csv(result$national_summary_95ci, file.path(output_dir, "mrp_national_summary_95ci.csv"))
	write_csv(result$extended_frame, file.path(output_dir, "mrp_extended_frame_predictions.csv"))
	write_csv(result$stage_diagnostics, file.path(output_dir, "mrp_stage_diagnostics.csv"))
	write_csv(result$aggregate_counts, file.path(output_dir, "mrp_aggregate_counts.csv"))
	write_csv(result$share_draws, file.path(output_dir, "mrp_share_draws.csv"))
	write_csv(result$share_draws_long, file.path(output_dir, "mrp_share_draws_long.csv"))
	write_csv(result$cell_party_probabilities, file.path(output_dir, "mrp_cell_party_probabilities.csv"))
	write_csv(result$stickbreaking_conditional_probs, file.path(output_dir, "mrp_stickbreaking_conditional_probs.csv"))
	write_csv(result$cd_party_point, file.path(output_dir, "mrp_cd_party_point_estimates.csv"))
	write_csv(result$cd_party_draws, file.path(output_dir, "mrp_cd_party_draws_long.csv"))
	write_csv(result$cd_party_ci, file.path(output_dir, "mrp_cd_party_95ci.csv"))

	if (!is.null(result$margin_summaries)) {
		for (margin_name in names(result$margin_summaries)) {
			write_csv(
				result$margin_summaries[[margin_name]], 
				file.path(output_dir, paste0("mrp_margin_", margin_name, ".csv"))
			)
		}
	}

	# if (!is.null(result$cell_draws)) {
	# 	write_csv(result$cell_draws, file.path(output_dir, "mrp_cell_draws.csv"))
	# }

	invisible(result)
}