# Model functions for the Recovery Zone Options dashboard.
#
# Hypothetical illustration only. Population trajectories are digitised from the published fine-scale model of
# spangled emperor at Ningaloo (Aston et al. 2025, Figs 4, 6 and 7a). Egg production applies the per-recruit
# reproductive biology of a demersal fish whose reproductive output scales hyperallometrically with body mass
# (Evans-Powell et al. 2024). Not a stock projection or management advice.
#
# Scenarios in Aston et al. (2025): S1 recovery zones only, S2 no closures, S3 spawning closures only,
# S4 recovery zones + spawning closures. Model years 2005-2018 are shown as 2027-2040.

# Shiny sources R/ before app.R, so this file loads its own packages
library(dplyr)
library(readr)
library(purrr)

# ---- Constants ---------------------------------------------------------------------------------------

SB0      <- 885.7  # unfished spawning biomass implied by the Fig. 4 right axis (t)
A0       <- 0.34   # recovery-zone share modelled by Aston et al. (2025)
CL0      <- 5      # modelled spawning closure length (months)
STEEP    <- 0.75   # Beverton-Holt steepness
START_SB <- 0.15   # common start: a depleted stock at 15% of unfished spawning biomass
NTZ0     <- 34     # recovery-zone slider default (%)
NTZ_MAX  <- 60     # recovery-zone slider ceiling (%)
SCEN     <- c("s1", "s2", "s3", "s4")
YEARS    <- 2027:2040

CLOSE_MONTHS <- c(`3` = "Dec–Feb", `4` = "Dec–Mar", `5` = "Nov–Mar", `6` = "Nov–Apr",
                  `7` = "Oct–Apr", `8` = "Oct–May", `9` = "Sep–May")

# Life-history parameters (Evans-Powell et al. 2024)
SPP <- list(Linf = 929, k = 0.11, t0 = -0.141, M = 0.11, T = 50, aW = 4.17e-5, bW = 2.859, A50 = 3.54, A95 = 4.77,
            AC50 = 3.55, AC95 = 4.78, AR50 = 7.4, AR95 = 8.89, delta = 0.5, aBF = 0.0841, bBF = -10.432,
            k1 = 9.17e-5, k2 = 0.000459, phi = 1.18, ad = 0.1722, bd = 319.64, axi = 2.15, bxi = 0.77, Llarge = 700)

effort_cut <- function(L) 0.0625 * (L + 3)  # share of annual effort removed by an L-month closure (5 months = 50%)

# ---- Data --------------------------------------------------------------------------------------------

# one list per scenario (s1-s4), each holding chosen columns as vectors ordered by year
by_scenario <- function(d, cols) {
  d <- d |> mutate(scenario = tolower(scenario)) |> arrange(model_year)
  map(set_names(SCEN), \(s) filter(d, scenario == s) |> select(all_of(cols)) |> as.list())
}

load_model_data <- function(dir = "data") {
  series <- \(f) read_csv(file.path(dir, f), show_col_types = FALSE) |>
    by_scenario(c(m = "median", lo = "lower_95", hi = "upper_95"))
  dens <- read_csv(file.path(dir, "aston2025_fig6_density_inside_zones.csv"), show_col_types = FALSE) |>
    by_scenario(c(mat = "mature_per_km2", big = "large_mature_per_km2"))
  
  list(
    SB   = series("aston2025_fig4_spawning_biomass_t.csv"),  # spawning biomass (t), Fig. 4
    CPUE = series("aston2025_fig7a_cpue_0_10km.csv"),        # fish per boat day per km², 0–10 km from ramps, Fig. 7a
    MAT  = map(dens, "mat"),                                 # mature fish per km² inside recovery zones, Fig. 6c
    BIG  = map(dens, "big"),                                 # large mature fish per km² inside recovery zones, Fig. 6d
    OPTS = read_csv(file.path(dir, "rz_options.csv"), show_col_types = FALSE),
    N    = length(YEARS)
  )
}

# ---- Per-recruit table across fishing mortality (F = 0 to 1) -----------------------------------------
# share of mature females >= Llarge, mean weight, eggs (e) and egg energy (x) per mature female, and
# per-recruit totals (sbT, eT, xT)

build_pr <- function(d = SPP) {
  ages <- 0:d$T
  lg  <- \(x, a50, a95) 1 / (1 + exp(-log(19) * (x - a50) / (a95 - a50)))
  L   <- d$Linf * (1 - exp(-d$k * (ages - d$t0)))
  W   <- exp(log(d$aW) + d$bW * log(L)) / 1000
  P   <- lg(ages, d$A50, d$A95)
  cc  <- lg(ages, d$AC50, d$AC95)
  rr  <- lg(ages, d$AR50, d$AR95)
  sel <- cc * rr + d$delta * cc * (1 - rr)
  b   <- d$aBF * L + d$bBF
  AF  <- ifelse(b > 0, b^3, 0) * (d$k1 + d$k2) * L * 365 / d$phi
  v   <- (4 / 3) * pi * ((d$ad * L + d$bd) / 2)^3 / 1e9
  EE  <- d$axi * v^d$bxi * AF
  
  map_dfr(seq(0, 1, by = 0.005), \(F) {
    N <- numeric(d$T + 1); N[1] <- 0.5
    for (t in 2:(d$T + 1)) N[t] <- N[t - 1] * exp(-(d$M + F * sel[t - 1]))
    N[d$T + 1] <- N[d$T + 1] / (1 - exp(-(d$M + F * sel[d$T + 1])))
    n <- N * P
    tibble(F = F, share = sum(n[L >= d$Llarge]) / sum(n), w = sum(n * W) / sum(n), e = sum(n * AF) / sum(n),
           x = sum(n * EE) / sum(n), sbT = sum(n * W), eT = sum(n * AF), xT = sum(n * EE))
  })
}

PR <- build_pr()

# size mix with a given value of `by` ("share" of big females or mean weight "w"); both fall as F rises
size_at <- function(value, by) {
  if (value >= PR[[by]][1]) return(as.list(PR[1, ]))
  j <- which(PR[[by]] <= value)[1]
  if (is.na(j)) return(as.list(PR[nrow(PR), ]))
  a <- PR[j - 1, ]; b <- PR[j, ]
  u <- (a[[by]] - value) / (a[[by]] - b[[by]])
  out <- map(c(share = "share", w = "w", e = "e", x = "x"), \(k) a[[k]] + u * (b[[k]] - a[[k]]))
  out[[by]] <- value
  out
}

rel_kg <- function(q, key) (q[[key]] / q$w) / (PR[[key]][1] / PR$w[1])  # eggs per kg relative to unfished

# equilibrium spawning biomass ("sbT") or egg output ("eT") relative to unfished, Beverton-Holt recruitment
eq_rel <- function(F, key) {
  j <- min(nrow(PR) - 2, floor(F / 0.005)); u <- F / 0.005 - j
  at <- \(k) PR[[k]][j + 1] + u * (PR[[k]][j + 2] - PR[[k]][j + 1])
  spr <- at("sbT") / PR$sbT[1]
  R <- max((4 * STEEP * spr + STEEP - 1) / ((5 * STEEP - 1) * spr), 0)
  R * at(key) / PR[[key]][1]
}

# fishing mortality (no closures) that holds the stock at the 15% start, about 0.22
F_B <- uniroot(\(F) eq_rel(F, "sbT") - START_SB, c(0.01, 0.9), tol = 1e-12)$root

# closure effect relative to the modelled 5-month closure
closure_mult <- function(L, key) {
  b0 <- eq_rel(F_B, key)
  (eq_rel(F_B * (1 - effort_cut(L)), key) - b0) / (eq_rel(F_B * (1 - effort_cut(CL0)), key) - b0)
}

# ---- Eggs per kg (whole stock) -----------------------------------------------------------------------
# Fig. 6 counts fish only inside recovery zones. With no zones (S2, S3) it describes the whole stock and
# density x mean weight reproduces the Fig. 4 totals with a near-constant factor K. For S1 and S4, spawning
# biomass inside zones = 0.34 x K x density x mean weight; fish in fished waters are sized from catch rates.

size_mix <- function(D, k, i) size_at(D$BIG[[k]][i] / D$MAT[[k]][i], "share")
calib_k  <- function(D, i) mean(map_dbl(c("s2", "s3"), \(k) D$SB[[k]]$m[i] / (D$MAT[[k]][i] * size_mix(D, k, i)$w)))

per_kg <- function(D, id, i, key = "e") {
  qi <- size_mix(D, id, i)
  if (!id %in% c("s1", "s4")) return(rel_kg(qi, key))
  K     <- calib_k(D, i)
  sb    <- D$SB[[id]]$m[i]
  sb_in <- min(A0 * K * D$MAT[[id]][i] * qi$w, 0.95 * sb)
  d_out <- D$MAT$s3[i] * D$CPUE[[id]]$m[i] / D$CPUE$s3$m[i]
  qo    <- size_at((sb - sb_in) / ((1 - A0) * K * d_out), "w")
  f_in  <- sb_in / sb
  f_in * rel_kg(qi, key) + (1 - f_in) * rel_kg(qo, key)
}

per_kg_table <- function(D, key = "e") map(set_names(SCEN), \(id) map_dbl(seq_len(D$N), \(i) per_kg(D, id, i, key)))

# ---- Scenario lines ----------------------------------------------------------------------------------
# Bilinear combination of the four published scenarios:
#   value(t, m) = S2 + t (S1 - S2) + m (S3 - S2) + t m (S4 - S1 - S3 + S2)
# t = recovery-zone share / 0.34, m = closure multiplier. t = 0 gives spawning closures only.

combine <- function(P, t, m, cap = Inf) {
  clamp <- \(v) pmin(pmax(v, 0), cap)
  pieces <- map(c(m = "m", lo = "lo", hi = "hi"), \(b) {
    s1 <- P$s1[[b]]; s2 <- P$s2[[b]]; s3 <- P$s3[[b]]; s4 <- P$s4[[b]]
    list(cl = clamp(s2 + m * (s3 - s2)),
         rz = clamp(s2 + t * (s1 - s2) + m * (s3 - s2) + t * m * (s4 - s1 - s3 + s2)))
  })
  list(cl = map(pieces, "cl"), rz = map(pieces, "rz"))
}

# shift each line to the common 2027 value, keeping its year-to-year changes
restart <- function(p, base) map(p, \(v) v + base - p$m[1])

# egg production (% of unfished) for spawning closures only (cl) and recovery zones + closures (rz)
egg_lines <- function(D, KG, L, ntz) {
  P <- map(set_names(SCEN), \(id) map(D$SB[[id]], \(v) v * KG[[id]]))
  C <- combine(P, t = ntz / 100 / A0, m = closure_mult(L, "eT"), cap = SB0)
  base <- eq_rel(F_B, "eT") * SB0
  out <- map(C, \(p) map(restart(p, base), \(v) v / SB0 * 100))
  out$start <- base / SB0 * 100
  out
}

# catch rate within 10 km of boat ramps (fish per boat day per km²)
cpue_lines <- function(D, L, ntz) {
  C <- combine(D$CPUE, t = ntz / 100 / A0, m = closure_mult(L, "sbT"))
  base <- D$CPUE$s2$m[1]
  out <- map(C, \(p) restart(p, base))
  out$start <- base
  out
}

# fixed y-axis maximum across all slider settings, so moving a slider never rescales the axis
axis_max <- function(fun, band) {
  max(map_dbl(list(c(3, 0), c(3, NTZ_MAX), c(9, 0), c(9, NTZ_MAX)), \(s) {
    r <- fun(s[1], s[2])
    max(r$cl$m, r$rz$m, if (band) c(r$cl$hi, r$rz$hi))
  }))
}