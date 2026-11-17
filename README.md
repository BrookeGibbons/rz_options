# Recovery Zone Options: Demersal Fish Scenarios (R Shiny app)

An interactive dashboard to investigate how recovery-zone options would interact with spawning closures, for egg
production (objective a) and catch rates near boat ramps (objective b).

**Concept illustration only.** The dashboard uses a published fine-scale population model of spangled emperor
(*Lethrinus nebulosus*) at Ningaloo (Aston et al. 2025), with added estimates of impact on egg production for a
similar species whose reproductive output scales hyperallometrically with body mass. The plots are not stock
projections or management advice.

This is the R Shiny version of the web dashboard (version of 3 Nov 2026). It reproduces the web version's numbers
exactly (checked across all closure lengths and a range of recovery-zone shares).

## Files

```
rz_options/
├── app.R                                       UI and server (the file Shiny Server looks for)
├── R/model.R                                   model functions: per-recruit model, eggs per kg, scenario lines
├── data/
│   ├── aston2025_fig4_spawning_biomass_t.csv   spawning biomass (t), S1–S4, median and 95% interval (Aston et al. 2025 Fig. 4)
│   ├── aston2025_fig7a_cpue_0_10km.csv         catch per boat day per km², 0–10 km from boat ramps (Fig. 7a)
│   ├── aston2025_fig6_density_inside_zones.csv mature and large-mature fish per km² inside recovery zones (Fig. 6)
│   └── rz_options.csv                          recovery-zone options and their share of mapped reef
├── www/styles.css                              page styling
└── README.md
```

Scenarios in Aston et al. (2025): S1 recovery zones only, S2 no closures, S3 spawning (temporal) closures only,
S4 recovery zones + spawning closures. Model years 2005–2018 are shown as 2027–2040. Values were digitised from
the published figures.

## Requirements

- R 4.0 or later
- R packages **shiny** (1.5 or later) and **plotly** (4.10 or later)

```r
install.packages(c("shiny", "plotly"))
```

Install the packages from CRAN (or Posit Package Manager binaries). The Ubuntu/Debian `r-cran-shiny` and
`r-cran-plotly` system packages repackage some JavaScript files and show harmless console errors; CRAN builds don't.

## Run it locally

From the folder that contains `rz_options/`:

```r
shiny::runApp("rz_options")
```

## Deploy on Shiny Server (open source or Pro)

1. Make sure `shiny` and `plotly` are installed in a library the `shiny` user can read, e.g. run as root:
   `sudo su - -c "R -e \"install.packages(c('shiny', 'plotly'), repos = 'https://cloud.r-project.org')\""`
2. Copy the whole `rz_options` folder to the server's app directory (default `/srv/shiny-server/`):
   `sudo cp -R rz_options /srv/shiny-server/`
3. Open `http://<your-server>:3838/rz_options/`. No restart is needed; Shiny Server picks up new apps.
   If the page doesn't load, check the logs in `/var/log/shiny-server/`.

## Deploy on shinyapps.io or Posit Connect

```r
install.packages("rsconnect")
rsconnect::deployApp("rz_options")
```

## Using the dashboard

- Each chart card has two sliders, **Spawning closure length** (3–9 months) and **Recovery-zone share of
  habitat** (0–60%). The two copies are linked, so moving either updates both charts.
- **Set to** buttons jump to an option: 0%, Small 13.0%, Medium 19.6%, Large 28.9%, Extra large 60% (hypothetical,
  no layout drawn). Shares are of the mapped reef (0–200 m, Kalbarri to Cape Leeuwin, 14,813 km²).
- Click a line in a chart legend to hide or show it. The y-axes stay fixed so slider changes are comparable.
- Data tables (with the values behind each chart at the current settings) are under **Data tables**.

Values to check after installing (2040, defaults: 5-month closure, 34% recovery zones): egg production 19.2% of
unfished with spawning closures only and 29.1% with recovery zones; catch rate 1.63 and 2.64 fish per boat day
per km². With the Large option (28.9%): 27.6% and 2.49.

## How the lines are built

- **Two lines.** Spawning closures only, and recovery zones + spawning closures. Each combines the four
  published scenarios bilinearly: value = S2 + t(S1 − S2) + m(S3 − S2) + t·m(S4 − S1 − S3 + S2), where
  t = recovery-zone share ÷ 34% (the coverage modelled by Aston et al.) and m = the closure effect relative to the
  modelled 5-month closure. At 0% the two lines match; at 34% the recovery-zone line is the published S4.
  Shares above 34% extrapolate beyond the model.
- **Closure length.** Effort removed = 0.0625 × (months + 3): 3 months 37.5%, 5 months 50%, 9 months 75%. Its
  effect is rescaled with a per-recruit model and Beverton–Holt recruitment (steepness 0.75), from a fishing
  mortality (≈0.22) that holds the stock at 15% of unfished spawning biomass.
- **Starting point.** Both lines start in 2027 from a depleted stock at 15% of unfished spawning biomass, which
  produces about 9.5% of unfished egg output. Each published line is shifted to start there, keeping its
  year-to-year changes ("same shape").
- **Egg production** = spawning biomass × eggs per kg. Eggs per kg rises with the share of big females (≥700 mm,
  from Aston et al. Fig. 6), using the per-recruit model of growth, maturity, batch fecundity and spawning
  duration and frequency (Evans-Powell et al. 2024). Fish in fished waters are sized from catch rates.
- **Catch rate** is fish per boat day per km² in model cells within 10 km of the four main boat ramps, per boat
  day while fishing is open.

These are re-scalings of published results, not new model runs. A model run for the West Coast with each
option's actual zone layout is needed to compare the options properly.

## Editing

- Text (disclaimer, headings, notes) is in `app.R`.
- Recovery-zone options: edit `data/rz_options.csv`; the **Set to** buttons and options table update
  automatically. Leave `zones` blank for a hypothetical option with no layout.
- Life-history parameters and constants (e.g. the 15% start, steepness, modelled 34%) are at the top of
  `R/model.R`.

## References

- Aston C, Hesp A, Navarro M, Goetze JS, Wise B, Langlois T (2025). A fine-scale fish population model reveals how
  integration of spatial and temporal management can maximise conservation and fisheries benefits. *Journal of
  Applied Ecology* 62(11): 2954–2970. https://doi.org/10.1111/1365-2664.70185 · Model code:
  https://doi.org/10.5281/zenodo.17060863
- Evans-Powell RT, Hesp SA, Denham A, Beckley LE (2024). Implications of big, old, fat, fecund, female fish
  (BOFFFFs) for the reproductive potential of a demersal teleost stock. *Fisheries Research* 272: 106934.
  https://doi.org/10.1016/j.fishres.2023.106934
- Barneche DR, Robertson DR, White CR, Marshall DJ (2018). Fish reproductive-energy output increases
  disproportionately with body size. *Science* 360: 642–645. https://doi.org/10.1126/science.aao6868
- Fisher EA, Fairclough DV, Lek E, Briggs J, Hesp SA, Denham A (2025). West Coast Demersal Scalefish Resource:
  2025 Assessment. Resource Assessment Report No. 2, Department of Primary Industries and Regional Development,
  Western Australia.
