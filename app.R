# Recovery Zone Options: Demersal Fish Scenarios
# Run locally with shiny::runApp("rz_options") or deploy the folder to Shiny Server (see README.md).
# Concept illustration only: not a stock projection or management advice.

library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(dplyr)
library(readr)
library(purrr)

# Shiny loads R/model.R automatically before this file; source it here only if that's switched off
if (!exists("egg_lines")) source("R/model.R", local = TRUE)

# ---- Setup -------------------------------------------------------------------------------------------

D  <- load_model_data("data")
KG <- per_kg_table(D, "e")

COL  <- c(cl = "#d9622b", rz = "#1d6fa5")                               # closures only / recovery zones + closures
BAND <- c(cl = "rgba(217,98,43,0.15)", rz = "rgba(29,111,165,0.15)")

YMAX <- list(
  eggs = map_dbl(c(off = FALSE, on = TRUE), \(b) axis_max(\(L, a) egg_lines(D, KG, L, a), b)),
  cpue = map_dbl(c(off = FALSE, on = TRUE), \(b) axis_max(\(L, a) cpue_lines(D, L, a), b))
)

PRESETS <- tibble(label = c("None", D$OPTS$preset_label), share = c(0, D$OPTS$share_of_mapped_reef_pct))

opt_at     <- function(v) D$OPTS |> filter(abs(share_of_mapped_reef_pct - v) < 0.05) |> slice_head(n = 1)
share_fmt  <- function(v) paste0(round(v, 1), "%")
pct_change <- function(a, b) { r <- round((a / b - 1) * 100); paste0(if (r >= 0) "+" else "−", abs(r), "%") }

# label with an info icon that shows a tooltip on hover or tap
info <- function(label, tip) {
  span(label, tooltip(bs_icon("info-circle", class = "text-secondary ms-1", a11y = "sem", title = "More information"), tip))
}

# ---- Text --------------------------------------------------------------------------------------------

about_modal <- function() {
  modalDialog(
    title = "About this dashboard",
    p(strong("Concept illustration only."), "This dashboard uses a published fine-scale population model of spangled
      emperor (", em("Lethrinus nebulosus"), ") at Ningaloo (Aston et al. 2025), with added estimates of the effect on
      egg production for a similar species whose reproductive output scales hyperallometrically with body mass."),
    p("It shows what a full scenario analysis of spawning closures and recovery zones could look like for spawning
      biomass, egg production and recreational fishing amenity. The plots are", strong("not"), "stock projections or
      management advice and should not be relied upon."),
    p(class = "text-secondary small mb-0", "Lines are re-scalings of published model results, not new model runs.
      A model run for the West Coast with each option's actual zone layout is needed to compare options properly."),
    footer = modalButton("I understand"),
    easyClose = TRUE, size = "l"
  )
}

TIPS <- list(
  closure = "Months closed to fishing each year during the spawning season. Effort removed = 0.0625 × (months + 3),
             so 5 months removes 50%. The modelled closure was 5 months; other lengths are rescaled with a
             per-recruit model.",
  ntz     = "Share of mapped reef (0–200 m, Kalbarri to Cape Leeuwin) inside recovery zones. Aston et al. modelled
             34%; other shares are scaled from that, and shares above 34% extrapolate beyond the model.",
  band    = "Placeholder for uncertainty: the published 95% intervals carried through the same rescaling.",
  eggs    = "Egg production as a percentage of an unfished stock, allowing for big, old females that spawn for
             longer, more often and in bigger batches. Both lines start in 2027 from a depleted stock at 15% of
             unfished spawning biomass (about 9.5% of unfished egg output).",
  cpue    = "Predicted catch per boat day while fishing is open, in model cells within 10 km of the four main boat
             ramps (Aston et al. 2025, Fig. 7a). Both lines start from the same depleted level in 2027."
)

# ---- UI ----------------------------------------------------------------------------------------------

sidebar_ui <- sidebar(
  width = 320,
  sliderInput("closure", info("Spawning closure length", TIPS$closure),
              min = 3, max = 9, value = CL0, step = 1, post = " months", ticks = FALSE),
  div(class = "control-note", textOutput("closure_txt", inline = TRUE)),
  hr(),
  sliderInput("ntz", info("Recovery-zone share of habitat", TIPS$ntz),
              min = 0, max = NTZ_MAX, value = NTZ0, step = 0.1, post = "%", ticks = FALSE),
  div(class = "control-note", textOutput("ntz_txt", inline = TRUE)),
  div(class = "presets",
      span(class = "small text-secondary", "Options:"),
      imap(PRESETS$label, \(lab, k) actionButton(paste0("preset_", k), lab, class = "btn-sm btn-outline-secondary"))),
  hr(),
  checkboxInput("band", info("Show 95% interval", TIPS$band), FALSE),
  actionButton("reset", "Reset to modelled settings", icon = icon("rotate-left"), class = "btn-sm btn-link px-0")
)

chart_card <- function(id, objective, title, tip) {
  card(
    full_screen = TRUE,
    card_header(
      div(class = "objective", objective),
      div(class = "d-flex align-items-center", h5(class = "mb-0", title), tooltip(bs_icon("info-circle", class = "text-secondary ms-2"), tip))
    ),
    card_body(plotlyOutput(paste0("plot_", id), height = "380px")),
    card_footer(uiOutput(paste0("boxes_", id)))
  )
}

ui <- page_sidebar(
  title = div(class = "d-flex align-items-center w-100 gap-3",
              span(class = "app-title", "Recovery Zone Options"),
              span(class = "app-subtitle d-none d-md-inline", "Demersal fish scenarios · West Coast"),
              actionButton("about", "About & disclaimer", icon = icon("circle-info"), class = "btn-sm btn-outline-light ms-auto")),
  window_title = "Recovery Zone Options",
  theme = bs_theme(version = 5, primary = "#12465e", "navbar-bg" = "#12465e",
                   base_font = font_google("Source Sans 3", local = FALSE), font_scale = 0.95),
  fillable = FALSE,
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  sidebar = sidebar_ui,
  
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(6, 6)),
    chart_card("eggs", "Objective A · Egg production", "Egg production (% of unfished)", TIPS$eggs),
    chart_card("cpue", "Objective B · Fishing amenity", "Catch rates near boat ramps", TIPS$cpue)
  ),
  
  accordion(
    open = FALSE,
    accordion_panel(
      "Why big females matter", icon = bs_icon("graph-up-arrow"),
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        value_box("Spawning season, 900 mm vs 350 mm female", "6 vs 2 months"),
        value_box("Spawning events per season", "155 vs 60"),
        value_box("2 kg females to match one 10 kg female", "≈ 15", p("(5 if eggs scaled with weight)"))
      ),
      p(class = "small text-secondary mt-2 mb-0", "Evans-Powell et al. (2024). Across marine fishes, reproductive-energy
        output rises faster than body mass (Barneche et al. 2018).")
    ),
    accordion_panel(
      "Data tables", icon = bs_icon("table"),
      h6("Recovery-zone options"), tableOutput("tbl_opt"),
      layout_columns(
        div(h6("Egg production (% of unfished)"), tableOutput("tbl_eggs")),
        div(h6("Catch rate (fish per boat day per km²)"), tableOutput("tbl_cpue"))
      )
    ),
    accordion_panel(
      "Methods and references", icon = bs_icon("book"),
      tags$ul(class = "small",
              tags$li("Each line combines the four published scenarios bilinearly, scaled by recovery-zone share (relative to
                 the modelled 34%) and closure effect (relative to the modelled 5 months)."),
              tags$li("Egg production = spawning biomass × eggs per kg; eggs per kg rises with the share of females ≥ 700 mm."),
              tags$li("Model years 2005–2018 are shown as 2027–2040. Values were digitised from published figures.")
      ),
      div(class = "small text-secondary refs",
          p("Aston C, Hesp A, Navarro M, Goetze JS, Wise B, Langlois T (2025). A fine-scale fish population model reveals
           how integration of spatial and temporal management can maximise conservation and fisheries benefits.",
           em("Journal of Applied Ecology"), "62(11): 2954–2970.",
           a("doi.org/10.1111/1365-2664.70185", href = "https://doi.org/10.1111/1365-2664.70185", target = "_blank")),
          p("Evans-Powell RT, Hesp SA, Denham A, Beckley LE (2024). Implications of big, old, fat, fecund, female fish
           (BOFFFFs) for the reproductive potential of a demersal teleost stock.", em("Fisheries Research"), "272: 106934.",
           a("doi.org/10.1016/j.fishres.2023.106934", href = "https://doi.org/10.1016/j.fishres.2023.106934", target = "_blank")),
          p("Barneche DR, Robertson DR, White CR, Marshall DJ (2018). Fish reproductive-energy output increases
           disproportionately with body size.", em("Science"), "360: 642–645.",
           a("doi.org/10.1126/science.aao6868", href = "https://doi.org/10.1126/science.aao6868", target = "_blank")),
          p("Fisher EA, Fairclough DV, Lek E, Briggs J, Hesp SA, Denham A (2025). West Coast Demersal Scalefish Resource:
           2025 Assessment. Resource Assessment Report No. 2, DPIRD, Western Australia.")
      )
    )
  )
)

# ---- Server ------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  showModal(about_modal())
  observeEvent(input$about, showModal(about_modal()))
  
  # option buttons and reset
  iwalk(PRESETS$share, \(share, k) observeEvent(input[[paste0("preset_", k)]], updateSliderInput(session, "ntz", value = share)))
  observeEvent(input$reset, {
    updateSliderInput(session, "closure", value = CL0)
    updateSliderInput(session, "ntz", value = NTZ0)
  })
  
  ntz  <- reactive(round(input$ntz, 1))
  eggs <- reactive(egg_lines(D, KG, input$closure, ntz()))
  cpue <- reactive(cpue_lines(D, input$closure, ntz()))
  rz_label <- reactive(paste0("Recovery zones (", share_fmt(ntz()), ") + closures"))
  
  output$closure_txt <- renderText(sprintf("%s · %d%% less effort · %d months open",
                                           CLOSE_MONTHS[[as.character(input$closure)]], round(effort_cut(input$closure) * 100), 12 - input$closure))
  
  output$ntz_txt <- renderText({
    o <- opt_at(ntz())
    case_when(nrow(o) > 0      ~ o$option[1],
              ntz() == NTZ0    ~ "As modelled by Aston et al.",
              ntz() > NTZ0     ~ "Extrapolated beyond the model",
              TRUE             ~ "Custom share")
  })
  
  # ---- charts ----
  trend_plot <- function(r, ymax, ylab, digits, suffix) {
    hover <- paste0("%{y:.", digits, "f}", suffix)
    p <- plot_ly(x = YEARS)
    if (input$band) {
      p <- p |>
        add_ribbons(ymin = r$rz$lo, ymax = r$rz$hi, fillcolor = BAND[["rz"]], line = list(width = 0), hoverinfo = "skip", showlegend = FALSE) |>
        add_ribbons(ymin = r$cl$lo, ymax = r$cl$hi, fillcolor = BAND[["cl"]], line = list(width = 0), hoverinfo = "skip", showlegend = FALSE)
    }
    p |>
      # traces drawn later sit on top, so the orange closures-only line goes last; legendrank keeps it first in the legend
      add_lines(y = r$rz$m, name = rz_label(), line = list(color = COL[["rz"]], width = 2.5), hovertemplate = hover, legendrank = 2) |>
      add_lines(y = r$cl$m, name = "Spawning closures only", line = list(color = COL[["cl"]], width = 2.5), hovertemplate = hover, legendrank = 1) |>
      add_markers(x = YEARS[1], y = r$start, name = "Common start (2027)", hovertemplate = hover,
                  marker = list(color = "#343a40", size = 9, line = list(color = "white", width = 1.5))) |>
      layout(
        font = list(family = "Source Sans 3, sans-serif", size = 13, color = "#495057"),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = -0.12),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        xaxis = list(title = "", dtick = 2, showgrid = FALSE, fixedrange = TRUE),
        yaxis = list(title = "", showticklabels = FALSE, ticks = "", range = c(0, ymax * 1.08), gridcolor = "#e9ecef",
                     zeroline = FALSE, fixedrange = TRUE)
      ) |>
      config(displayModeBar = FALSE)
  }
  
  band_key <- reactive(if (input$band) "on" else "off")
  output$plot_eggs <- renderPlotly(trend_plot(eggs(), YMAX$eggs[[band_key()]], "% of unfished", 1, "%"))
  output$plot_cpue <- renderPlotly(trend_plot(cpue(), YMAX$cpue[[band_key()]], "Fish per boat day per km²", 2, ""))
  
  # ---- summary boxes (values in 2040) ----
  summary_boxes <- function(r, digits, suffix) {
    fmt <- \(v) paste0(formatC(v, format = "f", digits = digits), suffix)
    cl <- last(r$cl$m); rz <- last(r$rz$m)
    box <- \(title, value, note, key) div(class = paste("stat", paste0("stat-", key)),
                                          div(class = "stat-title", title), div(class = "stat-value", value), div(class = "stat-note", note))
    layout_column_wrap(
      width = "140px", fill = FALSE,
      box("Closures only, 2040", fmt(cl), paste(pct_change(cl, r$start), "since 2027"), "cl"),
      box("With recovery zones, 2040", fmt(rz), paste(pct_change(rz, r$start), "since 2027"), "rz"),
      box("Gain from recovery zones", pct_change(rz, cl), "vs closures only", "gain")
    )
  }
  output$boxes_eggs <- renderUI(summary_boxes(eggs(), 1, "%"))
  output$boxes_cpue <- renderUI(summary_boxes(cpue(), 2, ""))
  
  # ---- tables ----
  series_table <- function(r, digits) {
    tibble(Year = as.character(YEARS),
           `Closures only` = formatC(r$cl$m, format = "f", digits = digits),
           `With recovery zones` = formatC(r$rz$m, format = "f", digits = digits))
  }
  output$tbl_eggs <- renderTable(series_table(eggs(), 1), align = "lrr", striped = TRUE, spacing = "s")
  output$tbl_cpue <- renderTable(series_table(cpue(), 2), align = "lrr", striped = TRUE, spacing = "s")
  
  output$tbl_opt <- renderTable(
    D$OPTS |>
      transmute(
        Option = option,
        `GIS layer` = coalesce(gis_layer, "–"),
        Zones = coalesce(as.character(zones), "–"),
        `Zone area (km²)` = if_else(is.na(zone_area_km2), "–", formatC(zone_area_km2, format = "d", big.mark = ",")),
        `Reef inside zones (km²)` = formatC(reef_inside_km2, format = "d", big.mark = ","),
        `Share of mapped reef` = sprintf("%.1f%%", share_of_mapped_reef_pct),
        `Share of modelled effect` = sprintf("%d%%", round(share_of_mapped_reef_pct / (A0 * 100) * 100))
      ),
    align = "llrrrrr", striped = TRUE, spacing = "s"
  )
}

shinyApp(ui, server)