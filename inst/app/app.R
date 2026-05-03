library(shiny)
library(bslib)

# Internal functions from your package
V_potential <- CuspCatastrophe:::V_potential
cusp_equilibria <- CuspCatastrophe:::cusp_equilibria
classify_equilibria <- CuspCatastrophe:::classify_equilibria
dX <- CuspCatastrophe:::dX
dY <- CuspCatastrophe:::dY

# Theme: keep Cyborg but enforce readable contrast (bg & fg must be paired)
app_theme <- bs_theme(
  version = 5,
  bootswatch = "cyborg",
  bg = "#060606",
  fg = "#f8f9fa"
)

# Preset examples for Two Factor Change Model (3 examples x 6 weights)
two_factor_presets <- data.frame(
  Example = c("Attractor (single sink)", "Ridge (saddle)", "Valley (line attractor)"),
  a0 = c(0.0, 0.0, 0.0),
  a1 = c(-1.5,  1.5, -1.5),
  a2 = c(0.0,  0.0,  0.0),
  b0 = c(0.0, 0.0, 0.0),
  b1 = c(0.0, 0.0, 0.0),
  b2 = c(-1.0, -1.0, 0.0),
  stringsAsFactors = FALSE
)

ui <- page_fluid(
  theme = app_theme,

  tags$head(
    tags$style(HTML("
      .card-accent-blue   { border-left: 6px solid #0d6efd !important; }
      .card-accent-green  { border-left: 6px solid #20c997 !important; }
      .card-accent-gray   { border-left: 6px solid #adb5bd !important; }
      .card { margin-bottom: 14px; }
      .helper { font-size: 0.90rem; opacity: 0.92; margin-top: 4px; }
      .eqn {
        font-size: 0.98rem;
        background: rgba(255,255,255,0.06);
        border: 1px solid rgba(255,255,255,0.12);
        border-radius: 6px;
        padding: 10px 12px;
        margin-top: 8px;
      }
    "))
  ),

  # -------------------------
  # Control Layer (own box)
  # -------------------------
  layout_columns(
    col_widths = c(4, 8),

    card(
      class = "card-accent-gray",
      card_header("Control Layer"),
      radioButtons(
        "mode",
        label = NULL,
        choices = c(
          "Topological Exploration" = "topo",
          "One Factor Change Model" = "onefactor",
          "Two Factor Change Model" = "twofactor"
        ),
        selected = "topo"
      )
    ),

    card(
      class = "card-accent-gray",
      card_header("What you are seeing"),
      uiOutput("mode_blurb")
    )
  ),

  # -------------------------
  # Main 3-column layout
  # -------------------------
  layout_columns(
    col_widths = c(4, 4, 4),

    uiOutput("col_controls"),
    uiOutput("col_visuals"),
    uiOutput("col_readouts")
  )
)

server <- function(input, output, session) {

  # -------------------------
  # Mode blurb
  # -------------------------
  output$mode_blurb <- renderUI({
    if (identical(input$mode, "topo")) {
      tagList(
        tags$p("Topological exploration: manipulate the cusp potential directly with alpha and beta."),
        tags$div(class = "eqn",
                 HTML("<b>V(z)</b> = (1/4)z<sup>4</sup> − (1/2)&beta;z<sup>2</sup> − &alpha;z"))
      )
    } else if (identical(input$mode, "onefactor")) {
      tagList(
        tags$p("One Factor Change Model: expected change in x depends on x and a single factor a."),
        tags$div(class = "eqn",
                 HTML("&Delta;x = b0 + b1·x + b2·a"))
      )
    } else {
      tagList(
        tags$p("Implied phase space (vector field) represents the expected change in x and y given x and y position."),
        tags$div(class = "eqn",
                 HTML("&Delta;X = a0 + a1·X + a2·Y<br/>&Delta;Y = b0 + b1·X + b2·Y"))
      )
    }
  })

  # -------------------------
  # Column 1: Controls
  # -------------------------
  output$col_controls <- renderUI({
    if (identical(input$mode, "topo")) {

      tagList(
        card(
          class = "card-accent-blue",
          card_header("Control Parameters (Topology)"),
          div(class = "helper", "Adjust alpha and beta to reshape the potential. z marks a point on the curve."),
          sliderInput("alpha", HTML("alpha (&alpha;)"), min = -5, max = 5, value = 0, step = 0.5),
          sliderInput("beta",  HTML("beta (&beta;)"),  min = -5, max = 5, value = 1, step = 0.5),
          hr(),
          sliderInput("z_marker", "z", min = -10, max = 10, value = 0, step = 0.5)
        )
      )

    } else if (identical(input$mode, "onefactor")) {

      tagList(
        card(
          class = "card-accent-green",
          card_header("Change Equation (One Factor)"),
          div(class = "helper", "Enter coefficients for Δx = b0 + b1·x + b2·a (typed input)."),
          tags$div(class = "eqn", HTML("&Delta;x = b0 + b1·x + b2·a")),
          fluidRow(
            column(4, numericInput("b0_1", "b0", 0,   min = -5, max = 5, step = 0.5)),
            column(4, numericInput("b1_1", "b1", -1,  min = -5, max = 5, step = 0.5)),
            column(4, numericInput("b2_1", "b2", 1,   min = -5, max = 5, step = 0.5))
          ),
          numericInput("a_1", "a", 0, min = -5, max = 5, step = 0.5)
        )
      )

    } else {

      tagList(
        card(
          class = "card-accent-green",
          card_header("Change Equations (Two Factor)"),
          div(class = "helper", "Enter coefficients (typed input)."),
          tags$div(class = "eqn", HTML("&Delta;X = a0 + a1·X + a2·Y")),
          fluidRow(
            column(4, numericInput("a0", "a0", 0,   min = -5, max = 5, step = 0.5)),
            column(4, numericInput("a1", "a1", -0.5, min = -5, max = 5, step = 0.5)),
            column(4, numericInput("a2", "a2", 0.5, min = -5, max = 5, step = 0.5))
          ),
          tags$div(class = "eqn", HTML("&Delta;Y = b0 + b1·X + b2·Y")),
          fluidRow(
            column(4, numericInput("b0", "b0", 0,   min = -5, max = 5, step = 0.5)),
            column(4, numericInput("b1", "b1", -0.5, min = -5, max = 5, step = 0.5)),
            column(4, numericInput("b2", "b2", -0.5, min = -5, max = 5, step = 0.5))
          ),
          hr(),
          sliderInput("grid_n", "Vector grid density", min = 7, max = 25, value = 13, step = 2),
          sliderInput("xy_lim", "X,Y plot limits (symmetric)", min = 1, max = 10, value = 5, step = 0.5)
        )
      )
    }
  })

  # -------------------------
  # Column 2: Visualizations
  # -------------------------
  output$col_visuals <- renderUI({
    if (identical(input$mode, "topo")) {
      tagList(
        card(
          class = "card-accent-gray",
          full_screen = TRUE,
          card_header("Potential V(z)"),
          plotOutput("potential_plot", height = 340)
        ),
        card(
          class = "card-accent-gray",
          full_screen = TRUE,
          card_header("Control Plane (beta, alpha)"),
          plotOutput("control_plane", height = 340)
        )
      )
    } else if (identical(input$mode, "onefactor")) {
      tagList(
        card(
          class = "card-accent-gray",
          full_screen = TRUE,
          card_header("One Factor: Δx vs x"),
          plotOutput("onefactor_plot", height = 700)
        )
      )
    } else {
      tagList(
        card(
          class = "card-accent-gray",
          full_screen = TRUE,
          card_header("Phase Space (X,Y) — Vector Field"),
          plotOutput("phase_plot", height = 700)
        )
      )
    }
  })

  # -------------------------
  # Column 3: Readouts / Tables
  # -------------------------
  output$col_readouts <- renderUI({
    if (identical(input$mode, "topo")) {
      tagList(
        card(
          class = "card-accent-blue",
          card_header("Equilibria and Stability"),
          div(class = "helper", "Equilibria are z where V'(z)=0; stability depends on the sign of the second derivative."),
          tableOutput("equilibria_table")
        )
      )
    } else if (identical(input$mode, "onefactor")) {
      tagList(
        card(
          class = "card-accent-green",
          card_header("Set Point"),
          tags$div(class = "eqn", HTML("Set Point = (b0 + b2·a)/(-b1)")),
          verbatimTextOutput("setpoint_readout"),
          div(class = "helper", "A vertical line is drawn at the set point on the plot (when b1 ≠ 0).")
        )
      )
    } else {
      tagList(
        card(
          class = "card-accent-green",
          card_header("Try out example weights"),
          div(class = "helper", "Select an example and load its weights into the inputs."),
          tableOutput("preset_table"),
          fluidRow(
            column(4, actionButton("load_attractor", "Load Attractor")),
            column(4, actionButton("load_ridge", "Load Ridge")),
            column(4, actionButton("load_valley", "Load Valley"))
          )
        ),
        card(
          class = "card-accent-green",
          card_header("Equation Readout"),
          verbatimTextOutput("eqn_readout")
        )
      )
    }
  })

  # -------------------------
  # Topological plots
  # -------------------------
  output$potential_plot <- renderPlot({
    req(identical(input$mode, "topo"))
    z <- seq(-10, 10, length.out = 1200)
    V <- V_potential(z, input$alpha, input$beta)

    # Clip to keep topology visible (adjust if you prefer a wider view)
    V_clip <- pmax(pmin(V, 10), -10)

    plot(
      z, V_clip, type = "l", lwd = 2,
      ylim = c(-6, 6),
      xlab = "z",
      ylab = "V(z) (clipped to [-6,6])",
      main = NULL
    )

    mtext(sprintf("alpha = %.1f    beta = %.1f", input$alpha, input$beta),
          side = 3, line = 0.2, adj = 0, cex = 0.9)

    z0 <- input$z_marker
    V0 <- V_potential(z0, input$alpha, input$beta)
    V0c <- max(min(V0, 6), -6)
    points(z0, V0c, pch = 19, col = "tomato")
    abline(v = z0, lty = 2, col = "gray60")
  })

  output$control_plane <- renderPlot({
    req(identical(input$mode, "topo"))
    beta_grid <- seq(0, 5, length.out = 400)
    alpha_bound <- (2 / (3 * sqrt(3))) * beta_grid^(3/2)

    plot(NA, xlim = c(-5, 5), ylim = c(-5, 5),
         xlab = "beta", ylab = "alpha", main = NULL)

    lines(beta_grid,  alpha_bound, col = "deepskyblue2", lwd = 2)
    lines(beta_grid, -alpha_bound, col = "deepskyblue2", lwd = 2)
    points(input$beta, input$alpha, pch = 19, col = "black")
  })

  output$equilibria_table <- renderTable({
    req(identical(input$mode, "topo"))
    eq <- classify_equilibria(
      cusp_equilibria(input$alpha, input$beta),
      input$alpha, input$beta
    )
    if (nrow(eq) == 0) return(NULL)
    eq
  }, digits = 3)

  # -------------------------
  # One Factor Change Model
  # -------------------------
  set_point <- reactive({
    # Set Point = (b0 + b2*a)/(-b1)
    # Handle b1 = 0 gracefully
    if (isTRUE(all.equal(input$b1_1, 0))) return(NA_real_)
    (input$b0_1 + input$b2_1 * input$a_1) / (-input$b1_1)
  })

  output$setpoint_readout <- renderPrint({
    req(identical(input$mode, "onefactor"))
    sp <- set_point()
    if (is.na(sp)) {
      cat("Set Point is undefined because b1 = 0.\n")
    } else {
      cat(sprintf("Set Point = %.3f\n", sp))
    }
  })

  output$onefactor_plot <- renderPlot({
    req(identical(input$mode, "onefactor"))

    x <- seq(-5, 5, length.out = 400)
    dx <- input$b0_1 + input$b1_1 * x + input$b2_1 * input$a_1

    plot(x, dx, type = "l", lwd = 2,
         xlim = c(-5, 5), ylim = c(-5, 5),
         xlab = "x", ylab = "Δx",
         main = NULL)

    abline(h = 0, col = "gray60", lty = 2)

    sp <- set_point()
    if (!is.na(sp) && sp >= -5 && sp <= 5) {
      abline(v = sp, col = "tomato", lwd = 2, lty = 2)
      text(sp, 4.5, labels = sprintf("Set Point = %.2f", sp),
           pos = 4, cex = 0.85, col = "tomato")
    }
  })

  # -------------------------
  # Two Factor Change Model: Preset table + load buttons
  # -------------------------
  output$preset_table <- renderTable({
    req(identical(input$mode, "twofactor"))
    two_factor_presets
  }, digits = 2)

  observeEvent(input$load_attractor, {
    row <- two_factor_presets[1, ]
    updateNumericInput(session, "a0", value = row$a0)
    updateNumericInput(session, "a1", value = row$a1)
    updateNumericInput(session, "a2", value = row$a2)
    updateNumericInput(session, "b0", value = row$b0)
    updateNumericInput(session, "b1", value = row$b1)
    updateNumericInput(session, "b2", value = row$b2)
  })

  observeEvent(input$load_ridge, {
    row <- two_factor_presets[2, ]
    updateNumericInput(session, "a0", value = row$a0)
    updateNumericInput(session, "a1", value = row$a1)
    updateNumericInput(session, "a2", value = row$a2)
    updateNumericInput(session, "b0", value = row$b0)
    updateNumericInput(session, "b1", value = row$b1)
    updateNumericInput(session, "b2", value = row$b2)
  })

  observeEvent(input$load_valley, {
    row <- two_factor_presets[3, ]
    updateNumericInput(session, "a0", value = row$a0)
    updateNumericInput(session, "a1", value = row$a1)
    updateNumericInput(session, "a2", value = row$a2)
    updateNumericInput(session, "b0", value = row$b0)
    updateNumericInput(session, "b1", value = row$b1)
    updateNumericInput(session, "b2", value = row$b2)
  })

  output$phase_plot <- renderPlot({
    req(identical(input$mode, "twofactor"))

    lim <- input$xy_lim
    n <- input$grid_n

    xs <- ys <- seq(-lim, lim, length.out = n)
    g <- expand.grid(x = xs, y = ys)

    u <- dX(g$x, g$y, input$a0, input$a1, input$a2)
    v <- dY(g$x, g$y, input$b0, input$b1, input$b2)

    mag <- sqrt(u^2 + v^2)
    mag[mag == 0] <- 1

    plot(NA, xlim = c(-lim, lim), ylim = c(-lim, lim),
         xlab = "X", ylab = "Y", main = NULL)
    abline(h = 0, v = 0, col = "gray70", lty = 2)

    arrows(g$x, g$y,
           g$x + u/mag*0.4,
           g$y + v/mag*0.4,
           length = 0.06, col = "black", lwd = 1.2)
  })

  output$eqn_readout <- renderPrint({
    req(identical(input$mode, "twofactor"))
    cat(sprintf("ΔX = %.1f + %.1f·X + %.1f·Y\n", input$a0, input$a1, input$a2))
    cat(sprintf("ΔY = %.1f + %.1f·X + %.1f·Y\n", input$b0, input$b1, input$b2))
  })
}

shinyApp(ui, server)
