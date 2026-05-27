
#' Title
#'
#' @returns
#' @export
#'
#' @examples

cb_shiny_wav_to_flac2 <- function() {

  # initialize once at app startup
  future::plan(future::multisession, workers = 4)

  ui <- shiny::fluidPage(

    shinyjs::useShinyjs(),

    shiny::titlePanel("Sierra Monitoring WAV to FLAC converter"),

    shiny::sidebarLayout(

      shiny::sidebarPanel(

        shiny::selectInput(
          "sd_card_path",
          "SD card path:",
          choices = c(
            "D:/" = "D:/",
            "E:/" = "E:/",
            "F:/" = "F:/",
            "G:/" = "G:/",
            "H:/" = "H:/",
            "I:/" = "I:/",
            "J:/" = "J:/",
            "K:/" = "K:/",
            "L:/" = "L:/",
            "M:/" = "M:/",
            "N:/" = "N:/",
            "O:/" = "O:/"
          ),
          selected = "D:/"
        ),

        shiny::textInput(
          "desktop_path",
          "Local path:",
          value = "C:/Users/jmwin/OneDrive/Desktop/temp"
        ),

        shiny::selectInput(
          "hard_drive_path",
          "External hard drive path:",
          choices = c(
            "D:/" = "D:/",
            "E:/" = "E:/",
            "F:/" = "F:/",
            "G:/" = "G:/",
            "H:/" = "H:/",
            "I:/" = "I:/",
            "J:/" = "J:/",
            "K:/" = "K:/",
            "L:/" = "L:/",
            "M:/" = "M:/",
            "N:/" = "N:/",
            "O:/" = "O:/"
          ),
          selected = "E:/"
        ),

        shiny::selectInput(
          "year",
          "Survey year:",
          choices = 2021:as.integer(format(Sys.Date(), "%Y")),
          selected = as.integer(format(Sys.Date(), "%Y"))
        ),

        shiny::selectInput(
          "processing",
          "Processing:",
          choices = c("Sequential", "Parallel"),
          selected = "Parallel"
        ),

        shiny::checkboxInput(
          "use_all_cores",
          "Use available cores - 1",
          value = TRUE
        ),

        shiny::numericInput(
          "workers",
          "Workers:",
          value = 4,
          min = 1,
          max = parallelly::availableCores() - 1,
          step = 1
        ),

        shiny::actionButton(
          "run_button",
          "Run FLAC compression",
          style = "background-color: #458B74; color: white;",
          icon = shiny::icon("play")
        )
      ),

      shiny::mainPanel(
        shiny::htmlOutput("runtime")
      )
    )
  )

  server <- function(input, output, session) {

    # track current worker count
    current_workers <- shiny::reactiveVal(4)

    # disable numeric input if auto-core mode selected
    shiny::observe({

      if (input$use_all_cores) {

        shinyjs::disable("workers")

      } else {

        shinyjs::enable("workers")

      }

    })

    shiny::observeEvent(input$run_button, {

      shinyjs::disable("run_button")

      on.exit(
        shinyjs::enable("run_button"),
        add = TRUE
      )

      start_time <- Sys.time()

      # determine requested worker count
      requested_workers <-

        if (input$use_all_cores) {

          max(1, parallelly::availableCores() - 1)

        } else {

          input$workers

        }

      # only reset future plan if needed
      if (
        input$processing == "Parallel" &&
        requested_workers != current_workers()
      ) {

        future::plan(
          future::multisession,
          workers = requested_workers
        )

        current_workers(requested_workers)

      }

      val <-
        CAbioacoustics:::get_deployment_info2(
        input$sd_card_path,
        input$year
      )

      sd_wavs <-
        fs::dir_ls(
          input$sd_card_path,
          recurse = TRUE,
          glob = "*.wav"
        )

      wav_dates <-
        unique(
          stringr::str_extract(sd_wavs, "[0-9]{8}")
        )

      deployment_name <- val$deployment_name

      if (input$processing == "Sequential") {

        wav_dates |>
          purrr::walk(\(x)
                      CAbioacoustics:::create_subfolders(
                        x,
                        deployment_name,
                        input$hard_drive_path
                      )
          )

        sd_wavs |>
          purrr::walk(\(x)
                      CAbioacoustics:::sequential_wav_to_flac(
                        x,
                        deployment_name,
                        input$hard_drive_path
                      )
          )

      } else {

        wav_dates |>
          furrr::future_walk(\(x)
                             CAbioacoustics:::create_subfolders(
                               x,
                               deployment_name,
                               input$hard_drive_path
                             )
          )

        sd_wavs |>
          furrr::future_walk(\(x)
                             CAbioacoustics:::wav_to_flac(
                               x,
                               deployment_name,
                               input$desktop_path,
                               input$hard_drive_path
                             )
          )

      }

      run_time <-
        round(
          as.numeric(
            Sys.time() - start_time,
            units = "mins"
          ),
          1
        )

      output$runtime <- shiny::renderText({
        paste("Runtime (minutes):", run_time)
      })

      beepr::beep("ping")

    })

  }

  shiny::shinyApp(ui = ui, server = server)

}
