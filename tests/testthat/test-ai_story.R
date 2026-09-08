# Offline coverage for R/ai_story.R.
# The LLM call lives in get_ai_story(); everything else (layout selection, prompt
# assembly, and the officer slide insertion) is pure and tested here without any
# network access. The end-to-end LLM path is exercised only when an API key is
# present.

basic_deck <- function() {
  officer::read_pptx(
    file.path(system.file(package = "autoslider.core"), "theme/basic.pptx")
  )
}

# Titles in *display* (presentation) order. officer::slide_summary() indexes the
# physical slide files, which do not follow the presentation order after
# move_slide(); presentation$slide_data() is the source of truth for order.
display_titles <- function(ppt) {
  sd <- ppt$presentation$slide_data()
  phys <- as.integer(gsub("[^0-9]", "", basename(sd$target)))
  vapply(phys, function(i) {
    sm <- officer::slide_summary(ppt, index = i)
    paste(sm$text[!is.na(sm$text)], collapse = " | ")
  }, character(1))
}

test_that("story_layout_choices returns only safe layouts present in the template", {
  ppt <- basic_deck()
  choices <- autoslider.core:::story_layout_choices(ppt)
  expect_true(all(choices %in% c("Section Header", "Title and Content", "Title Only")))
  expect_true(all(choices %in% officer::layout_summary(ppt)$layout))
  # basic.pptx ships all three.
  expect_setequal(choices, c("Section Header", "Title and Content", "Title Only"))
})

test_that("build_story_prompt embeds table text and the allowed layout names", {
  adsl <- eg_adsl |>
    dplyr::mutate(TRT01A = factor(TRT01A, levels = c("A: Drug X", "B: Placebo", "C: Combination")))
  out <- t_dm_slide(adsl = adsl) |> decorate(title = "Demographic table", footnote = "")

  prompt <- autoslider.core:::build_story_prompt(
    list(dm = out),
    allowed_layouts = c("Section Header", "Title and Content"),
    max_slides = 3L
  )
  expect_type(prompt, "character")
  expect_length(prompt, 1)
  expect_true(grepl("Section Header", prompt, fixed = TRUE))
  expect_true(grepl("Title and Content", prompt, fixed = TRUE))
  # the output name and some table content flow through
  expect_true(grepl("dm", prompt, fixed = TRUE))
  expect_true(grepl("Age", prompt, fixed = TRUE) || grepl("SEX|Sex", prompt))
})

test_that("build_story_prompt skips autoslider_error and non-table outputs", {
  err <- autoslider_error(
    "boom",
    spec = list(program = "t_dm_slide", suffix = "FAS", output = "err_out"),
    step = "generate"
  )
  prompt <- autoslider.core:::build_story_prompt(
    list(err_out = err),
    allowed_layouts = c("Title and Content"),
    max_slides = 2L
  )
  expect_type(prompt, "character")
  expect_false(grepl("err_out", prompt, fixed = TRUE))
})

test_that("add_story_slides puts summary at the front and conclusions at the end", {
  ppt <- basic_deck()
  n0 <- length(ppt)

  story <- list(
    summary = list(
      list(layout = "Section Header", title = "SUMMARY_DIVIDER", bullets = character(0)),
      list(layout = "Title and Content", title = "SUMMARY_KEY", bullets = c("point a", "point b"))
    ),
    conclusions = list(
      list(layout = "Title and Content", title = "CONCLUSION_1", bullets = c("done"))
    )
  )

  ppt <- autoslider.core:::add_story_slides(ppt, story)
  expect_s3_class(ppt, "rpptx")
  expect_equal(length(ppt), n0 + 3)

  titles <- display_titles(ppt)

  # First two slides are the summary section, in order.
  expect_true(grepl("SUMMARY_DIVIDER", titles[1]))
  expect_true(grepl("SUMMARY_KEY", titles[2]))
  # Last slide is the conclusion.
  expect_true(grepl("CONCLUSION_1", titles[length(titles)]))
})

test_that("add_story_slides coerces a Title Only slide carrying bullets to keep content", {
  ppt <- basic_deck()
  n0 <- length(ppt)
  story <- list(
    summary = list(),
    conclusions = list(
      list(layout = "Title Only", title = "HAS_BULLETS", bullets = c("kept one", "kept two"))
    )
  )
  ppt <- autoslider.core:::add_story_slides(ppt, story)
  expect_equal(length(ppt), n0 + 1)

  sm <- officer::slide_summary(ppt, index = length(ppt))
  all_text <- paste(sm$text[!is.na(sm$text)], collapse = " | ")
  # bullets survived (they were not dropped for lack of a body placeholder)
  expect_true(grepl("kept one", all_text))
  expect_true(grepl("kept two", all_text))
})

test_that("add_story_slides tolerates empty sections", {
  ppt <- basic_deck()
  n0 <- length(ppt)
  expect_equal(length(autoslider.core:::add_story_slides(ppt, list())), n0)
})

test_that("story_via_json parses a JSON reply, stripping reasoning and fences", {
  # Stub chat: mimics a provider (e.g. DeepSeek/ollama) that has no native
  # structured output and returns JSON wrapped in a <think> block and a fence,
  # with a little prose around it.
  fake_chat <- list(
    chat = function(prompt, echo = NULL) {
      paste0(
        "<think>the user wants slides</think>\n",
        "Here you go:\n```json\n",
        '{"summary":[{"layout":"Section Header","title":"S","bullets":[]}],',
        '"conclusions":[{"layout":"Title and Content","title":"C","bullets":["x","y"]}]}',
        "\n```\nHope that helps!"
      )
    }
  )
  res <- autoslider.core:::story_via_json(
    fake_chat, "PROMPT", c("Section Header", "Title and Content")
  )
  expect_named(res, c("summary", "conclusions"))
  expect_equal(res$summary[[1]]$title, "S")
  expect_equal(res$conclusions[[1]]$bullets, c("x", "y"))
})

test_that("add_ai_story validates its inputs before any network call", {
  expect_error(
    add_ai_story(outputs = "not a list", infile = tempfile(fileext = ".pptx")),
    "outputs"
  )
  expect_error(
    add_ai_story(outputs = list(), infile = tempfile(fileext = ".pptx")),
    "infile"
  )
})

test_that("get_ai_story end-to-end (requires ANTHROPIC_API_KEY)", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "ANTHROPIC_API_KEY not set")

  adsl <- eg_adsl |>
    dplyr::mutate(TRT01A = factor(TRT01A, levels = c("A: Drug X", "B: Placebo", "C: Combination")))
  out <- t_dm_slide(adsl = adsl) |> decorate(title = "Demographic table", footnote = "")

  story <- get_ai_story(
    list(dm = out),
    allowed_layouts = c("Section Header", "Title and Content", "Title Only"),
    platform = "anthropic",
    model = "claude-haiku-4-5",
    max_slides = 2L
  )
  expect_named(story, c("summary", "conclusions"))
  expect_true(all(vapply(story$summary, function(s) s$layout %in%
    c("Section Header", "Title and Content", "Title Only"), logical(1))))
})
