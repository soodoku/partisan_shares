test_that("no identifying columns ship in data/", {
  banned <- c(
    "workerid", "responseid", "assignmentid", "ipaddress", "latitude", "longitude",
    "startdate", "enddate", "recordeddate", "browser", "comment", "text", "email", "name"
  )
  for (f in list.files(data_path(), pattern = "\\.csv$", full.names = TRUE)) {
    cols <- tolower(names(readr::read_csv(f, n_max = 0, show_col_types = FALSE)))
    expect_false(any(vapply(banned, function(b) any(grepl(b, cols, fixed = TRUE)), logical(1))), label = basename(f))
  }
})

test_that("no MTurk worker IDs or AWS keys appear anywhere in the tracked text", {
  files <- list.files(project_root, recursive = TRUE, pattern = "\\.(csv|R|tex|md)$", full.names = TRUE)
  files <- files[!grepl("/renv/", files)]
  text <- unlist(lapply(files, readLines, warn = FALSE))
  expect_false(any(grepl("\\bA[0-9A-Z]{12,14}\\b", text)))
  expect_false(any(grepl("AKIA[0-9A-Z]{16}", text)))
})
