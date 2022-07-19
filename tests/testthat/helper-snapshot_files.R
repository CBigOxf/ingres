# Save a ggplot returned by code and return the path.
save_gg = function(code, width = 4, height = 2) {
  path = tempfile(fileext = ".png")
  ggsave(path, plot = code, width = width, height = height)
  path
}

# skips plot snapshot testing when not reproducible.
expect_snapshot_plot = function(name, code, width = 4, height = 2) {
  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  name = paste0(name, ".png")

  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  path = save_gg(code, width, height)
  expect_snapshot_file(path, name)
}
