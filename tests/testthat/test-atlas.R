library(ggseg)
library(ggseg3d)
library(ggplot2)

# ggseg ----
context("test-palettes")
test_that("check new palettes work", {
  expect_equal(length(brain_pal("flechsig", package = "ggsegFlechsig")), 46)

  expect_error(brain_pal("flechsig"), "not a valid")

  expect_true(all(brain_regions(flechsig) %in% names(brain_pal("flechsig", package = "ggsegFlechsig"))))
})

context("test-ggseg-atlas")
test_that("atlases are true ggseg atlases", {

  expect_true(is_brain_atlas(flechsig))

})

context("test-ggseg")
test_that("Check that polygon atlases are working", {
  expect_is(ggseg(atlas = flechsig),c("gg","ggplot"))

  expect_is(ggseg(atlas = flechsig, mapping = aes(fill = region)),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = flechsig, mapping = aes(fill = region)) +
              scale_fill_brain("flechsig", package = "ggsegFlechsig"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = flechsig, mapping = aes(fill = region)) +
              scale_fill_brain("flechsig", package = "ggsegFlechsig"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = flechsig, mapping=aes(fill=region), adapt_scales = FALSE ),c("gg","ggplot"))

})


# ggseg3d ----
context("test-ggseg3d")
test_that("Check that mesh atlases are working", {
  expect_is(
    ggseg3d(atlas=flechsig_3d),
    c("plotly", "htmlwidget")
  )
})



context("test-ggseg3d-atlas")
test_that("atlases are true ggseg3d atlases", {

  expect_true(is_ggseg3d_atlas(flechsig_3d))

})
