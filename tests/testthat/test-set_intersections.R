context("test-set_intersections")


# Messages ----------------------------------------------------------------

emptySet <- tribble(
  ~item, ~A, ~B, ~C,
      1,  1,  0,  0,
      2,  1,  1,  0,
      3,  0,  1,  0
  )

test_that("set intersection messages", {
  expect_warning(getSetIntersections(emptySet, c("A","B","C"), "item"),
                 "Dropping empty sets: C")
})

# Analysis ----------------------------------------------------------------

simpleSets <- tribble(
  ~item, ~A, ~B, ~C,
      1,  1,  0,  0,
      2,  1,  1,  0,
      3,  0,  1,  1
)

setNames <- c("A","B","C")

test_that("set intersection analysis", {
  expect_equal(getSetIntersections(simpleSets, setNames, "item") %>%
                   select(set1, set2, Ninter),
                 tibble(set1 = factor(rep(setNames, each = 3)),
                        set2 = factor(rep(setNames, 3)),
                        Ninter = c(2,1,0,1,2,1,0,1,1)))
})



