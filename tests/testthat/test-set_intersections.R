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

test_that("set intersection simple", {
  expect_equal(getSetIntersections(simpleSets, setNames, "item") %>%
                   select(set1, set2, Ninter),
                 tibble(set1 = factor(rep(setNames, each = 3)),
                        set2 = factor(rep(setNames, 3)),
                        Ninter = c(2,1,0,1,2,1,0,1,1)))
})

n <- 1e3
n1 <- n / 2
n2 <- n / 2
pop1 <- 1:n1
pop2 <- (n1 + 1):(n1 + n2)
setA <- c(sample(pop1, n1 / 4), sample(pop2, 3*n2 / 4))
setB <- c(sample(pop1, 2*n1 / 3), sample(pop2, n2 / 3))
setC <- c(sample(pop1, n1 / 5), sample(pop2, 4*n2 / 5))

complexSets <-
  tibble(item = c(setA,setB,setC),
       set = c(rep("A",length(setA)), rep("B",length(setB)), rep("C",length(setC))),
       value = 1) %>%
  distinct() %>%
  tidyr::spread(set, value, fill = 0)

test_that("set intersection complex", {
  expect_equal(getSetIntersections(complexSets, setNames, "item") %>%
                 select(set1, set2, Ntotal, Ninter, Nunion, N1, N2, prop, prop1),
               tibble(set1 = factor(rep(setNames, each = 3)),
                      set2 = factor(rep(setNames, 3)),
                      Ntotal = length(union(c(setA,setB),setC)),
                      Ninter = c(length(intersect(setA,setA)),
                                length(intersect(setA,setB)),
                                length(intersect(setA,setC)),
                                length(intersect(setB,setA)),
                                length(intersect(setB,setB)),
                                length(intersect(setB,setC)),
                                length(intersect(setC,setA)),
                                length(intersect(setC,setB)),
                                length(intersect(setC,setC))),
                      Nunion = c(length(union(setA,setA)),
                                 length(union(setA,setB)),
                                 length(union(setA,setC)),
                                 length(union(setB,setA)),
                                 length(union(setB,setB)),
                                 length(union(setB,setC)),
                                 length(union(setC,setA)),
                                 length(union(setC,setB)),
                                 length(union(setC,setC))),
                      N1 = rep(c(length(setA), length(setB), length(setC)), each = 3),
                      N2 = rep(c(length(setA), length(setB), length(setC)), 3)) %>%
                 mutate(prop = Ninter/Nunion,
                        prop1 = Ninter/N1) %>%
                 mutate_if(is.numeric, as.double))
})

