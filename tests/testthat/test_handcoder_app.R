library(testthat)
library(shinytest2)
library(handcodeR)

# Initialize input 1 category
a <- list(
  container = data.frame(kat1 = factor(rep("", 3),
                                       levels = c("",
                                                  "Not applicable",
                                                  "category 1",
                                                  "category 2")),
                         kat2 = factor(rep("", 3)),
                         kat3 = factor(rep("", 3)),
                         kat4 = factor(rep("", 3)),
                         kat5 = factor(rep("", 3)),
                         kat6 = factor(rep("", 3))),
  data_app = data.frame(id = seq(1,3),
                        before = c("", "Text 1", "Text 2"),
                        after = c("Text 2", "Text 3", ""),
                        texts = c("Text 1", "Text 2", "Text 3"),
                        kat1 = factor(rep("", 3),
                                      levels = c("",
                                                 "Not applicable",
                                                 "category 1",
                                                 "category 2"))),
  start_app = 1,
  classifications = list(kat1 = c("",
                                  "Not applicable",
                                  "category 1",
                                  "category 2")),
  context_app = FALSE
)

# Initialize App
shiny_app <- handcoder_app(a)

test_that("Test nextpage, 1 category", {
  skip_on_cran()

  app <- AppDriver$new(shiny_app, name = "handcoder_app1",
                       variant = platform_variant())

  # Click category 1
  app$set_inputs(code1 = "category 1")
  app$expect_values()

  # Click nextpage to get to page 2/3
  app$click("nextpage")
  app$expect_values()

  # Click previouspage to get to page 1/3
  app$click("previouspage")
  app$expect_values()

  # Click nextpage to get to page 2/3
  app$click("nextpage")
  app$expect_values()

  # Click category 2
  app$set_inputs(code1 = "category 2")
  app$expect_values()

  # Click nextpage to get to page 3/3
  app$click("nextpage")
  app$expect_values()

  # Click nextpage to save and exit
  app$click("nextpage")

})


test_that("Test save and exit, 1 category", {
  skip_on_cran()

  app <- AppDriver$new(shiny_app, name = "handcoder_app2",
                       variant = platform_variant())

  # Click category 1
  app$set_inputs(code1 = "Not applicable")
  app$expect_values()

  # Click save to save and exit
  app$click("save")
})



# Initialize input 2 categories
b <- list(
  container = data.frame(kat1 = factor(rep("", 3),
                                       levels = c("",
                                                  "Not applicable",
                                                  "category 1",
                                                  "category 2")),
                         kat2 = factor(rep("", 3),
                                       levels = c("",
                                                  "Not applicable",
                                                  "kategorie 1",
                                                  "kategorie 2")),
                         kat3 = factor(rep("", 3)),
                         kat4 = factor(rep("", 3)),
                         kat5 = factor(rep("", 3)),
                         kat6 = factor(rep("", 3))),
  data_app = data.frame(id = seq(1,3),
                        before = c("", "Text 1", "Text 2"),
                        after = c("Text 2", "Text 3", ""),
                        texts = c("Text 1", "Text 2", "Text 3"),
                        kat1 = factor(rep("", 3),
                                      levels = c("",
                                                 "Not applicable",
                                                 "category 1",
                                                 "category 2")),
                        kat2 = factor(rep("", 3),
                                      levels = c("",
                                                 "Not applicable",
                                                 "kategorie 1",
                                                 "kategorie 2"))),
  start_app = 2,
  classifications = list(kat1 = c("",
                                  "Not applicable",
                                  "category 1",
                                  "category 2"),
                         kat2 = c("",
                                  "Not applicable",
                                  "kategorie 1",
                                  "kategorie 2")),
  context_app = TRUE
)

# Initialize App
shiny_app2 <- handcoder_app(b)


test_that("Test 2 categories",{
  skip_on_cran()

  app <- AppDriver$new(shiny_app2, name = "handcoder_app3",
                       variant = platform_variant())

  # Click category 1
  app$set_inputs(code1 = "category 1")
  app$expect_values()

  # Click category 2
  app$set_inputs(code2 = "kategorie 2")
  app$expect_values()

  # Click nextpage to get to page 3/3
  app$click("nextpage")
  app$expect_values()

  # Click previouspage to get to page 2/3
  app$click("previouspage")
  app$expect_values()
})


# Initialize input 2 categories
c <- list(
  container = data.frame(kat1 = factor(rep("", 3),
                                       levels = c("",
                                                  "Not applicable",
                                                  "category 1",
                                                  "category 2")),
                         kat2 = factor(rep("", 3),
                                       levels = c("",
                                                  "Not applicable",
                                                  "kategorie 1",
                                                  "kategorie 2")),
                         kat3 = factor(rep("", 3),
                                       levels = c("",
                                                  "Not applicable",
                                                  "categoria 1",
                                                  "categoria 2",
                                                  "categoria 3")),
                         kat4 = factor(rep("", 3),
                                       levels = c("",
                                                  "Not applicable",
                                                  "apple",
                                                  "banana")),
                         kat5 = factor(rep("", 3),
                                       levels = c("",
                                                  "Not applicable",
                                                  "circle",
                                                  "square")),
                         kat6 = factor(rep("", 3),
                                       levels = c("",
                                                  "Not applicable",
                                                  "water",
                                                  "fire"))),
  data_app = data.frame(id = seq(1,3),
                        before = c("", "Text 1", "Text 2"),
                        after = c("Text 2", "Text 3", ""),
                        texts = c("Text 1", "Text 2", "Text 3"),
                        kat1 = factor(rep("", 3),
                                      levels = c("",
                                                 "Not applicable",
                                                 "category 1",
                                                 "category 2")),
                        kat2 = factor(rep("", 3),
                                      levels = c("",
                                                 "Not applicable",
                                                 "kategorie 1",
                                                 "kategorie 2")),
                        kat3 = factor(rep("", 3),
                                      levels = c("",
                                                 "Not applicable",
                                                 "categoria 1",
                                                 "categoria 2",
                                                 "categoria 3")),
                        kat4 = factor(rep("", 3),
                                      levels = c("",
                                                 "Not applicable",
                                                 "apple",
                                                 "banana")),
                        kat5 = factor(rep("", 3),
                                      levels = c("",
                                                 "Not applicable",
                                                 "circle",
                                                 "square")),
                        kat6 = factor(rep("", 3),
                                      levels = c("",
                                                 "Not applicable",
                                                 "water",
                                                 "fire"))),
  start_app = 2,
  classifications = list(kat1 = c("",
                                  "Not applicable",
                                  "category 1",
                                  "category 2"),
                         kat2 = c("",
                                  "Not applicable",
                                  "kategorie 1",
                                  "kategorie 2"),
                         kat3 = c("",
                                  "Not applicable",
                                  "categoria 1",
                                  "categoria 2",
                                  "categoria 3"),
                         kat4 = c("",
                                  "Not applicable",
                                  "apple",
                                  "banana"),
                         kat5 = c("",
                                  "Not applicable",
                                  "circle",
                                  "square"),
                         kat6 = c("",
                                  "Not applicable",
                                  "water",
                                  "fire")),
  context_app = TRUE
)

# Initialize App
shiny_app3 <- handcoder_app(c)


test_that("Test 6 categories",{
  skip_on_cran()

  app <- AppDriver$new(shiny_app3, name = "handcoder_app4",
                       variant = platform_variant())

  # Click category 1
  app$set_inputs(code1 = "category 1")
  app$expect_values()

  # Click category 2
  app$set_inputs(code2 = "kategorie 2")
  app$expect_values()

  # Click category 3
  app$set_inputs(code3 = "categoria 3")
  app$expect_values()

  # Click category 4
  app$set_inputs(code4 = "banana")
  app$expect_values()

  # Click category 5
  app$set_inputs(code5 = "circle")
  app$expect_values()

  # Click category 6
  app$set_inputs(code6 = "fire")
  app$expect_values()

  # Click nextpage to get to page 3/3
  app$click("nextpage")
  app$expect_values()

  # Click previouspage to get to page 2/3
  app$click("previouspage")
  app$expect_values()
})



# Test Server -------------------------------------------------------------


test_that("Test shiny server nextpage",{
  shiny::testServer(shiny_app, {
    # First Text displayed is first Text
    expect_equal(output$statement, a$data_app$texts[1])

    # Clicking categories generates codes
    session$setInputs(code1 = "category 1")
    expect_equal(input$code1, "category 1")

    # Clicking nextpage leads to next text and resets code
    session$setInputs(nextpage = input$nextpage + 1)
    expect_equal(output$statement, a$data_app$texts[2])

    # Clicking previouspage leads back to first text
    session$setInputs(previouspage = input$previouspage + 1)
    expect_equal(output$statement, a$data_app$texts[1])

    # Clicking nextpage until last page leads to exit
    session$setInputs(nextpage = input$nextpage + 1)
    session$setInputs(nextpage = input$nextpage + 1)
    expect_equal(values$counter, 3)

  })
})


test_that("Test shiny server context",{
  shiny::testServer(shiny_app2, {
    # First Text displayed is second Text (Start value is 2)
    expect_equal(output$statement, paste0("<font color =\"#C0C0C0\">", b$data_app$before[2], "</font> <b>", b$data_app$texts[2], "</b> <font color =\"#C0C0C0\">", b$data_app$after[2], "</font>"))
  })
})
















