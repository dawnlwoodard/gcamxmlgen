#test_that("multiplication works", {
#  expect_equal(2 * 2, 4)
#})


test_that("get_path handles all cases correctly",{

  expect_error(gcamxmlgen::get_path(NULL, NULL, NULL))
  expect_equal(gcamxmlgen::get_path("node_name", "attr_name", list("attr1", "attr2")), "//node_name[@attr_name='attr1' or @attr_name='attr2']")

  expect_equal(gcamxmlgen::get_path(NULL, "attr_name", list("attr1", "attr2")), "//*[@attr_name='attr1' or @attr_name='attr2']")
  expect_equal(gcamxmlgen::get_path(NULL, "attr_name", NULL), "//*[@attr_name]")
  expect_equal(gcamxmlgen::get_path("node_name", "attr_name", NULL), "//node_name[@attr_name]")

  expect_equal(gcamxmlgen::get_path(NULL, NULL, list("attr1", "attr2")), "//*[@*='attr1' or @*='attr2']")
  expect_equal(gcamxmlgen::get_path("node_name", NULL, NULL), "//node_name")
  expect_equal(gcamxmlgen::get_path("node_name", NULL, list("attr1", "attr2")), "//node_name[@*='attr1' or @*='attr2']")

})

test_that("get_path works with multiple attributes",{
  expect_equal(gcamxmlgen::get_path("node_name", "attr_name", list("attr1", "attr2")), "//node_name[@attr_name=attr1 or @attr_name=attr2]")
})


