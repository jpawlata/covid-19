app <- ShinyDriver$new("../", loadTimeout = 100000)
app$snapshotInit("test")

app$snapshot(list(output = "box.sick"))
app$snapshot(list(output = "box.death"))
app$snapshot(list(output = "box.recovered"))
app$setInputs(sidebarCollapsed = TRUE)
app$setInputs(sidebarCollapsed = FALSE)
