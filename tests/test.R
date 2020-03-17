app <- ShinyDriver$new("../", loadTimeout = 1e+05)
app$snapshotInit("test")

app$setInputs(sidebarCollapsed = TRUE)
app$setInputs(sidebarCollapsed = FALSE)
app$snapshot()
