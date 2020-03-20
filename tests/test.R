app <- ShinyDriver$new("../", loadTimeout = 100000)
app$snapshotInit("test")

app$snapshot(list(output = "box.sick"))
app$snapshot(list(output = "box.death"))
