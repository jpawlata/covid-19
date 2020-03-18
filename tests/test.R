app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$snapshot(list(output = "box.sick"))
app$snapshot(list(output = "box.death"))
