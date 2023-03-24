if (.Platform$OS.type == "windows") {
    tools::assertError(Rprof(timer = "cpu"))
}

if (.Platform$OS.type != "windows") {
    local({
	file <- tempfile()
	Rprof(file, timer = "real")
	on.exit(Rprof(NULL))
	tools::assertError(system("ls", timeout = 1))
    })
}
