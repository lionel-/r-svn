### Extracting declarations
stopifnot(
    is.null(compiler:::declarations(NULL)),
    is.null(compiler:::declarations(quote(foo))),
    is.null(compiler:::declarations(quote({ foo }))),
    is.null(compiler:::declarations(quote({ NULL }))),
    is.null(compiler:::declarations(quote({ NULL; declare(foo()) })))
)

stopifnot(
    identical(
	compiler:::declarations(quote({ declare(foo(), bar()) })),
	alist(foo(), bar())
    )
)
