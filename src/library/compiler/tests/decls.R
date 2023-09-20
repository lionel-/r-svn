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


### Declaring local variables

## Workaround because warnings do not currently use the condition system
expectSilent <- function(expr) {
    out <- capture.output(invisible(expr))
    if (length(out) != 0L)
	stop(paste0("Unexpected output:\n", paste0(out, collapse = "\n")))
}
expectOutput <- function(expr, pattern) {
    out <- capture.output(invisible(expr))
    if (!any(grepl(pattern, out)))
	stop("Expected output did not occur: '", pattern, "'")
}

## By default we warn
expr <- quote(function() bar )
expectOutput(
    compiler::compile(expr, options = list(suppressAll = FALSE)),
    "no visible binding for global variable 'bar'"
)

## Wrong declaration type warns
expr <- quote(function() {
    declare(variables("foo"))
})
expectOutput(
    compiler::compile(expr, options = list(suppressAll = FALSE)),
    "declaration must be a symbol"
)

## Declaring variable disables the warning
expr <- quote(function() {
    declare(variables(foo, bar))
    bar
})
expectSilent(
    compiler::compile(expr, options = list(suppressAll = FALSE))
)

## Can declare in nested blocks
expr <- quote(function() {
    NULL
    {
	declare(variables(foo))
	foo
    }
})
expectSilent(
    compiler::compile(expr, options = list(suppressAll = FALSE))
)

## Declaration is popped out after end of block
expr <- quote(function() {
    {
	declare(variables(foo))
	foo
    }
    foo
})
expectOutput(
    compiler::compile(expr, options = list(suppressAll = FALSE)),
    "no visible binding for global variable 'foo' "
)
