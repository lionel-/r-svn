### Extracting declarations
cenv <- compiler:::makeCenv(.GlobalEnv)
cntxt <- compiler:::make.toplevelContext(cenv, NULL)
stopifnot(
    is.null(compiler:::declarations(NULL, cntxt)),
    is.null(compiler:::declarations(quote(foo), cntxt)),
    is.null(compiler:::declarations(quote({ foo }), cntxt)),
    is.null(compiler:::declarations(quote({ NULL }), cntxt)),
    is.null(compiler:::declarations(quote({ NULL; declare(foo()) }), cntxt))
)

stopifnot(
    identical(
	compiler:::declarations(quote({ declare(foo(), bar()) }), cntxt),
	alist(foo(), bar())
    )
)

## Ignore declarations if `declare()` is not in scope
decls <- local({
    declare <- function(...) NULL
    cenv <- compiler:::makeCenv(environment())
    cntxt <- compiler:::make.toplevelContext(cenv, NULL)
    compiler:::declarations(quote({ declare(foo()) }), cntxt)
})
stopifnot(is.null(decls))


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

## Wrong declaration type is ignored
expr <- quote(function() {
    declare(variables("foo"))
})
expectSilent(compiler::compile(expr, options = list(suppressAll = FALSE)))

## Declaring variable disables the warning
expr <- quote(function() {
    declare(variables(foo, bar))
    bar
})
expectSilent(
    compiler::compile(expr, options = list(suppressAll = FALSE))
)

## If `declare()` is not in scope, ignore it. We get three warnings,
## including one about `foo` within the `declare()`
local({
    declare <- function(...) NULL
    expectOutput(
	compile(expr, env = environment(), options = list(suppressAll = FALSE)),
	"no visible binding for global variable 'foo' "
    )
})

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


### Quoted args

cenv <- compiler:::makeCenv(.GlobalEnv)
cntxt <- compiler:::make.toplevelContext(cenv, list(suppressAll = FALSE))

do <- function(e) {
    cntxt$env <- compiler:::addCenvVars(cenv, compiler:::findLocals(e, cntxt))
    compiler:::genCode(e, cntxt)
}

# Global quoting functions, one annotated, the other not
myQuote <- function(x) {
    declare(params(x = quote()))
    substitute(x)
}
myQuote2 <- function(x, y) {
    declare(params(y = quote()))
    substitute(y)
}
myBareQuote <- function(x) {
    substitute(x)
}

expr <- quote(myBareQuote(foo))
expectOutput(do(expr), "no visible binding for global variable 'foo' ")

expr <- quote(bquote(foo))
expectSilent(do(expr))

expr <- quote(bquote(foo, bar))
expectOutput(do(expr), "no visible binding for global variable 'bar'")

expr <- quote(myQuote(foo))
expectSilent(do(expr))

expr <- quote(myQuote2(, foo))
expectSilent(do(expr))

expr <- quote(myQuote2(y = foo))
expectSilent(do(expr))

expr <- quote(myQuote2(foo, bar))
expectOutput(do(expr), "no visible binding for global variable 'foo' ")
