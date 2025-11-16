# Manage commands to be evaluated

Functions to manage commands to be evaluated.

## Usage

``` r
.textEval(cmd, envir)
```

## Arguments

- cmd:

  A character vector containing commands to be executed.

- envir:

  An environment in which to execute the commands.

## Value

`.textEval` returns the output of `eval(parse(text=cmd), envir)`, unless
`cmd` is empty in which case it returns `NULL`.

## Author

Aaron Lun, Kevin Rue-Albrecht

## Examples

``` r
myenv <- new.env()
myenv$x <- "Hello world!"
.textEval("print(x)", myenv)
#> [1] "Hello world!"
```
