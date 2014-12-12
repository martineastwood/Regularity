## Regularity - Friendly Regular Expressions For R

Regularity is a friendly regular expression builder for R inspired by Ruby's 
[Regularity library](https://github.com/andrewberls/regularity) and
R's [Magrittr package](https://github.com/smbache/magrittr) package. 

What's simpler to create / decipher?

```
/^[0-9]{3}-[A-Za-z]{2}#?[a|b]a{2,4}\$$/
```

Or

```R
Regularity() %>%
StartWith(3, 'digits') %>%
Then('-') %>%
Then(2, 'letters') %>%
Maybe('#') %>%
OneOf(c('a','b')) %>%
Between(c(2,4), 'a') %>%
EndWith('$')
```
I know which I'd choose!

### Installation
Regularity is not currently on CRAN as it's still in early development but in the meantime it can be installed in R using devtools

```R
install_github("martineastwood/Regularity")
```

### Usage

All you need to do is create a Regularity object and then chain the 
regex functions together. The functions either take a single pattern, e.g. `Then("xyz")`,
or a numbered constraint such as `Then(2, 'digits')`.

The following special identifers are supported:

```R
digit        <- '[0-9]'
lowercase    <- '[a-z]'
uppercase    <- '[A-Z]'
letter       <- '[A-Za-z]'
alphanumeric <- '[A-Za-z0-9]'
whitespace   <- '\s'
space        <- ' '
tab          <- '\t'
```

Also, it doesn't matter if these identifiers are pluralized, i.e. `Then(2, 'letters')` works just 
the same as `Then(1, 'letter')`

### Functions

The following functions are currently supported:

`StartWith(pattern)`: The line must start with the specified pattern. This must be called before any of the other functions. (Also aliased to `StartsWith`).

`Append(pattern)`: Append a pattern to the end (Also aliased to `Then`), e.g. `Append('abc')`

`EndWith(pattern)`: The line must end with the specified pattern. This must be the final function called, e.g. `EndWith('X')`

`Maybe(pattern)`: Zero or one of the specified pattern, e.g. `Maybe(4, 'digits')`

`OneOf(values)`: Specify a choice, e.g. `OneOf(c('a', 'b', 'c'))`

`Between(range, pattern)`: Specify a bounded repetition, e.g. `between(c(2,4), 'digits')`

`ZeroOrMore(pattern)`: Specify that the pattern or identifer should appear zero or many times, e.g. `ZeroOrMore('letters')`

`OneOrMore(pattern)`: Specify that the pattern or identifer should appear one or many times, e.g. `OneOrMore('letters')`

`AtLeast(n, pattern)`: Specify that the pattern or identifer should appear n or more times, e.g. `AtLeast(5, 'letters')`