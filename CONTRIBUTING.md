# Contributing guidelines

The package development follows Hadley Whickham's book 
[R packages](http://r-pkgs.had.co.nz), we follow his advise for structuring 
the package, coding style and testing.

Regarding Git/Github workflow we follow *SEDESOL's Git Style Guide*.

### Code style
Follow Hadley's style as in [Code style](http://r-pkgs.had.co.nz/r.html#style).

Check the style before sending pull request with `lintr`:

```
install.packages("lintr")
lintr::lint_package()
```

### Testing
[testthat](http://r-pkgs.had.co.nz/tests.html)


### Advice for creating new functions
#### Run Queries Safely

To avoid SQL injection attacks, when queries depend on user input avoid using
`paste0` to create SQL commands (`dbGetQuery()`), instead consider:

1. Use a parameterised query with dbSendQuery() and dbBind():
  * When using PostgreSQL placeholders syntax uses `$` and a number indicating 
  the parameter(s) `$1, $2, ...`.

2. Use the sqlInterpolate() function to safely combine a SQL string with data

3. Manually escape the inputs using dbQuoteString()

One shall use option 1 whenever possible, then option 2 and finally option 3,
for they are ordered by level of safety.

Further explanation can be found in the [Run Queries Safely](https://db.rstudio.com/best-practices/run-queries-safely/) section of
RStudio Databases using R site.



### Git/Github Style guide
#### Branches
We use branches, certain branches can NOT be merged or pushed to without 
pull request:

1. master
2. develop
3. production

Other branches are named according to the following format:

`<group_token>/<lead_token>/<tracker-number>`


1. group_token: Where group tokens can be: bug, test, feature, junk, doc

2. lead_token: personal tag, selected according to the task to develop, e.g, 
fruit_prices, sagarpa, etl,... lead_token can be anything except master, develop, 
release or hotfix.

3. tracker-number: Jira, issue #, Trello task #

#### Commit messages
Shoud allow you can remember (and understand) what was done in future 
references.

First line should be used as an email title, when possible shorter than 50 
characters. 
The follwing lines will further explain the changes.
If more information is needed, use bullet points.
Language should be imperative, e.g. *Fix this bug*.

NEVER use `git add ` (all)

#### Pull Requests
Before doing pull request, check that the branch is correct, usually develop.
Keep pull request short.

Title:
<group> - <tracker-number> - Short Description

Body:
¿What does the pull request do?
¿What should be checked?
Give context to the changes.
¿What should be tested?

Indicate:
1. Erase branch
2. Squash commits


### Warnings

### Run Queries Safely
When creating a function we should be careful with SQL injection attacks, 
to avoid them, when queries depend on user input avoid using
`paste0` to create SQL commands (`dbGetQuery()`), instead consider:

1. Use a parameterised query with dbSendQuery() and dbBind():
  * When using PostgreSQL placeholders syntax uses `$` and a number indicating 
  the parameter(s) `$1, $2, ...`.

2. Use the sqlInterpolate() function to safely combine a SQL string with data

3. Manually escape the inputs using dbQuoteString()

One shall use option 1 whenever possible, then option 2 and finally option 3,
for they are ordered by level of safety.

Further explanation can be found in the [Run Queries Safely](https://db.rstudio.com/best-practices/run-queries-safely/) section of
RStudio Databases using R site
