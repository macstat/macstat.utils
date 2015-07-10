\name{stat.opisowe}
\alias{stat.opisowe}
\title{Tables with descriptive statistics}
\usage{
stat.opisowe(data, x, g="")
}
\description{
Checks if x is qualitative or quantitative variable, and gives tables with counts and percentages or mean, sd, medianm 1st, 3rd quartile. If categorical variable g is specified tables are showed for each of subgroups.
}
\examples{
stat,opisowe(mtcars, mtcars$mpg)
stat,opisowe(mtcars, mtcars$mpg, mtcars$cyl)
}
