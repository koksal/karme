[Java Development Kit 8]: http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
[sbt]: https://github.com/sbt/sbt
[ScalaZ3]: https://github.com/epfl-lara/ScalaZ3
[R]: https://www.r-project.org/

# karme [![Circle CI](https://circleci.com/gh/koksal/karme.svg?style=svg)](https://circleci.com/gh/koksal/karme)
Ongoing project on inferring executable models from single-cell data.

## Requirements

1. Install the [Java Development Kit 8].
2. Install [sbt] and make sure the `sbt` binary is in your path.
3. Build [ScalaZ3], and put the packaged jar in `lib`.
3. Install [R] and execute `./scripts/setup-r-packages.R` to install required R libraries.

## Running

See `./scripts/run-t-cell-default-args.sh` for an example invocation of the tool.
