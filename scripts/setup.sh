set -x
set -e
if [ ! -e ScalaZ3 ]; then
  git clone https://github.com/epfl-lara/ScalaZ3.git ScalaZ3
  cd ScalaZ3 && sbt package && cd ..
fi
