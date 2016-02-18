TIMESTAMP=`date +%F-%H-%M-%S`
JOBS=8
FILESELECTION=$1
SCRIPT=$2
shift; shift
ARGS="$@"

find $FILESELECTION -maxdepth 0 | parallel --gnu --use-cpus-instead-of-cores --jobs $JOBS --results log/$TIMESTAMP $SCRIPT {} $ARGS
