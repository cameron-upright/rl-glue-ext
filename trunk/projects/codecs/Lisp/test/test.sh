### It is a helper script for the test scripts.

function usage {
cat << EOF

    $0 [options]

    Options:

        -h
        Prints this help screen.

        -v
        Turns verbose mode on.

EOF
}

### Parsing parameters. #######################################################

verbose=
while getopts "hv" flag
do
    case $flag in
        h )
            ;;
        v )
            verbose=1
            ;;
        * )
            echo "Invalid option: $flag"
            echo "See $0 -h for valid options."
            exit -2
            ;;
    esac
done

### Checks wheter the source is compiled. #####################################

make -q >/dev/null
if [ $? -ne 0 ]; then
    echo "First $TEST should be compiled by 'make'."
    exit -1
fi

### Function definitions. #####################################################

function vprint {
    local text=$1
    local newline=$2

    if [ -n "$verbose" ]; then
        if [ -n "$newline" ]; then
            echo $text
        else
            echo -n $text
        fi
    fi
}

function run_test {
    local agent=$1
    local environment=$2
    local experiment=$3

    vprint "" 1
    vprint "Test $TEST ."
    rl_glue &
    sleep 1
    vprint "."
    $agent &
    sleep 1
    vprint "."
    $environment &
    sleep 1
    vprint "."
    $experiment
    local result=$?
    vprint "done" 1

    vprint "Result: $result" 1
    vprint "" 1

    return $result
}

