#!/bin/bash

nasthome=$(dirname $(dirname $(readlink -f $0)))

# TEST ARGUMENT

if [ $# == "0" ]; then
    echo "you must pass an argument"
    exit 1
elif [ $# != "1" ]; then
    echo "you must pass an unique argument"
    exit 2
fi

if [ ! -f "$1" ]; then
    echo "passed argument must be a file"
    exit 3
fi

# ENV VARIABLES

export RFDIR=$nasthome/rf
export DIRCTY="${TMPDIR:-/tmp}"

export FT05=$1

if [[ $1 =~ .*[a-zA-Z0-9]\.[a-zA-Z0-9].* ]]; then
    # removing extension
    progname=${1%.*}
else
    progname=$1
fi

export FT06=$progname.out
export NPTPNM=$progname.nptp
export PLTNM=$progname.plt
export PUNCHNM=$progname.pnh
export OUT11=$progname.out11
export IN12=$progname.in12
export LOGNM=$progname.log
export SOF1=$progname.sof1
export SOF2=$progname.sof2

# RUN NASTRAN

$nasthome/mds/nastran
#gdb $nasthome/mds/nastran


