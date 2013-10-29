#!/bin/bash -x

export OUTPUT=$WORKSPACE/logs
export DISPLAY=:1
export PATH=/opt/bin:$PATH

cp /Users/finnr/Work/Projects/Rfam/NewStuff/code/trunk/Rfam/Conf/rfam_local.conf $WORKSPACE/Conf
mkdir $OUTPUT

export PERL5LIB=$WORKSPACE/Schemata:$WORKSPACE/Lib:/Users/finnr/Work/Projects/Rfam/NewStuff/code/trunk/Bio-Easel/blib/lib:/Users/finnr/Work/Projects/Rfam/NewStuff/code/trunk/Bio-Easel/blib/arch
export RFAM_CONFIG=$WORKSPACE/Conf/rfam.conf

HARNESS_PERL_SWITCHES=-MDevel::Cover=-silent,on,-db,$OUTPUT/rfam_cover /opt/bin/prove --formatter TAP::Formatter::JUnit -v $WORKSPACE/Tests/*.t 2> /dev/null > $OUTPUT/tests.xml
/opt/bin/cover $OUTPUT/ipfam_cover

#/opt/bin/prove --formatter TAP::Formatter::JUnit -v $WORKSPACE/Tests/*.t > $OUTPUT/tests.xml
