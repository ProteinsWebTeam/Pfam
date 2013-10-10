#!/bin/bash -x

export OUTPUT=$WORKSPACE/logs
export DISPLAY=:1
export PATH=/opt/bin:$PATH

cp /Users/finnr/Work/Projects/Rfam/NewStuff/code/trunk/Rfam/Conf/rfam_local.conf $WORKSPACE/Conf

export PERL5LIB=$WORKSPACE/Schemata:$WORKSPACE/Lib:/Users/finnr/Work/Projects/Rfam/NewStuff/code/trunk/Bio-Easel/blib/lib:/Users/finnr/Work/Projects/Rfam/NewStuff/code/trunk/Bio-Easel/blib/arch
export RFAM_CONF=$WORKSPACE/Conf/rfam.conf
/opt/bin/prove -v $WORKSPACE/Tests/*.t > $OUTPUT/tests.xml
