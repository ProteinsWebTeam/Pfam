# example configuration file for the Rfam code base
export RFAM_ROOT=[PATH-TO-CODE-CHECKOUT]
#override parameters with a rfam_local.conf
export RFAM_CONFIG=$RFAM_ROOT/Conf/rfam.conf
export PERL5LIB=$RFAM_ROOT/Lib:$RFAM_ROOT/Schemata:$PERL5LIB
export PATH=[BINDIRS]/bin:$RFAM_ROOT/Scripts/make:$PATH
#export LD_LIBRARY_PATH= if somewhere else.
