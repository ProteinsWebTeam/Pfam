# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi


# load MPI stuff                                                                                                                                                                
if [ -f /etc/modulefiles/openmpi-x86_64 ]; then
    module load openmpi-x86_64
fi


# User specific aliases and functions
export PATH=$PATH:/nfs/production/xfam/rfam/software/bin/:/nfs/production/xfam/rfam/production_software/rfam_production/Rfam/Scripts/make/
export PATH=$PATH:/nfs/production/xfam/rfam/software/mafft-7.127-with-extensions/core/bin/
export PATH=$PATH:/nfs/production/xfam/rfam/software/ViennaRNA-2.0.7/Progs/
export PERL5LIB=/nfs/production/xfam/pfam/software/Modules/PfamLib/:/nfs/production/xfam/rfam/production_software/perl5/lib/perl5/x86_64-linux-thread-multi:/nfs/production/xfam/rfam/production_software/perl5/lib/perl5:/nfs/production/xfam/rfam/production_software/rfam_production/Bio-Easel/blib/lib:/nfs/production/xfam/rfam/production_software/rfam_production/Bio-Easel/blib/arch:/nfs/production/xfam/rfam/production_software/rfam_production/Rfam/Lib:/nfs/production/xfam/rfam/production_software/rfam_production/Rfam/Schemata:/homes/jd/Rfam/Lib/:/homes/jd/Rfam/Schemata/:$PERL5LIB
#export PERL5LIB=/nfs/production/xfam/rfam/production_software/perl5/lib/perl5/x86_64-linux-thread-multi:/nfs/production/xfam/rfam/production_software/perl5/lib/perl5:/nfs/production/xfam/rfam/production_software/rfam_production/Rfam:/nfs/production/xfam/rfam/production_software/rfam_production/Bio-Easel/blib/lib:/nfs/production/xfam/rfam/production_software/rfam_production/Bio-Easel/blib/arch
export RFAM_CONFIG=/nfs/production/xfam/rfam/production_software/rfam_production/Rfam/Conf/rfam.conf
export PFAM_CONFIG=/nfs/production/xfam/pfam/data/pfam_svn.conf
alias l="less"
alias m="more"
alias seqstat="esl-seqstat"
alias sreformat="esl-reformat"
alias gsd="grep SEED species"
umask 0002
eval $(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)
export LD_LIBRARY_PATH=/usr/lib64/openmpi/lib:/sw/arch/dbtools/oracle/product/11.1.0.6.2/client/lib:/sw/arch/dbtools/oracle/product/11.1.0.6.2/client/rdbms/lib:/sw/arch/dbtools/oracle/product/11.1.0.6.2/client/jdbc/lib:/lib
