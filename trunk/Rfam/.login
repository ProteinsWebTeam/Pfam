#
if ($?path) then
    set path=($HOME/bin $path)
else
    set path=($HOME/bin /usr/bin .)
endif
#if ( ! ${?DT} ) then
#	stty dec new
#	tset -I -Q
#endif
set prompt="`hostname`> "
set mail=/usr/spool/mail/$USER
