#if [ ! "$DT" ]; then
#	stty dec
#	tset -I -Q
#fi
PS1="`hostname`> "
MAIL=/usr/spool/mail/$USER
echo $PATH | /bin/grep -q "$HOME/bin" ||
{
	PATH=$HOME/bin:${PATH:-/usr/bin:.}
	export PATH
}
