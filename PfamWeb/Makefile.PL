use inc::Module::Install;

name 'PfamWeb';
all_from 'lib/PfamWeb.pm';

requires Catalyst => '5.64';

catalyst;

install_script glob('script/*.pl');
auto_install;
WriteAll;
