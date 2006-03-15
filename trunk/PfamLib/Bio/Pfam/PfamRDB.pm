
#
# Perl module for PfamRDB
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

PfamRDB

=head1 DESCRIPTION

This module contains the basic functionality for connecting to the Pfam 
database. The two main modules that rely upon it are Bio::Pfam::DB_RDB,
which provides a query-level interface to the RDB, and pfamrdb which provides
functions for update of the database.


=cut


# Let the code begin...
    

package Bio::Pfam::PfamRDB;


use vars qw($AUTOLOAD 
	    @ISA 
	    @EXPORT_OK ); 

use strict;
use DBI;
use Bio::Pfam::Root;

@ISA = qw(Bio::Pfam::Root);


sub new {
  my($class, %params) = @_;

  my ($db_name, $driver, $host, $user, $db_password) = 
      ( ($params{'-DB_NAME'} || $params{'-db_name'}),
	($params{'-DB_DRIVER'} || $params{'-db_driver'}),
	($params{'-DB_HOST'} || $params{'-db_host'}),
	($params{'-DB_USER'} || $params{'-db_user'}),
	($params{'-DB_PASSWORD'} || $params{'-db_password'}));

  my $self = $class->SUPER::new( %params );

  $self->_database_name($db_name);
  $self->_database_driver($driver);
  $self->_database_user($user);
  $self->_database_host($host);
  $self->_database_password( $db_password );

  return $self; # success - we hope!
}




=head2 connect

 Title   : connect
 Usage   : $rdb->connect();
 Function:
    This function connects to a database. A connection starts a 
    transaction. 
 Returns :
    A database handle
 Notes   :
    If we are already connected to the RDB, the existing database handle
    is returned. This allows us to call other query functions internally
    without reconnecting to the database each time.

=cut

sub connect {
   my ($self, $force) = @_;

   if ($self->{'_transaction_count'}) {
       my $error = "PfamRDB: Trying to open a query connection ";
       $error .= "while the database is connected for update"; 
       $self->throw($error);
   }
  

   if (not defined $self->_database_handle()) {
       my $driver = $self->_database_driver();
       my $host = $self->_database_host();
       my $db_name = $self->_database_name();
       my $user = $self->_database_user();
       my $password = $self->_database_password(); 

       $self->{'_connection_count'} = 0;

       my ($dbh);

       if ($user =~ /pfamro/) {
	 $dbh = DBI->connect("dbi:$driver:database=$db_name;host=$host", $user ); 
       } else {
	 $dbh = DBI->connect("dbi:$driver:database=$db_name;host=$host", $user, $password); 
	 #Needed is using /usr/local/ensembl/bin/perl 
	 eval{
	     if($DBI::VERSION >= 1.38){
		 $dbh = DBI->InactiveDestroy(1);
	     }
	 };
     }


       if( not ($dbh) or defined($DBI::err)) {
	 $self->throw("Could not open connection to dbi:$driver:database=$db_name;host=$host;user=$user with user $user dbh: $dbh ERR: " .$DBI::err .  " "); 
       }
       $self->_database_handle( $dbh );

   }
         
   $self->{'_connection_count'}++;
       
   return $self->_database_handle();
}


=head2 disconnect

 Title   : disconnect
 Usage   : $rdb->disconnect();
 Function:
    This function is complementary to connect. It removes a virtual
    connection to the database. When all virtual connections are 
    removed, the database connection itself is removed
 Args    :

=cut

sub disconnect {
   my ($self, @args) = @_;

   my $dbh = $self->_database_handle();

   if ($dbh) {
       $self->{'_connection_count'}--;
       if (not $self->{'_connection_count'}) {
	   $dbh->disconnect;
	   $self->_database_handle( undef, 1);
       }
   }
   else {
       $self->throw( "Could not disconnect - nothing to disconnect from");
   }
}





=head2 close_transaction

 Title   : close_transaction
 Usage   : $self->close_transaction( $tran, $dbh, $error)
 Function:
    This function is called by all functions doing table updates.
    It tidies up the transaction
 Returns : 
 Args    :

=cut

sub close_transaction{
   my ($self, $error) = @_;

   my $dbh = $self->_database_handle();
   if ($dbh) {
       $self->{'_transaction_count'}--;

       if (not $self->{'_transaction_count'}) {

	   eval {
	       $self->report_mode and print STDERR "Unlocking tables\n";
	       $dbh->do( $self->__unlock_tables_sql() );
	   };
	   $@ and $error = "Could not unlock tables on the update [$@]";

	   eval {
	       if ($error) {
		   if (! $dbh->{AutoCommit}) {
		       $self->report_mode and print STDERR "Rolling back transaction\n";
		       $dbh->rollback;
		   }
		   $dbh->disconnect;
		   $self->_database_handle( undef, 1);
		   $self->throw("$error");
	       }
	       else {
		   if (! $dbh->{AutoCommit}) {
		       $self->report_mode and print STDERR "Committing transaction\n";
		       $dbh->commit;
		   }
		   $dbh->disconnect;
		   $self->_database_handle( undef, 1);
	       }
	   };
	   $@ and $self->throw("Could not complete the transaction [$@]");
       }
   }
   else {
       $self->throw( "Error: Cannot close transaction; there is none open!");
   }
}




=head2 open_transaction

 Title   : open_transaction
 Usage   : $dbh = $self->open_transaction( @table_list )
 Function:
    This function is called by all functions doing table updates.
    It opens up a connection, starting the transaction.
 Returns : A database handle
 Args    : A list of tables that will be affected by the update
 Notes   : 
    A transaction is defined as a connection. Therefore, to start
    a transaction, we must open a connection to the database, which
    means closing any existing connection
    
=cut

sub open_transaction{
   my ($self, @table_list) = @_;
   my ($dbh, $tab);

   if ($self->{'_connection_count'}) {
       my $error = "PfamRDB: Trying to start an update transaction ";
       $error .= "while database is open for query"; 
       $self->throw( "$error");
   }


   if (not ($self->{'_transaction_count'} and defined $self->_database_handle)) {
       my $driver = $self->_database_driver();
       my $host = $self->_database_host();
       my $db_name = $self->_database_name();
       my $user = $self->_database_user();
       my $password = $self->_database_password();


       $self->{'_connection_count'} = 0;

       $self->report_mode and print STDERR "Connecting to database...\n";

       $dbh = DBI->connect("dbi:$driver:database=$db_name;host=$host", $user, $password);


       if( not ($dbh) or defined($DBI::err)) {
	   $self->throw("Could not open connection to dbi:$driver:database=$db_name;host=$host with user $user"); 
       }

       $self->_database_handle( $dbh );
       
       # if database does not support transactions, the following
       # will throw a fatal error. We don't want to die though,
       # just not to commit or rollback.
       
       eval {
	   $dbh->{AutoCommit} = undef;
	   $self->report_mode and print STDERR "Starting transaction\n";
       };
       
       my @tmp;
       foreach $tab (@table_list) {
	   push @tmp, $tab, 'write';
       }
       $self->report_mode and print STDERR "Locking tables @table_list\n";

       eval {
	   $dbh->do( $self->__lock_tables_sql( @tmp ) );    
       };
       if ($@) {
	   $self->throw("Bio::Pfam::PfamRDB->open_connection - failed to lock tables");
       }
   }
   
   $self->{'_transaction_count'}++;
   return $self->_database_handle;;
}




=head2 report_mode

 Title   : report_mode
 Usage   : if ($self->report_mode)...
 Function:
    Gets/sets the internally stored binary var. 'report_mode.' When
    set, progress of all database transactions are reported to stderr
 Returns : A boolean
 Args    : A boolean

=cut

sub report_mode {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'_report_mode'} = $value;
   }
   return $self->{'_report_mode'};
}




=head2 _database_handle

 Title   : _databse_handle
 Usage   : $pass = $self->_database_handle();
 Function:
    Gets/sets the the current database handle for the database.

    This method is nor quite like the standard get/set methods in that
    it is necessary to be able to set the field to null (indicating
    no open database connection). Therefore, if the third arg is non-null,
    the field is over-written with $value, whether it is defined or not

=cut

sub _database_handle {
   my ($self,$value, $overwrite) = @_;

   if (defined($value) or $overwrite) {
       $self->{'_rdb_handle'} = $value;
   }
   return $self->{'_rdb_handle'};
}




=head2 _database_driver

 Title   : _database_driver
 Usage   : $dbh = _database_driver();
 Function:
    Gets/sets the driver for this relation database
 Returns : A database driver-name installed on the  by native platform
 Args    : A database driver-name installed by the native platform(optional)

=cut

sub _database_driver {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'_rdb_driver'} = $value;
   }
   return $self->{'_rdb_driver'};
}




=head2 _database_host

 Title   : _database_host
 Usage   : $dbh = _database_host();
 Function:
    Gets/sets the host-name for this relation database
 Returns : A database host-name understood by native platform
 Args    : A database name understood by the native platform(optional)

=cut

sub _database_host {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'_rdb_host'} = $value;
   }
   return $self->{'_rdb_host'};
}



=head2 _database_name

 Title   : _database_name
 Usage   : $dbh = _database_name();
 Function:
    Gets/sets the name for this relation database
 Returns : A database name understood by the database server
 Args    : A database name understood by the database server (optional)

=cut

sub _database_name {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'_rdb_name'} = $value;
   }
   return $self->{'_rdb_name'};
}




=head2 _database_user

 Title   : _database_user
 Usage   : $dbh = _database_user();
 Function:
    Gets/sets the user for this relational database
 Returns : A user id understood by the database server
 Args    : A user id understood by the database server (optional)

=cut

sub _database_user {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'_rdb_user'} = $value;
   }
   return $self->{'_rdb_user'};
}



=head2 _database_password

 Title   : _database_password
 Usage   : $pass = $self->__database_password();
 Function:
    Gets/sets the password for this relation database
 Returns : A database password understood by the database server
 Args    : A database password understood by the database server (optional)

=cut

sub _database_password {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'_rdb_password'} = $value;
   }
   return $self->{'_rdb_password'};
}



########## The remaining methods return sql strings suitable for use in 
########## DBI->prepare and DBI->do statements


=head2 __dump_to_file_sql

 Title   : __dump_to_file_sql
 Usage   : $string = $rdb->_dump_to_file_sql('pfamseq', 'file.txt');
 Function:
    This function returns an sql string to insert data into the
    given table from the given file.
 Returns :
    A string
 Args    :
    1. Table name.
    2. Fully qualified file name

=cut

sub __dump_to_file_sql {
   my ($self, $table, $file) = @_;

   return "select * into outfile '$file' from $table";
}



=head2 __empty_table_sql

 Title   : __empty_table_sql
 Usage   : $string = $rdb->__empty_table_sql( $table_name );
 Function:
    This function returns an sql string to remove all records from 
    the given table
 Returns :
    A string
 Args    :

=cut

sub __empty_table_sql {
   my ($self, $table) = @_;

   return "delete from $table";
}



=head2 __insert_sql

 Title   : __insert_sql
 Usage   : $string = $rdb->__insert_sql('pfamseq', 5);
 Function:
    This function returns an sql string to insert data into the
    given table. The number is to specify how many bind
    parameters to use in the expression
 Returns :
    A string
 Args    :
    1. Table name.
    2. The number of bind parameters

=cut

sub __insert_sql {
   my ($self, $table, $binds) = @_;

   my $pars = '?,' x $binds; chop $pars;

   return "insert into $table values ($pars)";
}




=head2 __insert_from_file_sql

 Title   : __insert_from_file_sql
 Usage   : $string = $rdb->_insert_sql('pfamseq', 'file.txt');
 Function:
    This function returns an sql string to insert data into the
    given table from the given file.
 Returns :
    A string
 Args    :
    1. Table name.
    2. Fully qualified file name

=cut

sub __insert_from_file_sql {
   my ($self, $table, $file) = @_;

   return "load data infile '$file' replace into table $table";
}





=head2 __lock_tables_sql

 Title   : __lock_tables_sql
 Usage   : $string = $rdb->_lock_tables( $table_name, $lock_mode, ...);
 Function:
    This function returns an sql string to lock the given tables for the
    given opration. The returned string is suitable for use directly on
    the 'do' method of the transaction (or database) handle
 Returns :
    A string
 Args    :
    The function expects one or more PAIRS of args, each pair being
    (table_name, lock_mode) where lock mode is 'read' or 'write'

=cut

sub __lock_tables_sql {
   my ($self, @args) = @_;
   my $table_name = shift @args;
   my $lock_mode = shift @args;
   my $query = "lock tables $table_name $lock_mode";
   while (@args) {
       $table_name = shift @args;
       $lock_mode = shift @args;
       $query .= ", $table_name $lock_mode";
   }
   return $query;
}




=head2 __replace_sql

 Title   : __replace_sql
 Usage   : $string = $rdb->__replace_sql('pfamseq', 5);
 Function:
    This function returns an sql string to replace data into the
    given table. The number is to specify how many bind
    parameters to use in the expression
 Returns :
    A string
 Args    :
    1. Table name.
    2. The number of bind parameters

=cut

sub __replace_sql {
   my ($self, $table, $binds) = @_;

   my $pars = '?,' x $binds; chop $pars;

   return "replace into $table values ($pars)";
}



=head2 __unlock_tables_sql

 Title   : __unlock_tables_sql
 Usage   : $string = $rdb->__unlock_tables_sql();
 Function:
    Returns a sql string to release all currently held locks
 Args    :

=cut

sub __unlock_tables_sql {
   my ($self) = @_;

   return "unlock tables";
}




=head2 DESTROY

 Title   : DESTOY
 Function:
    The destructor will close the connection to the database if one exists

=cut

sub DESTROY {
    my $self = shift;
    my ($dbh);
    
    if ($dbh = $self->_database_handle) {
	eval {
	    $dbh->disconnect;
	    $self->_database_handle( undef, 1 );
	};
	$@ and $self->throw( "Could not disconnect from the database");
    }
}



1;
