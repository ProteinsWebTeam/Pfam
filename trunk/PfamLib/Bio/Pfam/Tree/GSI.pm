package GSI;

################################################################
# GSI - generic sequence index file support
# SRE, Mon Sep  7 07:56:57 1998
# Adapted from squid's gsi.c
#
# package variables available by $GSI::
# (mirrors GSIFILE structure in squid)
#      nfiles: number of files indexed by GSI file
#      recnum: number of records indexed by GSI file  
#      GSIFP : open file handle to GSI file
#
# NOTE: Only one GSI file can be open at a time.
#
# Modified from S. Eddy source by Lorenzo Cerutti 2000/05/10
################################################################

# Size of the records to store the indexed information
$RECSIZE = 38;

# Records used to store directories
$RECDIR  = 2;

# fmt numbers
$fmt_genbank = 2;
$fmt_embl    = 4;
$fmt_fasta   = 7;
$fmt_pir     = 12;


###########################################################
# Functions to retrive index informations                 #
###########################################################

###########################################################
# Return RECSIZE
sub getRecsize {
	return $RECSIZE;
}

###########################################################
# Return RECDIR
sub getRecdir {
	return $RECDIR;
}

###########################################################
# Return an array containing the indexed directories. 
# Requires the name of the index file: 
# ex. @dirList = GSI::getDirectories($GSIFileName);
sub getDirectories {
	my (@list, $record);
	my $fileName = shift;
	open(GSI, "$fileName") || 
		die "Failed to open file in GSI::getDirectories($fileName)\n";	
	my $recordNumber = 1;
	seek(GSI, $recordNumber*getRecsize(), 0);
	read(GSI, $record, getRecdir()*getRecsize());
	my ($dir, $check1, $check2) = unpack "A70 n N", $record;
	while($check1 == 0 && $check2 == 0) {
		push(@list,$dir);
		$recordNumber += getRecDir();
		seek(GSI, $recordNumber*getRecsize(), 0);
		read(GSI, $record, getRecdir()*getRecsize());
		($dir, $check1, $check2) = unpack "A70 n N", $record;
	}
	close(GSI);
	return @list;
} 

###########################################################
# Return an array containing the indexed files (full path). 
# Requires the name of the index file: 
# ex. @dirList = GSI::getFiles($GSIFileName);
	
sub getFiles {
	my (@list, $record);
	my $fileName = shift;
	open(GSI, "$fileName") || 
		die "Failed to open file in GSI::getDirectories($fileName)\n";	
	read(GSI, $record, getRecsize());
	($check, $nfiles, $nkeys) = unpack "A32 n N", $record;
	die "File $file is not in the GSI format" if ($check ne "GSI");
	my $recordNumber = 1;
	while ($recordNumber <= $nfiles) {
		seek(GSI, $recordNumber*getRecsize(), 0);
		read(GSI, $record, getRecsize());
		my ($seqfile, $fileNumber, $check) = unpack "A32 n N", $record;
		if ($check == $fmt_genbank || $check == $fmt_embl || $check == $fmt_fasta || $check == $fmt_pir) {
			seek(GSI, $fileNumber*getRecsize(), 0);
			read(GSI, $record, getRecsize()*getRecdir());
			my ($dir, $unused1, $unused2) = unpack "A70 n N", $record;
			push(@list,"$dir$seqfile");
			$recordNumber++;
		}
		else {
			$recordNumber += getRecdir();
		}
	}
	close(GSI);
	return @list;
}

###########################################################
# Functions to write an indexed file                      #
###########################################################

###########################################################
# writeHeader: create the header
sub  writeHeaderRecord
{
	my ($fh, $recordsReserved, $nkeys) = @_;
	print $fh pack "a32 n N", "GSI", $recordsReserved, $nkeys;
	$nfiles = 1;
}

###########################################################
# writeDir: write directory name (USE 2 RECORDS!!)
sub  writeDirRecord
{
	my ($fh, $dir) = @_;
	foreach (@$dir) 
		{	print $fh pack "a70 n N", $_, 0, 0 }
}

###########################################################
# writeFile: write a file name
sub  writeFileRecord
{
	my ($fh, $files, $fmt) = @_;
	foreach (@$files)
		{	print $fh pack "a32 n N", $_, $nfiles, $fmt }
	$nfiles += $RECDIR;
}

###########################################################
# writeKey: write a key
sub  writeKeyRecord
{
	my ($fh, $key, $nfile, $offset) = @_;
	print $fh pack "a32 n N", $key, $nfile, $offset;
}

###########################################################
# Functions to search a gsi indexed file                  #
###########################################################

###########################################################
# Open and setup variables
sub openGSI
{
	my $file = shift;

	open (GSI, "$file") || die "Failed to open GSI file $file";
	read(GSI, $record, $RECSIZE);
	($check, $nfiles, $nkeys) = unpack "A32 n N", $record;
	die "File $file is not in the GSI format" if ($check ne "GSI");
}

###########################################################
# Close file
sub closeGSI
{
	close GSI;
}

###########################################################
# Get offset
sub getOffset
{
	my $key = shift;
	my ($record, $left, $right, $mid, $path, $fmt);
	$left  = $nfiles + 1;
	$right = $nfiles + $nkeys;
	$mid   = int(($left + $right)/2);
	seek(GSI, $mid * $RECSIZE, 0);
	
	while (read(GSI, $record, $RECSIZE))
	{
		($name, $file_number, $offset) = unpack "A32 n N", $record;
		if    ($key  eq $name ) { last }
		elsif ($left >= $right) { return (-1, -1, -1) }
		elsif ($key  gt $name ) { $left  = int(($left+$right)/2) + 1 }
		else                    { $right = int(($left+$right)/2) - 1 }
	
		$mid = int(($left+$right)/2);
		seek(GSI, $mid * $RECSIZE, 0);
	}

	do 
	{
		seek(GSI, $file_number * $RECSIZE, 0);
		if (!$fmt)
		{
			# Get file name
			read(GSI, $record, $RECSIZE);
			($seqfile, $file_number, $fmt) = unpack "A32 n N", $record;
		}
		else
		{
			# Get directory (2x RECSIZE !!)
			read(GSI, $record, $RECSIZE*$RECDIR);
			($seqfile, $file_number, $fmt) = unpack "A70 n N", $record;
		}
		$path = $seqfile.$path;
	} while ($file_number != 0);
	
	return ($path, $fmt, $offset);
}

1;
