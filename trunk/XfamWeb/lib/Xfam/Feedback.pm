package Xfam::Feedback;

use warnings;
use strict;
use Moose;
use Email::Sender::Simple qw(sendmail);
use Email::Simple;
use Email::Simple::Creator;
use Email::Valid;
use Moose::Util::TypeConstraints;

subtype 'Email',
  as 'Str',
  where  { Email::Valid->address($_) },
  message { "The email address provided doesn't appear to be valid" };

subtype 'Feedback',
  as 'Str',
  where { $_ =~ /[a-z]+/i },
  message { "The annotation section is required."};

subtype 'IssueType',
  as 'Str',
  where { $_ =~ /^(Improvement|Feature|Bug)$/i },
  message { "The Issue Type must be Improvement/Feature/Bug."};

subtype 'Site',
  as 'Str',
  where { $_ =~ /^(Pfam|Dfam|Rfam|HMMER)$/i },
  message { "The Issue Type must be Improvement/Feature/Bug."};

has 'name'     => (is => 'rw');
has 'email'    => (is => 'rw', isa => 'Email', required => 1);
has 'feedback' => (is => 'rw', isa => 'Feedback', required => 1);
has 'site'     => (is => 'rw', isa => 'Site', required => 1);
has 'type'     => (is => 'rw', isa => 'IssueType', required => 1);

sub save {
  my $self = shift;

  my $subject = $self->type . " Request";

  my $email = Email::Simple->create(
    header => [
      To      => $self->recipient($self->site),
      From    => $self->email,
      Subject => $subject,
    ],
    body => qq(User:\n @{[$self->name]} <@{[$self->email]}>\n\nDetails:\n @{[$self->feedback]}),
  );
  sendmail($email);
}

sub recipient {
  my ($self, $site) = @_;
  my %email_map = (
    pfam  => 'pfam-help@ebi.ac.uk',
    rfam  => 'rfam-help@ebi.ac.uk',
    dfam  => 'dfam-help@janelia.hhmi.org',
    ipfam => 'ipfam@janelia.hhmi.org',
    treefam => 'pfam-help@ebi.ac.uk',
    antifam => 'pfam-help@ebi.ac.uk',
  );

  return $email_map{lc($site)};
}



1;
