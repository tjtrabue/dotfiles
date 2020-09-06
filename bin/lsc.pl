#!/usr/bin/env perl

# This script adds more colors to 'ls -la' output.

use warnings;
use strict;

my @stdin = <STDIN>;

# Hash map of field name to max field length
my %field_lengths = {
    perms         => 0,
    num_hardlinks => 0,
    size          => 0,
    month_edited  => 0,
    day_edited    => 0,
    time_edited   => 0,
};

# Colors
my $red        = "\033[01;31m";
my $green      = "\033[01;32m";
my $yellow     = "\033[01;33m";
my $blue       = "\033[01;34m";
my $purple     = "\033[01;35m";
my $cyan       = "\033[01;36m";
my $light_grey = "\033[01;37m";

# Reset sequence to remove colorization
my $reset = "\033[0m";

sub get_field_lengths {
    my @fields;
    my $len;
    for my $i ( 0 .. $#stdin ) {

        # Skip first record
        next if ( $i == 0 );

        @fields = split( " ", $stdin[$i] );
        $len    = length( $fields[0] );
        if ( $len > $field_lengths{"perms"} ) {
            $field_lengths{"perms"} = $len;
        }
    }
}

sub main {
    get_field_lengths();
    printf( "Perms length: %d\n", $field_lengths{"perms"} );
}

main;
