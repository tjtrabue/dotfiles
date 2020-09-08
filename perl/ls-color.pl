#!/usr/bin/env perl

# This script adds more colors to 'ls -la' output.

use warnings;
use strict;

# For colorized output
use Term::ANSIColor;

my @stdin = <STDIN>;

sub get_field_lengths {

    # Hash map of field name to max field length
    my %field_lengths = (
        perms         => 0,
        user          => 0,
        num_hardlinks => 0,
        size          => 0,
        month_edited  => 0,
        day_edited    => 0,
        time_edited   => 0,
    );

    my @fields;
    my $len;
    for my $i ( 0 .. $#stdin ) {

        # Skip first record
        next if ( $i == 0 );

        @fields                 = split( " ", $stdin[$i] );
        $len                    = length( $fields[0] );
        $field_lengths{"perms"} = $len
          if ( $field_lengths{"perms"} = $len );
        $len = length( $fields[1] );
        $field_lengths{"num_hardlinks"} = $len
          if ( $field_lengths{"num_hardlinks"} = $len );
        $len = length( $fields[2] );
        $field_lengths{"user"} = $len
          if ( $field_lengths{"user"} = $len );
        $len = length( $fields[3] );
        $field_lengths{"size"} = $len
          if ( $field_lengths{"size"} = $len );
        $len = length( $fields[4] );
        $field_lengths{"month_edited"} = $len
          if ( $field_lengths{"month_edited"} = $len );
        $len = length( $fields[5] );
        $field_lengths{"day_edited"} = $len
          if ( $field_lengths{"day_edited"} = $len );
        $len = length( $fields[6] );
        $field_lengths{"time_edited"} = $len
          if ( $field_lengths{"time_edited"} = $len );
    }

    return %field_lengths;
}

# Returns the colorized permissions string for a record.
sub get_perms_segment {
    my ($perms_string) = @_;
    my $colorized = "";
    foreach my $char ( split( '', $perms_string ) ) {
        if ( $char eq "d" ) {
            $colorized = $colorized . colored( $char, "blue" );
        }
        elsif ( $char eq "r" ) {
            $colorized = $colorized . colored( $char, "magenta" );
        }
        elsif ( $char eq "w" ) {
            $colorized = $colorized . colored( $char, "cyan" );
        }
        elsif ( $char eq "x" ) {
            $colorized = $colorized . colored( $char, "green" );
        }
        else {
            $colorized = $colorized . $char;
        }
    }
    return $colorized;
}

sub get_colorized_record {
    my ( $perms, $num_hardlinks, $user, $size, $month_edited, $day_edited,
        $time_edited )
      = @_;

    # printf( "Permissions: %s\n",   $perms );
    # printf( "Num Hardlinks: %s\n", $num_hardlinks );
    # printf( "User: %s\n",          $user );
    # printf( "Size: %s\n",          $size );
    # printf( "Month edited: %s\n",  $month_edited );
    # printf( "Day edited: %s\n",    $day_edited );
    # printf( "Time edited: %s\n",   $time_edited );
    my $colorized_record = "";
    $colorized_record = get_perms_segment($perms);
    return $colorized_record;
}

sub main {
    my @field_lengths = get_field_lengths();
    my @fields;
    for my $i ( 0 .. $#stdin ) {
        next if ( $i == 0 );
        @fields = split( " ", $stdin[$i] );
        print get_colorized_record(@fields), "\n";
    }
}

main;
