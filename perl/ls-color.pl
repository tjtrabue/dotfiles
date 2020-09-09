#!/usr/bin/env perl

# This script adds more colors to 'ls -la' output.

use warnings;
use strict;
use utf8;
use v5.32;

# For colorized output
use Term::ANSIColor;

# For file path information
use File::Spec;

my @stdin = <STDIN>;

sub get_segments_for_record {
    my ($record)   = @_;
    my @fields     = split( " ", $record );
    my $num_fields = @fields;
    my %segments   = (
        "perms"         => $fields[0],
        "num_hardlinks" => $fields[1],
        "user"          => $fields[2],
        "group"         => $fields[3],
        "size"          => $fields[4],
        "month_edited"  => $fields[5],
        "day_edited"    => $fields[6],
        "time_edited"   => $fields[7],
        "filename"      => $fields[ 7 .. $num_fields ],
    );

    return %segments;
}

sub update_field_width {
    my ( $field_widths_ref, $field, $new_len ) = @_;
    my %field_widths = %$field_widths_ref;

    $field_widths{$field} = $new_len if $new_len > $field_widths{$field};
    return %field_widths;
}

sub update_field_widths {
    my ( $field_widths_ref, $record ) = @_;
    my %field_widths = %$field_widths_ref;
    my %segments     = get_segments_for_record($record);

    %field_widths = update_field_width( \%field_widths, "perms",
        length( $segments{"perms"} ) );
    %field_widths = update_field_width( \%field_widths, "num_hardlinks",
        length( $segments{"num_hardlinks"} ) );
    %field_widths =
      update_field_width( \%field_widths, "user", length( $segments{"user"} ) );
    %field_widths = update_field_width( \%field_widths, "group",
        length( $segments{"group"} ) );
    %field_widths =
      update_field_width( \%field_widths, "size", length( $segments{"size"} ) );
    %field_widths = update_field_width( \%field_widths, "month_edited",
        length( $segments{"month_edited"} ) );
    %field_widths = update_field_width( \%field_widths, "day_edited",
        length( $segments{"day_edited"} ) );
    %field_widths = update_field_width( \%field_widths, "time_edited",
        length( $segments{"time_edited"} ) );

    return %field_widths;
}

sub get_field_widths {

    # Hash map of field name to max field length
    my %field_widths = (
        perms         => 0,
        user          => 0,
        group         => 0,
        num_hardlinks => 0,
        size          => 0,
        month_edited  => 0,
        day_edited    => 0,
        time_edited   => 0,
    );

    foreach my $line (@stdin) {
        chomp($line);

        # Skip first record
        next if ( $line =~ /^total/ );

        %field_widths = update_field_widths( \%field_widths, $line );
    }

    return %field_widths;
}

my %FIELD_WIDTHS = get_field_widths();

sub format_field {
    my ( $field, $field_width ) = @_;
    return sprintf( "%${field_width}s", $field );
}

# Returns the colorized permissions string for a record.
sub get_perms_segment {
    my ($perms) = @_;
    my $colorized = "";
    foreach my $char ( split( '', $perms ) ) {
        if ( $char eq "d" ) {
            $colorized = $colorized . colored( $char, "blue" );
        }
        elsif ( $char eq "l" ) {
            $colorized = $colorized . colored( $char, "cyan" );
        }
        elsif ( $char eq "r" ) {
            $colorized = $colorized . colored( $char, "yellow" );
        }
        elsif ( $char eq "w" ) {
            $colorized = $colorized . colored( $char, "magenta" );
        }
        elsif ( $char eq "x" ) {
            $colorized = $colorized . colored( $char, "green" );
        }
        else {
            $colorized = $colorized . $char;
        }
    }
    return format_field( $colorized, $FIELD_WIDTHS{"perms"} );
}

sub get_hardlinks_segment {
    my ($num_hardlinks) = @_;
    my $colorized = colored( $num_hardlinks, "yellow" );
    return format_field( $colorized, $FIELD_WIDTHS{"num_hardlinks"} );
}

sub get_user_segment {
    my ($user) = @_;
    my $colorized = colored( $user, "green" );
    if ( $user eq "root" ) {
        $colorized = colored( $user, "red" );
    }
    return format_field( $colorized, $FIELD_WIDTHS{"user"} );
}

sub get_group_segment {
    my ($group) = @_;
    my $colorized = colored( $group, "blue" );
    if ( $group eq "root" ) {
        $colorized = colored( $group, "red" );
    }
    return format_field( $colorized, $FIELD_WIDTHS{"group"} );
}

sub get_size_segment {
    my ($size) = @_;
    my $colorized = colored( $size, "cyan" );
    return format_field( $colorized, $FIELD_WIDTHS{"size"} );
}

sub get_month_edited_segment {
    my ($month_edited) = @_;
    my $colorized = colored( $month_edited, "magenta" );
    return format_field( $colorized, $FIELD_WIDTHS{"month_edited"} );
}

sub get_day_edited_segment {
    my ($day_edited) = @_;
    my $colorized = colored( $day_edited, "magenta" );
    return format_field( $colorized, $FIELD_WIDTHS{"day_edited"} );
}

sub get_time_edited_segment {
    my ($time_edited) = @_;
    my $colorized = colored( $time_edited, "magenta" );
    return format_field( $colorized, $FIELD_WIDTHS{"time_edited"} );
}

sub get_filename_segment {
    my ($filename_ref) = @_;
    my @filename_components = @$filename_ref;
    return join( " ", @filename_components );
}

sub print_record {
    my ($record) = @_;
    my %segments = get_segment_for_record($record);

    printf(
        "%s %s %s %s %s %s %s %s %s\n",
        get_perms_segment( $segments{"perms"} ),
        get_hardlinks_segment( $segments{"num_hardlinks"} ),
        get_user_segment( $segments{"user"} ),
        get_group_segment( $segments{"group"} ),
        get_size_segment( $segments{"size"} ),
        get_month_edited_segment( $segments{"month_edited"} ),
        get_day_edited_segment( $segments{"day_edited"} ),
        get_time_edited_segment( $segments{"time_edited"} ),
        get_filename_segment( $segments{"filename"} )
    );
}

sub main {
    foreach my $line (@stdin) {
        if ( $line =~ /^total/ ) {
            print "${line}\n";
            next;
        }
        print_record($line);
    }
}

main;
