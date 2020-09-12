#!/usr/bin/env perl

# This script adds more colors to 'ls -la' output.

use warnings;
use strict;
use utf8;
use v5.32;

# For colorized output
use Term::ANSIColor;

# For determining number vs. string types
use Scalar::Util;

my @stdin = <STDIN>;

# Map of storage units to their corresponding color.
my %UNIT_CHAR_COLOR =
  ( "K" => "blue", "M" => "green", "G" => "yellow", "T" => "red" );

# Whether we display the user's group in the ls output.
my $SHOW_GROUP = 1;

# Figure out whether or not we are presenting group information to the user.
sub determine_show_group {
    my ($record) = @_;
    my @fields = split( " ", $record );
    if ( Scalar::Util::looks_like_number( $fields[3] ) ) {
        $SHOW_GROUP = 0;
    }
}
determine_show_group( $stdin[0] );

sub construct_segment_struct {
    my ( $segment, $index ) = @_;
    my %segment_struct = ( segment => $segment, next_field_index => $index, );
    return %segment_struct;
}

sub parse_perms_segment {
    my ( $fields_ref, $index ) = @_;
    return construct_segement_struct( $$fields_ref[$index], ++$index );
}

sub parse_hardlinks_segment {
    my ( $fields_ref, $index ) = @_;
    return construct_segement_struct( $$fields_ref[$index], ++$index );
}

sub parse_user_segment {
    my ( $fields_ref, $index ) = @_;
    return construct_segement_struct( $$fields_ref[$index], ++$index );
}

sub parse_group_segment {
    my ( $fields_ref, $index ) = @_;
    my $group = $SHOW_GROUP ? $$fields_ref[ $index++ ] : "";
    return construct_segement_struct( $group, $index );
}

sub parse_size_segment {
    my ( $fields_ref, $index ) = @_;
    my $size = "";
    my $field;

    do {
        $field = $$fields_ref[ $index++ ];
        $size  = $size . " " . $field;
    } while ( $field =~ /,/ );

    return construct_segement_struct( $$fields_ref[$index], ++$index );
}

sub parse_month_modified_segment {
    my ( $fields_ref, $index ) = @_;
    return construct_segement_struct( $$fields_ref[$index], ++$index );
}

sub parse_day_modified_segment {
    my ( $fields_ref, $index ) = @_;
    return construct_segement_struct( $$fields_ref[$index], ++$index );
}

sub parse_time_modified_segment {
    my ( $fields_ref, $index ) = @_;
    return construct_segement_struct( $$fields_ref[$index], ++$index );
}

sub parse_filename_segment {
    my ( $fields_ref, $index ) = @_;
    my $len     = $#$fields_ref;
    my $segment = join( " ", @$fields_ref[ $index .. $len ] );
    return construct_segement_struct( $segment, $len );
}

sub get_segments_for_record {
    my ($record)           = @_;
    my @fields             = split( " ", $record );
    my $num_fields         = @fields;
    my $index_field_name   = "next_field_index";
    my $segment_field_name = "segment";

    my %perms_seg = parse_perms_segment( \@fields, 0 );
    my %hardlinks_seg =
      parse_hardlinks_segment( \@fields, $perms_seg{$index_field_name} );
    my %user_seg =
      parse_user_segment( \@fields, $hardlinks_seg{$index_field_name} );
    my %group_seg =
      parse_group_segment( \@fields, $user_seg{$index_field_name} );
    my %size_seg =
      parse_size_segment( \@fields, $group_seg{$index_field_name} );
    my %month_modified_seg =
      parse_month_modified_segment( \@fields, $size_seg{$index_field_name} );
    my %day_modified_seg =
      parse_day_modified_segment( \@fields,
        $month_modified_seg{$index_field_name} );
    my %time_modified_seg =
      parse_time_modified_segment( \@fields,
        $day_modified_seg{$index_field_name} );
    my %filename_seg =
      parse_filename_segment( \@fields, $time_modified_seg{$index_field_name} );

    my %segments = (
        "perms"         => $perms_seg{$segment_field_name},
        "num_hardlinks" => $hardlinks_seg{$segment_field_name},
        "user"          => $user_seg{$segment_field_name},
        "group"         => $group_seg{$segment_field_name},
        "size"          => $size_seg{$segment_field_name},
        "month_edited"  => $month_modified_seg{$segment_field_name},
        "day_edited"    => $day_modified_seg{$segment_field_name},
        "time_edited"   => $time_modified_seg{$segment_field_name},
        "filename"      => $filename_seg{$segment_field_name},
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
sub get_colorized_perms_segment {
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

sub get_colorized_hardlinks_segment {
    my ($num_hardlinks) = @_;
    my $colorized = colored( $num_hardlinks, "yellow" );
    return format_field( $colorized, $FIELD_WIDTHS{"num_hardlinks"} );
}

sub get_colorized_user_segment {
    my ($user) = @_;
    my $colorized = colored( $user, "green" );
    if ( $user eq "root" ) {
        $colorized = colored( $user, "red" );
    }
    return format_field( $colorized, $FIELD_WIDTHS{"user"} );
}

sub get_colorized_group_segment {
    my ($group) = @_;
    my $colorized = colored( $group, "blue" );
    if ( $group eq "root" ) {
        $colorized = colored( $group, "red" );
    }
    return format_field( $colorized, $FIELD_WIDTHS{"group"} );
}

sub get_colorized_size_segment {
    my ($size) = @_;
    my $colorized = colored( $size, "cyan" );
    return format_field( $colorized, $FIELD_WIDTHS{"size"} );
}

sub get_colorized_month_edited_segment {
    my ($month_edited) = @_;
    my $colorized = colored( $month_edited, "magenta" );
    return format_field( $colorized, $FIELD_WIDTHS{"month_edited"} );
}

sub get_colorized_day_edited_segment {
    my ($day_edited) = @_;
    my $colorized = colored( $day_edited, "magenta" );
    return format_field( $colorized, $FIELD_WIDTHS{"day_edited"} );
}

sub get_colorized_time_edited_segment {
    my ($time_edited) = @_;
    my $colorized = colored( $time_edited, "magenta" );
    return format_field( $colorized, $FIELD_WIDTHS{"time_edited"} );
}

sub get_colorized_filename_segment {
    my ($filename_ref) = @_;
    my @filename_components = @$filename_ref;
    return join( " ", @filename_components );
}

sub print_record {
    my ($record) = @_;
    my %segments = get_segment_for_record($record);

    printf(
        "%s %s %s %s %s %s %s %s %s\n",
        get_colorized_perms_segment( $segments{"perms"} ),
        get_colorized_hardlinks_segment( $segments{"num_hardlinks"} ),
        get_colorized_user_segment( $segments{"user"} ),
        get_colorized_group_segment( $segments{"group"} ),
        get_colorized_size_segment( $segments{"size"} ),
        get_colorized_month_edited_segment( $segments{"month_edited"} ),
        get_colorized_day_edited_segment( $segments{"day_edited"} ),
        get_colorized_time_edited_segment( $segments{"time_edited"} ),
        get_colorized_filename_segment( $segments{"filename"} )
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
