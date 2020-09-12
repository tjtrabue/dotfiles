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
        "perms"          => $perms_seg{$segment_field_name},
        "num_hardlinks"  => $hardlinks_seg{$segment_field_name},
        "user"           => $user_seg{$segment_field_name},
        "group"          => $group_seg{$segment_field_name},
        "size"           => $size_seg{$segment_field_name},
        "month_modified" => $month_modified_seg{$segment_field_name},
        "day_modified"   => $day_modified_seg{$segment_field_name},
        "time_modified"  => $time_modified_seg{$segment_field_name},
        "filename"       => $filename_seg{$segment_field_name},
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
    %field_widths =
      update_field_width( \%field_widths, "size", length( $segments{"size"} ) );
    %field_widths = update_field_width( \%field_widths, "month_modified",
        length( $segments{"month_modified"} ) );
    %field_widths = update_field_width( \%field_widths, "day_modified",
        length( $segments{"day_modified"} ) );
    %field_widths = update_field_width( \%field_widths, "time_modified",
        length( $segments{"time_modified"} ) );

    # May or may not have group information
    if ($SHOW_GROUP) {
        %field_widths = update_field_width( \%field_widths, "group",
            length( $segments{"group"} ) );
    }

    return %field_widths;
}

sub get_field_widths {

    # Hash map of field name to max field length
    my %field_widths = (
        perms          => 0,
        user           => 0,
        group          => 0,
        num_hardlinks  => 0,
        size           => 0,
        month_modified => 0,
        day_modified   => 0,
        time_modified  => 0,
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

### Begin Segments Functions ###

# Returns the colorized permissions string for a record.
sub get_colorized_perms_segment {
    my ($perms)         = @_;
    my $formatted_perms = format_fields( $perms, $FIELD_WIDTHS{"perms"} );
    my $colorized       = "";
    foreach my $char ( split( '', $formatted_perms ) ) {

        # Directories
        if ( $char eq "d" ) {
            $colorized = $colorized . colored( $char, "blue" );
        }

        # Symlinks
        elsif ( $char eq "l" ) {
            $colorized = $colorized . colored( $char, "cyan" );
        }

        # Block special devices
        elsif ( $char eq "b" ) {
            $colorized = $colorized . colored( $char, "red" );
        }

        # Character special devices
        elsif ( $char eq "c" ) {
            $colorized = $colorized . colored( $char, "red" );
        }

        # Permissions
        elsif ( $char eq "r" ) {
            $colorized = $colorized . colored( $char, "yellow" );
        }
        elsif ( $char eq "w" ) {
            $colorized = $colorized . colored( $char, "magenta" );
        }
        elsif ( $char eq "x" ) {
            $colorized = $colorized . colored( $char, "green" );
        }

        # Default: no color
        else {
            $colorized = $colorized . $char;
        }
    }

    return $colorized;
}

sub get_colorized_hardlinks_segment {
    my ($num_hardlinks) = @_;
    my $colorized =
      colored( format_field( $num_hardlinks, $FIELD_WIDTHS{"num_hardlinks"} ),
        "cyan" );

    return $colorized;
}

sub get_colorized_user_segment {
    my ($user) = @_;
    my $formatted_user = format_field( $user, $FIELD_WIDTHS{"user"} );
    my $colorized =
      $user eq "root"
      ? colored( $formatted_user, "red" )
      : colored( $formatted_user, "blue" );

    return $colorized;
}

sub get_colorized_group_segment {
    my ($group) = @_;
    my $formatted_group = format_field( $group, $FIELD_WIDTHS{"group"} );
    my $colorized =
      $group eq "root"
      ? colored( $formatted_group, "red" )
      : colored( $formatted_group, "green" );

    return $colorized;
}

sub get_colorized_size_segment {
    my ($size)         = @_;
    my $formatted_size = format_field( $size, $FIELD_WIDTHS{"size"} );
    my $colorized      = "";

    foreach my $char ( split( "", $formatted_size ) ) {
        if ( Scalar::Util::looks_like_number($char) ) {
            $colorized = $colorized . colored( $char, "yellow" );
        }
        elsif ( $UNIT_CHAR_COLOR{$char} ) {
            $colorized = $colorized . colored( $char, $UNIT_CHAR_COLOR{$char} );
        }
        else {
            $colorized = $colorized . $char;
        }
    }

    return $colorized;
}

sub get_colorized_month_modified_segment {
    my ($month_modified) = @_;
    my $colorized =
      colored( format_field( $month_modified, $FIELD_WIDTHS{"month_modified"} ),
        "magenta" );

    return $colorized;
}

sub get_colorized_day_modified_segment {
    my ($day_modified) = @_;
    my $colorized =
      colored( format_field( $day_modified, $FIELD_WIDTHS{"day_modified"} ),
        "magenta" );

    return $colorized;
}

sub get_colorized_time_modified_segment {
    my ($time_modified) = @_;
    my $colorized =
      colored( format_field( $time_modified, $FIELD_WIDTHS{"time_modified"} ),
        "magenta" );

    return $colorized;
}

# The filename's segment's colors are determined by the LS_COLORS variable, not
# by this script itself.
sub get_colorized_filename_segment {
    my ($filename) = @_;
    return $filename;
}

sub get_colorized_segments_for_record {
    my ($record) = @_;

    my %colorized_segments_map = {
        "perms"          => "",
        "num_hardlinks"  => "",
        "user"           => "",
        "size"           => "",
        "month_modified" => "",
        "day_modified"   => "",
        "time_modified"  => "",
        "filename"       => "",
    };

    my %segments_map = get_segments_map_for_record($record);
    my ( $perms, $num_hardlinks, $user, $size, $month_modified, $day_modified,
        $time_modified, $filename )
      = @segments_map{
        "perms",          "num_hardlinks", "user",          "size",
        "month_modified", "day_modified",  "time_modified", "filename"
      };

    $colorized_segments_map{"perms"} = get_colorized_perms_seg($perms);
    $colorized_segments_map{"num_hardlinks"} =
      get_colorized_hardlinks_seg($num_hardlinks);
    $colorized_segments_map{"user"} = get_colorized_user_seg($user);
    $colorized_segments_map{"size"} = get_colorized_size_seg($size);
    $colorized_segments_map{"month_modified"} =
      get_colorized_month_modified_seg($month_modified);
    $colorized_segments_map{"day_modified"} =
      get_colorized_day_modified_seg($day_modified);
    $colorized_segments_map{"time_modified"} =
      get_colorized_time_modified_seg($time_modified);
    $colorized_segments_map{"filename"} = get_colorized_filename_seg($filename);

    if ($SHOW_GROUP) {
        my $group = $segments_map{"group"};
        $colorized_segments_map{"group"} = get_colorized_group_seg($group);
    }

    return %colorized_segments_map;
}

sub print_record {
    my ($record) = @_;
    my %segments = get_segment_for_record($record);

    printf( "user",
        get_colorized_perms_segment( $segments{"perms"} ),
        get_colorized_hardlinks_segment( $segments{"num_hardlinks"} ),
        get_colorized_user_segment( $segments{"user"} ),
        get_colorized_group_segment( $segments{"group"} ),
        get_colorized_size_segment( $segments{"size"} ),
        get_colorized_month_modified_segment( $segments{"month_modified"} ),
        get_colorized_day_modified_segment( $segments{"day_modified"} ),
        get_colorized_time_modified_segment( $segments{"time_modified"} ),
        get_colorized_filename_segment( $segments{"filename"} ) );
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
