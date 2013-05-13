#!/usr/bin/perl -w
# -*- Mode: CPerl -*-
#
# Copyright (C) 2013 Rodolphe Quiédeville
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
#
#  Read the log file generated with backend="fullstats" attribute
#  print percentile 95,98 by default on stdout
#
# Author: Rodolphe Quiédeville <rodolphe@quiedeville.org>
#
use strict;
use Getopt::Long;
use vars qw ($help @files $version $stats @percentiles);
use JSON;

my $tagvsn = '1.5.0a';

GetOptions( "help",\$help,
            "stats=s",\$stats,
	    "percentiles=i@",\@percentiles,
            "version",\$version,
          );

&usage if $help or $Getopt::Long::error;
&version if $version;

@percentiles = (95,98) unless @percentiles;

for my $cper (@percentiles) {
  die "Wrong percentile value $cper" if $cper > 100 or $cper < 1;
}

$stats = "tsung-fullstats.log" unless $stats;
die "The stats file ($stats) does not exist, abort !" unless -e $stats;

print "Read : $stats\n";

foreach my $val ('connect','request','page') {
  &parse_stats_file("sample,$val", $stats);
}

sub parse_stats_file {
    my ($marker, $file) = @_;
    my $data;
    my $flag = 0;
    my @allvalues;

    open (FILE,"<$file") or die "Can't open $file $!";
    while (<FILE>) {
          if (/^{$marker/) {
	$flag = 1;
	$data = "";
      }
      if ($flag) {
	s/^\s+|\s+$//g;
	s/$marker,/"datas": /;
	$data .= $_;
	if (/}$/) {
	  $flag = 0;
	  my $json = decode_json $data;
	  foreach my $val (@{$json->{"datas"}}) {
	    push @allvalues, $val;
	  }
	  #print "-".@allvalues."\n";
	}
      }
    }
    close FILE;

    @allvalues = sort { $a <=> $b } @allvalues;

    print "Read : ".@allvalues ." values for $marker\n";

    for my $prcntl (sort { $a <=> $b } @percentiles) {
      print "Percentile $prcntl : ".percentile($prcntl, \@allvalues)."\n";
    }
}

sub percentile {
  my ($p,$aref) = @_;
  my $percentile = int($p * $#{$aref}/100);
  return (@$aref)[$percentile];
}

sub usage {
    print "this script is part of tsung version $tagvsn,
Copyright (C) 2013 Rodolphe Quiédeville <rodolphe\@quiedeville.org>\n\n";
    print "tsung comes with ABSOLUTELY NO WARRANTY; This is free software, and
ou are welcome to redistribute it under certain conditions
type `tsung_percentile.pl --version` for details.\n\n";

    print "Usage: $0 [<options>]\n","Available options:\n\t",
    "[--help] (this help text)\n\t",
    "[--percentiles] (percentile to compute, multiple values accepted)\n\t",
    "[--stats <file>] (stats file to analyse, default=tsung-fullstats.log)\n";
    exit;
}


sub version {
  print "
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program (see COPYING); if not, write to the 
Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
Boston, MA 02111-1307, USA.\n";
  exit;
}
