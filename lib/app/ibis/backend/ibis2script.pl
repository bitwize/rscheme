#! /usr/bin/perl

# Need to run:
#   tr -dc '\n -~' 
# first!


%kstab = ();
$kstab{'?'} = 'issue';
$kstab{'#'} = 'position';
$kstab{'#*'} = 'taken-position';
$kstab{'#/'} = 'declined-position';
$kstab{'#.'} = 'future-position';
$kstab{'+'} = 'support';
$kstab{'-'} = 'object-to';

while (<>) {
    chomp;
    if (m,^(\s*)([?#+-][*/.]?)\s*,) {
        $key = $kstab{$2};
        print "($key ";
        $n = length($1);
        print "$n ";
        $s = $';
        $s =~ s/(["])/\\$1/g;
        print "\"$s\"";
        print ")\n";
    } elsif (m/^(\s+)([^\s].+)/) {
        $n = length($1);
        $s = $2;
        $s =~ s/(["])/\\$1/g;
        print "(... $n \"$s\")\n";
    }
}
