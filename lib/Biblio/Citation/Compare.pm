package Biblio::Citation::Compare;

use 5.0;
use strict;
use warnings;
use Text::LevenshteinXS qw(distance);
use HTML::Entities;
use Text::Names qw/samePerson cleanName parseName parseName2/;
use Text::Roman qw/isroman roman2int/;
use List::Util qw(min max all);
use utf8;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
	sameWork sameAuthors toString extractEdition extractVolume sameAuthorBits sameAuthorsLoose sameTitle samePages mostlySameAuthors
) ] );


our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw( );

our $VERSION = '0.57';

# to correct bogus windows entities. unfixable ones are converted to spaces.
my %WIN2UTF = (
    hex('80')=> hex('20AC'),#  #EURO SIGN
    hex('81')=> hex('0020'),           #UNDEFINED
    hex('82')=> hex('201A'),#  #SINGLE LOW-9 QUOTATION MARK
    hex('83')=> hex('0192'),#  #LATIN SMALL LETTER F WITH HOOK
    hex('84')=> hex('201E'),#  #DOUBLE LOW-9 QUOTATION MARK
    hex('85')=> hex('2026'),#  #HORIZONTAL ELLIPSIS
    hex('86')=> hex('2020'),#  #DAGGER
    hex('87')=> hex('2021'),#  #DOUBLE DAGGER
    hex('88')=> hex('02C6'),#  #MODIFIER LETTER CIRCUMFLEX ACCENT
    hex('89')=> hex('2030'),#  #PER MILLE SIGN
    hex('8A')=> hex('0160'),#  #LATIN CAPITAL LETTER S WITH CARON
    hex('8B')=> hex('2039'),#  #SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    hex('8C')=> hex('0152'),#  #LATIN CAPITAL LIGATURE OE
    hex('8D')=> hex('0020'),#  #UNDEFINED
    hex('8E')=> hex('017D'),#  #LATIN CAPITAL LETTER Z WITH CARON
    hex('8F')=> hex('0020'),#  #UNDEFINED
    hex('90')=> hex('0020'),#  #UNDEFINED
    hex('91')=> hex('2018'),#  #LEFT SINGLE QUOTATION MARK
    hex('92')=> hex('2019'),#  #RIGHT SINGLE QUOTATION MARK
    hex('93')=> hex('201C'),#  #LEFT DOUBLE QUOTATION MARK
    hex('94')=> hex('201D'),#  #RIGHT DOUBLE QUOTATION MARK
    hex('95')=> hex('2022'),#  #BULLET
    hex('96')=> hex('2013'),#  #EN DASH
    hex('97')=> hex('2014'),#  #EM DASH
    hex('98')=> hex('02DC'),#  #SMALL TILDE
    hex('99')=> hex('2122'),#  #TRADE MARK SIGN
    hex('9A')=> hex('0161'),#  #LATIN SMALL LETTER S WITH CARON
    hex('9B')=> hex('203A'),#  #SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    hex('9C')=> hex('0153'),#  #LATIN SMALL LIGATURE OE
    hex('9D')=> hex('0020'),#  #UNDEFINED
    hex('9E')=> hex('017E'),#  #LATIN SMALL LETTER Z WITH CARON
    hex('9F')=> hex('0178')#  #LATIN CAPITAL LETTER Y WITH DIAERESIS
);
my $PARENS = '\s*[\[\(](.+?)[\]\)]\s*';
my $QUOTE = '"“”`¨´‘’‛“”‟„′″‴‵‶‷⁗❛❜❝❞';
my @ED_RES = (
    '(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth)',
    '([1-9])\s?\w{2,5}\s[ée]d',
    '\bv\.?(?:ersion)?\s?([0-9IXV]+)',
    '\s([IXV0-9]+)(?:$|:)',
    '\b(r(?:ev(?:ised)?)?|exp(?:anded)?)(?:\.|\s)\s*ed(?:\.|ition)?\b' # revised ed, expanded ed
);

#die "no" unless "2nd edition" =~ /$EDITION/i;

#my $TITLE_SPLIT = '(?:\?|\:|\.|!|\&quot;|[$QUOTE]\b)';
my $TITLE_SPLIT = '(?:\?|\:|\.|!)';

sub sameAuthors {
    my ($list1, $list2, %opts) = @_;
    my $max = 10;
    my $len1 = scalar(@$list1);
    my $len2 = scalar(@$list2);

    # Only compare up to the first 10 authors
    my @a1 = @$list1[0 .. ($len1-1 < $max-1 ? $len1-1 : $max-1)];
    my @a2 = @$list2[0 .. ($len2-1 < $max-1 ? $len2-1 : $max-1)];

    # If both lists have more than 10 authors, skip length comparison
    if (!($len1 > $max && $len2 > $max)) {
        return 0 if $#a1 != $#a2 and $opts{strict};
    }

    # Always compare the shorter list to the longer one
    if ($#a2 > $#a1) {
        my $t = \@a1;
        @a1 = @a2;
        @a2 = @$t;
    }
    for (my $i = 0; $i <= $#a2; $i++) {
        return 0 unless grep { samePerson($a2[$i],$_, %opts) } @a1;
    }
    return 1;
}

sub firstAuthor {
    my $e = shift;
    my $a = $e->{authors};
    if ($#$a > -1) {
        return $a->[0];
    } else {
        return undef;
    }
}

# Using just {0,3} allows matching blanks, but we want to force
# this to match only if a valid roman numeral is present.
# We are assuming we have no roman numeral pages greater than or equal to 400.
# basis can be swapped if we need higher numbers.
# my $basis = 'm{%s,3}(cm|cd|d?c{%s,3})(xc|xl|l?x{%s,3})(ix|iv|v?i{%s,3})"
my $basis = '(c{%s,3})(xc|xl|l?x{%s,3})(ix|iv|v?i{%s,3})';

my $occurrences = () = $basis =~ /\%s/g;
my @iterations = map { my @arr = (0) x $occurrences; $arr[abs(2-$_)] = 1; \@arr } 0..$occurrences-1;
my $lower_case = join("|", map { sprintf $basis, @$_ } @iterations);

my $roman = $lower_case;
my $r = qr/$roman/i;
# This would enforce matching, e.g., "iv" and "IV" but not "Iv" or "iV"
#   my $roman = "($lower_case)|(" . uc $lower_case . ")";
#   my $r = qr/$roman/;

my %page_rules = (
    arabic => {
        re => qr/(\d+)/,
        prefix => qr/([a-z]+)/i,
    },
    roman => {
        re => $r,
        # exclude roman numerals ivxlc from prefix
        # It's unlikely we have any 500+ page frontmatters,
        # so not excluding m,d
        prefix => qr/([abd-hjkm-uwyz]+)/i,
    }
);

my @page_regexes = map { 
    my $k = $_;
    my $v = $page_rules{$_};
    my $re = $v->{re};
    my $prefix = $v->{prefix};
    qr/^\s*
        (?<prefix>${prefix})?                         
        (?<${k}First>${re})                           
        (?:
            [\s\-–]+(?:\g{prefix})?(?<last>${re})   | 
            (?:\g{prefix})(?<last>${re})            | 
            [\s\-–]*(?:\g{prefix})?                   
        )?
    \s*$/ix 
} keys %page_rules;

sub normalizeLastPage {
    my ($first, $last) = @_;
    return $last if $last >= $first; # already normal
    return $last if length($last) >= length($first); # already normal
    my $overlap = substr($first, -length($last));
    return $last if $overlap >= $last; # cannot fix
    return substr($first, 0, length($first) - length($last)) . $last;
}

sub samePages {
    my ($pp1, $pp2, %opts) = @_;
    my $tolerance = $opts{tolerance}|| 1;
    $_ = decodeHTMLEntities($_) for ($pp1, $pp2);
    return 0 if $opts{strict} && !($pp1 && $pp2);
    return 0 if !$opts{match_on_blank} && !($pp1 && $pp2); 
    no warnings 'uninitialized';
    return 1 if $pp1 eq $pp2;
    
    $_ =~ s/\bpp?\.?(?=\s*\d)//g for ($pp1, $pp2); # remove "p", "pp.", etc

    PAGES: for my $pp ($pp1, $pp2) {
        for my $r (@page_regexes) {
            $pp = {%+} and next PAGES if $pp =~ /$r/;
        }
    }

    # return if any failed to match.
    return 0 unless all {ref $_ eq "HASH"} ($pp1, $pp2);

    if ($opts{match_style}) {
        # require same type of page numbers
        ($pp1->{$_} xor $pp2->{$_}) and return 0 for map { $_ . "First" } keys %page_rules;
    }

    if ($opts{prefixes_must_match}) {
        return 0 if ($pp1->{prefix} || $pp2->{prefix}) && $pp1->{prefix} ne $pp2->{prefix};
    }

    for my $pp ($pp1, $pp2) {
        for my $key (keys %$pp) {
            $pp->{$key} = lc $pp->{$key} if $key =~ /roman/i;
        }

        $pp->{last} = normalizeLastPage($pp->{arabicFirst}, $pp->{last}) if $pp->{arabicFirst} && $pp->{last};
        if ($pp->{romanFirst}) {
            $pp->{$_} = roman2int($pp->{$_}) for ('romanFirst', 'last');
        }
        # push all first pages to 'first'
        for (keys %page_rules) {
            $pp->{first} = $pp->{$_ . 'First'} if exists $pp->{$_ . 'First'};
        }
    }

    return 0 if ($pp1->{last} xor $pp2->{last}) && $opts{require_match_last};

    if ($pp1->{last} && $pp2->{last}) {
        return abs($pp1->{first} - $pp2->{first}) <= $tolerance && abs($pp1->{last} - $pp2->{last}) <= $tolerance;
    } else {
        return abs($pp1->{first} - $pp2->{first}) <= $tolerance;
    }
}

my %book_format = (
  'book' => 1,
  'thesis' => 1,
  'chapter' => 0,
  'journal' => 0,
  'online collection' => 0
);

sub sameWork {
 	my ($e, $c, $threshold,$loose,$nolinks,%opts) = @_;
    return 1 if defined $e->{id} && defined $c->{id} && $e->{id} ne "" and $e->{id} eq $c->{id};
  	return 0 if (!$c || !$e);

    my $debug = $opts{debug} || 0;

    $loose = 0 unless defined $loose;
    $threshold = 0.07 unless $threshold;
    $opts{loose} = 1 if $loose;
  
    if ($debug) {
        warn "sameWork 1: " . toString($e);
        warn "sameWork 2: " . toString($c);
    }

    if (defined $e->{doi} and length $e->{doi} and defined $c->{doi} and length $c->{doi}) {
        if ($e->{doi} eq $c->{doi}) {
            # we don't use doi to say 1 because often we have dois that are for a whole issue
            # however same doi increases the threshold
            $threshold *= 2 if $e->{doi} eq $c->{doi};
            $loose = 1;
            $opts{loose} = 1;
        } else {
            warn "doi mismatch" if $debug;
            return 0 unless $opts{conflate_versions};
        }
    }
    # also if incompatible formats
    my $e_book = $book_format{$e->{pub_type} || ''};
    my $c_book = $book_format{$c->{pub_type} || ''};
    return 0 if defined($e_book) and defined ($c_book) and ($e_book xor $c_book);

    # normalize encoding of relevant fields
    local $e->{title} = decodeHTMLEntities($e->{title});
    local $c->{title} = decodeHTMLEntities($c->{title});

    # first check if authors,date, and title are almost literally the same
    my $tsame = (lc $e->{title} eq lc $c->{title}) ? 1 : 0;
    my $asame = sameAuthors($e->{authors},$c->{authors},strict=>1);
    my $asame_loose = $asame || sameAuthors($e->{authors},$c->{authors},strict=>0); #asame_loose will be 1 while same is 0 when there are extra authors in one paper but all overlap authors match
    my $asame_bits = $asame_loose || sameAuthorBits($e->{authors},$c->{authors});
    my $dsame = sameDate($e->{date},$c->{date}, strict => 1);

    # duplicated in sameEntry
    # rule out identity when articles are published in the same journal but with different pages.
    # DB: disabled this because too often the pages are just wrong
    # no warnings;
    # if ($e->{pub_type} eq "journal" && $c->{pub_type} eq "journal" && $e->date ne 'forthcoming' && $c->date ne 'forthcoming') {
    #     if ($e->{source} && $c->{source} && ($e->{source} eq $c->{source} || $e->{jId} == $c->{jId})) {
    #         if ($e->{pages} && $c->{pages} && !samePages($e->{pages}, $c->{pages}, tolerance => 2)) {
    #             warn "Different pages for journal entry--not the same work" if $debug;
    #             return 0;
    #         }
    #     }
    # } 
    # rule out identity when chapters are published in the same work but with different titles or in different pages.
    # if ($e->{pub_type} eq "chapter" && $c->{pub_type} eq "chapter") {
    #     if ( 
    #         ($e->{source} && $c->{source} && $e->{source} eq $c->{source}) || 
    #         ($e->{book} && $c->{book} && $e->{book} eq $c->{book})
    #     ) {
    #         if ($e->{pages} && $c->{pages} && !samePages($e->{pages}, $c->{pages}, tolerance => 2)) {
    #             return 0;
    #         }
    #     }
    # }
    use warnings;

    if ($debug) {
        warn "tsame: $tsame";
        warn "asame: $asame";
        warn "asame_loose: $asame_loose";
        warn "asame_bits: $asame_bits";
        warn "dsame: $dsame";
    }

    return 1 if ($tsame and $asame and $dsame);

	# if authors quite different, not same
    if (!$asame_bits) {
      warn "authors too different" if $debug;
     	return 0;
    }
    # at this point the authors are plausibly the same

    no warnings 'uninitialized';
    if ($e->{pub_type} eq "book" && $c->{pub_type} eq "book" && $e->calcVolume != $c->calcVolume) {
        warn "Different volumes" if $debug;
        return 0
    } 
    use warnings 'uninitialized';

    # check dates
    my $date_wildcards = '^forthcoming|in press|manuscript|unknown|web$';
    my $compat_dates = ($dsame or ($e->{date} && $e->{date} =~ /$date_wildcards/) or ($c->{date} && $c->{date} =~ /$date_wildcards/));
    if (!$dsame and !$compat_dates) {

        #disabled for most cases because we want to conflate editions and republications for now. 
        if ($e->{title} =~ /^Introduction.?$/ or $e->{title} =~ /^Preface.?$/) {
            return 0 if ($e->{source} and $e->{source} ne $c->{source}) or 
                        ($e->{volume} and $e->{volume} ne $c->{volume});
        }

        # numeric dates
        local ($e->{date}) = $e->{date} =~ /(\d{4})/;
        local ($c->{date}) = $c->{date} =~ /(\d{4})/;
        if ($e->{date} and $e->{date}) {
            my $date_diff = $e->{date} - $c->{date};            
            # quite often people misremember dates so we permit some slack
            # we will consider the dates compat if they close in time
            # if dates are far apart, we know they are not exactly the same publicatoins. 
            # but they might be reprints of the same thing, which we want to conflate. 
            if ($date_diff > 3 or $date_diff < -3) {
                #$threshold /= 2 unless $opts{conflate_versions};
                warn "dates different, lowering similarity threshold" if $debug;
            } else {
                # nearby date
                #$threshold *= 2 unless $opts{conflate_versions};
            }

        } else {
            #non-numeric dates
            $threshold /=2;
        }

    } else {
      $loose = 1 if $asame_loose or $asame_bits;
    }

    warn "pre title length" if $debug;
  	# if title very different in lengths and do not contain ":" or brackets, not the same
  	return 0 if !$tsame and (
                    abs(length($e->{title}) - length($c->{title})) > 20 
                    and
					($e->{title} !~ /$TITLE_SPLIT/ and $c->{title} !~ /$TITLE_SPLIT/)
                    and
					($e->{title} !~ /$PARENS/ and $c->{title} !~ /$PARENS/)
				); 	

    warn "pre loose mode: loose = $loose" if $debug;


  	# perform fuzzy matching
   	#my $str1 = "$e->{date}|$e->{title}";
    my $str1 = lc _strip_non_word($e->{title});
    my $str2 = lc _strip_non_word($c->{title});

    # check for edition strings
    my $ed1 = extractEdition($str1);
    my $ed2 = extractEdition($str2);
    warn "ed1: $ed1" if $debug;
    warn "ed2: $ed2" if $debug;
    $loose =1 if $ed1 and $ed2 and $ed1 == $ed2 and !$dsame and $asame_loose;

    if ($opts{conflate_versions}) {
        # remove edition descriptions from titles
        $str1 =~ s/(\d+)(?:st|nd|rd|th) ed(?:ition)?\.?//i;
        $str2 =~ s/(\d+)(?:st|nd|rd|th) ed(?:ition)?\.?//i;
    } else {
        warn "not diff editions" if $debug;
        return 0 if ($ed1 and !$ed2) or ($ed2 and !$ed1) or ($ed1 && $ed1 != $ed2);
    }

    # remove brackets 
    my ($parens1,$parens2);
    $str1 =~ s/$PARENS//g;
    $parens1 = $1;
    $str2 =~ s/$PARENS//g;
    $parens2 = $1;
    unless ($opts{conflate_versions}) {
      return 0 if $parens1 && $parens2 && numdiff($parens1,$parens2);
    }

    warn "the text comparison is: '$str1' vs '$str2'" if $debug;

    unless ($opts{conflate_versions}) {
        warn "pre number check" if $debug;
        return 0 if numdiff($str1,$str2);
    }

    return 1 if fuzzyCompare($str1,$str2,$threshold,$debug);
 
	  # now if loose mode and only one of the titles has a ":" or other punctuation, compare the part before the punc with the other title instead
    if ($loose) {

        warn "trying loose match: $str1 -- $str2" if $debug;

        if ($e->{title} =~ /(.+?)\s*$TITLE_SPLIT\s*(.+)/) {

            $str1 = _strip_non_word($1);
            if ($c->{title} =~ /(.+?)\s*$TITLE_SPLIT\s*(.+)/) {
              # still try below
            } else {
                return 1 if fuzzyCompare($str1,$str2,$threshold);
            }

        } elsif ($c->{title} =~ /(.+?)\s*$TITLE_SPLIT\s*(.+)/) {

            $str2 = _strip_non_word($1);
            return 1 if fuzzyCompare($str1,$str2,$threshold);

        }

        # try something else: one is a substring of the other before :
        $str1 = _strip_non_word($e->{title}, 1);
        $str2 = _strip_non_word($c->{title}, 1);
        warn "Substring match: $str1 -- $str2" if $debug;
        if ($str1 =~ /^([^$TITLE_SPLIT]+)$TITLE_SPLIT/) {
        # if ($str1 =~ /^(.+)$TITLE_SPLIT/) {
            $str1 = $1 =~ s/^\s+|\s+$//gr;
            return 1 if $str2 =~ /^\Q$str1\E/
        } elsif ($str2 =~ /^([^$TITLE_SPLIT]+)$TITLE_SPLIT/) {
            $str2 = $1 =~ s/^\s+|\s+$//gr;
            return 1 if $str1 =~ /^\Q$str2\E/
        }

    }
        
    return 0;
}

sub fuzzyCompare {
  my ($str1, $str2, $threshold, $debug) = @_;
  my $dist = distance($str1,$str2);
  my $denum = (length($str1)+length($str2) +1);
  my $dist_score = $dist / $denum;
  warn "fuzzyCompare: $dist_score (threshold: $threshold)" if $debug;
  return $dist_score < $threshold;
}

sub sameAuthorsLoose {
  my ($a, $b) = @_;
  my $asame = sameAuthors($a,$b,strict=>1);
  my $asame_loose = $asame || sameAuthors($a,$b,strict=>0);
  return $asame_loose || sameAuthorBits($a,$b);
}

# of the first 10 authors, if more than half match in the same way as in sameAuthors, return 1
sub mostlySameAuthors {
    my ($a, $b, %opts) = @_;
    my @a = @$a;
    my @b = @$b;
    my $count = 0;
    my $total = min(10, scalar(@a), scalar(@b));
    for (my $i=0; $i<$total; $i++) {
        if (samePerson($a[$i], $b[$i], loose=>!$opts{strict})) {
            $count++;
        }
    }
    return $count > $total / 2;
}

my %author_bits_filter = (
  'dr' => 1,
  'mr' => 1,
  'ms' => 1,
  'mrs' => 1,
  'prof' => 1,
  'professor' => 1,
  'doctor' => 1,
  'phd' => 1,
);
sub sameAuthorBits {
    my ($a, $b) = @_;
    my (@alist, @blist);
    for (@$a) { 
        my $v = lc $_; # we copy so we don't modify the original
        $v =~ s/[,\.]//g;
        #$v =~ s/(\p{Ll})(\p{Lu})/$1 $2/g;
        push @alist, split(/\s+/, $v); 
    }
    for (@$b) { 
        my $v = lc $_;
        $v =~ s/[,\.]//g;
        #$v =~ s/(\p{Ll})(\p{Lu})/$1 $2/g;
        push @blist, split(/\s+/, $v); 
    }
    @alist = grep { !$author_bits_filter{$_} && !$author_bits_filter{$_ . '.'} } sort @alist;
    @blist = grep { !$author_bits_filter{$_} && !$author_bits_filter{$_ . '.'} } sort @blist;
    #use Data::Dumper;
    #print Dumper(\@alist);
    #print Dumper(\@blist);
    return 0 if $#alist != $#blist;
    for (my $i=0; $i<= $#alist; $i++) {
        return 0 if lc $alist[$i] ne lc $blist[$i];
    }
    return 1;
}

#wip
#sub author_bits {
#    my $list_ref = shift;
#    my @new;
#    for (@$list_ref) { 
#        my $v = $_; # we copy so we don't modify the original
#        $v =~ s/,//;
#        $v =~ s/(\p{Ll}\p
#        push @alist, split(/\s+/, $v); 
#    }
#}

sub _strip_non_word {
    my $str = shift;
    my $keep_punc = shift;
    #abbreviation "volume" v
    $str =~ s/\bvolume\b/v/gi;
    $str =~ s/\bvol\.?\b/v/gi;
    $str =~ s/\bv\.\b/v/gi;

    my $punc = $keep_punc ? $TITLE_SPLIT : ''; 
    $str =~ s/[^[0-9\p{L}\)\]\(\[$punc]+/ /g;
    $str =~ s/\s+/ /g;
    $str =~ s/^\s+//;
    $str =~ s/\s+$//;

    # remove "the" beginning
    $str =~ s/^\s*the\b//gi;
    $str; 
}

sub convert_number_string {
    my $s = shift;
    return $1 if $s =~ /(\d+)/;
    return roman2int($s) if isroman($s);
}

my %edition_res = (
    'first' => 1,
    'second' => 2,
    'third' => 3,
    'fourth' => 4,
    'fifth' => 5,
    'sixth' => 6,
    'seventh' => 7,
    'eighth' => 8,
    'ninth' => 9,
    'tenth' => 10,
    'r(:?ev(?:ised)?)?' => 'revised',
    'exp(?:anded)?' => 'expanded'
);

sub convert_edition {
    my $r = convert_number_string(@_);
    return $r if $r;
    my $s = shift;
    for my $key (keys %edition_res) {
        if ($s =~ /\b$key\b/i) {
            return $edition_res{$key};
        }
    }
    return $s;
}

sub extractEdition {
    my $s = shift;
    for my $re (@ED_RES) {
        if ($s =~ /$re/i) {
            return convert_edition($1) || $1;
        }
    }
    return undef;
}

my %volume_res = (
    'one' => 1,
    'two' => 2,
    'three' => 3,
    'four' => 4,
    'five' => 5,
    'six' => 6,
    'seven' => 7,
    'eight' => 8,
    'nine' => 9,
    'ten' => 10
);

sub convert_volume {
    my $r = convert_number_string(@_);
    return $r if $r;
    my $s = shift;
    for my $key (keys %volume_res) {
        if ($s =~ /\b$key\b/i) {
            return $volume_res{$key};
        }
    }    
}

sub extractVolume {
    my $s = shift;
    if ($s =~ /\bv(?:.|ol.?(?:ume)?)?\s*([0-9]+|[CXVI]+|(?:one|two|three|four|five|six|seven|eight|nine|ten))\b/i) {
        return convert_volume($1) || $1;
    }
    return undef;
}

sub numdiff {
  my ($s1,$s2) = @_;
  my @n1 = ($s1 =~ /\b([IXV0-9]{1,4}|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|eleventh|twelveth|1st|2nd|3rd|4th|5th|6th|7th|8th|9th|10th|11th|12th)\b/ig);
  my @n2 = ($s2 =~ /\b([IXV0-9]{1,4}|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|eleventh|twelveth|1st|2nd|3rd|4th|5th|6th|7th|8th|9th|10th|11th|12th)\b/ig);
  #print "In s1:" . join(",",@n1) . "\n";
  #print "In s2:" . join(",",@n2) . "\n";
  return 0 if $#n1 ne $#n2;
  for (0..$#n1) {
      return 1 if lc $n1[$_] ne lc $n2[$_];
  }
  return 0;
}


sub my_dist_text {
	my $a = lc shift;
	my $b = lc shift;
	$a =~ s/_/ /g;
	$b =~ s/_/ /g;
	return distance($a, $b);

}
sub decodeHTMLEntities {
    no warnings 'uninitialized';
    my $in = shift;
    $in =~ s/&([\d\w\#]+);/&safe_decode($1)/gei;
    return $in;
}

sub safe_decode {
    my $in = shift;
    if (substr($in,0,1) eq '#') {
        my $num = substr($in,1,1) eq 'x' ? hex(substr($in,1)) : substr($in,1);
        # we check and fix cp1232 entities
        return ($num < 127 or $num > 159) ? 
            HTML::Entities::decode_entities("&$in;") :
            HTML::Entities::decode_entities("&#" . $WIN2UTF{$num} . ";");
    } else {
            HTML::Entities::decode_entities("&$in;")
    }
}

sub toString {
    my $h = shift;
    return join("; ",@{$h->{authors}}) . " ($h->{date}) $h->{title}\n";
}

sub sameTitle {
  my ($a, $b, $threshold,$loose,$nolinks,%opts) = @_;
  return sameWork({ title => $a }, { title => $b }, $threshold,$loose,$nolinks,%opts);
}

sub sameDate {
    my ($a, $b, %opts) = @_;
    return 0 if $opts{strict} && !(defined $a && defined $b);
    return 1 if $a eq $b;
    # find any match when multiple years are given
    if ($a =~ /(?:^|\D)(\d{4})(?:\D|$)/g) {
        my $re = join("|", map { $_ } @{^CAPTURE});
        return $b =~ /$re/;
    }
    return 0;
}

1;
__END__

=head1 NAME

Biblio::Citation::Compare - Perl extension for performing fuzzy comparisons between bibliographic citations

=head1 SYNOPSIS

  use Biblio::Citation::Compare 'sameWork','sameAuthors';

  sameWork(
    # first item
    {
        authors => ['Bourget, D','Lukasiak, Zbigniew'],
        title => "A paper with such and such a title",
        date => 2010
    },
    # second item
    {
        authors => ['Bourget, David J. R.','Lukasiak, Zbigniew'],
        title => "A paper with such nd such a tlitle",
        date => undef
    }
  );

  # true!

  sameAuthors(
    ['Dave Bourget','Z Lukasiak'],
    ['Bourget DJR','Zbigniew Z. Lukasiak']
  );

  # true!


=head1 DESCRIPTION

This module exports two subroutines which perform fuzzy comparisons between citations (described using Perl hashes) and author lists. The subroutine attempt to determine if the citations and author lists refer to the same works and ordered lists of authors, respectively.

=head1 SUBROUTINES

=head2 sameWork(hashref citation1, hashref citation2): boolean

Takes as input two citations in a simple format illustrated in the synopsis. Returns true iff the two citations plausibly refer to the same work. A number of factors are taken into account to make the evaluation resistant to random variations. Among them: names are normalized and compared fuzzily using L<Text::Names>, allowances are made for random typos, allowances are made for short and long versions of titles (esp with titles containing a colon), small but important variations as in "Paper title part 1" and "Paper title part 2" are taken into account. The algorithm has been used to merge multiple data sources on L<PhilPapers.org>. 

Some advanced additional parameters are not explained here; they can only be explained by pointing to the source code. Their use should not normally be necessary.

=head2 sameAuthors(arrayref list1, arrayref list2): boolean

Returns true if the two lists are plausibly lists of the same authors. This is merely a convenient wrapper over L<Text::Names>::samePerson.

=head2 EXPORT

None by default.

=head1 SEE ALSO

See also L<Text::Names> for name normalization.

=head1 AUTHOR

David Bourget, http://www.dbourget.com 

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011 by David Bourget

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.1 or,
at your option, any later version of Perl 5 you may have available.


=cut
