#
#           WMLScript Language Specification Version 1.1
#
#   Lexer module
#

use strict;

use Math::BigInt;
use Math::BigFloat;
use Unicode::String qw(latin1 utf8 ucs2);

sub Error {
    my $parser = shift;
    my ($msg) = @_;

    $msg ||= "Syntax error.\n";

    if (exists $parser->YYData->{nb_error}) {
        $parser->YYData->{nb_error} ++;
    }
    else {
        $parser->YYData->{nb_error} = 1;
    }

    print STDOUT '#',$parser->YYData->{filename},':',$parser->YYData->{lineno},'#Error: ',$msg
            if (        exists $parser->YYData->{verbose_error}
                    and $parser->YYData->{verbose_error});
}

sub Warning {
    my $parser = shift;
    my ($msg) = @_;

    $msg ||= ".\n";

    if (exists $parser->YYData->{nb_warning}) {
        $parser->YYData->{nb_warning} ++;
    }
    else {
        $parser->YYData->{nb_warning} = 1;
    }

    print STDOUT '#',$parser->YYData->{filename},':',$parser->YYData->{lineno},'#Warning: ',$msg
            if (        exists $parser->YYData->{verbose_warning}
                    and $parser->YYData->{verbose_warning});
}

sub Info {
    my $parser = shift;
    my ($msg) = @_;

    $msg ||= ".\n";

    if (exists $parser->YYData->{nb_info}) {
        $parser->YYData->{nb_info} ++;
    }
    else {
        $parser->YYData->{nb_info} = 1;
    }

    print STDOUT '#',$parser->YYData->{filename},':',$parser->YYData->{lineno},'#Info: ',$msg
            if (        exists $parser->YYData->{verbose_info}
                    and $parser->YYData->{verbose_info});
}

sub _DoubleStringLexer {
    my $parser = shift;
    my $str = q{};
    my $type = 'STRING_LITERAL';

    while ($parser->YYData->{INPUT}) {

        for ($parser->YYData->{INPUT}) {

            s/^\"//
                and return($type,$str);

            if ($type eq 'UTF8_STRING_LITERAL') {
                s/^([^"\\]+)//
                    and $str .= ucs2($parser->YYData->{map}->to16($1))->utf8(),
                        last;
            }
            else {
                s/^([^"\\]+)//
                    and $str .= $1,
                        last;
            }

            s/^\\(['"\\\/])//
                and $str .= $1,     #  single quote, double quote, backslash, slash
                    last;
            s/^\\b//
                and $str .= "\b",   # backspace
                    last;
            s/^\\f//
                and $str .= "\f",   # form feed
                    last;
            s/^\\n//
                and $str .= "\n",   # new line
                    last;
            s/^\\r//
                and $str .= "\r",   # carriage return
                    last;
            s/^\\t//
                and $str .= "\t",   # horizontal tab
                    last;
            if ($type eq 'UTF8_STRING_LITERAL') {
                s/^\\([0-7]{1,2})//
                    and $str .= latin1(chr oct $1)->utf8(),
                        last;
                s/^\\([0-3][0-7]{2})//
                    and $str .= latin1(chr oct $1)->utf8(),
                        last;
                s/^\\x([0-9A-Fa-f]{2})//
                    and $str .= latin1(chr hex $1)->utf8(),
                        last;
            }
            else {
                if ($parser->YYData->{encoding} eq "ISO_8859-1:1987") {
                    s/^\\([0-7]{1,2})//
                        and $str .= chr oct $1,
                            last;
                    s/^\\([0-3][0-7]{2})//
                        and $str .= chr oct $1,
                            last;
                    s/^\\x([0-9A-Fa-f]{2})//
                        and $str .= chr hex $1,
                            last;
                }
                else {
                    s/^\\([0-7]{1,2})//
                        and $type = 'UTF8_STRING_LITERAL',
                        and $str = ucs2($parser->YYData->{map}->to16($str))->utf8(),
                        and $str .= latin1(chr oct $1)->utf8(),
                            last;
                    s/^\\([0-3][0-7]{2})//
                        and $type = 'UTF8_STRING_LITERAL',
                        and $str = ucs2($parser->YYData->{map}->to16($str))->utf8(),
                        and $str .= latin1(chr oct $1)->utf8(),
                            last;
                    s/^\\x([0-9A-Fa-f]{2})//
                        and $type = 'UTF8_STRING_LITERAL',
                        and $str = ucs2($parser->YYData->{map}->to16($str))->utf8(),
                        and $str .= latin1(chr hex $1)->utf8(),
                            last;
                }
            }
            if ($type eq 'UTF8_STRING_LITERAL') {
                s/^\\u([0-9A-Fa-f]{4})//
                    and $str .= Unicode::String->new("")->chr(hex $1)->utf8(),
                        last;
            }
            else {
                s/^\\u([0-9A-Fa-f]{4})//
                    and $type = 'UTF8_STRING_LITERAL',
                    and $str = ucs2($parser->YYData->{map}->to16($str))->utf8(),
                    and $str .= Unicode::String->new("")->chr(hex $1)->utf8(),
                        last;
            }
            s/^\\//
                and $parser->Error("Invalid escape sequence $_ .\n"),
                    last;
        }
    }

    $parser->Error("Untermined string.\n");
    $parser->YYData->{lineno} ++;
    return ($type, $str);
}

sub _SingleStringLexer {
    my $parser = shift;
    my $str = q{};
    my $type = 'STRING_LITERAL';

    while ($parser->YYData->{INPUT}) {

        for ($parser->YYData->{INPUT}) {

            s/^'//
                and return($type,$str);

            if ($type eq 'UTF8_STRING_LITERAL') {
                s/^([^'\\]+)//
                    and $str .= ucs2($parser->YYData->{map}->to16($1))->utf8(),
                        last;
            }
            else {
                s/^([^'\\]+)//
                    and $str .= $1,
                        last;
            }

            s/^\\(['"\\\/])//
                and $str .= $1,     #  single quote, double quote, backslash, slash
                    last;
            s/^\\b//
                and $str .= "\b",   # backspace
                    last;
            s/^\\f//
                and $str .= "\f",   # form feed
                    last;
            s/^\\n//
                and $str .= "\n",   # new line
                    last;
            s/^\\r//
                and $str .= "\r",   # carriage return
                    last;
            s/^\\t//
                and $str .= "\t",   # horizontal tab
                    last;
            if ($type eq 'UTF8_STRING_LITERAL') {
                s/^\\([0-7]{1,2})//
                    and $str .= latin1(chr oct $1)->utf8(),
                        last;
                s/^\\([0-3][0-7]{2})//
                    and $str .= latin1(chr oct $1)->utf8(),
                        last;
                s/^\\x([0-9A-Fa-f]{2})//
                    and $str .= latin1(chr hex $1)->utf8(),
                        last;
            }
            else {
                if ($parser->YYData->{encoding} eq "ISO_8859-1:1987") {
                    s/^\\([0-7]{1,2})//
                        and $str .= chr oct $1,
                            last;
                    s/^\\([0-3][0-7]{2})//
                        and $str .= chr oct $1,
                            last;
                    s/^\\x([0-9A-Fa-f]{2})//
                        and $str .= chr hex $1,
                            last;
                }
                else {
                    s/^\\([0-7]{1,2})//
                        and $type = 'UTF8_STRING_LITERAL',
                        and $str = ucs2($parser->YYData->{map}->to16($str))->utf8(),
                        and $str .= latin1(chr oct $1)->utf8(),
                            last;
                    s/^\\([0-3][0-7]{2})//
                        and $type = 'UTF8_STRING_LITERAL',
                        and $str = ucs2($parser->YYData->{map}->to16($str))->utf8(),
                        and $str .= latin1(chr oct $1)->utf8(),
                            last;
                    s/^\\x([0-9A-Fa-f]{2})//
                        and $type = 'UTF8_STRING_LITERAL',
                        and $str = ucs2($parser->YYData->{map}->to16($str))->utf8(),
                        and $str .= latin1(chr hex $1)->utf8(),
                            last;
                }
            }
            if ($type eq 'UTF8_STRING_LITERAL') {
                s/^\\u([0-9A-Fa-f]{4})//
                    and $str .= Unicode::String->new("")->chr(hex $1)->utf8(),
                        last;
            }
            else {
                s/^\\u([0-9A-Fa-f]{4})//
                    and $type = 'UTF8_STRING_LITERAL',
                    and $str = ucs2($parser->YYData->{map}->to16($str))->utf8(),
                    and $str .= Unicode::String->new("")->chr(hex $1)->utf8(),
                        last;
            }
            s/^\\//
                and $parser->Error("Invalid escape sequence $_ .\n"),
                    last;
        }
    }

    $parser->Error("Untermined string.\n");
    $parser->YYData->{lineno} ++;
    return ($type, $str);
}

sub _Identifier {
    my $parser = shift;
    my ($ident) = @_;

    if (exists $parser->YYData->{keyword}{$ident}) {
        return ($parser->YYData->{keyword}{$ident},$ident);
    }
    elsif (exists $parser->YYData->{invalid_keyword}{$ident}) {
        $parser->Error("Invalid keyword '$ident'.\n");
    }
    return ('IDENTIFIER', $ident);
}

sub _OctInteger {
    my $parser = shift;
    my ($str) = @_;

    my $val = new Math::BigInt(0);
    foreach (split //, $str) {
        $val = $val * new Math::BigInt(8) + new Math::BigInt(oct $_);
    }
    return ('INTEGER_LITERAL', $val);
}

sub _HexInteger {
    my $parser = shift;
    my ($str) = @_;

    my $val = new Math::BigInt(0);
    foreach (split //, $str) {
        $val = $val * new Math::BigInt(16) + new Math::BigInt(hex $_);
    }
    return ('INTEGER_LITERAL', $val);
}

sub _CommentLexer {
    my $parser = shift;

    while (1) {
            $parser->YYData->{INPUT}
        or  $parser->YYData->{INPUT} = readline $parser->YYData->{fh}
        or  return;

        for ($parser->YYData->{INPUT}) {
            s/^\n//
                    and $parser->YYData->{lineno} ++,
                    last;
            s/^\*\///
                    and return;
            s/^.//
                    and last;
        }
    }
}

sub _DocLexer {
    my $parser = shift;

    $parser->YYData->{doc} = q{};
    my $flag = 1;
    while (1) {
            $parser->YYData->{INPUT}
        or  $parser->YYData->{INPUT} = readline $parser->YYData->{fh}
        or  return;

        for ($parser->YYData->{INPUT}) {
            s/^(\n)//
                    and $parser->YYData->{lineno} ++,
                        $parser->YYData->{doc} .= $1,
                        $flag = 0,
                        last;
            s/^\*\///
                    and return;
            unless ($flag) {
                s/^\*//
                        and $flag = 1,
                        last;
            }
            s/^([ \r\t\f\013]+)//
                    and $parser->YYData->{doc} .= $1,
                    last;
            s/^(.)//
                    and $parser->YYData->{doc} .= $1,
                    $flag = 1,
                    last;
        }
    }
}

sub _Lexer {
    my $parser = shift;

    while (1) {
            $parser->YYData->{INPUT}
        or  $parser->YYData->{INPUT} = readline $parser->YYData->{fh}
        or  return ('', undef);

        for ($parser->YYData->{INPUT}) {

            s/^[ \r\t\f\013]+//;                            # Whitespace
            s/^\n//
                    and $parser->YYData->{lineno} ++,
                        last;

            s/^\/\*\*//                                     # documentation
                    and $parser->_DocLexer(),
                        last;

            s/^\/\*//                                       # MultiLineComment
                    and $parser->_CommentLexer(),
                        last;
            s/^\/\/(.*)\n//                                 # SingleLineComment
                    and $parser->YYData->{lineno} ++,
                        last;

            s/^([0-9]+\.[0-9]+[Ee][+\-]?[0-9]+)//
                    and return ('FLOAT_LITERAL', new Math::BigFloat($1));
            s/^([0-9]+[Ee][+\-]?[0-9]+)//
                    and return ('FLOAT_LITERAL', new Math::BigFloat($1));
            s/^(\.[0-9]+[Ee][+\-]?[0-9]+)//
                    and return ('FLOAT_LITERAL', new Math::BigFloat($1));
            s/^([0-9]+\.[0-9]+)//
                    and return ('FLOAT_LITERAL', new Math::BigFloat($1));
            s/^([0-9]+\.)//
                    and return ('FLOAT_LITERAL', new Math::BigFloat($1));
            s/^(\.[0-9]+)//
                    and return ('FLOAT_LITERAL', new Math::BigFloat($1));

            s/^0([0-7]+)//
                    and return $parser->_OctInteger($1);
            s/^0[Xx]([A-Fa-f0-9]+)//
                    and return $parser->_HexInteger($1);
            s/^(0)//
                    and return ('INTEGER_LITERAL', new Math::BigInt($1));
            s/^([1-9][0-9]*)//
                    and return ('INTEGER_LITERAL', new Math::BigInt($1));

            s/^\"//
                    and return $parser->_DoubleStringLexer();

            s/^\'//
                    and return $parser->_SingleStringLexer();

            s/^([A-Z_a-z][0-9A-Z_a-z]*)//
                    and return $parser->_Identifier($1);

            s/^(\+=)//
                    and return ($1, $1);
            s/^(\-=)//
                    and return ($1, $1);
            s/^(\*=)//
                    and return ($1, $1);
            s/^(\/=)//
                    and return ($1, $1);
            s/^(&=)//
                    and return ($1, $1);
            s/^(\|=)//
                    and return ($1, $1);
            s/^(\^=)//
                    and return ($1, $1);
            s/^(%=)//
                    and return ($1, $1);
            s/^(<<=)//
                    and return ($1, $1);
            s/^(>>=)//
                    and return ($1, $1);
            s/^(>>>=)//
                    and return ($1, $1);
            s/^(div=)//
                    and return ($1, $1);
            s/^(&&)//
                    and return ($1, $1);
            s/^(\|\|)//
                    and return ($1, $1);
            s/^(\+\+)//
                    and return ($1, $1);
            s/^(\-\-)//
                    and return ($1, $1);
            s/^(<<)//
                    and return ($1, $1);
            s/^(>>>)//
                    and return ($1, $1);
            s/^(>>)//
                    and return ($1, $1);
            s/^(<=)//
                    and return ($1, $1);
            s/^(>=)//
                    and return ($1, $1);
            s/^(==)//
                    and return ($1, $1);
            s/^(!=)//
                    and return ($1, $1);

            s/^([=><,!~\?:\.\+\-\*\/&\|\^%\(\)\{\};#])//
                    and return ($1, $1);                    # punctuator

            s/^([\S]+)//
                    and $parser->Error("lexer error $1.\n"),
                        last;
        }
    }
}

sub _InitLexico {
    my $parser = shift;

    my %keywords = (
        # Literal
        'true'          =>  'TRUE_LITERAL',
        'false'         =>  'FALSE_LITERAL',
        'invalid'       =>  'INVALID_LITERAL',
        # Keyword
        'access'        =>  'ACCESS',
        'agent'         =>  'AGENT',
        'break'         =>  'BREAK',
        'continue'      =>  'CONTINUE',
        'div'           =>  'DIV',
        'domain'        =>  'DOMAIN',
        'else'          =>  'ELSE',
        'equiv'         =>  'EQUIV',
        'extern'        =>  'EXTERN',
        'for'           =>  'FOR',
        'function'      =>  'FUNCTION',
        'header'        =>  'HEADER',
        'http'          =>  'HTTP',
        'if'            =>  'IF',
        'isvalid'       =>  'ISVALID',
        'meta'          =>  'META',
        'name'          =>  'NAME',
        'path'          =>  'PATH',
        'return'        =>  'RETURN',
        'typeof'        =>  'TYPEOF',
        'use'           =>  'USE',
        'user'          =>  'USER',
        'var'           =>  'VAR',
        'while'         =>  'WHILE',
        'url'           =>  'URL',
    );
    my %invalid_keywords = (
        # Keyword not used
        'delete'        =>  'DELETE',
        'in'            =>  'IN',
        'lib'           =>  'LIB',
        'new'           =>  'NEW',
        'null'          =>  'NULL',
        'this'          =>  'THIS',
        'void'          =>  'VOID',
        'with'          =>  'WITH',
        # Future reserved word
        'case'          =>  'CASE',
        'catch'         =>  'CATCH',
        'class'         =>  'CLASS',
        'const'         =>  'CONST',
        'debugger'      =>  'DEBUGGER',
        'default'       =>  'DEFAULT',
        'do'            =>  'DO',
        'enum'          =>  'ENUM',
        'export'        =>  'EXPORT',
        'extends'       =>  'EXTENDS',
        'finally'       =>  'FINALLY',
        'import'        =>  'IMPORT',
        'private'       =>  'PRIVATE',
        'public'        =>  'PUBLIC',
        'sizeof'        =>  'SIZEOF',
        'struct'        =>  'STRUCT',
        'super'         =>  'SUPER',
        'switch'        =>  'SWITCH',
        'throw'         =>  'THROW',
        'try'           =>  'TRY',
    );

    $parser->YYData->{keyword} = \%keywords;
    $parser->YYData->{invalid_keyword} = \%invalid_keywords;
}

sub _InitStandardLibrary {
    my $parser = shift;
    my $cfg = $INC{'WAP/wmls/lexer.pm'};
    $cfg =~ s/lexer\.pm$//;
    $cfg .= 'wmlslibs.cfg';
     open (my $IN, '<', $cfg)
        or warn "can't open $cfg.\n";

    my $lib = undef;
    my $LibID;
    while (<$IN>) {
        if      (/^#.*$/) {
#           print "Comment $_";
        } elsif (/^\s*$/) {
#           print "Empty\n";
        } elsif (/^\@([A-Z_a-z][0-9A-Z_a-z]*)\s+([0-9]+)\s*$/) {
#           print "Lib $1 $2\n";
            $lib = $1;
            $LibID = $2;
            $parser->YYData->{symbtab_lib}->Insert($lib, 1);
        } elsif (/^([A-Z_a-z][0-9A-Z_a-z]*)\s+([0-9]+)\s+([0-9]+)\s*$/) {
#           print "Fct $1 $2 $3\n";
            if (defined $lib) {
                my $symb = $lib . '.' . $1;
                $parser->YYData->{symbtab_func}->InsertLibrary($symb, $LibID, $2, $3);
            }
        } else {
            print "cfg? $_";
        }
    }
    close $IN;
}

1;


