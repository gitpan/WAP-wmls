####################################################################
#
#    This file was generated using Parse::Yapp version 1.02.
#
#        Don't edit this file, use source file instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
####################################################################
package parser;
use vars qw ( @ISA );
use strict;

@ISA= qw ( Parse::Yapp::Driver );
#Included Parse/Yapp/Driver.pm file----------------------------------------
{
#
# Module Parse::Yapp::Driver
#
# This module is part of the Parse::Yapp package available on your
# nearest CPAN
#
# Any use of this module in a standalone parser make the included
# text under the same copyright as the Parse::Yapp module itself.
#
# This notice should remain unchanged.
#
# (c) Copyright 1998-1999 Francois Desarmenien, all rights reserved.
# (see the pod text in Parse::Yapp module for use and distribution rights)
#

package Parse::Yapp::Driver;

require 5.004;

use strict;

use vars qw ( $VERSION $COMPATIBLE $FILENAME );

$VERSION = '1.02';
$COMPATIBLE = '0.07';
$FILENAME=__FILE__;

use Carp;

#Known parameters, all starting with YY (leading YY will be discarded)
my(%params)=(YYLEX => 'CODE', 'YYERROR' => 'CODE', YYVERSION => '',
			 YYRULES => 'ARRAY', YYSTATES => 'ARRAY', YYDEBUG => '');
#Mandatory parameters
my(@params)=('LEX','RULES','STATES');

sub new {
    my($class)=shift;
	my($errst,$nberr,$token,$value,$check,$dotpos);
    my($self)={ ERROR => \&_Error,
				ERRST => \$errst,
                NBERR => \$nberr,
				TOKEN => \$token,
				VALUE => \$value,
				DOTPOS => \$dotpos,
				STACK => [],
				DEBUG => 0,
				CHECK => \$check };

	_CheckParams( [], \%params, \@_, $self );

		exists($$self{VERSION})
	and	$$self{VERSION} < $COMPATIBLE
	and	croak "Yapp driver version $VERSION ".
			  "incompatible with version $$self{VERSION}:\n".
			  "Please recompile parser module.";

        ref($class)
    and $class=ref($class);

    bless($self,$class);
}

sub YYParse {
    my($self)=shift;
    my($retval);

	_CheckParams( \@params, \%params, \@_, $self );

	if($$self{DEBUG}) {
		_DBLoad();
		$retval = eval '$self->_DBParse()';#Do not create stab entry on compile
        $@ and die $@;
	}
	else {
		$retval = $self->_Parse();
	}
    $retval
}

sub YYData {
	my($self)=shift;

		exists($$self{USER})
	or	$$self{USER}={};

	$$self{USER};
	
}

sub YYErrok {
	my($self)=shift;

	${$$self{ERRST}}=0;
    undef;
}

sub YYNberr {
	my($self)=shift;

	${$$self{NBERR}};
}

sub YYRecovering {
	my($self)=shift;

	${$$self{ERRST}} != 0;
}

sub YYAbort {
	my($self)=shift;

	${$$self{CHECK}}='ABORT';
    undef;
}

sub YYAccept {
	my($self)=shift;

	${$$self{CHECK}}='ACCEPT';
    undef;
}

sub YYError {
	my($self)=shift;

	${$$self{CHECK}}='ERROR';
    undef;
}

sub YYSemval {
	my($self)=shift;
	my($index)= $_[0] - ${$$self{DOTPOS}} - 1;

		$index < 0
	and	-$index <= @{$$self{STACK}}
	and	return $$self{STACK}[$index][1];

	undef;	#Invalid index
}

sub YYCurtok {
	my($self)=shift;

        @_
    and ${$$self{TOKEN}}=$_[0];
    ${$$self{TOKEN}};
}

sub YYCurval {
	my($self)=shift;

        @_
    and ${$$self{VALUE}}=$_[0];
    ${$$self{VALUE}};
}

sub YYExpect {
    my($self)=shift;

    keys %{$self->{STATES}[$self->{STACK}[-1][0]]{ACTIONS}}
}

sub YYLexer {
    my($self)=shift;

	$$self{LEX};
}


#################
# Private stuff #
#################


sub _CheckParams {
	my($mandatory,$checklist,$inarray,$outhash)=@_;
	my($prm,$value);
	my($prmlst)={};

	while(($prm,$value)=splice(@$inarray,0,2)) {
        $prm=uc($prm);
			exists($$checklist{$prm})
		or	croak("Unknow parameter '$prm'");
			ref($value) eq $$checklist{$prm}
		or	croak("Invalid value for parameter '$prm'");
        $prm=unpack('@2A*',$prm);
		$$outhash{$prm}=$value;
	}
	for (@$mandatory) {
			exists($$outhash{$_})
		or	croak("Missing mandatory parameter '".lc($_)."'");
	}
}

sub _Error {
	print "Parse error.\n";
}

sub _DBLoad {
	{
		no strict 'refs';

			exists(${__PACKAGE__.'::'}{_DBParse})#Already loaded ?
		and	return;
	}
	my($fname)=__FILE__;
	my(@drv);
	open(DRV,"<$fname") or die "Report this as a BUG: Cannot open $fname";
	while(<DRV>) {
                	/^\s*sub\s+_Parse\s*{\s*$/ .. /^\s*}\s*#\s*_Parse\s*$/
        	and     do {
                	s/^#DBG>//;
                	push(@drv,$_);
        	}
	}
	close(DRV);

	$drv[0]=~s/_P/_DBP/;
	eval join('',@drv);
}

#Note that for loading debugging version of the driver,
#this file will be parsed from 'sub _Parse' up to '}#_Parse' inclusive.
#So, DO NOT remove comment at end of sub !!!
sub _Parse {
    my($self)=shift;

	my($rules,$states,$lex,$error)
     = @$self{ 'RULES', 'STATES', 'LEX', 'ERROR' };
	my($errstatus,$nberror,$token,$value,$stack,$check,$dotpos)
     = @$self{ 'ERRST', 'NBERR', 'TOKEN', 'VALUE', 'STACK', 'CHECK', 'DOTPOS' };

#DBG>	my($debug)=$$self{DEBUG};
#DBG>	my($dbgerror)=0;

#DBG>	my($ShowCurToken) = sub {
#DBG>		my($tok)='>';
#DBG>		for (split('',$$token)) {
#DBG>			$tok.=		(ord($_) < 32 or ord($_) > 126)
#DBG>					?	sprintf('<%02X>',ord($_))
#DBG>					:	$_;
#DBG>		}
#DBG>		$tok.='<';
#DBG>	};

	$$errstatus=0;
	$$nberror=0;
	($$token,$$value)=(undef,undef);
	@$stack=( [ 0, undef ] );
	$$check='';

    while(1) {
        my($actions,$act,$stateno);

        $stateno=$$stack[-1][0];
        $actions=$$states[$stateno];

#DBG>	print STDERR ('-' x 40),"\n";
#DBG>		$debug & 0x2
#DBG>	and	print STDERR "In state $stateno:\n";
#DBG>		$debug & 0x08
#DBG>	and	print STDERR "Stack:[".
#DBG>					 join(',',map { $$_[0] } @$stack).
#DBG>					 "]\n";


        if  (exists($$actions{ACTIONS})) {

				defined($$token)
            or	do {
				($$token,$$value)=&$lex($self);
#DBG>				$debug & 0x01
#DBG>			and	print STDERR "Need token. Got ".&$ShowCurToken."\n";
			};

            $act=   exists($$actions{ACTIONS}{$$token})
                    ?   $$actions{ACTIONS}{$$token}
                    :   exists($$actions{DEFAULT})
                        ?   $$actions{DEFAULT}
                        :   undef;
        }
        else {
            $act=$$actions{DEFAULT};
#DBG>			$debug & 0x01
#DBG>		and	print STDERR "Don't need token.\n";
        }

            defined($act)
        and do {

                $act > 0
            and do {        #shift

#DBG>				$debug & 0x04
#DBG>			and	print STDERR "Shift and go to state $act.\n";

					$$errstatus
				and	do {
					--$$errstatus;

#DBG>					$debug & 0x10
#DBG>				and	$dbgerror
#DBG>				and	$$errstatus == 0
#DBG>				and	do {
#DBG>					print STDERR "**End of Error recovery.\n";
#DBG>					$dbgerror=0;
#DBG>				};
				};


                push(@$stack,[ $act, $$value ]);

					$$token ne ''	#Don't eat the eof
				and	$$token=$$value=undef;
                next;
            };

            #reduce
            my($lhs,$len,$code,@sempar,$semval);
            ($lhs,$len,$code)=@{$$rules[-$act]};

#DBG>			$debug & 0x04
#DBG>		and	$act
#DBG>		and	print STDERR "Reduce using rule ".-$act." ($lhs,$len): ";

                $act
            or  $self->YYAccept();

            $$dotpos=$len;

                unpack('A1',$lhs) eq '@'    #In line rule
            and do {
                    $lhs =~ /^\@[0-9]+\-([0-9]+)$/
                or  die "In line rule name '$lhs' ill formed: ".
                        "report it as a BUG.\n";
                $$dotpos = $1;
            };

            @sempar =       $$dotpos
                        ?   map { $$_[1] } @$stack[ -$$dotpos .. -1 ]
                        :   ();

            $semval = $code ? &$code( $self, @sempar )
                            : @sempar ? $sempar[0] : undef;

            splice(@$stack,-$len,$len);

                $$check eq 'ACCEPT'
            and do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Accept.\n";

				return($semval);
			};

                $$check eq 'ABORT'
            and	do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Abort.\n";

				return(undef);

			};

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Back to state $$stack[-1][0], then ";

                $$check eq 'ERROR'
            or  do {
#DBG>				$debug & 0x04
#DBG>			and	print STDERR 
#DBG>				    "go to state $$states[$$stack[-1][0]]{GOTOS}{$lhs}.\n";

#DBG>				$debug & 0x10
#DBG>			and	$dbgerror
#DBG>			and	$$errstatus == 0
#DBG>			and	do {
#DBG>				print STDERR "**End of Error recovery.\n";
#DBG>				$dbgerror=0;
#DBG>			};

			    push(@$stack,
                     [ $$states[$$stack[-1][0]]{GOTOS}{$lhs}, $semval ]);
                $$check='';
                next;
            };

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Forced Error recovery.\n";

            $$check='';

        };

        #Error
            $$errstatus
        or   do {

            $$errstatus = 1;
            &$error($self);
                $$errstatus # if 0, then YYErrok has been called
            or  next;       # so continue parsing

#DBG>			$debug & 0x10
#DBG>		and	do {
#DBG>			print STDERR "**Entering Error recovery.\n";
#DBG>			++$dbgerror;
#DBG>		};

            ++$$nberror;

        };

			$$errstatus == 3	#The next token is not valid: discard it
		and	do {
				$$token eq ''	# End of input: no hope
			and	do {
#DBG>				$debug & 0x10
#DBG>			and	print STDERR "**At eof: aborting.\n";
				return(undef);
			};

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Dicard invalid token ".&$ShowCurToken.".\n";

			$$token=$$value=undef;
		};

        $$errstatus=3;

		while(	  @$stack
			  and (		not exists($$states[$$stack[-1][0]]{ACTIONS})
			        or  not exists($$states[$$stack[-1][0]]{ACTIONS}{error})
					or	$$states[$$stack[-1][0]]{ACTIONS}{error} <= 0)) {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Pop state $$stack[-1][0].\n";

			pop(@$stack);
		}

			@$stack
		or	do {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**No state left on stack: aborting.\n";

			return(undef);
		};

		#shift the error token

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Shift \$error token and go to state ".
#DBG>						 $$states[$$stack[-1][0]]{ACTIONS}{error}.
#DBG>						 ".\n";

		push(@$stack, [ $$states[$$stack[-1][0]]{ACTIONS}{error}, undef ]);

    }

    #never reached
	croak("Error in driver logic. Please, report it as a BUG");

}#_Parse
#DO NOT remove comment

1;

}
#End of include--------------------------------------------------




sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    my($self)=$class->SUPER::new( yyversion => '1.02',
                                  yystates =>
[
	{#State 0
		ACTIONS => {
			'FUNCTION' => 2,
			'USE' => 3,
			'EXTERN' => 5
		},
		GOTOS => {
			'Pragma' => 1,
			'func_decl' => 7,
			'FunctionDeclaration' => 4,
			'FunctionDeclarations' => 6,
			'Pragmas' => 9,
			'CompilationUnit' => 8
		}
	},
	{#State 1
		DEFAULT => -175
	},
	{#State 2
		ACTIONS => {
			'error' => 11,
			'IDENTIFIER' => 10
		}
	},
	{#State 3
		ACTIONS => {
			'error' => 19,
			'META' => 15,
			'URL' => 17,
			'ACCESS' => 16
		},
		GOTOS => {
			'AccessControlPragma' => 18,
			'MetaPragma' => 12,
			'PragmaDeclaration' => 13,
			'ExternalCompilationUnitPragma' => 14
		}
	},
	{#State 4
		DEFAULT => -200
	},
	{#State 5
		ACTIONS => {
			'error' => 21,
			'FUNCTION' => 20
		}
	},
	{#State 6
		ACTIONS => {
			'FUNCTION' => 2,
			'EXTERN' => 5
		},
		DEFAULT => -174,
		GOTOS => {
			'func_decl' => 7,
			'FunctionDeclaration' => 22
		}
	},
	{#State 7
		ACTIONS => {
			'error' => 26,
			")" => 23,
			'IDENTIFIER' => 24
		},
		GOTOS => {
			'FormalParameterList' => 25
		}
	},
	{#State 8
		ACTIONS => {
			'' => 27
		}
	},
	{#State 9
		ACTIONS => {
			'' => -173,
			'error' => 30,
			'FUNCTION' => 2,
			'USE' => 3,
			'EXTERN' => 5
		},
		GOTOS => {
			'Pragma' => 28,
			'func_decl' => 7,
			'FunctionDeclaration' => 4,
			'FunctionDeclarations' => 29
		}
	},
	{#State 10
		ACTIONS => {
			'error' => 31,
			"(" => 32
		}
	},
	{#State 11
		DEFAULT => -159
	},
	{#State 12
		DEFAULT => -182
	},
	{#State 13
		ACTIONS => {
			'error' => 34,
			";" => 33
		}
	},
	{#State 14
		DEFAULT => -180
	},
	{#State 15
		ACTIONS => {
			'USER' => 36,
			'NAME' => 37,
			'HTTP' => 41
		},
		GOTOS => {
			'MetaSpecifier' => 35,
			'MetaName' => 39,
			'MetaHttpEquiv' => 40,
			'MetaUserAgent' => 38
		}
	},
	{#State 16
		ACTIONS => {
			'PATH' => 44,
			'DOMAIN' => 42
		},
		GOTOS => {
			'AccessControlSpecifier' => 43
		}
	},
	{#State 17
		ACTIONS => {
			'IDENTIFIER' => 45
		}
	},
	{#State 18
		DEFAULT => -181
	},
	{#State 19
		DEFAULT => -178
	},
	{#State 20
		ACTIONS => {
			'error' => 47,
			'IDENTIFIER' => 46
		}
	},
	{#State 21
		DEFAULT => -156
	},
	{#State 22
		DEFAULT => -201
	},
	{#State 23
		ACTIONS => {
			'error' => 50,
			"{" => 49
		},
		GOTOS => {
			'Block' => 48
		}
	},
	{#State 24
		DEFAULT => -169
	},
	{#State 25
		ACTIONS => {
			'error' => 53,
			")" => 51,
			"," => 52
		}
	},
	{#State 26
		DEFAULT => -165
	},
	{#State 27
		DEFAULT => 0
	},
	{#State 28
		DEFAULT => -176
	},
	{#State 29
		ACTIONS => {
			'FUNCTION' => 2,
			'EXTERN' => 5
		},
		DEFAULT => -171,
		GOTOS => {
			'func_decl' => 7,
			'FunctionDeclaration' => 22
		}
	},
	{#State 30
		DEFAULT => -172
	},
	{#State 31
		DEFAULT => -160
	},
	{#State 32
		DEFAULT => -155
	},
	{#State 33
		DEFAULT => -177
	},
	{#State 34
		DEFAULT => -179
	},
	{#State 35
		DEFAULT => -188
	},
	{#State 36
		ACTIONS => {
			'AGENT' => 54
		}
	},
	{#State 37
		ACTIONS => {
			'STRING_LITERAL' => 56,
			'UTF8_STRING_LITERAL' => 59
		},
		GOTOS => {
			'MetaPropertyName' => 55,
			'StringLiteral' => 58,
			'MetaBody' => 57
		}
	},
	{#State 38
		DEFAULT => -191
	},
	{#State 39
		DEFAULT => -189
	},
	{#State 40
		DEFAULT => -190
	},
	{#State 41
		ACTIONS => {
			'EQUIV' => 60
		}
	},
	{#State 42
		ACTIONS => {
			'STRING_LITERAL' => 56,
			'UTF8_STRING_LITERAL' => 59
		},
		GOTOS => {
			'StringLiteral' => 61
		}
	},
	{#State 43
		DEFAULT => -184
	},
	{#State 44
		ACTIONS => {
			'STRING_LITERAL' => 56,
			'UTF8_STRING_LITERAL' => 59
		},
		GOTOS => {
			'StringLiteral' => 62
		}
	},
	{#State 45
		ACTIONS => {
			'STRING_LITERAL' => 56,
			'UTF8_STRING_LITERAL' => 59
		},
		GOTOS => {
			'StringLiteral' => 63
		}
	},
	{#State 46
		ACTIONS => {
			'error' => 64,
			"(" => 65
		}
	},
	{#State 47
		DEFAULT => -157
	},
	{#State 48
		ACTIONS => {
			";" => 66
		},
		DEFAULT => -164
	},
	{#State 49
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			"--" => 102,
			'FOR' => 101,
			'CONTINUE' => 70,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'RETURN' => 112,
			'TYPEOF' => 116,
			'WHILE' => 115,
			";" => 84,
			'INVALID_LITERAL' => 85,
			"{" => 49,
			"-" => 119,
			'BREAK' => 118,
			'IF' => 122,
			"}" => 89,
			'VAR' => 125,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'Block' => 68,
			'RelationalExpression' => 69,
			'EmptyStatement' => 71,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'ReturnStatement' => 74,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'WhileStatement' => 78,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'for_begin' => 83,
			'VariableStatement' => 86,
			'Expression' => 87,
			'StatementList' => 88,
			'ForStatement' => 90,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93,
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'ContinueStatement' => 107,
			'Statement' => 109,
			'IterationStatement' => 111,
			'FunctionName' => 113,
			'IfStatement' => 117,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'LibraryFunctionCall' => 124,
			'Literal' => 123,
			'ExpressionStatement' => 127,
			'BreakStatement' => 126,
			'LogicalORExpression' => 128
		}
	},
	{#State 50
		DEFAULT => -168
	},
	{#State 51
		ACTIONS => {
			'error' => 131,
			"{" => 49
		},
		GOTOS => {
			'Block' => 130
		}
	},
	{#State 52
		ACTIONS => {
			'IDENTIFIER' => 132
		}
	},
	{#State 53
		DEFAULT => -166
	},
	{#State 54
		ACTIONS => {
			'STRING_LITERAL' => 56,
			'UTF8_STRING_LITERAL' => 59
		},
		GOTOS => {
			'MetaPropertyName' => 55,
			'StringLiteral' => 58,
			'MetaBody' => 133
		}
	},
	{#State 55
		ACTIONS => {
			'STRING_LITERAL' => 56,
			'UTF8_STRING_LITERAL' => 59
		},
		GOTOS => {
			'MetaContent' => 134,
			'StringLiteral' => 135
		}
	},
	{#State 56
		DEFAULT => -7
	},
	{#State 57
		DEFAULT => -192
	},
	{#State 58
		DEFAULT => -197
	},
	{#State 59
		DEFAULT => -8
	},
	{#State 60
		ACTIONS => {
			'STRING_LITERAL' => 56,
			'UTF8_STRING_LITERAL' => 59
		},
		GOTOS => {
			'MetaPropertyName' => 55,
			'StringLiteral' => 58,
			'MetaBody' => 136
		}
	},
	{#State 61
		ACTIONS => {
			'PATH' => 137
		},
		DEFAULT => -185
	},
	{#State 62
		DEFAULT => -186
	},
	{#State 63
		DEFAULT => -183
	},
	{#State 64
		DEFAULT => -158
	},
	{#State 65
		DEFAULT => -154
	},
	{#State 66
		DEFAULT => -163
	},
	{#State 67
		DEFAULT => -5
	},
	{#State 68
		DEFAULT => -92
	},
	{#State 69
		ACTIONS => {
			"<=" => 138,
			"<" => 140,
			">" => 141,
			">=" => 139
		},
		DEFAULT => -59
	},
	{#State 70
		ACTIONS => {
			'error' => 143,
			";" => 142
		}
	},
	{#State 71
		DEFAULT => -94
	},
	{#State 72
		ACTIONS => {
			"&" => 144
		},
		DEFAULT => -64
	},
	{#State 73
		ACTIONS => {
			"." => 145
		}
	},
	{#State 74
		DEFAULT => -100
	},
	{#State 75
		ACTIONS => {
			"|" => 146
		},
		DEFAULT => -68
	},
	{#State 76
		ACTIONS => {
			"!=" => 147,
			"==" => 148
		},
		DEFAULT => -62
	},
	{#State 77
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 149,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 78
		DEFAULT => -122
	},
	{#State 79
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			"--" => 102,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'TYPEOF' => 116,
			'error' => 151,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'Expression' => 152,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 80
		DEFAULT => -30
	},
	{#State 81
		DEFAULT => -75
	},
	{#State 82
		DEFAULT => -14
	},
	{#State 83
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			";" => 153,
			'INVALID_LITERAL' => 85,
			'error' => 154,
			"-" => 119,
			"--" => 102,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			"~" => 129,
			'UTF8_STRING_LITERAL' => 59,
			'FLOAT_LITERAL' => 92,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'Expression' => 155,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 84
		DEFAULT => -114
	},
	{#State 85
		DEFAULT => -6
	},
	{#State 86
		DEFAULT => -93
	},
	{#State 87
		ACTIONS => {
			'error' => 158,
			";" => 156,
			"," => 157
		}
	},
	{#State 88
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			"--" => 102,
			'FOR' => 101,
			'CONTINUE' => 70,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'RETURN' => 112,
			'TYPEOF' => 116,
			'WHILE' => 115,
			";" => 84,
			'error' => 159,
			'INVALID_LITERAL' => 85,
			"{" => 49,
			"-" => 119,
			'BREAK' => 118,
			'IF' => 122,
			"}" => 160,
			'VAR' => 125,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'Block' => 68,
			'RelationalExpression' => 69,
			'EmptyStatement' => 71,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'ReturnStatement' => 74,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'WhileStatement' => 78,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'for_begin' => 83,
			'VariableStatement' => 86,
			'Expression' => 87,
			'ForStatement' => 90,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93,
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'ContinueStatement' => 107,
			'Statement' => 161,
			'IterationStatement' => 111,
			'FunctionName' => 113,
			'IfStatement' => 117,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExpressionStatement' => 127,
			'BreakStatement' => 126,
			'LogicalORExpression' => 128
		}
	},
	{#State 89
		DEFAULT => -103
	},
	{#State 90
		DEFAULT => -123
	},
	{#State 91
		ACTIONS => {
			"#" => 162
		}
	},
	{#State 92
		DEFAULT => -2
	},
	{#State 93
		ACTIONS => {
			"^" => 163
		},
		DEFAULT => -66
	},
	{#State 94
		ACTIONS => {
			'IDENTIFIER' => 164
		}
	},
	{#State 95
		DEFAULT => -42
	},
	{#State 96
		ACTIONS => {
			"&&" => 165
		},
		DEFAULT => -70
	},
	{#State 97
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 166,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 98
		DEFAULT => -90
	},
	{#State 99
		DEFAULT => -16
	},
	{#State 100
		DEFAULT => -33
	},
	{#State 101
		ACTIONS => {
			'error' => 168,
			"(" => 167
		}
	},
	{#State 102
		ACTIONS => {
			'IDENTIFIER' => 169
		}
	},
	{#State 103
		DEFAULT => -1
	},
	{#State 104
		DEFAULT => -3
	},
	{#State 105
		ACTIONS => {
			"%" => 172,
			"*" => 170,
			'DIV' => 171,
			"/" => 173
		},
		DEFAULT => -47
	},
	{#State 106
		ACTIONS => {
			">>>" => 174,
			"<<" => 175,
			">>" => 176
		},
		DEFAULT => -54
	},
	{#State 107
		DEFAULT => -98
	},
	{#State 108
		DEFAULT => -4
	},
	{#State 109
		DEFAULT => -104
	},
	{#State 110
		ACTIONS => {
			"+=" => 177,
			"-=" => 178,
			"(" => -21,
			"/=" => 179,
			"div=" => 180,
			"&=" => 181,
			"|=" => 182,
			"=" => 183,
			"." => -23,
			"++" => 184,
			"*=" => 185,
			"#" => -22,
			"--" => 187,
			"<<=" => 188,
			"%=" => 189,
			">>>=" => 190,
			">>=" => 192,
			"^=" => 191
		},
		DEFAULT => -9,
		GOTOS => {
			'AssignmentOperator' => 186
		}
	},
	{#State 111
		DEFAULT => -97
	},
	{#State 112
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			";" => 193,
			'INVALID_LITERAL' => 85,
			'error' => 194,
			"-" => 119,
			"--" => 102,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			"~" => 129,
			'UTF8_STRING_LITERAL' => 59,
			'FLOAT_LITERAL' => 92,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'Expression' => 195,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 113
		ACTIONS => {
			"(" => 196
		},
		GOTOS => {
			'Arguments' => 197
		}
	},
	{#State 114
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 198,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 115
		ACTIONS => {
			'error' => 200,
			"(" => 199
		}
	},
	{#State 116
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 201,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 117
		DEFAULT => -96
	},
	{#State 118
		ACTIONS => {
			'error' => 203,
			";" => 202
		}
	},
	{#State 119
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 204,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 120
		DEFAULT => -15
	},
	{#State 121
		ACTIONS => {
			"+" => 205,
			"-" => 206
		},
		DEFAULT => -50
	},
	{#State 122
		ACTIONS => {
			'error' => 208,
			"(" => 207
		}
	},
	{#State 123
		DEFAULT => -10
	},
	{#State 124
		DEFAULT => -17
	},
	{#State 125
		ACTIONS => {
			'error' => 209,
			'IDENTIFIER' => 212
		},
		GOTOS => {
			'VariableDeclaration' => 210,
			'VariableDeclarationList' => 211
		}
	},
	{#State 126
		DEFAULT => -99
	},
	{#State 127
		DEFAULT => -95
	},
	{#State 128
		ACTIONS => {
			"||" => 214,
			"?" => 213
		},
		DEFAULT => -72
	},
	{#State 129
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 215,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 130
		ACTIONS => {
			";" => 216
		},
		DEFAULT => -162
	},
	{#State 131
		DEFAULT => -167
	},
	{#State 132
		DEFAULT => -170
	},
	{#State 133
		DEFAULT => -194
	},
	{#State 134
		ACTIONS => {
			'STRING_LITERAL' => 56,
			'UTF8_STRING_LITERAL' => 59
		},
		DEFAULT => -196,
		GOTOS => {
			'MetaScheme' => 217,
			'StringLiteral' => 218
		}
	},
	{#State 135
		DEFAULT => -198
	},
	{#State 136
		DEFAULT => -193
	},
	{#State 137
		ACTIONS => {
			'STRING_LITERAL' => 56,
			'UTF8_STRING_LITERAL' => 59
		},
		GOTOS => {
			'StringLiteral' => 219
		}
	},
	{#State 138
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 220
		}
	},
	{#State 139
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 221
		}
	},
	{#State 140
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 222
		}
	},
	{#State 141
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 223
		}
	},
	{#State 142
		DEFAULT => -146
	},
	{#State 143
		DEFAULT => -147
	},
	{#State 144
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'EqualityExpression' => 224,
			'MultiplicativeExpression' => 105,
			'ExternalScriptName' => 91,
			'ShiftExpression' => 106
		}
	},
	{#State 145
		ACTIONS => {
			'IDENTIFIER' => 225
		},
		GOTOS => {
			'FunctionName' => 226
		}
	},
	{#State 146
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 227
		}
	},
	{#State 147
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 228,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ExternalScriptName' => 91,
			'ShiftExpression' => 106
		}
	},
	{#State 148
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 229,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ExternalScriptName' => 91,
			'ShiftExpression' => 106
		}
	},
	{#State 149
		DEFAULT => -35
	},
	{#State 150
		ACTIONS => {
			"#" => -22,
			"--" => 187,
			"(" => -21,
			"." => -23,
			"++" => 184
		},
		DEFAULT => -9
	},
	{#State 151
		DEFAULT => -12
	},
	{#State 152
		ACTIONS => {
			'error' => 230,
			")" => 231,
			"," => 157
		}
	},
	{#State 153
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			"--" => 102,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			")" => 234,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'TYPEOF' => 116,
			'error' => 232,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'Expression' => 233,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 154
		DEFAULT => -140
	},
	{#State 155
		ACTIONS => {
			'error' => 236,
			";" => 235,
			"," => 157
		}
	},
	{#State 156
		DEFAULT => -115
	},
	{#State 157
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 237,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 158
		DEFAULT => -116
	},
	{#State 159
		DEFAULT => -102
	},
	{#State 160
		DEFAULT => -101
	},
	{#State 161
		DEFAULT => -105
	},
	{#State 162
		ACTIONS => {
			'IDENTIFIER' => 225
		},
		GOTOS => {
			'FunctionName' => 238
		}
	},
	{#State 163
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'LibraryName' => 73,
			'BitwiseANDExpression' => 239,
			'StringLiteral' => 104,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExternalScriptName' => 91
		}
	},
	{#State 164
		DEFAULT => -36
	},
	{#State 165
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'EqualityExpression' => 76,
			'BitwiseORExpression' => 240,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 166
		DEFAULT => -41
	},
	{#State 167
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			";" => 241,
			'INVALID_LITERAL' => 85,
			'error' => 242,
			"-" => 119,
			"--" => 102,
			'VAR' => 244,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			"~" => 129,
			'UTF8_STRING_LITERAL' => 59,
			'FLOAT_LITERAL' => 92,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'Expression' => 243,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 168
		DEFAULT => -131
	},
	{#State 169
		DEFAULT => -37
	},
	{#State 170
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 245,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 171
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 246,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 172
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 247,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 173
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 248,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91
		}
	},
	{#State 174
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 249,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91,
			'MultiplicativeExpression' => 105
		}
	},
	{#State 175
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 250,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91,
			'MultiplicativeExpression' => 105
		}
	},
	{#State 176
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 251,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91,
			'MultiplicativeExpression' => 105
		}
	},
	{#State 177
		DEFAULT => -81
	},
	{#State 178
		DEFAULT => -82
	},
	{#State 179
		DEFAULT => -79
	},
	{#State 180
		DEFAULT => -89
	},
	{#State 181
		DEFAULT => -86
	},
	{#State 182
		DEFAULT => -88
	},
	{#State 183
		DEFAULT => -77
	},
	{#State 184
		DEFAULT => -31
	},
	{#State 185
		DEFAULT => -78
	},
	{#State 186
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 252,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 187
		DEFAULT => -32
	},
	{#State 188
		DEFAULT => -83
	},
	{#State 189
		DEFAULT => -80
	},
	{#State 190
		DEFAULT => -85
	},
	{#State 191
		DEFAULT => -87
	},
	{#State 192
		DEFAULT => -84
	},
	{#State 193
		DEFAULT => -150
	},
	{#State 194
		DEFAULT => -152
	},
	{#State 195
		ACTIONS => {
			'error' => 254,
			";" => 253,
			"," => 157
		}
	},
	{#State 196
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			")" => 257,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 256,
			'ArgumentList' => 255,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 197
		DEFAULT => -18
	},
	{#State 198
		DEFAULT => -38
	},
	{#State 199
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			"--" => 102,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'TYPEOF' => 116,
			'error' => 258,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'Expression' => 259,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 200
		DEFAULT => -125
	},
	{#State 201
		DEFAULT => -34
	},
	{#State 202
		DEFAULT => -148
	},
	{#State 203
		DEFAULT => -149
	},
	{#State 204
		DEFAULT => -39
	},
	{#State 205
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91,
			'MultiplicativeExpression' => 260
		}
	},
	{#State 206
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'CallExpression' => 80,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'LocalScriptFunctionCall' => 120,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'ExternalScriptName' => 91,
			'MultiplicativeExpression' => 261
		}
	},
	{#State 207
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			"--" => 102,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'TYPEOF' => 116,
			'error' => 262,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'Expression' => 263,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 208
		DEFAULT => -119
	},
	{#State 209
		DEFAULT => -107
	},
	{#State 210
		DEFAULT => -109
	},
	{#State 211
		ACTIONS => {
			'error' => 266,
			";" => 264,
			"," => 265
		}
	},
	{#State 212
		ACTIONS => {
			"=" => 267
		},
		DEFAULT => -112,
		GOTOS => {
			'VariableInitializer' => 268
		}
	},
	{#State 213
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 269,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 214
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 270,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 215
		DEFAULT => -40
	},
	{#State 216
		DEFAULT => -161
	},
	{#State 217
		DEFAULT => -195
	},
	{#State 218
		DEFAULT => -199
	},
	{#State 219
		DEFAULT => -187
	},
	{#State 220
		ACTIONS => {
			">>>" => 174,
			"<<" => 175,
			">>" => 176
		},
		DEFAULT => -57
	},
	{#State 221
		ACTIONS => {
			">>>" => 174,
			"<<" => 175,
			">>" => 176
		},
		DEFAULT => -58
	},
	{#State 222
		ACTIONS => {
			">>>" => 174,
			"<<" => 175,
			">>" => 176
		},
		DEFAULT => -55
	},
	{#State 223
		ACTIONS => {
			">>>" => 174,
			"<<" => 175,
			">>" => 176
		},
		DEFAULT => -56
	},
	{#State 224
		ACTIONS => {
			"!=" => 147,
			"==" => 148
		},
		DEFAULT => -63
	},
	{#State 225
		DEFAULT => -21
	},
	{#State 226
		ACTIONS => {
			"(" => 196
		},
		GOTOS => {
			'Arguments' => 271
		}
	},
	{#State 227
		ACTIONS => {
			"^" => 163
		},
		DEFAULT => -67
	},
	{#State 228
		ACTIONS => {
			"<=" => 138,
			"<" => 140,
			">" => 141,
			">=" => 139
		},
		DEFAULT => -61
	},
	{#State 229
		ACTIONS => {
			"<=" => 138,
			"<" => 140,
			">" => 141,
			">=" => 139
		},
		DEFAULT => -60
	},
	{#State 230
		DEFAULT => -13
	},
	{#State 231
		DEFAULT => -11
	},
	{#State 232
		DEFAULT => -144
	},
	{#State 233
		ACTIONS => {
			'error' => 272,
			")" => 273,
			"," => 157
		}
	},
	{#State 234
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			'CONTINUE' => 70,
			'FOR' => 101,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'RETURN' => 112,
			'TYPEOF' => 116,
			'WHILE' => 115,
			";" => 84,
			'INVALID_LITERAL' => 85,
			"{" => 49,
			"-" => 119,
			'BREAK' => 118,
			'IF' => 122,
			'VAR' => 125,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'Block' => 68,
			'RelationalExpression' => 69,
			'EmptyStatement' => 71,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'ReturnStatement' => 74,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'WhileStatement' => 78,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'for_begin' => 83,
			'VariableStatement' => 86,
			'Expression' => 87,
			'ForStatement' => 90,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93,
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'ContinueStatement' => 107,
			'Statement' => 274,
			'IterationStatement' => 111,
			'FunctionName' => 113,
			'IfStatement' => 117,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExpressionStatement' => 127,
			'BreakStatement' => 126,
			'LogicalORExpression' => 128
		}
	},
	{#State 235
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			"--" => 102,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			")" => 277,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'TYPEOF' => 116,
			'error' => 275,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'FunctionName' => 113,
			'PrimaryExpression' => 82,
			'Expression' => 276,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 236
		DEFAULT => -141
	},
	{#State 237
		DEFAULT => -91
	},
	{#State 238
		ACTIONS => {
			"(" => 196
		},
		GOTOS => {
			'Arguments' => 278
		}
	},
	{#State 239
		ACTIONS => {
			"&" => 144
		},
		DEFAULT => -65
	},
	{#State 240
		ACTIONS => {
			"|" => 146
		},
		DEFAULT => -69
	},
	{#State 241
		DEFAULT => -129
	},
	{#State 242
		DEFAULT => -132
	},
	{#State 243
		ACTIONS => {
			'error' => 280,
			";" => 279,
			"," => 157
		}
	},
	{#State 244
		ACTIONS => {
			'error' => 281,
			'IDENTIFIER' => 212
		},
		GOTOS => {
			'VariableDeclaration' => 210,
			'VariableDeclarationList' => 282
		}
	},
	{#State 245
		DEFAULT => -43
	},
	{#State 246
		DEFAULT => -45
	},
	{#State 247
		DEFAULT => -46
	},
	{#State 248
		DEFAULT => -44
	},
	{#State 249
		ACTIONS => {
			"+" => 205,
			"-" => 206
		},
		DEFAULT => -53
	},
	{#State 250
		ACTIONS => {
			"+" => 205,
			"-" => 206
		},
		DEFAULT => -51
	},
	{#State 251
		ACTIONS => {
			"+" => 205,
			"-" => 206
		},
		DEFAULT => -52
	},
	{#State 252
		DEFAULT => -76
	},
	{#State 253
		DEFAULT => -151
	},
	{#State 254
		DEFAULT => -153
	},
	{#State 255
		ACTIONS => {
			'error' => 284,
			")" => 285,
			"," => 283
		}
	},
	{#State 256
		DEFAULT => -27
	},
	{#State 257
		DEFAULT => -24
	},
	{#State 258
		DEFAULT => -126
	},
	{#State 259
		ACTIONS => {
			'error' => 286,
			")" => 287,
			"," => 157
		}
	},
	{#State 260
		ACTIONS => {
			"%" => 172,
			"*" => 170,
			'DIV' => 171,
			"/" => 173
		},
		DEFAULT => -48
	},
	{#State 261
		ACTIONS => {
			"%" => 172,
			"*" => 170,
			'DIV' => 171,
			"/" => 173
		},
		DEFAULT => -49
	},
	{#State 262
		DEFAULT => -120
	},
	{#State 263
		ACTIONS => {
			'error' => 288,
			")" => 289,
			"," => 157
		}
	},
	{#State 264
		DEFAULT => -106
	},
	{#State 265
		ACTIONS => {
			'IDENTIFIER' => 212
		},
		GOTOS => {
			'VariableDeclaration' => 290
		}
	},
	{#State 266
		DEFAULT => -108
	},
	{#State 267
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 150,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 291,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 268
		DEFAULT => -111
	},
	{#State 269
		ACTIONS => {
			'error' => 292,
			":" => 293
		}
	},
	{#State 270
		ACTIONS => {
			"&&" => 165
		},
		DEFAULT => -71
	},
	{#State 271
		DEFAULT => -20
	},
	{#State 272
		DEFAULT => -145
	},
	{#State 273
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			'CONTINUE' => 70,
			'FOR' => 101,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'RETURN' => 112,
			'TYPEOF' => 116,
			'WHILE' => 115,
			";" => 84,
			'INVALID_LITERAL' => 85,
			"{" => 49,
			"-" => 119,
			'BREAK' => 118,
			'IF' => 122,
			'VAR' => 125,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'Block' => 68,
			'RelationalExpression' => 69,
			'EmptyStatement' => 71,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'ReturnStatement' => 74,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'WhileStatement' => 78,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'for_begin' => 83,
			'VariableStatement' => 86,
			'Expression' => 87,
			'ForStatement' => 90,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93,
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'ContinueStatement' => 107,
			'Statement' => 294,
			'IterationStatement' => 111,
			'FunctionName' => 113,
			'IfStatement' => 117,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExpressionStatement' => 127,
			'BreakStatement' => 126,
			'LogicalORExpression' => 128
		}
	},
	{#State 274
		DEFAULT => -139
	},
	{#State 275
		DEFAULT => -142
	},
	{#State 276
		ACTIONS => {
			'error' => 295,
			")" => 296,
			"," => 157
		}
	},
	{#State 277
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			'CONTINUE' => 70,
			'FOR' => 101,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'RETURN' => 112,
			'TYPEOF' => 116,
			'WHILE' => 115,
			";" => 84,
			'INVALID_LITERAL' => 85,
			"{" => 49,
			"-" => 119,
			'BREAK' => 118,
			'IF' => 122,
			'VAR' => 125,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'Block' => 68,
			'RelationalExpression' => 69,
			'EmptyStatement' => 71,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'ReturnStatement' => 74,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'WhileStatement' => 78,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'for_begin' => 83,
			'VariableStatement' => 86,
			'Expression' => 87,
			'ForStatement' => 90,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93,
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'ContinueStatement' => 107,
			'Statement' => 297,
			'IterationStatement' => 111,
			'FunctionName' => 113,
			'IfStatement' => 117,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExpressionStatement' => 127,
			'BreakStatement' => 126,
			'LogicalORExpression' => 128
		}
	},
	{#State 278
		DEFAULT => -19
	},
	{#State 279
		DEFAULT => -128
	},
	{#State 280
		DEFAULT => -133
	},
	{#State 281
		DEFAULT => -134
	},
	{#State 282
		ACTIONS => {
			'error' => 299,
			";" => 298,
			"," => 265
		}
	},
	{#State 283
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			"--" => 102,
			'INTEGER_LITERAL' => 103,
			'ISVALID' => 77,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'TYPEOF' => 116,
			'error' => 300,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 301,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 284
		DEFAULT => -26
	},
	{#State 285
		DEFAULT => -25
	},
	{#State 286
		DEFAULT => -127
	},
	{#State 287
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			'CONTINUE' => 70,
			'FOR' => 101,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'RETURN' => 112,
			'TYPEOF' => 116,
			'WHILE' => 115,
			";" => 84,
			'INVALID_LITERAL' => 85,
			"{" => 49,
			"-" => 119,
			'BREAK' => 118,
			'IF' => 122,
			'VAR' => 125,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'Block' => 68,
			'RelationalExpression' => 69,
			'EmptyStatement' => 71,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'ReturnStatement' => 74,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'WhileStatement' => 78,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'for_begin' => 83,
			'VariableStatement' => 86,
			'Expression' => 87,
			'ForStatement' => 90,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93,
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'ContinueStatement' => 107,
			'Statement' => 302,
			'IterationStatement' => 111,
			'FunctionName' => 113,
			'IfStatement' => 117,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExpressionStatement' => 127,
			'BreakStatement' => 126,
			'LogicalORExpression' => 128
		}
	},
	{#State 288
		DEFAULT => -121
	},
	{#State 289
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			'CONTINUE' => 70,
			'FOR' => 101,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'RETURN' => 112,
			'TYPEOF' => 116,
			'WHILE' => 115,
			";" => 84,
			'INVALID_LITERAL' => 85,
			"{" => 49,
			"-" => 119,
			'BREAK' => 118,
			'IF' => 122,
			'VAR' => 125,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'Block' => 68,
			'RelationalExpression' => 69,
			'EmptyStatement' => 71,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'ReturnStatement' => 74,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'WhileStatement' => 78,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'for_begin' => 83,
			'VariableStatement' => 86,
			'Expression' => 87,
			'ForStatement' => 90,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93,
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'ContinueStatement' => 107,
			'Statement' => 303,
			'IterationStatement' => 111,
			'FunctionName' => 113,
			'IfStatement' => 117,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExpressionStatement' => 127,
			'BreakStatement' => 126,
			'LogicalORExpression' => 128
		}
	},
	{#State 290
		DEFAULT => -110
	},
	{#State 291
		DEFAULT => -113
	},
	{#State 292
		DEFAULT => -74
	},
	{#State 293
		ACTIONS => {
			'TRUE_LITERAL' => 108,
			"!" => 97,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'FALSE_LITERAL' => 67,
			'TYPEOF' => 116,
			'INVALID_LITERAL' => 85,
			"-" => 119,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			"++" => 94
		},
		GOTOS => {
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 304,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'RelationalExpression' => 69,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'StringLiteral' => 104,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'FunctionName' => 113,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'LogicalORExpression' => 128,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93
		}
	},
	{#State 294
		DEFAULT => -138
	},
	{#State 295
		DEFAULT => -143
	},
	{#State 296
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			'CONTINUE' => 70,
			'FOR' => 101,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'RETURN' => 112,
			'TYPEOF' => 116,
			'WHILE' => 115,
			";" => 84,
			'INVALID_LITERAL' => 85,
			"{" => 49,
			"-" => 119,
			'BREAK' => 118,
			'IF' => 122,
			'VAR' => 125,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'Block' => 68,
			'RelationalExpression' => 69,
			'EmptyStatement' => 71,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'ReturnStatement' => 74,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'WhileStatement' => 78,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'for_begin' => 83,
			'VariableStatement' => 86,
			'Expression' => 87,
			'ForStatement' => 90,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93,
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'ContinueStatement' => 107,
			'Statement' => 305,
			'IterationStatement' => 111,
			'FunctionName' => 113,
			'IfStatement' => 117,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExpressionStatement' => 127,
			'BreakStatement' => 126,
			'LogicalORExpression' => 128
		}
	},
	{#State 297
		DEFAULT => -137
	},
	{#State 298
		DEFAULT => -130
	},
	{#State 299
		DEFAULT => -135
	},
	{#State 300
		DEFAULT => -29
	},
	{#State 301
		DEFAULT => -28
	},
	{#State 302
		DEFAULT => -124
	},
	{#State 303
		ACTIONS => {
			'ELSE' => 306
		},
		DEFAULT => -118
	},
	{#State 304
		DEFAULT => -73
	},
	{#State 305
		DEFAULT => -136
	},
	{#State 306
		ACTIONS => {
			"!" => 97,
			'FALSE_LITERAL' => 67,
			'CONTINUE' => 70,
			'FOR' => 101,
			"--" => 102,
			'ISVALID' => 77,
			'INTEGER_LITERAL' => 103,
			'UTF8_STRING_LITERAL' => 59,
			"(" => 79,
			'TRUE_LITERAL' => 108,
			'IDENTIFIER' => 110,
			'STRING_LITERAL' => 56,
			"+" => 114,
			'RETURN' => 112,
			'TYPEOF' => 116,
			'WHILE' => 115,
			";" => 84,
			'INVALID_LITERAL' => 85,
			"{" => 49,
			"-" => 119,
			'BREAK' => 118,
			'IF' => 122,
			'VAR' => 125,
			"~" => 129,
			'FLOAT_LITERAL' => 92,
			"++" => 94
		},
		GOTOS => {
			'Block' => 68,
			'RelationalExpression' => 69,
			'EmptyStatement' => 71,
			'BitwiseANDExpression' => 72,
			'LibraryName' => 73,
			'ReturnStatement' => 74,
			'BitwiseORExpression' => 75,
			'EqualityExpression' => 76,
			'WhileStatement' => 78,
			'CallExpression' => 80,
			'ConditionalExpression' => 81,
			'PrimaryExpression' => 82,
			'for_begin' => 83,
			'VariableStatement' => 86,
			'Expression' => 87,
			'ForStatement' => 90,
			'ExternalScriptName' => 91,
			'BitwiseXORExpression' => 93,
			'UnaryExpression' => 95,
			'LogicalANDExpression' => 96,
			'AssignmentExpression' => 98,
			'ExternalScriptFunctionCall' => 99,
			'PostfixExpression' => 100,
			'StringLiteral' => 104,
			'MultiplicativeExpression' => 105,
			'ShiftExpression' => 106,
			'ContinueStatement' => 107,
			'Statement' => 307,
			'IterationStatement' => 111,
			'FunctionName' => 113,
			'IfStatement' => 117,
			'LocalScriptFunctionCall' => 120,
			'AdditiveExpression' => 121,
			'Literal' => 123,
			'LibraryFunctionCall' => 124,
			'ExpressionStatement' => 127,
			'BreakStatement' => 126,
			'LogicalORExpression' => 128
		}
	},
	{#State 307
		DEFAULT => -117
	}
],
                                  yyrules  =>
[
	[#Rule 0
		 '$start', 2, undef
	],
	[#Rule 1
		 'Literal', 1,
sub
#line 47 "parser.yp"
{
			# always positive
			use Math::BigInt;
			if ($_[1]->bcmp(new Math::BigInt('2 147 483 648')) > 0) {
				$_[0]->Error("Integer $_[1] is out of range.\n");
				new LoadConst($_[0],
						'TypeDef'			=>	'TYPE_INVALID',
				);
			} else {
				new LoadConst($_[0],
						'TypeDef'			=>	'TYPE_INTEGER',
						'Value'				=>	$_[1]
				);
			}
		}
	],
	[#Rule 2
		 'Literal', 1,
sub
#line 63 "parser.yp"
{
			# always positive
			use Math::BigFloat;
			if ($_[1]->fcmp(new Math::BigFloat('3.40282347e+38')) > 0) {
				$_[0]->Error("Float $_[1] is out of range.\n");
				new LoadConst($_[0],
						'TypeDef'			=>	'TYPE_INVALID',
				);
			} else {
				if ($_[1]->fcmp(new Math::BigFloat('1.17549435e-38')) < 0) {
					$_[0]->Warning("Float $_[1] is underflow.\n");
					$_[1] = new Math::BigFloat('0.0');
				}
				new LoadConst($_[0],
						'TypeDef'			=>	'TYPE_FLOAT',
						'Value'				=>	$_[1]
				);
			}
		}
	],
	[#Rule 3
		 'Literal', 1, undef
	],
	[#Rule 4
		 'Literal', 1,
sub
#line 84 "parser.yp"
{
			new LoadConst($_[0],
					'TypeDef'			=>	'TYPE_BOOLEAN',
					'Value'				=>	1
			);
		}
	],
	[#Rule 5
		 'Literal', 1,
sub
#line 91 "parser.yp"
{
			new LoadConst($_[0],
					'TypeDef'			=>	'TYPE_BOOLEAN',
					'Value'				=>	0
			);
		}
	],
	[#Rule 6
		 'Literal', 1,
sub
#line 98 "parser.yp"
{
			new LoadConst($_[0],
					'TypeDef'			=>	'TYPE_INVALID',
			);
		}
	],
	[#Rule 7
		 'StringLiteral', 1,
sub
#line 107 "parser.yp"
{
			new LoadConst($_[0],
					'TypeDef'			=>	'TYPE_STRING',
					'Value'				=>	$_[1]
			);
		}
	],
	[#Rule 8
		 'StringLiteral', 1,
sub
#line 114 "parser.yp"
{
			new LoadConst($_[0],
					'TypeDef'			=>	'TYPE_UTF8_STRING',
					'Value'				=>	$_[1]
			);
		}
	],
	[#Rule 9
		 'PrimaryExpression', 1,
sub
#line 124 "parser.yp"
{
			my $var = $_[0]->YYData->{symbtab_var}->Lookup($_[1]);
			new LoadVar($_[0],
					'Definition'		=>	$var
			);
		}
	],
	[#Rule 10
		 'PrimaryExpression', 1, undef
	],
	[#Rule 11
		 'PrimaryExpression', 3,
sub
#line 132 "parser.yp"
{
			$_[2];
		}
	],
	[#Rule 12
		 'PrimaryExpression', 2,
sub
#line 136 "parser.yp"
{
			$_[0]->Error("invalid expression.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 13
		 'PrimaryExpression', 3,
sub
#line 141 "parser.yp"
{
			$_[0]->Error("')' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 14
		 'CallExpression', 1, undef
	],
	[#Rule 15
		 'CallExpression', 1, undef
	],
	[#Rule 16
		 'CallExpression', 1, undef
	],
	[#Rule 17
		 'CallExpression', 1, undef
	],
	[#Rule 18
		 'LocalScriptFunctionCall', 2,
sub
#line 156 "parser.yp"
{
			my $nbargs = (defined $_[2]) ? $_[2]->{OpCode}->{Index} : 0;
			my $def = $_[0]->YYData->{symbtab_func}->LookupLocal($_[1]);
			my $call = new Call($_[0],
					'Definition'		=>	$def,
					'Index'				=>	$nbargs
			);
			(defined $_[2]) ? $_[2]->concat($call) : $call;
		}
	],
	[#Rule 19
		 'ExternalScriptFunctionCall', 4,
sub
#line 169 "parser.yp"
{
			my $nbargs = (defined $_[4]) ? $_[4]->{OpCode}->{Index} : 0;
			my $def = $_[0]->YYData->{symbtab_func}->LookupExternal($_[1], $_[3], $nbargs);
			my $call = new CallUrl($_[0],
					'Definition'		=>	$def,
					'Url'				=>	$_[0]->YYData->{symbtab_url}->Lookup($_[1])
			);
			(defined $_[4]) ? $_[4]->concat($call) : $call;
		}
	],
	[#Rule 20
		 'LibraryFunctionCall', 4,
sub
#line 182 "parser.yp"
{
			my $nbargs = (defined $_[4]) ? $_[4]->{OpCode}->{Index} : 0;
			my $def = $_[0]->YYData->{symbtab_func}->LookupLibrary($_[1], $_[3], $nbargs)
					if ($_[0]->YYData->{symbtab_lib}->Lookup($_[1]));
			my $call = new CallLib($_[0],
					'Definition'		=>	$def
			);
			(defined $_[4]) ? $_[4]->concat($call) : $call;
		}
	],
	[#Rule 21
		 'FunctionName', 1, undef
	],
	[#Rule 22
		 'ExternalScriptName', 1, undef
	],
	[#Rule 23
		 'LibraryName', 1, undef
	],
	[#Rule 24
		 'Arguments', 2,
sub
#line 207 "parser.yp"
{
			undef;
		}
	],
	[#Rule 25
		 'Arguments', 3,
sub
#line 211 "parser.yp"
{
			$_[2];
		}
	],
	[#Rule 26
		 'Arguments', 3,
sub
#line 215 "parser.yp"
{
			$_[0]->Error("')' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 27
		 'ArgumentList', 1,
sub
#line 223 "parser.yp"
{
			$_[1]->configure(
					'Index'				=>	1	# nb args
			);
		}
	],
	[#Rule 28
		 'ArgumentList', 3,
sub
#line 229 "parser.yp"
{
			$_[1]->concat($_[3]);
			$_[1]->configure(
					'Index'				=>	$_[1]->{OpCode}->{Index} + 1	# nb args
			);
		}
	],
	[#Rule 29
		 'ArgumentList', 3,
sub
#line 236 "parser.yp"
{
			$_[0]->Error("invalid argument.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 30
		 'PostfixExpression', 1, undef
	],
	[#Rule 31
		 'PostfixExpression', 2,
sub
#line 245 "parser.yp"
{
			my $var = $_[0]->YYData->{symbtab_var}->Lookup($_[1]);
			my $load = new LoadVar($_[0],
					'Definition'		=>	$var
			);
			my $incr = new IncrVar($_[0],
					'Definition'		=>	$var
			);
			$load->concat($incr);
		}
	],
	[#Rule 32
		 'PostfixExpression', 2,
sub
#line 256 "parser.yp"
{
			my $var = $_[0]->YYData->{symbtab_var}->Lookup($_[1]);
			my $load = new LoadVar($_[0],
					'Definition'		=>	$var
			);
			my $decr = new DecrVar($_[0],
					'Definition'		=>	$var
			);
			$load->concat($decr);
		}
	],
	[#Rule 33
		 'UnaryExpression', 1, undef
	],
	[#Rule 34
		 'UnaryExpression', 2,
sub
#line 271 "parser.yp"
{
			BuildUnop($_[0], $_[1], $_[2]);
		}
	],
	[#Rule 35
		 'UnaryExpression', 2,
sub
#line 275 "parser.yp"
{
			BuildUnop($_[0], $_[1], $_[2]);
		}
	],
	[#Rule 36
		 'UnaryExpression', 2,
sub
#line 279 "parser.yp"
{
			my $var = $_[0]->YYData->{symbtab_var}->Lookup($_[2]);
			my $incr = new IncrVar($_[0],
					'Definition'		=>	$var
			);
			my $load = new LoadVar($_[0],
					'Definition'		=>	$var
			);
			$incr->concat($load);
		}
	],
	[#Rule 37
		 'UnaryExpression', 2,
sub
#line 290 "parser.yp"
{
			my $var = $_[0]->YYData->{symbtab_var}->Lookup($_[2]);
			my $decr = new IncrVar($_[0],
					'Definition'		=>	$var
			);
			my $load = new LoadVar($_[0],
					'Definition'		=>	$var
			);
			$decr->concat($load);
		}
	],
	[#Rule 38
		 'UnaryExpression', 2,
sub
#line 301 "parser.yp"
{
			$_[2];
		}
	],
	[#Rule 39
		 'UnaryExpression', 2,
sub
#line 305 "parser.yp"
{
			BuildUnop($_[0], $_[1], $_[2]);
		}
	],
	[#Rule 40
		 'UnaryExpression', 2,
sub
#line 309 "parser.yp"
{
			BuildUnop($_[0], $_[1], $_[2]);
		}
	],
	[#Rule 41
		 'UnaryExpression', 2,
sub
#line 313 "parser.yp"
{
			BuildUnop($_[0], $_[1], $_[2]);
		}
	],
	[#Rule 42
		 'MultiplicativeExpression', 1, undef
	],
	[#Rule 43
		 'MultiplicativeExpression', 3,
sub
#line 321 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 44
		 'MultiplicativeExpression', 3,
sub
#line 325 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 45
		 'MultiplicativeExpression', 3,
sub
#line 329 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 46
		 'MultiplicativeExpression', 3,
sub
#line 333 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 47
		 'AdditiveExpression', 1, undef
	],
	[#Rule 48
		 'AdditiveExpression', 3,
sub
#line 341 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 49
		 'AdditiveExpression', 3,
sub
#line 345 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 50
		 'ShiftExpression', 1, undef
	],
	[#Rule 51
		 'ShiftExpression', 3,
sub
#line 353 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 52
		 'ShiftExpression', 3,
sub
#line 357 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 53
		 'ShiftExpression', 3,
sub
#line 361 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 54
		 'RelationalExpression', 1, undef
	],
	[#Rule 55
		 'RelationalExpression', 3,
sub
#line 369 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 56
		 'RelationalExpression', 3,
sub
#line 373 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 57
		 'RelationalExpression', 3,
sub
#line 377 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 58
		 'RelationalExpression', 3,
sub
#line 381 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 59
		 'EqualityExpression', 1, undef
	],
	[#Rule 60
		 'EqualityExpression', 3,
sub
#line 389 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 61
		 'EqualityExpression', 3,
sub
#line 393 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 62
		 'BitwiseANDExpression', 1, undef
	],
	[#Rule 63
		 'BitwiseANDExpression', 3,
sub
#line 401 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 64
		 'BitwiseXORExpression', 1, undef
	],
	[#Rule 65
		 'BitwiseXORExpression', 3,
sub
#line 409 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 66
		 'BitwiseORExpression', 1, undef
	],
	[#Rule 67
		 'BitwiseORExpression', 3,
sub
#line 417 "parser.yp"
{
			BuildBinop($_[0], $_[1], $_[2], $_[3]);
		}
	],
	[#Rule 68
		 'LogicalANDExpression', 1, undef
	],
	[#Rule 69
		 'LogicalANDExpression', 3,
sub
#line 425 "parser.yp"
{
			BuildLogop($_[0], $_[1], new ScAnd($_[0]), $_[3]);
		}
	],
	[#Rule 70
		 'LogicalORExpression', 1, undef
	],
	[#Rule 71
		 'LogicalORExpression', 3,
sub
#line 433 "parser.yp"
{
			BuildLogop($_[0], $_[1], new ScOr($_[0]), $_[3]);
		}
	],
	[#Rule 72
		 'ConditionalExpression', 1, undef
	],
	[#Rule 73
		 'ConditionalExpression', 5,
sub
#line 441 "parser.yp"
{
			BuildIfElse($_[0], $_[1], $_[3], $_[5]);
		}
	],
	[#Rule 74
		 'ConditionalExpression', 4,
sub
#line 445 "parser.yp"
{
			$_[0]->Error("':' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 75
		 'AssignmentExpression', 1, undef
	],
	[#Rule 76
		 'AssignmentExpression', 3,
sub
#line 454 "parser.yp"
{
			my $asg;
			my $var = $_[0]->YYData->{symbtab_var}->Lookup($_[1]);
			if        ($_[2] eq '=') {
				my $store1 = new StoreVar($_[0],
						'Definition'		=>	$var
				);
				$asg = $_[3]->concat($store1);
			} elsif ($_[2] eq '+=') {
				my $add = new AddAsg($_[0],
						'Definition'		=>	$var
				);
				$asg = $_[3]->concat($add);
			} elsif ($_[2] eq '-=') {
				my $sub = new SubAsg($_[0],
						'Definition'		=>	$var
				);
				$asg = $_[3]->concat($sub);
			} else {
				my $load1 = new LoadVar($_[0],
						'Definition'		=>	$var
				);
				my $binop = BuildBinop($_[0], $load1, $_[2], $_[3]);
				my $store2 = new StoreVar($_[0],
						'Definition'		=>	$var
				);
				$asg = $binop->concat($store2);
			}
			my $load2 = new LoadVar($_[0],
					'Definition'		=>	$var
			);
			$asg->concat($load2);
		}
	],
	[#Rule 77
		 'AssignmentOperator', 1, undef
	],
	[#Rule 78
		 'AssignmentOperator', 1,
sub
#line 492 "parser.yp"
{
			'*';
		}
	],
	[#Rule 79
		 'AssignmentOperator', 1,
sub
#line 496 "parser.yp"
{
			'/';
		}
	],
	[#Rule 80
		 'AssignmentOperator', 1,
sub
#line 500 "parser.yp"
{
			'%';
		}
	],
	[#Rule 81
		 'AssignmentOperator', 1, undef
	],
	[#Rule 82
		 'AssignmentOperator', 1, undef
	],
	[#Rule 83
		 'AssignmentOperator', 1,
sub
#line 506 "parser.yp"
{
			'<<';
		}
	],
	[#Rule 84
		 'AssignmentOperator', 1,
sub
#line 510 "parser.yp"
{
			'>>';
		}
	],
	[#Rule 85
		 'AssignmentOperator', 1,
sub
#line 514 "parser.yp"
{
			'>>>';
		}
	],
	[#Rule 86
		 'AssignmentOperator', 1,
sub
#line 518 "parser.yp"
{
			'&';
		}
	],
	[#Rule 87
		 'AssignmentOperator', 1,
sub
#line 522 "parser.yp"
{
			'^';
		}
	],
	[#Rule 88
		 'AssignmentOperator', 1,
sub
#line 526 "parser.yp"
{
			'|';
		}
	],
	[#Rule 89
		 'AssignmentOperator', 1,
sub
#line 530 "parser.yp"
{
			'DIV';
		}
	],
	[#Rule 90
		 'Expression', 1, undef
	],
	[#Rule 91
		 'Expression', 3,
sub
#line 538 "parser.yp"
{
			$_[1]->concat(new Pop($_[0]));
			$_[1]->concat($_[3]);
		}
	],
	[#Rule 92
		 'Statement', 1, undef
	],
	[#Rule 93
		 'Statement', 1, undef
	],
	[#Rule 94
		 'Statement', 1, undef
	],
	[#Rule 95
		 'Statement', 1, undef
	],
	[#Rule 96
		 'Statement', 1, undef
	],
	[#Rule 97
		 'Statement', 1, undef
	],
	[#Rule 98
		 'Statement', 1, undef
	],
	[#Rule 99
		 'Statement', 1, undef
	],
	[#Rule 100
		 'Statement', 1, undef
	],
	[#Rule 101
		 'Block', 3,
sub
#line 558 "parser.yp"
{
			$_[2];
		}
	],
	[#Rule 102
		 'Block', 3,
sub
#line 562 "parser.yp"
{
			$_[0]->Error("'\x7d' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 103
		 'Block', 2,
sub
#line 567 "parser.yp"
{
			undef;
		}
	],
	[#Rule 104
		 'StatementList', 1, undef
	],
	[#Rule 105
		 'StatementList', 2,
sub
#line 575 "parser.yp"
{
			if (! defined $_[1]) {
				$_[2];
			} else {
				if (! defined $_[2]) {
					$_[1];
				} else {
					$_[1]->concat($_[2]);
				}
			}
		}
	],
	[#Rule 106
		 'VariableStatement', 3,
sub
#line 590 "parser.yp"
{
			$_[2];
		}
	],
	[#Rule 107
		 'VariableStatement', 2,
sub
#line 594 "parser.yp"
{
			$_[0]->Error("invalid variable declaration.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 108
		 'VariableStatement', 3,
sub
#line 599 "parser.yp"
{
			$_[0]->Error("';' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 109
		 'VariableDeclarationList', 1, undef
	],
	[#Rule 110
		 'VariableDeclarationList', 3,
sub
#line 608 "parser.yp"
{
			if (! defined $_[1]) {
				$_[3];
			} else {
				if (! defined $_[3]) {
					$_[1];
				} else {
					$_[1]->concat($_[3]);
				}
			}
		}
	],
	[#Rule 111
		 'VariableDeclaration', 2,
sub
#line 623 "parser.yp"
{
			my $var = $_[0]->YYData->{symbtab_var}->InsertLocal($_[1]);
			my $store = new StoreVar($_[0],
					'Definition'		=>	$var
			);
			$_[2]->concat($store);
		}
	],
	[#Rule 112
		 'VariableDeclaration', 1,
sub
#line 631 "parser.yp"
{
			$_[0]->YYData->{symbtab_var}->InsertLocal($_[1]);
			undef;
		}
	],
	[#Rule 113
		 'VariableInitializer', 2,
sub
#line 639 "parser.yp"
{
			$_[2];
		}
	],
	[#Rule 114
		 'EmptyStatement', 1,
sub
#line 646 "parser.yp"
{
			undef;
		}
	],
	[#Rule 115
		 'ExpressionStatement', 2,
sub
#line 653 "parser.yp"
{
			$_[1]->concat(new Pop($_[0]));
		}
	],
	[#Rule 116
		 'ExpressionStatement', 2,
sub
#line 657 "parser.yp"
{
			$_[0]->Error("';' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 117
		 'IfStatement', 7,
sub
#line 665 "parser.yp"
{
			BuildIfElse($_[0], $_[3], $_[5], $_[7]);
		}
	],
	[#Rule 118
		 'IfStatement', 5,
sub
#line 669 "parser.yp"
{
			BuildIf($_[0], $_[3], $_[5]);
		}
	],
	[#Rule 119
		 'IfStatement', 2,
sub
#line 673 "parser.yp"
{
			$_[0]->Error("'(' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 120
		 'IfStatement', 3,
sub
#line 678 "parser.yp"
{
			$_[0]->Error("invalid expression.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 121
		 'IfStatement', 4,
sub
#line 683 "parser.yp"
{
			$_[0]->Error("')' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 122
		 'IterationStatement', 1, undef
	],
	[#Rule 123
		 'IterationStatement', 1, undef
	],
	[#Rule 124
		 'WhileStatement', 5,
sub
#line 696 "parser.yp"
{
			BuildFor($_[0], undef, $_[3], undef, $_[5]);
		}
	],
	[#Rule 125
		 'WhileStatement', 2,
sub
#line 700 "parser.yp"
{
			$_[0]->Error("'(' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 126
		 'WhileStatement', 3,
sub
#line 705 "parser.yp"
{
			$_[0]->Error("invalid expression.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 127
		 'WhileStatement', 4,
sub
#line 710 "parser.yp"
{
			$_[0]->Error("')' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 128
		 'for_begin', 4,
sub
#line 718 "parser.yp"
{
			$_[3]->concat(new Pop($_[0]));
		}
	],
	[#Rule 129
		 'for_begin', 3,
sub
#line 722 "parser.yp"
{
			undef;
		}
	],
	[#Rule 130
		 'for_begin', 5,
sub
#line 726 "parser.yp"
{
			$_[4];
		}
	],
	[#Rule 131
		 'for_begin', 2,
sub
#line 730 "parser.yp"
{
			$_[0]->Error("'(' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 132
		 'for_begin', 3,
sub
#line 735 "parser.yp"
{
			$_[0]->Error("invalid init expression.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 133
		 'for_begin', 4,
sub
#line 740 "parser.yp"
{
			$_[0]->Error("';' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 134
		 'for_begin', 4,
sub
#line 745 "parser.yp"
{
			$_[0]->Error("invalid variable declaration.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 135
		 'for_begin', 5,
sub
#line 750 "parser.yp"
{
			$_[0]->Error("';' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 136
		 'ForStatement', 6,
sub
#line 758 "parser.yp"
{
			my $upd = $_[4]->concat(new Pop($_[0]));
			BuildFor($_[0], $_[1], $_[2], $upd, $_[6]);
		}
	],
	[#Rule 137
		 'ForStatement', 5,
sub
#line 763 "parser.yp"
{
			BuildFor($_[0], $_[1], $_[2], undef, $_[5]);
		}
	],
	[#Rule 138
		 'ForStatement', 5,
sub
#line 767 "parser.yp"
{
			my $upd = $_[3]->concat(new Pop($_[0]));
			BuildFor($_[0], $_[1], undef, $upd, $_[5]);
		}
	],
	[#Rule 139
		 'ForStatement', 4,
sub
#line 772 "parser.yp"
{
			BuildFor($_[0], $_[1], undef, undef, $_[4]);
		}
	],
	[#Rule 140
		 'ForStatement', 2,
sub
#line 776 "parser.yp"
{
			$_[0]->Error("invalid control expression.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 141
		 'ForStatement', 3,
sub
#line 781 "parser.yp"
{
			$_[0]->Error("';' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 142
		 'ForStatement', 4,
sub
#line 786 "parser.yp"
{
			$_[0]->Error("invalid update expression.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 143
		 'ForStatement', 5,
sub
#line 791 "parser.yp"
{
			$_[0]->Error("')' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 144
		 'ForStatement', 3,
sub
#line 796 "parser.yp"
{
			$_[0]->Error("invalid update expression.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 145
		 'ForStatement', 4,
sub
#line 801 "parser.yp"
{
			$_[0]->Error("')' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 146
		 'ContinueStatement', 2,
sub
#line 809 "parser.yp"
{
			new Jump($_[0],
					'TypeDef'			=>	'LABEL_CONTINUE'
			);
		}
	],
	[#Rule 147
		 'ContinueStatement', 2,
sub
#line 815 "parser.yp"
{
			$_[0]->Error("';' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 148
		 'BreakStatement', 2,
sub
#line 823 "parser.yp"
{
			new Jump($_[0],
					'TypeDef'			=>	'LABEL_BREAK'
			);
		}
	],
	[#Rule 149
		 'BreakStatement', 2,
sub
#line 829 "parser.yp"
{
			$_[0]->Error("';' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 150
		 'ReturnStatement', 2,
sub
#line 837 "parser.yp"
{
			new ReturnES($_[0]);
		}
	],
	[#Rule 151
		 'ReturnStatement', 3,
sub
#line 841 "parser.yp"
{
			$_[2]->concat(new Return($_[0]));
		}
	],
	[#Rule 152
		 'ReturnStatement', 2,
sub
#line 845 "parser.yp"
{
			$_[0]->Error("Missing term.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 153
		 'ReturnStatement', 3,
sub
#line 850 "parser.yp"
{
			$_[0]->Error("';' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 154
		 'func_decl', 4,
sub
#line 858 "parser.yp"
{
			$_[0]->YYData->{symbtab_func}->InsertLocal($_[3],'PUBLIC_FUNC');
		}
	],
	[#Rule 155
		 'func_decl', 3,
sub
#line 862 "parser.yp"
{
			$_[0]->YYData->{symbtab_func}->InsertLocal($_[2],'PRIVATE_FUNC');
		}
	],
	[#Rule 156
		 'func_decl', 2,
sub
#line 866 "parser.yp"
{
			$_[0]->Error("function excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 157
		 'func_decl', 3,
sub
#line 871 "parser.yp"
{
			$_[0]->Error("invalid function name.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 158
		 'func_decl', 4,
sub
#line 876 "parser.yp"
{
			$_[0]->Error("'(' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 159
		 'func_decl', 2,
sub
#line 881 "parser.yp"
{
			$_[0]->Error("invalid function name.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 160
		 'func_decl', 3,
sub
#line 886 "parser.yp"
{
			$_[0]->Error("'(' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 161
		 'FunctionDeclaration', 5,
sub
#line 894 "parser.yp"
{
			new Function($_[0],
					'Definition'		=>	$_[1],
					'Param'				=>	$_[2],
					'Value'				=>	$_[4]
			);
		}
	],
	[#Rule 162
		 'FunctionDeclaration', 4,
sub
#line 902 "parser.yp"
{
			new Function($_[0],
					'Definition'		=>	$_[1],
					'Param'				=>	$_[2],
					'Value'				=>	$_[4]
			);
		}
	],
	[#Rule 163
		 'FunctionDeclaration', 4,
sub
#line 910 "parser.yp"
{
			new Function($_[0],
					'Definition'		=>	$_[1],
					'Value'				=>	$_[3]
			);
		}
	],
	[#Rule 164
		 'FunctionDeclaration', 3,
sub
#line 917 "parser.yp"
{
			new Function($_[0],
					'Definition'		=>	$_[1],
					'Value'				=>	$_[3]
			);
		}
	],
	[#Rule 165
		 'FunctionDeclaration', 2,
sub
#line 924 "parser.yp"
{
			$_[0]->Error("invalid parameters.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 166
		 'FunctionDeclaration', 3,
sub
#line 929 "parser.yp"
{
			$_[0]->Error("')' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 167
		 'FunctionDeclaration', 4,
sub
#line 934 "parser.yp"
{
			$_[0]->Error("block statement expected.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 168
		 'FunctionDeclaration', 3,
sub
#line 939 "parser.yp"
{
			$_[0]->Error("block statement expected.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 169
		 'FormalParameterList', 1,
sub
#line 947 "parser.yp"
{
			my $var = $_[0]->YYData->{symbtab_var}->InsertArg($_[1], 0);
			new Argument($_[0],
					'Definition'		=>	$var,
					'Index'				=>	1			# nb args
			);
		}
	],
	[#Rule 170
		 'FormalParameterList', 3,
sub
#line 955 "parser.yp"
{
			my $idx = $_[1]->{OpCode}->{Index};
			$_[1]->{OpCode}->{Index} ++;				# nb args
			my $var = $_[0]->YYData->{symbtab_var}->InsertArg($_[3], $idx);
			my $arg = new Argument($_[0],
					'Definition'		=>	$var,
			);
			$_[1]->concat($arg);
		}
	],
	[#Rule 171
		 'CompilationUnit', 2,
sub
#line 968 "parser.yp"
{
			$_[0]->YYData->{PragmaList} = $_[1];
			$_[0]->YYData->{FunctionList} = $_[2];
		}
	],
	[#Rule 172
		 'CompilationUnit', 2,
sub
#line 973 "parser.yp"
{
			$_[0]->YYData->{PragmaList} = $_[1];
			$_[0]->YYData->{FunctionList} = undef;
			$_[0]->Error("function declaration excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 173
		 'CompilationUnit', 1,
sub
#line 980 "parser.yp"
{
			$_[0]->YYData->{PragmaList} = $_[1];
			$_[0]->YYData->{FunctionList} = undef;
			$_[0]->Error("function declaration excepted.\n");
		}
	],
	[#Rule 174
		 'CompilationUnit', 1,
sub
#line 986 "parser.yp"
{
			$_[0]->YYData->{PragmaList} = undef;
			$_[0]->YYData->{FunctionList} = $_[1];
		}
	],
	[#Rule 175
		 'Pragmas', 1, undef
	],
	[#Rule 176
		 'Pragmas', 2,
sub
#line 995 "parser.yp"
{
			$_[1]->concat($_[2]);
		}
	],
	[#Rule 177
		 'Pragma', 3,
sub
#line 1002 "parser.yp"
{
			$_[2];
		}
	],
	[#Rule 178
		 'Pragma', 2,
sub
#line 1006 "parser.yp"
{
			$_[0]->Error("invalid pragma.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 179
		 'Pragma', 3,
sub
#line 1011 "parser.yp"
{
			$_[0]->Error("';' excepted.\n");
			$_[0]->YYErrok();
		}
	],
	[#Rule 180
		 'PragmaDeclaration', 1, undef
	],
	[#Rule 181
		 'PragmaDeclaration', 1,
sub
#line 1020 "parser.yp"
{
			if (exists $_[0]->YYData->{AccessControlPragma}) {
				$_[0]->Error("multiple access control pragma.\n");
				$_[0]->YYData->{AccessControlPragma} ++;
			} else {
				$_[0]->YYData->{AccessControlPragma} = 1;
			}
			$_[1];
		}
	],
	[#Rule 182
		 'PragmaDeclaration', 1, undef
	],
	[#Rule 183
		 'ExternalCompilationUnitPragma', 3,
sub
#line 1034 "parser.yp"
{
			new Url($_[0],
					'Value'				=>	$_[3],
					'Definition'		=>	$_[0]->YYData->{symbtab_url}->Insert($_[2])
			);
		}
	],
	[#Rule 184
		 'AccessControlPragma', 2,
sub
#line 1044 "parser.yp"
{
			$_[2];
		}
	],
	[#Rule 185
		 'AccessControlSpecifier', 2,
sub
#line 1051 "parser.yp"
{
			new AccessDomain($_[0],
					'Value'				=>	$_[2],
			);
		}
	],
	[#Rule 186
		 'AccessControlSpecifier', 2,
sub
#line 1057 "parser.yp"
{
			new AccessPath($_[0],
					'Value'				=>	$_[2]
			);
		}
	],
	[#Rule 187
		 'AccessControlSpecifier', 4,
sub
#line 1063 "parser.yp"
{
			my $domain = new AccessDomain($_[0],
					'Value'				=>	$_[2],
			);
			my $path = new AccessPath($_[0],
					'Value'				=>	$_[4],
			);
			$domain->concat($path);
		}
	],
	[#Rule 188
		 'MetaPragma', 2,
sub
#line 1076 "parser.yp"
{
			$_[2];
		}
	],
	[#Rule 189
		 'MetaSpecifier', 1, undef
	],
	[#Rule 190
		 'MetaSpecifier', 1, undef
	],
	[#Rule 191
		 'MetaSpecifier', 1, undef
	],
	[#Rule 192
		 'MetaName', 2,
sub
#line 1089 "parser.yp"
{
			new MetaName($_[0],
					'Value'				=>	$_[2],
			);
		}
	],
	[#Rule 193
		 'MetaHttpEquiv', 3,
sub
#line 1098 "parser.yp"
{
			new MetaHttpEquiv($_[0],
					'Value'				=>	$_[3],
			);
		}
	],
	[#Rule 194
		 'MetaUserAgent', 3,
sub
#line 1107 "parser.yp"
{
			new MetaUserAgent($_[0],
					'Value'				=>	$_[3],
			);
		}
	],
	[#Rule 195
		 'MetaBody', 3,
sub
#line 1116 "parser.yp"
{
			$_[2]->concat($_[3]);
			$_[1]->concat($_[2]);
		}
	],
	[#Rule 196
		 'MetaBody', 2,
sub
#line 1121 "parser.yp"
{
			$_[1]->concat($_[2]);
		}
	],
	[#Rule 197
		 'MetaPropertyName', 1, undef
	],
	[#Rule 198
		 'MetaContent', 1, undef
	],
	[#Rule 199
		 'MetaScheme', 1, undef
	],
	[#Rule 200
		 'FunctionDeclarations', 1, undef
	],
	[#Rule 201
		 'FunctionDeclarations', 2,
sub
#line 1141 "parser.yp"
{
			$_[1]->concat($_[2]);
		}
	]
],
                                  @_);
    bless($self,$class);
}

#line 1146 "parser.yp"


#	Number of rules         : 202
#	Number of terminals     : 80
#	Number of non-terminals : 66
#	Number of states        : 308

use strict;

use WAP::wmls::lexer;
use WAP::wmls::node;

sub BuildUnop {
	my($parser,$op,$expr) = @_;
	my $unop = new UnaryOp($parser,
			'Operator'					=>	$op
	);
	return $expr->concat($unop);
}

sub BuildBinop {
	my($parser,$expr1,$op,$expr2) = @_;
	my $binop = new BinaryOp($parser,
			'Operator'					=>	$op,
			'Left'						=>	$expr1->{Last}
	);
	$expr1->concat($expr2);
	return $expr1->concat($binop);
}

sub BuildLogop {
	my($parser,$expr1,$logop,$expr2) = @_;
	my $endif = new Label($parser,
			'TypeDef'					=>	'LABEL_ENDIF',
			'Index'						=>	0xffffffff
	);
	my $falsejump = new FalseJump($parser,
			'Value'						=>	$endif
	);
	$expr1->concat($logop);
	$expr1->concat($falsejump);
	$expr1->concat($expr2);
	$expr1->concat(new ToBool($parser));
	return $expr1->concat($endif);
}

sub BuildIf {
	my($parser, $expr, $stat) = @_;
	my $endif = $parser->YYData->{symbtab_label}->Next();
	my $label = new Label($parser,
			'Definition'				=>	$endif
	);
	$endif->{Node} = $label;
	my $falsejump = new FalseJump($parser,
			'Definition'				=>	$endif
	);
	$endif->{NbUse} ++;
	$expr->concat($falsejump);
	$expr->concat($stat) if (defined $stat);
	return $expr->concat($label);
}

sub BuildIfElse(NODE Expr, NODE Stat1, NODE Stat2) {
	my($parser, $expr, $stat1, $stat2) = @_;
	my $else = $parser->YYData->{symbtab_label}->Next();
	my $endif = $parser->YYData->{symbtab_label}->Next();
	my $label1 = new Label($parser,
			'Definition'				=>	$else
	);
	$else->{Node} = $label1;
	my $label2 = new Label($parser,
			'Definition'				=>	$endif
	);
	$endif->{Node} = $label2;
	my $falsejump = new FalseJump($parser,
			'Definition'				=>	$else
	);
	$else->{NbUse} ++;
	my $jump = new Jump($parser,
			'Definition'				=>	$endif
	);
	$endif->{NbUse} ++;
	$expr->concat($falsejump);
	$expr->concat($stat1) if (defined $stat1);
	$expr->concat($jump);
	$expr->concat($label1);
	$expr->concat($stat2) if (defined $stat2);
	return $expr->concat($label2);
}

sub BuildFor {
	my($parser, $init, $cond, $upd, $stat) = @_;
	my $for;
	my $loop = $parser->YYData->{symbtab_label}->Next();
	my $continue = $parser->YYData->{symbtab_label}->Next();
	my $break = $parser->YYData->{symbtab_label}->Next();
	my $label1 = new Label($parser,
			'Definition'				=>	$loop
	);
	$loop->{Node} = $label1;
	my $label2 = new Label($parser,
			'Definition'				=>	$continue
	);
	$continue->{Node} = $label2;
	my $label3 = new Label($parser,
			'Definition'				=>	$break
	);
	$break->{Node} = $label3;
	if (defined $cond) {
		my $falsejump = new FalseJump($parser,
				'Definition'				=>	$break
		);
		$break->{NbUse} ++;
		my $jump = new Jump($parser,
				'Definition'				=>	$loop
		);
		$loop->{NbUse} ++;
		$for = (defined $init) ? $init->concat($label1) : $label1;
		$for->concat($cond);
		$for->concat($falsejump);
		$for->concat($stat) if (defined $stat);
		$for->concat($label2);
		$for->concat($upd) if (defined $upd);
		$for->concat($jump);
		$for->concat($label3);
	} else {
		my $jump = new Jump($parser,
				'Definition'				=>	$loop
		);
		$loop->{NbUse} ++;
		$for = (defined $init) ? $init->concat($label1) : $label1;
		$for->concat($stat) if (defined $stat);
		$for->concat($label2);
		$for->concat($upd) if (defined $upd);
		$for->concat($jump);
		$for->concat($label3);
	}
	for (my $node = $for; defined $node; $node = $node->{Next}) {
		my $opcode = $node->{OpCode};
		if (	    $opcode->isa('Jump')
				and exists $opcode->{TypeDef} ) {
			my $type = $opcode->{TypeDef};
			if      ($type eq 'LABEL_CONTINUE') {
				$node->configure(
						'Definition'		=>	$continue
				);
				$continue->{NbUse} ++;
			} elsif ($type eq 'LABEL_BREAK') {
				$node->configure(
						'Definition'		=>	$break
				);
				$break->{NbUse} ++;
			}
		}
	}
	return $for;
}

sub Run {
	my $parser = shift;

	my $srcname = $parser->YYData->{filename};
	open(YYIN,$srcname)
		or die "can't open $srcname ($!).\n";

	$parser->_InitLexico();
	$parser->YYData->{symbtab_var} = new SymbTabVar($parser);
	$parser->YYData->{symbtab_lib} = new SymbTabLib($parser);
	$parser->YYData->{symbtab_func} = new SymbTabFunc($parser);
	$parser->YYData->{symbtab_url} = new SymbTabUrl($parser);
	$parser->YYData->{symbtab_label} = new SymbTabLabel($parser);
	$parser->_InitStandardLibrary();
	$parser->YYData->{doc} = '';
	$parser->YYData->{lineno} = 1;
	$parser->YYParse(
			yylex	=> \&_Lexer,
			yyerror	=> sub { return; }
	);

	close YYIN;
}



1;
