use strict;
use UNIVERSAL;

#
#			WMLScript Language Specification Version 1.1
#

package node;
use vars qw($VERSION);
$VERSION = '1.01';


sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = {};
	bless($self, $class);
	my($op) = @_;
	$self->{OpCode} = $op;
	$self->{Next} = undef;
	$self->{Prev} = undef;
	$self->{Last} = $self;
	$self->{Deleted} = 0;
	return $self;
}

sub del {
	my $self = shift;
	$self->{Deleted} = 1;
	$self->{OpCode}->{Deleted} = 1;
	return $self;
}

sub configure {
	my $self = shift;
	$self->{OpCode}->configure(@_);
	return $self;
}

sub concat {
	my $node1 = shift;
	my($node2) = @_;
	$node1->{Last}->{Next} = $node2;
	$node2->{Prev} = $node1->{Last};
	$node1->{Last} = $node2->{Last};
	return	$node1;
}

sub insert {
	my $node1 = shift;
	my($node2) = @_;
	$node2->{Next} = $node1->{Next};
	$node2->{Prev} = $node1;
	if (defined $node1->{Next}) {
		$node1->{Next}->{Prev} = $node2;
	}
	$node1->{Next} = $node2;
}

sub visit {
	my $self = shift;
	my $visitor = shift;
	for (my $node = $self; defined $node; $node = $node->{Next}) {
		my $opcode = $node->{OpCode};
		my $class = ref $opcode;
		my $func = 'visit' . $class;
		$visitor->$func($opcode,@_);
	}
}

sub visitActive {
	my $self = shift;
	my $visitor = shift;
	for (my $node = $self; defined $node; $node = $node->{Next}) {
		next if ($node->{Deleted});
		my $opcode = $node->{OpCode};
		my $class = ref $opcode;
		my $func = 'visit' . $class;
		$visitor->$func($opcode,@_);
	}
}

sub getFirstActive {
	my $self = shift;
	my $node;
	for ( $node = $self;
		  defined($node) and $node->{Deleted};
		  $node = $node->{Next} ) {}
	return $node;
}

sub getLastActive {
	my $self = shift;
	my $node;
	for ( $node = $self->{Last};
		  defined($node->{Next});
		  $node = $node->{Next} ) {}
	for ( ;
		  defined($node) and $node->{Deleted};
		  $node = $node->{Prev} ) {}
	return $node;
}

sub getNextActive {
	my $self = shift;
	my $node;
	for ( $node = $self->{Next};
		  defined($node) and $node->{Deleted};
		  $node = $node->{Next} ) {}
	return $node;
}

sub getPrevActive {
	my $self = shift;
	my $node;
	for ( $node = $self->{Prev};
		  defined $node and $node->{Deleted};
		  $node = $node->{Prev} ) {}
	return $node;
}

###############################################################################

package OpCode;

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $parser = shift;
	my %attr = @_;
	my $self = \%attr;
	foreach (keys %attr) {
		unless (defined $self->{$_}) {
			delete $self->{$_};
		}
	}
	$self->{Lineno} = $parser->YYData->{lineno};
	return $self;
}

sub configure {
	my $self = shift;
	my %attr = @_;
	my ($key,$value);
	while ( ($key,$value) = each(%attr) ) {
		if (defined $value) {
			$self->{$key} = $value;
		}
	}
	return $self;
}

package Url;

@Url::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package AccessDomain;

@AccessDomain::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package AccessPath;

@AccessPath::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package MetaName;

@MetaName::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package MetaHttpEquiv;

@MetaHttpEquiv::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package MetaUserAgent;

@MetaUserAgent::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package Function;

@Function::ISA = qw(OpCode);

use Carp;
use constant UINT8_MAX					=>	255;

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $parser = shift;
	my $self = new OpCode($parser, @_);
	bless($self, $class);
	# specific
	$self->_SetNbArg($parser);
	if (defined $self->{Value}) {
		$self->_CheckBreakContinue($parser, $self->{Value});
	} else {
		$parser->Warning("function without statement.\n");
	}
	$parser->YYData->{symbtab_var}->Check();
	return new node($self);
}

sub _SetNbArg {
	my $self = shift;
	my($parser) = @_;
	my $def = $self->{Definition};
	if (defined $self->{Param}) {
		my $nbargs = $self->{Param}->{OpCode}->{Index};
		if ($nbargs >= UINT8_MAX) {
			$parser->Error("too many function parameter.");
		} else {
			$def->{NumberOfArguments} = $nbargs;
		}
	} else {
		$def->{NumberOfArguments} = 0;
	}
}

sub _CheckBreakContinue {
	my $self = shift;
	my($parser,$block) = @_;
	for (my $node = $block; defined $node; $node = $node->{Next}) {
		my $opcode = $node->{OpCode};
		if (	    $opcode->isa('Jump')
				and !defined $opcode->{Definition} ) {
			my $type = $opcode->{TypeDef};
			if      ($type eq 'LABEL_CONTINUE') {
				$parser->Error("continue without loop.\n");
			} elsif ($type eq 'LABEL_BREAK') {
				$parser->Error("break without loop.\n");
			} else {
				croak "INTERNAL_ERROR: _CheckBreakContinue\n";
			}
		}
	}
}

package Argument;

@Argument::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package LoadVar;

@LoadVar::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package StoreVar;

@StoreVar::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package IncrVar;

@IncrVar::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package DecrVar;

@DecrVar::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package AddAsg;

@AddAsg::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package SubAsg;

@SubAsg::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package Label;

@Label::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package Pop;

@Pop::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package ToBool;

@ToBool::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package ScOr;

@ScOr::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package ScAnd;

@ScAnd::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package Return;

@Return::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package ReturnES;

@ReturnES::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package Call;

@Call::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package CallLib;

@CallLib::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package CallUrl;

@CallUrl::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package Jump;

@Jump::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package FalseJump;

@FalseJump::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package UnaryOp;

@UnaryOp::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package BinaryOp;

@BinaryOp::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

package LoadConst;

@LoadConst::ISA = qw(OpCode);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new OpCode(@_);
	bless($self, $class);
	return new node($self);
}

###############################################################################

package printVisitor;

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = {};
	bless($self, $class);
	$self->{level} = 0;
	return $self;
}

sub printLabel {
	my($level,$deleted,$label) = @_;

	print '~'
			if ($deleted);
	while ($level--) {
		print "\t";
	}
	print $label;
}

sub printDefn {
	my($def) = @_;

	if (defined $def) {
		print " $def->{Symbol}\n";
	} else {
		print " null\n";
	}
}

sub printOp {
	my($op) = @_;

	print " $op\n";
}

sub printConst {
	my($typedef,$value) = @_;

	if      ($typedef eq 'TYPE_INTEGER') {
		print " $value\n";
	} elsif ($typedef eq 'TYPE_FLOAT') {
		print " $value\n";
	} elsif ($typedef eq 'TYPE_STRING') {
		print " $value\n";
	} elsif ($typedef eq 'TYPE_UTF8_STRING') {
		print " $value\n";
	} elsif ($typedef eq 'TYPE_BOOLEAN') {
		if ($value) {
			print " true\n";
		} else {
			print " false\n";
		}
	} elsif ($typedef eq 'TYPE_INVALID') {
		print " INVALID\n";
	} else {
		print "type incompatible of CONST\n";
	}
}

sub visitUrl {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},0,"URL");
	printDefn($opcode->{Definition});
	$self->{level} ++;
	$opcode->{Value}->visit($self);
	$self->{level} --;
}

sub visitAccessDomain {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},0,"ACCESS DOMAIN\n");
	$self->{level} ++;
	$opcode->{Value}->visit($self);
	$self->{level} --;
}

sub visitAccessPath {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},0,"ACCESS PATH\n");
	$self->{level} ++;
	$opcode->{Value}->visit($self);
	$self->{level} --;
}

sub visitMetaName {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},0,"META NAME\n");
	$self->{level} ++;
	$opcode->{Value}->visit($self);
	$self->{level} --;
}

sub visitMetaHttpEquiv {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},0,"META HTTP EQUIV\n");
	$self->{level} ++;
	$opcode->{Value}->visit($self);
	$self->{level} --;
}

sub visitMetaUserAgent {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},0,"META USER AGENT\n");
	$self->{level} ++;
	$opcode->{Value}->visit($self);
	$self->{level} --;
}

sub visitFunction {
	my $self = shift;
	my($opcode) = @_;
	printf("\n");
	my $def = $opcode->{Definition};
	if      ($def->{Type} eq 'PRIVATE_FUNC') {
		printLabel($self->{level},0,"FUNCTION");
	} elsif ($def->{Type} eq 'PUBLIC_FUNC') {
		printLabel($self->{level},0,"EXTERN FUNCTION");
	} else {
		print "Incompatible type of FUNC\n";
	}
	printDefn($def);
	$self->{level} ++;
	$opcode->{Param}->visit($self)
			if (defined $opcode->{Param});
	$opcode->{Value}->visit($self)
			if (defined $opcode->{Value});
	$self->{level} --;
}

sub visitArgument {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},0,"ARG");
	printDefn($opcode->{Definition});
}

sub visitLoadVar {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"LOAD_VAR");
	printDefn($opcode->{Definition});
}

sub visitStoreVar {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"STORE_VAR");
	printDefn($opcode->{Definition});
}

sub visitIncrVar {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"INCR_VAR");
	printDefn($opcode->{Definition});
}

sub visitDecrVar {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"DECR_VAR");
	printDefn($opcode->{Definition});
}

sub visitAddAsg {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"ADD_ASG");
	printDefn($opcode->{Definition});
}

sub visitSubAsg {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"SUB_ASG");
	printDefn($opcode->{Definition});
}

sub visitLabel {
	my $self = shift;
	my($opcode) = @_;
	printLabel(0,$opcode->{Deleted},"LABEL\t");
	printDefn($opcode->{Definition});
}

sub visitPop {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"POP\n");
}

sub visitToBool {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"TOBOOL\n");
}

sub visitScOr {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"SCOR\n");
}

sub visitScAnd {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"SCAND\n");
}

sub visitReturn {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"RETURN\n");
}

sub visitReturnES {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"RETURN_ES\n");
}

sub visitCall {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"CALL");
	printDefn($opcode->{Definition});
}

sub visitCallLib {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"CALL_LIB");
	printDefn($opcode->{Definition});
}

sub visitCallUrl {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"CALL_URL");
	printDefn($opcode->{Definition});
}

sub visitJump {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"JUMP\t\t");
	printDefn($opcode->{Definition});
}

sub visitFalseJump {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"FALSE_JUMP\t");
	printDefn($opcode->{Definition});
}

sub visitUnaryOp {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"UNOP");
	printOp($opcode->{Operator});
}

sub visitBinaryOp {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"BINOP");
	printOp($opcode->{Operator});
}

sub visitLoadConst {
	my $self = shift;
	my($opcode) = @_;
	printLabel($self->{level},$opcode->{Deleted},"LOAD_CONST");
	printConst($opcode->{TypeDef},$opcode->{Value});
}

###############################################################################

package defn;

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = {};
	bless($self, $class);
	my($symb, $type) = @_;
	$self->{Symbol} = $symb;
	$self->{Type} = $type if (defined $type);
	$self->{ID} = 0xffff;
	$self->{NbUse} = 0;
	return $self;
}

###############################################################################

package SymbTab;

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my($parser) = @_;
	my $self = {};
	bless($self, $class);
	$self->{parser} = $parser;
	$self->{tab} = {};
	return $self;
}

sub Insert {
	my $self = shift;
	my($symb,$def) = @_;
	if (exists $self->{tab}{$symb}) {
		$self->{parser}->Error("Redefinition - $symb.\n");
	} else {
		$self->{tab}{$symb} = $def;
	}
}

###############################################################################

package SymbTabVar;

@SymbTabVar::ISA = qw(SymbTab);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new SymbTab(@_);
	bless($self, $class);
	return $self;
}

sub InsertLocal {
	my $self = shift;
	my($symb) = @_;
	my $def = new defn($symb);
	$def->{NbUse} ++;
	$self->SUPER::Insert($symb,$def);
	return $def;
}

sub InsertArg {
	my $self = shift;
	my($symb,$num) = @_;
	my $def = new defn($symb);
	$def->{ID} = $num;
	$self->SUPER::Insert($symb,$def);
	return $def;
}

sub Lookup {
	my $self = shift;
	my($symb) = @_;
	if (exists $self->{tab}{$symb}) {
		my $def = $self->{tab}{$symb};
		$def->{NbUse} ++;
		return $def;
	} else {
		$self->{parser}->Error("Variable undefined - $symb.\n");
		return undef;
	}
}

sub Check {
	my $self = shift;
	foreach (keys %{$self->{tab}}) {
		my $def = $self->{tab}{$_};
		unless ($def->{NbUse}) {
			$self->{parser}->Warning("Unused variable - $_.\n");
		}
	}
	$self->{tab} = {};
}

###############################################################################

package SymbTabLib;

@SymbTabLib::ISA = qw(SymbTab);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new SymbTab(@_);
	bless($self, $class);
	return $self;
}

sub Lookup {
	my $self = shift;
	my($library) = @_;
	unless (exists $self->{tab}{$library}) {
		$self->{parser}->Error("Library unknown - $library.\n");
		return undef;
	}
	return 1;
}

###############################################################################

package SymbTabFunc;

@SymbTabFunc::ISA = qw(SymbTab);

use constant UINT8_MAX					=>	255;

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new SymbTab(@_);
	bless($self, $class);
	$self->{FunctionID} = 0;
	return $self;
}

sub InsertLocal {
	my $self = shift;
	my($symb, $type) = @_;
	if (	    $type eq 'PUBLIC_FUNC'
			and length $symb > UINT8_MAX ) {
		$self->{parser}->Error("Too long public function name - $symb.\n");
	}
	my $def = $self->{tab}{$symb};
	if (defined $def) {
			if ($def->{Type} ne 'UNDEF_FUNC') {
				$self->{parser}->Error("Redefinition - $symb.\n");
			} else {
				$def->{Type} = $type;
			}
	} else {
		$def = new defn($symb, $type);
		$self->SUPER::Insert($symb,$def);
	}
	$def->{ID} = $self->{FunctionID} ++;
	return $def;
}

sub LookupLocal {
	my $self = shift;
	my($symb) = @_;
	my $def = $self->{tab}{$symb};
	unless (defined $def) {
		$def = new defn($symb, 'UNDEF_FUNC');
		$self->SUPER::Insert($symb,$def);
	}
	return $def;
}

sub LookupExternal {
	my $self = shift;
	my($script,$func,$nbargs) = @_;
	if (length $func > UINT8_MAX) {
		$self->{parser}->Error("Too long external function name - $func.\n");
	}
	if ($nbargs > UINT8_MAX) {
		$self->{parser}->Error("External function with too many parameter - $func.\n");
	}
	my $symb = $script . '#' . $func;
	my $def = $self->{tab}{$symb};
	if (defined $def) {
		if ($nbargs != $def->{NumberOfArguments}) {
			$self->{parser}->Error("Previous call with different argument number - $func.\n");
		}
	} else {
		$def = new defn($symb, 'EXTERN_FUNC');
		$def->{FunctionName} = $func;
		$def->{NumberOfArguments} = $nbargs;
		$self->SUPER::Insert($symb,$def);
	}
	return $def;
}

sub LookupLibrary {
	my $self = shift;
	my($library, $func, $nbargs) = @_;
	my $symb = $library . '.' . $func;
	my $def = $self->{tab}{$symb};
	if (defined $def) {
		if ($def->{NumberOfArguments} != $nbargs) {
			$self->{parser}->Error("Wrong argument number for standard function - $func.\n");
		}
		return $def;
	} else {
		$self->{parser}->Error("Library function unknown - $func.\n");
		return undef;
	}
}

sub InsertLibrary {
	my $self = shift;
	my($symb,$libId,$id,$nbargs) = @_;
	my $def = new defn($symb, 'STANDARD_FUNC');
	$def->{LibraryID} = $libId;
	$def->{ID} = $id;
	$def->{NumberOfArguments} = $nbargs;
	$self->SUPER::Insert($symb,$def);
	return $def;
}

###############################################################################

package SymbTabUrl;

@SymbTabUrl::ISA = qw(SymbTab);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new SymbTab(@_);
	bless($self, $class);
	return $self;
}

sub Insert {
	my $self = shift;
	my($symb) = @_;
	my $def = new defn($symb);
	$self->SUPER::Insert($symb,$def);
	return $def;
}

sub Lookup {
	my $self = shift;
	my($script) = @_;
	if (exists $self->{tab}{$script}) {
		my $def = $self->{tab}{$script};
		$def->{NbUse} ++;
		return $def;
	} else {
		$self->{parser}->Error("ScriptName undefined - $script.\n");
		return undef;
	}
}

###############################################################################

package SymbTabLabel;

use constant UINT32_MAX 				=>	4294967295;

@SymbTabLabel::ISA = qw(SymbTab);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = new SymbTab(@_);
	$self->{idx} = 0;
	bless($self, $class);
	return $self;
}

sub Next {
	my $self = shift;
	my $symb = sprintf("L%d", $self->{idx}++);
	my $def = new defn($symb);
	$def->{Index} = UINT32_MAX;
	$self->SUPER::Insert($symb,$def);
	return $def;
}

1;

