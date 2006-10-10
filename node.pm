use strict;
use warnings;
use UNIVERSAL;

#
#           WMLScript Language Specification Version 1.1
#

package node;

our $VERSION = '1.10';


sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    bless($self, $class);
    my ($op) = @_;
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
    my ($node2) = @_;
    $node1->{Last}->{Next} = $node2;
    $node2->{Prev} = $node1->{Last};
    $node1->{Last} = $node2->{Last};
    return  $node1;
}

sub insert {
    my $node1 = shift;
    my ($node2) = @_;
    $node2->{Next} = $node1->{Next};
    $node2->{Prev} = $node1;
    if (defined $node1->{Next}) {
        $node1->{Next}->{Prev} = $node2;
    }
    $node1->{Next} = $node2;
    return;
}

sub visit {
    my $self = shift;
    my $visitor = shift;
    for (my $node = $self; defined $node; $node = $node->{Next}) {
        my $opcode = $node->{OpCode};
        my $class = ref $opcode;
        my $func = 'visit' . $class;
        $visitor->$func($opcode, @_);
    }
    return;
}

sub visitActive {
    my $self = shift;
    my $visitor = shift;
    for (my $node = $self; defined $node; $node = $node->{Next}) {
        next if ($node->{Deleted});
        my $opcode = $node->{OpCode};
        my $class = ref $opcode;
        my $func = 'visit' . $class;
        $visitor->$func($opcode, @_);
    }
    return;
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
    my ($key, $value);
    while ( ($key, $value) = each(%attr) ) {
        if (defined $value) {
            $self->{$key} = $value;
        }
    }
    return $self;
}

package Url;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package AccessDomain;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package AccessPath;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package MetaName;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package MetaHttpEquiv;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package MetaUserAgent;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package Function;

use base qw(OpCode);

use Carp;
use constant UINT8_MAX                  =>  255;

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
    }
    else {
        $parser->Warning("function without statement.\n");
    }
    $parser->YYData->{symbtab_var}->Check();
    return new node($self);
}

sub _SetNbArg {
    my $self = shift;
    my ($parser) = @_;
    my $def = $self->{Definition};
    if (defined $self->{Param}) {
        my $nbargs = $self->{Param}->{OpCode}->{Index};
        if ($nbargs >= UINT8_MAX) {
            $parser->Error("too many function parameter.");
        }
        else {
            $def->{NumberOfArguments} = $nbargs;
        }
    }
    else {
        $def->{NumberOfArguments} = 0;
    }
    return;
}

sub _CheckBreakContinue {
    my $self = shift;
    my ($parser, $block) = @_;
    for (my $node = $block; defined $node; $node = $node->{Next}) {
        my $opcode = $node->{OpCode};
        if (        $opcode->isa('Jump')
                and !defined $opcode->{Definition} ) {
            my $type = $opcode->{TypeDef};
            if      ($type eq 'LABEL_CONTINUE') {
                $parser->Error("continue without loop.\n");
            }
            elsif ($type eq 'LABEL_BREAK') {
                $parser->Error("break without loop.\n");
            }
            else {
                croak "INTERNAL_ERROR: _CheckBreakContinue\n";
            }
        }
    }
    return;
}

package Argument;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package LoadVar;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package StoreVar;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package IncrVar;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package DecrVar;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package AddAsg;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package SubAsg;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package Label;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package Pop;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package ToBool;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package ScOr;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package ScAnd;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package Return;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package ReturnES;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package Call;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package CallLib;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package CallUrl;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package Jump;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package FalseJump;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package UnaryOp;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package BinaryOp;

use base qw(OpCode);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new OpCode(@_);
    bless($self, $class);
    return new node($self);
}

package LoadConst;

use base qw(OpCode);

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
    my ($level, $deleted, $label) = @_;

    print '~'
            if ($deleted);
    while ($level--) {
        print "\t";
    }
    print $label;
    return;
}

sub printDefn {
    my ($def) = @_;

    if (defined $def) {
        print " $def->{Symbol}\n";
    }
    else {
        print " null\n";
    }
    return;
}

sub printOp {
    my ($op) = @_;

    print " $op\n";
    return;
}

sub printConst {
    my ($typedef, $value) = @_;

    if    ($typedef eq 'TYPE_INTEGER') {
        print " $value\n";
    }
    elsif ($typedef eq 'TYPE_FLOAT') {
        print " $value\n";
    }
    elsif ($typedef eq 'TYPE_STRING') {
        print " $value\n";
    }
    elsif ($typedef eq 'TYPE_UTF8_STRING') {
        print " $value\n";
    }
    elsif ($typedef eq 'TYPE_BOOLEAN') {
        if ($value) {
            print " true\n";
        }
        else {
            print " false\n";
        }
    }
    elsif ($typedef eq 'TYPE_INVALID') {
        print " INVALID\n";
    }
    else {
        print "type incompatible of CONST\n";
    }
    return;
}

sub visitUrl {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, 0, "URL");
    printDefn($opcode->{Definition});
    $self->{level} ++;
    $opcode->{Value}->visit($self);
    $self->{level} --;
    return;
}

sub visitAccessDomain {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, 0, "ACCESS DOMAIN\n");
    $self->{level} ++;
    $opcode->{Value}->visit($self);
    $self->{level} --;
    return;
}

sub visitAccessPath {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, 0, "ACCESS PATH\n");
    $self->{level} ++;
    $opcode->{Value}->visit($self);
    $self->{level} --;
    return;
}

sub visitMetaName {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, 0, "META NAME\n");
    $self->{level} ++;
    $opcode->{Value}->visit($self);
    $self->{level} --;
    return;
}

sub visitMetaHttpEquiv {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, 0, "META HTTP EQUIV\n");
    $self->{level} ++;
    $opcode->{Value}->visit($self);
    $self->{level} --;
    return;
}

sub visitMetaUserAgent {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, 0, "META USER AGENT\n");
    $self->{level} ++;
    $opcode->{Value}->visit($self);
    $self->{level} --;
    return;
}

sub visitFunction {
    my $self = shift;
    my ($opcode) = @_;
    printf("\n");
    my $def = $opcode->{Definition};
    if    ($def->{Type} eq 'PRIVATE_FUNC') {
        printLabel($self->{level}, 0, "FUNCTION");
    }
    elsif ($def->{Type} eq 'PUBLIC_FUNC') {
        printLabel($self->{level}, 0, "EXTERN FUNCTION");
    }
    else {
        print "Incompatible type of FUNC\n";
    }
    printDefn($def);
    $self->{level} ++;
    $opcode->{Param}->visit($self)
            if (defined $opcode->{Param});
    $opcode->{Value}->visit($self)
            if (defined $opcode->{Value});
    $self->{level} --;
    return;
}

sub visitArgument {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, 0, "ARG");
    printDefn($opcode->{Definition});
    return;
}

sub visitLoadVar {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "LOAD_VAR");
    printDefn($opcode->{Definition});
    return;
}

sub visitStoreVar {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "STORE_VAR");
    printDefn($opcode->{Definition});
    return;
}

sub visitIncrVar {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "INCR_VAR");
    printDefn($opcode->{Definition});
    return;
}

sub visitDecrVar {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "DECR_VAR");
    printDefn($opcode->{Definition});
    return;
}

sub visitAddAsg {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "ADD_ASG");
    printDefn($opcode->{Definition});
    return;
}

sub visitSubAsg {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "SUB_ASG");
    printDefn($opcode->{Definition});
    return;
}

sub visitLabel {
    my $self = shift;
    my ($opcode) = @_;
    printLabel(0, $opcode->{Deleted}, "LABEL\t");
    printDefn($opcode->{Definition});
    return;
}

sub visitPop {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "POP\n");
    return;
}

sub visitToBool {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "TOBOOL\n");
    return;
}

sub visitScOr {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "SCOR\n");
    return;
}

sub visitScAnd {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "SCAND\n");
    return;
}

sub visitReturn {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "RETURN\n");
    return;
}

sub visitReturnES {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "RETURN_ES\n");
    return;
}

sub visitCall {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "CALL");
    printDefn($opcode->{Definition});
    return;
}

sub visitCallLib {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "CALL_LIB");
    printDefn($opcode->{Definition});
    return;
}

sub visitCallUrl {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "CALL_URL");
    printDefn($opcode->{Definition});
    return;
}

sub visitJump {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "JUMP\t\t");
    printDefn($opcode->{Definition});
    return;
}

sub visitFalseJump {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "FALSE_JUMP\t");
    printDefn($opcode->{Definition});
    return;
}

sub visitUnaryOp {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "UNOP");
    printOp($opcode->{Operator});
    return;
}

sub visitBinaryOp {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "BINOP");
    printOp($opcode->{Operator});
    return;
}

sub visitLoadConst {
    my $self = shift;
    my ($opcode) = @_;
    printLabel($self->{level}, $opcode->{Deleted}, "LOAD_CONST");
    printConst($opcode->{TypeDef}, $opcode->{Value});
    return;
}

###############################################################################

package defn;

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    bless($self, $class);
    my ($symb, $type) = @_;
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
    my ($parser) = @_;
    my $self = {};
    bless($self, $class);
    $self->{parser} = $parser;
    $self->{tab} = {};
    return $self;
}

sub Insert {
    my $self = shift;
    my ($symb, $def) = @_;
    if (exists $self->{tab}{$symb}) {
        $self->{parser}->Error("Redefinition - $symb.\n");
    }
    else {
        $self->{tab}{$symb} = $def;
    }
    return;
}

###############################################################################

package SymbTabVar;

use base qw(SymbTab);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new SymbTab(@_);
    bless($self, $class);
    return $self;
}

sub InsertLocal {
    my $self = shift;
    my ($symb) = @_;
    my $def = new defn($symb);
    $def->{NbUse} ++;
    $self->SUPER::Insert($symb, $def);
    return $def;
}

sub InsertArg {
    my $self = shift;
    my ($symb, $num) = @_;
    my $def = new defn($symb);
    $def->{ID} = $num;
    $self->SUPER::Insert($symb, $def);
    return $def;
}

sub Lookup {
    my $self = shift;
    my ($symb) = @_;
    if (exists $self->{tab}{$symb}) {
        my $def = $self->{tab}{$symb};
        $def->{NbUse} ++;
        return $def;
    }
    else {
        $self->{parser}->Error("Variable undefined - $symb.\n");
        return;
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
    return;
}

###############################################################################

package SymbTabLib;

use base qw(SymbTab);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new SymbTab(@_);
    bless($self, $class);
    return $self;
}

sub Lookup {
    my $self = shift;
    my ($library) = @_;
    unless (exists $self->{tab}{$library}) {
        $self->{parser}->Error("Library unknown - $library.\n");
        return;
    }
    return 1;
}

###############################################################################

package SymbTabFunc;

use base qw(SymbTab);

use constant UINT8_MAX                  =>  255;

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
    my ($symb, $type) = @_;
    if (        $type eq 'PUBLIC_FUNC'
            and length $symb > UINT8_MAX ) {
        $self->{parser}->Error("Too long public function name - $symb.\n");
    }
    my $def = $self->{tab}{$symb};
    if (defined $def) {
            if ($def->{Type} ne 'UNDEF_FUNC') {
                $self->{parser}->Error("Redefinition - $symb.\n");
            }
            else {
                $def->{Type} = $type;
            }
    }
    else {
        $def = new defn($symb, $type);
        $self->SUPER::Insert($symb, $def);
    }
    $def->{ID} = $self->{FunctionID} ++;
    return $def;
}

sub LookupLocal {
    my $self = shift;
    my ($symb) = @_;
    my $def = $self->{tab}{$symb};
    unless (defined $def) {
        $def = new defn($symb, 'UNDEF_FUNC');
        $self->SUPER::Insert($symb, $def);
    }
    return $def;
}

sub LookupExternal {
    my $self = shift;
    my ($script, $func, $nbargs) = @_;
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
    }
    else {
        $def = new defn($symb, 'EXTERN_FUNC');
        $def->{FunctionName} = $func;
        $def->{NumberOfArguments} = $nbargs;
        $self->SUPER::Insert($symb, $def);
    }
    return $def;
}

sub LookupLibrary {
    my $self = shift;
    my ($library, $func, $nbargs) = @_;
    my $symb = $library . '.' . $func;
    my $def = $self->{tab}{$symb};
    if (defined $def) {
        if ($def->{NumberOfArguments} != $nbargs) {
            $self->{parser}->Error("Wrong argument number for standard function - $func.\n");
        }
        return $def;
    }
    else {
        $self->{parser}->Error("Library function unknown - $func.\n");
        return;
    }
}

sub InsertLibrary {
    my $self = shift;
    my ($symb, $libId, $id, $nbargs) = @_;
    my $def = new defn($symb, 'STANDARD_FUNC');
    $def->{LibraryID} = $libId;
    $def->{ID} = $id;
    $def->{NumberOfArguments} = $nbargs;
    $self->SUPER::Insert($symb, $def);
    return $def;
}

###############################################################################

package SymbTabUrl;

use base qw(SymbTab);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = new SymbTab(@_);
    bless($self, $class);
    return $self;
}

sub Insert {
    my $self = shift;
    my ($symb) = @_;
    my $def = new defn($symb);
    $self->SUPER::Insert($symb, $def);
    return $def;
}

sub Lookup {
    my $self = shift;
    my ($script) = @_;
    if (exists $self->{tab}{$script}) {
        my $def = $self->{tab}{$script};
        $def->{NbUse} ++;
        return $def;
    }
    else {
        $self->{parser}->Error("ScriptName undefined - $script.\n");
        return;
    }
}

###############################################################################

package SymbTabLabel;

use constant UINT32_MAX                 =>  4294967295;

use base qw(SymbTab);

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
    $self->SUPER::Insert($symb, $def);
    return $def;
}

1;

