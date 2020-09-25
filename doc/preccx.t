(see Software--Practice & Experience, 25(11), Nov. 1995, pp 1263-1297, for
the full version: P.T. Breuer and J.P. Bowen, "A PREttier Compiler-Compiler:
Generating Higher Order Parsers in C")

A  PREttier  Compiler Compiler:
Higher Order Programming in C

P.T. Breuer
Oxford University Computing Laboratory,
11 Keble Road, Oxford OX1 3QD,
UK.

March 1992

ABSTRACT

PRE-CC is a compiler compiler that produces simple and modular ANSI
compliant C code, portable to any platform supporting the standard.
Parsers and compilers built with  PRE-CC have unlimited lookahead and
backtracking capabilities which mean that almost all language
compilers may be designed as one-pass utilities.  Technically,  PRE-CC
is a complete interpreter for higher-order inherited (that is, context
dependent) and synthetic attribute grammars.

In terms of programming languages,  PRE-CC proves that C can be a
declarative higher order language. The C run-time environment supports
the passing of functions as parameters, and function building can be
effected by printing out code, then running the C compiler over the
output. What is remarkable is that a single build pass by  PRE-CC is
all that is needed to convert higher-order attribute grammar
specifications into runnable C code.


DISCUSSION

The design of a programming or specification language is inevitably
followed by the building of a parser or compiler for it, and the
generic Unix utility YACC [YACC] is often the tool of choice for such
a project.  This is not only because it genuinely does permit the
implementation (and therefore later maintenance and version control)
to be carried out at a higher level, but because it has traditionally
been seen as conferring some degree of respectability upon the final
product.

On the face of it,  YACC offers attractive programming options too: it
implements a variety of the BNF grammar specification language, and
compiles definition scripts into a C program which implements a finite
state automaton, thus converting specification into implementation via
well-understood theory.

In practice, however, the utilities drawbacks may be more obvious than
its attractions.  The supported BNF is an extremely impoverished
variety, for example.  Every BNF construct has to be expanded out by
hand into the basic `sequential' and `alternation' components before
it can be incorporated into a  YACC script, which obscures the
specification and makes maintenance difficult.


# include "ccx.h"
# define INIT V(1)=0;
# define STEP V(1)=V(0)+1;
# define TOTL printf("%d terms OK\n\
next terms are %d,%d,..\n");

MAIN(fibs)[]
@fibs()   = { :INIT: fib(1,1) !$ }* fibs

@fib(a,b) = number(a) <','> :STEP: fib(b,a+b)
@         |     <'.'> <'.'> :TOTL:

@number(n)= digit(n)   
@         | digit(HEAD(n)) number(TAIL(n))

@digit(n) =  n+'0'

Figure 1(a): A parser which accepts only the Fibonacci sequence as
input and pinpoints errors to within a character (HEAD and  TAIL are
C macros).




 1,1,2,3,5,..
 5 terms OK
 Next terms are 8,13,..[]
 1,1,2,3,5,8,13,21,34,51,85,..
 (line 2 error) failed parse:
 probable error at 1,85,..

Figure 1(b): Two input lines and responses.


More importantly, the constructed automaton does not implement
`correctly' even the small fragment of BNF that  YACC allows, in the
sense that only those definition scripts can be implemented that do
not implicitly  require the automaton to look ahead more than one
token.  YACC reports all violations of this restriction as
`shift/reduce' clashes (or `shift/shift', etc.), a reference to the
underlying automaton which can be extremely difficult to relate to
what might be wrong with the script itself. This can be frustrating
for engineers hoping to use the compiler-compiler for rapid
prototyping, because time has to be spent debugging the automaton --
that is,  YACC's interpretation of the script -- as well as debugging
the script itself.

More problems are encountered when changes to a design are required,
whether for debugging purposes or as genuine developments.  First of
all,  YACC scripts have to be a single unit, and a change to any part
requires the whole script to be recompiled again. This can be a slow
process, because  YACC itself is not fast, and the monolithic output
has to be sent to the C compiler for conversion to native machine
code, which is also a slow process (the compiled C code is always a
very large file, and the C compiler is not well suited to the
compilation of that kind of code). There is, therefore, a long
turn-around time built into the basic development cycle under YACC.
And, secondly, the resulting executable is itself very large, because
of the nature of the automaton, and often causes swapping problems (or
worse) in the runtime environment which result in a poor perceived
performance for the application as a whole.

Recently, developments in declarative languages such as  Prolog
[Prolog] and  ML [ML] have led to the incorporation of parser (that
is, compiler) suites into the associated development environment, and
these allow more expressive use of BNF-style descriptions. When these
languages are compilable, the results from using their built-in
facilities as compiler-compilers are very acceptable in comparison
with  YACC, and much more acceptable from the point of view of the
convenience and succinctness of the definitional language itself.

However, these languages always compile into native machine code, with
a consequent lack of portability between different architectures, and
usually carry the overhead of at least some part of the development
environment with them into the final product. Therefore a niche
remains to be filled by a dedicated stand-alone and portable
compiler-compiler which fixes some of  YACC's infelicities and
introduces no more of its own.

The  PRE-CC design sprang from the belief that the restrictions imposed
by a  YACC-style automaton might be beaten by transplanting a well
known method of writing parsers and compilers in higher order
functional programming languages. The method turns out to transfer
quite naturally to C, where the resulting C code follows the form of
the intended grammar closely, and thus provides a natural modularity.
This (forseeable) modularity was intended to compensate for whatever
deficiencies in execution speed might occur in practice. After all,
most of the effort in writing languages does go into maintenance of
one kind or another.

Surprisingly,  PRE-CC-generated executables turn out to run at least as
fast as those generated by  YACC, and without the overhead of a large
sized object module; something which can, with hindsight, also be
attributed to the higher-order and modular nature of PRE-CC output
code.  Furthermore, the higher order model itself turns out to offer a
way out of the debugging nightmare of  YACC's `shift/reduce' errors:
all BNF constructs are implementable, so there can be no
implementation conflicts to report.  PRE-CC carries over from the ideal
world of functional programming the recombinant properties which
guarantee that all valid BNF definitions, no matter how complex, will
be modelled correctly.

The initial development effort furnished a definition of  PRE-CC in its
own language, which then made further development easy, after the
initial bootstrap. Improvements in the definition alone were found to
allow an order of magnitude improvement in  PRE-CC's own execution
speed (this does not affect the speed of parsers and compilers built
by  PRE-CC, but it illustrates the advantages offered by a clear and
succinct definitional language) and  PRE-CC is now known to be of
linear complexity with respect to input definition scripts. It runs at
thousands of lines of input per second on standard platforms such as
Sun 3/4 and HP9000 workstations, and the output code will compile
under any ANSI-compliant C compiler. Moreover, the output is indeed
completely modular: one can cut up definition scripts any which way,
pass them through  PRE-CC separately, run the C compiler on them
separately, and then link together the object modules.

In this respect, PRE-CC fulfils its design goals; it reduces the
turn-around time in language development, both because it requires
only local recompilations when changes are made, and also through the
expressive power of its definitional language. It has been used to
make full scale parsers and scanners for COBOL 74 [Cobol], and to
implement the programming language Uniform [Uniform]. The definition
scripts for these languages comprise between one and two thousand
lines, involving between one and two hundred parser definitions. The
number of keywords alone in COBOL is over one hundred.

Further,  PRE-CC turns out to provide truly higher order programming
facilities in the C runtime environment. This is a byproduct of C's
non-enforcement of typing restrictions, which means that functions
(such as other parsers) can be freely passed along with other
parameters to the parsers which PRE-CC builds, since they are passed as
integer-sized addresses.  Examples are given later in this article,
but the result is that  PRE-CC quite transparently  supports a higher
order `macro' language, and frequently used grammar constructions can
be defined as  PRE-CC `macros' in order to improve the clarity and
maintainability of a script.

Other features which have been incorporated into  PRE-CC include the
permitted embedding of C code and C preprocesser macros or
instructions anywhere in a  PRE-CC script, with the resulting mix of
advantages from PRE-CC's own referentially transparent `macros' and the
referentially opaque C ones. In general,  PRE-CC scripts are all
literate programs, in the sense of Knuth [Knuth], in that only certain
parts of the script are visible to the utility itself, and these can
therefore be embedded in ordinary text (i.e. literate prose) or other
computer language instructions.  PRE-CC also provides a simple hook for
tokenising pre-filters such as the Unix  lex [YACC] utility.

What disadvantages  PRE-CC does have include (1) its use of the C
runtime stack -- which can  grow large in deeply structured and
recursive grammar descriptions, and which is a limitation (of C) under
some memory models on restricted architectures, such as an 8086-based
PC (stack growth can be restricted by the use of `cuts' in the
definition script, however) -- and (2) an as yet inadequate facility
for defining local (volatile) data structures for use in the actions
attached to  PRE-CC scripts, an inheritance from its compatibility with
YACC.


DESCRIPTION

The following is a quick summary of the features by which PRE-CC
extends the well known Unix  YACC utility:

* Contextual definitions. Each grammar definition may be parameterized
with  contexts[^1].  These are variables of any valid C type.

[^1] For example, a language may determine whether a declaration is
local (and to what) or global in scope by its relative indentation
alone, and this kind of constraint can be expressed as follows in the
definition language:

@decl(n) =  space(n) expression <'\n'>
@           decl(n+1)*

*  Unlimited lookahead and backtracking in place of the  YACC 1-token
lookahead[^2].

[^2] PRE-CC parsers will distinguish correctly between sentences of the
form ` foo bah gum' and `foo bah NAY' on a single pass. Lookahead
problems can be severe on other compiler-compilers. For example, the
Fortran constructions `IF (...) ...' and `IF (...) THEN ... ELSE ...
ENDIF' are distinguishable only by means of late trailing tokens and
make Fortran unparsable in a single pass for native  YACC-generated
compilers.

* Arbitrarily complex and nested BNF expressions within definition
scripts.

* The full range of BNF operators and symbols.

* Modular output.  Parts of a script can be  PRE-CC'ed separately,
compiled separately, and then linked together later, which makes
maintenance and version control easy.

* Speed.  PRE-CC is fast and builds fast parsers, typically taking a
couple  of seconds to process scripts of many hundred lines.

* Higher order `macros' for commonly used constructions may be defined
in a script[^3].

[^3] For example, one may define the n-spaces construct,

@ space(n) = mult(n,blank)

where  mult is the higher order constructor:

@ mult(n,p) = )n>0( p mult(n-1,p) | )n==0(


The following features of  YACC have been preserved (or enhanced):

* A standard interface to the Unix  lex utility[^4].

[^4] Lexers, as for YACC, must (a) output the next TOKEN when called,
stashing a semantic value in STACKVALUE yylloc, if desired, (b) return
the value 0 to indicate EOF or any condition which should not be matched
as a TOKEN (PRE-CC contains specific intercepts for this condition in the
`!' and `$' and `$!' constructs). They must also (c) set yytchar to
the last character read, and to EOF at EOF; (d) set yylen to the length of
the string read for the token; (e) set yytext to the address of the string.
It is also advisable (f) to maintain a count of input lines in  yylineno if
the standard error report defaults are used.

*  Actions may be attached to any clause of a grammar definition, and
these will be accumulated during a parse and executed at specified
points. Actions can be arbitrary pieces of C code, and they may cause
side-effects which can influence later parsing activity (see below).

* C language data declarations and function definitions can be
embedded in a definition script.


SYNTAX

PRE-CC supports the basic postfix operators `*' (zero or more times)
`+' (one or more times) of BNF, along with the `[\ ]' (optionally)
outfix operator, and extra prefix operators may be defined using
PRE-CC higher order macros.  A  PRE-CC script consists of a sequence of
definitions, all of the form

@ name = BNFexpression

The `@' is a mark which draws  PRE-CCs `attention' to the line, and any
line which does not begin with `@' will drop through to the output
unchanged, whilst those that do will be interpreted as  PRE-CC
definitions and will be expanded `in place' to the C code equivalent.


The rest of the built-in syntax is:

?	 matches any token;

^	 beginning of line anchor;

$	 end of line match (O TOKEN), forcing the following line to be
     appended. See `$!';

|	 `or', placed between alternate phrases of the grammar;

{ }	 grouping brackets;

< >	 around literals, indicating an exact match
	 with the captured literal token;

> <	 `not a (particular) literal';

( )	 around the name of an  int 1 or 0--valued C function
         on tokens, (possibly defined elsewhere in the script), and
         matching tokens which satisfy the  predicate;

) (	 (anti-brackets) round an  int 1 or 0--valued C
         expression, indicating an independent logical test (carried
         out during the parse, not when actions are discharged);

!	 (cut, or `execute now') which forces all accumulated
         actions to be discharged and disables backtracking through
         this point;

$!	 to match an end of line (O TOKEN), execute accumulated actions,
	 and begin a new line for parsing, overwriting not appending. See `$';

] [	 (`invisible') surrounding context which must be
	 present but not parsed;

: :	 surrounding the C code for  actions which can be
	 attached to any clause.


Simple textual conjunction indicates sequence.

A complex line in a grammar definition script may look like:

@ expr = var <'+'>|<'-'> expr 
@      |     <'('> expr <')'>

Note that lines may be continued on to the next line by `escaping' the
newline character, or by beginning the next line with an `@' too.

Definitions may take parameters, and the following is one way of
picking up the value of a parsed token in the range A-Z and using it
to effect a conditional parse,  bar(1) following if the first token is
`A',  bar(2) if it is `B', etc.:

@ foo = <'A'> bar(1) | \
        <'B'> bar(2) | \
        ...
        <'Z'> bar(26)

but the following takes less space to write, and has exactly the same
semantics:

@ foo = gum(1)

@ gum(n) = <'A'+n-1> bar(n)
@        | )n<26(    gum(n+1)  

Alternatively, the first token can be left for capture by the  bar(n)
parse by enclosing the token match in anti-square brackets, thus:
]<'A'+n-1>[.


THEORY

The declarative model which  PRE-CC follows holds that a compiler is
simply a modified parser, and a parser is a function which consumes an
input stream of tokens, returning a boolean value to indicate whether
or not the input satisfies it, plus the unconsumed portion of the
input:

PARSER == [TOKEN] -> ([TOKEN],BOOLEAN)

and the fundamental idea for the C implementation is that the
side-effecting part of the functionality -- the consuming of the input
stream --  really may be implemented as a side-effect in C:

typedef BOOLEAN PARSER();

and the side-effect is always to move the global pointer  pstr to a
new position in the input stream (if the parse returns the SUCCESS
boolean value).

A  raw stream cannot be rewound, but  PRE-CC buffers input so that full
rewinding is possible only when actions are executed (where the
`!'/`$!' instructions occur in the specification) is the historical
stream input jetisoned, because accurate backtracking is no longer
possible -- the design justification is that unknown side-effects may
have been triggered which are certainly impossible to recall, so it is
not worth recalling anything from now on.  and thus remains faithful
to the functional model.

The functional model is based on an exposition given by Jon Fairbairn
in [Fairbairn]. To implement a BNF description as a
parser, it suffices to know how to (1) implement the description of a
sequence

	     a  b

as the parser which accepts the input which  a accepts, followed by
the input which  b accepts, given that one knows how to implement the
descriptions  a and  b correctly as parsers which accept precisely
what the descriptions  a and  b specify, and (2), do the same for the
description of an `or parse':

	     a | b

which accepts input which  a accepts, or, if that fails, accepts input
which  b accepts.

The functional definition of the `and parse' construction is

andparse(a,b) s =
     let (s',v) = a s in
                  if v then b s
                  else (s,False)

and  PRE-CCs kernel library duplicates this idea faithfully by
implementing the C prototype:

BOOLEAN andparse0(PARSER, PARSER);

replacing explicit references to the streams  s and  s' with real
side-effects.

Similarly, the  PRE-CC library function orparse0 implements the  a | b
construction of parsers. Note that the andparse0 and  orparse0
functions are essentially higher order, and that C supports this
functionality by passing the  addresses of functions (not their
machine code!) across as parameters whenever a datum of type  PARSER
is requested.

There has to be an address for C to pass, which implies that,
corresponding to every subexpression of a BNF form, there has to be
the C language declaration of some function, whose address it is.
Whenever  PRE-CC sees an expression of the form `a b' in the definition
script, it writes the corresponding C definition:

static BOOLEAN hiddenfn(){
    PARSER a, b;
    return(andparse0(a,b));
}

to the output, where ` hiddenfn' is an invented name which will, in
any case, be private to the module (because of the  static keyword),
and is of such a form as to be unlikely to be chosen deliberately by a
programmer.

This description is complicated in the real implementation by the use
of parameters to parsers (and parser descriptions), which requires
that the  andparse equivalent accept further arguments.  Parameters to
a and  b cannot be supplied before andparse calls them because this
would make the implementation over-strict, and result in infinite
recursions from quite ordinary constructions (`@a(n) = x a(n-1) | y',
for example), so they have to be passed to  andparse instead.

Now, compilers differ from parsers in that they cause actions to occur
which have effects on some external state (such as changing the image
on a display screen). The functional model sets compilers to be the
type:

COMPILER == (STATE,[TOKEN]) -> (STATE,[TOKEN],BOOLEAN)

and these can be implemented in C by making the side-effects on state
and the input stream into  real side-effects, which means that
compilers have the same C type as parsers, because the extra effect on
STATE cannot be made explicit.

We define an  action to be a side-effecting function in the functional
model:

ACTION == STATE -> STATE

and implement them in C as functions which take no arguments and
return no results:

typedef void ACTION();

Strictly, this prototype permits some arguments to the function call,
but that is exactly what is required when definitions are
parameterized.

Now, the functional model for building a compiler simply amounts to
attaching an action to a parser -- letting the action be executed
precisely when the parse succeeds:

attach :: PARSER -> ACTION -> COMPILER

attach p f (w,s) =
      let (s',v) = p s in
            if v then (f w,s',True)
            else (w,s,false)

and this can be duplicated in C by a higher order function with
prototype:

BOOLEAN attach(PARSER, ACTION);

which moves the side-effects out of the parameter list and implements
them directly.

One important complication arises, however. Whereas the input stream
can be rewound if the parser parameter fails, side-effects cannot be.
Therefore it is important that, if the parser succeeds, instead of
triggering the action immediately,  attach should simply place the
address of its action parameter onto a  fifo queue for later
execution. The queue is emptied whenever a cut `!' command is reached
in the grammar script (or at the end of stream input, if the parse is
successful, whichever occurs first).

Again, C passes addresses of functions as parameters, not code, so
there must be a C function declaration which corresponds to the action
parameter to the  attach routine, and  PRE-CC builds the C code for it
explicitly. When the form ` a :code:' is encountered in a script, the
action

static void hiddenfn1(){
  code
}

is written in its place, making the  code accessible via a C function
call, and it is followed by the parser definition:

static BOOLEAN hiddenfn2(){
  PARSER a; ACTION hiddenfn1;
  return(attach(a,hiddenfn1));
}

Further aspects of the action semantics are considered in the next
section.

It should be noted that  PRE-CC properly interprets all  inherited
attribute grammar descriptions, by virtue of the fact that it allows
parameters to all the parsers/compilers it constructs there is a
practical limit to the number of parameters allowed in the  PRE-CC
library routines, but it should not be a problem in practice. Up to
eight parameters are allowed at present., and it properly interprets
all  synthetic attribute grammar descriptions because actions may
manipulate a stack of  values, each notionally attached to a BNF
expression (see next section).

PRE-CC's theoretical limitation is that it cannot feed back synthetic
attributes as inherited attributes to sub-grammars (flow in the other
direction is perfectly feasible and, indeed, usual in PRE-CC definition
scripts). This is because synthesised attributes are constructed by
actions, and actions are triggered  after a parse, so cannot feed back
into the parse itself. In turn, synthetic attributes are constructed
by actions because arbitrary C expressions are allowed as values, and
these might have side-effects.

Parsing n input tokens causes  PRE-CC to stack exactly n actions for
later execution. Some of these actions are trivial (for example, on
successfully matching a literal, 'x', say, the token ` x' is by
default scheduled for attachment to the BNF expression 'x' as its
synthesised attribute, and this is achieved by stacking the action
which will place ` x' on the runtime value stack in the correct
position), but the nett effect is that  PRE-CC builds a program of size
that is linear in its uncut input stream length. It also necessarily
stacks C calls on the C call stack to a depth again linearly
(directly) bounded by the input stream, but the depth is usually much
less than the theoretical maximum.


ACTIONS

Now for the horrors of synthetic attributes. To make a parser
generated by  PRE-CC do anything significant, actions have to be
attached to the grammar definition script.

Actions are pieces of C code (terminated by a semi-colon) and placed
between a pair of colons (`::') in the grammar definition script. For
example:

@ addexpr = expr <'+'> expr
@          :VV(3)=V(1)+V(3);:

is not unreasonable. The `VV(3)' tells  PRE-CC that the addexpr clause
manipulates three attributes, notionally attached to each of the terms
on the right of the equals sign. `V(1)' is the value attached to the
leftmost term, and `V(3)' is that attached to the rightmost term.  The
result value is attached to the whole expression.

Literals (like '+') automatically generate an attached value --
themselves. They are stored on the underlying value stack as tokens,
and the `+' token can, in this case, be retrieved by the expression
`T(2)'.

The stacked values are structures with the type of the unions of the
VALUE and  TOKEN types, and `V( n)' in an action code retrieves the
value as a  VALUE, whilst `T( n)' retrieves it as a TOKEN.

PRE-CC supports two (internal) function-calling conventions. Under one
of these, it is not necessary to get the  VV(n) counts correct
everywhere (or to include them at all -- V(1) is a completely correct
destination for the attribute value), and under the other, it is.  Set

call_mode=0;

in the  BEGIN macro to use the `caller is responsible' (same as C)
convention for stack shuffling.  PRE-CC is the `caller' and this
setting relieves the programmer of all responsibility for getting the
VV( n) counts right. The alternative convention, `callee is
responsible' (as in Pascal).  is set by

call_mode=1;

The programmer is the `callee'. The latter convention is much more
efficient in terms of the runtime program size, but the former should
probably be used for all development work.

The C convention is compatible with the Pascal one provided that no
`tricks' appear in the script. It is unlikely that a novice will have
used any tricks, but the  PRE-CC.y script, for example, contains many
deliberately late stack-pops, and these will not be conserved if run
under the C convention instead of the more efficient Pascal one (this
makes no difference to users, since they only use the compiled  PRE-CC
executable).

A  VV(n) should appear on the left hand side of the action code (under
the Pascal calling convention), and an easy way to do this is to put
it on the left of the first equals sign (assignment).  After the
VV(n) (or  TT(n)) call, the  V(n) and  T(n) values will be correctly
aligned with the grammar expressions under the Pascal convention, and
without it, they will not be.

This is all essentially exactly the same treatment as in YACC, and it
also allows one to incorporate the same incomprehensible trick of
pulling values off the stack when they are notionally `further to the
left' than the current expression, using  V(0) or even lower
references.

Side-effecting actions can be sequenced as follows (recall that no
side-effect takes place until after the parse, by which time PRE-CC has
built an internal list of actions to execute which corresponds exactly
to the successful trace through the grammar).  The specification:

@abc=a b c :printf("D");:

@a=<'a'>   :printf("A");:

@b=<'b'>   :printf("B");:

@c=<'c'>   :printf("C");:

will, upon receiving input "abc", generate the program

printf("A");printf("B");printf("C");printf("D");

to be executed later.

Thus, actions attached to a sequence expression may be thought of as
occurring immediately after the actions attached to sub-expressions,
and so on down.

EXAMPLES

The following script defines a simple +/- calculator in classic
synthetic attribute grammar style:


# define VALUE int
# define BEGIN call_mode=1; /*pascal style*/
# include "cc.h"
# include <ctype.h>

static int acc;

@digit = (isdigit)      :VV(1)=T(1)-'0';\
                         acc=acc*10+V(1);:

@posint= digit posint   :VV(2)=V(2);:      |\
         digit          :VV(1)=V(1);acc=0;:

@int   = <'-'> posint   :VV(2)=-V(2);:     |\
         posint

@atom  = <'('>expr<')'> :VV(3)=V(2);:      |\
         int

@expr  = atom sign_sum  :VV(2)=V(1)+V(2);: |\
         atom

@sign_sum= <'-'> atom sign_sum\
                        :VV(3)=-V(1)+V(3);:|\
           <'-'> atom   :VV(2)=-V(2);:     |\
           <'+'> atom sign_sum\
                        :VV(3)=V(1)+V(3);: |\
           <'+'> atom   :VV(2)=V(2);:

MAIN(expr)

Although no cut (`!'/`$!') commands occur in the script,  PRE-CC always
executes its top level parser repeatedly, which gives the required
effect here (nor has the behaviour on the occurrence of errors been
explicitly specified). All the defaults combine to make the `true' top
level parser specification invoked by MAIN(expr) equivalent to the
explicit rendering:

@ top  = main ! top

@ main = expr [?+ :ON_ERROR(0):] |\
               ?* :ON_ERROR(1):

where the defaults for the  ON_ERROR macro are assumed.  The  main
definition is executed repeatedly, and  main is a parse which cannot
fail, consisting as it does of either an  expr parse (optionally
followed by some extra tokens, which triggers the the ON_ERROR(0)
behaviour) or of anything else at all (which triggers the ON_ERROR(1)
action).

Note that `VV(1)=V(1)' has no overall effect, so it has been dropped
from most of the points in the script where it might have been
expected.

Also note that it would have been more efficient to use an optional
following action and write

@expr= atom  [ sign_sum :VV(2)=..: ]

instead of

@expr= atom sign_sum :VV(2)=..: | atom

because the latter expression will build a parser which needlessly
checks twice for  atom when no sign_sum follows.

Now for a higher order example. The following code defines the `macro'
key(x), used to expand a string argument  x into a match against the
sequence of its character components:

@ key(x) = )x[0]!=0( <x[0]> key(&x[1])
@        | )x[0]==0( 

so that key("foo") will be equivalent to

<'f'> <'o'> <'o'>

Higher order behaviour may be observed when one uses  key(x)
instantiations as  building blocks:

@ keyword = key("key") key("word")

because then it is apparent that the constructed parsers really are
almost `first class citizens' of the parser world.  The following
`macro' defines the concept of a list with separators, using a parser
p as its abstract parameter:

@ sep_list(p,s) = p [ <s> sep_list(p,s) ]

and one may generate a match for a comma-separated list of the word
"boring" by supplying the constructed parser key("boring") as the
parameter  p:

@ boring = key("boring")

@ boring_list = sep_list(boring,',')

but note that the nested construction sep_list(key("boring"),',') is
not allowed. The only legal parameters are those of valid  C type,
either a character, an integer, a float, a structure or an address,
and whilst  boring has an address, since it is a parser, and hence a C
function, key("boring") is a C call, and not a function in its own
right.

Figures 2 and 3 respectively show portions of the definition scripts
for COBOL and  PRE-CC itself.


SUMMARY

PRE-CC is a compiler-compiler based on a higher order functional
programming model which produces modular and highly portable (and
maintainable) C code. All the standard BNF constructs are built into
the grammar definition language, and more may be defined using  PRE-CCs
own `macro's. The utility has been used to generate parsers and
compilers for both commercially available (COBOL [Cobol]) and
experimental (Uniform [Uniform]) languages.


# define BEGIN call_mode=0;
MAIN(c_program)

@c_program = [ identification_division ] 
@            [ environment_division ] 
@            [ data_division ] 
@            procedure_division 

@ stop     =  <'.'>  $!

@identification_division  =   
@            C_IDENTIFICATION 
@            C_DIVISION stop 
@            [ program_identification ] 
@            [ remarks    ] 

@program_identification  =   
@            C_PROGRAM_ID stop 
@            identifier( progname) stop

@data_division  =   
@            C_DATA C_DIVISION stop 
@            [ file_section ] 
@            [ working_storage_section ] 
@            [ linkage_section ] 
@            [ report_section ] 

Figure 2: A portion of the parse definition for COBOL 74.

PRE-CC has been designed with the aim of lifting the programming and
implementational restrictions imposed by compiler-compilers like the
Unix YACC utility. The design

(1) removes the debugging stage implicitly required by  Yacc's
one-token lookahead restriction ( PRE-CC-generated applications have
unlimited lookahead and backtracking);

(2) allows arbitrarily complex BNF expressions in definition scripts;

(3) uses parameterized grammar descriptions (referentially transparent
`macros') for clearer coding and maintainability;

(4) makes  PRE-CC modules entirely independent of and linkable with one
another, so that changes may be carried out locally.


# define BEGIN call_mode=1;
MAIN(line)

/*we will pop values later*/

@line     =  declaration  
@              :VV(1)=V(1);: 
@         |  notanat literal  
@              :/* so no VV(2)=.. */: 
@         |   
@              :VV(0)="";printf("n");:

@notanat  =  '@' 
@              :VV(1)=savechar(T(1));:

/* here's the pending pop */

@literal  =  anychar literal 
@             :VV(2)=V(1);:  
@         |    
@             :VV(1)=spendchars( V(1));  
@             /* Ah! VV(1) felt good .. */ 
@              printf("%s\n",V(1));
@              RESET;:

@anychar  =  ? 
@              :VV(1)=savechar(T(1));:

@pad(p)   =  w_space p w_space 
@              :VV(3)=V(2);:

@w_space  =  (isspace) w_space 
@              :VV(2)=V(2);: 
@         |  
@              :VV(0)="";:

@ldecl    =  ^ <'@'> pad(lvalue) 
@              :VV(3)=V(3);:

@declaration =   
@           ldecl '=' rdecl 
@             :VV(3)=P_REN(V(1),V(3)); 
@              RESET;:

@rdecl    =  pad(expression) 
@              :VV(1)=V(1);:


Figure 3: A portion of the compile definition for  PRE-CC, complete
with attached actions.


REFERENCES

[Prolog] W.,F. Clocksin and C.,S. Mellish,
Programming in Prolog,
3rd Edition, Springer-Verlag, 1987.

[Fairbairn] J. Fairbairn,
Making Form Follow Function: An Exercise in Functional Programming Style,
Software -- Practice and Experience,
17(6), 379--386, 1987.

[YACC] S.,C. Johnson and M.,E. Lesk,
Language Development Tools,
The Bell System Technical Journal
57(6), part 2, 2155--2175, July/August 1978.

[Knuth] D.,E. Knuth,
Literate Programming,
The Computer Journal,
27(2), 97--111, 1976.

[ML] P. Milner and Mads Tofte,
Commentary on Standard ML,
MIT Press, 1991.

[Cobol] A. Parkin,
COBOL for Students,
Edward Arnold, London, 1984.

[Quintus]
Quintus Prolog Version 2.5 Manual,
Artificial Intelligence International Ltd., Watford, U.K., 1990.

[Uniform] C. Stanley-Smith and A. Cahill,
UNIFORM: A Language Geared To System Description and Transformation,
University of Limerick, 1990.


Author

Peter Breuer, Programming Research Group,
Oxford University Computing Laboratory, 11 Keble Rd., Oxford  OX1 3QD,
UK.



USAGE

PRE-CC can be used as a  stdin to  stdout filter, when it takes no
options or arguments:

 PRE-CC <  file.y >  file.c

but it is preferable to use the command line options (so that error
messages can be routed to the correct destinations without fear of
confusion).

 PRE-CC file.y  file.c

If no file.c is given, output will go to stdout, and if no file.y is
given either, input will be taken from stdin.

PRE-CC grammar description files conventionally have the ` .y' suffix,
and should follow the following format:

# define TOKEN ...
# define VALUE ...
# define BEGIN ...
# define END   ...
# define ON_ERROR(x) ...
# include "cc.h"  (or ccx.h)
@ first definition : attached action; :
@ ...
@ ...
MAIN(name of entry clause)



# define TOKEN  tokentype (default  char) defines the space reserved
for each incoming token in the parser, and

# define VALUE  valuetype (default  char*) defines the space reserved
for each constructed attribute on the runtime stack available to
actions.

# define BEGIN  C-code and

# define END  C-code (default empty) define code to be executed at the
begining and end of a complete parse, respectively (on opening the
input stream and on closing it again), whilst

# define ON_ERROR(x)  C-code (default to standard actions) defines
actions to take when an error is encountered during a parse.


Two sorts of errors are recognized: ON_ERROR(x).  x=0 indicates that a
partial parse successfully took place, and  x=1 that the parse failed
entirely. The former error is to be expected when the defined grammar
matches only an initial segment of the input stream (an empty grammar
definition, for example), and the latter when no match at all is
possible.

The header file "cc.h" may be subsituted for "ccx.h" in modules which
do not use parameterized grammar definitions, and results in faster
runtime code.

The way to compile a C source code file `foo.c' generated by PRE-CC
into an executable `foo' is to use an incantation like:

 gcc -ansi -o foo foo.c -L  PRE-CC dir -lcc

which links in the small  PRE-CC runtime kernel in the  libcc.a
library.  A default do-nothing tokeniser is provided in the library
and will be automatically linked in unless a different  yylex()
routine is specified to the C compiler by including its object module
ahead of the -lcc option.

The default parser generated from a  PRE-CC script will signal valid
input by absorbing it silently, and signal invalid input by rejecting
it and generating an error message. This is a standard style for
compiler-compilers. To get the parser to do anything else, the
definition script must be decorated with  actions in the  YACC style.


