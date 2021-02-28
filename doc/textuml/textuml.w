\documentclass[12pt]{article}
\usepackage[a4paper, margin=2.7cm]{geometry}
\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue,urlcolor=blue,breaklinks]{hyperref}
\usepackage{graphicx}
\usepackage{multicol}

\setlength{\parindent}{0cm}
\setlength{\parskip}{0.2em}

\begin{document}

\title{Using ColdFrame's TextUML}
\author{Simon Wright {\it simon@@pushface.org}}

\maketitle

\begin{abstract}
  \setlength{\parindent}{0cm}
  \setlength{\parskip}{0.2em}
  \noindent
  A worked example of the use of a textual form of UML to prepare
  translatable models.
\end{abstract}

\tableofcontents

\section{Introduction}

\href{https://github.com/simonjwright/coldframe}{ColdFrame} is an
open-source code generator backend for use with UML tools, targeted at
\href{https://www.adaic.org}{Ada}.

Until recently (2019) the UML tool of choice has been
\href{https://github.com/argouml-tigris-org/argouml}{ArgoUML}; it is
still being worked on, but the released versions don't work on newer
macOS releases, and the development version only runs from an awkward
command line interface.

The \href{https://github.com/abstratt/textuml}{TextUML} project is a
Java-based tool to encode UML models in textual form. It goes beyond
the aims of this project, in that it provides an action language. This
means that the whole application can be written in TextUML and
executable code can be generated from it.

ColdFrame doesn't go as far as this: it generates a framework, which
can call up user code in the form of separate subprograms. Recently,
it's been made possible to include some user code (in Ada) in the
model.

This document has been generated using
\href{https://github.com/simonjwright/nuweb.py}{nuweb.py}, with
conversion to PDF via \href{https://www.tug.org/texlive/}{TeX Live}.

\section{Worked Example: Simple Buttons}

The syntax of ColdFrame's version of TextUML is reproduced in Section
\ref{sec-syntax}.

A TextUML model can contain multiple
\href{https://simonjwright.github.io/coldframe/domains.html}{domains}. It
acts only as a holder; its name has no significance. This is a `file'
scrap (the introductory \verb|@@o|), as encoded in the source web,
which results in the other scraps in the document being `tangled' into
the file indicated.

\begin{verbatim}
@@o textuml.tuml @@{
(*
  This is a model comment, which appears before the element concerned
  and will be included in the output.
*)
/* This is a textual comment, which will be ignored. */
model TextUML\_Demonstration;
   @@< The domains @@>
end.
@@}
\end{verbatim}

It gets `woven' as

@o textuml.tuml @{
(*
  This is a model comment, which appears before the element concerned
  and will be included in the output.
*)
/* This is a textual comment, which will be ignored. */
model TextUML_Demonstration;
   @< The domains @>
end.
@}

If you were generating the TextUML file by hand, you'd write
documentation as model comments. Here, they've been expressed in the
document instead, to improve readabiity.

The \verb|Simple_Buttons| domain (the only one in this model) is
intended for demonstrating ColdFrame's use in Ravenscar systems.

Because sampler boards have very few buttons and user-accessible LEDs,
the design is very restricted. A Button can receive a short push (less
than a quarter of a second) or a long push; after a short push, it's
`set' for 5 seconds; after a long push, it's `set' until another
push. A Button can be wired to control one or more LEDs; an LED can be
wired to be controlled by one or more Buttons.

In TextUML, a domain is a package with the {\it annotation}
\verb|[domain]| (``annotation'' is the TextUML word for {\it
  stereotype}). Other ways of decorating the model elements are {\it
  modifiers}, which are (reserved) keywords in the syntax; for
example, you could specify that an attribute is identifying either by
using the modifier \verb|id| or the annotation \verb|[\id]| (the
backslash is removed during processing, but allows you to use
otherwise-reserved identifiers).

A domain package can have nested packages, whose contents are
incorporated directly into the domain.

%You can annotate such nested
%packages with \verb|[domain_interface]|, in which case ...

@d The domains @{
[domain]
package Simple_Buttons;
   @< SB.Enumerations @>

   @< SB.Imported types @>

   @< SB.Signals @>

   @< SB.Classes @>

   @< SB.Associations @>
end;
@| Simple_Buttons @}

Signals correspond to ColdFrame's
\href{https://simonjwright.github.io/coldframe/events.html}{events}.

A \href{https://simonjwright.github.io/coldframe/classes.html}{class}
typically is an abstraction of something in the domain of interest. It
represents the common properties and behaviour shared by all instances
of the class.

An
\href{https://simonjwright.github.io/coldframe/attributes.html}{attribute}
holds a property of an object (either one per instance, for example
the Accession Number of a Book in a Library, or per class, for example
the next Accession Number to be used).

The purpose of
\href{https://simonjwright.github.io/coldframe/operations.html}{operations}
is to implement the actual functionality of the domain.

An
\href{https://simonjwright.github.io/coldframe/associations.html}{association}
is a relationship between two classes in the model (it is possible,
though uncommon, to have a {\it reflexive} association between a class
and itself, e.g. {\it Action is-a-consequence-of Action}).

\subsection{Enumerations}

This enumeration names the buttons. Only \verb|B1| will be used.

@d SB.Enumerations @{
enumeration Button_Name
   B1,
   B2
end;
@| Button_Name @}

This enumeration names the LEDs. Only \verb|L1| will be used.

@d SB.Enumerations @{
enumeration LED_Name
   L1,
   L2
end;
@| LED_Name @}

\subsection{Imported Types}

This imported type is used by the supporting Digital IO domain to
report input (switch) state changes. The annotation \verb|[imported]|
includes a tagged value (tag \verb|imported|, value
\verb|Digital_IO|).

@d SB.Imported types @{
[imported (imported = Digital_IO)]
datatype Input_Signal_State;
@| Input_Signal_State @}

This type is used by the supporting Digital IO domain to name
outputs (LEDs).

@d SB.Imported types @{
[imported (imported = Digital_IO)]
datatype Output_Signal;
@| Output_Signal @}

\subsection{Signals}

As noted above, signals correspond to ColdFrame's events.

UML has them declared at package (domain) level, though ColdFrame's
implementation actually declares the corresponding event types in the
specification of the class where they're used (in this case, Button);
hence the need, in general, to specify the target class here (suppose
there was more than one class in the domain that had to receive
events?). Note the namespace separator \verb|::|.

This event indicates that the button `pushed' period (after a short
push) has expired.

@d SB.Signals @{
signal Button::Lit_Timeout;
@| Button::Lit_Timeout @}

This event indicates that the button has been pushed.

@d SB.Signals @{
signal Button::Push;
@| Button::Push @}

This event indicates that the button has been pushed long enough to
make this a long push.

@d SB.Signals @{
signal Button::Push_Timeout;
@| Button::Push_Timeout @}

This event indicates that the button has been released.

@d SB.Signals @{
signal Button::Release;
@| Button::Release @}

\subsection{Class Button}

A Button controls a number of LEDs. When the Button is `set', the
LEDs related by A1 are lit.

Buttons respond to both `short' and `long' pushes.

After a long push, the button remains set until it's pushed again
(long or short).

After a short push, the Button remains set for a period, which can
be extended by a further short push or a long push.

@d SB.Classes @{
class Button
   @< SB.Button attributes @>

   @< SB.Button operations @>

   @< SB.Button state machine @>
end;
@| Button @}

\subsubsection{Button attributes}

This identifying attribute (the \verb|id| modifier) is the name of the
Button.

@d SB.Button attributes @{
id attribute Name : Button_Name;
@| Name @}

This attribute holds the time when the Button was pushed, so that the
Lit timeout can run from this initial time rather than (e.g.) when the
Button was released.

@d SB.Button attributes @{
attribute Pushed_Time : Time;
@| Pushed_Time @}

This ColdFrame timer controls how long the Button needs to remain
pushed before transition to the Held state.

@d SB.Button attributes @{
attribute Lit_Timer : Timer;
@| Lit_Timer @}

This timer controls how long the Button needs to remain pushed before
transition to the Held state.

@d SB.Button attributes @{
attribute Pushed_Timer : Timer;
@| Pushed_Timer @}

\subsubsection{Button operations}

The state of the button has changed; tell the controlled LEDs to
reevaluate their own states (by checking whether any of the Buttons
they are controlled by is set). Note the modifier \verb|private|.

@d SB.Button operations @{
private operation Changed();
@| Changed @}

This operation stores the time at which the Button was pushed: the Lit
timeout runs from this time, not the time of Button release.

This operation is short enough that we can include its code here,
within the curly braces.

@d SB.Button operations @{
private operation Note_Pushed_Time();
{
   This.Pushed_Time := ColdFrame.Project.Calendar.Clock;
}
@| Note_Pushed_Time @}

This operation sets the Pushed timeout, again including the code in
the model. The indentation will be preserved (actually, relative to
the first non-space character of the first line)

@d SB.Button operations @{
private operation Set_Pushed_Timeout();
{
   ColdFrame.Project.Events.Set
     (The_Timer => This.Pushed_Timer,
      On => Events.Dispatcher,
      To_Fire => new Push_Timeout (This),
      After => 0.25);
}
@| Set_Pushed_Timeout @}

This operation clears the Pushed timeout.

@d SB.Button operations @{
private operation Clear_Pushed_Timeout();
{
   ColdFrame.Project.Events.Unset
     (The_Timer => This.Pushed_Timer,
      On => Events.Dispatcher);
}
@| Clear_Pushed_Timeout @}

This operation sets the Lit timeout. It's called on button release
after a short push, but the time is relative to the time when the
button was pushed.

@d SB.Button operations @{
private operation Set_Lit_Timeout();
@| Set_Lit_Timeout @}

This operation clears the Lit timeout.

@d SB.Button operations @{
private operation Clear_Lit_Timeout();
{
   ColdFrame.Project.Events.Unset
     (The_Timer => This.Lit_Timer,
      On => Events.Dispatcher);
}
@| Clear_Lit_Timeout @}

This operation indicates whether the Button is set or not. It's set if
it's in any of the states \verb|Pushed|, \verb|Held|, \verb|Timed|,
\verb|Pushed_Again|.

Note, the code is emitted in the body of the Ada subprogram, so if any
local variables are needed a \verb|declare| block has to be used (in
this particular case, a one-liner would actually have been possible).

@d SB.Button operations @{
public operation Is_Set(): Boolean;
{
   declare
      Set_In_State : constant array (State_Machine_State_T) of Boolean
        := (Pushed | Held | Timed | Pushed_Again => True,
            others => False);
   begin
      return Set_In_State (This.State_Machine_State);
   end;
}
@| Is_Set @}

This operation acts as receiver of state changes from Digital\_IO, via
Input Signal State Callback. The annotation \verb|[callback]|
triggers the necessary event generation. The modifier \verb|static|
isn't strictly necessary, since ColdFrame would automatically generate
a class operation anyway, but avoids a warning.

Calls the instance Changed so the Button can take the appropriate
action.

@d SB.Button operations @{
[callback]
private static operation Receive_Change(S : Input_Signal_State);
@| Receive_Change @}

\subsubsection{Button state machine}

This is a \href{https://en.wikipedia.org/wiki/Moore_machine}{Moore
  model state machine}; all the actions take place on entry to a
state. See Figure~\ref{fig:button-statechart} for the generated
statechart.

ColdFrame also supports
\href{https://en.wikipedia.org/wiki/Mealy_machine}{Mealy model state
  machines}, where all the actions take place on transitions, as well
as mixed machines.

\begin{figure}
  \caption{Generated Button statechart}
  \label{fig:button-statechart}
  \begin{center}
    \includegraphics[width=7.5cm]
                    {Simple_Buttons.images/Simple_Buttons.Button.state.png}
  \end{center}
\end {figure}

@d SB.Button state machine @{
statemachine Button
   @< SB.Button states @>
end;
@}

This is the inital state (indicated by the \verb|initial|
modifier). It performs a completion transition to Off.

@d SB.Button states @{
initial state Initial
   transition to Off;
end;
@| Initial @}

In the state Off, the button is off, waiting for a Push. If this state
was entered as a result of a Push in the Held state, there will be a
corresponding Release, which is ignored (the annotation
\verb|[ignore]|).

On entry, \verb|Changed| is called to tell the connected LEDs that
they need to reconsider whether they should be lit.

@d SB.Button states @{
state Off
   entry(Changed);
   transition on signal(Button::Push) to Pushed;
   [ignore] transition on signal(Button::Release) to Off;
end;
@| Off @}

In the state Pushed, the button is on, awaiting a Push\_Timeout, which
transitions to the Held state (a long push), or a Release (a short
push), which transitions to the Timed state.

The entry actions are
\begin{enumerate}
  \item Note\_Pushed\_Time: note when the button was pushed, so that
    if it's released before the coming Push\_Timeout, this time can
    be used to determine how long the button remains `pushed'.
  \item Changed: tell the connected LEDs that they need to reconsider
    whether they should be lit.
  \item Set\_Pushed\_Timeout: if this timeout occurs, this was a long
    push.
\end{enumerate}

@d SB.Button states @{
state Pushed
   entry(Note_Pushed_Time; Changed; Set_Pushed_Timeout);
   transition on signal(Button::Push_Timeout) to Held;
   transition on signal(Button::Release) to Timed;
end;
@| Pushed @}

In the state Timed, the button is on after a short push, awaiting a
Lit\_Timeout (which transitions to the Off state) or another Push
(which transitions to Pushed\_Again).

The entry actions are
\begin{enumerate}
  \item Clear\_Pushed\_Timeout: The Pushed\_Timeout that was started
    in the state Pushed is cancelled, because it's been overtaken by
    the short push that just occurred.
  \item Set\_Lit\_Timeout: This determines how long the button remains
    `pushed' for.
\end{enumerate}

@d SB.Button states @{
state Timed
   entry(Clear_Pushed_Timeout; Set_Lit_Timeout);
   transition on signal(Button::Push) to Pushed_Again;
   transition on signal(Button::Lit_Timeout) to Off;
end;
@| Timed @}

In the state Pushed\_Again, the button has been pushed during the
timeout after a short push.  Resets the timeout (in the entry action)
and performs a completion transition to Pushed to start another check
(this Push can be the start of another short push or a new long push).

@d SB.Button states @{
state Pushed_Again
   entry(Clear_Lit_Timeout);
   transition to Pushed;
end;
@| Pushed_Again @}

In the state Held, the button is on, after a long push, awaiting
another Push to transition to the Off state. The button is still
pushed, so there will be a corresponding Release, which is ignored.

@d SB.Button states @{
state Held
   transition on signal(Button::Push) to Off;
   [ignore] transition on signal(Button::Release) to Held;
end;
@| Held @}

Note that the state model could have been cast as a mixed Moore-Mealy
machine, by writing the state Timed as

\begin{verbatim}
@@d SB.Button states @@{
state Timed
   entry(Clear_Pushed_Timeout; Set_Lit_Timeout);
   transition on signal(Button::Push) to Pushed
      do (Clear_Lit_Timeout);
   transition on signal(Button::Lit_Timeout) to Off;
end;
@@}
\end{verbatim}

which implements the \verb|Clear_Lit_Timeout| action as the (only)
effect of the transition signalled by the \verb|Button::Pushed| event,
and eliminates the need for the Pushed\_Again state.

\subsection{Class LED}

An LED is lit when any of the Buttons it's controlled by is set.

@d SB.Classes @{
class LED
   @< SB.LED attributes @>

   @< SB.LED operations @>
end;
@| LED @}

\subsubsection{LED attributes}

This attribute identifies the LED.

@d SB.LED attributes @{
id attribute Name : LED_Name;
@| Name @}

\subsubsection{LED operations}

This operation initialises the domain (this is indicated by the
annotation \verb|[init]|) by creating Button(s) and LED(s) as
required, and associating them according to the required ``circuit
diagram''.

@d SB.LED operations @{
[init]
private static operation Initialize();
@| Intialize @}

This operation is called from a controlling Button which has changed
to evaluate whether the LED should be lit (if any of the controlling
Buttons is set) or not.

@d SB.LED operations @{
public operation Changed();
@| Changed @}

This operation maps the LED to the corresponding Digital\_IO output
pin.

@d SB.LED operations @{
private operation Output_Signal_For_LED(): Output_Signal;
{
  --  This isn't going to be very extendable, but there's only one
  --  LED in this simple demo.
  return LED_Name'Pos (This.Name);
}
@| Output_Signal_For_LED @}

\subsection{Associations}

This association relates each LED to the Button(s) it's controlled by.

Each Button controls one or more LEDs.

Each LED is controlled by one or more Buttons.

This is a many-to-many relationship, so ColdFrame requires that it be
implemented as an Association Class, even though there are (as yet) no
useful attributes for the Class part to contain.

@d SB.Associations @{
association_class A1
   Button Controls LED[1,*];
   LED Is_Controlled_By Button[1,*];
end;
@| A1 @}

\section{TextUML tokens}\label{sec-tokens}

These are the tokens used (and, importantly, reserved) by
TextUML. Those bolded correspond to
\href{https://simonjwright.github.io/coldframe/ColdFrameProfile.html}{stereotypes}
in ColdFrame.

\begin{multicols}{4}
abstract\\
association\\
association\_class\\
attribute\\
{\bf class}\\
component\\
{\bf datatype}\\
do\\
end\\
{\bf entry}\\
enumeration\\
exception\\
false\\
{\bf final}\\
{\bf id}\\
in\\
initial\\
inout\\
interface\\
model\\
{\bf null}\\
on\\
operation\\
out\\
package\\
primitive\\
private\\
{\bf protected}\\
{\bf public}\\
signal\\
specializes\\
state\\
statemachine\\
static\\
terminate\\
to\\
transition\\
true\\
\end{multicols}

In most cases, there won't be a problem, but if you need to use one in
an annotation (e.g. \verb|[class]|, which at present is still needed
in class signals and state machines -- \verb|static| should be
allowed) you can either precede it with a backslash (\verb|[\class]|)
or capitalise it (\verb|[Class]|).

Some of the ColdFrame stereotypes have hyphens, which isn't supported
in TextUML because the name needs to be an identifier. Because of
this, underscores in annotation names are translated to hyphens.

An example would be

@O test.tuml @{
model test;
   [domain_interface (name=test)]
   package test_it;
      [\protected] public datatype prot
         operation set(value : integer);
         [\entry] operation get(out value : integer);
         private attribute value : integer := 42;
      end;
   end;
end.
@}

\section{Syntax}\label{sec-syntax}

Note, this syntax doesn't include the tokens; they are the UPPER CASE
elements below. In most cases, the actual token is the lower-case
version of the element here.

\begin{verbatim}
start : \
    model_comment annotations model_heading \
         namespace_contents END DOT

model_heading : MODEL qualified_identifier SEMICOLON

qualified_identifier \
    : identifier NAMESPACE_SEPARATOR qualified_identifier
    | identifier

namespace_contents \
    : top_level_element namespace_contents
    | top_level_element

sub_namespace \
    : package_heading \
      namespace_contents END SEMICOLON

package_heading : PACKAGE qualified_identifier SEMICOLON

top_level_element \
    : model_comment annotations top_level_element_choice

top_level_element_choice \
    : association_class_def
    | association_def
    | class_def
    | datatype_def
    | enumeration_def
    | exception_def
    | primitive_def
    | signal_def
    | sub_namespace

single_type_identifier : qualified_identifier

type_identifier \
    : single_type_identifier optional_multiplicity
    | function_signature optional_multiplicity

optional_multiplicity \
    : L_BRACKET multiplicity_spec R_BRACKET
    | empty

multiplicity_spec \
    : multiplicity_value COMMA multiplicity_value
    | multiplicity_value

association_def \
    : annotations ASSOCIATION identifier association_role_decl_list \
        END SEMICOLON

association_class_def \
    : annotations ASSOCIATION_CLASS identifier \
        association_role_decl_list feature_decl_list \
        END SEMICOLON
    | annotations ASSOCIATION_CLASS identifier \
        association_role_decl_list \
        END SEMICOLON

association_multiplicity \
    : L_BRACKET multiplicity_spec R_BRACKET

association_role_decl_list \
    : association_role_decl association_role_decl

association_role_decl \
    : model_comment annotations \
        identifier identifier identifier association_multiplicity SEMICOLON

class_def : class_header feature_decl_list END SEMICOLON

class_header \
    : class_modifiers class_type identifier class_specializes_section

class_modifiers \
    : class_modifier_list
    | empty

class_modifier_list \
    : class_modifier class_modifier_list
    | class_modifier

class_modifier \
    : visibility_modifier
    | ABSTRACT

class_specializes_section \
    : SPECIALIZES class_specializes_list
    | empty

class_specializes_list \
    : identifier COMMA class_specializes_list
    | identifier

class_type \
    : CLASS
    | INTERFACE
    | COMPONENT

feature_decl_list \
    : feature_decl feature_decl_list
    | feature_decl

feature_decl \
    : model_comment annotations feature_modifiers feature_type

feature_modifiers \
    : feature_modifier_list
    | empty

feature_modifier_list \
    : feature_modifier feature_modifier_list
    | feature_modifier

feature_modifier \
    : visibility_modifier
    | STATIC
    | ABSTRACT
    | ID

visibility_modifier \
    : PUBLIC
    | PRIVATE
    | PACKAGE
    | PROTECTED

feature_type \
    : state_machine_decl
    | operation_decl
    | attribute_decl

state_machine_decl \
    : STATEMACHINE identifier state_decls END SEMICOLON
    | STATEMACHINE state_decls END SEMICOLON

state_decls \
    : state_decl state_decls
    | state_decl

state_decl \
    : model_comment state_modifier STATE identifier state_behaviours \
        transition_decls END SEMICOLON
    | model_comment STATE identifier state_behaviours \
      transition_decls END SEMICOLON

state_modifier \
    : INITIAL
    | TERMINATE
    | FINAL

state_behaviours \
    : state_behaviour_list
    | empty

state_behaviour_list \
    : state_behaviour state_behaviour_list
    | state_behaviour

state_behaviour : ENTRY state_behaviour_definition SEMICOLON

state_behaviour_definition : simple_statement_block

transition_decls \
    : transition_decl_list
    | empty

transition_decl_list \
    : transition_decl transition_decl_list
    | transition_decl

transition_decl \
    : model_comment annotations TRANSITION ON SIGNAL \
        L_PAREN qualified_identifier R_PAREN \
        TO identifier transition_effect_opt SEMICOLON
    | model_comment annotations TRANSITION TO identifier \
        transition_effect_opt SEMICOLON

transition_effect_opt \
    : DO simple_statement_block
    | empty

simple_statement_block \
    : L_PAREN statement_list R_PAREN
    | identifier

statement_list \
    : identifier SEMICOLON statement_list
    | identifier

operation_body : OPERATION_BODY

operation_decl \
    : operation_header SEMICOLON operation_body
    | operation_header SEMICOLON

operation_header : OPERATION identifier signature

attribute_decl \
    : ATTRIBUTE identifier COLON type_identifier \
        initialization_expression_opt SEMICOLON

initialization_expression_opt \
    : initialization_expression
    | empty

initialization_expression : ASSIGNOP simple_initialization

simple_initialization : literal_or_identifier

function_signature : L_CURLY_BRACKET simple_signature R_CURLY_BRACKET

signature : L_PAREN param_decl_list R_PAREN optional_return_type

simple_signature \
    : L_PAREN simple_param_decl_list R_PAREN simple_optional_return_type
    | L_PAREN simple_param_decl_list R_PAREN

optional_return_type \
    : annotations simple_optional_return_type
    | empty

simple_optional_return_type : COLON type_identifier

param_decl_list \
    : param_decl COMMA param_decl_list
    | param_decl
    | empty

simple_param_decl_list \
    : simple_param_decl COMMA simple_param_decl_list
    | simple_param_decl
    | empty

param_decl : annotations parameter_modifiers simple_param_decl

simple_param_decl \
    : optional_parameter_name COLON type_identifier \
        initialization_expression_opt

optional_parameter_name \
    : identifier
    | empty

parameter_modifiers \
    : parameter_modifier parameter_modifiers
    | empty

parameter_modifier \
    : IN
    | OUT
    | INOUT

annotations \
    : L_BRACKET annotation_list R_BRACKET
    | empty

annotation_list \
    : annotation COMMA annotation_list
    | annotation

annotation \
    : qualified_identifier annotation_value_specs
    | qualified_identifier

annotation_value_specs \
    : L_PAREN annotation_value_spec_list R_PAREN

annotation_value_spec_list \
    : annotation_value_spec COMMA annotation_value_spec_list
    | annotation_value_spec

annotation_value_spec : identifier EQUALS annotation_value

annotation_value \
    : literal
    | qualified_identifier

datatype_def \
    : datatype_header feature_decl_list END SEMICOLON
    | datatype_header SEMICOLON

datatype_header : class_modifiers DATATYPE identifier

enumeration_def \
    : visibility_modifier ENUMERATION identifier \
        enumeration_literal_decl_list END SEMICOLON
    | ENUMERATION identifier \
        enumeration_literal_decl_list END SEMICOLON

enumeration_literal_decl_list \
    : enumeration_literal_decl enumeration_literal_decl_list_tail

enumeration_literal_decl : model_comment identifier

enumeration_literal_decl_list_tail \
    : COMMA enumeration_literal_decl_list
    | empty

exception_def \
    : visibility_modifier EXCEPTION identifier SEMICOLON
    | EXCEPTION identifier SEMICOLON

signal_def : signal_decl

signal_decl \
    : SIGNAL qualified_identifier signal_attributes END SEMICOLON
    | SIGNAL qualified_identifier SEMICOLON

signal_attributes \
    : signal_attribute_decl signal_attributes
    | signal_attribute_decl

signal_attribute_decl \
    : ATTRIBUTE identifier COLON type_identifier SEMICOLON

primitive_def \
    : visibility_modifier PRIMITIVE identifier SEMICOLON
    | PRIMITIVE identifier SEMICOLON

model_comment \
    : MODEL_COMMENT
    | empty

identifier : IDENTIFIER

literal \
    : boolean
    | number
    | STRING
    | NULL

literal_or_identifier \
    : literal
    | identifier

boolean \
    : TRUE
    | FALSE

number \
    : INTEGER
    | REAL

multiplicity_value \
    : INTEGER
    | MULT
\end{verbatim}

\newpage

\section{Files}

@f

\section{Macros}

@m

\section{Definitions}

@u

\end{document}
