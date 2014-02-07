% LESSCASE(1) lesscase user manual
% Stefan Berthold <stefan.berthold@gmx.net>
% January 28, 2014

# NAME

lesscase - a LaTeX pre-processor for formatting uppercase letters

# SYNOPSIS

lesscase [*options*] [*file*]

# DESCRIPTION

lesscase is a pre-processor for LaTeX files. It formats sequences of uppercase letters, e.g., ABC, with TeX's `\small` macro, e.g., `{\small ABC}`. Uppercase letters at the beginning of sentences are not formatted. The LaTeX macro can be chosen by the user as well as the formatting conditions.

lesscase can be configured through its command line arguments as well as through TeX comments. In the default configuration, lesscase does not change the  content of  any  macro  argument  or  environment  (different  from document). More (`conservative`) and less restrictive configuration profiles (`busy`) can be activated and adapted. The default configuration can be restored at any time, even while processing of the input file.

# EXAMPLES

`lesscase`
:   Runs lesscase in filter mode.

`lesscase letter.tex`
:   Processes `letter.tex` with all inline configurations and replaces the file with the program output.

`lesscase --macro-arg='\\lesscase' letter.tex`
:   Processes `letter.tex` as above, but uses the macro `\lesscase` instead of the default `\small` for formatting. The uppercase letters that are to be formatted are passed as argument to the macro.

`lesscase -x ".!?:"`
:   lesscase uses the colon as the end of a sentence in addition to the period, the exclamation mark, and the question mark. Useful in German texts.

# OPTIONS

\--prefix=*path*
:   Use *path* as a prefix for all output files. The input files are not altered. When you use a prefix, the *file* argument needs to be a relative path.

-r, \--recursive
:   Follow recursively `\include` and `\input` statements in the LaTeX source code.

\--no-inline
:   Ignore configuration statements embedded in the LaTeX source code.

-p *name*, \--profile=*name*
:   Load a specific start configuration.
    Use `conservative` to start with a very restrictive configuration.
    Use `busy` to start with a configuration that enters all LaTeX macros and environment.
    Use `clean` to switch off all functionality of lesscase. You can switch it on using inline configuration later in the LaTeX file.

-x *chars*, \--periods=*chars*
:   Use a number of characters that mark the end of a sentence. *chars* is a character string, no comma or other seperator is needed.
    (Default: "`.!?`")

-m *code*, \--macro=*code*
:   Use the macro *code* for formatting uppercase letters. The macro *code* will be put in its own LaTeX block, surrounded by a pair of `{}`-braces.
    (Default: "`\small`").

-M *code*, \--macro-arg=*code*
:   Similar to `-m`, but passes the letters as argument to the macro *code*.

-s *list*, \--search=*list*
:   A list of LaTeX macros and environments that will be processed by lesscase.
    If *list* begins with `+` or `-`, the comma-separated macros and environments will be added or removed from the internal search list, respectively.
    If *list* begins with `*` or `/`, the list is reset to all or none macros and environments, respectively.
    The *list* has to start with one of the four operators.
    Macro names start with a backslash, environment names do not have any prefix.
    (Default: "`+ document`")

-i *list*, \--isolate=*list*
:   A list of LaTeX macros and environments that will be processed separately from the rest of the text.
    The *list* works as in `--search`.
    (Default: "` + \footnote, \marginpar`")

-S *list*, \--skip=*list*
:   A list of LaTeX macros and environments that forces lesscase to ignore the rest of the LaTeX block. It can be used to avoid double processing of text.
    The *list* works as in `--search`.
    (Default: "`+ \tiny, \scriptsize, \footnotesize, \small, \large, \Large, \LARGE, \huge, \Huge`")

-u *list*, \--unskip=*list*
:   A list of LaTeX macros and environments that undo `--skip`.
    The *list* works as in `--search`.
    (Default: "`+ \normalsize`")

-e *list*, \--eos=*list*
:   A list of LaTeX macros and environments that mark the end of a sentence.
    The *list* works as in `--search`.
    (Default: "`+ \par, \part, \chapter, \section, \subsection, \subsubsection, \paragraph, \include, itemize, enumerate, description`")

# INLINE CONFIGURATION

The following LaTeX comments can be embedded into LaTeX source code and will reconfigure lesscase while it is parsing the LaTeX source.

`% smallcaps reset profile` *name*
:   This comment loads a configuration profile as in the program option `--profile`. Use the `default` as *name* to restore the default configuration.

`% smallcaps restore profile` *name*
:   Same as "`% smallcaps reset profile` *name*".

`% smallcaps store profile` *name*
:   This comment stores the current configuration profile as *name*. Existing profiles will be overwritten. Note that you can very well overwrite the initial configuration profiles, such as `default`.

`% smallcaps periods are` *chars*
:   This comment sets the characters that would end a sentence, as in the program option `--periods`.

`% smallcaps substitution in block with` *macro*
:   This comment resets the formatting macro, as in the program options `--macro`.

`% smallcaps substitution as argument of` *macro*
:   This comment resets the formatting macro, as in the program options `--macro-arg`.

`% smallcaps search` *list*
:   This comment modifies the search list.
    Use the *list* as in the program option `--search`.

`% smallcaps isolate` *list*
:   This comment modifies the isolation list.
    Use the *list* as in the program option `--isolate`.
    In addition, you may add a profile name as a prefix for a "`+`" list or a "`*`". The isolated processing will start with this profile for the named macros and environments. If no profile name is given, "`default`" will be used.

`% smallcaps skip` *list*
:   This comment modifies the skip list.
    Use the *list* as in the program option `--skip`.

`% smallcaps unskip` *list*
:   This comment modifies the unskip list.
    Use the *list* as in the program option `--unskip`.

`% smallcaps eos` *list*
:   This comment modifies the end of sentence list.
    Use the *list* as in the program option `--eos`.

# BUGS

lesscase is known to ignore the tab characters (&) in tabular environments.

lesscase does not know about the somewhat more flexible delimiters of verbatim macros (`\verb`).
