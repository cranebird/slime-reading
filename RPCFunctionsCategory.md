## `defslimefun` マクロで定義される全関数(カテゴリー)

### PING (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| PING | (TAG) |

### CONNECTION-INFO (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| CONNECTION-INFO | NIL |

### Evaluation (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| INTERACTIVE-EVAL | (STRING) |
| INTERACTIVE-EVAL-REGION | (STRING) |
| EVAL-AND-GRAB-OUTPUT | (STRING) |
| RE-EVALUATE-DEFVAR | (FORM) |
| PPRINT-EVAL | (STRING) |
| SET-PACKAGE | (NAME) |

## Edit (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| ED-IN-EMACS | (&OPTIONAL WHAT) |
| INSPECT-IN-EMACS | (WHAT &KEY WAIT) |
| VALUE-FOR-EDITING | (FORM) |
| COMMIT-EDITED-VALUE | (FORM VALUE) |

### COMPLETION (swank-c-p-c.lisp)

| Name | Arglist |
| ---- | ---- |
| COMPLETIONS | (STRING DEFAULT-PACKAGE-NAME) |
| COMPLETIONS-FOR-CHARACTER | (PREFIX) |

### ARGLIST (swank-arglist.lisp)

| Name | Arglist |
| ---- | ---- |
| AUTODOC | (RAW-FORM &KEY PRINT-RIGHT-MARGIN) |
| COMPLETE-FORM | (RAW-FORM) |
| COMPLETIONS-FOR-KEYWORD | (KEYWORD-STRING RAW-FORM) |

### Debug (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| BACKTRACE | (START END) |
| SLDB-ABORT | NIL |
| SLDB-BREAK | (NAME) |
| SLDB-BREAK-WITH-DEFAULT-DEBUGGER | (DONT-UNWIND) |
| SLDB-CONTINUE | NIL |
| SLDB-DISASSEMBLE | (INDEX) |
| SLDB-NEXT | (FRAME) |
| SLDB-OUT | (FRAME) |
| SLDB-RETURN-FROM-FRAME | (INDEX STRING) |
| SLDB-STEP | (FRAME) |
| INVOKE-NTH-RESTART | (INDEX) |
| INVOKE-NTH-RESTART-FOR-EMACS | (SLDB-LEVEL N) |
| EVAL-STRING-IN-FRAME | (STRING FRAME PACKAGE) |
| PPRINT-EVAL-STRING-IN-FRAME | (STRING FRAME PACKAGE) |
| THROW-TO-TOPLEVEL | NIL |
| TOGGLE-BREAK-ON-SIGNALS | NIL |
| FRAME-LOCALS-AND-CATCH-TAGS | (INDEX) |
| FRAME-PACKAGE-NAME | (FRAME) |
| SIMPLE-BREAK | (&OPTIONAL (DATUM "Interrupt from Emacs") &REST ARGS) |
| DEBUGGER-INFO-FOR-EMACS | (START END) |
| SDLB-PRINT-CONDITION | NIL |

`sdlb-print-condition` はタイポ?

### Compilation Commands (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| COMPILE-FILE-FOR-EMACS | (FILENAME LOAD-P &REST OPTIONS) |
| COMPILE-FILE-IF-NEEDED | (FILENAME LOADP) |
| COMPILE-MULTIPLE-STRINGS-FOR-EMACS | (STRINGS POLICY) |
| COMPILE-STRING-FOR-EMACS | (STRING BUFFER POSITION FILENAME POLICY) |

### Loading (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| LOAD-FILE | (FILENAME) |

### swank-require (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| SWANK-REQUIRE | (MODULES &OPTIONAL FILENAME) |

### Macroexpand (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| SWANK-MACROEXPAND | (STRING) |
| SWANK-MACROEXPAND-1 | (STRING) |
| SWANK-MACROEXPAND-ALL | (STRING) |
| SWANK-COMPILER-MACROEXPAND | (STRING) |
| SWANK-COMPILER-MACROEXPAND-1 | (STRING) |
| SWANK-EXPAND | (STRING) |
| SWANK-EXPAND-1 | (STRING) |
| SWANK-FORMAT-STRING-EXPAND | (STRING) |

# Simple Completion (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| SIMPLE-COMPLETIONS | (PREFIX PACKAGE) |

# Simple arglist display (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| OPERATOR-ARGLIST | (NAME PACKAGE) |

# Documentation (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| APROPOS-LIST-FOR-EMACS | (NAME &OPTIONAL EXTERNAL-ONLY CASE-SENSITIVE PACKAGE)|
| DESCRIBE-FUNCTION | (NAME) |
| DESCRIBE-INSPECTEE | NIL |
| DESCRIBE-SYMBOL | (SYMBOL-NAME) |
| DOCUMENTATION-SYMBOL | (SYMBOL-NAME) |

# Package Commands (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| LIST-ALL-PACKAGE-NAMES | (&OPTIONAL NICKNAMES) |

# Tracing (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| SWANK-TOGGLE-TRACE | (SPEC-STRING) |

# Undefing (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| UNDEFINE-FUNCTION | (FNAME-STRING) |
| UNINTERN-SYMBOL | (NAME PACKAGE) |

# Profiling (swank.lisp)

| Name | Arglist |
| ---- | ---- |
| TOGGLE-PROFILE-FDEFINITION | (FNAME-STRING) |
| PROFILE-BY-SUBSTRING | (SUBSTRING PACKAGE) |

# Source Locations

| Name | Arglist |
| ---- | ---- |
| FIND-DEFINITION-FOR-THING | (THING) |
| FIND-DEFINITIONS-FOR-EMACS | (NAME) |
| FIND-SOURCE-LOCATION-FOR-EMACS | (SPEC) |
| XREF | (TYPE NAME) |
| XREFS | (TYPES NAME) |

# Inspector

TODO; inspector 以降

### rest

| Name | Arglist |
| ---- | ---- |
| CLEAR-REPL-RESULTS | NIL |
| CLEAR-REPL-VARIABLES | NIL |
| CREATE-REPL | (TARGET &KEY CODING-SYSTEM) |
| DEBUG-NTH-THREAD | (INDEX) |
| DESCRIBE-DEFINITION-FOR-EMACS | (NAME KIND) |
| DISASSEMBLE-FORM | (FORM) |
| EXPORT-STRUCTURE | (NAME PACKAGE) |
| EXPORT-SYMBOL-FOR-EMACS | (SYMBOL-STR PACKAGE-STR) |
| FLOW-CONTROL-TEST | (N DELAY) |
| FUZZY-COMPLETION-SELECTED | (ORIGINAL-STRING COMPLETION) |
| FUZZY-COMPLETIONS | (STRING DEFAULT-PACKAGE-NAME &KEY LIMIT TIME-LIMIT-IN-MSEC)|
| INIT-INSPECTOR | (STRING) |
| INSPECT-CURRENT-CONDITION | NIL |
| INSPECT-FRAME-VAR | (FRAME VAR) |
| INSPECT-IN-FRAME | (STRING INDEX) |
| INSPECT-NTH-PART | (INDEX) |
| INSPECT-PRESENTATION | (ID RESET-P) |
| INSPECTOR-CALL-NTH-ACTION | (INDEX &REST ARGS) |
| INSPECTOR-EVAL | (STRING) |
| INSPECTOR-HISTORY | NIL |
| INSPECTOR-NEXT | NIL |
| INSPECTOR-NTH-PART | (INDEX) |
| INSPECTOR-POP | NIL |
| INSPECTOR-RANGE | (FROM TO) |
| INSPECTOR-REINSPECT | NIL |
| INSPECTOR-TOGGLE-VERBOSE | NIL |
| IO-SPEED-TEST | (&OPTIONAL (N 1000) (M 1)) |
| KILL-NTH-THREAD | (INDEX) |
| LIST-THREADS | NIL |
| LISTENER-EVAL | (STRING &KEY (WINDOW-WIDTH NIL WINDOW-WIDTH-P)) |
| LOOKUP-PRESENTED-OBJECT | (ID) |
| LOOKUP-PRESENTED-OBJECT-OR-LOSE | (ID) |
| MOP | (TYPE SYMBOL-NAME) |
| PACKAGE= | (STRING1 STRING2) |
| PPRINT-INSPECTOR-PART | (INDEX) |
| QUIT-INSPECTOR | NIL |
| QUIT-THREAD-BROWSER | NIL |
| REDIRECT-TRACE-OUTPUT | (TARGET) |
| START-SWANK-SERVER-IN-THREAD | (INDEX PORT-FILE-NAME) |
| TOGGLE-DEBUG-ON-SWANK-ERROR | NIL |
| UNEXPORT-SYMBOL-FOR-EMACS | (SYMBOL-STR PACKAGE-STR) |
| UNTRACE-ALL | NIL |
| UPDATE-INDENTATION-INFORMATION | NIL |

