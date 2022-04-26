# IsabelleDSL
IsabelleDSL (iDSL) is a framework for generating Domain-Specific Languages from specifications written in [Isabelle](https://isabelle.in.tum.de/).

This project is part of my third-year Dissertation Project at the [University of Sheffield](https://www.sheffield.ac.uk/dcs), supervised by [Dr Andrei Popescu](https://www.andreipopescu.uk/)

## Usage

First, define a theory to export. There are example theories in the `Theories` directory. You must supply a file containing a `pp` function for converting sessions into the target language. You do not need to add an `export_code` statement to your theory - this will be automatically added. However, you should add `StringUtils` to the theory's imports. This then allows you to use the functions `string_of_int` etc when writing the `pp` function.

The `pp` function must be of the type `session => String.literal`. The theory file must also define the structure of a `session`, which is essentially a sequence of user actions (function calls) in your DSL. You can then write several different pp functions for different languages.

You must provide a user sessions file, which contains a call to functions in your theory file that is to be pretty printed into code in your target language. Make sure values in the sessions file are cast to their appropriate types, e.g. with `Int_of_integer` (see [here](./Theories/Calculator/user_sessions) for an example).

If you do not specify a `ROOT` file, the script will attempt to create one for you. If you do, it must have an `export_files` statement in it, and import `HOL-Library`. See [ROOT.example](./ROOT.example) for an example `ROOT` file structure.

A boilerplate file must be supplied, and the `SESSIONS[]` placeholder indicates where all sessions will be inserted. Within this, any string can be inserted - within the string, the string `{session}` will be replaced by each pretty-printed user session.

Then run the script, passing the theory file, list of user sessions, and boilerplate code:

```
python3 iDSL_Master.py -t Theories/Calculator/Calculator.thy -l python -s ./Theories/Calculator/user_sessions -b ./Boilerplate/Calculator/calculator_boilerplate_python.txt -f ./PrettyPrinters/Calculator/calculator_pp_python.txt
```

This will create a temporary theory file and ROOT file, if needed, in `/tmp`. It will then build the theory, extract the exported Haskell code, and run the pretty-printing functions within the Haskell file to print the specified session. Boilerplate code will then be added. The final file will be `export.X`, where `X` is the target language file extension, in the output directory specified by `-O` flag.

If you want to see the results of your code export and compare its execution to that of the logically-equivalent haskell file (created as an intermediate step by `export_code` in Isabelle), use the `--auto_test` flag and provide a `--test_string` (representing a Haskell command to run for each session). For example:

```
python3 iDSL_Master.py -t Theories/Calculator/Calculator.thy -l python -s ./Theories/Calculator/user_sessions -b ./Boilerplate/Calculator/calculator_boilerplate_python.txt -f ./PrettyPrinters/Calculator/calculator_pp_python.txt --auto_test --test_string="eval (St (Int_of_integer 0)) (----)"

=== Generating DSL ===

Existing ROOT file found - copying - please make sure this file contains an export_files statement

Building theory file...
Done building

Running Haskell file
Inserting pretty-printed sessions into boilerplate code

=== List of User Sessions (Haskell Syntax) ===

Clear GetResult

=== Test Cases from Exported Haskell Code ===

St (Int_of_integer 0)

=== Results of Exported File ===

0
```

The text `----` will be replaced by each user session string.

### Building PP Functions

You must define a case in your `pp` function for every case in your `session` datatype.

It is worth noting that the order of execution of statements in your Isabelle theory specification must match the order in which it will be interpreted by your target language. For example, in the [stack specification](./Theories/Stack/Stack.thy), the base case is `"'a list" "'a list"` and the `Items` function will be on the 'outside' (left-hand side) of the statement:

```
Items (Pop (Pop (Push (Int_of_integer 4) (Pop (Push (Int_of_integer 3) (Push (Int_of_integer 2) (Push (Int_of_integer 1) [] [])))))))
```

However, the Python code will call `items()` on the final result, so the function call will appear on the right-hand side. This means the `pp` function must append function calls to the right-hand side when recursively evaluating sessions:

```
fun pp :: "session => String.literal" where
"pp (Items ses) = pp ses + STR ''.items()''" |
"pp (Push i ses) = pp ses + STR ''.push('' + (string_of_int i) + STR '')''" |
"pp (Pop ses) = pp ses + STR ''.pop()''" |
"pp llist rlist = (string_of_int_list llist) + (string_of_int_list rlist)"
```

### Troubleshooting

Verbose mode (`-v`) shows a fair amount of extra debugging information:

```
mac@mac-ubuntu:~/Documents/Diss/IsabelleDSL$ python3 iDSL_Master.py -t Theories/Stack/Stack.thy -l c -s ./Theories/Stack/user_sessions -b ./Boilerplate/Stack/stack_boilerplate_c.txt -f ./PrettyPrinters/Stack/stack_pp_c.txt -v

=== Generating DSL ===

Existing ROOT file found - copying - please make sure this file contains an export_files statement
Modified theory file created at /tmp/Stack.thy

Building theory file...
Build results:

----
Build started for Isabelle/Stack ...
Running Stack ...
Stack FAILED
(see also /home/mac/.isabelle/Isabelle2021-1/heaps/polyml-5.9_x86_64_32-linux/log/Stack)
*** Extra type variables on right-hand side: "'a"
*** At command "datatype" (line 22 of "/tmp/Stack.thy")
Unfinished session(s): Stack
----
```

## Notes for Myself - How to Manually Interact with System

### Building Manually

Once an `export_code` command has been added to a `.thy` file, it can be built manually with:

`isabelle build -e -D .`

The first time you run this, `HOL-Theory` will need to build. This can take some time depending on the speed of your computer, but should only need to be done once.

### Accessing Exports

To access the exports from an Isabelle file, run:

```
$ isabelle export -l -d . -x "*:**.hs" [SESSION_NAME]
```

From within the directory with the `ROOT` file. The root file must contain the line `export_files (in ".") "*:**.hs"`.

This should export the haskell code to the export folder, e.g. for Calculator theory: `./export/Calculator.Calculator/code/calculator`

### generatedHaskellFiles

This directory has files generated by the Isabelle `build` command. The framework exports the `pp` function, which pretty prints an aspect of the system, and any of its dependencies.

To run the Haskell file manually:

```bash
$ ghci
Prelude> :load Calculator.hs
[1 of 1] Compiling Calculator       ( Calculator.hs, interpreted )
Ok, one module loaded.
*Calculator> pp GetResult
".getResult()"
*Calculator> pp (Clear GetResult)
".clear().getResult()"
*Calculator> pp (Add (Int_of_integer 5) (Sub (Int_of_integer 4) (Div (Int_of_integer 4) (GetResult))))
".add(5).sub(4).div(4).getResult()"
```
