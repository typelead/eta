# Reference

## Commands available from the prompt

| Command                                | Description                                                                                       |
| ------------------------------         | ----------------------------------------------------                                              |
| `(statement)`                          | Evaluate/run (statement)                                                                          |
| `:`                                    | Repeat last command                                                                               |
| `:{\n ..lines.. \n:}\n`                | Multiline command                                                                                 |
| `:add [*](module) ...`                 | Add module(s) to the current target set                                                           |
| `:browse[!] [[*](mod)]`                | Display the names defined by module (mod) (!: more details; *: all top-level names)               |
| `:cd (dir)`                            | Change directory to (dir)                                                                         |
| `:cmd (expr)`                          | Add module(s) to the current target set                                                           |
| `:complete (domain) [(range)] (input)` | List completions for partial input string                                                         |
| `:ctags[!] [(file)]`                   | Create tags file (file) for Vi (default: "tags") (!: use regex instead of line number)            |
| `:def (cmd) (expr)`                    | Define command :(cmd) (later defined command has precedence, ::(cmd) is always a builtin command) |
| `:edit (file)`                         | Edit file                                                                                         |
| `:edit`                                | Edit last module                                                                                  |
| `:etags [(file)]`                      | Create tags file (file) for Emacs (default: "TAGS")                                               |
| `:help, :?`                            | Display this list of commands                                                                     |
| `:info[!] [(name) ...]`                | Display information about the given names (!: do not filter instances)                            |
| `:issafe [(mod)]`                      | Display safe haskell information of module (mod)                                                  |
| `:kind[!] (type)`                      | Show the kind of (type) (!: also print the normalised type)                                       |
| `:load[!] [*](module) ...`             | Load module(s) and their dependents (!: defer type errors)                                        |
| `:main [(arguments) ...]`              | Run the main function with the given arguments                                                    |
| `:module [+/-] [*](mod) ...`           | Set the context for expression evaluation                                                         |
| `:run function [(arguments) ...]`      | Run the function with the given arguments                                                         |
| `:reload[!] `                          | Reload the current module set (!: defer type errors)                                              |
| `:script (file)`                       | Run the script (file)                                                                             |
| `:type (expr)`                         | Show the type of (expr)                                                                           |
| `:unadd (module) ...`                  | Remove module(s) from the current target set                                                      |
| `:undef (cmd)`                         | Undefine user-defined command :(cmd)                                                              |
| `:!(command)`                          | Run the shell command (command)                                                                   |

## Commands for changing settings

| Command                            | Description                                          |
| ------------------------------     | ---------------------------------------------------- |
| `:set (option) ...`                | Set options                                          |
| `:seti (option) ..`                | Set options for interactive evaluation only          |
| `:set args (arg) ...`              | Set the arguments returned by System.getArgs         |
| `:set prog (progname)`             | Set the value returned by System.getProgName         |
| `:set prompt (prompt)`             | Set the prompt used in Eta REPL                      |
| `:set prompt-cont (prompt)`        | Set the continuation prompt used in Eta REPL         |
| `:set prompt-function (expr)`      | Set the function to handle the prompt                |
| `:set prompt-cont-function (expr)` | Set the function to handle the continuation prompt   |
| `:set editor (cmd)`                | Set the command used for `:edit`                     |
| `:set stop [(n)] (cmd)`            | Set the command to run when a breakpoint is hit      |
| `:unset (option) ...`              | Unset options                                        |

## Options for ':set' and ':unset':

| Command                        | Description                                                                                                                                                                   |
| ------------------------------ | ----------------------------------------------------                                                                                                                          |
| `+m`                           | Allow multiline commands                                                                                                                                                      |
| `+r`                           | Revert top-level expressions after each evaluation                                                                                                                            |
| `+s`                           | Print timing/memory stats after each evaluation                                                                                                                               |
| `+t`                           | Print type after evaluation                                                                                                                                                   |
| `+c`                           | Collect type/location info after loading modules                                                                                                                              |
| `-(flags)`                     | Most Eta command line flags can also be set here (eg. -v2, -XFlexibleInstances, etc.) for Eta REPL-specific flags, see User's Guide, Flag reference, Interactive-mode options |

## Commands for displaying information

| Command                        | Description                                                         |
| ------------------------------ | ----------------------------------------------------                |
| `:show bindings`               | Show the current bindings made at the prompt                        |
| `:show imports`                | Show the current imports                                            |
| `:show linker`                 | Show current linker state                                           |
| `:show modules`                | Show the currently loaded modules                                   |
| `:show packages`               | Show the currently active package flags                             |
| `:show paths`                  | Show the currently active search paths                              |
| `:show language`               | Show the currently active language flags                            |
| `:show targets`                | Show the current set of targets                                     |
| `:show (setting)`              | Show value of (setting), which is one of [args, prog, editor, stop] |
| `:showi language `             | Show language flags for interactive evaluation                      |
