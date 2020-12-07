# Col-Advent-Of-Code-2020

Repo containing the challenges for the Cave of Linguists - Tech Club Advent of Code 2020

## Repo structure

All the code is in `src/`, one file per challenge. In the root there are some `.asd` files
to compile the code in each challenge as an executable.  
Ignore the `tests/` folder for now, I'd like to have real tests instead of a main but the testing
framework I picked doesn't want to cooperate :(.

## How to use this stuff

These instructions allow you to set up an environment to run the code here.

### If nothing of the stuff below works

You can always run Common Lisp code online on websites like [this one](https://www.tutorialspoint.com/execute_lisp_online.php) and [this one](https://rextester.com/l/common_lisp_online_compiler).  
You'll need to copy-paste the code from the `.lisp` files in the sources though.
The thingies starting with `defun` are functions, copy the ones you need and invoke them
like this:

`(function-name arg1 arg2 ...)`

This should be a last-resort measure. I tried to organize the repo so that you can basically compile the code
of a challenge and run it as a normal executable, which is way better than copy-pasting code around.

### Installing a Common Lisp implementation

You'll need to install a Common Lisp implementation. I used [sbcl](http://www.sbcl.org/)
but any other implementation should work.  
Apparently the easy way to work with Common Lisp is to get a whole blob of stuff that should be
self-contained and ready to run. People recommend using [Portacle](https://portacle.github.io/)
which is available for Linux, Windows and Mac. It contains `sbcl`, Quicklisp and Emacs all ready
to be used.

The alternative is to install each piece manually.  
On Linux you'll probably find `sbcl` available in your package manager. If you want to
edit/play with the code, you'll probably want to use `emacs` with `SLIME` (something similar
is available for `vim`).  
On Windows, people still recommend using [Portacle](https://portacle.github.io/) as a complete
IDE for Common Lisp, because it's probably way more painful to set up the whole thing on Windows.

### Terminology

A few quick definitions to explain what we'll do next:

- A **package** is a group of symbols. Basically like a Python module, or a C++ namespace.
- A **system** is a collection of source files bundled with an `.asd` file that tells you how
  to compile and load them. You can think of a system as a "compilation target" or something like
  that.

Here I have a `.asd` for each challenge. Each of these defines a system for a challenge, which will
become an executable for each challenge. To build these systems we'll need to install a few more things.

### Installing Quicklisp

**You don't need to do this if you're using Portacle. Reading this section might be informative though.**

Quicklisp is roughly the Common Lisp equivalent of `pip` or `npm`: it allows you to load a system and
its dependencies. We'll need it to load and compile our stuff.

To install it you can run this (assuming you're using `sbcl` as your Common Lisp implementation):

```
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
```

You should now be in a Common Lisp REPL. Here you can actually install quicklisp like this:

```
(quicklisp-quickstart:install)
```

This will allow you to load a project with little to no effort if you place it in one of these
folders:

- `~/common-lisp`
- `~/.local/share/common-lisp/source`
- `~/quicklisp/local-projects`

I'm pretty sure Portacle will tell you where to put your projects to make sure it can find them.

### TL;DR: compile a challenge like this

If you want to build the challenge "day-1" for example, you can run this:

```
sbcl --load day-1.asd --eval '(ql:quickload :day-1)' --eval '(asdf:make :day-1)' --eval '(quit)'
```

This will compile the code for the first challenge as an executable called "day-1" or "day-1.exe".
This executable will load all the functions in `src/day-1.lisp` and run the function `-main`. The program
ends when `-main` returns.
If you want to change a challenge's input, either modify the function calls in `-main` or read the
next section.

In theory I'd have some tests, but I can't figure out how to run them properly, but compiling and running
an executable is basically the same, that will do for now.

The other challenges will have the same name with a different number. I'm sure you can figure out how
to adapt the line above to test each challenge.

### Loading a challenge

If the TL;DR above wasn't enough, here are some instructions on how to run the code and work with it.  
Open a REPL (Read-Eval-Print Loop) like this:

`sbcl`

or run a REPL with Portacle to have a more comfortable experience :).

Assuming this project has been cloned in one of the folders that Quicklisp knows, you should be able
to load a system called "day-1" like this:

`(ql:quickload :day-1)`

If that fails, you can try to load the `.asd` file first:

```
(load "path/to/day-1.asd") ;; Load the system definition
(ql:quickload :day-1)      ;; Let Quickload actually load the system
```

Now you should be able to call the functions you can see in the file.
Functions need to be called with their package name unless you use `use-package` like this:

`(use-package :day-1)       ;; Enter the package so you can use its functions`

if you don't do that, you'll need to use functions like this:

`(package-name:function-name function-arguments)`

instead of this:

`(function-name function-arguments)`

I can't explain how the whole language works (I'm still learning) so if you want to experiment
with the code I made I'd recommend you to read something quick about Common Lisp's syntax and rules
like ["Learn X in Y minutes - Where X = Common Lisp"](https://learnxinyminutes.com/docs/common-lisp/).
For something more in-depth, there are books like ["Practical Common Lisp"](http://www.gigamonkeys.com/book/)
