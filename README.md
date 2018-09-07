# mext: Maxima package manager and third party packages

## Summary

This repository contains the mext packaging system and packages for
the Maxima computer algebra system. I wrote this because Maxima by
itself has no system for managing third-party packages (or packages in share or contrib).
Features missing in stock Maxima that are provided here include:

* installation of code to canonical locations.
* namespaces
* canonical organization of tests
* specifying dependencies that are loaded automatically
* compiling dependencies only once rather than repeatedly
* loading dependencies once rather than repeatedly
* managing multiple versions of maxima and multiple lisp implementations
* many others

Much of the `share`, `contrib`, and third-party code is packaged for `mext`.

I wrote this package manager because it is necessary to support
several packages that I wrote: `defmfun1`, `aex`, and other packages that depend on
these. These pacakges are included in this repository.

Included here are also several "packages" from the Maxima
third party code repository adapted to use this packaging system.
Until now, there has been no system to prevent symbol collisions, manage versions,
etc. A few `mext` packages were made and are included here from code in the Maxima share and contrib folders.
(Currently, we refer to all
of these as 'distributions' rather than 'packages')

### Maxima version compatibility

If you have any difficulty, please [file an issue](https://github.com/jlapeyre/mext/issues).

* Maxima 5.41.0 compiled with sbcl 1.4.6:  230 tests failed out of 1,302 total tests.

When first written, mext supported many lisp implementations and Maxima versions. Changes
in both of these may cause failures. mext *does* support Maxima 5.41.0 compiled with sbcl 1.4.6.
The latest versions of gcl are not yet supported due to changes in gcl. Other lisp implementations
have not been tested recently.

### Installation

For installation instructions, see the files [doc/INSTALL](doc/INSTALL)
and [doc/INSTALL-windows.txt](doc/INSTALL-windows.txt)

<!-- Zipped prebuilt installations for windows can be found at -->
<!-- http://www.johnlapeyre.com/mext_windows/ -->

In linux, you can install to your `.maxima` directory by running
`./install_mext.sh maxima`
where maxima is the name of your maxima program. Read the comments
at the top of the file [install_mext.sh](install_mext.sh).

### Uninstall

To remove the mext system and all pacakges, delete the files and
the single folder in your home .maxima directory or maxima folder
that begin with `mext`.

### Testing

See [bin/0README_bin.md](bin/0README_bin.md) for testing the mext system
and packages for several versions of Maxima simultaneously. Scripts for
building the documentation are also in the [bin/](bin/) directory.

### Usage

For a brief introduction to using mext, see `doc/USAGE`.

### Summary of features

#### mext, a system for standardized compilation, installation, loading of packages.

* Minimizes recompilation and reloading. In stock Maxima, code is sometimes recompiled every
 time it is loaded for use.

* Minimizes filename collision between packages.

* Standardized specification of dependencies. Dependencies are
  loaded only once.

* Separate (binary and source) installation trees for different Maxima versions and lisp vendors and versions.

* Basic system functions, for example: `chdir, list_directory, pwd, popdir, updir, dir_exists`, etc.

* Package management and information on installed and loaded packages: `mext_list, mext_list_loaded, mext_info`.

* Run package tests (rtests) with `mext_test(packname)`. rtests are installed in a
  standard location.

* Supports installation of stock Maxima `share` packages to the mext system
  directly from the Maxima installation. This allows the packages to
  be compiled (once) and installed for multiple Maxima/lisp versions
  and to be loaded (once) as dependencies, and to survive `kill(all)`, and
  to be tested quickly and easily. See for example [packages/grobner](packages/grobner).

* After mext is installed: Download third party mext package
  'packname' and unzip.  Do `load("packname/ibuild.mac")` to build and
  install software and rtests to a directory automatically named for
  the running Maxima/lisp. Do `mext_test(packname)` to run installed tests.
  Packager specifies which files are compiled or left as source, etc.
  Currently, all but one third party package is in mext repository.

#### Documentation systems

* Multiple documentation systems are provided and integrated for access via `describe` and `??`.  These do
 not require one to use `texinfo`, which removes a large barrier for writing docs.

* Two documentation systems for user code are provided: one simple, and one more full-featured.

* A documentation system for internal/devel documents is provided.

* Easy mechanism to dynamically include/exclude these documentation databases from ??.

* Examples are integrated into the documentation system.

* Many ways to enter examples: Examples are not executed before displaying;
  or are executed but with variables transparently localized.
  (stock Maxima does not localize variables for examples.)

* Macros are provided for user and internal functions and variables to automatically
  generate documentation that is accessible immediately with ??.

* Automatic html and pdf documents generated from documentation.

#### defmfun1

`defmfun1` is a lisp macro for writing Maxima functions in lisp.
It provides a lot of standardization and functionality:

* Automatic (optional) detailed argument checking.

* Standardized option passing: `opt->val`. Automatic, standarized
  extraction, of required, optional, and option arguments.

* Automatically (optionally) generate documentation using
  various sources (such as argument checks.)

* Standardized, automatic, error messages. No extra coding required.

* Support "attributes": e.g. for each function to switch between raising
  error or returning input form, or printing warning. Attributes also
  control argument evaluation. This is modeled on Mathematica.

* Option `compile` to automatically compile lambda functions passed
  as arguments. Used in many functions listed below.

#### quicklisp

A Maxima interface to quicklisp. This is used by other packages and is
also available to users.

#### store

Provides serialization of Maxima objects via an interface to the lisp library `store`.
The library `store` is installed via quicklisp.

#### aex

`aex` provides expressions stored as lisp arrays, rather than lisp lists.

* The main advantage is that random access is `O(1)`, rather than `O(n)`.

* Many functions based on `aex` are included in mext packages. These usually use standard
  lisp lists for input and output, unless aex is requested. A main goal
  is to code for and to test for efficiency.

    * `lrange` -- list of range of numbers.
    * `table` -- defmfun1 wrapper around Ziga Lenarcic's code.
    * `constant_list, count, drop_while, fold, fold_list,
      length_while, nest, nest_list, nest_while, nreverse, partition_list
      select, take, take_while, tuples, string_drop, string_take,
       with_output_to_string`

  Some replacements of Maxima functions. These replacements are less generally
  applicable, but are much more efficient in some cases:
  `icons` (`cons`), `imap` (`map`), `every1` (`every`).

#### discrete_aex

Efficient routines for:

`from_digits,integer_digits,integer_string,
ae_random_permutation,cycles_to_perm,
inverse_permutation,perm_to_cycles,perm_to_transpositions,
permutation_p,random_cycle,random_permutation_sym,
signature_permutation,transpositions_to_perm`

#### numerical

* `nintegrate` -- currently a front end to `quadpack`.  Subdivides
  interval, calls quadpack routines, combines results. Finds some
  singularities and passes information to quadpack. Identifies and
  separately integrates real and imaginary parts.

#### Notes on lisp versions, interaction with Maxima

Much of the third party software packaged here uses the maxdoc
documentation system, with documentation embedded in the source code.
This allows printing online documentation via `?` and `??`.  A latex
translator is also available. A texi translator is partially done.
See the output in the files thirdparty.{tex,pdf,html}. The latex file
is created by loading mext packages and then giving the command
`print_sections_latex();` to print the documentation for all loaded
packages.

The mext system packages the distributions in a
uniform way. They are built and installed using only Maxima,
rather than depending directly on any system tool. (There are
some shell/perl scripts to automate testing with several lisps
under linux, but this is not part of the system.)

Most or all of this has been found to work on at least one
platform running Maxima with:
linux gcl, sbcl, ccl, clisp, cmucl, ecl; win32 gcl, ccl.

This code is an "add-on" to Maxima. But it overwrites some Maxima code (Only when you load
it. It does not touch your Maxima installation!) The redefinitions are
isolated in two files `maxdoc/descr1.lisp` and `aex/system-essential.lisp`. Much of
this could be included in stock Maxima without breaking the test-suite,
but for the time being, I decided to overwrite stock Maxima only
when it is essential.

#### Acknowledgments

Authors of third-party code in this repository are credited in the
package folders and the files contained therein. A lot of invaluable
advice and code snippets were provided by Maxima developers. In
particular, Robert Dodier, Richard J Fateman, Stavros Macrakis, and
Barton Willis.

#### Features in detail

* Only one subdirectory of the user directory is added to the Maxima
search paths. For each distribution, there is one file in this
directory that loads code. The code for each distribution, as well as
rtests, etc. are located in further subfolders that are not added to
the Maxima search path by default. This helps to reduce filename
collisions.  Actually the subdirectory that is put in the search path
is named for the Maxima version and the lisp implementation and the
lisp version. So, different Maxima and lisps can be used in
parallel. This system is set up with `load('mext);`.

* A distribution can be loaded with `require('distname);` or
`require("distname");` The distribution is registered for the session
and subsequent calls to `require('distname)` do nothing. Clobbering
by `kill(all)`, has been addressed. See `?? dont_kill`.

* Installing binary only, source only, or a combination is
supported. In stock Maxima, there is often a good amount of reloading
and recompiling every time a user loads code. The mext system and
packages attempt to compile once and load compiled code when possible,

* Installation and running of rtests is standardized:
`mext_test(dist1,dist2);`

* Building and installation is done mostly with defsystem. There are
some other parts. Much of the distribution definition follows
defsystem or asdf exactly. This might make it easier to move to asdf
in the future.

* Building from the Maxima share and contrib directories is supported.
That is, a mext definition is written that optionally compiles and
installs source from the Maxima share directory to the user mext
directory.

* Functions for dealing with pathnames, files, and directories are
included in mext. This includes `cl-fad`, a portable pathname library,
hacked to support gcl somewhat. Some functions in gcl are missing, not
ansi-compliant, or broken. (but, I found an error reported by gcl that 5 other
lisp implmentations missed!)  mext uses an interface to many of the ansi pathname
functions and `cl-fad` functions, implemented as functions with the ansi
name preceded with an 'f'.  For behavior not specified by the
standard, I standardized on sbcl. Most of these functions for most
lisps call the standard function. The result is not well tested, but
currently works for installing distributions with six lisp
implementations under linux and gcl and clozure lisp under win32.

* I tried as much as possible to keep the pathname code portable. That
is, I avoided '.\/'. This makes the maintainer level code, i.e. calls
to provide and require a bit more verbose, as paths are specified as
lists rather than strings with directory separators.

* Much of the underlying code and more seems to be provided as well by
asdf and related tools. But, the code in mext serves as a stop-gap as
long as Maxima on gcl/win32 is widely used because asdf and gcl are incompatible.

#### Examples

Below are some examples (with some
lines edited out.) Note that the key `:maintainer` below is used
in the sense of a linux package maintainer or asdf maintainer:
It refers to the person who packaged the code with the packaging
system.

There is a mext package called `mext_defmfun1`. This redefines some
of the mext functions with documentation and error checking. If this
package is installed, then online help is available via `?` for
`chdir, mext_test, mext_list, mext_info, truename, probe_file, and
pwd`. If you install `mext_defmfun1`, then `?? runtime` will give
documentation for some functions.

```maxima
(%i1) showtime:true$
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i2) load(mext);
Evaluation took 0.0300 seconds (0.0330 elapsed) using 4.570 MB.
(%o2)                  /home/jlapeyre/.maxima/mext.lisp
(%i3) require(aex);
loading /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/aex.mac
loading /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/defmfun1.mac
loading /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/maxdoc.mac
Evaluation took 0.4800 seconds (0.4680 elapsed) using 92.449 MB.
(%o3)  /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/aex.mac
(%i4) require(aex);
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%o4)                                true
(%i5) require(nelder_mead);
loading /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/nelder_mead.mac
Evaluation took 0.0600 seconds (0.0550 elapsed) using 11.157 MB.
(%o5) /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/nelder_mead.mac
(%i6) require(pw);
loading /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/pw.mac
Evaluation took 0.8900 seconds (0.9070 elapsed) using 135.455 MB.
(%o6)  /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/pw.mac
(%i7) mext_info(aex);
 Name: aex
 Description: array representation for maxima expressions
 Version: 0.0.1
 Author: John Lapeyre
 License: GPL
 Maintainer: John Lapeyre
(%o7)                                done
(%i8) mext_info(nelder_mead);
 Name: nelder_mead
 Description: Nelder-Mead optimization algorithms
 Version:
 Author: Mario S. Mommer
 License: in_dist
 Maintainer: John Lapeyre
(%o8)                                done
(%i9) mext_info(pw);
 Name: pw
 Description: functions for symbolic work with piecewise functions
 Version: 6.5
 Author: Richard V. Hennessy <rich.hennessy@verizon.net>
 License: GPL2
 Maintainer: John Lapeyre
(%o9)                                done
(%i10) mext_test(aex);
Running tests in /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/aex/rtests/rtest_take.mac: 30/30 tests passed
Running tests in /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/aex/rtests/rtest_table.mac: 2/2 tests passed
Running tests in /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/aex/rtests/rtest_list.mac: 9/9 tests passed
Running tests in /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/aex/rtests/rtest_afuncs.mac: 2/2 tests passed
No unexpected errors found out of 43 tests....
(%o10)                               done
(%i11) mext_test(nelder_mead);
No unexpected errors found out of 0 tests.
(%o11)                               done
(%i12) mext_test(pw);
Running tests in /home/jlapeyre/.maxima/mext/v5.28.0-sbcl-v1.0.57.0.debian/pw/rtests/rtest_pw.mac: 226/226 tests passed
No unexpected errors found out of 226 tests.
(%o12)                               done
(%i13) pwd();
(%o13)                    /home/jlapeyre/maxima/mext/
(%i14) list_directory();
          ....
(%i15) ?? nelder

------------------------------------------------
 -- Function: nelder_mead: nelder_mead(<expr>, <vars>, <init>)
    Section: Functions and Variables for Equations

Description:
   The Nelder-Mead optimization algorithm.
Arguments:
   nelder_mead requires three arguments.
    The second argument <vars> must be a list of symbols.
    The third argument <init> must be a list of numbers.

Examples:

   Find the minimum of a function at a non-analytic point.
(%i1) nelder_mead(if x<0 then -x else x^2, [x], [4]);
(%o1) [x = 9.536387892694629e-11]
(%i1) f(x) := if x<0 then -x else x^2$
(%i2) nelder_mead(f, [x], [4]);
(%o2) [x = 9.536387892694628e-11]
(%i3) nelder_mead(f(x), [x], [4]);
(%o3) [x = 9.536387892694628e-11]
(%i1) nelder_mead(x^4+y^4-2*x*y-4*x-3*y, [x,y], [2,2]);
(%o1) [x = 1.157212489168102,y = 1.099342680267472]


  Author: Mario S. Mommer.

-------------------------------------------------------------------
-------------------------------------------------------------------
```

<!-- LocalWords:  mext maxima defmfun aex contrib perl linux gcl sbcl ccl -->
<!-- LocalWords:  clisp cmucl ecl subdirectory rtests subfolders distname -->
<!-- LocalWords:  defsystem asdf pathnames pathname ansi clozure chdir pwd -->
<!-- LocalWords:  truename runtime nelder pw Lapeyre GPL Mommer piecewise -->
<!-- LocalWords:  expr init maxdoc texi thirdparty tex pdf html popdir dir -->
<!-- LocalWords:  updir packname filename grobner devel lrange Ziga tuples -->
<!-- LocalWords:  Lenarcic's nreverse imap ae sym nintegrate quadpack dont -->
<!-- LocalWords:  redefinitions -->
