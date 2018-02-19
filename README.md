# QFMtool
A tool to evaluate fuzzy quantified expressions using Quantifier Fuzzification Mechanisms (QFM).

![qfmtool1](https://user-images.githubusercontent.com/36513234/36345491-df3490c8-142b-11e8-8055-100396611e1a.png)##

## List of implemented QFMs
 - Three choice models from Ingo Glöckner (http://www.springer.com/de/book/9783540296348)
 - Four models from Diaz-Hermida et al. (http://ieeexplore.ieee.org/abstract/document/1516158/, https://www.sciencedirect.com/science/article/pii/S016501140400096X)
 - One QFM based on representation levels from Sanchez et al. (https://www.sciencedirect.com/science/article/pii/S0165011411003186)
 - A "closeness-based" QFM model from Baldi and Fermüller (https://link.springer.com/chapter/10.1007/978-3-319-66830-7_11)
 
__Note:__ Using the plotting tool requires a folder "Plots" to be present in the same directory. A copy of this is provided in the repository, and the plots can be rendered manually by installing the QFM library and running the function "generate_svgs". Instructions for this optional step are given below.

## Syntax of queries

Todo

## Download

On the [Release page](https://github.com/cem-okulmus/QFMtool/releases/tag/1.0)


## Build instructions

__Needed tools__:   ocaml, opam

__Needed libraries__: lablgtk2 (including lablgtk2.rsvg), (optionally: plplot)


### On Linux and MacOS

_Note on MacOS: _In principle_ this should be identical, as MacOS is natively supported by the OCaml developer, INRIA. However, I have no MacOS machine available to make sure things work as described here. In a perfect world, it should behave identical to how it does on Linux, though._ 

0. __Make sure that the needed tools and libraries are installed.__ 

On Linux just use the packaging system of your distro of choice, on MacOS [a few options are available](https://ocaml.org/docs/install.html#macOS). __Note:__ The corresponding developement packages (-dev, -devel, etc.) _have_ to be installed as well.

1. __Install Oasis.__ 

The build process uses Oasis, a build manager for Ocaml. Install it through opam by typing `opam install oasis`.

2. __Install needed OCaml libraries.__ 

The following libraries need to be installed (through opam). 

  - OCaml Batteries Included `opam install batteries`
  - OCaml GTK2 bindings `opam install lablgtk2` (__Note:__ if lablgtk2.rsvg was not already installed during step 0, then no bindings will be created, which are needed to succesfully build QFMtool)
  
Optionally, [Plplot](http://plplot.sourceforge.net/) can also be installed. To use this in the QFM library, the \_oasis file has to slightly changed, as it doesn't install this by default, as it is not needed. In "BuildDepends:", "plplot" must be added, and in "Modules:",  "Plot" must be added. This _must_ be done before running `oasis setup`. Otherwise, it must be rerun. 
  
3. __Using Oasis to build/install QFM__

 Open a terminal, and set the working directory to be where the \_oasis file is located. 
 
Then type `oasis setup && make && make install`
This will first set up the build process, and then proceed to build and install the QFM library. 

These can then be imported on the Ocaml toplevel. The QFM folder should also have a ".ocamlinit" file, so just typing 
"ocaml" here should already load the library. 

There is a build script to build the QFMtool UI (the Linux version should work on MacOS too), but compiling  requires local installation of a few libraries, beyond the ones mentioned here (no catch-all for all environments, read the error-messages and make sure to have everything needed for linking, _should_ be fairly straightforward). 

### On Windows

1. __Set up the OCaml environment.__

Install the "graphical installer" [from here](https://fdopen.github.io/opam-repository-mingw/installation/) (32-bit recommended 
for compatability purposes, maybe this gets better in time) The installer should be straightforward to use. It will setup Cygwin
and everything else needed. 

2. __Install needed libraries.__

First install depext and depext-cygports through opam 

  - `opam install depext` 
  - `opam install depext-cygports` 
  
__Note:__ make sure to add "/usr/i686-w64-mingw/sys-root/mingw/bin" to the PATH, as recommended by the last command.

Afterward, the following commands should install all that is needed: 
  - `opam install oasis`
  - `opam depext -i lablgtk` 
  - `opam install batteries`
  
3. __Using Oasis to build/install QFM (and the UI)__

Go to the directory with the src folder and \_oasis file. 

Then,

  - `oasis setup`  (to set up the make environment) 
  - `make`
  - `make install` (use 'make reinstall' in future executions) 
  
Use the denoted buildscript (should be called 'buildui_win.sh')
  - `./buildui_win.sh`
  
This will generate a "QFMtool.exe" file. 

__Note__: This will only run with all needed dlls from 
"/usr/i686-w64-mingw/sys-root/mingw/bin" unfortunately
