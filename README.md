`jda-minor-mode` is an Emacs minor-mode written by Lee Jong-Gyu for all developers.
I developed `jda-minor-mode` with the aim of providing developers with convenience
because I had been deeply impressed by TextMate and Visual Assist.
No doubt you will always enjoy writing a source code in any programming language if you use `jda-minor-mode`.

Quick setup
===========

* Download `jda-minor-mode`

```sh
~$ git clone git://github.com/jglee1027/jda-minor-mode.git
```

* Add the following in ~/.emacs

```lisp
(add-to-list 'load-path "/path/to/jda-minor-mode")
(require 'jda)
(jda-minor-mode)
```

Features
========

Build
-----
If there is Makefile or .xcodeproj in your project,
you can build a project easily by pressing `C-c c` in any subdirectory of your project.

Find development doucmentation
------------------------------
Press `C-c h` at the symbol which you want to find in a development documentation(Android or Xcode).

Open a counterpart file
-----------------------
You can quickly open the counterpart of the current buffer by pressing `C-c j p`.
In other words, if the current buffer is c or cpp source file,
you can easily visit the header file related it by pressing `C-c j p`.

Find a symbol in the project
----------------------------
You can simply search positions which a symbol at point is used in your project by pressing `C-c j s`.
It uses (grep-find).

Find a file in the project
--------------------------
You can rapidly find a file in your project by pressing `C-c j f`.

Visit a file in the project
--------------------------
You can easily open the file you want to visit in your project by pressing `C-c j i` or `C-c i`.
It supports incremental search like TextMate or Visual Assist.
As you type text, one or more possible matches for the text are found and immediately displayed.

Find a symbol in the project
----------------------------
If you want to find a symbol in your project, press `C-c j s`.
After finding it, you can easily navigate the symbols by pressing `M-g p` or `M-g n`.
   
Go to a symbol in the current buffer
------------------------------------
Press `C-c j m` or `C-c m` if you want to go to a function.
You can see all functions defined in the current buffer.
It supports incremental search like TextMate or Visual Assist.

Replace a string in several files
---------------------------------
If you want to replace a string in several files, press `C-c j 5` or `C-c j %`.
You can easily replace a string in specified files in your project.

Create a TAG in your project
----------------------------
Press `C-c j t` and you can easily create TAG file in your project using 'find' and 'etags'.
   
Go to previous or next marker
-----------------------------
Press `C-c ,` to go to the previous marker and Press `C-c .` to go to the next marker.
Press `C-x <down>` to save the current marker.
   
Highlight symbol
----------------
If you want to see highlighted symbol at point, press `C-c j h`.
After specified idle time, the current symbol at point is highlighted.
It only works in file buffer.
   
Insert or Delete a bracket in Objective-C mode
----------------------------------------------
Press `C-c ]` to insert a right bracket to pair.
Press `C-c [` to delete left and right brackets to pair.
   
License
=======

It is distributed under the GNU General Public License.
See the accompanying [GPL-3.0.txt](GPL-3.0.txt) file for more details.

Bug report
==========

Please use the issues tab to report any issues.
