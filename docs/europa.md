project:          Europa
summary:          ![EUROPA](|media|/europa-logo.png)
author:           Stephen W. Soliday
src_dir:          ../src
output_dir:       ./html/publish/
media_dir:        ./media
exclude_dir:      ../src/tests
exclude:          iso_varying_string.f90
favicon:          ./media/europa-032.png
project_github:   https://github.com/solsteve
project_download: https://github.com/solsteve
github:           https://github.com
website:          http://www.soliday.com/stephen
preprocessor:     gfortran -E
predocmark_alt:   >
predocmark:       <
docmark_alt:
docmark:          !
display:          public
                  protected
                  private
source:           true
graph:            true
sort:             alpha
coloured_edges:   true
extra_filetypes:  .inc !
print_creation_date: true
creation_date:    %Y-%m-%d %H:%M %z
extra_mods:  iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html

md_extensions: markdown.extensions.toc(anchorlink=False)
               markdown.extensions.smarty(smart_quotes=False)

# A library of modern FORTRAN modules.

Starting in 1979 a set of basic fortran helper functions was started with FORTRAN IV.
In 1982 the library was extended with PDP-11/34 FORTRAN 77.
From 1989-1991 the library was again extended with VAX and Cray FORTRAN.
The next major evolution occured in 1993 with Sun Sparc 20.
From 1996-2000 The library was again extended.
In 2015 a major evolution occured with the complete rewrite in FORTRAN 2003.
Finally, in 2019 the library was being brought up to full 2018 compliance,
with all deprecated features removed.

Europa is the culmanation of decades research in modeling and sim and machine learning.
