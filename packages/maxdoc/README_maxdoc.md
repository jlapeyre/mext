# maxdoc

maxdoc extends the maxima online documentation system.

The command-line interface to the maxima manual is modified to allow
other documents to be displayed with ? and ??  and to allow other
systems to make queries. The documentation system is divided into
three parts

* a) databases

* b) querying and organizing component.

* c) front ends.  Databases are registered with the querying component by
  sending a struct. The original describe command-line interface calls
  the querying component.

## Databases

There are four examples of databases:

*  a) the maxima info database.

*  b) simple-doc. This is available immediately. (I later saw this is
    similar to L. Butler's code.)

   (%i3) simple_doc_add("new_function", "The new description.");

   (%o3) "new_function"
   (%i4) ? new_function

      The new description.

   (%o4) true

*  c) max-doc, a more capable documentation database, but not
  coherent.  It would be nice if there were a lisp parser of
  gnu texinfo code. The idea is to make it easy to convert
  to normal maxima doc.

*  d) developer or internal documentation. Eg, there is a macro
    ddefun that expands to defun but also captures the doc
    string to a database along with the name of the source
    file, and lambda list. This is then also available via ? and ??.

### Pager

There is a variable controlling sending the output to a pager, as
well, that only works with some lisp/platfrom combinations.

## Additional information

See the README in defmfun1 for more information on maxdoc.
