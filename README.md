diyfea
======

This code is a very simple prototype of a linear FEA solver written in Scala.  
It was quickly hacked-together to investigate system assembly, element
numerical integration, and shape function evaluation.  A more sophisticated
system is in the works.

Example Output
--------------

For an example of mesh convergence produced by this code, please see
[this YouTube video](http://www.youtube.com/watch?v=QrzcakDmJgc).

Compiling and Running
---------------------

Just install [sbt](http://www.scala-sbt.org/), and then execute `sbt run`.  You
will be given the option to execute a plate loading test or a truss test.  For
more details on both of these tests, please see the code.