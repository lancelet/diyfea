diyfea
======

This code is a very simple prototype of a linear FEA solver written in Scala.  
It was quickly hacked-together to investigate system assembly, element
numerical integration, and shape function evaluation.  A more sophisticated
system is in the works.

Compiling the Code
------------------

Just install [sbt](http://www.scala-sbt.org/), and then execute `sbt run`.  You
will be given the option to execute a plate loading test or a truss test.  For
more details on both of these tests, please see the code.