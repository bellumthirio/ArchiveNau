## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

To run, you need an <x>_digit_primes.txt file or files in the numbers folder. The file should consist of newline-separated prime numbers. Do run the factor-generating algorithm, run the Main.scala main function (it has a k_maxs val defined, which can be changed to run the factoring algorithm with different k_max values), which will generate a csv with factoring data. For the order-finding algorithm, run the Order.scala order main function, which will generate the element orders for the factored numbers. For the ranks sage program, run `sage -python ranks.py "<output of running Main.scala main function>".