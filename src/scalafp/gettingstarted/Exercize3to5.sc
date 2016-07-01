package org.scalafp.section2

object Exercize3to5 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
  	(a : A) => ((b : B) => f(a, b))
  }                                               //> curry: [A, B, C](f: (A, B) => C)A => (B => C)
  curry((x: Int, y: Int) => x + y)                //> res0: Int => (Int => Int) = <function1>
  curry((x: Int, y: Int) => x + y)(1)(2)          //> res1: Int = 3
  
  def uncurry[A, B, C](f : A => B => C) : (A, B) => C = {
  	(a : A, b : B) => f(a)(b)
  }                                               //> uncurry: [A, B, C](f: A => (B => C))(A, B) => C
  uncurry(curry((x: Int, y: Int) => x + y))       //> res2: (Int, Int) => Int = <function2>
  uncurry(curry((x: Int, y: Int) => x + y))(1, 2) //> res3: Int = 3
  
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
  	(a : A) => f(g(a))
  }                                               //> compose: [A, B, C](f: B => C, g: A => B)A => C
}