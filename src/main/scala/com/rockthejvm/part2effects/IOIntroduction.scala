package com.rockthejvm.part2effects

import cats.effect.IO
import cats.syntax.apply._

object IOIntroduction {
  
  // 1. sequence two IOs and take the result of the LAST one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    _ <- ioa
    b <- iob
  } yield b
  // Also
  // ioa *> iob
  // ioa >> iob - ioa is lazily evaluated

  // 2. sequence 2 IOs and take the result of the First one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = for {
    a <- ioa
    _ <- iob
  } yield a
  // ioa <* iob
  // ioa << iob

  // mapN doesn't necessarily run the IOs sequentially.
  def takeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = (ioa, iob).mapN { (_, snd) => snd }
  def sequenceTakeFirst2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = (ioa, iob).mapN { (fst, _) => fst }

  // 3. repeat an IO effect forever.
  def forever[A](io: IO[A]): IO[A] = for {
    _ <- io
    a <- forever(io)
  } yield a
  // io *> forever(io)  - will stackoverflow - because it's evaluated strictly
  // io >> forever(io)
  // io.foreverM - same as forever with tail recursion

  // 4. convert an IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)
  // io.as(value)

  // 5. discart value inside and IO and just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] = convert(ioa, ())
  // ioa.void

  // 6. fix stack recursion
  def sumIO(n: Int): IO[Int] = {
    // def loop(x: Int, acc: Int = 0): Int = {
    //   if (x <= 0) acc
    //   else loop(x-1, acc + x)
    // }
    // IO.delay(loop(n))
    if (n <= 0) IO.pure(0)
    else for {
      l <- IO.pure(n)
      p <- sumIO(n-1)
    } yield l + p
  }


  def fib(n: Int): Int = {
    def loop(n: Int, acc: List[Int] = Nil): List[Int] =
      if (n == 0) acc
      else acc match {
        case Nil => loop(n-1, 0::Nil)
        case h::Nil => loop(n-1, 1::acc)
        case h1::h2::t => loop(n-1, h1+h2::acc)
      }

    loop(n).head
  }
  // 7. stack safe fibonacci
  def fibonacci(n: Int): IO[BigInt] = {
    // def next(a: Option[Int], b: Option[Int]): IO[(Int, Int)] = 
    //   (b, a + b)

    // for {
    //   a <- IO.pure(0)
    //   b <- IO.pure(1)
    //   x <- if (n == 0) a else if (n == 1) b else fibonacci(n - 1)
    // } yield x

    if (n > 2) for {
      a <- fibonacci(n-2)
      b <- fibonacci(n-1)
    } yield a + b
    // fibonacci(n - 2).flatMap { a =>
    //   fibonacci(n - 1)
    //     .map(b => a + b)
    // }
    else if (n == 2) IO.pure(BigInt(1))
    else IO.pure(BigInt(0))
  }


  def main(args: Array[String]): Unit = {
  }
}
