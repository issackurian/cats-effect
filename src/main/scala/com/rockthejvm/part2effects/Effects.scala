package com.rockthejvm.part2effects

import java.time.Instant

object Effects {

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = {
      MyIO(() => f(unsafeRun()))
    }
    def flatMap[B](f: A => MyIO[B]): MyIO[B] = {
      MyIO(() => f(unsafeRun()).unsafeRun())
    }
  }
    // Exercises
    // 1. An IO which returns the current time of the system
    def currentTime: MyIO[Instant] = MyIO(() => Instant.now())

    // 2. An IO which measures the duration of a computation
    def measure[A](computation: MyIO[A]): MyIO[Long] = {
      for {
        start <- currentTime
        _ <- computation
        end <- currentTime
      } yield end.getEpochSecond() - start.getEpochSecond()
    }

    // 3. An IO which prints something to the console
    def printIO[A](value: A): MyIO[Unit] = MyIO(() => println(value))

    // 4. An IO that reads from the console.
    def readIO: MyIO[String] = MyIO(() => io.StdIn.readLine())

  def main(args: Array[String]): Unit = {

  }
}
