package com.sutemi

import cats.effect.{ExitCode, IO, IOApp}

import java.io.File

object Main extends IOApp {

  // This is your new "main"!
//  def run: IO[Unit] =
//    HelloWorld.say().flatMap(IO.println)

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files"))
        else IO.unit
      orig = new File(args(0))
      dest = new File(args(1))
      count <- CopyFile.copy[IO](orig, dest)
      _ <- IO.println(s"Copied $count bytes from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success
}
