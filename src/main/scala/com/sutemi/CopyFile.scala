package com.sutemi

import cats.effect.{IO, Resource, Sync}
import cats.syntax.all._

import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}

object CopyFile {

  def copy[F[_]: Sync](origin: File, destination: File): F[Long] =
    inputOutputStreams(origin, destination).use { case(in, out) =>
      transfer(in, out)
    }

  def transfer[F[_]: Sync](origin: InputStream, destination: OutputStream): F[Long] =
    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)

  def transmit[F[_]: Sync](origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): F[Long] =
    for {
      amount <- Sync[F].blocking(origin.read(buffer, 0, buffer.size))
      count <- if (amount > -1) Sync[F].blocking(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
        else Sync[F].pure(acc)
    } yield count

  def inputStream[F[_]: Sync](f: File): Resource[F, FileInputStream] =
    Resource.make {
      Sync[F].blocking(new FileInputStream(f))
    } { inStream =>
      Sync[F].blocking(inStream.close()).handleErrorWith(_ => Sync[F].unit)
      // this error handler swallows the errors
    }

  def outputStream[F[_]: Sync](f: File): Resource[F, FileOutputStream] =
    Resource.make {
      Sync[F].blocking(new FileOutputStream(f))
    } { outStream =>
      Sync[F].blocking(outStream.close()).handleErrorWith(_ => Sync[F].unit)
    }

  def inputOutputStreams[F[_]: Sync](in: File, out: File): Resource[F, (FileInputStream, FileOutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)


}
