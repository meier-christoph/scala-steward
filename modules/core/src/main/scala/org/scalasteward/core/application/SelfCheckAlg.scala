/*
 * Copyright 2018-2020 Scala Steward contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalasteward.core.application

import cats.syntax.all._
import io.chrisdavenport.log4cats.Logger
import org.scalasteward.core.git.GitAlg
import org.scalasteward.core.scalafmt.ScalafmtAlg
import org.scalasteward.core.util.{HttpExistenceClient, MonadThrow}

final class SelfCheckAlg[F[_]](config: Config)(implicit
    gitAlg: GitAlg[F],
    httpExistenceClient: HttpExistenceClient[F],
    logger: Logger[F],
    scalafmtAlg: ScalafmtAlg[F],
    F: MonadThrow[F]
) {
  def checkAll: F[Unit] =
    for {
      _ <- logger.info("Run self checks")
      _ <- checkGitBinary
      _ <- checkScalafmtBinary
      _ <- checkHttpExistenceClient
    } yield ()

  private def checkGitBinary: F[Unit] =
    gitAlg.version.attempt.flatMap {
      case Right(output)   => logger.info(s"Using $output")
      case Left(throwable) => logger.warn(throwable)("Failed to execute git")
    }

  private def checkScalafmtBinary: F[Unit] =
    scalafmtAlg.version.attempt.flatMap {
      case Right(output)   => logger.info(s"Using $output")
      case Left(throwable) => logger.warn(throwable)("Failed to execute scalafmt")
    }

  private def checkHttpExistenceClient: F[Unit] =
    for {
      res <- httpExistenceClient.exists(config.selfCheckUri)
      url = config.selfCheckUri
      msg = s"Self check of HttpExistenceClient failed: checking that $url exists failed"
      _ <- if (!res) logger.warn(msg) else F.unit
    } yield ()
}
