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

package org.scalasteward.core.buildtool.sbt

import better.files.File
import cats.Functor
import cats.data.OptionT
import cats.syntax.all._
import io.chrisdavenport.log4cats.Logger
import org.scalasteward.core.application.Config
import org.scalasteward.core.buildtool.BuildToolAlg
import org.scalasteward.core.buildtool.sbt.command._
import org.scalasteward.core.buildtool.sbt.data.SbtVersion
import org.scalasteward.core.data.{Dependency, Scope}
import org.scalasteward.core.io.{FileAlg, FileData, ProcessAlg, WorkspaceAlg}
import org.scalasteward.core.scalafix.Migration
import org.scalasteward.core.util.{BracketThrow, Nel}
import org.scalasteward.core.vcs.data.Repo

trait SbtAlg[F[_]] extends BuildToolAlg[F] {
  def addGlobalPluginTemporarily[A](plugin: FileData)(fa: F[A]): F[A]

  def addGlobalPlugins[A](fa: F[A]): F[A]

  def getSbtVersion(repo: Repo): F[Option[SbtVersion]]

  final def getSbtDependency(repo: Repo)(implicit F: Functor[F]): F[Option[Dependency]] =
    OptionT(getSbtVersion(repo)).subflatMap(sbtDependency).value
}

object SbtAlg {
  def create[F[_]](config: Config)(implicit
      fileAlg: FileAlg[F],
      logger: Logger[F],
      processAlg: ProcessAlg[F],
      workspaceAlg: WorkspaceAlg[F],
      F: BracketThrow[F]
  ): SbtAlg[F] =
    new SbtAlg[F] {
      override def addGlobalPluginTemporarily[A](plugin: FileData)(fa: F[A]): F[A] =
        sbtDir.flatMap { dir =>
          val plugins = "plugins"
          fileAlg.createTemporarily(dir / "0.13" / plugins / plugin.name, plugin.content) {
            fileAlg.createTemporarily(dir / "1.0" / plugins / plugin.name, plugin.content) {
              fa
            }
          }
        }

      override def addGlobalPlugins[A](fa: F[A]): F[A] =
        logger.info("Add global sbt plugins") >>
          stewardPlugin.flatMap(addGlobalPluginTemporarily(_)(fa))

      override def containsBuild(repo: Repo): F[Boolean] =
        workspaceAlg.repoDir(repo).flatMap(repoDir => fileAlg.isRegularFile(repoDir / "build.sbt"))

      override def getSbtVersion(repo: Repo): F[Option[SbtVersion]] =
        for {
          repoDir <- workspaceAlg.repoDir(repo)
          maybeProperties <- fileAlg.readFile(repoDir / "project" / "build.properties")
          version = maybeProperties.flatMap(parser.parseBuildProperties)
        } yield version

      override def getDependencies(repo: Repo): F[List[Scope.Dependencies]] =
        for {
          repoDir <- workspaceAlg.repoDir(repo)
          commands = Nel.of(crossStewardDependencies, reloadPlugins, stewardDependencies)
          lines <- sbt(commands, repoDir)
          dependencies = parser.parseDependencies(lines)
          additionalDependencies <- getAdditionalDependencies(repo)
        } yield additionalDependencies ::: dependencies

      override def runMigrations(repo: Repo, migrations: Nel[Migration]): F[Unit] =
        addGlobalPluginTemporarily(scalaStewardScalafixSbt) {
          workspaceAlg.repoDir(repo).flatMap { repoDir =>
            migrations.traverse_ { migration =>
              val withScalacOptions =
                migration.scalacOptions.fold[F[Unit] => F[Unit]](identity) { opts =>
                  val file = scalaStewardScalafixOptions(opts.toList)
                  fileAlg.createTemporarily(repoDir / file.name, file.content)(_)
                }

              val scalafixCmds = migration.rewriteRules.map(rule => s"$scalafixAll $rule").toList
              withScalacOptions(sbt(Nel(scalafixEnable, scalafixCmds), repoDir).void)
            }
          }
        }

      val sbtDir: F[File] =
        fileAlg.home.map(_ / ".sbt")

      def sbt(sbtCommands: Nel[String], repoDir: File): F[List[String]] =
        maybeIgnoreOptsFiles(repoDir) {
          val command =
            Nel.of(
              "sbt",
              "-Dsbt.color=false",
              "-Dsbt.log.noformat=true",
              "-Dsbt.supershell=false",
              sbtCommands.mkString_(";", ";", "")
            )
          processAlg.execSandboxed(command, repoDir)
        }

      def maybeIgnoreOptsFiles[A](dir: File)(fa: F[A]): F[A] =
        if (config.ignoreOptsFiles) ignoreOptsFiles(dir)(fa) else fa

      def ignoreOptsFiles[A](dir: File)(fa: F[A]): F[A] =
        fileAlg.removeTemporarily(dir / ".jvmopts") {
          fileAlg.removeTemporarily(dir / ".sbtopts") {
            fa
          }
        }

      def getAdditionalDependencies(repo: Repo): F[List[Scope.Dependencies]] =
        getSbtDependency(repo)
          .map(_.map(dep => Scope(List(dep), List(config.defaultResolver))).toList)
    }
}
