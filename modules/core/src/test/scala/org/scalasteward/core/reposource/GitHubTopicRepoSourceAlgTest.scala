package org.scalasteward.core.reposource

import cats.effect.IO
import io.circe.literal._
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.scalasteward.core.mock.MockContext._
import org.scalasteward.core.util.HttpJsonClient
import org.scalasteward.core.vcs.data.Repo
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GitHubTopicRepoSourceAlgTest extends AnyFunSuite with Matchers {
  object page extends QueryParamDecoderMatcher[Int]("page")
  val routes: HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case req @ GET -> Root / "search" / "repositories" :? page(p) =>
        println(req.toString())
        p match {
          case 1 =>
            Ok(
              json"""{
                "items": [
                  { "name":"r1", "owner": { "login": "u1"}},
                  { "name":"r2", "owner": { "login": "u2"}},
                  { "name":"r3", "owner": { "login": "u3"}}
                ]
              }"""
            )
          case 2 =>
            Ok(
              json"""{
                "items": [
                  { "name":"r4", "owner": { "login": "u4"}},
                  { "name":"r5", "owner": { "login": "u5"}}
                ]
              }"""
            )
          case _ =>
            Ok(
              json"""{
                "items": []
              }"""
            )
        }
      case req =>
        println(req.toString())
        NotFound()
    }

  implicit val client: Client[IO] = Client.fromHttpApp(routes.orNotFound)
  implicit val httpJsonClient: HttpJsonClient[IO] = new HttpJsonClient[IO]
  val repoSource = new GitHubTopicRepoSourceAlg[IO](config.copy(githubTopicForRepos = Some("foo")), user)

  test("fetch repos from github topics") {
    val repos = repoSource.fetchRepos().compile.toList.unsafeRunSync()
    repos shouldBe List(
      Repo("u1", "r1"),
      Repo("u2", "r2"),
      Repo("u3", "r3"),
      Repo("u4", "r4"),
      Repo("u5", "r5")
    )
  }
}
