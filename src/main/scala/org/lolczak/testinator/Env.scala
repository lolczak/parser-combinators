package org.lolczak.testinator

trait Env {

  val host: String

}

object AppSpotEnv extends Env {

  override val host: String = "testinator-project.appspot.com"

}