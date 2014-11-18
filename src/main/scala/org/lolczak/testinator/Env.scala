package org.lolczak.testinator

/*
Host should be loaded from config file, but I had no time to do it.
 */
trait Env {

  val host: String

}

object AppSpotEnv extends Env {

  override val host: String = "testinator-project.appspot.com"

}