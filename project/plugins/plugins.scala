import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {

  // ===================================================================================================================
  // Repositories
  // ===================================================================================================================

  object Repositories {
    lazy val aquteRepo = "aQute Maven Repository" at "http://www.aqute.biz/repo"
    lazy val sbtIdeaRepo = "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
  }

  // ===================================================================================================================
  // ModuleConfigurations
  // ===================================================================================================================
  import Repositories._

  lazy val aquteModuleConfig = ModuleConfiguration("biz.aQute", aquteRepo)

  // ===================================================================================================================
  // Dependencies
  // ===================================================================================================================

  lazy val bnd4sbt = "com.weiglewilczek.bnd4sbt" % "bnd4sbt" % "1.0.0"

  lazy val sbtIdea = "com.github.mpeltonen" % "sbt-idea-plugin" % "0.3.0"
}
