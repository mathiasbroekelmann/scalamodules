package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
case class ImportDefinition[A](clazz: Class[A], attributeFilter: Option[Filter] = None) {

  self =>

  def apply(registry: ServiceRegistry): Import[A] = registry.collect(clazz, attributeFilter).head

  def all: ImportsDefinition[A] = ImportsDefinition(clazz, attributeFilter)

  def detached: DetachedImportDefinition[A] = new DetachedImportDefinition[A] {
    def apply(registry: ServiceRegistry) = registry.collect(clazz, attributeFilter).head.detached
  }

  def map[B](call: Import[A] => B): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      Some(call(self.apply(registry)))
    }
  }

  def flatMap[B](call: Import[A] => ImportDependent[B]): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      call(self.apply(registry)).apply(registry)
    }
  }
}

object ImportDefinition {
  def ofType[A](implicit manifest: ClassManifest[A]): ImportDefinition[A] =
    ImportDefinition(manifest.erasure.asInstanceOf[Class[A]])
}