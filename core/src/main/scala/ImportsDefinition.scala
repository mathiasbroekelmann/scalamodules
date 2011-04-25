package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
case class ImportsDefinition[A](clazz: Class[A], attributeFilter: Option[Filter] = None) {

  self =>

  def apply(registry: ServiceRegistry): Imports[A] = registry.collect(clazz, attributeFilter)

  def head: ImportDefinition[A] = ImportDefinition(clazz, attributeFilter)

  def map[B](call: Imports[A] => B): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      Some(call(self.apply(registry)))
    }
  }

  def flatMap[B](call: Imports[A] => ImportDependent[B]): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      call(self.apply(registry)).apply(registry)
    }
  }
}