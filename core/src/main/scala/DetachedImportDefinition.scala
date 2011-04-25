package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
trait DetachedImportDefinition[A] {

  self =>

  def apply(registry: ServiceRegistry): Option[DetachedImport[A]]

  def map[B](call: DetachedImport[A] => B): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      self.apply(registry).map(call)
    }
  }

  def flatMap[B](call: DetachedImport[A] => ImportDependent[B]): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      self.apply(registry).flatMap(call(_).apply(registry))
    }
  }

  def filter(call: DetachedImport[A] => Boolean): DetachedImportDefinition[A] = new DetachedImportDefinition[A] {
    def apply(registry: ServiceRegistry) = {
      self.apply(registry).filter(call)
    }
  }
}