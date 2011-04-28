package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
trait ServiceRegistry {
  def collect[A <: AnyRef](clazz: Class[A], filter: Option[Filter] = None): Imports[A]
  //def expose[A <: AnyRef](implicit manifest: ClassManifest[A]): ExportDefinition[A]
}