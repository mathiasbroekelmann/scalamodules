package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
trait ServiceRegistry {
  def collect[A](clazz: Class[A], filter: Option[Filter] = None): Imports[A]
}