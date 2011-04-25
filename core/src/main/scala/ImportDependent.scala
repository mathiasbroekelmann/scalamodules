package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
trait ImportDependent[A] {
  def apply(registry: ServiceRegistry): Option[A]
}