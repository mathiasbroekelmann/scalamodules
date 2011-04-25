package com.weiglewilczek.scalamodules.internal

import com.weiglewilczek.scalamodules.{Import, Imports}

/**
 * @author mathias
 * @since 22.04.11
 */
abstract class HeadImport[A](imports: Imports[A]) extends Import[A] {
  def map[B](call: A => B): Option[B] =
    imports.map(service => call(service get)).headOption

  def flatMap[B](call: A => Option[B]): Option[B] = {
    imports.flatMap(service => call(service get).toIterable).headOption
  }

  def attributes = imports.map(_.attributes).headOption.getOrElse(Map.empty)
}