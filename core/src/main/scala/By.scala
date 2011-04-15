package com.weiglewilczek.scalamodules

import org.osgi.framework.{Filter => OsgiFilter}

/**
 * define a by clause which is used to select service references.
 */
trait By[A] {

  def clazz: Class[A]

  //def filter: Option[OsgiFilter]
}

object By {

  def byType[A <: AnyRef](implicit manifest: ClassManifest[A]): By[A] = {
    ByType(manifest.erasure.asInstanceOf[Class[A]])
  }
}

case class ByType[A](clazz: Class[A]) extends By[A]