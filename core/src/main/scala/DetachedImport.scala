package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
trait DetachedImport[A] extends Attributes with Disposable {
  def get: A
}