package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
trait ServiceImport[A] extends Attributes {
  def get: A
}