package com.weiglewilczek.scalamodules

trait ExportDefinition[A] {
  // expose the given service by using this definition
  def apply(instance: A): Export[A]

  def using(instance: A) = apply(instance)

  // add/replace attributes for exported services
  def attributes(attributes: Map[String, AnyRef]): ExportDefinition[A]

  // add a type to expose for the exported service
  def exposing[B >: A](implicit manifest: ClassManifest[B]): ExportDefinition[A]

  def exposing[B >: A](clazz: Class[B]): ExportDefinition[A]
}

