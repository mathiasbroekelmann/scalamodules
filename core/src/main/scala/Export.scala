package com.weiglewilczek.scalamodules

trait Export[A] {

  // get the exported service instance.
  def get: A

  // add/update attributes for exported service
  def attributes(attributes: Map[String, AnyRef]): Unit

  // update the exported service to a new instance.
  def update(instance: A): Export[A]

  // releases this exported instance from the service registry
  def release: Unit
}