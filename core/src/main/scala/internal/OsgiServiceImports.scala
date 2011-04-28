package com.weiglewilczek.scalamodules.internal

import org.osgi.framework.{ServiceReference, BundleContext}
import com.weiglewilczek.scalamodules._

/**
 * @author mathias
 * @since 22.04.11
 */
class OsgiServiceImports[A <: AnyRef](context: BundleContext,
                                      clazz: Class[A],
                                      filter: Option[Filter]) extends Imports[A] {

  def map[B](call: ServiceImport[A] => B): Iterable[B] = {
    serviceReferences.flatMap(callWithServiceImport(_, call).toIterable)
  }

  def flatMap[B](call: ServiceImport[A] => Iterable[B]): Iterable[B] = {
    serviceReferences.flatMap(callWithServiceImport(_, call).toIterable).flatten
  }

  def head: Import[A] = new HeadImport[A](this) {
    def detached = {
      serviceReferences.map(importOf(_).detached).flatten.headOption
    }
  }

  private[this] def callWithServiceImport[B](reference: ServiceReference, call: ServiceImport[A] => B): Option[B] = {
    for (s <- importOf(reference).detached) yield {
      try {
        call(new ServiceImport[A] {
          lazy val attributes = s.attributes
          val get = s.get
        })
      } finally {
        s.dispose
      }
    }
  }

  private[this] def importOf(reference: ServiceReference): Import[A] =
    new OsgiImport[A](context, reference) {}

  private[this] def serviceReferences = {
    val filterString = filter.map(_.toString).orNull
    val serviceReferences = context.getServiceReferences(clazz.getName, filterString)
    Option(serviceReferences).map(_.toStream).getOrElse(Stream.empty)
  }
}


import ImportDefinition._











