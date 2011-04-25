package com.weiglewilczek.scalamodules.internal

import org.osgi.framework.{ServiceReference, BundleContext}
import com.weiglewilczek.scalamodules.Import

/**
 * @author mathias
 * @since 22.04.11
 */
abstract class OsgiImport[A](context: BundleContext, reference: ServiceReference) extends Import[A] {

  self =>

  def getService = Option(context.getService(reference)).map(_.asInstanceOf[A])

  def callAndReleaseService[B](call: => B): B = {
    try {
      call
    } finally {
      context.ungetService(reference)
    }
  }

  def flatMap[B](call: A => Option[B]): Option[B] =
    getService.flatMap(callAndReleaseService(call(_)))

  def map[B](call: A => B): Option[B] =
    getService.map(callAndReleaseService(call(_)))

  def attributes = reference.properties

  def detached = getService.map {
    service => new OsgiDetachedImport(service, context, reference)
  }
}