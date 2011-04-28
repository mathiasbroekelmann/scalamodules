package com.weiglewilczek.scalamodules.internal

import org.osgi.framework.{ServiceReference, BundleContext}
import com.weiglewilczek.scalamodules._

/**
 * @author mathias
 * @since 22.04.11
 */
class OsgiDetachedImport[A <: AnyRef](service: A,
                                      context: BundleContext,
                                      reference: ServiceReference) extends DetachedImport[A] {
  @volatile
  private[this] var disposed = false

  def dispose = {
    if (!disposed) {
      synchronized {
        context.ungetService(reference)
        disposed = true
      }
    }
  }

  def attributes = reference.properties

  def get = {
    require(!disposed, "Detached import was already disposed. Used ServiceReverence: " + reference)
    service
  }
}