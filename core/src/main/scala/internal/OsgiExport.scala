package com.weiglewilczek.scalamodules.internal

import org.osgi.framework.{ServiceRegistration}
import com.weiglewilczek.scalamodules._

case class OsgiExport[A <: AnyRef](definition: OsgiExportDefinition[A],
                                   registration: ServiceRegistration,
                                   get: A)
  extends Export[A] {

  def release = registration.unregister

  def attributes(attributes: Map[String, AnyRef]) {
    registration.setProperties(attributes)
  }

  def update(instance: A): Export[A] = {
    release
    definition.apply(instance)
  }
}





