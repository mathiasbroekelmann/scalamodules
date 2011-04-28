package com.weiglewilczek.scalamodules.internal

import org.osgi.framework.{BundleContext}
import com.weiglewilczek.scalamodules._

private[scalamodules] class OsgiExportDefinition[A <: AnyRef](context: BundleContext,
                                                              exposingTypes: List[Class[AnyRef]],
                                                              attributes: Map[String, Any] = Map.empty)
  extends ExportDefinition[A] {

  self =>

  def exposing[B >: A](implicit manifest: ClassManifest[B]) =
    exposing(manifest.erasure.asInstanceOf[Class[B]])

  def exposing[B >: A](clazz: Class[B]) =
    new OsgiExportDefinition(context, clazz.asInstanceOf[Class[AnyRef]] :: exposingTypes, attributes)

  def attributes(attributes: Map[String, AnyRef]) =
    new OsgiExportDefinition(context, exposingTypes, self.attributes ++ attributes)

  def apply(service: A) = {
    val clazzes = exposingTypes.map(_.getName).toArray
    val registration = context.registerService(clazzes, service, attributes)
    OsgiExport[A](self, registration, service)
  }
}





