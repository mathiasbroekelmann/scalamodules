package com.weiglewilczek.scalamodules

import org.osgi.framework.{ServiceRegistration, BundleContext, Filter => OsgiFilter}

/**
 * Resolves a service from some context.
 */
trait Service[A] {
  def apply(context: ServiceContext): Option[A]

  //type ServiceType <: Service[A]

  /**
   * define a function which is called when a service is registered.
   * TODO: needs to be implemented
   */
  //def bind(f: A => Unit): ServiceType = self

  /**
   * define a function which is called when a registered service is unregistered. Use this to call any clean up functions on the service.
   * TODO: needs to be implemented
   */
  //def unbind(f: A => Unit): ServiceType = self
}

trait ServiceDependency[A] extends Service[A] {

  self =>

  def map[B](f: A => B): ServiceDefinition[B] = {
    new ServiceDefinition[B] {
      def apply(context: ServiceContext) = {
        self.apply(context).map(f)
      }
    }
  }

  def flatMap[B](f: A => ServiceDefinition[B]): ServiceDefinition[B] = {
    new ServiceDefinition[B] {
      def apply(context: ServiceContext) = {
        self.apply(context).flatMap(f(_).apply(context))
      }
    }
  }

  def withFilter(f: A => Boolean): ServiceDependency[A] = {
    new ServiceDependency[A] {
      def apply(context: ServiceContext) = {
        self.apply(context).filter(f)
      }
    }
  }
}

trait ServiceDefinition[A] extends Service[A] {
  /**
   * define a class/interface which is exported by this service definition
   */
  //def exposing[B>:A](implicit manifest: ClassManifest[B]): ServiceDefinition[A] = this
  //def exposing[B>:A, C>:A](implicit bmanifest: ClassManifest[B], cmanifest: ClassManifest[C]): ServiceDefinition[A] = this
  //def exposingAllInterfaces: ServiceDefinition[A] = this
}

object Service {

  def serviceOf[A <: AnyRef](implicit manifest: ClassManifest[A]): ServiceDependency[A] = {

    new ServiceDependency[A] {
      def apply(context: ServiceContext) = context.head(By.byType[A])
    }
  }

  /**
   * wrapps a constant service which don't have any service dependencies.
   */
  def service[A <: AnyRef](someService: A): ServiceDefinition[A] = {

    new ServiceDefinition[A] {
      def apply(context: ServiceContext) = Some(someService)
    }
  }
}

private[scalamodules] case class OsgiExportDefinition[A <: AnyRef](context: BundleContext,
                                                                   exposingTypes: List[Class[AnyRef]],
                                                                   attributes: Map[String, Any] = Map.empty)
  extends ExportDefinition[A] {

  self =>

  def exposing[B >: A](implicit manifest: ClassManifest[B]) =
    OsgiExportDefinition(context, manifest.erasure.asInstanceOf[Class[AnyRef]] :: exposingTypes, attributes)

  def attributes(attributes: Map[String, AnyRef]) =
    OsgiExportDefinition(context, exposingTypes, self.attributes ++ attributes)

  def apply[B <: A](service: B) = {
    val clazzes = exposingTypes.map(_.getName).toArray
    val registration = context.registerService(clazzes, service, attributes)
    OsgiExport[A](self, registration, service)
  }
}

case class OsgiExport[A <: AnyRef](definition: OsgiExportDefinition[A],
                                   registration: ServiceRegistration,
                                   instance: A)
  extends Export[A] {

  def release = registration.unregister

  def attributes(attributes: Map[String, AnyRef]) {
    registration.setProperties(attributes)
  }

  def update[B <: A](instance: B): Export[A] = {
    release
    definition.apply(instance)
  }
}

trait ExportDefinition[A] {
  // export the given service by using this export definition
  def apply[B <: A](instance: B): Export[A]

  def using[B <: A](instance: B) = apply(instance)

  // add/replace attributes for exported services
  def attributes(attributes: Map[String, AnyRef]): ExportDefinition[A]

  // add a type to expose for the exported service
  def exposing[B >: A](implicit manifest: ClassManifest[B]): ExportDefinition[A]
}

trait Export[A] {

  // get the exported service instance.
  def instance: A

  // add/update attributes for exported service
  def attributes(attributes: Map[String, AnyRef]): Unit

  // update the exported service to a new instance.
  def update[B <: A](instance: B): Export[A]

  // releases this exported instance from the service registry
  def release: Unit
}