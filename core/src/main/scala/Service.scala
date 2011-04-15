package com.weiglewilczek.scalamodules

import org.osgi.framework.{Filter => OsgiFilter}

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