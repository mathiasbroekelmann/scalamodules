package com.weiglewilczek.scalamodules

import org.osgi.framework.{ServiceReference, BundleContext, Filter => OsgiFilter}

/**
 * define the context to collect services from.
 */
trait ServiceContext {

  /**
   * Collects all available services which matches the given by clause.
   * Each access to Iterable#size or Iterable#iterator will query 
   */
  def collect[A, B >: A](by: By[A]): Iterable[B]

  /**
   * Return some first service if at last one service is available, otherwise return None
   * 
   * A service definition will be registered as soon as the first service which satisfies the by clause is registered.
   * A service definition will be unregistered if the service that satisfies the given by claus is unregistered.
   */
  def head[A, B >: A](by: By[A]): Option[B]
}

private[scalamodules] class BundleServiceContext(bundleContext: BundleContext) extends ServiceContext {

  def collect[A, B >: A](by: By[A]): Iterable[B] = {
    new Iterable[B] {

      def freshServiceReferences: List[ServiceReference] = {
        val referencesArray = bundleContext.getServiceReferences(by.clazz.getName, null)
        Option(referencesArray).map(_.toList).getOrElse(Nil)
      }

      def iterator = {

        def serviceOf(reference: ServiceReference): Option[B] = {
          Option(bundleContext.getService(reference).asInstanceOf[B])
        }

        def nextService(references: List[ServiceReference]): Stream[B] = {
          references match {
            case Nil => Stream.empty
            case head :: tail => serviceOf(head) match {
              case None => nextService(tail)
              case Some(service) => Stream.cons(service, nextService(tail))
            }
          }
        }

        val sortedReferences = freshServiceReferences.sortWith(_.compareTo(_) < 0)

        nextService(sortedReferences).iterator
      }

      override def size = freshServiceReferences.length
    }
  }

  def head[A, B >: A](by: By[A]) = collect(by).headOption
}