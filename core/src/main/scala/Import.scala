package com.weiglewilczek.scalamodules

import org.osgi.framework.{ServiceReference, BundleContext}

/**
 * @author mathias
 * @since 22.04.11
 */
trait Import[A] extends Attributes {

  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  def detached: Option[DetachedImport[A]]
}

trait Attributes {
  def attributes: Map[String, Any]
}

trait DetachedImport[A] extends Attributes with Disposable {
  def get: A
}

trait Disposable {
  def dispose: Unit
}

object Import {
  def empty[A]: Import[A] = new Import[A] {
    def attributes = Map.empty

    def flatMap[B](f: A => Option[B]) = None

    def map[B](f: A => B) = None

    def detached = None
  }
}

case class ImportDefinition[A](clazz: Class[A], attributeFilter: Option[Filter] = None) {

  self =>

  def apply(registry: ServiceRegistry): Import[A] = registry.collect(clazz, attributeFilter).head

  def all: ImportsDefinition[A] = ImportsDefinition(clazz, attributeFilter)

  def detached: DetachedImportDefinition[A] = new DetachedImportDefinition[A] {
    def apply(registry: ServiceRegistry) = registry.collect(clazz, attributeFilter).head.detached
  }

  def map[B](call: Import[A] => B): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      Some(call(self.apply(registry)))
    }
  }

  def flatMap[B](call: Import[A] => ImportDependent[B]): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      call(self.apply(registry)).apply(registry)
    }
  }
}

case class ImportsDefinition[A](clazz: Class[A], attributeFilter: Option[Filter] = None) {

  self =>

  def apply(registry: ServiceRegistry): Imports[A] = registry.collect(clazz, attributeFilter)

  def head: ImportDefinition[A] = ImportDefinition(clazz, attributeFilter)

  def map[B](call: Imports[A] => B): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      Some(call(self.apply(registry)))
    }
  }

  def flatMap[B](call: Imports[A] => ImportDependent[B]): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      call(self.apply(registry)).apply(registry)
    }
  }
}

object ImportDefinition {
  def ofType[A](implicit manifest: ClassManifest[A]): ImportDefinition[A] =
    ImportDefinition(manifest.erasure.asInstanceOf[Class[A]])
}

trait DetachedImportDefinition[A] {

  self =>

  def apply(registry: ServiceRegistry): Option[DetachedImport[A]]

  def map[B](call: DetachedImport[A] => B): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      self.apply(registry).map(call)
    }
  }

  def flatMap[B](call: DetachedImport[A] => ImportDependent[B]): ImportDependent[B] = new ImportDependent[B] {
    def apply(registry: ServiceRegistry): Option[B] = {
      self.apply(registry).flatMap(call(_).apply(registry))
    }
  }

  def filter(call: DetachedImport[A] => Boolean): DetachedImportDefinition[A] = new DetachedImportDefinition[A] {
    def apply(registry: ServiceRegistry) = {
      self.apply(registry).filter(call)
    }
  }
}

trait ImportDependent[A] {
  def apply(registry: ServiceRegistry): Option[A]
}

trait ServiceRegistry {
  def collect[A](clazz: Class[A], filter: Option[Filter] = None): Imports[A]
}

class OsgiServiceRegistry(context: BundleContext) extends ServiceRegistry {
  def collect[A](clazz: Class[A], filter: Option[Filter]) = new OsgiServiceImports[A](context, clazz, filter)
}

class OsgiServiceImports[A](context: BundleContext,
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

class OsgiDetachedImport[A](service: A,
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

abstract class HeadImport[A](imports: Imports[A]) extends Import[A] {
  def map[B](call: A => B): Option[B] =
    imports.map(service => call(service get)).headOption

  def flatMap[B](call: A => Option[B]): Option[B] = {
    imports.flatMap(service => call(service get).toIterable).headOption
  }

  def attributes = imports.map(_.attributes).headOption.getOrElse(Map.empty)
}

trait Imports[A] {

  def head: Import[A]

  def map[B](call: ServiceImport[A] => B): Iterable[B]

  def flatMap[B](call: ServiceImport[A] => Iterable[B]): Iterable[B]
}

trait ServiceImport[A] extends Attributes {
  def get: A
}

import ImportDefinition._

object Test {

  val dependentAppleTreeByFor: ImportDependent[AppleTree] = for {
    tree <- ofType[Tree]
    apple <- ofType[Apple]
  } yield {
    AppleTree(tree, apple)
  }

  val detachedAppleTreeByFor: ImportDependent[DetachedAppleTree with Disposable] = for {
    tree <- ofType[Tree].detached
    apple <- ofType[Apple].detached
  } yield {
    new DetachedAppleTree(tree get, apple get) with Disposable {
      def dispose = {
        tree.dispose
        apple.dispose
      }
    }
  }

  val detachedAppleAllTreesByFor: ImportDependent[DetachedAppleTrees] = for {
    trees <- ofType[Tree].all
    apple <- ofType[Apple].detached
  } yield {
    new DetachedAppleTrees(trees, apple get) with Disposable {
      def dispose = {
        apple.dispose
      }
    }
  }
}

trait Tree

trait Apple

case class AppleTree(tree: Import[Tree], apple: Import[Apple])

class DetachedAppleTree(tree: Tree, apple: Apple)

class DetachedAppleTrees(trees: Imports[Tree], apple: Apple) {

}