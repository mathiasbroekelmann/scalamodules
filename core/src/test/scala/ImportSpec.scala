package com.weiglewilczek.scalamodules

import org.specs.mock.Mockito
import org.specs.Specification

import com.weiglewilczek.scalamodules._

/**
 * User: mathias
 * Date: 25.04.11 11:42
 * Time: 11:42
 */

class ImportSpec extends Specification with Mockito {

  "import" should {
    val apple = ofType[Apple]

    val sweet = "sweet".present
    val sweetApple = ofType[Apple].whichIs(sweet)

    val tree = ofType[Tree]

    val registry = new TestServiceRegistry
    val registerApple = registry.register[Apple]
    registerApple using (Elstar)
    registerApple using (Gala)

    "be defined for some type" in {
      apple must notBeNull
    }

    "have a map function" in {
      val appleImports = apple apply registry
      var calls = 0
      val someApple = for(apple <- appleImports) yield {
        calls += 1
        apple
      }
      someApple must beSome(Elstar)
      calls must beEqual(1)
    }

    "be filtered" in {
      sweetApple must notBeNull
    }

    "be combined" in {
      val appleTree = for (someApple <- apple; someTree <- tree) yield {
        new AppleTree(someTree, someApple)
      }

      appleTree must notBeNull
    }
  }
}

case object Elstar extends Apple

case object Gala extends Apple

class TestServiceRegistry extends ServiceRegistry {

  var imports: List[RegisteredImport[_]] = Nil

  def collect[A <: AnyRef](clazz: Class[A], filter: Option[Filter]) = new TestImports[A](this, clazz, filter)

  def register[A <: AnyRef](implicit manifest: ClassManifest[A]) = {
    val clazz = manifest.erasure.asInstanceOf[Class[A]]
    new RegisteringService[A](clazz)
  }

  class RegisteringService[A <: AnyRef](clazz: Class[A]) {
    def using(service: A) = {
      imports = imports ::: RegisteredImport(clazz, service, Map.empty) :: Nil
    }
  }

}

case class RegisteredImport[A <: AnyRef](clazz: Class[A],
                                         instance: A,
                                         attributes: Map[String, Any])

class TestImports[A <: AnyRef](registry: TestServiceRegistry,
                               clazz: Class[A],
                               filter: Option[Filter]) extends Imports[A] {
  def flatMap[B](call: ServiceImport[A] => Iterable[B]): Iterable[B] = {
    map(call).flatten
  }

  def map[B](call: ServiceImport[A] => B): Iterable[B] = {
    for (registeredImport <- registry.imports.toStream; if registeredImport.clazz == clazz) yield {
      call(new ServiceImport[A] {
        def attributes = registeredImport.attributes

        def get = registeredImport.instance.asInstanceOf[A]
      })
    }
  }

  def head = new TestImport[A](registry, clazz, filter)
}

class TestImport[A <: AnyRef](registry: TestServiceRegistry,
                              clazz: Class[A],
                              filter: Option[Filter]) extends Import[A] {

  def attributes = registry.collect(clazz, filter).map(_.attributes).headOption.getOrElse(Map.empty)

  def detached = None

  def flatMap[B](f: A => Option[B]) = registry.collect(clazz, filter).flatMap(service => f(service.get)).headOption

  def map[B](f: A => B) = registry.collect(clazz, filter).map(service => f(service.get)).headOption
}

