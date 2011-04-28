package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
trait Import[A] extends Attributes {

  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  def detached: Option[DetachedImport[A]]
}

object Import {
  def empty[A]: Import[A] = new Import[A] {
    def attributes = Map.empty

    def flatMap[B](f: A => Option[B]) = None

    def map[B](f: A => B) = None

    def detached = None
  }
}

import ImportDefinition._

object Test {

  val dependentAppleTreeByFor: ImportDependent[AppleTree] = for {
    tree <- ofType[Tree]
    apple <- ofType[Apple]
  } yield {
    new AppleTree(tree, apple)
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





class AppleTree(tree: Import[Tree], apple: Import[Apple])

class DetachedAppleTree(tree: Tree, apple: Apple)

class DetachedAppleTrees(trees: Imports[Tree], apple: Apple) {

}