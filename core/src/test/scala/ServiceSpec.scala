package com.weiglewilczek.scalamodules

import org.specs.Specification
import org.specs.mock.Mockito

/**
 * @author mathias.broekelmann
 * @since 15.04.11 09:37
 */
class ServiceSpec extends Specification with Mockito {

  "some service" should {
    val apple: ServiceDependency[Apple] = Service.serviceOf[Apple]
    val tree = Service.serviceOf[Tree]
    val water = Service.serviceOf[Water]
    val context = mock[ServiceContext]

    "be created for a type" in {
      apple must notBeNull
    }

    "can be used in a simple for expression" in {
      val needsApple: ServiceDefinition[NeedsApple] = for (someApple <- apple) yield {
        NeedsApple(someApple)
      }
      needsApple must notBeNull
    }

    "can be used in a for expression with condition" in {
      val needsApple: ServiceDefinition[NeedsApple] = for (someApple <- apple; if true) yield {
        NeedsApple(someApple)
      }
      needsApple must notBeNull
    }

    "can be chained with other services to build a new service definition" in {
      val appleTree: ServiceDefinition[AppleTree] = for {
        someApple <- apple
        someTree <- tree
        someWater <- water
      } yield {
        AppleTree(someTree, someApple)
      }

      appleTree must notBeNull
    }

    "collects the service from the service context" in {
      context.head(By.byType[Apple]) returns Some(new Apple {})

      val someApple = apple(context)

      someApple must beSome[Apple]
      there was one(context).head(any[By[Apple]])
    }
  }
}

trait Apple

trait Tree

trait Water

case class AppleTree(tree: Tree, apple: Apple) extends Apple with Tree

case class NeedsApple(apple: Apple)