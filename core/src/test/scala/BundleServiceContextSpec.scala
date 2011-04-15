package com.weiglewilczek.scalamodules

import org.specs.Specification
import org.specs.mock.Mockito
import org.osgi.framework.{ServiceReference, BundleContext}

/**
 * @author mathias.broekelmann
 * @since 15.04.11 10:05
 */
class BundleServiceContextSpec extends Specification with Mockito {
  
  "bundle service context" should {
    val bundleContext = mock[BundleContext]
    val appleReference = mock[ServiceReference]
    val serviceContext: ServiceContext = new BundleServiceContext(bundleContext)
    val byApple = By.byType[Apple]
    val someApple = new Apple {}

    "collect empty services refereces for a given by clause if service not found" in {
      bundleContext.getServiceReferences(classOf[Apple].getName, null) returns null
      val apples = serviceContext.collect(byApple)
      apples must beEmpty
    }

    "collect returns service for a given by clause if service is found" in {
      bundleContext.getServiceReferences(classOf[Apple].getName, null) returns Array(appleReference)
      bundleContext.getService(appleReference) returns someApple
      val apples = serviceContext.collect(byApple)
      apples must haveSize(1)
      apples.head must be(someApple)
    }

    "collect returns no service for a given by clause if service reference is valid but the service is not available any more" in {
      bundleContext.getServiceReferences(classOf[Apple].getName, null) returns Array(appleReference)
      bundleContext.getService(appleReference) returns null
      val apples = serviceContext.collect(byApple)
      apples must beEmpty
    }
  }
}