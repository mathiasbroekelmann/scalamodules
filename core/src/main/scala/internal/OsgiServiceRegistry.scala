package com.weiglewilczek.scalamodules.internal

import org.osgi.framework.{BundleContext}
import com.weiglewilczek.scalamodules.{Filter, ServiceRegistry}

/**
 * @author mathias
 * @since 22.04.11
 */
class OsgiServiceRegistry(context: BundleContext) extends ServiceRegistry {
  def collect[A](clazz: Class[A], filter: Option[Filter]) = new OsgiServiceImports[A](context, clazz, filter)
}