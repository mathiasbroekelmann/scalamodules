package com.weiglewilczek.scalamodules

/**
 * @author mathias
 * @since 22.04.11
 */
trait Imports[A] {

  def head: Import[A]

  def map[B](call: ServiceImport[A] => B): Iterable[B]

  def flatMap[B](call: ServiceImport[A] => Iterable[B]): Iterable[B]
}