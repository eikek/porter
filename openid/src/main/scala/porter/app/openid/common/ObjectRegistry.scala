package porter.app.openid.common

import java.util.concurrent.atomic.AtomicReference

abstract class ObjectRegistry {

  type Elem

  private val objects = new AtomicReference(Vector.empty[Elem])

  @scala.annotation.tailrec
  final protected def register(e: Elem): Elem = {
    val v = objects.get()
    val next = v :+ e
    if (! objects.compareAndSet(v, next)) {
      register(e)
    } else {
      e
    }
  }

  final def all: Vector[Elem] = objects.get()
}
