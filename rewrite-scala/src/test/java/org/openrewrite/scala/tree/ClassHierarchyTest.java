/*
 * Copyright 2025 the original author or authors.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * https://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openrewrite.scala.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.scala.Assertions.scala;

class ClassHierarchyTest implements RewriteTest {

    @Test
    void abstractClass() {
        rewriteRun(
          scala(
            """
            abstract class Shape {
              def area: Double
            }
            """
          )
        );
    }

    @Test
    void abstractClassWithConcreteMethod() {
        rewriteRun(
          scala(
            """
            abstract class Shape {
              def area: Double
              def describe: String = "I am a shape"
            }
            """
          )
        );
    }

    @Test
    void classExtendsAbstract() {
        rewriteRun(
          scala(
            """
            abstract class Shape {
              def area: Double
            }

            class Circle(val radius: Double) extends Shape {
              def area: Double = 3.14 * radius * radius
            }
            """
          )
        );
    }

    @Test
    void sealedTraitWithCaseClasses() {
        rewriteRun(
          scala(
            """
            sealed trait Shape
            case class Circle(radius: Double) extends Shape
            case class Rectangle(width: Double, height: Double) extends Shape
            """
          )
        );
    }

    @Test
    void traitWithTypeParameter() {
        rewriteRun(
          scala(
            """
            trait Container[T] {
              def get: T
              def put(value: T): Unit
            }
            """
          )
        );
    }

    @Test
    void classWithMultipleConstructorParams() {
        rewriteRun(
          scala(
            """
            class Person(val name: String, val age: Int, val email: String)
            """
          )
        );
    }

    @Test
    void classWithPrivateConstructor() {
        rewriteRun(
          scala(
            """
            class Singleton private(val value: Int)
            """
          )
        );
    }

    @Test
    void classWithSecondaryConstructor() {
        rewriteRun(
          scala(
            """
            class Point(val x: Int, val y: Int) {
              def this(x: Int) = this(x, 0)
            }
            """
          )
        );
    }

    @Test
    void objectWithApply() {
        rewriteRun(
          scala(
            """
            object Factory {
              def apply(name: String): String = "Hello, " + name
            }
            """
          )
        );
    }

    @Test
    void caseClassWithMethods() {
        rewriteRun(
          scala(
            """
            case class Point(x: Int, y: Int) {
              def distanceTo(other: Point): Double = {
                val dx = x - other.x
                val dy = y - other.y
                Math.sqrt(dx * dx + dy * dy)
              }
            }
            """
          )
        );
    }

    @Test
    void thisReference() {
        rewriteRun(
          scala(
            """
            class Person(val name: String) {
              def greet(): String = this.name
            }
            """
          )
        );
    }

    @Test
    void superMethodCall() {
        rewriteRun(
          scala(
            """
            class Animal {
              def sound: String = "..."
            }
            class Dog extends Animal {
              override def sound: String = super.sound + " Woof"
            }
            """
          )
        );
    }
}
