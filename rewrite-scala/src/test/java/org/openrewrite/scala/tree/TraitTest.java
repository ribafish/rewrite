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

class TraitTest implements RewriteTest {

    @Test
    void simpleTrait() {
        rewriteRun(
          scala(
            """
            trait Greeter {
              def greet(name: String): String
            }
            """
          )
        );
    }

    @Test
    void traitWithDefaultMethod() {
        rewriteRun(
          scala(
            """
            trait Greeter {
              def greet(name: String): String = "Hello, " + name
            }
            """
          )
        );
    }

    @Test
    void traitWithMultipleMembers() {
        rewriteRun(
          scala(
            """
            trait Animal {
              def name: String
              def sound: String
              def describe(): String = name + " says " + sound
            }
            """
          )
        );
    }

    @Test
    void traitExtendsTrait() {
        rewriteRun(
          scala(
            """
            trait Base {
              def value: Int
            }

            trait Extended extends Base {
              def doubled: Int = value * 2
            }
            """
          )
        );
    }

    @Test
    void classImplementsTrait() {
        rewriteRun(
          scala(
            """
            trait Printable {
              def print(): Unit
            }

            class Document(content: String) extends Printable {
              def print(): Unit = println(content)
            }
            """
          )
        );
    }

    @Test
    void traitWithVal() {
        rewriteRun(
          scala(
            """
            trait Config {
              val maxRetries: Int = 3
              val timeout: Long = 5000L
            }
            """
          )
        );
    }

    @Test
    void sealedTrait() {
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
              def set(value: T): Unit
            }
            """
          )
        );
    }
}
