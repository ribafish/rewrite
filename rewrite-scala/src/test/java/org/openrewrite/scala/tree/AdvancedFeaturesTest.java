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

class AdvancedFeaturesTest implements RewriteTest {

    @Test
    void implicitParameter() {
        rewriteRun(
          scala(
            """
            object Test {
              def greet(name: String)(implicit greeting: String): String = greeting + " " + name
            }
            """
          )
        );
    }

    @Test
    void implicitConversion() {
        rewriteRun(
          scala(
            """
            object Test {
              implicit def intToString(x: Int): String = x.toString
            }
            """
          )
        );
    }

    @Test
    void genericMethod() {
        rewriteRun(
          scala(
            """
            object Test {
              def identity[T](x: T): T = x
            }
            """
          )
        );
    }

    @Test
    void multipleImports() {
        rewriteRun(
          scala(
            """
            import scala.collection.mutable
            import scala.util.Try

            object Test {
              val map = mutable.Map[String, Int]()
            }
            """
          )
        );
    }

    @Test
    void wildcardImport() {
        rewriteRun(
          scala(
            """
            import scala.collection.mutable._

            object Test {
              val list = ListBuffer(1, 2, 3)
            }
            """
          )
        );
    }

    @Test
    void renameImport() {
        rewriteRun(
          scala(
            """
            import scala.collection.mutable.{ListBuffer => MutableList}

            object Test {
              val list = MutableList(1, 2, 3)
            }
            """
          )
        );
    }

    @Test
    void multipleClassesInFile() {
        rewriteRun(
          scala(
            """
            class Foo {
              def foo: String = "foo"
            }

            class Bar {
              def bar: String = "bar"
            }

            object Baz {
              val x = 42
            }
            """
          )
        );
    }

    @Test
    void innerClass() {
        rewriteRun(
          scala(
            """
            class Outer {
              class Inner {
                def value: Int = 42
              }
            }
            """
          )
        );
    }

    @Test
    void singleExpressionMethod() {
        rewriteRun(
          scala(
            """
            object Test {
              def add(x: Int, y: Int): Int = x + y
              def multiply(x: Int, y: Int): Int = x * y
            }
            """
          )
        );
    }

    @Test
    void unitReturnType() {
        rewriteRun(
          scala(
            """
            object Test {
              def printHello(): Unit = println("Hello")
            }
            """
          )
        );
    }

    @Test
    void typeAppliedMethodCall() {
        rewriteRun(
          scala(
            """
            object Test {
              val list = List.empty[String]
            }
            """
          )
        );
    }

    @Test
    void curriedMethodCall() {
        rewriteRun(
          scala(
            """
            object Test {
              def fold(z: Int)(op: (Int, Int) => Int): Int = z
              val result = fold(0)((a, b) => a + b)
            }
            """
          )
        );
    }

    @Test
    void unionTypeParam() {
        rewriteRun(
          scala(
            """
            object Test {
              def handle(x: Int | String): String = x.toString
            }
            """
          )
        );
    }

    @Test
    void intersectionTypeParam() {
        rewriteRun(
          scala(
            """
            trait Printable {
              def print(): String
            }
            trait Loggable {
              def log(): Unit
            }
            object Test {
              def process(x: Printable & Loggable): String = x.print()
            }
            """
          )
        );
    }
}
