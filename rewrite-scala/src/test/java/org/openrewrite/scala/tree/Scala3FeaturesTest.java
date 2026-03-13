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

class Scala3FeaturesTest implements RewriteTest {

    @Test
    void enumType() {
        rewriteRun(
          scala(
            """
            enum Color {
              case Red, Green, Blue
            }
            """
          )
        );
    }

    @Test
    void enumWithParameters() {
        rewriteRun(
          scala(
            """
            enum Planet(mass: Double, radius: Double) {
              case Mercury extends Planet(3.303e+23, 2.4397e6)
              case Venus extends Planet(4.869e+24, 6.0518e6)
            }
            """
          )
        );
    }

    @Test
    void extensionMethod() {
        rewriteRun(
          scala(
            """
            extension (s: String)
              def greet: String = "Hello, " + s
            """
          )
        );
    }

    @Test
    void usingParameter() {
        rewriteRun(
          scala(
            """
            object Test {
              def max[T](x: T, y: T)(using ord: Ordering[T]): T =
                if (ord.compare(x, y) >= 0) x else y
            }
            """
          )
        );
    }

    @Test
    @org.junit.jupiter.api.Disabled("Scala 3 given/with not yet supported")
    void givenInstance() {
        rewriteRun(
          scala(
            """
            given intOrdering: Ordering[Int] with {
              def compare(x: Int, y: Int): Int = x - y
            }
            """
          )
        );
    }

    @Test
    void opaqueType() {
        rewriteRun(
          scala(
            """
            object Types {
              opaque type Name = String
            }
            """
          )
        );
    }

    @Test
    void unionType() {
        rewriteRun(
          scala(
            """
            object Test {
              def process(input: String | Int): String = input match {
                case s: String => s
                case i: Int => i.toString
              }
            }
            """
          )
        );
    }

    @Test
    void intersectionType() {
        rewriteRun(
          scala(
            """
            trait A {
              def a: String = "a"
            }
            trait B {
              def b: String = "b"
            }
            object Test {
              def process(x: A & B): String = x.a + x.b
            }
            """
          )
        );
    }

    @Test
    void topLevelDefinitions() {
        rewriteRun(
          scala(
            """
            def hello(): String = "Hello"
            val greeting = "World"
            """
          )
        );
    }

    @Test
    void chainedUnionType() {
        rewriteRun(
          scala(
            """
            object Test {
              def process(x: Int | String | Boolean): String = x.toString
            }
            """
          )
        );
    }

    @Test
    void mixedInfixTypes() {
        rewriteRun(
          scala(
            """
            trait A
            trait B
            trait C
            object Test {
              def process(x: (A & B) | C): String = "ok"
            }
            """
          )
        );
    }

    @Test
    void typeAliasWithUnion() {
        rewriteRun(
          scala(
            """
            object Test {
              type StringOrInt = String | Int
            }
            """
          )
        );
    }

    @Test
    void unionTypeReturn() {
        rewriteRun(
          scala(
            """
            object Test {
              def parse(s: String): Int | String = {
                try {
                  s.toInt
                } catch {
                  case _: NumberFormatException => s
                }
              }
            }
            """
          )
        );
    }

    @Test
    void enumWithMethods() {
        rewriteRun(
          scala(
            """
            enum Direction {
              case North, South, East, West

              def opposite: Direction = this match {
                case North => South
                case South => North
                case East => West
                case West => East
              }
            }
            """
          )
        );
    }

    @Test
    void usingParameterTypeApplied() {
        rewriteRun(
          scala(
            """
            object Test {
              def sorted[T](list: List[T])(using ord: Ordering[T]): List[T] = list.sorted
            }
            """
          )
        );
    }
}
