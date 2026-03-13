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

class TypesTest implements RewriteTest {

    @Test
    void tupleType() {
        rewriteRun(
          scala(
            """
            object Test {
              val pair: (Int, String) = (1, "hello")
            }
            """
          )
        );
    }

    @Test
    void functionType() {
        rewriteRun(
          scala(
            """
            object Test {
              val f: Int => String = _.toString
            }
            """
          )
        );
    }

    @Test
    void higherOrderFunction() {
        rewriteRun(
          scala(
            """
            object Test {
              def apply(f: Int => Int, x: Int): Int = f(x)
            }
            """
          )
        );
    }

    @Test
    void genericClass() {
        rewriteRun(
          scala(
            """
            class Box[T](val value: T) {
              def get: T = value
            }
            """
          )
        );
    }

    @Test
    void upperTypeBound() {
        rewriteRun(
          scala(
            """
            object Test {
              def process[T <: Comparable[T]](x: T): Int = x.compareTo(x)
            }
            """
          )
        );
    }

    @Test
    void lowerTypeBound() {
        rewriteRun(
          scala(
            """
            object Test {
              def add[B >: String](x: B): List[B] = List(x)
            }
            """
          )
        );
    }

    @Test
    void existentialType() {
        rewriteRun(
          scala(
            """
            object Test {
              val list: List[_] = List(1, 2, 3)
            }
            """
          )
        );
    }

    @Test
    void multipleTypeParameters() {
        rewriteRun(
          scala(
            """
            object Test {
              def map[A, B](list: List[A], f: A => B): List[B] = list.map(f)
            }
            """
          )
        );
    }

    @Test
    void multiParamFunctionType() {
        rewriteRun(
          scala(
            """
            object Test {
              val f: (Int, String) => Boolean = (i, s) => s.length == i
            }
            """
          )
        );
    }

    @Test
    void nestedFunctionType() {
        rewriteRun(
          scala(
            """
            object Test {
              val f: Int => Int => Int = x => y => x + y
            }
            """
          )
        );
    }

    @Test
    void typeAscription() {
        rewriteRun(
          scala(
            """
            object Test {
              val x = 1: Double
            }
            """
          )
        );
    }

    @Test
    void typeAscriptionWithParameterizedType() {
        rewriteRun(
          scala(
            """
            object Test {
              val list = Nil: List[Int]
            }
            """
          )
        );
    }

    @Test
    void functionTypeAsReturnType() {
        rewriteRun(
          scala(
            """
            object Test {
              def adder(x: Int): Int => Int = y => x + y
            }
            """
          )
        );
    }

    @Test
    void simpleTypeAlias() {
        rewriteRun(
          scala(
            """
            object Test {
              type Name = String
            }
            """
          )
        );
    }

    @Test
    void tupleExpression() {
        rewriteRun(
          scala(
            """
            object Test {
              val t = (1, "hello", true)
            }
            """
          )
        );
    }

    @Test
    void tupleReturnType() {
        rewriteRun(
          scala(
            """
            object Test {
              def pair(x: Int, y: String): (Int, String) = (x, y)
            }
            """
          )
        );
    }

    @Test
    void parameterizedTypeAlias() {
        rewriteRun(
          scala(
            """
            object Test {
              type Pair[A, B] = (A, B)
            }
            """
          )
        );
    }

    @Test
    void typeAliasWithFunctionType() {
        rewriteRun(
          scala(
            """
            object Test {
              type Callback[A] = A => Unit
            }
            """
          )
        );
    }

    @Test
    void abstractTypeMembers() {
        rewriteRun(
          scala(
            """
            trait Container {
              type Element
              def get: Element
            }
            """
          )
        );
    }

    @Test
    void unionTypeVal() {
        rewriteRun(
          scala(
            """
            object Test {
              val x: Int | String = 42
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
            trait A
            trait B
            object Test {
              def process(x: A & B): String = "ok"
            }
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
              val x: Int | String | Boolean = 42
            }
            """
          )
        );
    }

    @Test
    void unionTypeReturnType() {
        rewriteRun(
          scala(
            """
            object Test {
              def either(b: Boolean): Int | String = if (b) 1 else "hello"
            }
            """
          )
        );
    }

    @Test
    void functionTypeWithUnionParam() {
        rewriteRun(
          scala(
            """
            object Test {
              def f(g: (Int | String) => Boolean): Boolean = g(1)
            }
            """
          )
        );
    }

    @Test
    void infixTypeWithUserDefinedOp() {
        rewriteRun(
          scala(
            """
            object Test {
              type Or[A, B] = Either[A, B]
              val x: Int Or String = Left(1)
            }
            """
          )
        );
    }
}
