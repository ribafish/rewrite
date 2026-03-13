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

class FunctionalTest implements RewriteTest {

    @Test
    void simpleLambda() {
        rewriteRun(
          scala(
            """
            object Test {
              val double = (x: Int) => x * 2
            }
            """
          )
        );
    }

    @Test
    void multiParamLambda() {
        rewriteRun(
          scala(
            """
            object Test {
              val add = (x: Int, y: Int) => x + y
            }
            """
          )
        );
    }

    @Test
    void lambdaWithBlock() {
        rewriteRun(
          scala(
            """
            object Test {
              val process = (x: Int) => {
                val doubled = x * 2
                doubled + 1
              }
            }
            """
          )
        );
    }

    @Test
    void methodReturningFunction() {
        rewriteRun(
          scala(
            """
            object Test {
              def adder(x: Int): Int => Int = (y: Int) => x + y
            }
            """
          )
        );
    }

    @Test
    void chainedMethodCalls() {
        rewriteRun(
          scala(
            """
            object Test {
              val result = List(1, 2, 3, 4, 5)
                .filter(_ > 2)
                .map(_ * 2)
            }
            """
          )
        );
    }

    @Test
    void chainedMethodCallsWithExplicitLambda() {
        rewriteRun(
          scala(
            """
            object Test {
              val result = List(1, 2, 3)
                .map(x => x * 2)
                .filter(x => x > 3)
            }
            """
          )
        );
    }

    @Test
    void partialFunction() {
        rewriteRun(
          scala(
            """
            object Test {
              val list = List(1, 2, 3, 4, 5)
              val even = list.collect {
                case x if x % 2 == 0 => x
              }
            }
            """
          )
        );
    }

    @Test
    void foldLeft() {
        rewriteRun(
          scala(
            """
            object Test {
              val sum = List(1, 2, 3).foldLeft(0)(_ + _)
            }
            """
          )
        );
    }

    @Test
    void flatMap() {
        rewriteRun(
          scala(
            """
            object Test {
              val result = List(List(1, 2), List(3, 4)).flatMap(x => x)
            }
            """
          )
        );
    }

    @Test
    void multipleParameterLists() {
        rewriteRun(
          scala(
            """
            object Test {
              def fold(z: Int)(op: (Int, Int) => Int): Int = z
            }
            """
          )
        );
    }

    @Test
    void multipleParameterListsThree() {
        rewriteRun(
          scala(
            """
            object Test {
              def curried(a: Int)(b: String)(c: Boolean): String = b
            }
            """
          )
        );
    }

    @Test
    void implicitParameterList() {
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
    void multipleParameterListsWithTypeParams() {
        rewriteRun(
          scala(
            """
            object Test {
              def foldLeft[A, B](list: List[A])(z: B)(op: (B, A) => B): B = list.foldLeft(z)(op)
            }
            """
          )
        );
    }

    @Test
    void emptySecondParameterList() {
        rewriteRun(
          scala(
            """
            object Test {
              def run(name: String)(): Unit = println(name)
            }
            """
          )
        );
    }

    @Test
    void curriedFunctionCall() {
        rewriteRun(
          scala(
            """
            object Test {
              def add(x: Int)(y: Int): Int = x + y
              val result = add(1)(2)
            }
            """
          )
        );
    }

    @Test
    void curriedFunctionCallThree() {
        rewriteRun(
          scala(
            """
            object Test {
              def f(a: Int)(b: Int)(c: Int): Int = a + b + c
              val result = f(1)(2)(3)
            }
            """
          )
        );
    }

    @Test
    void foldLeftCurried() {
        rewriteRun(
          scala(
            """
            object Test {
              val sum = List(1, 2, 3).foldLeft(0)((a, b) => a + b)
            }
            """
          )
        );
    }

    @Test
    void typeAppliedWithArgs() {
        rewriteRun(
          scala(
            """
            object Test {
              def identity[T](x: T): T = x
              val result = identity[Int](42)
            }
            """
          )
        );
    }

    @Test
    void methodWithByNameParam() {
        rewriteRun(
          scala(
            """
            object Test {
              def runTwice(body: => Unit): Unit = {
                body
                body
              }
            }
            """
          )
        );
    }
}
