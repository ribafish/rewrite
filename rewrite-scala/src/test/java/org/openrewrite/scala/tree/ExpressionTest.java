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

class ExpressionTest implements RewriteTest {

    @Test
    void ifExpression() {
        rewriteRun(
          scala(
            """
            object Test {
              val x = if (true) 1 else 0
            }
            """
          )
        );
    }

    @Test
    void ternaryLikeExpression() {
        rewriteRun(
          scala(
            """
            object Test {
              val max = if (5 > 3) 5 else 3
            }
            """
          )
        );
    }

    @Test
    void blockExpression() {
        rewriteRun(
          scala(
            """
            object Test {
              val result = {
                val x = 1
                val y = 2
                x + y
              }
            }
            """
          )
        );
    }

    @Test
    void stringConcatenation() {
        rewriteRun(
          scala(
            """
            object Test {
              val name = "World"
              val greeting = "Hello, " + name + "!"
            }
            """
          )
        );
    }

    @Test
    void multilineExpression() {
        rewriteRun(
          scala(
            """
            object Test {
              val result = 1 +
                2 +
                3
            }
            """
          )
        );
    }

    @Test
    void nestedMethodCall() {
        rewriteRun(
          scala(
            """
            object Test {
              val result = Math.max(1, Math.min(2, 3))
            }
            """
          )
        );
    }

    @Test
    void methodCallWithNamedArgs() {
        rewriteRun(
          scala(
            """
            object Test {
              def greet(name: String, greeting: String): String = greeting + " " + name
              val result = greet(name = "World", greeting = "Hello")
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
              val x = 1: Int
            }
            """
          )
        );
    }

    @Test
    void nullExpression() {
        rewriteRun(
          scala(
            """
            object Test {
              val x: String = null
            }
            """
          )
        );
    }

    @Test
    void throwExpression() {
        rewriteRun(
          scala(
            """
            object Test {
              def fail(): Nothing = throw new RuntimeException("error")
            }
            """
          )
        );
    }
}
