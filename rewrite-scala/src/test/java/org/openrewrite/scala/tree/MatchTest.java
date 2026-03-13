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

class MatchTest implements RewriteTest {

    @Test
    void simpleMatch() {
        rewriteRun(
          scala(
            """
            object Test {
              def describe(x: Int): String = x match {
                case 1 => "one"
                case 2 => "two"
                case _ => "other"
              }
            }
            """
          )
        );
    }

    @Test
    void matchWithTypePattern() {
        rewriteRun(
          scala(
            """
            object Test {
              def process(x: Any): String = x match {
                case s: String => s
                case i: Int => i.toString
                case _ => "unknown"
              }
            }
            """
          )
        );
    }

    @Test
    void matchWithGuard() {
        rewriteRun(
          scala(
            """
            object Test {
              def classify(x: Int): String = x match {
                case n if n > 0 => "positive"
                case n if n < 0 => "negative"
                case _ => "zero"
              }
            }
            """
          )
        );
    }

    @Test
    void matchWithMultipleStatements() {
        rewriteRun(
          scala(
            """
            object Test {
              def process(x: Any): Unit = x match {
                case s: String =>
                  val upper = s.toUpperCase
                  println(upper)
                case _ =>
                  println("not a string")
              }
            }
            """
          )
        );
    }

    @Test
    void matchAsExpression() {
        rewriteRun(
          scala(
            """
            object Test {
              val x = 42
              val result = x match {
                case 1 => "one"
                case _ => "other"
              }
            }
            """
          )
        );
    }

    @Test
    void matchWithTuplePattern() {
        rewriteRun(
          scala(
            """
            object Test {
              def check(pair: (Int, String)): String = pair match {
                case (1, s) => s
                case (_, s) => s"other: " + s
              }
            }
            """
          )
        );
    }
}
