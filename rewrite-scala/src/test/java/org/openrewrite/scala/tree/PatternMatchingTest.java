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

class PatternMatchingTest implements RewriteTest {

    @Test
    void matchOnLiterals() {
        rewriteRun(
          scala(
            """
            object Test {
              val x = 2
              val result = x match {
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
    void matchOnType() {
        rewriteRun(
          scala(
            """
            object Test {
              def describe(x: Any): String = x match {
                case s: String => "String: " + s
                case i: Int => "Int: " + i.toString
                case _ => "Unknown"
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
              val x = 5
              val result = x match {
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
    void matchWithCaseClass() {
        rewriteRun(
          scala(
            """
            case class Person(name: String, age: Int)

            object Test {
              def greet(p: Person): String = p match {
                case Person(name, age) => "Hello " + name
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
              val x = 1
              x match {
                case 1 =>
                  println("one")
                  println("selected")
                case _ =>
                  println("other")
              }
            }
            """
          )
        );
    }

    @Test
    void nestedMatch() {
        rewriteRun(
          scala(
            """
            object Test {
              val x = (1, "hello")
              val result = x match {
                case (1, s) => s match {
                  case "hello" => "greeting"
                  case _ => "other"
                }
                case _ => "unknown"
              }
            }
            """
          )
        );
    }
}
