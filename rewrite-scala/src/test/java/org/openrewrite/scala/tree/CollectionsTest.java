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

class CollectionsTest implements RewriteTest {

    @Test
    void listCreation() {
        rewriteRun(
          scala(
            """
            object Test {
              val list = List(1, 2, 3)
            }
            """
          )
        );
    }

    @Test
    void mapCreation() {
        rewriteRun(
          scala(
            """
            object Test {
              val map = Map("a" -> 1, "b" -> 2)
            }
            """
          )
        );
    }

    @Test
    void setCreation() {
        rewriteRun(
          scala(
            """
            object Test {
              val set = Set(1, 2, 3)
            }
            """
          )
        );
    }

    @Test
    void tupleCreation() {
        rewriteRun(
          scala(
            """
            object Test {
              val pair = (1, "hello")
            }
            """
          )
        );
    }

    @Test
    void listWithTypeParam() {
        rewriteRun(
          scala(
            """
            object Test {
              val list: List[String] = List("a", "b", "c")
            }
            """
          )
        );
    }

    @Test
    void mapWithTypeParams() {
        rewriteRun(
          scala(
            """
            object Test {
              val map: Map[String, Int] = Map("a" -> 1)
            }
            """
          )
        );
    }

    @Test
    void listConcatenation() {
        rewriteRun(
          scala(
            """
            object Test {
              val a = List(1, 2)
              val b = List(3, 4)
              val c = a ++ b
            }
            """
          )
        );
    }

    @Test
    void listPrepend() {
        rewriteRun(
          scala(
            """
            object Test {
              val list = List(2, 3)
              val newList = 1 :: list
            }
            """
          )
        );
    }

    @Test
    void optionSomeNone() {
        rewriteRun(
          scala(
            """
            object Test {
              val some: Option[Int] = Some(42)
              val none: Option[Int] = None
            }
            """
          )
        );
    }

    @Test
    void rangeExpression() {
        rewriteRun(
          scala(
            """
            object Test {
              val range = 1 to 10
              val until = 0 until 5
            }
            """
          )
        );
    }
}
