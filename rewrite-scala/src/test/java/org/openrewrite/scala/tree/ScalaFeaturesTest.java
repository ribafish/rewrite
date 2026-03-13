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

class ScalaFeaturesTest implements RewriteTest {

    @Test
    void companionObject() {
        rewriteRun(
          scala(
            """
            class Person(val name: String, val age: Int)

            object Person {
              def apply(name: String): Person = new Person(name, 0)
            }
            """
          )
        );
    }

    @Test
    void overrideMethod() {
        rewriteRun(
          scala(
            """
            class Animal {
              def speak(): String = "..."
            }

            class Dog extends Animal {
              override def speak(): String = "Woof"
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
              def add(x: Int)(y: Int): Int = x + y
            }
            """
          )
        );
    }

    @Test
    void defaultParameterValues() {
        rewriteRun(
          scala(
            """
            object Test {
              def greet(name: String = "World"): String = "Hello, " + name
            }
            """
          )
        );
    }

    @Test
    void varArgs() {
        rewriteRun(
          scala(
            """
            object Test {
              def sum(nums: Int*): Int = nums.sum
            }
            """
          )
        );
    }

    @Test
    void lazyVal() {
        rewriteRun(
          scala(
            """
            object Test {
              lazy val expensive = {
                println("computing")
                42
              }
            }
            """
          )
        );
    }

    @Test
    void typeAlias() {
        rewriteRun(
          scala(
            """
            object Test {
              type StringList = List[String]
            }
            """
          )
        );
    }

    @Test
    void traitMixin() {
        rewriteRun(
          scala(
            """
            trait A {
              def a: String = "a"
            }

            trait B {
              def b: String = "b"
            }

            class C extends A with B
            """
          )
        );
    }

    @Test
    void optionPatternMatch() {
        rewriteRun(
          scala(
            """
            object Test {
              def process(opt: Option[String]): String = opt match {
                case Some(value) => value
                case None => "default"
              }
            }
            """
          )
        );
    }

    @Test
    void forYield() {
        rewriteRun(
          scala(
            """
            object Test {
              val doubled = for (x <- List(1, 2, 3)) yield x * 2
            }
            """
          )
        );
    }

    @Test
    void mapAndFilter() {
        rewriteRun(
          scala(
            """
            object Test {
              val nums = List(1, 2, 3, 4, 5)
              val even = nums.filter(_ % 2 == 0)
              val doubled = nums.map(_ * 2)
            }
            """
          )
        );
    }

    @Test
    void multilineString() {
        rewriteRun(
          scala(
            """
            object Test {
              val text = \"\"\"
                |Hello
                |World
                |\"\"\".stripMargin
            }
            """
          )
        );
    }

    @Test
    void implicitClass() {
        rewriteRun(
          scala(
            """
            object Extensions {
              implicit class RichInt(val n: Int) {
                def isEven: Boolean = n % 2 == 0
              }
            }
            """
          )
        );
    }

    @Test
    void nestedMethods() {
        rewriteRun(
          scala(
            """
            object Test {
              def outer(x: Int): Int = {
                def inner(y: Int): Int = x + y
                inner(10)
              }
            }
            """
          )
        );
    }

    @Test
    void namedArguments() {
        rewriteRun(
          scala(
            """
            object Test {
              def point(x: Int, y: Int): String = x.toString + "," + y.toString
              val p = point(y = 2, x = 1)
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
              def swap(pair: (Int, String)): (String, Int) = (pair._2, pair._1)
            }
            """
          )
        );
    }

    @Test
    void wildcardTypeParam() {
        rewriteRun(
          scala(
            """
            object Test {
              def printAll(list: List[_]): Unit = list.foreach(println)
            }
            """
          )
        );
    }

    @Test
    void caseObjectEnum() {
        rewriteRun(
          scala(
            """
            sealed trait Color
            case object Red extends Color
            case object Green extends Color
            case object Blue extends Color
            """
          )
        );
    }

    @Test
    void typeAppliedMethodCallDot() {
        rewriteRun(
          scala(
            """
            object Test {
              val list = List.empty[Int]
              val head = list.headOption.getOrElse(0)
            }
            """
          )
        );
    }

    @Test
    void curriedFoldLeft() {
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
    void unionTypeMatch() {
        rewriteRun(
          scala(
            """
            object Test {
              def show(x: Int | String): String = x match {
                case i: Int => i.toString
                case s: String => s
              }
            }
            """
          )
        );
    }
}
