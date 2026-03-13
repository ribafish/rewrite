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
package org.openrewrite.scala.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.util.Spans
import org.openrewrite.Tree
import org.openrewrite.java.tree.*
import org.openrewrite.marker.Markers
import org.openrewrite.scala.marker.Implicit
import org.openrewrite.scala.marker.LambdaParameter
import org.openrewrite.scala.marker.OmitBraces
import org.openrewrite.scala.marker.SObject
import org.openrewrite.scala.marker.ScalaForLoop
import org.openrewrite.scala.marker.ScalaLazyVal
import org.openrewrite.scala.marker.{TypeBoundOperator, TypeProjection, TypeReferencePrefix}
import org.openrewrite.scala.marker.{AdditionalParameterLists, MethodBody}
import org.openrewrite.scala.marker.UnderscorePlaceholderLambda
import org.openrewrite.scala.tree.S

import java.util
import java.util.{Collections, Arrays, UUID}

/**
 * Visitor that traverses the Scala compiler AST and builds OpenRewrite LST nodes.
 */
class ScalaTreeVisitor(source: String, offsetAdjustment: Int = 0)(implicit ctx: Context) {
  
  private var cursor = 0
  private var isInImportContext = false
  private var currentSyntheticParams: Set[String] = Set.empty
  
  def getCursor: Int = cursor
  
  def updateCursor(position: Int): Unit = {
    val adjustedPosition = Math.max(0, position - offsetAdjustment)
    if (adjustedPosition > cursor && adjustedPosition <= source.length) {
      cursor = adjustedPosition
    }
  }

  /** Convert a J to a Statement, wrapping Expressions in S.ExpressionStatement if needed */
  private def asStatement(j: J): Statement = j match {
    case stmt: Statement => stmt
    case expr: Expression => new S.ExpressionStatement(expr)
    case _ => throw new IllegalArgumentException(s"Cannot convert ${j.getClass.getSimpleName} to Statement")
  }
  
  def visitTree(tree: untpd.Tree): J = {
    tree match {
      case _ if tree.isEmpty => visitUnknown(tree)
      case lit: untpd.Literal => visitLiteral(lit)
      case num: untpd.Number => visitNumber(num)
      case id: untpd.Ident => visitIdent(id)
      case app: untpd.Apply => visitApply(app)
      case sel: untpd.Select => visitSelect(sel)
      case infixOp: untpd.InfixOp => visitInfixOp(infixOp)
      case prefixOp: untpd.PrefixOp => visitPrefixOp(prefixOp)
      case postfixOp: untpd.PostfixOp => visitPostfixOp(postfixOp)
      case parens: untpd.Parens => visitParentheses(parens)
      case imp: untpd.Import => visitImport(imp)
      case pkg: untpd.PackageDef => visitPackageDef(pkg)
      case newTree: untpd.New => visitNew(newTree)
      case vd: untpd.ValDef => visitValDef(vd)
      case md: untpd.ModuleDef => visitModuleDef(md)
      case asg: untpd.Assign => visitAssign(asg)
      case ifTree: untpd.If => visitIf(ifTree)
      case whileTree: untpd.WhileDo => visitWhileDo(whileTree)
      case forTree: untpd.ForDo => visitForDo(forTree)
      case block: untpd.Block => visitBlock(block)
      case td: untpd.TypeDef if !td.isClassDef => visitTypeAlias(td)
      case td: untpd.TypeDef if td.isClassDef => visitClassDef(td)
      case dd: untpd.DefDef => visitDefDef(dd)
      case ret: untpd.Return => visitReturn(ret)
      case thr: untpd.Throw => visitThrow(thr)
      case parsedTry: untpd.ParsedTry => visitParsedTry(parsedTry)
      case tryTree: untpd.Try => visitTry(tryTree)
      case ta: untpd.TypeApply => visitTypeApply(ta)
      case at: untpd.AppliedTypeTree => visitAppliedTypeTree(at)
      case func: untpd.Function => visitFunction(func)
      case typed: untpd.Typed => visitTyped(typed)
      case namedArg: untpd.NamedArg => visitNamedArg(namedArg)
      case patDef: untpd.PatDef => visitPatDef(patDef)
      case interp: untpd.InterpolatedString => visitInterpolatedString(interp)
      case m: untpd.Match => visitMatch(m)
      case bnt: untpd.ByNameTypeTree => visitByNameTypeTree(bnt)
      case thisTree: untpd.This => visitThis(thisTree)
      case superTree: untpd.Super => visitSuper(superTree)
      case tuple: untpd.Tuple => visitTuple(tuple)
      case forYield: untpd.ForYield => visitForYield(forYield)
      case _ => visitUnknown(tree)
    }
  }
  
  private def visitLiteral(lit: untpd.Literal): J.Literal = {
    val prefix = extractPrefix(lit.span)
    val value = lit.const.value
    val valueSource = extractSource(lit.span)
    val javaType = constantToJavaType(lit.const)
    
    new J.Literal(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      value,
      valueSource,
      Collections.emptyList(),
      javaType
    )
  }
  
  private def visitNumber(num: untpd.Number): J.Literal = {
    val prefix = extractPrefix(num.span)
    val valueSource = extractSource(num.span)
    
    // Parse the number to determine its type and value
    val (value: Any, javaType: JavaType.Primitive) = valueSource match {
      case s if s.startsWith("0x") || s.startsWith("0X") =>
        // Hexadecimal literal
        val hexStr = s.substring(2)
        val longVal = java.lang.Long.parseLong(hexStr, 16)
        if (longVal <= Integer.MAX_VALUE) {
          (java.lang.Integer.valueOf(longVal.toInt), JavaType.Primitive.Int)
        } else {
          (java.lang.Long.valueOf(longVal), JavaType.Primitive.Long)
        }
      case s if s.endsWith("L") || s.endsWith("l") => 
        (java.lang.Long.valueOf(s.dropRight(1)), JavaType.Primitive.Long)
      case s if s.endsWith("F") || s.endsWith("f") => 
        (java.lang.Float.valueOf(s.dropRight(1)), JavaType.Primitive.Float)
      case s if s.endsWith("D") || s.endsWith("d") => 
        (java.lang.Double.valueOf(s.dropRight(1)), JavaType.Primitive.Double)
      case s if s.contains(".") || s.contains("e") || s.contains("E") =>
        (java.lang.Double.valueOf(s), JavaType.Primitive.Double)
      case s =>
        try {
          (java.lang.Integer.valueOf(s), JavaType.Primitive.Int)
        } catch {
          case _: NumberFormatException =>
            (java.lang.Long.valueOf(s), JavaType.Primitive.Long)
        }
    }
    
    new J.Literal(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      value,
      valueSource,
      Collections.emptyList(),
      javaType
    )
  }
  
  private def visitInterpolatedString(interp: untpd.InterpolatedString): J = {
    val prefix = extractPrefix(interp.span)
    val interpolatorName = interp.id.toString

    // Extract the full source text for the interpolated string
    val fullSource = extractSource(interp.span)

    // Parse the source text to extract parts
    // The source includes the interpolator prefix and quotes: s"Hello, $name"
    // We need to split it into literal segments and expression references
    val parts = new util.ArrayList[J]()

    // Skip past interpolator name and opening quote to get content
    val contentStart = fullSource.indexOf('"')
    if (contentStart < 0) {
      // Fallback: couldn't parse, return as unknown
      cursor = cursor - fullSource.length // reset cursor since extractSource advanced it
      return visitUnknown(interp)
    }
    val content = fullSource.substring(contentStart + 1, fullSource.length - 1) // strip quotes

    // Parse the content: split on $ references
    var i = 0
    val sb = new StringBuilder()
    while (i < content.length) {
      if (content.charAt(i) == '$') {
        // Flush accumulated literal text
        if (sb.nonEmpty) {
          parts.add(new J.Literal(
            Tree.randomId(), Space.EMPTY, Markers.EMPTY,
            sb.toString(), sb.toString(), Collections.emptyList(), JavaType.Primitive.String
          ))
          sb.clear()
        }
        // Parse the interpolated expression
        i += 1
        if (i < content.length && content.charAt(i) == '{') {
          // ${expr} form - find matching closing brace
          val exprStart = i + 1
          var braceDepth = 1
          i += 1
          while (i < content.length && braceDepth > 0) {
            if (content.charAt(i) == '{') braceDepth += 1
            else if (content.charAt(i) == '}') braceDepth -= 1
            if (braceDepth > 0) i += 1
          }
          val exprText = content.substring(exprStart, i)
          i += 1 // skip closing brace
          val exprSource = "${" + exprText + "}"
          parts.add(new J.Unknown(
            Tree.randomId(), Space.EMPTY, Markers.EMPTY,
            new J.Unknown.Source(Tree.randomId(), Space.EMPTY, Markers.EMPTY, exprSource)
          ))
        } else {
          // $name form - read identifier
          val nameStart = i
          while (i < content.length && (Character.isLetterOrDigit(content.charAt(i)) || content.charAt(i) == '_')) {
            i += 1
          }
          val nameText = content.substring(nameStart, i)
          val exprSource = "$" + nameText
          parts.add(new J.Unknown(
            Tree.randomId(), Space.EMPTY, Markers.EMPTY,
            new J.Unknown.Source(Tree.randomId(), Space.EMPTY, Markers.EMPTY, exprSource)
          ))
        }
      } else {
        sb.append(content.charAt(i))
        i += 1
      }
    }
    // Flush remaining literal text
    if (sb.nonEmpty) {
      parts.add(new J.Literal(
        Tree.randomId(), Space.EMPTY, Markers.EMPTY,
        sb.toString(), sb.toString(), Collections.emptyList(), JavaType.Primitive.String
      ))
    }

    new S.InterpolatedString(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      interpolatorName,
      parts,
      JavaType.Primitive.String
    )
  }

  private def visitIdent(id: untpd.Ident): J = {
    val prefix = extractPrefix(id.span)
    val sourceText = extractSource(id.span) // Extract source to move cursor
    var simpleName = id.name.toString
    
    // Special handling for wildcard imports: convert Scala's "_" to Java's "*"
    // This is needed because J.Import expects "*" for wildcard imports
    if (simpleName == "_" && isInImportContext) {
      simpleName = "*"
      new J.Identifier(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        Collections.emptyList(),
        simpleName,
        null, // type will be set later
        null  // variable will be set later
      )
    } else if (simpleName == "_" || simpleName.matches("_\\$\\d+") || (currentSyntheticParams.nonEmpty && currentSyntheticParams.contains(simpleName))) {
      // This is an expression wildcard for partially applied functions or pattern matching
      // Synthetic parameters like _$1, _$2 are always wildcards (generated by compiler for underscore placeholders)
      val wildcard = new S.Wildcard(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        null // type will be set later
      )
      wildcard
    } else {
      new J.Identifier(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        Collections.emptyList(),
        simpleName,
        null, // type will be set later
        null  // variable will be set later
      )
    }
  }
  
  private def visitApply(app: untpd.Apply): J = {
    // In Scala, binary operations like "1 + 2" are parsed as Apply(Select(1, +), List(2))
    // Unary operations like "-x" are parsed as Apply(Select(x, unary_-), List())
    // Constructor calls like "new Person()" are parsed as Apply(New(Person), List())
    // Annotations like "@deprecated" are parsed as Apply(Select(New(Ident(deprecated)), <init>), List())
    
    // Check if this is an annotation pattern (will be handled specially when called from visitClassDef)
    // Annotations look like Apply(Select(New(...), <init>), args) with @ in source
    // Constructor calls look the same but have "new" in source
    val isAnnotationPattern = app.fun match {
      case sel: untpd.Select if sel.name.toString == "<init>" =>
        sel.qualifier match {
          case newNode: untpd.New =>
            // Check if the source has @ before the type (annotation) or "new" (constructor)
            if (app.span.exists) {
              val adjustedStart = Math.max(0, app.span.start - offsetAdjustment)
              val adjustedEnd = Math.max(0, app.span.end - offsetAdjustment)
              if (adjustedStart < adjustedEnd && adjustedEnd <= source.length) {
                val sourceText = source.substring(adjustedStart, adjustedEnd)
                sourceText.trim.startsWith("@")
              } else {
                false
              }
            } else {
              false
            }
          case _ => false
        }
      case _ => false
    }
    
    if (isAnnotationPattern) {
      // This is an annotation - convert to J.Annotation
      return visitAnnotation(app)
    }
    
    app.fun match {
      case newTree: untpd.New =>
        // This is a constructor call with arguments (shouldn't happen in Scala 3)
        visitNewClassWithArgs(newTree, app)
      case sel: untpd.Select if sel.name.toString == "<init>" =>
        // This is a constructor call like new Person()
        sel.qualifier match {
          case newTree: untpd.New =>
            visitNewClassWithArgs(newTree, app)
          case _ =>
            visitUnknown(app)
        }
      case sel: untpd.Select if app.args.isEmpty && isUnaryOperator(sel.name.toString) =>
        // This is a unary operation
        visitUnary(sel)
      case sel: untpd.Select if app.args.length == 1 && isBinaryOperator(sel.name.toString) =>
        // This is likely a binary operation (infix notation)
        visitBinary(sel, app.args.head, Some(app.span))
      case sel: untpd.Select =>
        // Method call with dot notation like "obj.method(args)"
        if (isBlockArgumentCall(app)) {
          visitBlockArgMethodInvocation(app, sel)
        } else {
          visitMethodInvocation(app)
        }
      case id: untpd.Ident =>
        // Function application syntax: func(args)
        // This includes array access (arr(0)), function calls, and more
        if (isBlockArgumentCall(app)) {
          visitBlockArgFunctionApplication(app, id)
        } else {
          visitFunctionApplication(app, id)
        }
      case innerApp: untpd.Apply =>
        // Curried function call like add(1)(2) or matrix(0)(1)
        visitCurriedApplication(app)
      case typeApp: untpd.TypeApply =>
        // Type-applied calls like List.empty[Int], obj.method[T](args), f[T](args)
        visitTypeAppliedCall(app, typeApp)
      case _ =>
        // Other kinds of applications
        visitUnknown(app)
    }
  }
  
  private def visitUnary(sel: untpd.Select): J.Unary = {
    val expr = visitTree(sel.qualifier).asInstanceOf[Expression]
    val operator = mapUnaryOperator(sel.name.toString)
    
    new J.Unary(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      JLeftPadded.build(operator),
      expr,
      null // type will be set later
    )
  }
  
  private def isUnaryOperator(name: String): Boolean = {
    name match {
      case "unary_-" | "unary_+" | "unary_!" | "unary_~" => true
      case _ => false
    }
  }
  
  private def visitAnnotation(app: untpd.Apply): J.Annotation = {
    val prefix = extractPrefix(app.span)
    
    
    // Extract the annotation type and arguments
    val (annotationType, args) = app.fun match {
      case sel: untpd.Select if sel.name.toString == "<init>" =>
        sel.qualifier match {
          case newTree: untpd.New =>
            val typeIdent = newTree.tpt match {
              case id: untpd.Ident => id
              case _ => return visitUnknown(app).asInstanceOf[J.Annotation]
            }
            (typeIdent, app.args)
          case _ => return visitUnknown(app).asInstanceOf[J.Annotation]
        }
      case _ => return visitUnknown(app).asInstanceOf[J.Annotation]
    }
    
    // Create the annotation type
    val annotName = annotationType.name.toString
    val annotTypeTree = new J.Identifier(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      Collections.emptyList(),
      annotName,
      null,
      null
    )

    // Advance cursor past @AnnotationType before visiting arguments
    val searchFrom = cursor
    val searchEnd = Math.min(cursor + annotName.length + 10, source.length)
    val searchText = source.substring(searchFrom, searchEnd)
    val atIdx = searchText.indexOf('@')
    if (atIdx >= 0) {
      cursor = searchFrom + atIdx + 1 + annotName.length
    }

    // Convert arguments
    val arguments = if (args.isEmpty) {
      null
    } else {
      // Advance cursor past the opening paren
      val parenSearchEnd = Math.min(cursor + 10, source.length)
      val parenSearch = source.substring(cursor, parenSearchEnd)
      val parenIdx = parenSearch.indexOf('(')
      if (parenIdx >= 0) {
        cursor = cursor + parenIdx + 1
      }

      val argList = new util.ArrayList[JRightPadded[Expression]]()
      var ai = 0
      while (ai < args.length) {
        val expr = visitTree(args(ai)).asInstanceOf[Expression]
        val after = if (ai < args.length - 1) {
          val commaEnd = Math.min(cursor + 20, source.length)
          val commaSearch = source.substring(cursor, commaEnd)
          val commaIdx = commaSearch.indexOf(',')
          if (commaIdx >= 0) {
            cursor = cursor + commaIdx + 1
          }
          Space.EMPTY
        } else {
          Space.EMPTY
        }
        argList.add(JRightPadded.build(expr).withAfter(after))
        ai += 1
      }
      JContainer.build(
        Space.EMPTY,
        argList,
        Markers.EMPTY
      )
    }
    
    val annotation = new J.Annotation(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      annotTypeTree,
      arguments
    )
    
    // Update cursor to the end of the annotation
    val adjustedEnd = Math.max(0, app.span.end - offsetAdjustment)
    if (adjustedEnd > cursor) {
      cursor = adjustedEnd
    }
    
    
    annotation
  }
  
  private def mapUnaryOperator(op: String): J.Unary.Type = op match {
    case "unary_-" => J.Unary.Type.Negative
    case "unary_+" => J.Unary.Type.Positive
    case "unary_!" => J.Unary.Type.Not
    case "unary_~" => J.Unary.Type.Complement
    case _ => J.Unary.Type.Not // default
  }
  
  private def visitPrefixOp(prefixOp: untpd.PrefixOp): J.Unary = {
    val prefix = extractPrefix(prefixOp.span)
    val operator = mapPrefixOperator(prefixOp.op.name.toString)
    
    // Update cursor to the end of the operator
    updateCursor(prefixOp.op.span.end)
    
    // Now visit the expression
    val expr = visitTree(prefixOp.od) match {
      case e: Expression => e
      case _ => return visitUnknown(prefixOp).asInstanceOf[J.Unary]
    }
    
    new J.Unary(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      JLeftPadded.build(operator).withBefore(Space.EMPTY),
      expr,
      JavaType.Primitive.Boolean
    )
  }
  
  private def visitPostfixOp(postfixOp: untpd.PostfixOp): J = {
    // Check if this is a member reference (method _)
    postfixOp.op match {
      case id: untpd.Ident if id.name.toString == "_" =>
        // This is a member reference like "greet _"
        // Extract prefix for the entire member reference expression
        val prefix = extractPrefix(postfixOp.span)
        
        // Visit the containing expression (e.g., "greet")
        // It already has its content, we just need to convert it to a J node
        val expr = visitTree(postfixOp.od) match {
          case id: J.Identifier => id.withPrefix(Space.EMPTY)
          case fa: J.FieldAccess => fa.withPrefix(Space.EMPTY)
          case mi: J.MethodInvocation => mi.withPrefix(Space.EMPTY)
          case e: Expression => e  // For other expressions, keep as is
          case _ => return visitUnknown(postfixOp)
        }
        
        // Update cursor to the end of the expression to avoid duplication
        updateCursor(postfixOp.span.end)
        
        // Extract the space between the method and underscore
        // Use the od (operand) span and op span to find the space
        val spaceBeforeUnderscore = if (postfixOp.od.span.exists && postfixOp.op.span.exists) {
          val odEnd = postfixOp.od.span.end - offsetAdjustment
          val opStart = postfixOp.op.span.start - offsetAdjustment
          if (odEnd < opStart && odEnd >= 0 && opStart <= source.length) {
            Space.format(source.substring(odEnd, opStart))
          } else {
            Space.SINGLE_SPACE
          }
        } else {
          Space.SINGLE_SPACE
        }
        
        // Create a member reference
        new J.MemberReference(
          Tree.randomId(),
          prefix,
          Markers.EMPTY,
          JRightPadded.build(expr),
          null, // No type parameters for now
          JLeftPadded.build(new J.Identifier(
            Tree.randomId(),
            spaceBeforeUnderscore,
            Markers.EMPTY,
            Collections.emptyList(),
            "_",
            null,
            null
          )),
          null, // type
          null, // method type
          null  // variable type  
        )
        
      case _ =>
        // Other postfix operators - treat as unary for now
        val prefix = extractPrefix(postfixOp.span)
        
        val expr = visitTree(postfixOp.od) match {
          case e: Expression => e
          case _ => return visitUnknown(postfixOp)
        }
        
        // For postfix operators, we need to determine the operator type
        // Currently only handling as PostDecrement (as a placeholder)
        val operator = J.Unary.Type.PostDecrement // This is a placeholder
        
        new J.Unary(
          Tree.randomId(),
          prefix,
          Markers.EMPTY,
          JLeftPadded.build(operator).withBefore(Space.EMPTY),
          expr,
          JavaType.Primitive.Boolean
        )
    }
  }
  
  private def mapPrefixOperator(op: String): J.Unary.Type = op match {
    case "!" => J.Unary.Type.Not
    case "+" => J.Unary.Type.Positive
    case "-" => J.Unary.Type.Negative
    case "~" => J.Unary.Type.Complement
    case _ => J.Unary.Type.Not // default
  }
  
  private def visitFunctionApplication(app: untpd.Apply, id: untpd.Ident): J.MethodInvocation = {
    val prefix = extractPrefix(app.span)

    // In Scala, arr(0) is syntactic sugar for arr.apply(0)
    // We'll represent it as a method invocation with "apply" as the method name

    // The select is the identifier (e.g., "arr")
    val select = visitIdent(id).asInstanceOf[Expression]

    // Skip past the opening parenthesis to position cursor correctly for arguments
    val parenPos = positionOfNext("(")
    if (parenPos >= 0) {
      cursor = parenPos + 1
    }

    // Visit the arguments
    val args = new util.ArrayList[JRightPadded[Expression]]()
    for ((arg, i) <- app.args.zipWithIndex) {
      visitTree(arg) match {
        case expr: Expression =>
          // After visiting each argument (except the last), skip past the comma
          if (i < app.args.length - 1) {
            val commaPos = positionOfNext(",")
            if (commaPos >= 0) {
              cursor = commaPos + 1
            }
          }
          // Let the printer handle comma and spacing between arguments
          args.add(JRightPadded.build(expr).withAfter(Space.EMPTY))
        case _ => // Skip non-expressions
      }
    }

    // Skip past the closing parenthesis to position cursor correctly
    val closeParenPos = positionOfNext(")")
    if (closeParenPos >= 0) {
      cursor = closeParenPos + 1
    }

    // Create the method invocation
    // We use "apply" as the method name since that's what Scala desugars to
    val methodName = new J.Identifier(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      Collections.emptyList(),
      "apply",
      null,
      null
    )

    // Add a marker to indicate this is function application syntax
    import org.openrewrite.scala.marker.FunctionApplication

    new J.MethodInvocation(
      Tree.randomId(),
      prefix,
      Markers.build(Collections.singletonList(FunctionApplication.create())),
      JRightPadded.build(select),
      null, // typeParameters
      methodName,
      JContainer.build(Space.EMPTY, args, Markers.EMPTY),
      null  // method type
    )
  }
  
  private def visitCurriedApplication(app: untpd.Apply): J = {
    // Flatten the chain of Apply nodes: add(1)(2)(3) => [add(1), (2), (3)]
    // The outermost Apply has the last arg list, innermost has the first
    val argListsReversed = new util.ArrayList[untpd.Apply]()
    var current: untpd.Tree = app
    while (current.isInstanceOf[untpd.Apply] && current.asInstanceOf[untpd.Apply].fun.isInstanceOf[untpd.Apply]) {
      argListsReversed.add(current.asInstanceOf[untpd.Apply])
      current = current.asInstanceOf[untpd.Apply].fun
    }
    // `current` is now the innermost Apply (whose fun is an Ident/Select, not another Apply)

    // Visit the innermost call as a regular method invocation or function application
    val baseResult = current match {
      case baseApp: untpd.Apply => visitApply(baseApp)
      case _ => return visitUnknown(app)
    }

    baseResult match {
      case baseInvocation: J.MethodInvocation =>
        // Now collect the additional argument lists from the outer Apply nodes
        val additionalArgs = new util.ArrayList[JContainer[Expression]]()
        for (i <- argListsReversed.size() - 1 to 0 by -1) {
          val outerApp = argListsReversed.get(i)
          additionalArgs.add(visitArgumentList(outerApp))
        }

        if (additionalArgs.isEmpty) {
          baseInvocation
        } else {
          import org.openrewrite.scala.marker.AdditionalArgumentLists
          val marker = new AdditionalArgumentLists(Tree.randomId(), additionalArgs)
          baseInvocation.withMarkers(baseInvocation.getMarkers.add(marker))
        }
      case other =>
        // If the base call didn't produce a MethodInvocation, fall back
        visitUnknown(app)
    }
  }

  private def visitArgumentList(app: untpd.Apply): JContainer[Expression] = {
    // Parse the arguments of a single Apply node into a JContainer
    val parenPos = positionOfNext("(")
    val beforeParen = if (parenPos >= 0) {
      val space = Space.format(source.substring(cursor, parenPos))
      cursor = parenPos + 1
      space
    } else Space.EMPTY

    val args = new util.ArrayList[JRightPadded[Expression]]()
    for ((arg, i) <- app.args.zipWithIndex) {
      visitTree(arg) match {
        case expr: Expression =>
          var afterSpace = Space.EMPTY
          if (i < app.args.length - 1) {
            val commaPos = positionOfNext(",")
            if (commaPos >= 0) {
              afterSpace = Space.format(source.substring(cursor, commaPos))
              cursor = commaPos + 1
            }
          }
          args.add(JRightPadded.build(expr).withAfter(afterSpace))
        case _ =>
      }
    }

    val closeParenPos = positionOfNext(")")
    if (closeParenPos >= 0) {
      cursor = closeParenPos + 1
    }

    JContainer.build(beforeParen, args, Markers.EMPTY)
  }

  private def visitTypeAppliedCall(app: untpd.Apply, typeApp: untpd.TypeApply): J = {
    val prefix = extractPrefix(app.span)

    typeApp.fun match {
      case sel: untpd.Select =>
        // obj.method[T](args)
        val target = visitTree(sel.qualifier) match {
          case expr: Expression => expr
          case _ => return visitUnknown(app)
        }

        // Extract space around dot
        var selectBeforeDot = Space.EMPTY
        var selectAfterDot = Space.EMPTY
        val qualEnd = Math.max(0, sel.qualifier.span.end - offsetAdjustment)
        val nameStartAdj = Math.max(0, sel.nameSpan.start - offsetAdjustment)
        if (qualEnd < nameStartAdj && qualEnd >= cursor && nameStartAdj <= source.length) {
          val between = source.substring(qualEnd, nameStartAdj)
          val dotIdx = between.indexOf('.')
          if (dotIdx >= 0) {
            selectBeforeDot = Space.format(between.substring(0, dotIdx))
            if (dotIdx + 1 < between.length) {
              selectAfterDot = Space.format(between.substring(dotIdx + 1))
            }
          }
        }

        // Advance past method name
        val nameEnd = Math.max(0, sel.nameSpan.end - offsetAdjustment)
        if (nameEnd > cursor) cursor = nameEnd

        // Parse type arguments [T, U, ...]
        val typeParams = visitTypeArguments(typeApp)

        // Parse value arguments (args)
        val argContainer = visitArgumentList(app)

        val methodName = new J.Identifier(
          Tree.randomId(),
          selectAfterDot,
          Markers.EMPTY,
          Collections.emptyList(),
          sel.name.toString,
          null, null
        )

        new J.MethodInvocation(
          Tree.randomId(),
          prefix,
          Markers.EMPTY,
          JRightPadded.build(target).withAfter(selectBeforeDot),
          typeParams,
          methodName,
          argContainer,
          null
        )

      case id: untpd.Ident =>
        // f[T](args) - function application with type params
        val select = visitIdent(id).asInstanceOf[Expression]

        // Parse type arguments
        val typeParams = visitTypeArguments(typeApp)

        // Parse value arguments
        val argContainer = visitArgumentList(app)

        val methodName = new J.Identifier(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          Collections.emptyList(),
          "apply",
          null, null
        )

        import org.openrewrite.scala.marker.FunctionApplication
        new J.MethodInvocation(
          Tree.randomId(),
          prefix,
          Markers.build(Collections.singletonList(FunctionApplication.create())),
          JRightPadded.build(select),
          typeParams,
          methodName,
          argContainer,
          null
        )

      case _ =>
        visitUnknown(app)
    }
  }

  private def visitTypeArguments(typeApp: untpd.TypeApply): JContainer[Expression] = {
    val typeParams = new util.ArrayList[JRightPadded[Expression]]()

    // Find opening bracket
    val openBracketPos = positionOfNext("[")
    val beforeBracket = if (openBracketPos >= 0) {
      val space = Space.format(source.substring(cursor, openBracketPos))
      cursor = openBracketPos + 1
      space
    } else Space.EMPTY

    for ((targ, i) <- typeApp.args.zipWithIndex) {
      visitTree(targ) match {
        case expr: Expression =>
          var afterSpace = Space.EMPTY
          if (i < typeApp.args.length - 1) {
            val commaPos = positionOfNext(",")
            if (commaPos >= 0) {
              afterSpace = Space.format(source.substring(cursor, commaPos))
              cursor = commaPos + 1
            }
          }
          typeParams.add(JRightPadded.build(expr).withAfter(afterSpace))
        case _ =>
      }
    }

    // Find closing bracket
    val closeBracketPos = positionOfNext("]")
    if (closeBracketPos >= 0) {
      cursor = closeBracketPos + 1
    }

    JContainer.build(beforeBracket, typeParams, Markers.EMPTY)
  }

  private def visitMethodInvocation(app: untpd.Apply): J = {
    val savedCursor = cursor
    val prefix = extractPrefix(app.span)
    
    // Note: We deliberately don't create J.ArrayAccess for explicit .apply() calls.
    // In Scala, arr.apply(0) is an explicit method call and should be represented as such.
    // Only the implicit apply syntax arr(0) gets the FunctionApplication marker (handled in visitFunctionApplication).
    
    // Check for special cases like Array creation
    app.fun match {
      case sel: untpd.Select if sel.name.toString == "apply" =>
        // Check if this is Array creation: Array.apply(elements...)
        sel.qualifier match {
          case id: untpd.Ident if id.name.toString == "Array" =>
            // This is array creation: Array(1, 2, 3) which desugars to Array.apply(1, 2, 3)
            return visitNewArray(app, sel)
          case _ =>
            // Continue with regular method invocation (including explicit .apply() calls)
        }
      case ta: untpd.TypeApply =>
        // Handle type applications like Array[String]("hello", "world")
        // In Scala 3, Array[Int]() desugars to Array.apply[Int]()
        val isArrayCreate = ta.fun match {
          case id: untpd.Ident if id.name.toString == "Array" => true
          case sel: untpd.Select if sel.name.toString == "apply" =>
            sel.qualifier match {
              case id: untpd.Ident if id.name.toString == "Array" => true
              case _ => false
            }
          case _ => false
        }
        if (isArrayCreate) {
          return visitNewArrayWithType(app, ta)
        }
      case _ =>
        // Continue with regular method invocation
    }
    
    // Handle the method call target
    var selectBeforeDot = Space.EMPTY
    var selectAfterDot = Space.EMPTY
    val (select: Expression, methodName: String, typeParams: java.util.List[Expression]) = app.fun match {
      case sel: untpd.Select =>
        // Method call like obj.method(...) or package.Class.method(...)
        val target = visitTree(sel.qualifier) match {
          case expr: Expression => expr
          case _ => cursor = savedCursor; return visitUnknown(app)
        }

        // Extract space before and after the dot between qualifier and method name
        val qualEnd = Math.max(0, sel.qualifier.span.end - offsetAdjustment)
        val nameStartAdj = Math.max(0, sel.nameSpan.start - offsetAdjustment)
        if (qualEnd < nameStartAdj && qualEnd >= cursor && nameStartAdj <= source.length) {
          val between = source.substring(qualEnd, nameStartAdj)
          val dotIdx = between.indexOf('.')
          if (dotIdx >= 0) {
            selectBeforeDot = Space.format(between.substring(0, dotIdx))
            if (dotIdx + 1 < between.length) {
              selectAfterDot = Space.format(between.substring(dotIdx + 1))
            }
          }
        }

        // Update cursor position to after the method name to avoid re-reading it
        if (sel.nameSpan.exists) {
          val nameEnd = Math.max(0, sel.nameSpan.end - offsetAdjustment)
          if (nameEnd > cursor) {
            cursor = nameEnd
          }
        }

        (target, sel.name.toString, Collections.emptyList[Expression]())

      case id: untpd.Ident =>
        // Simple function call like println(...)
        (null, id.name.toString, Collections.emptyList[Expression]())

      case typeApp: untpd.TypeApply =>
        // Method with type parameters like Array[Int](), List.empty[Int](args)
        // Visit the TypeApply as a type-applied expression, then use it as the method select
        val typeAppResult = visitTypeApply(typeApp)
        typeAppResult match {
          case mi: J.MethodInvocation =>
            // The TypeApply produced a MethodInvocation with type params but no args.
            // Use its select/name and type params for the outer invocation.
            (mi.getSelect, mi.getSimpleName, Collections.emptyList[Expression]())
          case pt: J.ParameterizedType =>
            // E.g., Array[Int]() — the type application is the "function" being called
            (pt.asInstanceOf[Expression], "", Collections.emptyList[Expression]())
          case expr: Expression =>
            (expr, "", Collections.emptyList[Expression]())
          case _ =>
            cursor = savedCursor; return visitUnknown(app)
        }

      case _ =>
        // Other kinds of function applications
        cursor = savedCursor; return visitUnknown(app)
    }
    
    // Don't extract the opening parenthesis - let the printer handle it
    // The printer's visitContainer will add the structural parentheses
    val argContainerPrefix = Space.EMPTY

    // Skip past the opening parenthesis to position cursor correctly for arguments
    if (app.args.nonEmpty) {
      val parenPos = positionOfNext("(")
      if (parenPos >= 0) {
        cursor = parenPos + 1
      }
    }

    // Visit arguments - use while loop to avoid non-local return issues with for comprehensions
    val args = new util.ArrayList[JRightPadded[Expression]]()
    var argFailed = false
    var i = 0
    while (i < app.args.length && !argFailed) {
      val arg = app.args(i)

      // Extract prefix space for this argument (space after previous comma)
      var argPrefix = Space.EMPTY
      if (i > 0) {
        val prevEnd = Math.max(0, app.args(i - 1).span.end - offsetAdjustment)
        val thisStart = Math.max(0, arg.span.start - offsetAdjustment)
        if (prevEnd < thisStart && prevEnd >= cursor && thisStart <= source.length) {
          val between = source.substring(prevEnd, thisStart)
          val commaIndex = between.indexOf(',')
          if (commaIndex >= 0) {
            argPrefix = Space.format(between.substring(commaIndex + 1))
            cursor = prevEnd + commaIndex + 1
          }
        }
      }

      visitTree(arg) match {
        case expr: Expression =>
          // Apply the prefix space to the expression
          val exprWithPrefix = expr match {
            case lit: J.Literal => lit.withPrefix(argPrefix)
            case id: J.Identifier => id.withPrefix(argPrefix)
            case mi: J.MethodInvocation => mi.withPrefix(argPrefix)
            case na: J.NewArray => na.withPrefix(argPrefix)
            case bin: J.Binary => bin.withPrefix(argPrefix)
            case aa: J.ArrayAccess => aa.withPrefix(argPrefix)
            case fa: J.FieldAccess => fa.withPrefix(argPrefix)
            case paren: J.Parentheses[_] => paren.withPrefix(argPrefix)
            case unknown: J.Unknown => unknown.withPrefix(argPrefix)
            case nc: J.NewClass => nc.withPrefix(argPrefix)
            case asg: J.Assignment => asg.withPrefix(argPrefix)
            case lambda: J.Lambda => lambda.withPrefix(argPrefix)
            case _ => expr
          }

          args.add(JRightPadded.build(exprWithPrefix))
        case _ => argFailed = true
      }
      i += 1
    }

    if (argFailed) {
      cursor = savedCursor
      visitUnknown(app)
    } else {
      // Skip past the closing parenthesis to position cursor correctly
      if (app.args.nonEmpty) {
        val closeParenPos = positionOfNext(")")
        if (closeParenPos >= 0) {
          cursor = closeParenPos + 1
        }
      }

      // Create the method name identifier with space after dot
      val name = new J.Identifier(
        Tree.randomId(),
        selectAfterDot,
        Markers.EMPTY,
        Collections.emptyList(),
        methodName,
        null,
        null
      )

      // Build the arguments container
      val argContainer = JContainer.build(
        argContainerPrefix,
        args,
        Markers.EMPTY
      )

      // Update cursor to end of the apply expression
      if (app.span.exists) {
        val adjustedEnd = Math.max(0, app.span.end - offsetAdjustment)
        if (adjustedEnd > cursor && adjustedEnd <= source.length) {
          cursor = adjustedEnd
        }
      }

      new J.MethodInvocation(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        if (select != null) JRightPadded.build(select).withAfter(selectBeforeDot) else null,
        null, // typeParameters - handled separately in TypeApply
        name,
        argContainer,
        null // method type will be set later
      )
    }
  }
  
  private def isBlockArgumentCall(app: untpd.Apply): Boolean = {
    if (app.args.length != 1) return false
    // Partial functions (Match with EmptyTree selector) are always block argument syntax
    if (app.args.head.isInstanceOf[untpd.Match]) return true

    // Check for Scala 3 fewer braces syntax: method: arg (colon instead of braces/parens)
    val adjustedArgStart = Math.max(0, app.args.head.span.start - offsetAdjustment)
    val funEnd = Math.max(0, app.fun.span.end - offsetAdjustment)
    if (funEnd > 0 && adjustedArgStart > funEnd && adjustedArgStart <= source.length) {
      val between = source.substring(funEnd, adjustedArgStart)
      val colonIdx = between.indexOf(':')
      val braceIdx = between.indexOf('{')
      val parenIdx = between.indexOf('(')
      if (colonIdx >= 0 && (braceIdx < 0 || colonIdx < braceIdx) && (parenIdx < 0 || colonIdx < parenIdx)) {
        return true // Fewer braces syntax: method: arg
      }
    }

    if (!app.args.head.isInstanceOf[untpd.Block]) {
      false
    } else {
      // Check source text to determine if this is block argument syntax (no parens)
      // vs regular parenthesized call with a lambda argument
      val adjustedStart = Math.max(0, app.args.head.span.start - offsetAdjustment)
      val adjustedEnd = Math.max(0, app.span.end - offsetAdjustment)
      if (adjustedStart > 0 && adjustedStart <= source.length && adjustedEnd <= source.length) {
        val blockContent = source.substring(adjustedStart, Math.min(adjustedEnd, source.length))
        // If the block contains `=>` but also contains `case`, it's a partial function
        // (which is block argument syntax). Regular lambdas have `=>` without `case`.
        val hasArrow = blockContent.contains("=>")
        val hasCase = blockContent.contains("case")
        if (hasArrow && !hasCase) {
          // Regular lambda argument like { x => x * 2 } — not block argument
          false
        } else {
          // Either no arrow (simple block) or has case (partial function) — block argument
          true
        }
      } else {
        false
      }
    }
  }

  private def visitBlockArgMethodInvocation(app: untpd.Apply, sel: untpd.Select): J = {
    val prefix = extractPrefix(app.span)
    visitTree(sel.qualifier) match {
      case target: Expression =>
        if (sel.nameSpan.exists) {
          val nameEnd = Math.max(0, sel.nameSpan.end - offsetAdjustment)
          if (nameEnd > cursor) cursor = nameEnd
        }
        visitBlockArgumentInvocation(app, prefix, target, target, sel.name.toString)
      case _ =>
        visitUnknown(app)
    }
  }

  private def visitBlockArgFunctionApplication(app: untpd.Apply, id: untpd.Ident): J.MethodInvocation = {
    val prefix = extractPrefix(app.span)
    val select = visitIdent(id).asInstanceOf[Expression]
    visitBlockArgumentInvocation(app, prefix, null, select, id.name.toString)
  }

  private def visitBlockArgumentInvocation(
    app: untpd.Apply,
    prefix: Space,
    select: Expression,
    selectExpr: Expression,
    methodName: String
  ): J.MethodInvocation = {
    import org.openrewrite.scala.marker.BlockArgument

    // Check for Scala 3 fewer braces / colon syntax
    val adjustedArgStart = Math.max(0, app.args.head.span.start - offsetAdjustment)
    val funEnd = Math.max(0, app.fun.span.end - offsetAdjustment)
    val isColonSyntax = if (funEnd > 0 && adjustedArgStart > funEnd && adjustedArgStart <= source.length) {
      val between = source.substring(funEnd, adjustedArgStart)
      val colonIdx = between.indexOf(':')
      colonIdx >= 0 && !between.substring(0, colonIdx).contains('{') && !between.substring(0, colonIdx).contains('(')
    } else false

    if (isColonSyntax) {
      sourceBefore(":")
    }

    // Visit the block argument — either a regular Block or a partial function (Match)
    val lambdaBody: J = app.args.head match {
      case m: untpd.Match => visitPartialFunction(m)
      case _ => visitTree(app.args.head)
    }

    // Wrap in a J.Lambda with no parameters so it fits as Expression
    val lambda = new J.Lambda(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY.addIfAbsent(new BlockArgument()),
      new J.Lambda.Parameters(
        Tree.randomId(),
        Space.EMPTY,
        Markers.EMPTY,
        false,
        Collections.emptyList()
      ),
      Space.EMPTY,
      lambdaBody,
      null
    )

    val name = new J.Identifier(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      Collections.emptyList(),
      methodName,
      null,
      null
    )

    val argList = new util.ArrayList[JRightPadded[Expression]]()
    argList.add(JRightPadded.build(lambda.asInstanceOf[Expression]))
    var argMarkers = Markers.EMPTY.addIfAbsent(new BlockArgument())
    if (isColonSyntax) {
      argMarkers = argMarkers.addIfAbsent(new org.openrewrite.scala.marker.ColonToken(Tree.randomId()))
    }
    val argContainer = JContainer.build(
      Space.EMPTY,
      argList,
      argMarkers
    )

    // Update cursor to end of the apply expression
    if (app.span.exists) {
      val adjustedEnd = Math.max(0, app.span.end - offsetAdjustment)
      if (adjustedEnd > cursor && adjustedEnd <= source.length) {
        cursor = adjustedEnd
      }
    }

    new J.MethodInvocation(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      if (select != null) JRightPadded.build(select) else null,
      null,
      name,
      argContainer,
      null
    )
  }

  private def visitArrayAccess(app: untpd.Apply, sel: untpd.Select): J.ArrayAccess = {
    val prefix = extractPrefix(app.span)
    
    // Visit the array/collection expression
    val array = visitTree(sel.qualifier) match {
      case expr: Expression => expr
      case _ => return visitUnknown(app).asInstanceOf[J.ArrayAccess]
    }
    
    // Visit the index expression
    val index = visitTree(app.args.head) match {
      case expr: Expression => expr
      case _ => return visitUnknown(app).asInstanceOf[J.ArrayAccess]
    }
    
    // Create the dimension with the index
    val dimension = new J.ArrayDimension(
      Tree.randomId(),
      Space.EMPTY, // Space before '['
      Markers.EMPTY,
      JRightPadded.build(index)
    )
    
    new J.ArrayAccess(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      array,
      dimension,
      null // type will be set later
    )
  }
  
  private def visitNewArray(app: untpd.Apply, sel: untpd.Select): J.NewArray = {
    val prefix = extractPrefix(app.span)
    
    // In Scala, Array(1, 2, 3) is syntactic sugar for Array.apply(1, 2, 3)
    // We need to map this to J.NewArray
    
    // For now, we'll assume no explicit type parameters (handled elsewhere)
    val typeExpression: TypeTree = null
    
    // Visit array dimensions (empty for Scala array literals)
    val dimensions = Collections.emptyList[J.ArrayDimension]()
    
    // Visit the array initializer elements
    val elements = new util.ArrayList[Expression]()
    for (arg <- app.args) {
      visitTree(arg) match {
        case expr: Expression => elements.add(expr)
        case _ => return visitUnknown(app).asInstanceOf[J.NewArray]
      }
    }
    
    // Create the initializer container
    val initializer = if (elements.isEmpty) {
      null
    } else {
      // Extract space before opening parenthesis (which acts like opening brace in Java)
      val initPrefix = sourceBefore("(")
      
      // Build padded elements with proper spacing
      val paddedElements = new util.ArrayList[JRightPadded[Expression]]()
      for (i <- 0 until elements.size()) {
        val elem = elements.get(i)
        // Extract space after element (before comma or closing paren)
        val afterSpace = if (i < elements.size() - 1) {
          sourceBefore(",")
        } else {
          sourceBefore(")")
        }
        paddedElements.add(JRightPadded.build(elem).withAfter(afterSpace))
      }
      
      JContainer.build(initPrefix, paddedElements, Markers.EMPTY)
    }
    
    // Update cursor to end of expression
    updateCursor(app.span.end)
    
    new J.NewArray(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      typeExpression,
      dimensions,
      initializer,
      null // type will be set later
    )
  }
  
  private def visitNewArrayWithType(app: untpd.Apply, ta: untpd.TypeApply): J.NewArray = {
    val prefix = extractPrefix(app.span)
    
    // In Scala, Array[String]("hello", "world") creates a typed array
    // We need to map this to J.NewArray with a type expression
    
    // Skip cursor past "Array[" since the printer hardcodes it
    val openBracketPos = source.indexOf('[', cursor)
    if (openBracketPos >= 0) {
      cursor = openBracketPos + 1
    }

    // Visit the type parameter
    val typeExpression = if (ta.args.nonEmpty) {
      visitTree(ta.args.head) match {
        case tt: TypeTree => tt
        case id: J.Identifier => id
        case _ => null
      }
    } else {
      null
    }
    
    // Visit array dimensions (empty for Scala array literals)
    val dimensions = Collections.emptyList[J.ArrayDimension]()
    
    // Update cursor to skip past the type parameter section before processing arguments
    if (ta.args.nonEmpty && ta.args.head.span.exists) {
      // Move cursor past the closing ] of the type parameter
      val typeEnd = Math.max(0, ta.args.head.span.end - offsetAdjustment)
      val closeBracketPos = source.indexOf(']', typeEnd)
      if (closeBracketPos >= 0) {
        cursor = closeBracketPos + 1
      }
    }
    
    // Visit the array initializer elements
    val elements = new util.ArrayList[Expression]()
    for (arg <- app.args) {
      visitTree(arg) match {
        case expr: Expression => elements.add(expr)
        case _ => return visitUnknown(app).asInstanceOf[J.NewArray]
      }
    }
    
    // Create the initializer container
    val initializer = if (elements.isEmpty) {
      // Empty array with type: Array[Int]()
      val initPrefix = sourceBefore("(")
      // Look for closing paren
      sourceBefore(")")
      JContainer.build(initPrefix, Collections.emptyList[JRightPadded[Expression]](), Markers.EMPTY)
    } else {
      // Extract space before opening parenthesis
      val initPrefix = sourceBefore("(")
      
      // Build padded elements with proper spacing
      val paddedElements = new util.ArrayList[JRightPadded[Expression]]()
      for (i <- 0 until elements.size()) {
        val elem = elements.get(i)
        // Extract space after element (before comma or closing paren)
        val afterSpace = if (i < elements.size() - 1) {
          sourceBefore(",")
        } else {
          sourceBefore(")")
        }
        paddedElements.add(JRightPadded.build(elem).withAfter(afterSpace))
      }
      
      JContainer.build(initPrefix, paddedElements, Markers.EMPTY)
    }
    
    // Update cursor to end of expression
    updateCursor(app.span.end)
    
    new J.NewArray(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      typeExpression,
      dimensions,
      initializer,
      null // type will be set later
    )
  }
  
  private def isBinaryOperator(name: String): Boolean = {
    // Check if this is a known binary operator (Java-compatible operators only)
    Set("+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=",
        "&&", "||", "&", "|", "^", "<<", ">>", ">>>").contains(name)
  }
  
  private def visitBinary(sel: untpd.Select, right: untpd.Tree, appSpan: Option[Spans.Span] = None): J.Binary = {
    // For method calls like "1.+(2)", we need to handle the full span from the Apply node
    val prefix = appSpan match {
      case Some(span) if span.exists => extractPrefix(span)
      case _ => Space.EMPTY
    }
    
    val left = visitTree(sel.qualifier).asInstanceOf[Expression]
    val operator = mapOperator(sel.name.toString)
    val rightExpr = visitTree(right).asInstanceOf[Expression]
    
    // Extract any remaining source from the Apply span if provided
    appSpan.foreach { span =>
      if (span.exists) {
        val adjustedEnd = Math.max(0, span.end - offsetAdjustment)
        if (adjustedEnd > cursor && adjustedEnd <= source.length) {
          cursor = adjustedEnd
        }
      }
    }
    
    new J.Binary(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      left,
      JLeftPadded.build(operator),
      rightExpr,
      null // type will be set later
    )
  }
  
  private def mapOperator(op: String): J.Binary.Type = op match {
    case "+" => J.Binary.Type.Addition
    case "-" => J.Binary.Type.Subtraction
    case "*" => J.Binary.Type.Multiplication
    case "/" => J.Binary.Type.Division
    case "%" => J.Binary.Type.Modulo
    case "==" => J.Binary.Type.Equal
    case "!=" => J.Binary.Type.NotEqual
    case "<" => J.Binary.Type.LessThan
    case ">" => J.Binary.Type.GreaterThan
    case "<=" => J.Binary.Type.LessThanOrEqual
    case ">=" => J.Binary.Type.GreaterThanOrEqual
    case "&&" => J.Binary.Type.And
    case "||" => J.Binary.Type.Or
    case "&" => J.Binary.Type.BitAnd
    case "|" => J.Binary.Type.BitOr
    case "^" => J.Binary.Type.BitXor
    case "<<" => J.Binary.Type.LeftShift
    case ">>" => J.Binary.Type.RightShift
    case ">>>" => J.Binary.Type.UnsignedRightShift
    case _ => 
      // For custom operators or method calls, we'll need a different approach
      // For now, treat as method reference
      J.Binary.Type.Addition // placeholder
  }
  
  private def visitSelect(sel: untpd.Select): J = {
    // Check if this is a unary operator method reference without application
    if (isUnaryOperator(sel.name.toString)) {
      // This is something like "x.unary_-" without parentheses - preserve as Unknown
      visitUnknown(sel)
    } else {
      // Map Select to J.FieldAccess
      // Extract prefix for this select
      val prefix = extractPrefix(sel.span)
      
      // Visit the qualifier (target) - this could be an identifier, another select, etc.
      val target = visitTree(sel.qualifier) match {
        case expr: Expression => expr
        case _ => 
          // If the qualifier doesn't produce an expression, fall back to Unknown
          return visitUnknown(sel)
      }
      
      // Extract the space before and after the dot (or # for type projections)
      val qualifierEnd = sel.qualifier.span.end
      val nameStart = sel.nameSpan.start
      var isTypeProjection = false
      var beforeDot = Space.EMPTY
      val dotSpace = if (qualifierEnd < nameStart) {
        val dotStart = Math.max(0, qualifierEnd - offsetAdjustment)
        val nameStartAdjusted = Math.max(0, nameStart - offsetAdjustment)
        if (dotStart < nameStartAdjusted && dotStart >= cursor && nameStartAdjusted <= source.length) {
          val between = source.substring(dotStart, nameStartAdjusted)
          // Check for type projection (#) first, then dot (.)
          val hashIndex = between.indexOf('#')
          val dotIndex = between.indexOf('.')
          val sepIndex = if (hashIndex >= 0) { isTypeProjection = true; hashIndex } else dotIndex
          if (sepIndex >= 0) {
            beforeDot = Space.format(between.substring(0, sepIndex))
            if (sepIndex + 1 < between.length) {
              Space.format(between.substring(sepIndex + 1))
            } else {
              Space.EMPTY
            }
          } else {
            Space.EMPTY
          }
        } else {
          Space.EMPTY
        }
      } else {
        Space.EMPTY
      }
      
      // Create the name identifier
      val name = new J.Identifier(
        Tree.randomId(),
        dotSpace,
        Markers.EMPTY,
        Collections.emptyList(),
        sel.name.toString,
        null,
        null
      )
      
      // Consume up to the end of the selection
      if (sel.span.exists) {
        val adjustedEnd = Math.max(0, sel.span.end - offsetAdjustment)
        if (adjustedEnd > cursor && adjustedEnd <= source.length) {
          cursor = adjustedEnd
        }
      }
      
      val fieldAccessMarkers = if (isTypeProjection) {
        Markers.EMPTY.addIfAbsent(new TypeProjection(Tree.randomId()))
      } else {
        Markers.EMPTY
      }

      new J.FieldAccess(
        Tree.randomId(),
        prefix,
        fieldAccessMarkers,
        target,
        JLeftPadded.build(name).withBefore(beforeDot),
        null
      )
    }
  }

  private def visitInfixOp(infixOp: untpd.InfixOp): J = {
    val opName = infixOp.op.name.toString
    
    // Check if this is a compound assignment operator
    if (opName.endsWith("=") && opName != "==" && opName != "!=" && opName != "<=" && opName != ">=" && opName.length > 1) {
      // This is a compound assignment like +=, -=, *=, /=
      val prefix = extractPrefix(infixOp.span)
      
      // Visit the left side (variable)
      val variable = visitTree(infixOp.left) match {
        case expr: Expression => expr
        case _ => return visitUnknown(infixOp)
      }
      
      // Map the operator
      val baseOp = opName.dropRight(1) // Remove the '='
      val operator = baseOp match {
        case "+" => J.AssignmentOperation.Type.Addition
        case "-" => J.AssignmentOperation.Type.Subtraction
        case "*" => J.AssignmentOperation.Type.Multiplication
        case "/" => J.AssignmentOperation.Type.Division
        case "%" => J.AssignmentOperation.Type.Modulo
        case "&" => J.AssignmentOperation.Type.BitAnd
        case "|" => J.AssignmentOperation.Type.BitOr
        case "^" => J.AssignmentOperation.Type.BitXor
        case "<<" => J.AssignmentOperation.Type.LeftShift
        case ">>" => J.AssignmentOperation.Type.RightShift
        case ">>>" => J.AssignmentOperation.Type.UnsignedRightShift
        case _ => return visitUnknown(infixOp)
      }
      
      // Extract space around the operator
      val leftEnd = Math.max(0, infixOp.left.span.end - offsetAdjustment)
      val opStart = Math.max(0, infixOp.op.span.start - offsetAdjustment)
      val opEnd = Math.max(0, infixOp.op.span.end - offsetAdjustment)
      val rightStart = Math.max(0, infixOp.right.span.start - offsetAdjustment)
      
      var operatorSpace = Space.EMPTY
      var valueSpace = Space.EMPTY
      
      if (leftEnd < opStart && leftEnd >= cursor && opStart <= source.length) {
        operatorSpace = Space.format(source.substring(leftEnd, opStart))
      }
      
      if (opEnd < rightStart && opEnd >= cursor && rightStart <= source.length) {
        valueSpace = Space.format(source.substring(opEnd, rightStart))
      }
      
      // Visit the right side (value)
      cursor = Math.max(0, infixOp.right.span.start - offsetAdjustment)
      val value = visitTree(infixOp.right) match {
        case expr: Expression => expr
        case _ => return visitUnknown(infixOp)
      }
      
      // Update cursor to the end
      updateCursor(infixOp.span.end)
      
      new J.AssignmentOperation(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        variable,
        JLeftPadded.build(operator).withBefore(operatorSpace),
        value.withPrefix(valueSpace),
        null // type
      )
    } else if (isBinaryOperator(opName)) {
      // This is a regular binary operation like +, -, *, /
      visitBinaryOperation(infixOp)
    } else {
      // This is an infix method call like "list map func"
      visitInfixMethodCall(infixOp)
    }
  }
  
  private def visitBinaryOperation(infixOp: untpd.InfixOp): J.Binary = {
    val prefix = extractPrefix(infixOp.span)
    
    // Visit left expression
    val left = visitTree(infixOp.left) match {
      case expr: Expression => expr
      case _ => return visitUnknown(infixOp).asInstanceOf[J.Binary]
    }
    
    // Map operator
    val operator = mapOperator(infixOp.op.name.toString)
    
    // Extract operator space
    val leftEnd = Math.max(0, infixOp.left.span.end - offsetAdjustment)
    val opStart = Math.max(0, infixOp.op.span.start - offsetAdjustment) 
    val opEnd = Math.max(0, infixOp.op.span.end - offsetAdjustment)
    val rightStart = Math.max(0, infixOp.right.span.start - offsetAdjustment)
    
    var operatorSpace = Space.format(" ")
    var rightSpace = Space.format(" ")
    
    if (leftEnd < opStart && leftEnd >= cursor && opStart <= source.length) {
      operatorSpace = Space.format(source.substring(leftEnd, opStart))
    }
    
    if (opEnd < rightStart && opEnd >= cursor && rightStart <= source.length) {
      rightSpace = Space.format(source.substring(opEnd, rightStart))
    }
    
    // Visit right expression  
    cursor = Math.max(0, infixOp.right.span.start - offsetAdjustment)
    val right = visitTree(infixOp.right) match {
      case expr: Expression => expr
      case _ => return visitUnknown(infixOp).asInstanceOf[J.Binary]
    }
    
    // Update cursor
    updateCursor(infixOp.span.end)
    
    new J.Binary(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      left,
      JLeftPadded.build(operator).withBefore(operatorSpace),
      right.withPrefix(rightSpace),
      null // type
    )
  }
  
  private def visitInfixMethodCall(infixOp: untpd.InfixOp): J.MethodInvocation = {
    val prefix = extractPrefix(infixOp.span)
    
    // Visit the select (left side)
    val select = visitTree(infixOp.left) match {
      case expr: Expression => expr
      case _ => return visitUnknown(infixOp).asInstanceOf[J.MethodInvocation]
    }
    
    // Extract method name
    val methodName = infixOp.op.name.toString
    
    // Extract space before the method name
    val leftEnd = Math.max(0, infixOp.left.span.end - offsetAdjustment)
    val opStart = Math.max(0, infixOp.op.span.start - offsetAdjustment)
    val methodNameSpace = if (leftEnd < opStart && leftEnd >= cursor && opStart <= source.length) {
      cursor = opStart
      Space.format(source.substring(leftEnd, opStart))
    } else {
      Space.format(" ")
    }
    
    // Move cursor past the method name
    cursor = Math.max(0, infixOp.op.span.end - offsetAdjustment)
    
    // Extract space before the argument
    val opEnd = Math.max(0, infixOp.op.span.end - offsetAdjustment)
    val rightStart = Math.max(0, infixOp.right.span.start - offsetAdjustment)
    val argSpace = if (opEnd < rightStart && opEnd >= cursor && rightStart <= source.length) {
      cursor = rightStart
      Space.format(source.substring(opEnd, rightStart))
    } else {
      Space.format(" ")
    }
    
    // Visit the argument (right side)
    val arg = visitTree(infixOp.right) match {
      case expr: Expression => expr
      case _ => return visitUnknown(infixOp).asInstanceOf[J.MethodInvocation]
    }
    
    // Create the method name identifier
    val name = new J.Identifier(
      Tree.randomId(),
      methodNameSpace,
      Markers.EMPTY,
      Collections.emptyList(),
      methodName,
      null,
      null
    )
    
    // Create the arguments container
    val args = new util.ArrayList[JRightPadded[Expression]]()
    args.add(JRightPadded.build(arg.withPrefix(argSpace)).withAfter(Space.EMPTY))
    
    // Import the InfixNotation marker
    import org.openrewrite.scala.marker.InfixNotation
    
    // Create the method invocation with the InfixNotation marker
    new J.MethodInvocation(
      Tree.randomId(),
      prefix,
      Markers.build(Collections.singletonList(InfixNotation.create())),
      JRightPadded.build(select),
      null, // typeParameters
      name,
      JContainer.build(Space.EMPTY, args, Markers.EMPTY),
      null  // method type
    )
  }

  private def visitParentheses(parens: untpd.Parens): J = {
    // Extract prefix - but check if cursor is already at the opening paren
    val adjustedStart = Math.max(0, parens.span.start - offsetAdjustment)
    val adjustedEnd = Math.max(0, parens.span.end - offsetAdjustment)
    
    
    
    val prefix = if (cursor <= adjustedStart) {
      extractPrefix(parens.span)
    } else {
      // Cursor is already past the start, don't extract prefix
      Space.EMPTY
    }
    
    // Update cursor to skip the opening parenthesis
    if (cursor == adjustedStart) {
      cursor = adjustedStart + 1
    }
    
    // Try to access the inner expression directly
    // Parens might have a field like 'tree' or 'expr'
    val innerTree = try {
      // Try different possible field names
      val treeField = parens.getClass.getDeclaredFields.find(f => 
        f.getName.contains("tree") || f.getName.contains("expr") || f.getName.contains("arg")
      )
      
      treeField match {
        case Some(field) =>
          field.setAccessible(true)
          field.get(parens).asInstanceOf[untpd.Tree]
        case None =>
          // Fall back to productElement approach
          if (parens.productArity > 0) {
            parens.productElement(0).asInstanceOf[untpd.Tree]
          } else {
            return visitUnknown(parens)
          }
      }
    } catch {
      case _: Exception => return visitUnknown(parens)
    }
    
    // Visit the inner tree
    val innerExpr = visitTree(innerTree) match {
      case expr: Expression => expr
      case _ => return visitUnknown(parens)
    }
    
    // Extract space before the closing parenthesis
    val innerEnd = innerTree.span.end
    val parenEnd = parens.span.end
    val closingSpace = if (innerEnd < parenEnd - 1) {
      val adjustedInnerEnd = Math.max(0, innerEnd - offsetAdjustment)
      val adjustedParenEnd = Math.max(0, parenEnd - 1 - offsetAdjustment)
      if (adjustedInnerEnd < adjustedParenEnd && adjustedInnerEnd >= cursor && adjustedParenEnd <= source.length) {
        Space.format(source.substring(adjustedInnerEnd, adjustedParenEnd))
      } else {
        Space.EMPTY
      }
    } else {
      Space.EMPTY
    }
    
    // Update cursor to just after the closing parenthesis
    // The span might include extra characters, so we need to find the actual closing paren
    val spanText = source.substring(adjustedStart, Math.min(adjustedEnd, source.length))
    val lastParenIndex = spanText.lastIndexOf(')')
    if (lastParenIndex >= 0) {
      cursor = adjustedStart + lastParenIndex + 1
    } else {
      updateCursor(parens.span.end)
    }
    
    new J.Parentheses[Expression](
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      JRightPadded.build(innerExpr).withAfter(closingSpace)
    )
  }
  
  private def visitNewClassWithArgs(newTree: untpd.New, app: untpd.Apply): J.NewClass = {
    // The Apply node has the full span including "new", use its prefix
    val prefix = extractPrefix(app.span)
    
    // Extract space between "new" and the type
    // First, consume "new" keyword
    val newPos = positionOfNext("new")
    if (newPos >= 0 && newPos == cursor) {
      cursor += 3 // Move past "new"
    }
    
    // Extract space between "new" and type
    val typeStart = Math.max(0, newTree.tpt.span.start - offsetAdjustment)
    val typeSpace = if (cursor < typeStart && typeStart <= source.length) {
      val spaceStr = source.substring(cursor, typeStart)
      cursor = typeStart
      Space.format(spaceStr)
    } else {
      Space.EMPTY
    }
    
    // Visit the type being instantiated
    val clazz = visitTree(newTree.tpt) match {
      case typeTree: TypeTree => typeTree.withPrefix(typeSpace)
      case id: J.Identifier => id.withPrefix(typeSpace)
      case fieldAccess: J.FieldAccess => fieldAccess.withPrefix(typeSpace)
      case _ => return visitUnknown(app).asInstanceOf[J.NewClass]
    }
    
    // Extract space before parentheses
    val typeEnd = Math.max(0, newTree.tpt.span.end - offsetAdjustment)
    val argsStart = if (app.args.nonEmpty) {
      Math.max(0, app.args.head.span.start - offsetAdjustment)
    } else {
      Math.max(0, app.span.end - offsetAdjustment) - 1 // Looking for the closing paren
    }
    
    var beforeParenSpace = Space.EMPTY
    var hasParentheses = false
    if (typeEnd < argsStart && typeEnd >= cursor && argsStart <= source.length) {
      val between = source.substring(typeEnd, argsStart)
      val parenIndex = between.indexOf('(')
      if (parenIndex >= 0) {
        hasParentheses = true
        beforeParenSpace = Space.format(between.substring(0, parenIndex))
        cursor = typeEnd + parenIndex + 1
      }
    } else if (app.args.isEmpty && typeEnd >= cursor) {
      // Check if there are parentheses for empty args
      val endBound = Math.min(source.length, Math.max(0, app.span.end - offsetAdjustment))
      if (typeEnd < endBound) {
        val after = source.substring(typeEnd, endBound)
        hasParentheses = after.contains("(") && after.contains(")")
        if (hasParentheses) {
          val parenIndex = after.indexOf('(')
          beforeParenSpace = Space.format(after.substring(0, parenIndex))
          cursor = typeEnd + after.indexOf(')') + 1
        }
      }
    }
    
    // Visit arguments
    val args = new util.ArrayList[JRightPadded[Expression]]()
    for (i <- app.args.indices) {
      val arg = app.args(i)
      
      // Extract prefix space for this argument (space after previous comma)
      var argPrefix = Space.EMPTY
      if (i > 0) {
        val prevEnd = Math.max(0, app.args(i - 1).span.end - offsetAdjustment)
        val thisStart = Math.max(0, arg.span.start - offsetAdjustment)
        if (prevEnd < thisStart && prevEnd >= cursor && thisStart <= source.length) {
          val between = source.substring(prevEnd, thisStart)
          val commaIndex = between.indexOf(',')
          if (commaIndex >= 0) {
            argPrefix = Space.format(between.substring(commaIndex + 1))
            cursor = prevEnd + commaIndex + 1
          }
        }
      }
      
      visitTree(arg) match {
        case expr: Expression =>
          // Apply the prefix space to the expression
          val exprWithPrefix = expr match {
            case lit: J.Literal => lit.withPrefix(argPrefix)
            case id: J.Identifier => id.withPrefix(argPrefix)
            case mi: J.MethodInvocation => mi.withPrefix(argPrefix)
            case na: J.NewArray => na.withPrefix(argPrefix)
            case bin: J.Binary => bin.withPrefix(argPrefix)
            case aa: J.ArrayAccess => aa.withPrefix(argPrefix)
            case fa: J.FieldAccess => fa.withPrefix(argPrefix)
            case paren: J.Parentheses[_] => paren.withPrefix(argPrefix)
            case unknown: J.Unknown => unknown.withPrefix(argPrefix)
            case nc: J.NewClass => nc.withPrefix(argPrefix)
            case asg: J.Assignment => asg.withPrefix(argPrefix)
            case _ => expr
          }
          
          args.add(JRightPadded.build(exprWithPrefix))
        case _ => return visitUnknown(app).asInstanceOf[J.NewClass]
      }
    }
    
    // Update cursor to the end
    updateCursor(app.span.end)
    
    val argContainer = if (!hasParentheses && args.isEmpty) {
      // No parentheses and no arguments - e.g. "new Person"
      null
    } else if (args.isEmpty) {
      // Empty parentheses - e.g. "new Person()"
      JContainer.build(beforeParenSpace, Collections.emptyList[JRightPadded[Expression]](), Markers.EMPTY)
    } else {
      // Has arguments - e.g. "new Person(name, age)"
      JContainer.build(beforeParenSpace, args, Markers.EMPTY)
    }
    
    new J.NewClass(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      null, // enclosing
      Space.EMPTY,
      clazz,
      argContainer,
      null, // body for anonymous classes
      null // constructorType
    )
  }

  private def visitNew(newTree: untpd.New): J.NewClass = {
    // Anonymous classes in Scala are represented as New nodes with Template bodies
    // Example: new Runnable { def run() = ... }
    // The Template contains the parent types and the body implementation
    
    // Extract prefix but skip the "new" keyword
    var prefix = extractPrefix(newTree.span)
    
    // Skip past "new" in the source if present
    if (newTree.span.exists) {
      val start = Math.max(0, newTree.span.start - offsetAdjustment)
      val end = Math.max(0, newTree.span.end - offsetAdjustment)
      if (start >= cursor && end <= source.length && start < end) {
        val sourceText = source.substring(start, end)
        val newIndex = sourceText.indexOf("new")
        if (newIndex >= 0) {
          // Move cursor past "new" and any following space
          val afterNew = start + newIndex + 3
          if (afterNew < end) {
            updateCursor(afterNew)
            // Extract space after "new" keyword  
            val afterNewText = source.substring(afterNew, end)
            val spaceMatch = afterNewText.takeWhile(_.isWhitespace)
            if (spaceMatch.nonEmpty) {
              updateCursor(afterNew + spaceMatch.length)
            }
          }
        }
      }
    }
    
    // The New node's tpt is the Template containing the anonymous class definition
    newTree.tpt match {
      case template: untpd.Template =>
        // Extract the parent type(s) - usually the first parent is the main type
        val parents = template.parents
        if (parents.isEmpty) {
          return visitUnknown(newTree).asInstanceOf[J.NewClass]
        }
        
        // The first parent is typically an Apply node for constructor calls
        // or just an Ident/Select for interfaces/traits
        val firstParent = parents.head
        
        // Extract the class type and arguments
        val (clazz, args) = firstParent match {
          case app: untpd.Apply if app.fun.isInstanceOf[untpd.Select] && 
               app.fun.asInstanceOf[untpd.Select].name.toString == "<init>" =>
            // Constructor call with arguments: new Person("John", 30) { ... }
            val sel = app.fun.asInstanceOf[untpd.Select]
            sel.qualifier match {
              case newInner: untpd.New =>
                // Visit the type tree directly
                val typeTree = visitTree(newInner.tpt).asInstanceOf[TypeTree]
                
                // Now handle the arguments
                val argContainer = if (app.args.nonEmpty) {
                  val args = new util.ArrayList[JRightPadded[Expression]]()
                  
                  // Find the opening parenthesis
                  var beforeParenSpace = Space.EMPTY
                  if (app.span.exists) {
                    val typeEnd = Math.max(0, newInner.tpt.span.end - offsetAdjustment)
                    val argsStart = Math.max(0, app.span.start - offsetAdjustment)
                    
                    if (typeEnd < argsStart && typeEnd >= cursor && argsStart <= source.length) {
                      val between = source.substring(typeEnd, argsStart)
                      val parenIndex = between.indexOf('(')
                      if (parenIndex >= 0) {
                        beforeParenSpace = Space.format(between.substring(0, parenIndex))
                        updateCursor(typeEnd + parenIndex + 1)
                      }
                    }
                  }
                  
                  // Visit arguments
                  for ((arg, i) <- app.args.zipWithIndex) {
                    var argPrefix = Space.EMPTY
                    if (i > 0) {
                      val prevEnd = Math.max(0, app.args(i - 1).span.end - offsetAdjustment)
                      val thisStart = Math.max(0, arg.span.start - offsetAdjustment)
                      if (prevEnd < thisStart && prevEnd >= cursor && thisStart <= source.length) {
                        val between = source.substring(prevEnd, thisStart)
                        val commaIndex = between.indexOf(',')
                        if (commaIndex >= 0) {
                          argPrefix = Space.format(between.substring(commaIndex + 1))
                          updateCursor(prevEnd + commaIndex + 1)
                        }
                      }
                    }
                    
                    val argJ = visitTree(arg)
                    val visitedArg: Expression = if (argJ.isInstanceOf[Expression]) {
                      argJ.asInstanceOf[Expression].withPrefix(argPrefix)
                    } else {
                      visitUnknown(arg).asInstanceOf[Expression].withPrefix(argPrefix)
                    }
                    args.add(new JRightPadded[Expression](visitedArg, Space.EMPTY, Markers.EMPTY))
                  }
                  
                  // Extract space before closing parenthesis for last argument
                  if (app.span.exists && app.args.nonEmpty) {
                    val lastArgEnd = Math.max(0, app.args.last.span.end - offsetAdjustment)
                    val appEnd = Math.max(0, app.span.end - offsetAdjustment)
                    if (lastArgEnd < appEnd && lastArgEnd >= cursor && appEnd <= source.length) {
                      val remaining = source.substring(lastArgEnd, appEnd)
                      val closeParenIndex = remaining.indexOf(')')
                      if (closeParenIndex >= 0) {
                        val beforeCloseSpace = Space.format(remaining.substring(0, closeParenIndex))
                        updateCursor(lastArgEnd + closeParenIndex + 1)
                        
                        // Update the last argument's after space
                        if (!args.isEmpty) {
                          val lastArg = args.get(args.size() - 1)
                          args.set(args.size() - 1, lastArg.withAfter(beforeCloseSpace))
                        }
                      }
                    }
                  }
                  
                  JContainer.build(beforeParenSpace, args, Markers.EMPTY)
                } else {
                  null
                }
                
                (typeTree, argContainer)
              case _ =>
                (visitUnknown(sel.qualifier).asInstanceOf[TypeTree], null)
            }
          case _ =>
            // Simple interface/trait: new Runnable { ... }
            val typeTree = visitTree(firstParent).asInstanceOf[TypeTree]
            (typeTree, null)
        }
        
        // Create the anonymous class body
        val body = if (template.body.nonEmpty) {
          // Filter out the synthetic constructor and self-reference
          val bodyStatements = template.body.filter {
            case dd: untpd.DefDef if dd.name.toString == "<init>" => false
            case vd: untpd.ValDef if vd.name.toString == "_" => false
            case _ => true
          }
          
          if (bodyStatements.nonEmpty) {
            // Extract space before the opening brace
            var beforeBrace = Space.EMPTY
            if (newTree.span.exists && clazz.getPrefix.getWhitespace.isEmpty) {
              val newStart = Math.max(0, newTree.span.start - offsetAdjustment)
              val newEnd = Math.max(0, newTree.span.end - offsetAdjustment)
              
              // Find the position after the type/arguments and before the brace
              val searchStart = if (args != null && args.getElements != null && args.getElements.size() > 0) {
                // After the closing parenthesis of arguments
                Math.max(cursor, newStart)
              } else {
                // After the type name
                Math.max(cursor, newStart)
              }
              
              if (searchStart < newEnd && searchStart >= 0 && newEnd <= source.length) {
                val sourceText = source.substring(searchStart, newEnd)
                val braceIndex = sourceText.indexOf('{')
                if (braceIndex >= 0) {
                  beforeBrace = Space.format(sourceText.substring(0, braceIndex))
                  updateCursor(searchStart + braceIndex + 1)
                }
              }
            }
            
            // Convert body statements
            val statements = new util.ArrayList[J]()
            val statementPaddings = new util.ArrayList[JRightPadded[Statement]]()
            
            bodyStatements.foreach { stmt =>
              val stmtJ = visitTree(stmt)
              if (stmtJ.isInstanceOf[Statement]) {
                statementPaddings.add(new JRightPadded[Statement](
                  stmtJ.asInstanceOf[Statement],
                  Space.EMPTY,
                  Markers.EMPTY
                ))
              }
            }
            
            // Extract space before closing brace
            var beforeCloseBrace = Space.EMPTY
            if (newTree.span.exists) {
              val newEnd = Math.max(0, newTree.span.end - offsetAdjustment)
              if (cursor < newEnd && cursor >= 0 && newEnd <= source.length) {
                val remaining = source.substring(cursor, newEnd)
                val closeBraceIndex = remaining.lastIndexOf('}')
                if (closeBraceIndex >= 0) {
                  beforeCloseBrace = Space.format(remaining.substring(0, closeBraceIndex))
                  updateCursor(cursor + closeBraceIndex + 1)
                }
              }
            }
            
            new J.Block(
              Tree.randomId(),
              beforeBrace,
              Markers.EMPTY,
              new JRightPadded[java.lang.Boolean](false, Space.EMPTY, Markers.EMPTY),
              statementPaddings,
              beforeCloseBrace
            )
          } else {
            null
          }
        } else {
          null
        }
        
        new J.NewClass(
          Tree.randomId(),
          prefix,
          Markers.EMPTY,
          null, // enclosing
          Space.SINGLE_SPACE, // Space after "new" keyword
          clazz,
          args,
          body,
          null // constructorType
        )
        
      case _ =>
        // Not an anonymous class, shouldn't happen in visitNew
        visitUnknown(newTree).asInstanceOf[J.NewClass]
    }
  }

  private def visitImport(imp: untpd.Import): J = {
    // Extract the prefix - should only be whitespace/comments before "import"
    val adjustedStart = Math.max(0, imp.span.start - offsetAdjustment)
    val prefix = if (cursor < adjustedStart) {
      val prefixText = source.substring(cursor, adjustedStart)
      cursor = adjustedStart
      Space.format(prefixText)
    } else {
      Space.EMPTY
    }

    // Set import context flag for identifier processing
    val oldInImportContext = isInImportContext
    isInImportContext = true

    // Move cursor to the start of the import expression
    if (imp.expr.span.exists) {
      val exprStart = Math.max(0, imp.expr.span.start - offsetAdjustment)
      cursor = exprStart
    }

    // Visit the import expression to get the field access
    val expr = visitTree(imp.expr)

    // Restore import context flag
    isInImportContext = oldInImportContext

    // For imports, we need a FieldAccess that includes the selectors
    var qualid = expr match {
      case fa: J.FieldAccess => fa
      case id: J.Identifier =>
        new J.FieldAccess(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          new J.Empty(Tree.randomId(), Space.EMPTY, Markers.EMPTY),
          JLeftPadded.build(id),
          null
        )
      case other =>
        return visitUnknown(imp)
    }

    // Handle selectors
    if (imp.selectors.nonEmpty && imp.selectors.size == 1) {
      val selector = imp.selectors.head
      selector match {
        case untpd.ImportSelector(ident: untpd.Ident, untpd.EmptyTree, untpd.EmptyTree) =>
          // Simple selector like "List" in "import java.util.List"
          if (cursor < source.length && source.charAt(cursor) == '.') {
            cursor += 1 // Skip the dot
          }

          val selectorName = new J.Identifier(
            Tree.randomId(),
            Space.EMPTY,
            Markers.EMPTY,
            Collections.emptyList(),
            ident.name.toString,
            null,
            null
          )

          qualid = new J.FieldAccess(
            Tree.randomId(),
            Space.EMPTY,
            Markers.EMPTY,
            qualid,
            JLeftPadded.build(selectorName),
            null
          )
        case _ =>
          // Complex selectors (aliases, wildcards, braces) - consume remaining text
          qualid = consumeSelectorsFromSource(imp, qualid)
      }
    } else if (imp.selectors.size > 1) {
      // Selective import with braces: import scala.util.{Try, Success, Failure}
      qualid = consumeSelectorsFromSource(imp, qualid)
    }

    // Fix "given" imports where Dotty encodes the name as empty
    qualid match {
      case fa: J.FieldAccess if fa.getName().getSimpleName().isEmpty =>
        val adjustedEnd = if (imp.span.exists) Math.max(0, imp.span.end - offsetAdjustment) else source.length
        if (cursor < adjustedEnd) {
          val remaining = source.substring(cursor, adjustedEnd).trim
          if (remaining == "given") {
            val givenName = new J.Identifier(
              Tree.randomId(),
              fa.getName().getPrefix(),
              Markers.EMPTY,
              Collections.emptyList(),
              "given",
              null,
              null
            )
            qualid = fa.withName(givenName)
          }
        }
      case _ =>
    }

    // Update cursor to the end of the import
    if (imp.span.exists) {
      val adjustedEnd = Math.max(0, imp.span.end - offsetAdjustment)
      updateCursor(adjustedEnd)
    }

    // Create J.Import
    new J.Import(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      JLeftPadded.build(false), // static imports are not supported in Scala
      qualid,
      null // no alias for simple imports
    )
  }

  private def consumeSelectorsFromSource(imp: untpd.Import, currentQualid: J.FieldAccess): J.FieldAccess = {
    val adjustedEnd = if (imp.span.exists) Math.max(0, imp.span.end - offsetAdjustment) else source.length
    if (cursor < adjustedEnd) {
      val remaining = source.substring(cursor, adjustedEnd)
      val dotIdx = remaining.indexOf('.')
      if (dotIdx >= 0) {
        cursor += dotIdx + 1 // Skip past the dot
        val selectorText = remaining.substring(dotIdx + 1)

        val selectorName = new J.Identifier(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          Collections.emptyList(),
          selectorText,
          null,
          null
        )

        return new J.FieldAccess(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          currentQualid,
          JLeftPadded.build(selectorName),
          null
        )
      }
    }
    currentQualid
  }

  private def visitPackageDef(pkg: untpd.PackageDef): J = {
    // Package definitions at the statement level should not be converted to statements
    // They are handled at the compilation unit level
    // Return null to indicate this node should be skipped
    null
  }
  
  private def visitLambdaParameter(vd: untpd.ValDef): J = {
    val prefix = extractPrefix(vd.span)
    
    // Check if the type was explicitly written in source or inferred
    // If the source doesn't contain a colon after the name, it's inferred
    val sourceText = extractSource(vd.span)
    val hasExplicitType = sourceText.contains(":")
    
    // If there's no explicit type in source, just return an identifier
    if (!hasExplicitType || vd.tpt == untpd.EmptyTree) {
      new J.Identifier(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        Collections.emptyList(),
        vd.name.toString,
        null,
        null
      )
    } else {
      // With a type, we need a full variable declaration
      val name = new J.Identifier(
        Tree.randomId(),
        Space.EMPTY,
        Markers.EMPTY,
        Collections.emptyList(),
        vd.name.toString,
        null,
        null
      )
      
      // Extract the type
      val sourceText = extractSource(vd.span)
      val colonIdx = sourceText.indexOf(':')
      val typeSpace = if (colonIdx >= 0 && colonIdx + 1 < sourceText.length) {
        Space.format(sourceText.substring(colonIdx + 1).takeWhile(_.isWhitespace))
      } else {
        Space.SINGLE_SPACE
      }
      
      val typeExpr: TypeTree = visitTree(vd.tpt) match {
        case tt: TypeTree => tt.withPrefix(typeSpace)
        case id: J.Identifier => id.withPrefix(typeSpace)
        case unknown: J.Unknown => unknown.withPrefix(typeSpace)  // Handle J.Unknown types
        case _ => null
      }
      
      // Create the variable
      val variable = new J.VariableDeclarations.NamedVariable(
        Tree.randomId(),
        Space.EMPTY,
        Markers.EMPTY,
        name,
        Collections.emptyList(),
        null,
        null
      )
      
      // Create the variable declarations with a marker to indicate it's a lambda parameter
      import org.openrewrite.scala.marker.LambdaParameter
      new J.VariableDeclarations(
        Tree.randomId(),
        prefix,
        Markers.build(Collections.singletonList(new LambdaParameter())),
        Collections.emptyList(), // no annotations
        Collections.emptyList(), // no modifiers
        typeExpr,
        null,
        Collections.emptyList(),
        Collections.singletonList(JRightPadded.build(variable))
      )
    }
  }
  
  private def visitValDef(vd: untpd.ValDef, isLambdaParam: Boolean = false): J = {
    // For lambda parameters, don't look for val/var keywords
    if (isLambdaParam) {
      return visitLambdaParameter(vd)
    }
    
    // Special handling for variables with annotations
    val hasAnnotations = vd.mods != null && vd.mods.annotations.nonEmpty
    val prefix = if (hasAnnotations) {
      // Don't extract prefix yet - annotations will consume their own prefix
      Space.EMPTY
    } else {
      extractPrefix(vd.span)
    }
    
    // Handle annotations first
    val leadingAnnotations = new util.ArrayList[J.Annotation]()
    if (hasAnnotations) {
      for (annot <- vd.mods.annotations) {
        visitTree(annot) match {
          case ann: J.Annotation => leadingAnnotations.add(ann)
          case _ => // Skip if not mapped to annotation
        }
      }
    }
    
    // Extract modifiers and keywords from source
    // When we have annotations, cursor is positioned after them
    val adjustedStart = if (hasAnnotations) cursor else Math.max(0, vd.span.start - offsetAdjustment)
    val adjustedEnd = Math.max(0, vd.span.end - offsetAdjustment)
    
    // Extract source to find modifier keywords and val/var
    var valVarKeyword = ""
    var beforeValVar = Space.EMPTY
    var afterValVar = Space.EMPTY
    val modifiers = new util.ArrayList[J.Modifier]()
    var hasExplicitFinal = false
    var hasExplicitLazy = false
    
    if (adjustedStart >= 0 && adjustedEnd <= source.length && adjustedStart < adjustedEnd) {
      val sourceSnippet = source.substring(adjustedStart, adjustedEnd)
      
      // First, extract any modifiers before val/var
      var modifierEndPos = 0
      
      // Check for access modifiers
      if (sourceSnippet.startsWith("private ")) {
        modifiers.add(new J.Modifier(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          "private",
          J.Modifier.Type.Private,
          Collections.emptyList()
        ))
        modifierEndPos = "private ".length
      } else if (sourceSnippet.startsWith("protected ")) {
        modifiers.add(new J.Modifier(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          "protected",
          J.Modifier.Type.Protected,
          Collections.emptyList()
        ))
        modifierEndPos = "protected ".length
      }
      
      // Check for final modifier after access modifier
      val afterAccess = sourceSnippet.substring(modifierEndPos)
      if (afterAccess.startsWith("final ")) {
        hasExplicitFinal = true
        modifiers.add(new J.Modifier(
          Tree.randomId(),
          if (modifierEndPos > 0) Space.SINGLE_SPACE else Space.EMPTY,
          Markers.EMPTY,
          "final",
          J.Modifier.Type.Final,
          Collections.emptyList()
        ))
        modifierEndPos += "final ".length
      }
      
      // Check for lazy modifier
      val afterFinal = sourceSnippet.substring(modifierEndPos)
      if (afterFinal.startsWith("lazy ")) {
        hasExplicitLazy = true
        modifiers.add(new J.Modifier(
          Tree.randomId(),
          if (modifierEndPos > 0) Space.SINGLE_SPACE else Space.EMPTY,
          Markers.EMPTY,
          "lazy",
          J.Modifier.Type.LanguageExtension,
          Collections.emptyList()
        ))
        modifierEndPos += "lazy ".length
      }
      
      // Now find val/var after modifiers
      val afterModifiers = sourceSnippet.substring(modifierEndPos)
      val valIndex = afterModifiers.indexOf("val")
      val varIndex = afterModifiers.indexOf("var")
      
      val (keywordStart, keyword) = if (valIndex >= 0 && (varIndex < 0 || valIndex < varIndex)) {
        (valIndex, "val")
      } else if (varIndex >= 0) {
        (varIndex, "var")
      } else {
        (-1, "")
      }
      
      if (keywordStart >= 0) {
        // Extract space before val/var (after modifiers or annotations)
        if (keywordStart > 0) {
          beforeValVar = Space.format(afterModifiers.substring(0, keywordStart))
        } else if (hasAnnotations && modifierEndPos == 0) {
          // When we have annotations but no other modifiers, the space is already in beforeValVar
          // since cursor is positioned after annotations
          beforeValVar = Space.EMPTY
        }
        
        // Move cursor past all modifiers and the keyword
        cursor = adjustedStart + modifierEndPos + keywordStart + keyword.length
        valVarKeyword = keyword
        
        // Extract space after val/var
        // Look for the variable name in the source
        val varNameStr = vd.name.toString
        val nameIndex = source.indexOf(varNameStr, cursor)
        if (nameIndex >= cursor) {
          afterValVar = Space.format(source.substring(cursor, nameIndex))
          cursor = nameIndex
        }
      }
    }
    
    // Val is implicitly final in Scala (but don't add it if we already have explicit final)
    val isFinal = valVarKeyword == "val"
    if (isFinal && !hasExplicitFinal) {
      modifiers.add(new J.Modifier(
        Tree.randomId(),
        if (modifiers.isEmpty) beforeValVar else Space.SINGLE_SPACE,
        Markers.EMPTY,
        null, // No keyword for implicit final
        J.Modifier.Type.Final,
        Collections.emptyList()
      ))
    }
    
    // Handle type annotation if present
    var typeExpression: TypeTree = null
    var beforeColon = Space.EMPTY
    var afterColon = Space.EMPTY
    
    if (vd.tpt != null && !vd.tpt.isEmpty && vd.tpt.span.exists) {
      // Find the end of the variable name in source
      val nameEnd = cursor + vd.name.toString.length
      val typeStart = Math.max(0, vd.tpt.span.start - offsetAdjustment)
      
      if (nameEnd < typeStart && typeStart <= source.length) {
        val between = source.substring(nameEnd, typeStart)
        val colonIndex = between.indexOf(':')
        if (colonIndex >= 0) {
          beforeColon = Space.format(between.substring(0, colonIndex))
          afterColon = Space.format(between.substring(colonIndex + 1))
          cursor = typeStart
        }
      }
      
      // Visit the type - handle special Scala type syntax
      val typeTree = vd.tpt match {
        case f: untpd.Function => visitFunctionType(f)
        case io: untpd.InfixOp => visitInfixType(io)
        case _: untpd.PostfixOp => visitUnknown(vd.tpt)
        case _ => visitTree(vd.tpt)
      }
      typeExpression = typeTree match {
        case tt: TypeTree =>
          // For type expressions in variable declarations, we need to preserve
          // the space after the colon
          tt match {
            case pt: J.ParameterizedType => pt.withPrefix(afterColon)
            case id: J.Identifier => id.withPrefix(afterColon)
            case fa: J.FieldAccess => fa.withPrefix(afterColon)
            case u: J.Unknown => u.withPrefix(afterColon)
            case ft: S.FunctionType => ft.withPrefix(afterColon)
            case bnt: S.ByNameType => bnt.withPrefix(afterColon)
            case tp: S.TuplePattern => tp.withPrefix(afterColon)
            case it: S.InfixType => it.withPrefix(afterColon)
            case _ => tt
          }
        case _ => null
      }
    }
    
    // Check if this is a tuple pattern
    val isTuplePattern = {
      // Look ahead in source to see if we have a tuple pattern like (a, b)
      val lookAhead = source.substring(cursor, Math.min(cursor + 100, source.length))
      lookAhead.trim.startsWith("(") && vd.name.toString == "<pat>"
    }
    
    // Extract variable name or tuple pattern
    val varName: VariableDeclarator = if (isTuplePattern) {
      // This is a tuple pattern like (a, b)
      val tupleStart = source.indexOf('(', cursor)
      if (tupleStart >= 0) {
        val beforeTuple = Space.format(source.substring(cursor, tupleStart))
        cursor = tupleStart + 1 // Move past opening paren
        
        // Find the matching closing paren
        var parenCount = 1
        var tupleEnd = cursor
        while (parenCount > 0 && tupleEnd < source.length) {
          if (source.charAt(tupleEnd) == '(') parenCount += 1
          else if (source.charAt(tupleEnd) == ')') parenCount -= 1
          if (parenCount > 0) tupleEnd += 1
        }
        
        // Parse the elements inside the tuple
        val elements = new util.ArrayList[JRightPadded[Expression]]()
        val tupleContent = source.substring(cursor, tupleEnd)
        val parts = tupleContent.split(",")
        
        for (i <- parts.indices) {
          val part = parts(i).trim
          val beforePart = if (i == 0) Space.EMPTY else Space.SINGLE_SPACE
          val elem = new J.Identifier(
            Tree.randomId(),
            beforePart,
            Markers.EMPTY,
            Collections.emptyList(),
            part,
            null,
            null
          )
          val isLast = i == parts.length - 1
          val rightPadding = if (isLast) Space.EMPTY else Space.EMPTY
          elements.add(JRightPadded.build(elem.asInstanceOf[Expression]).withAfter(rightPadding))
        }
        
        cursor = tupleEnd + 1 // Move past closing paren
        
        S.TuplePattern.build(
          Tree.randomId(),
          beforeTuple,
          Markers.EMPTY,
          JContainer.build(Space.EMPTY, elements, Markers.EMPTY),
          null
        )
      } else {
        // Fallback to regular identifier if we can't parse the tuple
        new J.Identifier(
          Tree.randomId(),
          afterValVar,
          Markers.EMPTY,
          Collections.emptyList(),
          vd.name.toString,
          null,
          null
        )
      }
    } else {
      new J.Identifier(
        Tree.randomId(),
        afterValVar,
        Markers.EMPTY,
        Collections.emptyList(),
        vd.name.toString,
        null,
        null
      )
    }
    
    // Update cursor past the name only if we haven't parsed a type and it's not a tuple pattern
    // If we parsed a type or a tuple pattern, the cursor is already past them
    if (typeExpression == null && !isTuplePattern) {
      cursor = cursor + vd.name.toString.length
    }
    
    // Handle initializer
    var beforeEquals = Space.EMPTY
    var initializer: Expression = null

    // Check for the special underscore initializer first
    if (vd.rhs != null && vd.rhs.toString == "Ident(_)") {
      // Handle uninitialized var: var x: Int = _
      // Look for the underscore in source
      val underscoreIndex = source.indexOf('_', cursor)
      if (underscoreIndex >= 0) {
        val beforeUnderscore = source.substring(cursor, underscoreIndex)
        val equalsIndex = beforeUnderscore.indexOf('=')
        if (equalsIndex >= 0) {
          beforeEquals = Space.format(beforeUnderscore.substring(0, equalsIndex))
          val afterEqualsStr = beforeUnderscore.substring(equalsIndex + 1)
          val afterEquals = Space.format(afterEqualsStr)
          cursor = underscoreIndex + 1
          
          // Create a wildcard for the underscore (Scala's default initializer)
          initializer = new S.Wildcard(
            Tree.randomId(),
            afterEquals,
            Markers.EMPTY,
            null
          )
        }
      }
    } else if (vd.rhs != null && !vd.rhs.isEmpty && vd.rhs.span.exists) {
      val rhsStart = Math.max(0, vd.rhs.span.start - offsetAdjustment)

      // Look for equals sign
      if (cursor < rhsStart && rhsStart <= source.length) {
        val beforeRhs = source.substring(cursor, rhsStart)
        val equalsIndex = beforeRhs.indexOf('=')
        if (equalsIndex >= 0) {
          beforeEquals = Space.format(beforeRhs.substring(0, equalsIndex))
          val afterEqualsStr = beforeRhs.substring(equalsIndex + 1)
          cursor = rhsStart
          
          // Visit the initializer
          val rhsTree = visitTree(vd.rhs)

          rhsTree match {
            case block: J.Block =>
              // In Scala, blocks are expressions. Wrap the block in S.BlockExpression
              val blockExpr = new S.BlockExpression(
                Tree.randomId(),
                Space.format(afterEqualsStr),
                Markers.EMPTY,
                block.withPrefix(Space.EMPTY),
                null // type
              )
              initializer = blockExpr
              
            case expr: Expression =>
              // Set initializer with space after equals
              initializer = expr match {
                case lit: J.Literal => lit.withPrefix(Space.format(afterEqualsStr))
                case id: J.Identifier => id.withPrefix(Space.format(afterEqualsStr))
                case mi: J.MethodInvocation => mi.withPrefix(Space.format(afterEqualsStr))
                case na: J.NewArray => na.withPrefix(Space.format(afterEqualsStr))
                case bin: J.Binary => bin.withPrefix(Space.format(afterEqualsStr))
                case aa: J.ArrayAccess => aa.withPrefix(Space.format(afterEqualsStr))
                case fa: J.FieldAccess => fa.withPrefix(Space.format(afterEqualsStr))
                case paren: J.Parentheses[_] => paren.withPrefix(Space.format(afterEqualsStr))
                case unknown: J.Unknown => unknown.withPrefix(Space.format(afterEqualsStr))
                case nc: J.NewClass => nc.withPrefix(Space.format(afterEqualsStr))
                case lambda: J.Lambda => lambda.withPrefix(Space.format(afterEqualsStr))
                case mr: J.MemberReference => mr.withPrefix(Space.format(afterEqualsStr))
                case tc: J.TypeCast => tc.withPrefix(Space.format(afterEqualsStr))
                case io: J.InstanceOf => io.withPrefix(Space.format(afterEqualsStr))
                case un: J.Unary => un.withPrefix(Space.format(afterEqualsStr))
                case is: S.InterpolatedString => is.withPrefix(Space.format(afterEqualsStr))
                case me: S.MatchExpression => me.withPrefix(Space.format(afterEqualsStr))
                case ta: S.TypeAscription => ta.withPrefix(Space.format(afterEqualsStr))
                case tp: S.TuplePattern => tp.withPrefix(Space.format(afterEqualsStr))
                case _ =>
                  // For any other expression type, just return it as-is
                  expr
              }
              
            case stmt: Statement =>
              // In Scala, statements like try/if/match can be used as expressions.
              // Wrap in J.Unknown since J model doesn't support them as Expression.
              // Use the original source text from the rhs span.
              val stmtSourceStart = Math.max(0, vd.rhs.span.start - offsetAdjustment)
              val stmtSourceEnd = Math.max(0, vd.rhs.span.end - offsetAdjustment)
              val stmtSourceText = if (stmtSourceStart < stmtSourceEnd && stmtSourceEnd <= source.length) {
                source.substring(stmtSourceStart, stmtSourceEnd)
              } else ""
              val unknownSource = new J.Unknown.Source(
                Tree.randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                stmtSourceText
              )
              initializer = new J.Unknown(
                Tree.randomId(),
                Space.format(afterEqualsStr),
                Markers.EMPTY,
                unknownSource
              )
            case _ =>
              // Other types are not valid initializers
              initializer = null
          }
        }
      }
    }
    
    // Update cursor to end of ValDef
    updateCursor(vd.span.end)
    
    // Create variable declarator
    val namedVariable = new J.VariableDeclarations.NamedVariable(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      varName, // VariableDeclarator - J.Identifier implements this
      Collections.emptyList(), // dimensionsAfterName - not used in Scala
      if (initializer != null) JLeftPadded.build(initializer).withBefore(beforeEquals) else null,
      null // variableType - will be set later by type attribution
    )
    
    val declarator = JRightPadded.build(namedVariable)
    
    // Create the variable declarations
    // In Scala, we need to put the type expression in the overall declaration
    // even though it's syntactically attached to each variable
    new J.VariableDeclarations(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      leadingAnnotations, // Pass the annotations we collected
      modifiers,
      typeExpression, // Store type here for now
      null, // varargs
      Collections.emptyList(), // dimensionsBeforeName
      Collections.singletonList(declarator)
    )
  }
  
  private def visitEnumCaseFromModule(md: untpd.ModuleDef): S.EnumCase = {
    val prefix = extractPrefix(md.span)
    val adjustedEnd = Math.max(0, md.span.end - offsetAdjustment)

    // Advance cursor past "case" keyword
    if (cursor < adjustedEnd && adjustedEnd <= source.length) {
      val src = source.substring(cursor, adjustedEnd)
      val caseIdx = src.indexOf("case")
      if (caseIdx >= 0) {
        cursor = cursor + caseIdx + "case".length
      }
    }

    // Extract the name
    val nameStart = if (md.nameSpan.exists) {
      Math.max(0, md.nameSpan.start - offsetAdjustment)
    } else {
      cursor
    }
    val nameSpace = if (cursor < nameStart && nameStart <= source.length) {
      Space.format(source.substring(cursor, nameStart))
    } else {
      Space.format(" ")
    }
    val name = new J.Identifier(
      Tree.randomId(),
      nameSpace,
      Markers.EMPTY,
      Collections.emptyList(),
      md.name.toString,
      null,
      null
    )
    if (md.nameSpan.exists) {
      cursor = Math.max(cursor, md.nameSpan.end - offsetAdjustment)
    }

    // Check for extends clause
    var extending: JLeftPadded[TypeTree] = null
    var arguments: JContainer[Expression] = null

    val template = md.impl
    if (template != null && template.parents.nonEmpty) {
      val searchEnd = Math.min(adjustedEnd, source.length)
      if (cursor < searchEnd) {
        val betweenText = source.substring(cursor, searchEnd)
        val extendsIdx = betweenText.indexOf("extends")
        if (extendsIdx >= 0) {
          val extendsSpace = Space.format(betweenText.substring(0, extendsIdx))
          cursor = cursor + extendsIdx + "extends".length

          val parent = template.parents.head
          // Extract the type identifier from the parent tree
          // Dotty represents `case X extends Y(args)` as Apply(Select(New(Ident(Y)), <init>), args)
          val (typeIdent: untpd.Ident, appArgs: List[untpd.Tree]) = parent match {
            case app: untpd.Apply =>
              val ident = app.fun match {
                case id: untpd.Ident => id
                case sel: untpd.Select =>
                  sel.qualifier match {
                    case nw: untpd.New => nw.tpt.asInstanceOf[untpd.Ident]
                    case id: untpd.Ident => id
                    case _ => null
                  }
                case _ => null
              }
              (ident, if (ident != null) app.args else Nil)
            case id: untpd.Ident => (id, Nil)
            case _ => (null, Nil)
          }

          if (typeIdent != null) {
            val typeId = visitIdent(typeIdent) match {
              case ident: J.Identifier => ident.asInstanceOf[TypeTree]
              case other => other.asInstanceOf[TypeTree]
            }
            extending = JLeftPadded.build(typeId).withBefore(extendsSpace)

            if (appArgs.nonEmpty) {
              val argSearchEnd = Math.min(cursor + 100, source.length)
              val argSearchText = source.substring(cursor, argSearchEnd)
              val parenIdx = argSearchText.indexOf('(')
              val beforeParen = if (parenIdx >= 0) {
                val bp = if (parenIdx > 0) Space.format(argSearchText.substring(0, parenIdx)) else Space.EMPTY
                cursor = cursor + parenIdx + 1
                bp
              } else Space.EMPTY

              val args = new util.ArrayList[JRightPadded[Expression]]()
              appArgs.zipWithIndex.foreach { case (arg, idx) =>
                visitTree(arg) match {
                  case expr: Expression =>
                    val isLast = idx == appArgs.size - 1
                    val after = if (!isLast && cursor < source.length) {
                      val text = source.substring(cursor, Math.min(cursor + 50, source.length))
                      val commaIdx = text.indexOf(',')
                      if (commaIdx >= 0) {
                        val space = Space.format(text.substring(0, commaIdx))
                        cursor = cursor + commaIdx + 1
                        space
                      } else Space.EMPTY
                    } else Space.EMPTY
                    args.add(JRightPadded.build(expr).withAfter(after))
                  case _ =>
                }
              }

              if (cursor < source.length) {
                val closeSearch = source.substring(cursor, Math.min(cursor + 50, source.length))
                val closeIdx = closeSearch.indexOf(')')
                if (closeIdx >= 0) cursor = cursor + closeIdx + 1
              }

              arguments = JContainer.build(beforeParen, args, Markers.EMPTY)
            }
          }
        }
      }
    }

    updateCursor(md.span.end)

    new S.EnumCase(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      name,
      extending,
      arguments
    )
  }

  private def visitModuleDef(md: untpd.ModuleDef): J = {
    // Detect enum cases (e.g., "case Mercury extends Planet(...)")
    if (md.span.exists) {
      val s = Math.max(0, md.span.start - offsetAdjustment)
      val e = Math.max(0, md.span.end - offsetAdjustment)
      if (s < e && e <= source.length) {
        val src = source.substring(s, e).trim
        if (src.startsWith("case") && !src.startsWith("case class") && !src.startsWith("case object")) {
          return visitEnumCaseFromModule(md)
        }
      }
    }

    val prefix = extractPrefix(md.span)
    
    // Extract the source text to find modifiers  
    val adjustedStart = Math.max(0, md.span.start - offsetAdjustment)
    val adjustedEnd = Math.max(0, md.span.end - offsetAdjustment)
    var modifierText = ""
    var objectIndex = -1
    
    if (adjustedStart >= cursor && adjustedEnd <= source.length) {
      val sourceSnippet = source.substring(cursor, adjustedEnd)
      objectIndex = sourceSnippet.indexOf("object")
      if (objectIndex > 0) {
        modifierText = sourceSnippet.substring(0, objectIndex)
      }
    }
    
    // Extract modifiers from text
    val (modifiers, lastModEnd) = extractModifiersFromText(md.mods, modifierText)
    
    // Check for case modifier (special handling as it's not a traditional modifier)
    if (modifierText.contains("case")) {
      val caseIndex = modifierText.indexOf("case")
      if (caseIndex >= 0) {
        // Add case modifier in the correct position
        val caseSpace = if (caseIndex > lastModEnd) {
          Space.format(modifierText.substring(lastModEnd, caseIndex))
        } else {
          Space.EMPTY
        }
        modifiers.add(new J.Modifier(
          Tree.randomId(),
          caseSpace,
          Markers.EMPTY,
          "case",
          J.Modifier.Type.LanguageExtension,
          Collections.emptyList()
        ))
      }
    }
    
    // Objects are implicitly final
    modifiers.add(new J.Modifier(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      null, // No keyword for implicit final
      J.Modifier.Type.Final,
      Collections.emptyList()
    ).withMarkers(Markers.build(Collections.singletonList(new Implicit(Tree.randomId())))))
    
    // Find where "object" keyword ends
    val objectKeywordPos = if (objectIndex >= 0) {
      cursor + objectIndex + "object".length
    } else {
      cursor
    }
    
    // Extract space between modifiers and "object" keyword
    val kindPrefix = if (!modifiers.isEmpty && objectIndex > 0) {
      val afterModifiers = if (modifierText.contains("case")) {
        modifierText.indexOf("case") + "case".length
      } else {
        lastModEnd
      }
      Space.format(modifierText.substring(afterModifiers, objectIndex))
    } else {
      Space.EMPTY
    }
    
    // Update cursor to after "object" keyword
    cursor = objectKeywordPos
    
    // Create the class kind (object instead of class)
    val kind = new J.ClassDeclaration.Kind(
      Tree.randomId(),
      kindPrefix,
      Markers.EMPTY,
      Collections.emptyList(),
      J.ClassDeclaration.Kind.Type.Class // We use Class type but mark with SObject
    )
    
    // Extract space between "object" and the name
    val nameStart = if (md.nameSpan.exists) {
      Math.max(0, md.nameSpan.start - offsetAdjustment)
    } else {
      objectKeywordPos
    }
    
    val nameSpace = if (objectKeywordPos < nameStart && nameStart <= source.length) {
      Space.format(source.substring(objectKeywordPos, nameStart))
    } else {
      Space.format(" ") // Default to single space
    }
    
    // Extract object name
    val name = new J.Identifier(
      Tree.randomId(),
      nameSpace,
      Markers.EMPTY,
      Collections.emptyList(),
      md.name.toString,
      null,
      null
    )
    
    // Update cursor to after the name
    if (md.nameSpan.exists) {
      cursor = Math.max(0, md.nameSpan.end - offsetAdjustment)
    }
    
    // Objects cannot have type parameters
    val typeParameters: JContainer[J.TypeParameter] = null
    
    // Objects cannot have constructor parameters  
    val primaryConstructor: JContainer[Statement] = null
    
    // Extract extends/with clauses from the implementation template
    var extendings: JLeftPadded[TypeTree] = null
    var implementings: JContainer[TypeTree] = null
    
    md.impl match {
      case tmpl: untpd.Template if tmpl.parents.nonEmpty =>
        // Handle extends/with clauses similar to classes
        // Look for "extends" keyword and extract space before it
        var extendsSpace = Space.format(" ")
        if (cursor < source.length && tmpl.parents.head.span.exists) {
          val parentStart = Math.max(0, tmpl.parents.head.span.start - offsetAdjustment)
          if (cursor < parentStart && parentStart <= source.length) {
            val beforeParent = source.substring(cursor, parentStart)
            val extendsIdx = beforeParent.indexOf("extends")
            if (extendsIdx >= 0) {
              // Space is only the whitespace before "extends"
              extendsSpace = Space.format(beforeParent.substring(0, extendsIdx))
              // Update cursor to after "extends" keyword
              cursor = cursor + extendsIdx + "extends".length
            } else {
              // No "extends" found, use full space
              extendsSpace = Space.format(beforeParent)
              cursor = parentStart
            }
          }
        }
        
        // Now visit the parent with cursor positioned correctly
        val firstParent = tmpl.parents.head
        val extendsType = visitTree(firstParent) match {
          case typeTree: TypeTree => typeTree
          case _ => visitUnknown(firstParent).asInstanceOf[TypeTree]
        }
        
        extendings = new JLeftPadded(extendsSpace, extendsType, Markers.EMPTY)
        
        // Handle additional parents as implements (with clauses)
        if (tmpl.parents.size > 1) {
          val implementsList = new util.ArrayList[JRightPadded[TypeTree]]()
          
          // Find space before first "with"
          var containerSpace = Space.format(" ")
          if (cursor < source.length && tmpl.parents(1).span.exists) {
            val firstWithParentStart = Math.max(0, tmpl.parents(1).span.start - offsetAdjustment)
            if (cursor < firstWithParentStart) {
              val beforeFirstWith = source.substring(cursor, firstWithParentStart)
              val withIdx = beforeFirstWith.indexOf("with")
              if (withIdx >= 0) {
                containerSpace = Space.format(beforeFirstWith.substring(0, withIdx))
                cursor = cursor + withIdx + "with".length
              }
            }
          }
          
          for (i <- 1 until tmpl.parents.size) {
            val parent = tmpl.parents(i)
            val implType = visitTree(parent) match {
              case typeTree: TypeTree => typeTree
              case _ => visitUnknown(parent).asInstanceOf[TypeTree]
            }
            
            // For subsequent traits, extract space between them
            var trailingSpace = Space.EMPTY
            if (i < tmpl.parents.size - 1 && parent.span.exists && tmpl.parents(i + 1).span.exists) {
              val thisEnd = Math.max(0, parent.span.end - offsetAdjustment)
              val nextStart = Math.max(0, tmpl.parents(i + 1).span.start - offsetAdjustment)
              if (thisEnd < nextStart && nextStart <= source.length) {
                val between = source.substring(thisEnd, nextStart)
                val withIdx = between.indexOf("with")
                if (withIdx >= 0) {
                  trailingSpace = Space.format(between.substring(0, withIdx))
                  // Update cursor past "with"
                  cursor = thisEnd + withIdx + "with".length
                } else {
                  trailingSpace = Space.format(between)
                }
              }
            }
            
            implementsList.add(new JRightPadded(implType, trailingSpace, Markers.EMPTY))
          }
          implementings = JContainer.build(containerSpace, implementsList, Markers.EMPTY)
        }
        
      case _ =>
    }
    
    // Extract body
    val body = md.impl match {
      case tmpl: untpd.Template if tmpl.body.nonEmpty =>
        // Find the opening brace
        var bodyPrefix = Space.EMPTY
        if (cursor < source.length && md.span.exists) {
          val remaining = source.substring(cursor, Math.min(md.span.end - offsetAdjustment, source.length))
          val braceIdx = remaining.indexOf('{')
          if (braceIdx >= 0) {
            bodyPrefix = Space.format(remaining.substring(0, braceIdx))
            cursor = cursor + braceIdx + 1 // Skip past the opening brace
          }
        }
        
        // Create a block from the template body
        val statements = new util.ArrayList[JRightPadded[Statement]]()
        tmpl.body.foreach { stat =>
          visitTree(stat) match {
            case stmt: Statement => statements.add(JRightPadded.build(stmt))
            case _ => // Skip non-statements
          }
        }
        
        // Find the closing brace to get the end space
        var endSpace = Space.EMPTY
        if (cursor < source.length && md.span.exists) {
          val endPos = Math.max(0, md.span.end - offsetAdjustment) 
          val remaining = source.substring(cursor, Math.min(endPos, source.length))
          val closeBraceIdx = remaining.lastIndexOf('}')
          if (closeBraceIdx >= 0) {
            endSpace = Space.format(remaining.substring(0, closeBraceIdx))
            cursor = endPos // Update to end of object
          }
        }
        
        new J.Block(
          Tree.randomId(),
          bodyPrefix,
          Markers.EMPTY,
          JRightPadded.build(false),
          statements,
          endSpace
        )
        
      case _ =>
        // Empty body - object without braces
        new J.Block(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          JRightPadded.build(false),
          Collections.emptyList(),
          Space.EMPTY
        ).withMarkers(Markers.build(Collections.singletonList(new OmitBraces(Tree.randomId()))))
    }
    
    // Update cursor to end of module def
    if (md.span.exists) {
      cursor = Math.max(cursor, md.span.end - offsetAdjustment)
    }
    
    // Create the class declaration with SObject marker
    new J.ClassDeclaration(
      Tree.randomId(),
      prefix,
      Markers.build(Collections.singletonList(SObject.create())),
      Collections.emptyList(), // annotations
      modifiers,
      kind,
      name,
      typeParameters,
      primaryConstructor,
      extendings,
      implementings,
      null, // permits
      body,
      null // type
    )
  }
  
  private def visitAssign(asg: untpd.Assign): J = {
    val prefix = extractPrefix(asg.span)

    // Check if the left-hand side is a tuple pattern
    val isTuplePattern = asg.lhs match {
      case _: untpd.Tuple => true
      case _ => false
    }
    
    // Visit the left-hand side (variable or tuple pattern)
    val variable = if (isTuplePattern) {
      // Parse tuple pattern assignment like (a, b) = (3, 4)
      // We maintain our own internal cursor for parsing the tuple
      var tupleCursor = cursor
      val elements = new util.ArrayList[JRightPadded[Expression]]()
      
      // Capture prefix space before the tuple
      val prefixStart = tupleCursor
      // Skip to opening paren
      while (tupleCursor < source.length && source.charAt(tupleCursor) != '(') {
        tupleCursor += 1
      }
      val tuplePrefix = Space.format(source.substring(prefixStart, tupleCursor))
      
      if (tupleCursor < source.length && source.charAt(tupleCursor) == '(') {
        tupleCursor += 1 // Skip opening paren
        
        // Parse each element
        var done = false
        while (tupleCursor < source.length && !done) {
          // Skip whitespace
          var beforeElem = ""
          while (tupleCursor < source.length && source.charAt(tupleCursor).isWhitespace) {
            beforeElem += source.charAt(tupleCursor)
            tupleCursor += 1
          }
          
          // Check if we've reached the closing paren
          if (tupleCursor < source.length && source.charAt(tupleCursor) == ')') {
            done = true
          } else if (tupleCursor < source.length) {
            // Parse the identifier
            var identStart = tupleCursor
            while (tupleCursor < source.length && 
                   source.charAt(tupleCursor) != ',' && 
                   source.charAt(tupleCursor) != ')' &&
                   !source.charAt(tupleCursor).isWhitespace) {
              tupleCursor += 1
            }
            
            val identName = source.substring(identStart, tupleCursor)
            
            // Skip trailing whitespace
            var afterElem = ""
            while (tupleCursor < source.length && source.charAt(tupleCursor).isWhitespace) {
              afterElem += source.charAt(tupleCursor)
              tupleCursor += 1
            }
            
            // Create the identifier
            val elem = new J.Identifier(
              Tree.randomId(),
              Space.format(beforeElem),
              Markers.EMPTY,
              Collections.emptyList(),
              identName,
              null,
              null
            )
            
            // Check for comma
            if (tupleCursor < source.length && source.charAt(tupleCursor) == ',') {
              tupleCursor += 1 // Skip comma
              elements.add(JRightPadded.build(elem.asInstanceOf[Expression]).withAfter(Space.EMPTY))
            } else {
              // Last element or reached closing paren
              elements.add(JRightPadded.build(elem.asInstanceOf[Expression]).withAfter(Space.format(afterElem)))
              if (tupleCursor < source.length && source.charAt(tupleCursor) == ')') {
                done = true
              }
            }
          }
        }
        
        // Skip closing paren
        if (tupleCursor < source.length && source.charAt(tupleCursor) == ')') {
          tupleCursor += 1
        }
      }
      
      // Update main cursor to after the tuple (but before the equals sign)
      cursor = tupleCursor
      
      S.TuplePattern.build(
        Tree.randomId(),
        tuplePrefix,
        Markers.EMPTY,
        JContainer.build(Space.EMPTY, elements, Markers.EMPTY),
        null
      )
    } else {
      visitTree(asg.lhs) match {
        case expr: Expression => expr
        case _ => return visitUnknown(asg)
      }
    }
    
    // Find the position of the equals sign
    // For tuple patterns, use our cursor position; otherwise use the span
    val lhsEnd = if (isTuplePattern) cursor else Math.max(0, asg.lhs.span.end - offsetAdjustment)
    val rhsStart = Math.max(0, asg.rhs.span.start - offsetAdjustment)
    var equalsSpace = Space.EMPTY
    var valueSpace = Space.EMPTY
    var isCompoundAssignment = false
    var compoundOperator: J.AssignmentOperation.Type = null
    
    if (lhsEnd < rhsStart && lhsEnd >= cursor && rhsStart <= source.length) {
      val between = source.substring(lhsEnd, rhsStart)
      
      // Check for compound assignment operators
      val compoundPattern = """(\s*)([\+\-\*/%&\|\^]|<<|>>|>>>)=(\s*)""".r
      compoundPattern.findFirstMatchIn(between) match {
        case Some(m) =>
          isCompoundAssignment = true
          equalsSpace = Space.format(m.group(1))
          valueSpace = Space.format(m.group(3))
          compoundOperator = m.group(2) match {
            case "+" => J.AssignmentOperation.Type.Addition
            case "-" => J.AssignmentOperation.Type.Subtraction
            case "*" => J.AssignmentOperation.Type.Multiplication
            case "/" => J.AssignmentOperation.Type.Division
            case "%" => J.AssignmentOperation.Type.Modulo
            case "&" => J.AssignmentOperation.Type.BitAnd
            case "|" => J.AssignmentOperation.Type.BitOr
            case "^" => J.AssignmentOperation.Type.BitXor
            case "<<" => J.AssignmentOperation.Type.LeftShift
            case ">>" => J.AssignmentOperation.Type.RightShift
            case ">>>" => J.AssignmentOperation.Type.UnsignedRightShift
            case _ => J.AssignmentOperation.Type.Addition // fallback
          }
          cursor = rhsStart
        case None =>
          // Regular assignment
          val equalsIndex = between.indexOf('=')
          if (equalsIndex >= 0) {
            equalsSpace = Space.format(between.substring(0, equalsIndex))
            val afterEquals = equalsIndex + 1
            if (afterEquals < between.length) {
              valueSpace = Space.format(between.substring(afterEquals))
            }
            cursor = rhsStart
          }
      }
    }
    
    // Visit the right-hand side (value)
    val value = visitTree(asg.rhs) match {
      case expr: Expression => expr
      case _ => return visitUnknown(asg)
    }
    
    // Update cursor to the end of the assignment
    updateCursor(asg.span.end)
    
    if (isCompoundAssignment) {
      // Check if rhs is a binary operation with lhs as the left operand
      // Scala desugars x += 5 to x = x + 5
      val assignment = asg.rhs match {
        case app: untpd.Apply =>
          app.fun match {
            case sel: untpd.Select if sel.qualifier == asg.lhs =>
              // This is the desugared form, extract just the right operand
              visitTree(app.args.head) match {
                case expr: Expression => expr
                case _ => value
              }
            case _ => value
          }
        case _ => value
      }
      
      new J.AssignmentOperation(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        variable,
        JLeftPadded.build(compoundOperator).withBefore(equalsSpace),
        assignment.withPrefix(valueSpace),
        null // type
      )
    } else {
      new J.Assignment(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        variable,
        JLeftPadded.build(value.withPrefix(valueSpace)).withBefore(equalsSpace),
        null // type - will be inferred later
      )
    }
  }
  
  private def visitIf(ifTree: untpd.If): J = {
    val prefix = extractPrefix(ifTree.span)
    
    
    // Find where the condition parentheses start
    val adjustedStart = Math.max(0, ifTree.span.start - offsetAdjustment)
    val condStart = Math.max(0, ifTree.cond.span.start - offsetAdjustment)
    
    // Extract space before parentheses and move cursor past "if" to the condition
    var beforeParenSpace = Space.EMPTY
    if (adjustedStart < condStart && cursor <= condStart) {
      // Look for the opening parenthesis after "if"
      val searchEnd = Math.min(condStart + 1, source.length) // Include the '(' character
      val between = source.substring(cursor, searchEnd)
      val ifIndex = between.indexOf("if")
      if (ifIndex >= 0) {
        val afterIf = ifIndex + 2
        val remainingStr = between.substring(afterIf)
        val parenIndex = remainingStr.indexOf('(')
        if (parenIndex >= 0) {
          beforeParenSpace = Space.format(remainingStr.substring(0, parenIndex))
          // Move cursor to the opening parenthesis
          cursor = cursor + afterIf + parenIndex
        }
      }
    }
    
    // For if conditions, we need to handle Parens specially - extract the inner expression
    val conditionExpr = ifTree.cond match {
      case parens: untpd.Parens =>
        // Skip the opening parenthesis since ControlParentheses will add it
        cursor = cursor + 1
        // Get the inner expression from Parens
        val innerTree = try {
          // Try different possible field names
          val treeField = parens.getClass.getDeclaredFields.find(f => 
            f.getName.contains("tree") || f.getName.contains("expr") || f.getName.contains("arg")
          )
          
          treeField match {
            case Some(field) =>
              field.setAccessible(true)
              field.get(parens).asInstanceOf[untpd.Tree]
            case None =>
              // Fall back to productElement approach
              if (parens.productArity > 0) {
                parens.productElement(0).asInstanceOf[untpd.Tree]
              } else {
                parens
              }
          }
        } catch {
          case _: Exception => parens
        }
        innerTree
      case other => other
    }
    
    // Visit the condition expression
    val condition = visitTree(conditionExpr) match {
      case expr: Expression => expr
      case _ => return visitUnknown(ifTree).asInstanceOf[J.If]
    }
    
    // Extract space after condition
    var afterCondSpace = Space.EMPTY
    ifTree.cond match {
      case parens: untpd.Parens =>
        // For Parens, we need to extract the space before the closing paren
        val innerEnd = conditionExpr.span.end
        val parenEnd = parens.span.end
        if (innerEnd < parenEnd - 1) {
          val adjustedInnerEnd = Math.max(0, innerEnd - offsetAdjustment)
          val adjustedParenEnd = Math.max(0, parenEnd - 1 - offsetAdjustment)
          if (adjustedInnerEnd < adjustedParenEnd && adjustedInnerEnd >= cursor && adjustedParenEnd <= source.length) {
            afterCondSpace = Space.format(source.substring(adjustedInnerEnd, adjustedParenEnd))
            cursor = adjustedParenEnd + 1 // Skip the closing paren
          } else {
            cursor = Math.max(0, parenEnd - offsetAdjustment)
          }
        } else {
          cursor = Math.max(0, parenEnd - offsetAdjustment)
        }
      case _ =>
        // For non-parenthesized conditions, just move cursor to end
        cursor = Math.max(0, ifTree.cond.span.end - offsetAdjustment)
    }
    
    // Visit the then branch
    val thenResult = visitTree(ifTree.thenp)
    val thenPart = thenResult match {
      case stmt: Statement => JRightPadded.build(stmt)
      case expr: Expression =>
        // In Scala, expressions like literals can appear as then branches
        JRightPadded.build(asStatement(expr))
      case _ => return visitUnknown(ifTree).asInstanceOf[J.If]
    }
    
    // Handle optional else branch
    val elsePart = if (ifTree.elsep.isEmpty) {
      null
    } else {
      // Extract space before "else"
      val thenEnd = Math.max(0, ifTree.thenp.span.end - offsetAdjustment)
      val elseStart = Math.max(0, ifTree.elsep.span.start - offsetAdjustment)
      var elsePrefix = Space.EMPTY
      if (thenEnd < elseStart && cursor <= thenEnd) {
        val between = source.substring(thenEnd, elseStart)
        val elseIndex = between.indexOf("else")
        if (elseIndex >= 0) {
          elsePrefix = Space.format(between.substring(0, elseIndex))
          cursor = thenEnd + elseIndex + 4 // "else" is 4 chars
        }
      }
      
      val elseResult = visitTree(ifTree.elsep)
      elseResult match {
        case stmt: Statement =>
          new J.If.Else(
            Tree.randomId(),
            elsePrefix,
            Markers.EMPTY,
            JRightPadded.build(stmt)
          )
        case expr: Expression =>
          new J.If.Else(
            Tree.randomId(),
            elsePrefix,
            Markers.EMPTY,
            JRightPadded.build(asStatement(expr))
          )
        case _ => return visitUnknown(ifTree).asInstanceOf[J.If]
      }
    }
    
    // Update cursor to end of the if expression
    updateCursor(ifTree.span.end)
    
    new J.If(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      new J.ControlParentheses(
        Tree.randomId(),
        beforeParenSpace,
        Markers.EMPTY,
        JRightPadded.build(condition).withAfter(afterCondSpace)
      ),
      thenPart,
      elsePart
    )
  }
  
  private def visitWhileDo(whileTree: untpd.WhileDo): J.WhileLoop = {
    val prefix = extractPrefix(whileTree.span)
    
    // Find where the condition parentheses start
    val adjustedStart = Math.max(0, whileTree.span.start - offsetAdjustment)
    val condStart = Math.max(0, whileTree.cond.span.start - offsetAdjustment)
    
    // Extract space before parentheses and move cursor past "while" to the condition
    var beforeParenSpace = Space.EMPTY
    if (adjustedStart < condStart && cursor <= condStart) {
      val searchEnd = Math.min(condStart + 1, source.length) // Include the '(' character
      val between = source.substring(cursor, searchEnd)
      val whileIndex = between.indexOf("while")
      if (whileIndex >= 0) {
        val afterWhile = whileIndex + 5 // "while" is 5 chars
        val remainingStr = between.substring(afterWhile)
        val parenIndex = remainingStr.indexOf('(')
        if (parenIndex >= 0) {
          beforeParenSpace = Space.format(remainingStr.substring(0, parenIndex))
          // Move cursor to the opening parenthesis
          cursor = cursor + afterWhile + parenIndex
        }
      }
    }
    
    // For while conditions, we need to handle Parens specially - extract the inner expression
    val conditionExpr = whileTree.cond match {
      case parens: untpd.Parens =>
        // Skip the opening parenthesis since ControlParentheses will add it
        cursor = cursor + 1
        // Get the inner expression from Parens
        val innerTree = try {
          val treeField = parens.getClass.getDeclaredFields.find(f => 
            f.getName.contains("tree") || f.getName.contains("expr") || f.getName.contains("arg")
          )
          
          treeField match {
            case Some(field) =>
              field.setAccessible(true)
              field.get(parens).asInstanceOf[untpd.Tree]
            case None =>
              if (parens.productArity > 0) {
                parens.productElement(0).asInstanceOf[untpd.Tree]
              } else {
                parens
              }
          }
        } catch {
          case _: Exception => parens
        }
        innerTree
      case other => other
    }
    
    // Visit the condition expression
    val condition = visitTree(conditionExpr) match {
      case expr: Expression => expr
      case _ => return visitUnknown(whileTree).asInstanceOf[J.WhileLoop]
    }
    
    // Extract space after condition
    var afterCondSpace = Space.EMPTY
    whileTree.cond match {
      case parens: untpd.Parens =>
        val innerEnd = conditionExpr.span.end
        val parenEnd = parens.span.end
        if (innerEnd < parenEnd - 1) {
          val adjustedInnerEnd = Math.max(0, innerEnd - offsetAdjustment)
          val adjustedParenEnd = Math.max(0, parenEnd - 1 - offsetAdjustment)
          if (adjustedInnerEnd < adjustedParenEnd && adjustedInnerEnd >= cursor && adjustedParenEnd <= source.length) {
            afterCondSpace = Space.format(source.substring(adjustedInnerEnd, adjustedParenEnd))
            cursor = adjustedParenEnd + 1 // Skip the closing paren
          } else {
            cursor = Math.max(0, parenEnd - offsetAdjustment)
          }
        } else {
          cursor = Math.max(0, parenEnd - offsetAdjustment)
        }
      case _ =>
        cursor = Math.max(0, whileTree.cond.span.end - offsetAdjustment)
    }
    
    // Visit the body
    val body = visitTree(whileTree.body) match {
      case stmt: Statement => JRightPadded.build(stmt)
      case _ => return visitUnknown(whileTree).asInstanceOf[J.WhileLoop]
    }
    
    // Update cursor to end of the while loop
    updateCursor(whileTree.span.end)
    
    new J.WhileLoop(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      new J.ControlParentheses(
        Tree.randomId(),
        beforeParenSpace,
        Markers.EMPTY,
        JRightPadded.build(condition).withAfter(afterCondSpace)
      ),
      body
    )
  }
  
  private def visitMatch(matchTree: untpd.Match): J = {
    val prefix = extractPrefix(matchTree.span)

    // Visit the selector expression
    val selector = visitTree(matchTree.selector) match {
      case expr: Expression => expr
      case other =>
        cursor = Math.max(0, matchTree.span.start - offsetAdjustment)
        return visitUnknown(matchTree)
    }

    // Find "match" keyword and opening brace in source
    val searchEnd = Math.min(cursor + 200, source.length)
    val searchText = source.substring(cursor, searchEnd)
    val matchIdx = searchText.indexOf("match")
    if (matchIdx < 0) {
      cursor = Math.max(0, matchTree.span.start - offsetAdjustment)
      return visitUnknown(matchTree)
    }
    val beforeMatch = Space.format(searchText.substring(0, matchIdx))
    cursor = cursor + matchIdx + "match".length

    // Find opening brace
    val braceSearchEnd = Math.min(cursor + 50, source.length)
    val braceText = source.substring(cursor, braceSearchEnd)
    val braceIdx = braceText.indexOf('{')
    if (braceIdx < 0) {
      cursor = Math.max(0, matchTree.span.start - offsetAdjustment)
      return visitUnknown(matchTree)
    }
    cursor = cursor + braceIdx + 1

    // Visit each CaseDef
    val caseClauses = new util.ArrayList[S.CaseClause]()
    var i = 0
    while (i < matchTree.cases.size) {
      val caseDef = matchTree.cases(i)
      val caseClause = visitCaseDef(caseDef, i < matchTree.cases.size - 1,
        if (i + 1 < matchTree.cases.size) matchTree.cases(i + 1) else null)
      caseClauses.add(caseClause)
      i += 1
    }

    // Find closing brace
    val lastCaseEnd = if (matchTree.cases.nonEmpty && matchTree.cases.last.span.exists) {
      Math.max(0, matchTree.cases.last.span.end - offsetAdjustment)
    } else cursor
    if (lastCaseEnd > cursor) cursor = lastCaseEnd

    var endSpace = Space.EMPTY
    if (cursor < source.length) {
      val remaining = source.substring(cursor, Math.min(cursor + 200, source.length))
      val closeBrace = remaining.indexOf('}')
      if (closeBrace >= 0) {
        endSpace = Space.format(remaining.substring(0, closeBrace))
        cursor = cursor + closeBrace + 1
      }
    }

    updateCursor(matchTree.span.end)

    new S.MatchExpression(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      selector,
      beforeMatch,
      caseClauses,
      endSpace,
      null
    )
  }

  private def visitPartialFunction(matchTree: untpd.Match): J.Block = {
    val blockPrefix = extractPrefix(matchTree.span)

    // Find and advance past opening brace
    if (cursor < source.length) {
      val searchEnd = Math.min(cursor + 50, source.length)
      val braceText = source.substring(cursor, searchEnd)
      val braceIdx = braceText.indexOf('{')
      if (braceIdx >= 0) cursor = cursor + braceIdx + 1
    }

    // Visit each CaseDef using existing visitCaseDef
    val statements = new util.ArrayList[JRightPadded[Statement]]()
    matchTree.cases.zipWithIndex.foreach { case (caseDef, i) =>
      val caseClause = visitCaseDef(caseDef, i < matchTree.cases.size - 1,
        if (i + 1 < matchTree.cases.size) matchTree.cases(i + 1) else null)
      statements.add(JRightPadded.build(caseClause.asInstanceOf[Statement]))
    }

    // Find closing brace
    val lastCaseEnd = if (matchTree.cases.nonEmpty && matchTree.cases.last.span.exists) {
      Math.max(0, matchTree.cases.last.span.end - offsetAdjustment)
    } else cursor
    if (lastCaseEnd > cursor) cursor = lastCaseEnd

    var endSpace = Space.EMPTY
    if (cursor < source.length) {
      val remaining = source.substring(cursor, Math.min(cursor + 200, source.length))
      val closeBrace = remaining.indexOf('}')
      if (closeBrace >= 0) {
        endSpace = Space.format(remaining.substring(0, closeBrace))
        cursor = cursor + closeBrace + 1
      }
    }

    new J.Block(
      Tree.randomId(),
      blockPrefix,
      Markers.EMPTY,
      JRightPadded.build(false),
      statements,
      endSpace
    )
  }

  private def visitCaseDef(caseDef: untpd.CaseDef, hasNext: Boolean,
                           nextCase: untpd.CaseDef): S.CaseClause = {
    // Extract prefix (whitespace before "case")
    val casePrefix = extractPrefix(caseDef.span)

    // Move cursor past "case "
    val caseStart = Math.max(0, caseDef.span.start - offsetAdjustment)
    if (caseStart >= cursor) {
      cursor = caseStart
    }
    // Find "case" keyword
    val searchText = source.substring(cursor, Math.min(cursor + 20, source.length))
    val caseKeyIdx = searchText.indexOf("case")
    if (caseKeyIdx >= 0) {
      cursor = cursor + caseKeyIdx + "case".length
    }

    // Now we need to find the pattern, optional guard, and arrow "=>"
    // The pattern ends at either "if" (guard) or "=>" (arrow)
    // We'll find the arrow position first, then check for guard
    val caseEnd = Math.max(0, caseDef.span.end - offsetAdjustment)
    val caseSource = source.substring(cursor, Math.min(caseEnd, source.length))

    // Find "=>" in the case source (need to be careful about nested =>)
    val arrowIdx = findArrowIndex(caseSource)
    if (arrowIdx < 0) {
      // Fallback - return as unknown-based case clause
      val unknownSource = new J.Unknown.Source(
        Tree.randomId(), Space.EMPTY, Markers.EMPTY,
        source.substring(cursor, Math.min(caseEnd, source.length))
      )
      val pattern = new J.Unknown(Tree.randomId(), Space.EMPTY, Markers.EMPTY, unknownSource)
      cursor = caseEnd
      return new S.CaseClause(
        Tree.randomId(), casePrefix, Markers.EMPTY,
        pattern, null, Space.EMPTY, Collections.emptyList()
      )
    }

    val beforeArrow = caseSource.substring(0, arrowIdx)
    val afterArrow = caseSource.substring(arrowIdx + 2)

    // Check for guard ("if" in the pattern area)
    // Need to find "if" that's NOT inside parentheses or braces
    val guardIfIdx = findGuardIfIndex(beforeArrow)

    var pattern: J = null
    var guard: JLeftPadded[Expression] = null

    if (guardIfIdx >= 0) {
      // Pattern is before "if", guard is between "if" and "=>"
      val patternText = beforeArrow.substring(0, guardIfIdx).trim
      val guardText = beforeArrow.substring(guardIfIdx + 2).trim

      val patternSource = new J.Unknown.Source(
        Tree.randomId(), Space.EMPTY, Markers.EMPTY, patternText
      )
      pattern = new J.Unknown(Tree.randomId(), Space.EMPTY, Markers.EMPTY, patternSource)

      val guardSource = new J.Unknown.Source(
        Tree.randomId(), Space.EMPTY, Markers.EMPTY, guardText
      )
      val guardExpr = new J.Unknown(
        Tree.randomId(), Space.format(" "), Markers.EMPTY, guardSource
      )
      guard = new JLeftPadded(Space.format(" "), guardExpr.asInstanceOf[Expression], Markers.EMPTY)
    } else {
      // No guard - pattern is everything before "=>"
      val patternText = beforeArrow.trim
      val patternSource = new J.Unknown.Source(
        Tree.randomId(), Space.EMPTY, Markers.EMPTY, patternText
      )
      pattern = new J.Unknown(Tree.randomId(), Space.EMPTY, Markers.EMPTY, patternSource)
    }

    // Parse body statements after "=>"
    cursor = cursor + arrowIdx + 2
    // Space before "=>" - trailing whitespace of beforeArrow
    val arrowSpace = Space.format(beforeArrow.substring(beforeArrow.stripTrailing().length))

    // The body extends to the end of this CaseDef
    val bodyStatements = new util.ArrayList[JRightPadded[Statement]]()

    if (caseDef.body != null && !caseDef.body.isEmpty && caseDef.body.span.exists) {
      // For multi-statement case bodies, Dotty wraps them in a Block.
      // Visit individual statements from the block to preserve whitespace properly,
      // or visit the single expression/statement directly.
      caseDef.body match {
        case block: untpd.Block if block.stats.nonEmpty =>
          // Multi-statement body - visit each statement individually
          for (stat <- block.stats) {
            val statTree = visitTree(stat)
            statTree match {
              case s: Statement => bodyStatements.add(JRightPadded.build(s))
              case _ =>
                val unk = visitUnknown(stat)
                bodyStatements.add(JRightPadded.build(unk.asInstanceOf[Statement]))
            }
          }
          // Visit the final expression (block.expr)
          if (!block.expr.isEmpty) {
            val exprTree = visitTree(block.expr)
            exprTree match {
              case s: Statement => bodyStatements.add(JRightPadded.build(s))
              case _ =>
                val unk = visitUnknown(block.expr)
                bodyStatements.add(JRightPadded.build(unk.asInstanceOf[Statement]))
            }
          }
        case _ =>
          // Single statement/expression body
          val bodyTree = visitTree(caseDef.body)
          bodyTree match {
            case block: J.Block =>
              // Block returned from non-Block AST node - extract statements
              var si = 0
              while (si < block.getStatements.size()) {
                bodyStatements.add(JRightPadded.build(block.getStatements.get(si)))
                si += 1
              }
            case stmt: Statement =>
              bodyStatements.add(JRightPadded.build(stmt))
            case _ =>
              val unk = visitUnknown(caseDef.body)
              bodyStatements.add(JRightPadded.build(unk.asInstanceOf[Statement]))
          }
      }
    }

    new S.CaseClause(
      Tree.randomId(), casePrefix, Markers.EMPTY,
      pattern, guard, arrowSpace, bodyStatements
    )
  }

  private def findArrowIndex(text: String): Int = {
    var i = 0
    var parenDepth = 0
    var braceDepth = 0
    while (i < text.length - 1) {
      val c = text.charAt(i)
      if (c == '(') parenDepth += 1
      else if (c == ')') parenDepth -= 1
      else if (c == '{') braceDepth += 1
      else if (c == '}') braceDepth -= 1
      else if (c == '=' && text.charAt(i + 1) == '>' && parenDepth == 0 && braceDepth == 0) {
        return i
      }
      i += 1
    }
    -1
  }

  private def findGuardIfIndex(text: String): Int = {
    // Find "if" that is not inside parens/braces and is preceded by whitespace
    var i = 0
    var parenDepth = 0
    var braceDepth = 0
    while (i < text.length - 1) {
      val c = text.charAt(i)
      if (c == '(') parenDepth += 1
      else if (c == ')') parenDepth -= 1
      else if (c == '{') braceDepth += 1
      else if (c == '}') braceDepth -= 1
      else if (c == 'i' && text.charAt(i + 1) == 'f' && parenDepth == 0 && braceDepth == 0) {
        // Check that it's preceded by whitespace and followed by whitespace/paren
        val precededBySpace = i == 0 || text.charAt(i - 1).isWhitespace
        val followedBySpace = i + 2 >= text.length || text.charAt(i + 2).isWhitespace || text.charAt(i + 2) == '('
        if (precededBySpace && followedBySpace) {
          return i
        }
      }
      i += 1
    }
    -1
  }

  private def visitForDo(forTree: untpd.ForDo): J = {
    visitForComprehension(forTree.enums, forTree.body, forTree.span, isYield = false)
  }

  private def visitForComprehension(enums: scala.collection.immutable.List[untpd.Tree], body: untpd.Tree, span: Spans.Span, isYield: Boolean): J = {
    // Only handle single-generator for comprehensions as J.ForEachLoop for now
    // Multi-generator comprehensions fall back to J.Unknown
    val savedCursor = cursor
    try {
      if (enums.size != 1 || !enums.head.isInstanceOf[untpd.GenFrom]) {
        // Multi-generator or complex comprehension - fall back
        cursor = savedCursor
        val prefix = extractPrefix(span)
        val adjustedEnd = Math.max(0, span.end - offsetAdjustment)
        val text = if (cursor <= adjustedEnd && adjustedEnd <= source.length) {
          source.substring(cursor, adjustedEnd)
        } else ""
        cursor = adjustedEnd
        new J.Unknown(
          Tree.randomId(),
          prefix,
          Markers.EMPTY,
          new J.Unknown.Source(
            Tree.randomId(),
            Space.EMPTY,
            Markers.EMPTY,
            text
          )
        )
      } else {
        val genFrom = enums.head.asInstanceOf[untpd.GenFrom]
        visitSingleGeneratorFor(genFrom, body, span, isYield)
      }
    } catch {
      case _: Exception =>
        cursor = savedCursor
        val prefix = extractPrefix(span)
        val adjustedEnd = Math.max(0, span.end - offsetAdjustment)
        val text = if (cursor <= adjustedEnd && adjustedEnd <= source.length) {
          source.substring(cursor, adjustedEnd)
        } else ""
        cursor = adjustedEnd
        new J.Unknown(
          Tree.randomId(),
          prefix,
          Markers.EMPTY,
          new J.Unknown.Source(
            Tree.randomId(),
            Space.EMPTY,
            Markers.EMPTY,
            text
          )
        )
    }
  }

  private def visitSingleGeneratorFor(genFrom: untpd.GenFrom, body: untpd.Tree, span: Spans.Span, isYield: Boolean): J = {
    val prefix = extractPrefix(span)

    // Consume "for"
    val forPos = positionOfNext("for")
    if (forPos >= 0) cursor = forPos + "for".length

    // Find opening paren/brace for the control
    val controlSearchEnd = Math.min(cursor + 30, source.length)
    val controlSearch = source.substring(cursor, controlSearchEnd)
    val parenIdx = controlSearch.indexOf('(')
    val braceIdx = controlSearch.indexOf('{')
    val usesBraces = braceIdx >= 0 && (parenIdx < 0 || braceIdx < parenIdx)
    val openChar = if (usesBraces) '{' else '('
    val closeChar = if (usesBraces) '}' else ')'
    val openIdx = if (usesBraces) braceIdx else parenIdx

    val controlPrefix = if (openIdx >= 0) {
      val pfx = if (openIdx > 0) Space.format(controlSearch.substring(0, openIdx)) else Space.EMPTY
      cursor = cursor + openIdx + 1
      pfx
    } else Space.EMPTY

    // Visit the pattern (loop variable)
    val patPrefix = if (genFrom.pat.span.exists) {
      val patStart = Math.max(0, genFrom.pat.span.start - offsetAdjustment)
      if (cursor < patStart && patStart <= source.length) {
        val pfx = Space.format(source.substring(cursor, patStart))
        cursor = patStart
        pfx
      } else Space.EMPTY
    } else Space.EMPTY

    val varName = genFrom.pat match {
      case ident: untpd.Ident => ident.name.toString
      case _ => throw new UnsupportedOperationException("Complex for patterns not yet supported")
    }

    // Advance cursor past the pattern
    if (genFrom.pat.span.exists) {
      val patEnd = Math.max(0, genFrom.pat.span.end - offsetAdjustment)
      if (patEnd > cursor) cursor = patEnd
    }

    // Find "<-" and extract space before it
    val arrowSearchEnd = Math.min(cursor + 30, source.length)
    val arrowSearch = source.substring(cursor, arrowSearchEnd)
    val arrowIdx = arrowSearch.indexOf("<-")
    val beforeArrow = if (arrowIdx >= 0) {
      val sp = if (arrowIdx > 0) Space.format(arrowSearch.substring(0, arrowIdx)) else Space.EMPTY
      cursor = cursor + arrowIdx + 2 // past "<-"
      sp
    } else Space.EMPTY

    // Create variable declaration (with LambdaParameter marker to suppress val/var printing)
    val varDecl = new J.VariableDeclarations(
      Tree.randomId(),
      patPrefix,
      Markers.EMPTY.addIfAbsent(new LambdaParameter(Tree.randomId())),
      Collections.emptyList(),
      Collections.emptyList(),
      null, // no type
      null, // no varargs
      Collections.singletonList(
        JRightPadded.build(
          new J.VariableDeclarations.NamedVariable(
            Tree.randomId(),
            Space.EMPTY,
            Markers.EMPTY,
            new J.Identifier(Tree.randomId(), Space.EMPTY, Markers.EMPTY, Collections.emptyList(), varName, null, null),
            Collections.emptyList(),
            null,
            null
          )
        )
      )
    )

    // Visit the iterable expression
    val iterable = visitTree(genFrom.expr) match {
      case expr: Expression => expr
      case other => throw new UnsupportedOperationException("Iterable not an expression: " + other.getClass)
    }

    // Find closing paren/brace
    val closeSearchEnd = Math.min(cursor + 30, source.length)
    val closeSearch = source.substring(cursor, closeSearchEnd)
    val closeIdx = closeSearch.indexOf(closeChar)
    val iterableAfter = if (closeIdx >= 0) {
      val sp = if (closeIdx > 0) Space.format(closeSearch.substring(0, closeIdx)) else Space.EMPTY
      cursor = cursor + closeIdx + 1
      sp
    } else Space.EMPTY

    val control = new J.ForEachLoop.Control(
      Tree.randomId(),
      controlPrefix,
      Markers.EMPTY,
      JRightPadded.build(varDecl.asInstanceOf[Statement]).withAfter(beforeArrow),
      JRightPadded.build(iterable).withAfter(iterableAfter)
    )

    // For yield: consume "yield" keyword before body, capturing the space before it
    var yieldSpace = ""
    if (isYield) {
      val yieldSearchEnd = Math.min(cursor + 30, source.length)
      val yieldSearch = source.substring(cursor, yieldSearchEnd)
      val yieldIdx = yieldSearch.indexOf("yield")
      if (yieldIdx >= 0) {
        yieldSpace = yieldSearch.substring(0, yieldIdx)
        cursor = cursor + yieldIdx + "yield".length
      }
    }

    // Visit the body
    val bodyJ = visitTree(body)
    val bodyStmt: Statement = bodyJ match {
      case stmt: Statement => stmt
      case expr: Expression => asStatement(expr)
      case _ => throw new UnsupportedOperationException("Body not a statement: " + bodyJ.getClass)
    }

    updateCursor(span.end)

    new J.ForEachLoop(
      Tree.randomId(),
      prefix,
      Markers.EMPTY.addIfAbsent(ScalaForLoop.create(if (isYield) yieldSpace + "yield" else "")),
      control,
      JRightPadded.build(bodyStmt)
    )
  }
  
  private def visitBlock(block: untpd.Block): J.Block = {
    val prefix = extractPrefix(block.span)

    // Move cursor past the opening brace
    val adjustedStart = Math.max(0, block.span.start - offsetAdjustment)
    if (cursor <= adjustedStart && adjustedStart < source.length) {
      val braceIndex = source.indexOf('{', adjustedStart)
      if (braceIndex >= 0 && braceIndex < source.length) {
        cursor = braceIndex + 1
      }
    }
    val statements = new util.ArrayList[JRightPadded[Statement]]()
    // Visit all statements in the block
    for (i <- block.stats.indices) {
      val stat = block.stats(i)
      val visited = visitTree(stat)
      val stmtOpt: Statement = visited match {
        case null => null
        case stmt: Statement => stmt
        case expr: Expression => asStatement(expr)
        case _ => null
      }
      if (stmtOpt != null) {
          // Extract trailing space after this statement
          val statEnd = Math.max(0, stat.span.end - offsetAdjustment)
          val nextStart = if (i < block.stats.length - 1) {
            Math.max(0, block.stats(i + 1).span.start - offsetAdjustment)
          } else if (!block.expr.isEmpty) {
            Math.max(0, block.expr.span.start - offsetAdjustment)
          } else {
            // Last statement - look for closing brace
            Math.max(0, block.span.end - offsetAdjustment) - 1
          }

          var trailingSpace = Space.EMPTY
          if (statEnd < nextStart && cursor <= statEnd) {
            trailingSpace = Space.format(source.substring(statEnd, nextStart))
            cursor = nextStart
          }

          statements.add(JRightPadded.build(stmtOpt).withAfter(trailingSpace))
      }
    }
    
    // Handle the expression part of the block (if any)
    if (!block.expr.isEmpty) {
      visitTree(block.expr) match {
        case expr: Expression =>
          // In Scala, the last expression in a block is the return value
          val exprStmt = asStatement(expr)
          
          // Extract space before closing brace
          val exprEnd = Math.max(0, block.expr.span.end - offsetAdjustment)
          val blockEnd = Math.max(0, block.span.end - offsetAdjustment)
          var endSpace = Space.EMPTY
          if (exprEnd < blockEnd && cursor <= exprEnd) {
            val remaining = source.substring(exprEnd, blockEnd)
            val braceIndex = remaining.lastIndexOf('}')
            if (braceIndex > 0) {
              endSpace = Space.format(remaining.substring(0, braceIndex))
            }
          }
          statements.add(JRightPadded.build(exprStmt).withAfter(endSpace))
        case stmt: Statement => 
          // If it's already a statement (like a variable declaration), just add it
          // Extract space before closing brace
          val exprEnd = Math.max(0, block.expr.span.end - offsetAdjustment)
          val blockEnd = Math.max(0, block.span.end - offsetAdjustment)
          var endSpace = Space.EMPTY
          if (exprEnd < blockEnd && cursor <= exprEnd) {
            val remaining = source.substring(exprEnd, blockEnd)
            val braceIndex = remaining.lastIndexOf('}')
            if (braceIndex > 0) {
              endSpace = Space.format(remaining.substring(0, braceIndex))
            }
          }
          statements.add(JRightPadded.build(stmt).withAfter(endSpace))
        case _ => // Skip
      }
    }
    
    // Extract end padding before closing brace
    val blockEnd = Math.max(0, block.span.end - offsetAdjustment)
    var endPadding = Space.EMPTY
    if (cursor < blockEnd && statements.isEmpty()) {
      // Empty block - extract space between braces
      val remaining = source.substring(cursor, blockEnd)
      val braceIndex = remaining.lastIndexOf('}')
      if (braceIndex > 0) {
        endPadding = Space.format(remaining.substring(0, braceIndex))
      }
    }
    
    // Update cursor to end of the block
    updateCursor(block.span.end)
    
    new J.Block(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      JRightPadded.build(false), // not static
      statements,
      endPadding
    )
  }
  
  private def visitEnumCase(td: untpd.TypeDef): S.EnumCase = {
    val prefix = extractPrefix(td.span)

    // Advance cursor past "case" keyword
    val adjustedStart = Math.max(0, td.span.start - offsetAdjustment)
    val adjustedEnd = Math.max(0, td.span.end - offsetAdjustment)
    if (cursor <= adjustedStart && adjustedEnd <= source.length) {
      val src = source.substring(cursor, adjustedEnd)
      val caseIdx = src.indexOf("case")
      if (caseIdx >= 0) {
        cursor = cursor + caseIdx + "case".length
      }
    }

    // Extract the name
    val nameStart = if (td.nameSpan.exists) {
      Math.max(0, td.nameSpan.start - offsetAdjustment)
    } else {
      cursor
    }
    val nameSpace = if (cursor < nameStart && nameStart <= source.length) {
      Space.format(source.substring(cursor, nameStart))
    } else {
      Space.format(" ")
    }
    val name = new J.Identifier(
      Tree.randomId(),
      nameSpace,
      Markers.EMPTY,
      Collections.emptyList(),
      td.name.toString,
      null,
      null
    )
    if (td.nameSpan.exists) {
      cursor = Math.max(cursor, td.nameSpan.end - offsetAdjustment)
    }

    // Check for extends clause
    var extending: JLeftPadded[TypeTree] = null
    var arguments: JContainer[Expression] = null

    val template = td.rhs match {
      case tmpl: untpd.Template => tmpl
      case _ => null
    }

    if (template != null && template.parents.nonEmpty) {
      // Find "extends" keyword in source
      val searchEnd = Math.min(adjustedEnd, source.length)
      if (cursor < searchEnd) {
        val betweenText = source.substring(cursor, searchEnd)
        val extendsIdx = betweenText.indexOf("extends")
        if (extendsIdx >= 0) {
          val extendsSpace = Space.format(betweenText.substring(0, extendsIdx))
          cursor = cursor + extendsIdx + "extends".length

          // Extract the type identifier from the parent tree
          // Dotty represents `case X extends Y(args)` as Apply(Select(New(Ident(Y)), <init>), args)
          val parent = template.parents.head
          val (typeIdent: untpd.Ident, appArgs: List[untpd.Tree]) = parent match {
            case app: untpd.Apply =>
              val ident = app.fun match {
                case id: untpd.Ident => id
                case sel: untpd.Select =>
                  sel.qualifier match {
                    case nw: untpd.New => nw.tpt.asInstanceOf[untpd.Ident]
                    case id: untpd.Ident => id
                    case _ => null
                  }
                case _ => null
              }
              (ident, if (ident != null) app.args else Nil)
            case id: untpd.Ident => (id, Nil)
            case _ => (null, Nil)
          }

          if (typeIdent != null) {
            val typeId = visitIdent(typeIdent) match {
              case ident: J.Identifier => ident.asInstanceOf[TypeTree]
              case other => other.asInstanceOf[TypeTree]
            }
            extending = JLeftPadded.build(typeId).withBefore(extendsSpace)

            if (appArgs.nonEmpty) {
              val argSearchEnd = Math.min(cursor + 100, source.length)
              val argSearchText = source.substring(cursor, argSearchEnd)
              val parenIdx = argSearchText.indexOf('(')
              val beforeParen = if (parenIdx >= 0) {
                val bp = if (parenIdx > 0) Space.format(argSearchText.substring(0, parenIdx)) else Space.EMPTY
                cursor = cursor + parenIdx + 1
                bp
              } else Space.EMPTY

              val args = new util.ArrayList[JRightPadded[Expression]]()
              appArgs.zipWithIndex.foreach { case (arg, idx) =>
                visitTree(arg) match {
                  case expr: Expression =>
                    val isLast = idx == appArgs.size - 1
                    val after = if (!isLast && cursor < source.length) {
                      val text = source.substring(cursor, Math.min(cursor + 50, source.length))
                      val commaIdx = text.indexOf(',')
                      if (commaIdx >= 0) {
                        val space = Space.format(text.substring(0, commaIdx))
                        cursor = cursor + commaIdx + 1
                        space
                      } else Space.EMPTY
                    } else Space.EMPTY
                    args.add(JRightPadded.build(expr).withAfter(after))
                  case _ =>
                }
              }

              if (cursor < source.length) {
                val closeSearch = source.substring(cursor, Math.min(cursor + 50, source.length))
                val closeIdx = closeSearch.indexOf(')')
                if (closeIdx >= 0) cursor = cursor + closeIdx + 1
              }

              arguments = JContainer.build(beforeParen, args, Markers.EMPTY)
            }
          }
        }
      }
    }

    updateCursor(td.span.end)

    new S.EnumCase(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      name,
      extending,
      arguments
    )
  }

  private def visitClassDef(td: untpd.TypeDef): J = {
    // Detect enum cases (e.g., "case Mercury extends Planet(...)")
    if (td.span.exists) {
      val s = Math.max(0, td.span.start - offsetAdjustment)
      val e = Math.max(0, td.span.end - offsetAdjustment)
      if (s < e && e <= source.length) {
        val src = source.substring(s, e).trim
        if (src.startsWith("case") && !src.startsWith("case class") && !src.startsWith("case object")) {
          return visitEnumCase(td)
        }
      }
    }

    // Special handling for classes with annotations
    val hasAnnotations = td.mods.annotations.nonEmpty
    val prefix = if (hasAnnotations) {
      // Don't extract prefix yet - annotations will consume their own prefix
      Space.EMPTY
    } else {
      extractPrefix(td.span)
    }
    
    // Handle annotations first
    val leadingAnnotations = new util.ArrayList[J.Annotation]()
    for (annot <- td.mods.annotations) {
      visitTree(annot) match {
        case ann: J.Annotation => leadingAnnotations.add(ann)
        case _ => // Skip if not mapped to annotation
      }
    }
    
    // After processing annotations, we need to find where modifiers/class keyword start
    // The cursor should now be positioned after the last annotation
    
    // Extract the source text to find modifiers and class/trait keyword
    val adjustedStart = Math.max(0, td.span.start - offsetAdjustment)
    val adjustedEnd = Math.max(0, td.span.end - offsetAdjustment)
    var modifierText = ""
    var classIndex = -1
    var isTrait = false
    var sourceSnippet = ""
    
    // Use cursor position (after annotations) instead of adjustedStart
    var isEnum = false
    if (cursor >= 0 && adjustedEnd <= source.length && cursor <= adjustedEnd) {
      sourceSnippet = source.substring(cursor, adjustedEnd)
      classIndex = sourceSnippet.indexOf("class")
      if (classIndex < 0) {
        classIndex = sourceSnippet.indexOf("trait")
        if (classIndex >= 0) {
          isTrait = true
        }
      }
      if (classIndex < 0) {
        classIndex = sourceSnippet.indexOf("enum")
        if (classIndex >= 0) {
          isEnum = true
        }
      }
      if (classIndex > 0) {
        modifierText = sourceSnippet.substring(0, classIndex)
      }
    }
    
    // Extract modifiers
    val (modifiers, lastModEnd) = extractModifiersFromText(td.mods, modifierText)
    
    // Check for case modifier (special handling as it's not a traditional modifier)
    if (modifierText.contains("case")) {
      val caseIndex = modifierText.indexOf("case")
      if (caseIndex >= 0) {
        // Add case modifier in the correct position
        val caseSpace = if (caseIndex > lastModEnd) {
          Space.format(modifierText.substring(lastModEnd, caseIndex))
        } else {
          Space.EMPTY
        }
        modifiers.add(new J.Modifier(
          Tree.randomId(),
          caseSpace,
          Markers.EMPTY,
          "case",
          J.Modifier.Type.LanguageExtension,
          Collections.emptyList()
        ))
      }
    }
    
    // Find where "class", "trait", or "enum" keyword ends
    val keywordLength = if (isTrait) "trait".length else if (isEnum) "enum".length else "class".length
    val classKeywordPos = if (classIndex >= 0) {
      cursor + classIndex + keywordLength
    } else {
      cursor
    }
    
    // Extract space between "class" and the name
    val nameStart = if (td.nameSpan.exists) {
      Math.max(0, td.nameSpan.start - offsetAdjustment)
    } else {
      classKeywordPos
    }
    
    val nameSpace = if (classKeywordPos < nameStart && nameStart <= source.length) {
      Space.format(source.substring(classKeywordPos, nameStart))
    } else {
      Space.format(" ") // Default to single space
    }
    
    // Extract class kind with proper prefix space
    val kindPrefix = if (hasAnnotations && classIndex >= 0) {
      // When we have annotations, the space between the last annotation and "class" goes here
      Space.format(sourceSnippet.substring(0, classIndex))
    } else if (!modifiers.isEmpty && classIndex > 0) {
      val afterModifiers = if (modifierText.contains("case")) {
        val caseIndex = modifierText.indexOf("case")
        if (caseIndex >= 0) {
          caseIndex + "case".length
        } else {
          lastModEnd
        }
      } else {
        lastModEnd
      }
      if (afterModifiers < classIndex) {
        Space.format(modifierText.substring(afterModifiers, classIndex))
      } else {
        Space.EMPTY
      }
    } else {
      Space.EMPTY
    }
    
    val kindType = if (isTrait) {
      J.ClassDeclaration.Kind.Type.Interface
    } else if (isEnum) {
      J.ClassDeclaration.Kind.Type.Enum
    } else {
      J.ClassDeclaration.Kind.Type.Class
    }
    
    val kind = new J.ClassDeclaration.Kind(
      Tree.randomId(),
      kindPrefix,
      Markers.EMPTY,
      Collections.emptyList(),
      kindType
    )
    
    // Update cursor to after "class" keyword
    cursor = classKeywordPos
    
    // Extract class name
    val name = new J.Identifier(
      Tree.randomId(),
      nameSpace,
      Markers.EMPTY,
      Collections.emptyList(),
      td.name.toString,
      null,
      null
    )
    
    // Update cursor to after name
    if (td.nameSpan.exists) {
      val nameEnd = Math.max(0, td.nameSpan.end - offsetAdjustment)
      if (nameEnd > cursor && nameEnd <= source.length) {
        cursor = nameEnd
      }
    }
    
    // Extract template early to access type parameters
    val template = td.rhs match {
      case tmpl: untpd.Template => tmpl
      case _ => null
    }
    
    // Extract type parameters from the template  
    val typeParameters: JContainer[J.TypeParameter] = if (template != null && template.constr.paramss.nonEmpty) {
      // Check if the first param list contains type parameters (TypeDef nodes)
      val firstParamList = template.constr.paramss.head
      val typeParams = firstParamList.collect { case tparam: untpd.TypeDef => tparam }
      
      if (typeParams.nonEmpty) {
        // Look for opening bracket in source
        var bracketStart = cursor
        if (cursor < source.length) {
          val searchEnd = Math.min(cursor + 100, source.length)
          val searchText = source.substring(cursor, searchEnd)
          val bracketIdx = searchText.indexOf('[')
          if (bracketIdx >= 0) {
            bracketStart = cursor + bracketIdx
          }
        }
        
        val openingBracketSpace = if (bracketStart > cursor) {
          Space.format(source.substring(cursor, bracketStart))
        } else {
          Space.EMPTY
        }
        
        // Update cursor to after opening bracket
        cursor = bracketStart + 1
        
        // Convert TypeDef nodes to J.TypeParameter
        val jTypeParams = new util.ArrayList[JRightPadded[J.TypeParameter]]()
        typeParams.zipWithIndex.foreach { case (tparam, idx) =>
          val jTypeParam = visitTypeParameter(tparam)
          val isLast = idx == typeParams.size - 1
          
          // Determine trailing space/comma
          val trailingSpace = if (!isLast) {
            // Look for comma in source between this param and next
            if (idx + 1 < typeParams.size && tparam.span.exists && typeParams(idx + 1).span.exists) {
              val thisEnd = tparam.span.end - offsetAdjustment
              val nextStart = typeParams(idx + 1).span.start - offsetAdjustment
              if (thisEnd < nextStart && nextStart <= source.length) {
                val between = source.substring(thisEnd, nextStart)
                val commaIdx = between.indexOf(',')
                if (commaIdx >= 0) {
                  Space.format(between.substring(commaIdx + 1))
                } else {
                  Space.EMPTY
                }
              } else {
                Space.EMPTY
              }
            } else {
              Space.EMPTY
            }
          } else {
            Space.EMPTY
          }
          
          if (!isLast && trailingSpace != Space.EMPTY) {
            jTypeParams.add(new JRightPadded(jTypeParam, trailingSpace, Markers.EMPTY))
          } else {
            jTypeParams.add(JRightPadded.build(jTypeParam))
          }
        }
        
        // Update cursor to after closing bracket
        if (typeParams.nonEmpty && typeParams.last.span.exists) {
          val lastParamEnd = typeParams.last.span.end - offsetAdjustment
          if (lastParamEnd < source.length) {
            val searchEnd = Math.min(lastParamEnd + 10, source.length)
            val afterParams = source.substring(lastParamEnd, searchEnd)
            val closeBracketIdx = afterParams.indexOf(']')
            if (closeBracketIdx >= 0) {
              cursor = lastParamEnd + closeBracketIdx + 1
            }
          }
        }
        
        JContainer.build(openingBracketSpace, jTypeParams, Markers.EMPTY)
      } else {
        null
      }
    } else {
      null
    }
    
    // Handle constructor parameters - extract only value parameters
    val constructorParamsSource = if (template != null && template.constr.paramss.size > 1) {
      // If we have type parameters, constructor params are in the second list
      extractConstructorParametersSource(td)
    } else if (template != null && template.constr.paramss.nonEmpty) {
      // Check if the first list has only value parameters
      val firstList = template.constr.paramss.head
      if (firstList.forall(_.isInstanceOf[untpd.ValDef])) {
        extractConstructorParametersSource(td)
      } else {
        ""
      }
    } else {
      ""
    }
    
    val primaryConstructor = if (constructorParamsSource.nonEmpty) {
      // Create Unknown node to preserve constructor parameters
      val unknown = new J.Unknown(
        Tree.randomId(),
        Space.EMPTY,
        Markers.EMPTY,
        new J.Unknown.Source(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          constructorParamsSource
        )
      )
      // Wrap in a container
      JContainer.build(
        Space.EMPTY,
        Collections.singletonList(JRightPadded.build(unknown.asInstanceOf[Statement])),
        Markers.EMPTY
      )
    } else {
      null
    }
    
    // Extract extends/implements from Template
    var extendings: JLeftPadded[TypeTree] = null
    var implementings: JContainer[TypeTree] = null
    
    if (template != null && template.parents.nonEmpty) {
        // In Scala, the first parent after the primary constructor is the extends clause
        // Additional parents are the with clauses (implements in Java)
        
        // First, we need to find where "extends" keyword starts in the source
        val extendsKeywordPos = if (td.nameSpan.exists && constructorParamsSource.nonEmpty) {
          // After constructor parameters
          cursor
        } else if (td.nameSpan.exists) {
          // After class name (no constructor params)
          Math.max(0, td.nameSpan.end - offsetAdjustment)
        } else {
          cursor
        }
        
        // Look for "extends" keyword in source
        var extendsSpace = Space.EMPTY
        if (extendsKeywordPos < source.length && template.parents.head.span.exists) {
          val firstParentStart = Math.max(0, template.parents.head.span.start - offsetAdjustment)
          if (extendsKeywordPos < firstParentStart && firstParentStart <= source.length) {
            val betweenText = source.substring(extendsKeywordPos, firstParentStart)
            val extendsIndex = betweenText.indexOf("extends")
            if (extendsIndex >= 0) {
              extendsSpace = Space.format(betweenText.substring(0, extendsIndex))
              // Update cursor to after "extends" keyword
              cursor = extendsKeywordPos + extendsIndex + "extends".length
            }
          }
        }
        
        // First parent is the extends clause
        val firstParent = template.parents.head
        val extendsTypeExpr = visitTree(firstParent) match {
          case id: J.Identifier =>
            // Simple type like "Animal" - already has the right prefix from visiting
            id
          case fieldAccess: J.FieldAccess =>
            // Qualified type like "com.example.Animal"
            fieldAccess
          case unknown: J.Unknown =>
            // Complex type we can't handle yet (like generics)
            unknown
          case _ =>
            // Fallback to Unknown
            visitUnknown(firstParent)
        }
        
        // Convert to TypeTree
        val extendsType: TypeTree = extendsTypeExpr match {
          case id: J.Identifier =>
            // The identifier already has the correct prefix from visitIdent, just use it as is
            id
          case fieldAccess: J.FieldAccess =>
            // The field access already has the correct prefix from visitSelect
            fieldAccess
          case unknown: J.Unknown =>
            // The unknown already has the correct prefix
            unknown
          case other =>
            // This shouldn't happen but let's be safe
            val typeSpace = if (cursor < firstParent.span.start - offsetAdjustment) {
              Space.format(source.substring(cursor, firstParent.span.start - offsetAdjustment))
            } else {
              Space.format(" ")
            }
            new J.Unknown(
              Tree.randomId(),
              typeSpace,
              Markers.EMPTY,
              new J.Unknown.Source(
                Tree.randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                other.toString
              )
            )
        }
        
        extendings = new JLeftPadded(extendsSpace, extendsType, Markers.EMPTY)
        
        // Update cursor to after first parent
        if (firstParent.span.exists) {
          cursor = Math.max(cursor, firstParent.span.end - offsetAdjustment)
        }
        
        // Handle additional parents as implements (with clauses)
        if (template.parents.size > 1) {
          val implementsList = new util.ArrayList[JRightPadded[TypeTree]]()
          
          // Extract space before the first "with" or "extends" (if no extends clause)
          var containerSpace = Space.EMPTY
          if (extendings == null && template.parents.nonEmpty) {
            // No extends clause, so first trait uses "extends"
            val firstParent = template.parents.head
            if (firstParent.span.exists) {
              containerSpace = sourceBefore("extends")
            }
          } else if (extendings != null && template.parents.size > 1) {
            // We have extends, so look for first "with"
            containerSpace = sourceBefore("with")
          }
          
          for (i <- 1 until template.parents.size) {
            val parent = template.parents(i)
            
            val implTypeExpr = visitTree(parent) match {
              case id: J.Identifier =>
                id
              case fieldAccess: J.FieldAccess =>
                fieldAccess
              case unknown: J.Unknown =>
                unknown
              case _ =>
                visitUnknown(parent)
            }
            
            // Convert to TypeTree - the expression already has its prefix from visiting
            val implType: TypeTree = implTypeExpr match {
              case id: J.Identifier =>
                id
              case fieldAccess: J.FieldAccess =>
                fieldAccess
              case unknown: J.Unknown =>
                unknown
              case other =>
                new J.Unknown(
                  Tree.randomId(),
                  Space.EMPTY,
                  Markers.EMPTY,
                  new J.Unknown.Source(
                    Tree.randomId(),
                    Space.EMPTY,
                    Markers.EMPTY,
                    other.toString
                  )
                )
            }
            
            // Build the right-padded element
            val rightPadded = if (i < template.parents.size - 1) {
              // Not the last element, look for space before next "with"
              val afterSpace = sourceBefore("with")
              new JRightPadded(implType, afterSpace, Markers.EMPTY)
            } else {
              // Last element, no trailing space needed
              JRightPadded.build(implType)
            }
            
            implementsList.add(rightPadded)
          }
          
          if (!implementsList.isEmpty) {
            implementings = JContainer.build(
              containerSpace,
              implementsList,
              Markers.EMPTY
            )
          }
        }
    }
    
    // Handle the body - TypeDef has rhs which should be a Template for classes
    // For classes without explicit body, we should NOT print empty braces
    val hasExplicitBody = td.rhs match {
      case tmpl: untpd.Template =>
        // A class has an explicit body if:
        // 1. The template has any body statements, OR
        // 2. There's a "{" in the source (even for empty bodies)
        if (tmpl.body.nonEmpty) {
          // If there are body statements, we definitely have a body
          true
        } else if (td.span.exists) {
          // For empty bodies, check if there's a "{" in the entire class span
          val classStart = Math.max(0, td.span.start - offsetAdjustment)
          val classEnd = Math.max(0, td.span.end - offsetAdjustment)
          if (classStart < classEnd && classEnd <= source.length) {
            val classSource = source.substring(classStart, classEnd)
            classSource.contains("{")
          } else {
            false
          }
        } else {
          false
        }
      case _ => false
    }
    
    val body = if (hasExplicitBody) {
      td.rhs match {
        case template: untpd.Template =>
          // Extract space before the opening brace
          val bodyPrefix = if (td.span.exists) {
            val classEnd = Math.max(0, td.span.end - offsetAdjustment)
            if (cursor < classEnd && classEnd <= source.length) {
              val afterCursor = source.substring(cursor, classEnd)
              val braceIndex = afterCursor.indexOf("{")
              if (braceIndex >= 0) {
                val prefix = Space.format(afterCursor.substring(0, braceIndex))
                // Update cursor to after the opening brace
                cursor = cursor + braceIndex + 1
                prefix
              } else {
                // The brace might already be consumed, look for it from class start
                val classStart = Math.max(0, td.span.start - offsetAdjustment)
                val classSource = source.substring(classStart, classEnd)
                val nameEnd = classSource.indexOf(td.name.toString) + td.name.toString.length
                val afterName = classSource.substring(nameEnd)
                val braceInAfterName = afterName.indexOf("{")
                if (braceInAfterName >= 0) {
                  // Found the brace, update cursor to after it
                  val bracePos = classStart + nameEnd + braceInAfterName + 1
                  if (bracePos > cursor) {
                    val prefix = Space.format(source.substring(cursor, bracePos - 1))
                    cursor = bracePos
                    prefix
                  } else {
                    // Brace is before cursor, just use single space
                    Space.format(" ")
                  }
                } else {
                  Space.format(" ")
                }
              }
            } else {
              Space.EMPTY
            }
          } else {
            Space.EMPTY
          }
          
          // Visit the template body to get statements
          val statements = new util.ArrayList[JRightPadded[Statement]]()
          
          // Visit each statement in the template body
          for (stat <- template.body) {
            // Check if this is a method declaration (DefDef) - these should always be included
            stat match {
              case _: untpd.DefDef =>
                // Always include method declarations, even if marked synthetic
                visitTree(stat) match {
                  case null => // Skip null statements
                  case stmt: Statement => 
                    statements.add(JRightPadded.build(stmt))
                  case _ => // Skip non-statement nodes
                }
              case _ =>
                // For non-methods, skip synthetic nodes (like the ??? in abstract classes)
                if (!stat.span.isSynthetic) {
                  visitTree(stat) match {
                    case null => // Skip null statements
                    case stmt: Statement => 
                      statements.add(JRightPadded.build(stmt))
                    case _ => // Skip non-statement nodes
                  }
                }
            }
          }
          
          // Extract the space before the closing brace
          val endSpace = if (td.span.exists) {
            val classEnd = Math.max(0, td.span.end - offsetAdjustment)
            if (cursor < classEnd && classEnd <= source.length) {
              val remaining = source.substring(cursor, classEnd)
              val closeBraceIndex = remaining.lastIndexOf("}")
              if (closeBraceIndex >= 0) {
                cursor = classEnd // Move cursor to end
                Space.format(remaining.substring(0, closeBraceIndex))
              } else {
                Space.EMPTY
              }
            } else {
              Space.EMPTY
            }
          } else {
            Space.EMPTY
          }
          
          new J.Block(
            Tree.randomId(),
            bodyPrefix,
            Markers.EMPTY,
            JRightPadded.build(false),
            statements,
            endSpace
          )
        case _ =>
          // Fallback - shouldn't happen if hasExplicitBody is true
          null
      }
    } else {
      // For classes without body (like "class Empty"), return null
      null
    }
    
    // Update cursor to end of the class
    if (td.span.exists) {
      val adjustedEnd = Math.max(0, td.span.end - offsetAdjustment)
      if (adjustedEnd > cursor && adjustedEnd <= source.length) {
        cursor = adjustedEnd
      }
    }
    
    new J.ClassDeclaration(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      leadingAnnotations, // annotations
      modifiers,
      kind,
      name,
      typeParameters,
      primaryConstructor,
      extendings,
      implementings,
      null, // permits
      body,
      null  // type
    )
  }
  
  private def visitReturn(ret: untpd.Return): J.Return = {
    val prefix = extractPrefix(ret.span)

    // Advance cursor past "return" keyword
    cursor = cursor + "return".length

    // Extract the expression being returned (if any)
    val expr = if (ret.expr.isEmpty) {
      null // void return
    } else {
      visitTree(ret.expr) match {
        case expression: Expression => expression
        case _ => return visitUnknown(ret).asInstanceOf[J.Return]
      }
    }
    
    // Update cursor to the end of the return statement
    updateCursor(ret.span.end)
    
    new J.Return(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      expr
    )
  }

  private def visitThrow(thr: untpd.Throw): J.Throw = {
    val prefix = extractPrefix(thr.span)

    // Advance cursor past "throw" keyword
    cursor = cursor + "throw".length

    // Visit the exception expression
    val exception = visitTree(thr.expr) match {
      case expr: Expression => expr
      case _ => return visitUnknown(thr).asInstanceOf[J.Throw]
    }
    
    // Update cursor to the end of the throw statement
    updateCursor(thr.span.end)
    
    new J.Throw(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      exception
    )
  }
  
  private def visitTry(tryTree: untpd.Try): J = {
    val savedCursor = cursor
    try {
      visitTryImpl(tryTree)
    } catch {
      case _: Exception =>
        cursor = savedCursor
        visitUnknown(tryTree)
    }
  }

  private def visitTryImpl(tryTree: untpd.Try): J.Try = {
    import org.openrewrite.scala.marker.ScalaCatch

    val prefix = extractPrefix(tryTree.span)

    // Advance cursor past "try" keyword
    cursor = cursor + "try".length

    // Visit the try body - should produce a J.Block
    val tryBody = visitTree(tryTree.expr) match {
      case block: J.Block => block
      case stmt: Statement =>
        val stmts = new util.ArrayList[JRightPadded[Statement]]()
        stmts.add(JRightPadded.build(stmt))
        new J.Block(
          Tree.randomId(),
          stmt.getPrefix,
          Markers.EMPTY.addIfAbsent(new OmitBraces(Tree.randomId())),
          JRightPadded.build(false),
          stmts,
          Space.EMPTY
        )
      case _ => return visitUnknown(tryTree).asInstanceOf[J.Try]
    }

    // Handle catch clauses
    val catches = new util.ArrayList[J.Try.Catch]()
    if (tryTree.cases.nonEmpty) {
      // Find "catch" keyword in source
      val searchEnd = Math.min(cursor + 100, source.length)
      val searchText = source.substring(cursor, searchEnd)
      val catchIdx = searchText.indexOf("catch")
      if (catchIdx >= 0) {
        val catchSpace = Space.format(searchText.substring(0, catchIdx))
        cursor = cursor + catchIdx + "catch".length

        // Find opening brace of catch block
        val braceSearchEnd = Math.min(cursor + 20, source.length)
        val braceText = source.substring(cursor, braceSearchEnd)
        val braceIdx = braceText.indexOf('{')
        val catchBlockPrefix = if (braceIdx >= 0) {
          val pfx = Space.format(braceText.substring(0, braceIdx))
          cursor = cursor + braceIdx + 1
          pfx
        } else Space.EMPTY

        // Visit each CaseDef as an S.CaseClause
        val caseStmts = new util.ArrayList[JRightPadded[Statement]]()
        var i = 0
        while (i < tryTree.cases.size) {
          val caseDef = tryTree.cases(i)
          val caseClause = visitCaseDef(caseDef, i < tryTree.cases.size - 1,
            if (i + 1 < tryTree.cases.size) tryTree.cases(i + 1) else null)
          caseStmts.add(JRightPadded.build(caseClause.asInstanceOf[Statement]))
          i += 1
        }

        // Find closing brace and end space
        val lastCaseEnd = if (tryTree.cases.nonEmpty && tryTree.cases.last.span.exists) {
          Math.max(0, tryTree.cases.last.span.end - offsetAdjustment)
        } else cursor
        if (lastCaseEnd > cursor) cursor = lastCaseEnd
        var catchEndSpace = Space.EMPTY
        if (cursor < source.length) {
          val remaining = source.substring(cursor, Math.min(cursor + 200, source.length))
          val closeBrace = remaining.indexOf('}')
          if (closeBrace >= 0) {
            catchEndSpace = Space.format(remaining.substring(0, closeBrace))
            cursor = cursor + closeBrace + 1
          }
        }

        val catchBody = new J.Block(
          Tree.randomId(),
          catchBlockPrefix,
          Markers.EMPTY,
          JRightPadded.build(false),
          caseStmts,
          catchEndSpace
        )

        // Create a dummy parameter (Scala catch doesn't use Java-style parameters)
        val dummyParam = new J.VariableDeclarations(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          Collections.emptyList(),
          Collections.emptyList(),
          null,
          null,
          Collections.singletonList(JRightPadded.build(
            new J.VariableDeclarations.NamedVariable(
              Tree.randomId(), Space.EMPTY, Markers.EMPTY,
              new J.Identifier(Tree.randomId(), Space.EMPTY, Markers.EMPTY, Collections.emptyList(), "", null, null),
              Collections.emptyList(), null, null
            )
          ))
        )

        val catchNode = new J.Try.Catch(
          Tree.randomId(),
          catchSpace,
          Markers.EMPTY.addIfAbsent(new ScalaCatch(Tree.randomId())),
          new J.ControlParentheses(Tree.randomId(), Space.EMPTY, Markers.EMPTY, JRightPadded.build(dummyParam)),
          catchBody
        )
        catches.add(catchNode)
      }
    }

    // Handle finally
    var finallyBlock: JLeftPadded[J.Block] = null
    if (!tryTree.finalizer.isEmpty) {
      val searchEnd2 = Math.min(cursor + 100, source.length)
      val searchText2 = source.substring(cursor, searchEnd2)
      val finallyIdx = searchText2.indexOf("finally")
      if (finallyIdx >= 0) {
        val finallySpace = Space.format(searchText2.substring(0, finallyIdx))
        cursor = cursor + finallyIdx + "finally".length

        val finallyBody = visitTree(tryTree.finalizer) match {
          case block: J.Block => block
          case stmt: Statement =>
            val stmts = new util.ArrayList[JRightPadded[Statement]]()
            stmts.add(JRightPadded.build(stmt))
            new J.Block(
              Tree.randomId(),
              stmt.getPrefix,
              Markers.EMPTY.addIfAbsent(new OmitBraces(Tree.randomId())),
              JRightPadded.build(false),
              stmts,
              Space.EMPTY
            )
          case _ => null
        }
        if (finallyBody != null) {
          finallyBlock = JLeftPadded.build(finallyBody).withBefore(finallySpace)
        }
      }
    }

    updateCursor(tryTree.span.end)

    new J.Try(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      null, // no resources
      tryBody,
      catches,
      finallyBlock
    )
  }

  private def visitParsedTry(parsedTry: untpd.ParsedTry): J = {
    import org.openrewrite.scala.marker.ScalaCatch

    val savedCursor = cursor
    try {
      val prefix = extractPrefix(parsedTry.span)

      // Advance cursor past "try" keyword
      cursor = cursor + "try".length

      // Visit the try body
      val tryBody = visitTree(parsedTry.expr) match {
        case block: J.Block => block
        case stmt: Statement =>
          val stmts = new util.ArrayList[JRightPadded[Statement]]()
          stmts.add(JRightPadded.build(stmt))
          new J.Block(
            Tree.randomId(),
            stmt.getPrefix,
            Markers.EMPTY.addIfAbsent(new OmitBraces(Tree.randomId())),
            JRightPadded.build(false),
            stmts,
            Space.EMPTY
          )
        case _ =>
          cursor = savedCursor
          return visitUnknown(parsedTry)
      }

      // Handle catch handler - extract cases from Match handler
      val catches = new util.ArrayList[J.Try.Catch]()
      if (!parsedTry.handler.isEmpty) {
        // Extract cases from the handler Match tree
        val cases: scala.collection.immutable.List[untpd.CaseDef] = parsedTry.handler match {
          case m: untpd.Match => m.cases
          case _ => scala.collection.immutable.Nil
        }

        if (cases.nonEmpty) {
          // Find "catch" keyword in source
          val searchEnd = Math.min(cursor + 100, source.length)
          val searchText = source.substring(cursor, searchEnd)
          val catchIdx = searchText.indexOf("catch")
          if (catchIdx >= 0) {
            val catchSpace = Space.format(searchText.substring(0, catchIdx))
            cursor = cursor + catchIdx + "catch".length

            // Find opening brace of catch block
            val braceSearchEnd = Math.min(cursor + 20, source.length)
            val braceText = source.substring(cursor, braceSearchEnd)
            val braceIdx = braceText.indexOf('{')
            val catchBlockPrefix = if (braceIdx >= 0) {
              val pfx = Space.format(braceText.substring(0, braceIdx))
              cursor = cursor + braceIdx + 1
              pfx
            } else Space.EMPTY

            // Visit each CaseDef as an S.CaseClause
            val caseStmts = new util.ArrayList[JRightPadded[Statement]]()
            var i = 0
            while (i < cases.size) {
              val caseDef = cases(i)
              val caseClause = visitCaseDef(caseDef, i < cases.size - 1,
                if (i + 1 < cases.size) cases(i + 1) else null)
              caseStmts.add(JRightPadded.build(caseClause.asInstanceOf[Statement]))
              i += 1
            }

            // Find closing brace and end space
            val lastCaseEnd = if (cases.nonEmpty && cases.last.span.exists) {
              Math.max(0, cases.last.span.end - offsetAdjustment)
            } else cursor
            if (lastCaseEnd > cursor) cursor = lastCaseEnd
            var catchEndSpace = Space.EMPTY
            if (cursor < source.length) {
              val remaining = source.substring(cursor, Math.min(cursor + 200, source.length))
              val closeBrace = remaining.indexOf('}')
              if (closeBrace >= 0) {
                catchEndSpace = Space.format(remaining.substring(0, closeBrace))
                cursor = cursor + closeBrace + 1
              }
            }

            val catchBody = new J.Block(
              Tree.randomId(),
              catchBlockPrefix,
              Markers.EMPTY,
              JRightPadded.build(false),
              caseStmts,
              catchEndSpace
            )

            // Create a dummy parameter (Scala catch doesn't use Java-style parameters)
            val dummyParam = new J.VariableDeclarations(
              Tree.randomId(), Space.EMPTY, Markers.EMPTY,
              Collections.emptyList(), Collections.emptyList(), null, null,
              Collections.singletonList(JRightPadded.build(
                new J.VariableDeclarations.NamedVariable(
                  Tree.randomId(), Space.EMPTY, Markers.EMPTY,
                  new J.Identifier(Tree.randomId(), Space.EMPTY, Markers.EMPTY, Collections.emptyList(), "", null, null),
                  Collections.emptyList(), null, null
                )
              ))
            )

            catches.add(new J.Try.Catch(
              Tree.randomId(),
              catchSpace,
              Markers.EMPTY.addIfAbsent(new ScalaCatch(Tree.randomId())),
              new J.ControlParentheses(Tree.randomId(), Space.EMPTY, Markers.EMPTY, JRightPadded.build(dummyParam)),
              catchBody
            ))
          }
        } else {
          // Handler is not a Match - fall back to visiting it as Unknown
          val searchEnd = Math.min(cursor + 100, source.length)
          val searchText = source.substring(cursor, searchEnd)
          val catchIdx = searchText.indexOf("catch")
          if (catchIdx >= 0) {
            val catchSpace = Space.format(searchText.substring(0, catchIdx))
            cursor = cursor + catchIdx + "catch".length

            val handlerUnknown = visitUnknown(parsedTry.handler)
            val caseStmts = new util.ArrayList[JRightPadded[Statement]]()
            caseStmts.add(JRightPadded.build(handlerUnknown.asInstanceOf[Statement]))

            val catchBody = new J.Block(
              Tree.randomId(),
              handlerUnknown.getPrefix,
              Markers.EMPTY.addIfAbsent(new OmitBraces(Tree.randomId())),
              JRightPadded.build(false),
              caseStmts,
              Space.EMPTY
            )

            val dummyParam = new J.VariableDeclarations(
              Tree.randomId(), Space.EMPTY, Markers.EMPTY,
              Collections.emptyList(), Collections.emptyList(), null, null,
              Collections.singletonList(JRightPadded.build(
                new J.VariableDeclarations.NamedVariable(
                  Tree.randomId(), Space.EMPTY, Markers.EMPTY,
                  new J.Identifier(Tree.randomId(), Space.EMPTY, Markers.EMPTY, Collections.emptyList(), "", null, null),
                  Collections.emptyList(), null, null
                )
              ))
            )

            catches.add(new J.Try.Catch(
              Tree.randomId(),
              catchSpace,
              Markers.EMPTY.addIfAbsent(new ScalaCatch(Tree.randomId())),
              new J.ControlParentheses(Tree.randomId(), Space.EMPTY, Markers.EMPTY, JRightPadded.build(dummyParam)),
              catchBody
            ))
          }
        }
      }

      // Handle finally
      var finallyBlock: JLeftPadded[J.Block] = null
      if (!parsedTry.finalizer.isEmpty) {
        val searchEnd2 = Math.min(cursor + 100, source.length)
        val searchText2 = source.substring(cursor, searchEnd2)
        val finallyIdx = searchText2.indexOf("finally")
        if (finallyIdx >= 0) {
          val finallySpace = Space.format(searchText2.substring(0, finallyIdx))
          cursor = cursor + finallyIdx + "finally".length

          val finallyBody = visitTree(parsedTry.finalizer) match {
            case block: J.Block => block
            case stmt: Statement =>
              val stmts = new util.ArrayList[JRightPadded[Statement]]()
              stmts.add(JRightPadded.build(stmt))
              new J.Block(
                Tree.randomId(),
                stmt.getPrefix,
                Markers.EMPTY.addIfAbsent(new OmitBraces(Tree.randomId())),
                JRightPadded.build(false),
                stmts,
                Space.EMPTY
              )
            case _ => null
          }
          if (finallyBody != null) {
            finallyBlock = JLeftPadded.build(finallyBody).withBefore(finallySpace)
          }
        }
      }

      updateCursor(parsedTry.span.end)

      new J.Try(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        null,
        tryBody,
        catches,
        finallyBlock
      )
    } catch {
      case _: Exception =>
        cursor = savedCursor
        visitUnknown(parsedTry)
    }
  }

  private def visitTypeApply(ta: untpd.TypeApply): J = {
    // TypeApply represents a type application like List.empty[Int] or obj.asInstanceOf[Type]
    ta.fun match {
      case sel: untpd.Select =>
        // Check if this is asInstanceOf
        if (sel.name.toString == "asInstanceOf" && ta.args.size == 1) {
          // This is a type cast operation: obj.asInstanceOf[Type]
          
          // Visit the expression being cast (with its own prefix)
          // The expression (sel.qualifier) is the object before .asInstanceOf
          val expr = visitTree(sel.qualifier) match {
            case e: Expression => e
            case _ => return visitUnknown(ta)
          }
          
          // Update cursor past ".asInstanceOf"
          val asInstanceOfEnd = sel.span.end
          if (asInstanceOfEnd > cursor) {
            cursor = asInstanceOfEnd
          }
          
          // Now handle the type argument in brackets
          // Extract any space before the opening bracket
          val typeArgStart = ta.args.head.span.start - offsetAdjustment
          val spaceBeforeBracket = if (cursor < typeArgStart && typeArgStart <= source.length) {
            val between = source.substring(cursor, typeArgStart)
            // Find the bracket position
            val bracketPos = between.indexOf('[')
            if (bracketPos >= 0) {
              cursor = cursor + bracketPos + 1  // Move past the bracket
              Space.format(between.substring(0, bracketPos))
            } else {
              Space.EMPTY
            }
          } else {
            Space.EMPTY
          }
          
          // Visit the target type
          val targetType = visitTree(ta.args.head) match {
            case tt: TypeTree => tt
            case _ => return visitUnknown(ta)
          }
          
          // Update cursor past the closing bracket
          if (ta.span.end > cursor) {
            cursor = ta.span.end
          }
          
          return new J.TypeCast(
            Tree.randomId(),
            Space.EMPTY,  // TypeCast itself has no prefix - the space is handled by the variable initializer
            Markers.EMPTY,
            new J.ControlParentheses[TypeTree](
              Tree.randomId(),
              spaceBeforeBracket,
              Markers.EMPTY,
              JRightPadded.build(targetType)
            ),
            expr
          )
        }
        
        // Check if this is isInstanceOf
        if (sel.name.toString == "isInstanceOf" && ta.args.size == 1) {
          // This is a type check operation: obj.isInstanceOf[Type]
          
          // Extract prefix
          val startPos = Math.max(0, ta.span.start - offsetAdjustment)
          val prefix = if (startPos > cursor && startPos <= source.length) {
            Space.format(source.substring(cursor, startPos))
          } else {
            Space.EMPTY
          }
          
          // Update cursor to start of the expression (sel.qualifier)
          cursor = Math.max(0, sel.qualifier.span.start - offsetAdjustment)
          
          // Visit the expression being checked
          val expr = visitTree(sel.qualifier) match {
            case e: Expression => e
            case _ => return visitUnknown(ta)
          }
          
          // Update cursor to start of type argument
          cursor = Math.max(0, ta.args.head.span.start - offsetAdjustment)
          
          // Visit the target type
          val clazz = visitTree(ta.args.head) match {
            case tt: TypeTree => tt
            case _ => return visitUnknown(ta)
          }
          
          // Update cursor to the end of the TypeApply
          updateCursor(ta.span.end)
          
          return new J.InstanceOf(
            Tree.randomId(),
            prefix,
            Markers.EMPTY,
            JRightPadded.build(expr),
            clazz,
            null, // pattern (not used in Scala)
            null  // type
          )
        }
        
      case _ =>
        // Other TypeApply cases
    }
    
    // For other TypeApply cases, preserve as Unknown
    visitUnknown(ta)
  }
  
  private def visitAppliedTypeTree(at: untpd.AppliedTypeTree): J = {
    // AppliedTypeTree represents a parameterized type like List[String]
    val prefix = extractPrefix(at.span)
    
    // Save original cursor position
    val originalCursor = cursor
    
    // Visit the base type (e.g., List, Map, Option)
    val clazz = visitTree(at.tpt) match {
      case nt: NameTree => nt
      case _ => return visitUnknown(at)
    }
    
    // Extract the source to find bracket positions
    val source = extractSource(at.span)
    val openBracketIdx = source.indexOf('[')
    val closeBracketIdx = source.lastIndexOf(']')
    
    if (openBracketIdx < 0 || closeBracketIdx < 0) {
      return visitUnknown(at)
    }
    
    // Extract space before opening bracket
    val baseTypeEnd = clazz match {
      case id: J.Identifier => id.getSimpleName.length
      case fa: J.FieldAccess => source.indexOf('[')
      case _ => source.indexOf('[')
    }
    
    val beforeOpenBracket = if (baseTypeEnd < openBracketIdx) {
      Space.format(source.substring(baseTypeEnd, openBracketIdx))
    } else {
      Space.EMPTY
    }
    
    // Process type arguments
    val typeArgs = new util.ArrayList[JRightPadded[Expression]]()
    
    if (at.args.nonEmpty) {
      // Update cursor to the start of the first argument
      val firstArgStart = Math.max(0, at.args.head.span.start - offsetAdjustment)
      cursor = firstArgStart
      
      for (i <- at.args.indices) {
        val arg = at.args(i)
        val argTree = visitTree(arg) match {
          case expr: Expression => expr
          case _ => return visitUnknown(at)
        }
        
        // Extract trailing comma/space
        val isLast = i == at.args.size - 1
        val afterSpace = if (isLast) {
          // Space before closing bracket
          val argEnd = Math.max(0, arg.span.end - offsetAdjustment)
          if (argEnd < closeBracketIdx + originalCursor) {
            val spaceStr = this.source.substring(argEnd, closeBracketIdx + originalCursor)
            Space.format(spaceStr)
          } else {
            Space.EMPTY
          }
        } else {
          // Look for comma and space after it
          val argEnd = Math.max(0, arg.span.end - offsetAdjustment)
          val nextArgStart = if (i + 1 < at.args.size) {
            Math.max(0, at.args(i + 1).span.start - offsetAdjustment)
          } else {
            closeBracketIdx + originalCursor
          }
          
          if (argEnd < nextArgStart && argEnd < this.source.length && nextArgStart <= this.source.length) {
            val between = this.source.substring(argEnd, nextArgStart)
            val commaIdx = between.indexOf(',')
            if (commaIdx >= 0) {
              // The "after" space should be everything from the end of the argument up to (but not including) the comma
              // The visitContainer will add the comma and then the prefix of the next element will have the space after the comma
              cursor = argEnd + commaIdx + 1  // Move cursor past the comma
              Space.format(between.substring(0, commaIdx))
            } else {
              Space.EMPTY
            }
          } else {
            Space.EMPTY
          }
        }
        
        typeArgs.add(JRightPadded.build(argTree).withAfter(afterSpace))
      }
    }
    
    // Update cursor to the end of the AppliedTypeTree
    updateCursor(at.span.end)
    
    // Create the type parameters container
    val typeParameters = JContainer.build(
      beforeOpenBracket,
      typeArgs,
      Markers.EMPTY
    )
    
    new J.ParameterizedType(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      clazz,
      typeParameters,
      null // type
    )
  }

  private def visitDefDef(dd: untpd.DefDef): J = {
    // Skip synthetic constructors and compiler-generated lambda DefDefs
    val defName = dd.name.toString
    if (defName == "<init>" || defName.startsWith("$anonfun")) {
      return visitUnknown(dd)
    }

    // Save cursor position so we can fall back to J.Unknown on failure
    val savedCursor = cursor
    try {
      visitDefDefImpl(dd)
    } catch {
      case _: Exception =>
        cursor = savedCursor
        visitUnknown(dd)
    }
  }

  private def visitDefDefImpl(dd: untpd.DefDef): J = {

    // Handle annotations - must come before extractPrefix if annotations are present
    val hasAnnotations = dd.mods != null && dd.mods.annotations.nonEmpty
    val prefix = if (hasAnnotations) {
      // Annotations are part of the span, so extract prefix to the first annotation
      val firstAnnot = dd.mods.annotations.head
      if (firstAnnot.span.exists) {
        extractPrefix(firstAnnot.span)
      } else {
        extractPrefix(dd.span)
      }
    } else {
      extractPrefix(dd.span)
    }

    val leadingAnnotations = new util.ArrayList[J.Annotation]()
    if (hasAnnotations) {
      for (annot <- dd.mods.annotations) {
        visitTree(annot) match {
          case ann: J.Annotation => leadingAnnotations.add(ann)
          case _ => // Skip
        }
      }
    }

    // Extract source from cursor to end of DefDef to find modifiers and def keyword
    val adjustedEnd = Math.max(0, dd.span.end - offsetAdjustment)
    val cursorBeforeMods = cursor
    var modifierText = ""
    var defAbsPos = -1 // absolute position of "def" in source

    if (cursor < adjustedEnd && adjustedEnd <= source.length) {
      val sourceSnippet = source.substring(cursor, adjustedEnd)
      val defIdx = sourceSnippet.indexOf("def")
      if (defIdx >= 0) {
        defAbsPos = cursor + defIdx
        if (defIdx > 0) {
          modifierText = sourceSnippet.substring(0, defIdx)
        }
      }
    }

    // Extract modifiers (private, protected, abstract, final, override)
    val (modifiers, _) = extractModifiersFromText(dd.mods, modifierText)

    // Find "def" position - after extractModifiersFromText may have advanced cursor
    val defPos = if (defAbsPos >= 0) {
      defAbsPos
    } else {
      // Fallback: search from current cursor
      val searchEnd = Math.min(cursor + 200, source.length)
      val searchText = source.substring(cursor, searchEnd)
      val idx = searchText.indexOf("def")
      if (idx >= 0) cursor + idx else cursor
    }

    val defSpace = if (defPos > cursor) {
      Space.format(source.substring(cursor, defPos))
    } else {
      Space.EMPTY
    }

    modifiers.add(new J.Modifier(
      Tree.randomId(),
      defSpace,
      Markers.EMPTY,
      "def",
      J.Modifier.Type.LanguageExtension,
      Collections.emptyList()
    ))

    cursor = defPos + "def".length

    // Extract method name
    val nameStart = if (dd.nameSpan.exists) {
      Math.max(0, dd.nameSpan.start - offsetAdjustment)
    } else {
      cursor
    }

    val nameSpace = if (cursor < nameStart && nameStart <= source.length) {
      Space.format(source.substring(cursor, nameStart))
    } else {
      Space.format(" ")
    }

    val name = new J.Identifier(
      Tree.randomId(),
      nameSpace,
      Markers.EMPTY,
      Collections.emptyList(),
      dd.name.toString,
      null,
      null
    )

    if (dd.nameSpan.exists) {
      cursor = Math.max(cursor, dd.nameSpan.end - offsetAdjustment)
    }

    // Handle type parameters (first param list may contain TypeDef nodes)
    var typeParameters: J.TypeParameters = null
    val valueParamLists = dd.paramss.filter(paramList => {
      val typeParams = paramList.collect { case tparam: untpd.TypeDef => tparam }
      if (typeParams.nonEmpty) {
        // This is a type parameter list - process it
        val searchEnd = Math.min(cursor + 50, source.length)
        val searchText = source.substring(cursor, searchEnd)
        val bracketIdx = searchText.indexOf('[')

        if (bracketIdx >= 0) {
          val bracketStart = cursor + bracketIdx
          val tpPrefix = if (bracketStart > cursor) {
            Space.format(source.substring(cursor, bracketStart))
          } else {
            Space.EMPTY
          }
          cursor = bracketStart + 1 // past '['

          val jTypeParams = new util.ArrayList[JRightPadded[J.TypeParameter]]()
          typeParams.zipWithIndex.foreach { case (tparam, idx) =>
            val jTypeParam = visitTypeParameter(tparam)
            val isLast = idx == typeParams.size - 1
            // Extract padding (comma or closing bracket)
            val after = if (!isLast && cursor < source.length) {
              val searchEnd2 = Math.min(cursor + 50, source.length)
              val text = source.substring(cursor, searchEnd2)
              val commaIdx = text.indexOf(',')
              if (commaIdx >= 0) {
                val space = Space.format(text.substring(0, commaIdx))
                cursor = cursor + commaIdx + 1
                space
              } else Space.EMPTY
            } else Space.EMPTY
            jTypeParams.add(JRightPadded.build(jTypeParam).withAfter(after))
          }

          // Find matching closing bracket (accounting for nested brackets)
          if (cursor < source.length) {
            var depth = 1
            var i = cursor
            val end = Math.min(cursor + 200, source.length)
            while (i < end && depth > 0) {
              source.charAt(i) match {
                case '[' => depth += 1
                case ']' => depth -= 1
                case _ =>
              }
              i += 1
            }
            if (depth == 0) {
              cursor = i // i is already past the closing ']'
            }
          }

          typeParameters = new J.TypeParameters(
            Tree.randomId(),
            tpPrefix,
            Markers.EMPTY,
            Collections.emptyList(),
            jTypeParams
          )
        }
        false // not a value param list
      } else {
        true // is a value param list
      }
    })

    // Handle value parameters
    val parameters: JContainer[Statement] = if (valueParamLists.nonEmpty) {
      val firstParamList = valueParamLists.head

      // Find opening parenthesis
      val searchEnd = Math.min(cursor + 50, source.length)
      val searchText = source.substring(cursor, searchEnd)
      val parenIdx = searchText.indexOf('(')

      if (parenIdx >= 0) {
        val parenStart = cursor + parenIdx
        val beforeParen = if (parenStart > cursor) {
          Space.format(source.substring(cursor, parenStart))
        } else {
          Space.EMPTY
        }
        cursor = parenStart + 1 // past '('

        val params = new util.ArrayList[JRightPadded[Statement]]()

        if (firstParamList.isEmpty) {
          // Empty parameter list: ()
          // Find closing paren
          val closeSearch = source.substring(cursor, Math.min(cursor + 50, source.length))
          val closeIdx = closeSearch.indexOf(')')
          if (closeIdx >= 0) {
            val emptySpace = if (closeIdx > 0) {
              Space.format(closeSearch.substring(0, closeIdx))
            } else {
              Space.EMPTY
            }
            params.add(JRightPadded.build(new J.Empty(Tree.randomId(), emptySpace, Markers.EMPTY).asInstanceOf[Statement]))
            cursor = cursor + closeIdx + 1
          }
        } else {
          firstParamList.zipWithIndex.foreach { case (param, idx) =>
            param match {
              case vd: untpd.ValDef =>
                val paramJ = visitMethodParameter(vd)
                val isLast = idx == firstParamList.size - 1
                val after = if (!isLast && cursor < source.length) {
                  val text = source.substring(cursor, Math.min(cursor + 50, source.length))
                  val commaIdx = text.indexOf(',')
                  if (commaIdx >= 0) {
                    val space = Space.format(text.substring(0, commaIdx))
                    cursor = cursor + commaIdx + 1
                    space
                  } else Space.EMPTY
                } else Space.EMPTY
                params.add(JRightPadded.build(paramJ.asInstanceOf[Statement]).withAfter(after))
              case _ =>
                // Fallback
            }
          }

          // Find closing parenthesis
          if (cursor < source.length) {
            val closeSearch = source.substring(cursor, Math.min(cursor + 100, source.length))
            val closeIdx = closeSearch.indexOf(')')
            if (closeIdx >= 0) {
              cursor = cursor + closeIdx + 1
            }
          }
        }

        JContainer.build(beforeParen, params, Markers.EMPTY)
      } else {
        // No parenthesis found - parameterless method like "def toString: String"
        JContainer.empty[Statement]()
      }
    } else if (dd.paramss.isEmpty) {
      // No parameter lists at all - check if source has parens
      val searchEnd = Math.min(cursor + 30, source.length)
      val searchText = source.substring(cursor, searchEnd)
      val parenIdx = searchText.indexOf('(')
      val colonIdx = searchText.indexOf(':')
      val equalsIdx = searchText.indexOf('=')

      // Only process parens if they come before colon/equals
      if (parenIdx >= 0 && (colonIdx < 0 || parenIdx < colonIdx) && (equalsIdx < 0 || parenIdx < equalsIdx)) {
        val parenStart = cursor + parenIdx
        val beforeParen = if (parenStart > cursor) {
          Space.format(source.substring(cursor, parenStart))
        } else {
          Space.EMPTY
        }
        cursor = parenStart + 1

        val params = new util.ArrayList[JRightPadded[Statement]]()
        val closeSearch = source.substring(cursor, Math.min(cursor + 50, source.length))
        val closeIdx = closeSearch.indexOf(')')
        if (closeIdx >= 0) {
          val emptySpace = if (closeIdx > 0) Space.format(closeSearch.substring(0, closeIdx)) else Space.EMPTY
          params.add(JRightPadded.build(new J.Empty(Tree.randomId(), emptySpace, Markers.EMPTY).asInstanceOf[Statement]))
          cursor = cursor + closeIdx + 1
        }
        JContainer.build(beforeParen, params, Markers.EMPTY)
      } else {
        // Truly parameterless: "def toString: String = ..."
        JContainer.empty[Statement]()
      }
    } else {
      JContainer.empty[Statement]()
    }

    // Handle additional parameter lists (curried methods)
    val additionalParamLists = new util.ArrayList[JContainer[Statement]]()
    if (valueParamLists.size > 1) {
      for (paramListIdx <- 1 until valueParamLists.size) {
        val paramList = valueParamLists(paramListIdx)

        // Find opening parenthesis
        val searchEnd2 = Math.min(cursor + 50, source.length)
        val searchText2 = source.substring(cursor, searchEnd2)
        val parenIdx2 = searchText2.indexOf('(')

        if (parenIdx2 >= 0) {
          val parenStart2 = cursor + parenIdx2
          val beforeParen2 = if (parenStart2 > cursor) {
            Space.format(source.substring(cursor, parenStart2))
          } else {
            Space.EMPTY
          }
          cursor = parenStart2 + 1

          val params2 = new util.ArrayList[JRightPadded[Statement]]()

          if (paramList.isEmpty) {
            val closeSearch2 = source.substring(cursor, Math.min(cursor + 50, source.length))
            val closeIdx2 = closeSearch2.indexOf(')')
            if (closeIdx2 >= 0) {
              val emptySpace2 = if (closeIdx2 > 0) Space.format(closeSearch2.substring(0, closeIdx2)) else Space.EMPTY
              params2.add(JRightPadded.build(new J.Empty(Tree.randomId(), emptySpace2, Markers.EMPTY).asInstanceOf[Statement]))
              cursor = cursor + closeIdx2 + 1
            }
          } else {
            paramList.zipWithIndex.foreach { case (param, idx) =>
              param match {
                case vd: untpd.ValDef =>
                  val paramJ = visitMethodParameter(vd)
                  val isLast = idx == paramList.size - 1
                  val after = if (!isLast && cursor < source.length) {
                    val text = source.substring(cursor, Math.min(cursor + 50, source.length))
                    val commaIdx = text.indexOf(',')
                    if (commaIdx >= 0) {
                      val space = Space.format(text.substring(0, commaIdx))
                      cursor = cursor + commaIdx + 1
                      space
                    } else Space.EMPTY
                  } else Space.EMPTY
                  params2.add(JRightPadded.build(paramJ.asInstanceOf[Statement]).withAfter(after))
                case _ =>
              }
            }

            // Find closing parenthesis
            if (cursor < source.length) {
              val closeSearch2 = source.substring(cursor, Math.min(cursor + 100, source.length))
              val closeIdx2 = closeSearch2.indexOf(')')
              if (closeIdx2 >= 0) {
                cursor = cursor + closeIdx2 + 1
              }
            }
          }

          additionalParamLists.add(JContainer.build(beforeParen2, params2, Markers.EMPTY))
        }
      }
    }

    // Handle return type with TypeReferencePrefix marker
    var markers = Markers.EMPTY
    if (!additionalParamLists.isEmpty) {
      markers = markers.addIfAbsent(new AdditionalParameterLists(Tree.randomId(), additionalParamLists))
    }
    val returnTypeExpression: TypeTree = dd.tpt match {
      case untpd.EmptyTree => null
      case tpt if tpt.span.exists && tpt.span.start != tpt.span.end =>
        val tptStart = Math.max(0, tpt.span.start - offsetAdjustment)
        if (cursor < tptStart && tptStart <= source.length) {
          val beforeType = source.substring(cursor, tptStart)
          val colonIdx = beforeType.indexOf(':')
          if (colonIdx >= 0) {
            val colonSpace = Space.format(beforeType.substring(0, colonIdx))
            markers = markers.addIfAbsent(new TypeReferencePrefix(Tree.randomId(), colonSpace))
            cursor = cursor + colonIdx + 1
          }
        }

        // Handle special Scala type syntax
        val typeResult = tpt match {
          case f: untpd.Function => visitFunctionType(f)
          case io: untpd.InfixOp => visitInfixType(io)
          case _: untpd.PostfixOp => visitUnknown(tpt)
          case _ => visitTree(tpt)
        }
        typeResult match {
          case tt: TypeTree => tt
          case id: J.Identifier => id
          case _ => null
        }
      case _ => null
    }

    // Handle method body
    val body: J.Block = dd.rhs match {
      case untpd.EmptyTree => null
      case rhs if rhs.span.exists =>
        val rhsStart = Math.max(0, rhs.span.start - offsetAdjustment)
        if (cursor < rhsStart && rhsStart <= source.length) {
          val beforeBody = source.substring(cursor, rhsStart)
          val equalsIdx = beforeBody.indexOf('=')
          if (equalsIdx >= 0) {
            val equalsSpace = Space.format(beforeBody.substring(0, equalsIdx))
            markers = markers.addIfAbsent(new MethodBody(Tree.randomId(), equalsSpace))
            cursor = cursor + equalsIdx + 1
          }
        }

        // Check if there's a method body `{` between cursor and rhs start
        // (separate from any `{` that's part of the rhs Block's span)
        val outerBraceIdx = if (cursor < rhsStart) {
          val between = source.substring(cursor, rhsStart)
          val bi = between.indexOf('{')
          if (bi >= 0) cursor + bi else -1
        } else -1

        val outerBracePrefix = if (outerBraceIdx >= 0) {
          val pfx = if (outerBraceIdx > cursor) {
            Space.format(source.substring(cursor, outerBraceIdx))
          } else Space.EMPTY
          cursor = outerBraceIdx + 1
          pfx
        } else null

        val innerResult = visitTree(rhs)

        // If there were outer braces, find the closing `}` and wrap
        if (outerBracePrefix != null) {
          val innerStmt: Statement = innerResult match {
            case block: J.Block => block
            case stmt: Statement => stmt
            case expr: Expression => asStatement(expr)
            case _ => null
          }
          if (innerStmt != null) {
            val rhsEnd = Math.max(0, rhs.span.end - offsetAdjustment)
            val ddEnd = Math.max(0, dd.span.end - offsetAdjustment)
            var endSpace = Space.EMPTY
            if (rhsEnd < ddEnd) {
              val remaining = source.substring(rhsEnd, ddEnd)
              val closeBraceIdx = remaining.indexOf('}')
              if (closeBraceIdx > 0) {
                endSpace = Space.format(remaining.substring(0, closeBraceIdx))
              }
            }
            val stmts = new util.ArrayList[JRightPadded[Statement]]()
            stmts.add(JRightPadded.build(innerStmt).withAfter(endSpace))
            new J.Block(
              Tree.randomId(),
              outerBracePrefix,
              Markers.EMPTY,
              JRightPadded.build(false),
              stmts,
              Space.EMPTY
            )
          } else null
        } else {
          innerResult match {
            case block: J.Block => block
            case stmt: Statement =>
              // Statement already implements Statement, wrap directly
              val statements = new util.ArrayList[JRightPadded[Statement]]()
              statements.add(JRightPadded.build(stmt))
              new J.Block(
                Tree.randomId(),
                Space.EMPTY,
                Markers.EMPTY.addIfAbsent(new OmitBraces(Tree.randomId())),
                JRightPadded.build(false),
                statements,
                Space.EMPTY
              )
            case expr: Expression =>
              val statements = new util.ArrayList[JRightPadded[Statement]]()
              statements.add(JRightPadded.build(asStatement(expr)))
              new J.Block(
                Tree.randomId(),
                Space.EMPTY,
                Markers.EMPTY.addIfAbsent(new OmitBraces(Tree.randomId())),
                JRightPadded.build(false),
                statements,
                Space.EMPTY
              )
          case _ => null
          }
        }
      case _ => null
    }

    updateCursor(dd.span.end)

    new J.MethodDeclaration(
      Tree.randomId(),
      prefix,
      markers,
      leadingAnnotations,
      modifiers,
      typeParameters,
      returnTypeExpression,
      new J.MethodDeclaration.IdentifierWithAnnotations(
        name,
        Collections.emptyList()
      ),
      parameters,
      null,
      body,
      null,
      null
    )
  }

  private def visitNamedArg(namedArg: untpd.NamedArg): J = {
    val savedCursor = cursor
    try {
      val prefix = extractPrefix(namedArg.span)

      // Create identifier for the argument name
      val name = new J.Identifier(
        Tree.randomId(),
        Space.EMPTY,
        Markers.EMPTY,
        Collections.emptyList(),
        namedArg.name.toString,
        null,
        null
      )

      // Advance cursor past the name
      cursor = cursor + namedArg.name.toString.length

      // Find the '=' between name and value
      val argStart = Math.max(0, namedArg.arg.span.start - offsetAdjustment)
      var equalsSpace = Space.EMPTY
      if (cursor < argStart && argStart <= source.length) {
        val between = source.substring(cursor, argStart)
        val eqIdx = between.indexOf('=')
        if (eqIdx >= 0) {
          equalsSpace = Space.format(between.substring(0, eqIdx))
          cursor = cursor + eqIdx + 1
        }
      }

      // Visit the argument value
      val argValue = visitTree(namedArg.arg) match {
        case expr: Expression => expr
        case _ =>
          cursor = savedCursor
          return visitUnknown(namedArg)
      }

      updateCursor(namedArg.span.end)

      new J.Assignment(
        Tree.randomId(),
        prefix,
        Markers.EMPTY,
        name,
        JLeftPadded.build(argValue).withBefore(equalsSpace),
        null
      )
    } catch {
      case _: Exception =>
        cursor = savedCursor
        visitUnknown(namedArg)
    }
  }

  private def visitMethodParameter(vd: untpd.ValDef): J = {
    val hasAnnotations = vd.mods != null && vd.mods.annotations.nonEmpty
    val paramPrefix = if (hasAnnotations) {
      // Don't extract prefix here - annotations handle their own prefix
      Space.EMPTY
    } else {
      extractPrefix(vd.span)
    }

    // Handle annotations on the parameter
    val leadingAnnotations = new util.ArrayList[J.Annotation]()
    if (hasAnnotations) {
      for (annot <- vd.mods.annotations) {
        visitTree(annot) match {
          case ann: J.Annotation => leadingAnnotations.add(ann)
          case _ => // Skip if not mapped to annotation
        }
      }
    }

    // After annotations, extract space before the name
    val namePrefix = if (leadingAnnotations.size() > 0 && vd.nameSpan.exists) {
      val nameStart = Math.max(0, vd.nameSpan.start - offsetAdjustment)
      if (cursor < nameStart && nameStart <= source.length) {
        val between = source.substring(cursor, nameStart)
        Space.format(between)
      } else Space.EMPTY
    } else Space.EMPTY

    val paramName = new J.Identifier(
      Tree.randomId(),
      namePrefix,
      Markers.EMPTY,
      Collections.emptyList(),
      vd.name.toString,
      null,
      null
    )

    // Update cursor past name
    if (vd.nameSpan.exists) {
      cursor = Math.max(cursor, vd.nameSpan.end - offsetAdjustment)
    }

    // Handle type annotation
    val typeExpr: TypeTree = vd.tpt match {
      case untpd.EmptyTree => null
      case tpt if tpt.span.exists =>
        // Find colon between name and type
        val tptStart = Math.max(0, tpt.span.start - offsetAdjustment)
        if (cursor < tptStart && tptStart <= source.length) {
          val between = source.substring(cursor, tptStart)
          val colonIdx = between.indexOf(':')
          if (colonIdx >= 0) {
            cursor = cursor + colonIdx + 1
          }
        }
        // Handle special Scala type syntax in parameter types
        val tptResult = tpt match {
          case f: untpd.Function => visitFunctionType(f)
          case io: untpd.InfixOp => visitInfixType(io)
          case _: untpd.PostfixOp => visitUnknown(tpt)
          case bnt: untpd.ByNameTypeTree => visitByNameTypeTree(bnt)
          case _ => visitTree(tpt)
        }
        tptResult match {
          case tt: TypeTree => tt
          case id: J.Identifier => id
          case unknown: J.Unknown => unknown
          case _ => null
        }
      case _ => null
    }

    // Handle default value
    val initializer: JLeftPadded[Expression] = vd.rhs match {
      case untpd.EmptyTree => null
      case rhs if rhs.span.exists =>
        val rhsStart = Math.max(0, rhs.span.start - offsetAdjustment)
        var equalsSpace = Space.EMPTY
        if (cursor < rhsStart && rhsStart <= source.length) {
          val between = source.substring(cursor, rhsStart)
          val equalsIdx = between.indexOf('=')
          if (equalsIdx >= 0) {
            equalsSpace = Space.format(between.substring(0, equalsIdx))
            cursor = cursor + equalsIdx + 1
          }
        }
        visitTree(rhs) match {
          case expr: Expression =>
            JLeftPadded.build(expr).withBefore(equalsSpace)
          case _ => null
        }
      case _ => null
    }

    val variable = new J.VariableDeclarations.NamedVariable(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      paramName,
      Collections.emptyList(),
      initializer,
      null
    )

    new J.VariableDeclarations(
      Tree.randomId(),
      paramPrefix,
      Markers.build(Collections.singletonList(new LambdaParameter())),
      leadingAnnotations,
      Collections.emptyList(),
      typeExpr,
      null,
      Collections.emptyList(),
      Collections.singletonList(JRightPadded.build(variable))
    )
  }
  
  private def visitPatDef(patDef: untpd.PatDef): J = {
    // PatDef is used for pattern-based definitions like `val (a, b) = (1, 2)`
    // It's also used for enum cases like `case Red, Green, Blue` — fall back for those
    val spanStart = Math.max(0, patDef.span.start - offsetAdjustment)
    val spanEnd = Math.max(0, patDef.span.end - offsetAdjustment)

    // Look backwards from span start to find val/var keyword
    val keywordStart = if (spanStart > 0 && cursor < spanStart) {
      val beforeSpan = source.substring(cursor, spanStart)
      val valIdx = beforeSpan.lastIndexOf("val")
      val varIdx = beforeSpan.lastIndexOf("var")
      val kwIdx = Math.max(valIdx, varIdx)
      if (kwIdx >= 0) cursor + kwIdx else spanStart
    } else {
      spanStart
    }

    // Check if this is actually a val/var pattern definition
    // If the source at keywordStart doesn't start with val or var, fall back to J.Unknown
    if (keywordStart >= 0 && keywordStart < source.length) {
      val atKeyword = source.substring(keywordStart, Math.min(keywordStart + 4, source.length))
      if (!atKeyword.startsWith("val") && !atKeyword.startsWith("var")) {
        // Not a val/var pattern — likely an enum case or other construct
        val prefix = extractPrefix(patDef.span)
        val sourceText = extractSource(patDef.span)
        cursor = spanEnd
        return visitUnknownFromSource(prefix, sourceText)
      }
    }

    // Extract prefix (whitespace before the keyword)
    val prefix = if (keywordStart > cursor && keywordStart <= source.length) {
      val prefixText = source.substring(cursor, keywordStart)
      cursor = keywordStart
      Space.format(prefixText)
    } else {
      Space.EMPTY
    }

    // Determine val/var keyword
    val isVal = if (cursor < source.length) {
      val afterCursor = source.substring(cursor, Math.min(cursor + 4, source.length))
      afterCursor.startsWith("val")
    } else true

    // Consume the val/var keyword
    val keyword = if (isVal) "val" else "var"
    sourceBefore(keyword)

    // Visit the pattern - typically a tuple like (a, b)
    if (patDef.pats.isEmpty) {
      // Fallback for empty patterns
      cursor = spanEnd
      return visitUnknownFromSource(prefix, source.substring(keywordStart, spanEnd))
    }

    val pat = patDef.pats.head
    val patResult = pat match {
      case tuple: untpd.Tuple => visitTuple(tuple)
      case _ => visitTree(pat)
    }

    // Handle optional type annotation
    var typeExpression: TypeTree = null
    if (patDef.tpt != null && !patDef.tpt.isEmpty && patDef.tpt.span.exists) {
      // Consume the colon
      sourceBefore(":")
      val tptResult = visitTree(patDef.tpt)
      tptResult match {
        case tt: TypeTree => typeExpression = tt
        case _ => // ignore
      }
    }

    // Handle the initializer (rhs)
    var initializer: Expression = null
    var beforeEquals = Space.EMPTY
    if (patDef.rhs != null && !patDef.rhs.isEmpty && patDef.rhs.span.exists) {
      beforeEquals = sourceBefore("=")
      val rhsResult = visitTree(patDef.rhs)
      rhsResult match {
        case expr: Expression =>
          initializer = expr
        case _ =>
          // Fallback
          initializer = visitUnknown(patDef.rhs).asInstanceOf[Expression]
      }
    }

    // Build NamedVariable with the pattern as the name
    val namedVar = patResult match {
      case tp: S.TuplePattern =>
        new J.VariableDeclarations.NamedVariable(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          tp, // TuplePattern implements VariableDeclarator so can serve as the name
          Collections.emptyList(),
          if (initializer != null) JLeftPadded.build(initializer).withBefore(beforeEquals) else null,
          null
        )
      case id: J.Identifier =>
        new J.VariableDeclarations.NamedVariable(
          Tree.randomId(),
          Space.EMPTY,
          Markers.EMPTY,
          id,
          Collections.emptyList(),
          if (initializer != null) JLeftPadded.build(initializer).withBefore(beforeEquals) else null,
          null
        )
      case _ =>
        // Fallback to J.Unknown for patterns we don't handle yet
        cursor = spanEnd
        return visitUnknownFromSource(prefix, source.substring(keywordStart, spanEnd))
    }

    // Build the variable declarations modifiers
    // val maps to Final modifier (printer uses Final to decide val vs var)
    val modifiers = new util.ArrayList[J.Modifier]()
    if (isVal) {
      modifiers.add(new J.Modifier(
        Tree.randomId(),
        Space.EMPTY,
        Markers.EMPTY,
        null,
        J.Modifier.Type.Final,
        Collections.emptyList()
      ))
    }

    new J.VariableDeclarations(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      Collections.emptyList(), // leading annotations
      modifiers,
      typeExpression,
      null, // varargs
      Collections.emptyList(), // dimension brackets
      Collections.singletonList(JRightPadded.build(namedVar))
    )
  }

  private def visitUnknownFromSource(prefix: Space, sourceText: String): J.Unknown = {
    new J.Unknown(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      new J.Unknown.Source(Tree.randomId(), Space.EMPTY, Markers.EMPTY, sourceText)
    )
  }

  private def visitUnknown(tree: untpd.Tree): J.Unknown = {
    val prefix = extractPrefix(tree.span)
    val sourceText = extractSource(tree.span)

    val unknownSource = new J.Unknown.Source(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      sourceText
    )

    new J.Unknown(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      unknownSource
    )
  }
  
  def extractPrefix(span: Spans.Span): Space = {
    if (!span.exists) {
      return Space.EMPTY
    }
    
    val start = cursor
    val adjustedTreeStart = Math.max(0, span.start - offsetAdjustment)
    
    if (adjustedTreeStart > cursor && adjustedTreeStart <= source.length) {
      cursor = adjustedTreeStart
      // Use Space.format to properly extract comments from whitespace
      Space.format(source.substring(start, adjustedTreeStart))
    } else {
      Space.EMPTY
    }
  }
  
  private def extractSource(span: Spans.Span): String = {
    if (!span.exists) {
      return ""
    }
    
    val adjustedStart = Math.max(0, span.start - offsetAdjustment)
    val adjustedEnd = Math.max(0, span.end - offsetAdjustment)
    
    if (adjustedStart >= 0 && adjustedEnd <= source.length && adjustedEnd > adjustedStart) {
      cursor = adjustedEnd
      val result = source.substring(adjustedStart, adjustedEnd)
      result
    } else {
      ""
    }
  }
  
  /**
   * Extract whitespace and comments before the next occurrence of a delimiter.
   * Similar to sourceBefore in ReloadableJava17Parser.
   */
  private def sourceBefore(untilDelim: String): Space = {
    val delimIndex = source.indexOf(untilDelim, cursor)
    if (delimIndex < 0) {
      Space.EMPTY
    } else {
      val prefix = source.substring(cursor, delimIndex)
      cursor = delimIndex + untilDelim.length
      Space.format(prefix)
    }
  }
  
  /**
   * Extract whitespace between the current cursor position and the given position.
   */
  private def spaceBetween(startPos: Int, endPos: Int): Space = {
    val adjustedStart = Math.max(0, startPos - offsetAdjustment)
    val adjustedEnd = Math.max(0, endPos - offsetAdjustment)
    
    if (adjustedStart >= cursor && adjustedEnd > adjustedStart && adjustedEnd <= source.length) {
      val spaceText = source.substring(cursor, adjustedStart)
      cursor = adjustedStart
      Space.format(spaceText)
    } else {
      Space.EMPTY
    }
  }
  
  /**
   * Find the position of the next occurrence of a delimiter.
   */
  private def positionOfNext(delimiter: String, startFrom: Int = cursor): Int = {
    val pos = source.indexOf(delimiter, startFrom)
    if (pos >= 0) pos else -1
  }
  
  /**
   * Skip whitespace and return the position of the next non-whitespace character.
   */
  private def indexOfNextNonWhitespace(startFrom: Int = cursor): Int = {
    var i = startFrom
    while (i < source.length && Character.isWhitespace(source.charAt(i))) {
      i += 1
    }
    i
  }
  
  private def extractModifiersFromText(mods: untpd.Modifiers, modifierText: String): (util.ArrayList[J.Modifier], Int) = {
    import dotty.tools.dotc.core.Flags
    val modifierList = new util.ArrayList[J.Modifier]()
    
    // The order matters - we'll add them in the order they appear in source
    val modifierKeywords = List(
      ("private", Flags.Private, J.Modifier.Type.Private),
      ("protected", Flags.Protected, J.Modifier.Type.Protected),
      ("abstract", Flags.Abstract, J.Modifier.Type.Abstract),
      ("final", Flags.Final, J.Modifier.Type.Final),
      ("sealed", Flags.Sealed, J.Modifier.Type.LanguageExtension),
      ("implicit", Flags.Implicit, J.Modifier.Type.LanguageExtension),
      ("override", Flags.Override, J.Modifier.Type.LanguageExtension)
    )
    
    // Create a list of (position, keyword, type) for modifiers that are present
    val presentModifiers = modifierKeywords.flatMap { case (keyword, flag, modType) =>
      if (mods.is(flag)) {
        val pos = modifierText.indexOf(keyword)
        if (pos >= 0) Some((pos, keyword, modType)) else None
      } else None
    }.sortBy(_._1) // Sort by position in source
    
    // Build modifiers with proper spacing
    var lastEnd = 0
    for ((pos, keyword, modType) <- presentModifiers) {
      // Space before this modifier
      val spaceBefore = if (pos > lastEnd) {
        Space.format(modifierText.substring(lastEnd, pos))
      } else {
        Space.EMPTY
      }
      
      modifierList.add(new J.Modifier(
        Tree.randomId(),
        spaceBefore,
        Markers.EMPTY,
        keyword,
        modType,
        Collections.emptyList()
      ))
      
      lastEnd = pos + keyword.length
    }
    
    // Update cursor to skip past the modifiers we've consumed
    if (!modifierList.isEmpty && modifierText.nonEmpty) {
      cursor = cursor + lastEnd
    }
    
    (modifierList, lastEnd)
  }
  
  private def constantToJavaType(const: Constant): JavaType.Primitive = const.tag match {
    case BooleanTag => JavaType.Primitive.Boolean
    case ByteTag => JavaType.Primitive.Byte
    case CharTag => JavaType.Primitive.Char
    case ShortTag => JavaType.Primitive.Short
    case IntTag => JavaType.Primitive.Int
    case LongTag => JavaType.Primitive.Long
    case FloatTag => JavaType.Primitive.Float
    case DoubleTag => JavaType.Primitive.Double
    case StringTag => JavaType.Primitive.String
    case NullTag => JavaType.Primitive.Null
    case _ => null
  }
  
  private def visitTypeParameter(tparam: untpd.TypeDef): J.TypeParameter = {
    val prefix = extractPrefix(tparam.span)
    
    // Check for variance annotation in the source
    val adjustedStart = Math.max(0, tparam.span.start - offsetAdjustment)
    val adjustedEnd = Math.max(0, tparam.span.end - offsetAdjustment)
    var varianceSpace = Space.EMPTY
    var nameStr = tparam.name.toString
    
    if (adjustedStart < adjustedEnd && adjustedStart >= cursor && adjustedEnd <= source.length) {
      val paramSource = source.substring(adjustedStart, adjustedEnd)
      // Check if it starts with + or -
      if (paramSource.startsWith("+") || paramSource.startsWith("-")) {
        // Include the variance annotation in the name
        val variance = paramSource.charAt(0)
        nameStr = variance + tparam.name.toString
        cursor = adjustedStart + 1 // Skip past the variance symbol
      }
    }
    
    // Update cursor past the name
    if (tparam.nameSpan.exists) {
      cursor = Math.max(cursor, tparam.nameSpan.end - offsetAdjustment)
    }

    // Check for sub-type-parameters like [_] in F[_]
    if (cursor < adjustedEnd && adjustedEnd <= source.length) {
      val afterName = source.substring(cursor, adjustedEnd)
      if (afterName.startsWith("[")) {
        var depth = 0
        var i = 0
        var bracketEnd = afterName.length
        var found = false
        while (i < afterName.length && !found) {
          afterName.charAt(i) match {
            case '[' => depth += 1
            case ']' =>
              depth -= 1
              if (depth == 0) {
                bracketEnd = i + 1
                found = true
              }
            case _ =>
          }
          i += 1
        }
        nameStr = nameStr + afterName.substring(0, bracketEnd)
        cursor = cursor + bracketEnd
      }
    }

    // Extract the type parameter name (after sub-type-params are included)
    val name = new J.Identifier(
      Tree.randomId(),
      varianceSpace,
      Markers.EMPTY,
      Collections.emptyList(),
      nameStr,
      null,
      null
    )

    var boundOperator: String = null
    val bounds: JContainer[TypeTree] = tparam.rhs match {
      case bt: untpd.TypeBoundsTree if !bt.hi.isEmpty =>
        // Upper bound: T <: Upper
        val searchEnd = Math.min(cursor + 100, source.length)
        val searchText = source.substring(cursor, searchEnd)
        val boundIdx = searchText.indexOf("<:")
        if (boundIdx >= 0) {
          boundOperator = "<:"
          val beforeBound = Space.format(searchText.substring(0, boundIdx))
          cursor = cursor + boundIdx + 2 // past "<:"
          val boundType = visitTree(bt.hi) match {
            case tt: TypeTree => tt
            case id: J.Identifier => id
            case other => other.asInstanceOf[TypeTree]
          }
          val boundList = new util.ArrayList[JRightPadded[TypeTree]]()
          boundList.add(JRightPadded.build(boundType))
          JContainer.build(beforeBound, boundList, Markers.EMPTY)
        } else {
          null
        }
      case bt: untpd.TypeBoundsTree if !bt.lo.isEmpty =>
        // Lower bound: T >: Lower
        val searchEnd = Math.min(cursor + 100, source.length)
        val searchText = source.substring(cursor, searchEnd)
        val boundIdx = searchText.indexOf(">:")
        if (boundIdx >= 0) {
          boundOperator = ">:"
          val beforeBound = Space.format(searchText.substring(0, boundIdx))
          cursor = cursor + boundIdx + 2 // past ">:"
          val boundType = visitTree(bt.lo) match {
            case tt: TypeTree => tt
            case id: J.Identifier => id
            case other => other.asInstanceOf[TypeTree]
          }
          val boundList = new util.ArrayList[JRightPadded[TypeTree]]()
          boundList.add(JRightPadded.build(boundType))
          JContainer.build(beforeBound, boundList, Markers.EMPTY)
        } else {
          null
        }
      case _ => null
    }

    val markers = if (boundOperator != null) {
      Markers.EMPTY.addIfAbsent(new TypeBoundOperator(Tree.randomId(), boundOperator))
    } else {
      Markers.EMPTY
    }

    new J.TypeParameter(
      Tree.randomId(),
      prefix,
      markers,
      Collections.emptyList(), // annotations
      Collections.emptyList(), // modifiers
      name,
      bounds
    )
  }
  
  private def extractTypeParametersSource(td: untpd.TypeDef): String = {
    // This method is not actually used anymore since we get type params from the AST
    // We only need to update the cursor position correctly
    ""
  }
  
  private def extractConstructorParametersSource(td: untpd.TypeDef): String = {
    // Extract constructor parameters from source
    if (td.span.exists && td.nameSpan.exists) {
      // First check if we have type parameters and skip past them
      var searchStart = Math.max(0, td.nameSpan.end - offsetAdjustment)
      
      // Skip type parameters if present
      if (searchStart < source.length && source.charAt(searchStart) == '[') {
        var depth = 1
        var i = searchStart + 1
        while (i < source.length && depth > 0) {
          source.charAt(i) match {
            case '[' => depth += 1
            case ']' => depth -= 1
            case _ =>
          }
          i += 1
        }
        if (depth == 0) {
          searchStart = i // Start looking for constructor params after type params
        }
      }
      
      val classEnd = Math.max(0, td.span.end - offsetAdjustment)
      
      if (searchStart < classEnd && searchStart >= 0 && classEnd <= source.length) {
        val afterNameAndTypeParams = source.substring(searchStart, classEnd)
        
        // Look for opening parenthesis after class name and type parameters
        // Check if it starts with parenthesis (possibly with whitespace)
        val trimmed = afterNameAndTypeParams.trim()

        // Handle access modifier before constructor params (e.g., "private(val value: Int)")
        var effectiveTrimmed = trimmed
        var modPrefix = ""
        for (kw <- List("private", "protected")) {
          if (modPrefix.isEmpty && effectiveTrimmed.startsWith(kw) && effectiveTrimmed.length > kw.length) {
            val rest = effectiveTrimmed.substring(kw.length).dropWhile(_.isWhitespace)
            if (rest.startsWith("(")) {
              modPrefix = kw
              effectiveTrimmed = rest
            }
          }
        }

        if (effectiveTrimmed.startsWith("(")) {
          // Find the position of the opening parenthesis
          val parenStartSearch = if (modPrefix.nonEmpty) {
            afterNameAndTypeParams.indexOf(modPrefix)
          } else {
            0
          }
          val parenStart = afterNameAndTypeParams.indexOf("(", parenStartSearch)

          // Find matching closing parenthesis
          var depth = 1
          var i = parenStart + 1
          while (i < afterNameAndTypeParams.length && depth > 0) {
            afterNameAndTypeParams(i) match {
              case '(' => depth += 1
              case ')' => depth -= 1
              case _ =>
            }
            i += 1
          }

          if (depth == 0) {
            // For regular params "(val x: Int)" starts from paren
            // For modified params "private(val x: Int)" starts from modifier
            // Include leading whitespace so space between name and params is preserved
            val extractStart = if (modPrefix.nonEmpty) {
              // Find start of modifier, but include any leading whitespace in afterNameAndTypeParams
              // since the Unknown source text is what gets printed verbatim
              0
            } else {
              parenStart
            }
            val params = afterNameAndTypeParams.substring(extractStart, i)
            cursor = searchStart + i
            return params
          }
        }
      }
    }
    ""
  }
  
  private def createPrimaryConstructor(constructorParams: List[untpd.ValDef], template: untpd.Template): J.MethodDeclaration = {
    // Create method name with Implicit marker (similar to Kotlin)
    val name = new J.Identifier(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY, // TODO: Add Scala implicit marker
      Collections.emptyList(),
      "<constructor>",
      null,
      null
    )
    
    // Visit constructor parameters
    val params = new util.ArrayList[JRightPadded[Statement]]()
    for (param <- constructorParams) {
      // For now, preserve constructor parameters as Unknown
      val paramTree = visitUnknown(param)
      params.add(JRightPadded.build(paramTree.asInstanceOf[Statement]))
    }
    
    // Build parameter container
    val paramContainer = if (params.isEmpty) {
      JContainer.empty[Statement]()
    } else {
      JContainer.build(
        Space.EMPTY,
        params,
        Markers.EMPTY
      )
    }
    
    new J.MethodDeclaration(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY, // TODO: Add Scala PrimaryConstructor marker
      Collections.emptyList(), // annotations
      Collections.emptyList(), // modifiers
      null, // type parameters
      null, // return type
      new J.MethodDeclaration.IdentifierWithAnnotations(
        name,
        Collections.emptyList()
      ),
      paramContainer,
      null, // throws
      null, // body
      null, // default value
      null  // method type
    )
  }
  
  private def visitTyped(typed: untpd.Typed): J = {
    
    // Check if this is a member reference pattern (expr _)
    typed.tpt match {
      case id: untpd.Ident if id.name.toString == "_" =>
        // This is a member reference like "greet _"
        val prefix = extractPrefix(typed.span)
        
        // Visit the expression part (the method/field being referenced)
        val expr = visitTree(typed.expr) match {
          case e: Expression => e
          case _ => return visitUnknown(typed)
        }
        
        // Create a member reference
        new J.MemberReference(
          Tree.randomId(),
          prefix,
          Markers.EMPTY,
          JRightPadded.build(expr),
          null, // No type parameters for now
          JLeftPadded.build(new J.Identifier(
            Tree.randomId(),
            Space.SINGLE_SPACE,
            Markers.EMPTY,
            Collections.emptyList(),
            "_",
            null,
            null
          )),
          null, // type
          null, // method type
          null  // variable type  
        )
      case _ =>
        // Type ascription: expr: Type
        val prefix = extractPrefix(typed.span)

        // Visit the expression
        val expr = visitTree(typed.expr) match {
          case e: Expression => e
          case _ => return visitUnknown(typed)
        }

        // Find and consume the colon
        val colonSpace = sourceBefore(":")

        // Visit the type
        val typeTree = typed.tpt match {
          case f: untpd.Function => visitFunctionType(f)
          case _ => visitTree(typed.tpt) match {
            case tt: TypeTree => tt
            case id: J.Identifier => id.asInstanceOf[TypeTree]
            case _ => return visitUnknown(typed)
          }
        }

        new S.TypeAscription(
          Tree.randomId(),
          prefix,
          Markers.EMPTY,
          expr,
          typeTree,
          null
        )
    }
  }

  private def visitByNameTypeTree(bnt: untpd.ByNameTypeTree): S.ByNameType = {
    val prefix = extractPrefix(bnt.span)

    // Advance cursor past "=>"
    val bntStart = Math.max(0, bnt.span.start - offsetAdjustment)
    val bntEnd = Math.max(0, bnt.span.end - offsetAdjustment)
    if (cursor <= bntStart && bntEnd <= source.length) {
      val text = source.substring(cursor, bntEnd)
      val arrowIdx = text.indexOf("=>")
      if (arrowIdx >= 0) {
        cursor = cursor + arrowIdx + 2
      }
    }

    // Visit the inner result type (e.g., Unit)
    val innerType = visitTree(bnt.result) match {
      case tt: TypeTree => tt
      case id: J.Identifier => id
      case other =>
        // Fallback: create identifier from source
        new J.Identifier(
          Tree.randomId(),
          extractPrefix(bnt.result.span),
          Markers.EMPTY,
          Collections.emptyList(),
          extractSource(bnt.result.span),
          null,
          null
        )
    }

    new S.ByNameType(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      innerType,
      null
    )
  }

  private def visitFunction(func: untpd.Function): J = {
    
    // Check if this is a partially applied function or underscore placeholder lambda
    // In Scala, `add(5, _)` is parsed as Function(List(_$1), Apply(add, List(5, _$1)))
    // Also `_ * 2` is parsed as Function(List(_$1), InfixOp(_$1, *, 2))
    // We need to detect these patterns and handle them specially
    
    // Get the parameter names generated by the compiler (like _$1, _$2, etc.)
    val syntheticParams = func.args.collect {
      case vd: untpd.ValDef if vd.name.toString.startsWith("_$") => vd.name.toString
    }.toSet
    
    if (syntheticParams.nonEmpty) {
      // Check if the source contains actual underscore placeholders
      val funcSource = extractSource(func.span)
      val hasUnderscorePlaceholder = funcSource.contains("_")
      
      // If we have synthetic params and underscore in source, it's likely a placeholder lambda
      // These should be treated as regular lambdas but we skip the synthetic param
      if (hasUnderscorePlaceholder) {
        func.body match {
          case app: untpd.Apply =>
            // This might be a partially applied function like add(5, _)
            // Check if it's a method invocation with underscore arguments
            var hasPartialApplication = false
            app.args.foreach {
              case id: untpd.Ident if syntheticParams.contains(id.name.toString) =>
                hasPartialApplication = true
              case _ =>
            }
            
            if (hasPartialApplication) {
              // Partially applied function - return the method invocation
              val prefix = extractPrefix(func.span)
              val result = visitApply(app)
              result match {
                case mi: J.MethodInvocation => return mi.withPrefix(prefix)
                case _ => return result
              }
            }
          case _ =>
            // For other cases like `_ * 2`, this is an underscore placeholder lambda
            // We need to create a proper lambda with S.Wildcard in the body
            val prefix = extractPrefix(func.span)
            
            // Set a flag to indicate we're in an underscore placeholder context
            val oldSyntheticParams = currentSyntheticParams
            currentSyntheticParams = syntheticParams
            
            // Visit the body - the visitIdent method will now create S.Wildcard for synthetic params
            val body = visitTree(func.body)
            
            // Restore the flag
            currentSyntheticParams = oldSyntheticParams
            
            // Create a wildcard parameter for the lambda parameter list
            val wildcard = new S.Wildcard(
              Tree.randomId(),
              Space.EMPTY,
              Markers.EMPTY,
              null
            )
            
            val params = new util.ArrayList[JRightPadded[J]]()
            params.add(JRightPadded.build(wildcard))
            
            val parameters = new J.Lambda.Parameters(
              Tree.randomId(),
              Space.EMPTY,
              Markers.EMPTY,
              false, // no parentheses for underscore syntax
              params
            )
            
            // Create lambda with the underscore placeholder marker
            val lambda = new J.Lambda(
              Tree.randomId(),
              prefix,
              Markers.build(Collections.singletonList(new UnderscorePlaceholderLambda(UUID.randomUUID()))),
              parameters,
              Space.EMPTY, // no space before => for underscore syntax
              body,
              null
            )
            
            return lambda
        }
      }
    }
    
    val prefix = extractPrefix(func.span)
    
    // Build lambda parameters
    val parameters = new J.Lambda.Parameters(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      false, // parenthesized - will be set based on syntax
      Collections.emptyList() // Will fill in the parameters
    )
    
    // Visit lambda parameters
    val params = new util.ArrayList[JRightPadded[J]]()
    var hasParentheses = false
    
    // Check if parameters are parenthesized by looking at the source
    val funcSource = extractSource(func.span)
    hasParentheses = funcSource.trim.startsWith("(")
    val arrowIndex = funcSource.indexOf("=>")
    
    for (i <- func.args.indices) {
      val param = func.args(i)
      
      // For parameters after the first, we need to handle the comma and space
      if (i > 0) {
        // Look for comma between previous and current parameter
        val prevParam = func.args(i - 1)
        val prevEnd = prevParam.span.end - offsetAdjustment
        val currentStart = param.span.start - offsetAdjustment
        
        if (prevEnd < currentStart && prevEnd >= cursor && currentStart <= source.length) {
          val between = source.substring(prevEnd, currentStart)
          val commaIdx = between.indexOf(',')
          if (commaIdx >= 0) {
            // Move cursor past the comma
            cursor = prevEnd + commaIdx + 1
          }
        }
      }
      
      // Visit parameter as lambda parameter - it will extract its own prefix
      val paramTree = param match {
        case vd: untpd.ValDef => visitLambdaParameter(vd)
        case _ => visitTree(param)
      }
      
      // Extract space after the parameter (before comma or closing paren)
      var afterSpace = Space.EMPTY
      if (i < func.args.length - 1) {
        // Not the last parameter, space before comma
        val currentEnd = param.span.end - offsetAdjustment
        if (currentEnd >= cursor) {
          // Look for comma after this parameter
          val searchStart = Math.max(cursor, currentEnd)
          val commaSearchEnd = Math.min(searchStart + 20, source.length) // reasonable search distance
          val searchStr = source.substring(searchStart, commaSearchEnd)
          val commaIdx = searchStr.indexOf(',')
          if (commaIdx >= 0) {
            afterSpace = Space.format(searchStr.substring(0, commaIdx))
            cursor = searchStart + commaIdx + 1
          }
        }
      } else {
        // Last parameter, look for space before closing paren
        val paramEnd = param.span.end - offsetAdjustment
        if (paramEnd >= cursor) {
          cursor = paramEnd
        }
      }
      
      params.add(JRightPadded.build(paramTree).withAfter(afterSpace))
    }
    
    // Update parameters with the actual params
    val updatedParams = new J.Lambda.Parameters(
      parameters.getId,
      parameters.getPrefix,
      parameters.getMarkers,
      hasParentheses,
      params
    )
    
    // Extract arrow and spacing (arrowIndex already computed above)
    var arrowPrefix = Space.EMPTY
    if (arrowIndex >= 0) {
      // Find the space before =>
      var spaceStart = arrowIndex - 1
      while (spaceStart >= 0 && Character.isWhitespace(funcSource.charAt(spaceStart))) {
        spaceStart -= 1
      }
      if (spaceStart < arrowIndex - 1) {
        arrowPrefix = Space.format(funcSource.substring(spaceStart + 1, arrowIndex))
      }
      // Move cursor to right after the arrow (past =>)
      val arrowEndPos = func.span.start + arrowIndex + 2 - offsetAdjustment
      cursor = arrowEndPos
    }
    
    // Visit the lambda body - it will extract its own prefix including space after =>
    val body = visitTree(func.body)
    
    new J.Lambda(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      updatedParams,
      arrowPrefix,
      body,
      null // type
    )
  }
  
  private def visitTypeAlias(td: untpd.TypeDef): S.TypeAlias = {
    val prefix = extractPrefix(td.span)

    // Extract modifiers (e.g., opaque) before the 'type' keyword
    val modifiers = new util.ArrayList[J.Modifier]()

    // Check for 'opaque' modifier before 'type' keyword
    val typeKeywordIdx = source.indexOf("type", cursor)
    if (typeKeywordIdx > cursor) {
      val beforeType = source.substring(cursor, typeKeywordIdx).trim
      if (beforeType.contains("opaque")) {
        val opaqueSpace = sourceBefore("opaque")
        cursor = cursor // sourceBefore already advances
        modifiers.add(new J.Modifier(
          Tree.randomId(),
          opaqueSpace,
          Markers.EMPTY,
          "opaque",
          J.Modifier.Type.LanguageExtension,
          Collections.emptyList()
        ))
      }
    }

    // Space before 'type' keyword (e.g., space between 'opaque' and 'type', or empty if no modifiers)
    val typeKeywordSpace = sourceBefore("type")

    // Space between 'type' and name
    val nameStr = td.name.toString
    val namePrefix = sourceBefore(nameStr)

    val name = new J.Identifier(
      Tree.randomId(),
      namePrefix,
      Markers.EMPTY,
      Collections.emptyList(),
      nameStr,
      null,
      null
    )
    // sourceBefore already advanced cursor past the name

    // Handle type parameters if present (e.g., type Pair[A, B] = (A, B))
    // In Dotty, parameterized type aliases have rhs as LambdaTypeTree
    var typeParams: JContainer[J.TypeParameter] = null
    val actualRhs: untpd.Tree = td.rhs match {
      case ltt: untpd.LambdaTypeTree =>
        // Extract type parameters from the LambdaTypeTree
        if (ltt.tparams.nonEmpty) {
          // Look for opening bracket
          val searchEnd = Math.min(cursor + 100, source.length)
          val searchText = source.substring(cursor, searchEnd)
          val bracketIdx = searchText.indexOf('[')
          val openingBracketSpace = if (bracketIdx >= 0) {
            val space = Space.format(searchText.substring(0, bracketIdx))
            cursor = cursor + bracketIdx + 1
            space
          } else {
            Space.EMPTY
          }

          val jTypeParams = new util.ArrayList[JRightPadded[J.TypeParameter]]()
          ltt.tparams.zipWithIndex.foreach { case (tparam, idx) =>
            val jTypeParam = visitTypeParameter(tparam)
            val isLast = idx == ltt.tparams.size - 1
            val after = if (!isLast) {
              sourceBefore(",")
            } else {
              Space.EMPTY
            }
            jTypeParams.add(new JRightPadded(jTypeParam, after, Markers.EMPTY))
          }

          // Advance past closing bracket
          val closePos = positionOfNext("]")
          if (closePos >= 0) {
            cursor = closePos + 1
          }

          typeParams = JContainer.build(openingBracketSpace, jTypeParams, Markers.EMPTY)
        }
        ltt.body
      case other => other
    }

    // Check if this is an abstract type member (no = sign, no rhs) or a type alias
    val rhs = actualRhs
    val hasInitializer = rhs != null && !rhs.isEmpty && rhs.span.exists && (rhs match {
      case bt: untpd.TypeBoundsTree => !bt.lo.isEmpty || !bt.hi.isEmpty
      case _ => true
    })

    val initializer: JLeftPadded[Expression] = if (hasInitializer) {
      // Find and consume the = sign
      val equalsSpace = sourceBefore("=")

      // Visit the right-hand side type
      val rhsType = rhs match {
        case f: untpd.Function => visitFunctionType(f).asInstanceOf[Expression]
        case io: untpd.InfixOp => visitInfixType(io).asInstanceOf[Expression]
        case _ => visitTree(rhs) match {
          case tt: TypeTree => tt.asInstanceOf[Expression]
          case id: J.Identifier => id.asInstanceOf[Expression]
          case other => visitUnknown(rhs).asInstanceOf[Expression]
        }
      }

      JLeftPadded.build(rhsType).withBefore(equalsSpace)
    } else {
      // Abstract type member: type Inner (no = ...)
      null
    }

    new S.TypeAlias(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      modifiers,
      typeKeywordSpace,
      name,
      typeParams,
      initializer,
      null
    )
  }

  private def visitThis(thisTree: untpd.This): J = {
    val prefix = extractPrefix(thisTree.span)
    val src = extractSource(thisTree.span)

    // For qualified this like Outer.this, the source will contain "Outer.this"
    // For simple this, it's just "this"
    new J.Identifier(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      Collections.emptyList(),
      src,
      null,
      null
    )
  }

  private def visitSuper(superTree: untpd.Super): J = {
    val prefix = extractPrefix(superTree.span)
    val src = extractSource(superTree.span)

    // For super[Trait], the source will contain "super[Trait]"
    // For simple super, it's just "super"
    new J.Identifier(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      Collections.emptyList(),
      src,
      null,
      null
    )
  }

  private def visitTuple(tuple: untpd.Tuple): S.TuplePattern = {
    val prefix = extractPrefix(tuple.span)

    // Advance past opening paren
    val openParenPos = positionOfNext("(")
    if (openParenPos >= 0) {
      cursor = openParenPos + 1
    }

    val elements = new util.ArrayList[JRightPadded[Expression]]()
    for (i <- tuple.trees.indices) {
      val elem = visitTree(tuple.trees(i)) match {
        case expr: Expression => expr
        case other => visitUnknown(tuple.trees(i)).asInstanceOf[Expression]
      }
      val after = if (i < tuple.trees.size - 1) {
        sourceBefore(",")
      } else {
        Space.EMPTY
      }
      elements.add(JRightPadded.build(elem).withAfter(after))
    }

    // Advance past closing paren
    val closeParenPos = positionOfNext(")")
    if (closeParenPos >= 0) {
      cursor = closeParenPos + 1
    }

    S.TuplePattern.build(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      JContainer.build(Space.EMPTY, elements, Markers.EMPTY),
      null
    )
  }

  private def visitForYield(forYield: untpd.ForYield): J = {
    visitForComprehension(forYield.enums, forYield.expr, forYield.span, isYield = true)
  }

  private def visitInfixType(infixOp: untpd.InfixOp): S.InfixType = {
    val prefix = extractPrefix(infixOp.span)

    // Visit left type operand
    val left = visitTree(infixOp.left) match {
      case tt: TypeTree => tt
      case id: J.Identifier => id.asInstanceOf[TypeTree]
      case other => visitUnknown(infixOp.left).asInstanceOf[TypeTree]
    }

    // Extract space before the operator
    val opName = infixOp.op.name.toString
    val operatorSpace = sourceBefore(opName)

    // Create operator identifier (space after operator is on the right operand)
    val operatorId = new J.Identifier(
      Tree.randomId(),
      Space.EMPTY,
      Markers.EMPTY,
      Collections.emptyList(),
      opName,
      null,
      null
    )

    // Visit right type operand - recurse for chained infix types
    val right = infixOp.right match {
      case nested: untpd.InfixOp => visitInfixType(nested)
      case f: untpd.Function => visitFunctionType(f)
      case _ => visitTree(infixOp.right) match {
        case tt: TypeTree => tt
        case id: J.Identifier => id.asInstanceOf[TypeTree]
        case other => visitUnknown(infixOp.right).asInstanceOf[TypeTree]
      }
    }

    new S.InfixType(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      left,
      JLeftPadded.build(operatorId).withBefore(operatorSpace),
      right
    )
  }

  private def visitFunctionType(func: untpd.Function): S.FunctionType = {
    val prefix = extractPrefix(func.span)

    val funcSource = extractSource(func.span)
    val hasParentheses = funcSource.trim.startsWith("(")

    // Parse parameter types
    val paramTypes = new util.ArrayList[JRightPadded[TypeTree]]()

    if (hasParentheses) {
      // Advance cursor past opening paren
      val openParenPos = positionOfNext("(")
      if (openParenPos >= 0) {
        cursor = openParenPos + 1
      }
    }

    for (i <- func.args.indices) {
      val arg = func.args(i)
      val paramType = arg match {
        case io: untpd.InfixOp => visitInfixType(io)
        case f: untpd.Function => visitFunctionType(f)
        case _ => visitTree(arg) match {
          case tt: TypeTree => tt
          case id: J.Identifier => id.asInstanceOf[TypeTree]
          case other => visitUnknown(arg).asInstanceOf[TypeTree]
        }
      }

      var afterSpace = Space.EMPTY
      if (i < func.args.length - 1) {
        // Look for comma after this parameter type
        val commaPos = positionOfNext(",")
        if (commaPos >= 0) {
          afterSpace = Space.format(source.substring(cursor, commaPos))
          cursor = commaPos + 1
        }
      } else if (hasParentheses) {
        // Last param, look for closing paren
        val closeParenPos = positionOfNext(")")
        if (closeParenPos >= 0) {
          afterSpace = Space.format(source.substring(cursor, closeParenPos))
          cursor = closeParenPos + 1
        }
      }

      paramTypes.add(JRightPadded.build(paramType).withAfter(afterSpace))
    }

    // Find and consume the => arrow
    val arrowSpace = sourceBefore("=>")

    // Visit the return type -- if it's also a Function, recurse as FunctionType
    val returnType = func.body match {
      case innerFunc: untpd.Function => visitFunctionType(innerFunc)
      case io: untpd.InfixOp => visitInfixType(io)
      case _ => visitTree(func.body) match {
        case tt: TypeTree => tt
        case id: J.Identifier => id.asInstanceOf[TypeTree]
        case other => visitUnknown(func.body).asInstanceOf[TypeTree]
      }
    }

    val containerBefore = if (hasParentheses) Space.EMPTY else Space.EMPTY
    val parameters = JContainer.build(containerBefore, paramTypes, Markers.EMPTY)

    new S.FunctionType(
      Tree.randomId(),
      prefix,
      Markers.EMPTY,
      parameters,
      arrowSpace,
      returnType
    )
  }

  def getRemainingSource: String = {
    if (cursor < source.length) {
      val remaining = source.substring(cursor)
      // If we have offset adjustment (wrapped expression), we might have extra wrapper code
      // Check if remaining is just whitespace or closing braces from the wrapper
      if (offsetAdjustment > 0) {
        val trimmed = remaining.trim
        // Check if it's just the closing brace from the wrapper
        if (trimmed == "}" || trimmed.isEmpty) {
          ""
        } else {
          remaining
        }
      } else {
        remaining
      }
    } else {
      ""
    }
  }
}


