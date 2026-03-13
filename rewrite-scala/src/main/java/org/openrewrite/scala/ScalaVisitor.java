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
package org.openrewrite.scala;

import org.openrewrite.SourceFile;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.java.JavaVisitor;
import org.openrewrite.java.tree.Expression;
import org.openrewrite.java.tree.J;
import org.openrewrite.java.tree.JContainer;
import org.openrewrite.java.tree.JLeftPadded;
import org.openrewrite.java.tree.Space;
import org.openrewrite.scala.tree.S;

/**
 * ScalaVisitor extends JavaVisitor to support visiting both Java (J) and Scala (S) AST elements.
 * This allows Scala code to be processed by Java-focused recipes while also supporting
 * Scala-specific transformations.
 */
public class ScalaVisitor<P> extends JavaVisitor<P> {

    @Override
    public boolean isAcceptable(SourceFile sourceFile, P p) {
        return sourceFile instanceof S.CompilationUnit;
    }

    @Override
    public String getLanguage() {
        return "scala";
    }

    public J visitCompilationUnit(S.CompilationUnit cu, P p) {
        S.CompilationUnit c = cu;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COMPILATION_UNIT_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        
        if (c.getPackageDeclaration() != null) {
            c = c.withPackageDeclaration(visitAndCast(c.getPackageDeclaration(), p));
        }
        
        c = c.withImports(ListUtils.map(c.getImports(), i -> visitAndCast(i, p)));
        c = c.withStatements(ListUtils.map(c.getStatements(), s -> visitAndCast(s, p)));
        c = c.withEof(visitSpace(c.getEof(), Space.Location.COMPILATION_UNIT_EOF, p));
        
        return c;
    }

    // Additional visit methods for Scala-specific constructs will be added here
    // as we implement more S types (e.g., visitTrait, visitObject, visitMatch, etc.)

    public J visitTuplePattern(S.TuplePattern tuplePattern, P p) {
        S.TuplePattern t = tuplePattern;
        t = t.withPrefix(visitSpace(t.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        t = t.withMarkers(visitMarkers(t.getMarkers(), p));
        t = t.getPadding().withElements(visitContainer(t.getPadding().getElements(), JContainer.Location.LANGUAGE_EXTENSION, p));
        return t;
    }

    public J visitWildcard(S.Wildcard wildcard, P p) {
        S.Wildcard w = wildcard;
        w = w.withPrefix(visitSpace(w.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        w = w.withMarkers(visitMarkers(w.getMarkers(), p));
        return w;
    }

    public J visitBlockExpression(S.BlockExpression blockExpression, P p) {
        S.BlockExpression b = blockExpression;
        b = b.withPrefix(visitSpace(b.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        b = b.withMarkers(visitMarkers(b.getMarkers(), p));
        b = b.withBlock(visitAndCast(b.getBlock(), p));
        return b;
    }

    public J visitInterpolatedString(S.InterpolatedString interpolatedString, P p) {
        S.InterpolatedString s = interpolatedString;
        s = s.withPrefix(visitSpace(s.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public J visitMatchExpression(S.MatchExpression matchExpression, P p) {
        S.MatchExpression m = matchExpression;
        m = m.withPrefix(visitSpace(m.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withSelector((Expression) visit(m.getSelector(), p));
        m = m.withCases(ListUtils.map(m.getCases(), c -> (S.CaseClause) visitCaseClause(c, p)));
        return m;
    }

    public J visitCaseClause(S.CaseClause caseClause, P p) {
        S.CaseClause c = caseClause;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public J visitExpressionStatement(S.ExpressionStatement expressionStatement, P p) {
        return expressionStatement.acceptScala(this, p);
    }

    public J visitByNameType(S.ByNameType byNameType, P p) {
        S.ByNameType b = byNameType;
        b = b.withPrefix(visitSpace(b.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        b = b.withMarkers(visitMarkers(b.getMarkers(), p));
        b = b.withTypeTree(visitAndCast(b.getTypeTree(), p));
        return b;
    }

    public J visitTypeAlias(S.TypeAlias typeAlias, P p) {
        S.TypeAlias t = typeAlias;
        t = t.withPrefix(visitSpace(t.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        t = t.withMarkers(visitMarkers(t.getMarkers(), p));
        t = t.withModifiers(ListUtils.map(t.getModifiers(), m -> visitAndCast(m, p)));
        t = t.withTypeKeyword(visitSpace(t.getTypeKeyword(), Space.Location.LANGUAGE_EXTENSION, p));
        t = t.withName(visitAndCast(t.getName(), p));
        if (t.getPadding().getTypeParameters() != null) {
            t = t.getPadding().withTypeParameters(visitContainer(t.getPadding().getTypeParameters(), JContainer.Location.TYPE_PARAMETERS, p));
        }
        if (t.getPadding().getInitializer() != null) {
            t = t.getPadding().withInitializer(visitLeftPadded(t.getPadding().getInitializer(), JLeftPadded.Location.LANGUAGE_EXTENSION, p));
        }
        return t;
    }

    public J visitTypeAscription(S.TypeAscription typeAscription, P p) {
        S.TypeAscription t = typeAscription;
        t = t.withPrefix(visitSpace(t.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        t = t.withMarkers(visitMarkers(t.getMarkers(), p));
        t = t.withExpression(visitAndCast(t.getExpression(), p));
        t = t.withTypeTree(visitAndCast(t.getTypeTree(), p));
        return t;
    }

    public J visitFunctionType(S.FunctionType functionType, P p) {
        S.FunctionType f = functionType;
        f = f.withPrefix(visitSpace(f.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        f = f.withMarkers(visitMarkers(f.getMarkers(), p));
        f = f.getPadding().withParameters(visitContainer(f.getPadding().getParameters(), JContainer.Location.LANGUAGE_EXTENSION, p));
        f = f.withArrow(visitSpace(f.getArrow(), Space.Location.LANGUAGE_EXTENSION, p));
        f = f.withReturnType(visitAndCast(f.getReturnType(), p));
        return f;
    }

    public J visitInfixType(S.InfixType infixType, P p) {
        S.InfixType t = infixType;
        t = t.withPrefix(visitSpace(t.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        t = t.withMarkers(visitMarkers(t.getMarkers(), p));
        t = t.withLeft(visitAndCast(t.getLeft(), p));
        t = t.getPadding().withOperator(visitLeftPadded(t.getPadding().getOperator(), JLeftPadded.Location.LANGUAGE_EXTENSION, p));
        t = t.withRight(visitAndCast(t.getRight(), p));
        return t;
    }

    public J visitEnumCase(S.EnumCase enumCase, P p) {
        S.EnumCase e = enumCase;
        e = e.withPrefix(visitSpace(e.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withName(visitAndCast(e.getName(), p));
        if (e.getPadding().getExtending() != null) {
            e = e.getPadding().withExtending(visitLeftPadded(e.getPadding().getExtending(), JLeftPadded.Location.LANGUAGE_EXTENSION, p));
        }
        if (e.getPadding().getArguments() != null) {
            e = e.getPadding().withArguments(visitContainer(e.getPadding().getArguments(), JContainer.Location.LANGUAGE_EXTENSION, p));
        }
        return e;
    }
}