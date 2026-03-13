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

import lombok.*;
import lombok.experimental.FieldDefaults;
import lombok.experimental.NonFinal;
import org.jspecify.annotations.Nullable;
import org.openrewrite.*;
import org.openrewrite.java.internal.TypesInUse;
import org.openrewrite.java.tree.*;
import org.openrewrite.marker.Markers;
import org.openrewrite.scala.ScalaPrinter;
import org.openrewrite.scala.ScalaVisitor;

import java.beans.Transient;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * The Scala language-specific AST types extend the J interface and its sub-types.
 * S types represent Scala-specific constructs that have no direct equivalent in Java.
 * When a Scala construct can be represented using Java's AST, we compose J types.
 */
public interface S extends J {
    @SuppressWarnings("unchecked")
    @Override
    default <R extends Tree, P> R accept(TreeVisitor<R, P> v, P p) {
        return (R) acceptScala(v.adapt(ScalaVisitor.class), p);
    }

    @Override
    default <P> boolean isAcceptable(TreeVisitor<?, P> v, P p) {
        return v.isAdaptableTo(ScalaVisitor.class);
    }

    default <P> @Nullable J acceptScala(ScalaVisitor<P> v, P p) {
        return v.defaultValue(this, p);
    }

    @Override
    Space getPrefix();

    @Override
    default List<Comment> getComments() {
        return getPrefix().getComments();
    }

    /**
     * Represents a Scala compilation unit (.scala file).
     * Extends J.CompilationUnit to reuse package, imports, and type declarations.
     */
    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    final class CompilationUnit implements S, JavaSourceFile, SourceFile {
        @Nullable
        @NonFinal
        transient SoftReference<TypesInUse> typesInUse;

        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @EqualsAndHashCode.Include
        @With
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @With
        @Getter
        Path sourcePath;

        @With
        @Getter
        @Nullable
        FileAttributes fileAttributes;

        @Nullable // for backwards compatibility
        @With(AccessLevel.PRIVATE)
        String charsetName;

        @With
        @Getter
        boolean charsetBomMarked;

        @With
        @Getter
        @Nullable
        Checksum checksum;

        @Nullable
        JRightPadded<J.Package> packageDeclaration;

        @Override
        public J.Package getPackageDeclaration() {
            return packageDeclaration == null ? null : packageDeclaration.getElement();
        }

        @Override
        public S.CompilationUnit withPackageDeclaration(J.Package packageDeclaration) {
            return getPadding().withPackageDeclaration(JRightPadded.withElement(this.packageDeclaration, packageDeclaration));
        }

        List<JRightPadded<J.Import>> imports;

        @Override
        public List<J.Import> getImports() {
            return JRightPadded.getElements(imports);
        }

        @Override
        public S.CompilationUnit withImports(List<J.Import> imports) {
            return (S.CompilationUnit) getPadding().withImports(JRightPadded.withElements(this.imports, imports));
        }

        List<JRightPadded<Statement>> statements;

        public List<Statement> getStatements() {
            return JRightPadded.getElements(statements);
        }

        public S.CompilationUnit withStatements(List<Statement> statements) {
            return getPadding().withStatements(JRightPadded.withElements(this.statements, statements));
        }

        @With
        @Getter
        Space eof;

        @Override
        public Charset getCharset() {
            return charsetName == null ? Charset.defaultCharset() : Charset.forName(charsetName);
        }

        @SuppressWarnings("unchecked")
        @Override
        public SourceFile withCharset(Charset charset) {
            return withCharsetName(charset.name());
        }

        public S.CompilationUnit withCharsetName(String charsetName) {
            return this.charsetName == charsetName ? this : new S.CompilationUnit(
                this.typesInUse, this.padding, id, prefix, markers, sourcePath, fileAttributes, 
                charsetName, charsetBomMarked, checksum, packageDeclaration, imports, statements, eof
            );
        }

        @Override
        public List<J.ClassDeclaration> getClasses() {
            // TODO: Extract class declarations from statements
            return Collections.emptyList();
        }

        @Override
        public S.CompilationUnit withClasses(List<J.ClassDeclaration> classes) {
            // TODO: Handle class updates
            return this;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitCompilationUnit(this, p);
        }

        @Override
        public <P> TreeVisitor<?, PrintOutputCapture<P>> printer(Cursor cursor) {
            return new ScalaPrinter<>();
        }

        @Override
        public TypesInUse getTypesInUse() {
            TypesInUse cache;
            if (this.typesInUse == null) {
                cache = TypesInUse.build(this);
                this.typesInUse = new SoftReference<>(cache);
            } else {
                cache = this.typesInUse.get();
                if (cache == null || cache.getCu() != this) {
                    cache = TypesInUse.build(this);
                    this.typesInUse = new SoftReference<>(cache);
                }
            }
            return cache;
        }

        @Override
        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding implements JavaSourceFile.Padding {
            private final S.CompilationUnit t;

            public @Nullable JRightPadded<J.Package> getPackageDeclaration() {
                return t.packageDeclaration;
            }

            public S.CompilationUnit withPackageDeclaration(@Nullable JRightPadded<J.Package> packageDeclaration) {
                return t.packageDeclaration == packageDeclaration ? t : new S.CompilationUnit(
                    t.typesInUse, t.padding, t.id, t.prefix, t.markers, t.sourcePath, t.fileAttributes,
                    t.charsetName, t.charsetBomMarked, t.checksum, packageDeclaration, t.imports, t.statements, t.eof
                );
            }

            @Override
            public List<JRightPadded<J.Import>> getImports() {
                return t.imports;
            }

            @Override
            public S.CompilationUnit withImports(List<JRightPadded<J.Import>> imports) {
                return t.imports == imports ? t : new S.CompilationUnit(
                    t.typesInUse, t.padding, t.id, t.prefix, t.markers, t.sourcePath, t.fileAttributes,
                    t.charsetName, t.charsetBomMarked, t.checksum, t.packageDeclaration, imports, t.statements, t.eof
                );
            }

            public List<JRightPadded<Statement>> getStatements() {
                return t.statements;
            }

            public S.CompilationUnit withStatements(List<JRightPadded<Statement>> statements) {
                return t.statements == statements ? t : new S.CompilationUnit(
                    t.typesInUse, t.padding, t.id, t.prefix, t.markers, t.sourcePath, t.fileAttributes,
                    t.charsetName, t.charsetBomMarked, t.checksum, t.packageDeclaration, t.imports, statements, t.eof
                );
            }
        }
    }

    /**
     * Represents a tuple pattern used in destructuring assignments and declarations.
     * For example: val (a, b) = (1, 2) or (x, y) = pair
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    @Data
    final class TuplePattern implements S, Expression, TypeTree, VariableDeclarator {

        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;

        JContainer<Expression> elements;
        
        public static TuplePattern build(UUID id, Space prefix, Markers markers, JContainer<Expression> elements, JavaType type) {
            return new TuplePattern(null, id, prefix, markers, elements, type);
        }

        public List<Expression> getElements() {
            return elements.getElements();
        }

        public S.TuplePattern withElements(List<Expression> elements) {
            return getPadding().withElements(JContainer.withElements(this.elements, elements));
        }

        @With
        @Nullable
        JavaType type;

        @Override
        public List<J.Identifier> getNames() {
            List<J.Identifier> names = new ArrayList<>();
            collectNames(elements.getElements(), names);
            return names;
        }

        private void collectNames(List<Expression> expressions, List<J.Identifier> names) {
            for (Expression expr : expressions) {
                if (expr instanceof J.Identifier) {
                    names.add((J.Identifier) expr);
                } else if (expr instanceof S.TuplePattern) {
                    collectNames(((S.TuplePattern) expr).getElements(), names);
                }
            }
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitTuplePattern(this, p);
        }

        @Override
        public CoordinateBuilder.Expression getCoordinates() {
            return new CoordinateBuilder.Expression(this);
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final S.TuplePattern t;

            public JContainer<Expression> getElements() {
                return t.elements;
            }

            public S.TuplePattern withElements(JContainer<Expression> elements) {
                return t.elements == elements ? t : new S.TuplePattern(null, t.id, t.prefix, t.markers, elements, t.type);
            }
        }
    }

    /**
     * Represents a wildcard/underscore placeholder in expressions.
     * Used for partially applied functions (e.g., add(5, _)) and pattern matching.
     * This is NOT for type wildcards (use J.Wildcard) or import wildcards (use * in J.Import).
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @Data
    final class Wildcard implements S, Expression, TypedTree {

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;

        @With
        @Nullable
        JavaType type;

        public Wildcard(UUID id, Space prefix, Markers markers, @Nullable JavaType type) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.type = type;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitWildcard(this, p);
        }

        @Override
        public CoordinateBuilder.Expression getCoordinates() {
            return new CoordinateBuilder.Expression(this);
        }
    }

    /**
     * Represents a block used as an expression in Scala.
     * In Scala, blocks are expressions that return the value of their last statement.
     * For example: val x = { val temp = 10; temp * 2 }
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @Data
    final class BlockExpression implements S, Expression, TypedTree {

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;
        
        @With
        J.Block block;

        @With
        @Nullable
        JavaType type;

        public BlockExpression(UUID id, Space prefix, Markers markers, J.Block block, @Nullable JavaType type) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.block = block;
            this.type = type;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitBlockExpression(this, p);
        }

        @Override
        public CoordinateBuilder.Expression getCoordinates() {
            return new CoordinateBuilder.Expression(this);
        }
    }

    /**
     * Represents a Scala string interpolation expression.
     * For example: s"Hello, $name", s"The answer is ${x + 1}", raw"path\\$x"
     *
     * The interpolator id (e.g., "s", "f", "raw") is stored along with
     * the parts list which alternates between literal string segments and
     * interpolated expressions.
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @Data
    final class InterpolatedString implements S, Expression, TypedTree {

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;

        /**
         * The interpolator identifier (e.g., "s", "f", "raw").
         */
        @With
        String interpolator;

        /**
         * The parts of the interpolated string, alternating between
         * J.Literal (string segments) and expressions (interpolated values).
         */
        @With
        List<J> parts;

        @With
        @Nullable
        JavaType type;

        public InterpolatedString(UUID id, Space prefix, Markers markers, String interpolator, List<J> parts, @Nullable JavaType type) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.interpolator = interpolator;
            this.parts = parts;
            this.type = type;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitInterpolatedString(this, p);
        }

        @Override
        public CoordinateBuilder.Expression getCoordinates() {
            return new CoordinateBuilder.Expression(this);
        }
    }

    /**
     * Represents a Scala match expression.
     * For example: {@code x match { case 1 => "one" case _ => "other" }}
     * <p>
     * The selector is the expression being matched, and cases is a list of CaseClause entries.
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @Data
    final class MatchExpression implements S, Expression, Statement, TypedTree {

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;

        @With
        Expression selector;

        /**
         * Space before the opening brace of the match body.
         */
        @With
        Space beforeBrace;

        @With
        List<CaseClause> cases;

        /**
         * Space before the closing brace.
         */
        @With
        Space endSpace;

        @With
        @Nullable
        JavaType type;

        public MatchExpression(UUID id, Space prefix, Markers markers, Expression selector,
                               Space beforeBrace, List<CaseClause> cases, Space endSpace,
                               @Nullable JavaType type) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.selector = selector;
            this.beforeBrace = beforeBrace;
            this.cases = cases;
            this.endSpace = endSpace;
            this.type = type;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitMatchExpression(this, p);
        }

        @Override
        @Transient
        public CoordinateBuilder.Statement getCoordinates() {
            return new CoordinateBuilder.Statement(this);
        }
    }

    /**
     * Represents a single case clause in a Scala match expression or catch block.
     * For example: {@code case n: Int if n > 0 => println(n)}
     * <p>
     * Contains a pattern, an optional guard, and a body (list of statements).
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @Data
    final class CaseClause implements S, Statement {

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;

        /**
         * The pattern after "case". Stored as the raw source text of the pattern
         * since Scala patterns can be complex (type patterns, extractors, guards, etc.)
         */
        @With
        J pattern;

        /**
         * Optional guard expression (the "if" condition).
         */
        @With
        @Nullable
        JLeftPadded<Expression> guard;

        /**
         * The body statements after "=>".
         */
        @With
        Space arrow;

        @With
        List<JRightPadded<Statement>> body;

        public CaseClause(UUID id, Space prefix, Markers markers, J pattern,
                          @Nullable JLeftPadded<Expression> guard, Space arrow,
                          List<JRightPadded<Statement>> body) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.pattern = pattern;
            this.guard = guard;
            this.arrow = arrow;
            this.body = body;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitCaseClause(this, p);
        }

        @Override
        public CoordinateBuilder.Statement getCoordinates() {
            return new CoordinateBuilder.Statement(this);
        }
    }

    /**
     * Represents a Scala by-name parameter type ({@code => Unit}).
     * Similar to C#'s {@code Cs.RefType} — wraps an inner type with a keyword prefix.
     * <p>
     * For example: {@code def runTwice(body: => Unit)}
     * where {@code => Unit} is the by-name type.
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @Data
    final class ByNameType implements S, TypeTree, Expression {

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;

        @With
        TypeTree typeTree;

        @With
        @Nullable
        JavaType type;

        public ByNameType(UUID id, Space prefix, Markers markers, TypeTree typeTree,
                          @Nullable JavaType type) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.typeTree = typeTree;
            this.type = type;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitByNameType(this, p);
        }

        @Override
        public CoordinateBuilder.Expression getCoordinates() {
            return new CoordinateBuilder.Expression(this);
        }
    }

    /**
     * Represents a Scala type alias ({@code type Foo = Bar}).
     * <p>
     * Type aliases can have modifiers, type parameters, and bounds:
     * {@code type Pair[A, B] = (A, B)}, {@code opaque type Name = String}.
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    final class TypeAlias implements S, Statement, TypedTree {

        @Nullable
        @NonFinal
        transient WeakReference<TypeAlias.Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @With
        @Getter
        List<J.Modifier> modifiers;

        @With
        @Getter
        Space typeKeyword;

        @With
        @Getter
        J.Identifier name;

        @Nullable
        JContainer<J.TypeParameter> typeParameters;

        public @Nullable List<J.TypeParameter> getTypeParameters() {
            return typeParameters == null ? null : typeParameters.getElements();
        }

        public TypeAlias withTypeParameters(@Nullable List<J.TypeParameter> typeParameters) {
            return getPadding().withTypeParameters(JContainer.withElementsNullable(this.typeParameters, typeParameters));
        }

        /**
         * The right-hand side of the type alias (after {@code =}).
         * Null for abstract type members like {@code type Inner}.
         */
        @Nullable
        @With
        @Getter
        JLeftPadded<Expression> initializer;

        @Nullable
        @With
        @Getter
        JavaType type;

        @SuppressWarnings("all")
        private TypeAlias(WeakReference<Padding> padding, UUID id, Space prefix, Markers markers,
                          List<J.Modifier> modifiers, Space typeKeyword, J.Identifier name,
                          @Nullable JContainer<J.TypeParameter> typeParameters,
                          JLeftPadded<Expression> initializer, @Nullable JavaType type) {
            this.padding = padding;
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.modifiers = modifiers;
            this.typeKeyword = typeKeyword;
            this.name = name;
            this.typeParameters = typeParameters;
            this.initializer = initializer;
            this.type = type;
        }

        public TypeAlias(UUID id, Space prefix, Markers markers,
                         List<J.Modifier> modifiers, Space typeKeyword, J.Identifier name,
                         @Nullable JContainer<J.TypeParameter> typeParameters,
                         JLeftPadded<Expression> initializer, @Nullable JavaType type) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.modifiers = modifiers;
            this.typeKeyword = typeKeyword;
            this.name = name;
            this.typeParameters = typeParameters;
            this.initializer = initializer;
            this.type = type;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitTypeAlias(this, p);
        }

        @Override
        @Transient
        public CoordinateBuilder.Statement getCoordinates() {
            return new CoordinateBuilder.Statement(this);
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        public static class Padding {
            private final TypeAlias t;

            public Padding(TypeAlias t) {
                this.t = t;
            }

            public @Nullable JContainer<J.TypeParameter> getTypeParameters() {
                return t.typeParameters;
            }

            public TypeAlias withTypeParameters(@Nullable JContainer<J.TypeParameter> typeParameters) {
                return t.typeParameters == typeParameters ? t : new TypeAlias(t.id, t.prefix, t.markers,
                        t.modifiers, t.typeKeyword, t.name, typeParameters, t.initializer, t.type);
            }

            public JLeftPadded<Expression> getInitializer() {
                return t.initializer;
            }

            public TypeAlias withInitializer(JLeftPadded<Expression> initializer) {
                return t.initializer == initializer ? t : new TypeAlias(t.id, t.prefix, t.markers,
                        t.modifiers, t.typeKeyword, t.name, t.typeParameters, initializer, t.type);
            }
        }
    }

    /**
     * Represents a Scala type ascription ({@code expr: Type}).
     * <p>
     * Type ascription is used to explicitly annotate an expression with a type,
     * e.g., {@code 1: Double} or {@code Nil: List[Int]}.
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @Data
    final class TypeAscription implements S, Expression, TypedTree {

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;

        @With
        Expression expression;

        @With
        TypeTree typeTree;

        @With
        @Nullable
        JavaType type;

        public TypeAscription(UUID id, Space prefix, Markers markers,
                              Expression expression, TypeTree typeTree,
                              @Nullable JavaType type) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.expression = expression;
            this.typeTree = typeTree;
            this.type = type;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitTypeAscription(this, p);
        }

        @Override
        public CoordinateBuilder.Expression getCoordinates() {
            return new CoordinateBuilder.Expression(this);
        }
    }

    /**
     * Represents a Scala function type ({@code Int => String}, {@code (Int, String) => Boolean}).
     * <p>
     * Function types in Scala are types, not lambdas. When {@code Int => String} appears in
     * type position (e.g., {@code val f: Int => String}), it produces this AST node.
     * <p>
     * The parameters may be parenthesized ({@code (Int, String) => Boolean}) or not ({@code Int => String}).
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    final class FunctionType implements S, TypeTree, Expression {

        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        /**
         * The parameter types. For a single unparenthesized parameter like {@code Int => String},
         * this contains just one element. For {@code (Int, String) => Boolean}, it contains two.
         */
        JContainer<TypeTree> parameters;

        @SuppressWarnings("all")
        private FunctionType(WeakReference<Padding> padding, UUID id, Space prefix, Markers markers,
                             JContainer<TypeTree> parameters, Space arrow, TypeTree returnType) {
            this.padding = padding;
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.parameters = parameters;
            this.arrow = arrow;
            this.returnType = returnType;
        }

        public FunctionType(UUID id, Space prefix, Markers markers,
                            JContainer<TypeTree> parameters, Space arrow, TypeTree returnType) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.parameters = parameters;
            this.arrow = arrow;
            this.returnType = returnType;
        }

        public List<TypeTree> getParameters() {
            return parameters.getElements();
        }

        public FunctionType withParameters(List<TypeTree> parameters) {
            return getPadding().withParameters(JContainer.withElements(this.parameters, parameters));
        }

        /**
         * Space before the {@code =>} arrow.
         */
        @With
        @Getter
        Space arrow;

        /**
         * The return type after {@code =>}.
         */
        @With
        @Getter
        TypeTree returnType;

        @Override
        public @Nullable JavaType getType() {
            return returnType.getType();
        }

        @Override
        public <T extends J> T withType(@Nullable JavaType type) {
            //noinspection unchecked
            return (T) withReturnType(returnType.withType(type));
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitFunctionType(this, p);
        }

        @Override
        public CoordinateBuilder.Expression getCoordinates() {
            return new CoordinateBuilder.Expression(this);
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        public static class Padding {
            private final FunctionType t;

            public Padding(FunctionType t) {
                this.t = t;
            }

            public JContainer<TypeTree> getParameters() {
                return t.parameters;
            }

            public FunctionType withParameters(JContainer<TypeTree> parameters) {
                return t.parameters == parameters ? t : new FunctionType(t.id, t.prefix, t.markers, parameters, t.arrow, t.returnType);
            }
        }
    }

    /**
     * Represents a Scala 3 enum case with optional extends clause and constructor arguments.
     * <p>
     * For example: {@code case Mercury extends Planet(3.303e+23, 2.4397e6)}
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @Data
    final class EnumCase implements S, Statement {

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;

        @With
        J.Identifier name;

        /**
         * The extends type (e.g., "extends Planet").
         */
        @With
        @Nullable
        JLeftPadded<TypeTree> extending;

        /**
         * Constructor arguments (e.g., "(3.303e+23, 2.4397e6)").
         */
        @With
        @Nullable
        JContainer<Expression> arguments;

        public EnumCase(UUID id, Space prefix, Markers markers, J.Identifier name,
                        @Nullable JLeftPadded<TypeTree> extending,
                        @Nullable JContainer<Expression> arguments) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.name = name;
            this.extending = extending;
            this.arguments = arguments;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitEnumCase(this, p);
        }

        @Override
        @Transient
        public CoordinateBuilder.Statement getCoordinates() {
            return new CoordinateBuilder.Statement(this);
        }

        public Padding getPadding() {
            return new Padding(this);
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final EnumCase t;

            public @Nullable JLeftPadded<TypeTree> getExtending() {
                return t.extending;
            }

            public EnumCase withExtending(@Nullable JLeftPadded<TypeTree> extending) {
                return t.extending == extending ? t : new EnumCase(t.id, t.prefix, t.markers, t.name, extending, t.arguments);
            }

            public @Nullable JContainer<Expression> getArguments() {
                return t.arguments;
            }

            public EnumCase withArguments(@Nullable JContainer<Expression> arguments) {
                return t.arguments == arguments ? t : new EnumCase(t.id, t.prefix, t.markers, t.name, t.extending, arguments);
            }
        }
    }

    /**
     * Represents a Scala infix type ({@code A | B}, {@code A & B}, {@code A with B}, {@code A Either B}).
     * <p>
     * Infix types use an operator between two type operands. Common uses:
     * <ul>
     *   <li>Union types: {@code String | Int}</li>
     *   <li>Intersection types: {@code A & B}</li>
     *   <li>Compound types: {@code A with B}</li>
     *   <li>User-defined infix types: {@code A Either B}</li>
     * </ul>
     */
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @Data
    final class InfixType implements S, TypeTree, Expression {

        @With
        @EqualsAndHashCode.Include
        UUID id;

        @With
        Space prefix;

        @With
        Markers markers;

        @With
        TypeTree left;

        @With
        JLeftPadded<J.Identifier> operator;

        @With
        TypeTree right;

        public InfixType(UUID id, Space prefix, Markers markers,
                         TypeTree left, JLeftPadded<J.Identifier> operator, TypeTree right) {
            this.id = id;
            this.prefix = prefix;
            this.markers = markers;
            this.left = left;
            this.operator = operator;
            this.right = right;
        }

        @Override
        public @Nullable JavaType getType() {
            return null;
        }

        @Override
        public <T extends J> T withType(@Nullable JavaType type) {
            //noinspection unchecked
            return (T) this;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            return v.visitInfixType(this, p);
        }

        @Override
        public CoordinateBuilder.Expression getCoordinates() {
            return new CoordinateBuilder.Expression(this);
        }

        public Padding getPadding() {
            return new Padding(this);
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final InfixType t;

            public JLeftPadded<J.Identifier> getOperator() {
                return t.operator;
            }

            public InfixType withOperator(JLeftPadded<J.Identifier> operator) {
                return t.operator == operator ? t : new InfixType(t.id, t.prefix, t.markers, t.left, operator, t.right);
            }
        }
    }

    /**
     * Wraps an Expression to also implement Statement, allowing expressions to appear
     * in statement position (e.g., last expression in a block, if-then branches).
     * This is the Scala equivalent of Kotlin's K.ExpressionStatement.
     */
    @Getter
    @SuppressWarnings("unchecked")
    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @With
    final class ExpressionStatement implements S, Expression, Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Expression expression;

        // Convenience constructor
        @SuppressWarnings("unused")
        public ExpressionStatement(Expression expression) {
            this.id = Tree.randomId();
            this.expression = expression;
        }

        @Override
        public <P> J acceptScala(ScalaVisitor<P> v, P p) {
            J j = v.visit(getExpression(), p);
            if (j instanceof ExpressionStatement) {
                return j;
            } else if (j instanceof Expression) {
                return withExpression((Expression) j);
            }
            return j;
        }

        @Override
        public <J2 extends J> J2 withPrefix(Space space) {
            return (J2) withExpression(expression.withPrefix(space));
        }

        @Override
        public Space getPrefix() {
            return expression.getPrefix();
        }

        @Override
        public <J2 extends Tree> J2 withMarkers(Markers markers) {
            return (J2) withExpression(expression.withMarkers(markers));
        }

        @Override
        public Markers getMarkers() {
            return expression.getMarkers();
        }

        @Override
        public @Nullable JavaType getType() {
            return expression.getType();
        }

        @Override
        public <T extends J> T withType(@Nullable JavaType type) {
            return (T) withExpression(expression.withType(type));
        }

        @Transient
        @Override
        public CoordinateBuilder.Statement getCoordinates() {
            return new CoordinateBuilder.Statement(this);
        }
    }
}