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

import org.jspecify.annotations.Nullable;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.Tree;
import org.openrewrite.java.JavaPrinter;
import org.openrewrite.java.tree.Expression;
import org.openrewrite.java.tree.J;
import org.openrewrite.java.tree.JContainer;
import org.openrewrite.java.tree.JLeftPadded;
import org.openrewrite.java.tree.JRightPadded;
import org.openrewrite.java.tree.Space;
import org.openrewrite.java.tree.Statement;
import org.openrewrite.java.tree.TypeTree;
import org.openrewrite.scala.marker.BlockArgument;
import org.openrewrite.scala.marker.SObject;
import org.openrewrite.scala.marker.ScalaCatch;
import org.openrewrite.scala.marker.ScalaForLoop;
import org.openrewrite.scala.marker.TypeBoundOperator;
import org.openrewrite.scala.marker.TypeProjection;
import org.openrewrite.scala.marker.UnderscorePlaceholderLambda;
import org.openrewrite.scala.tree.S;

import java.util.List;

/**
 * ScalaPrinter is responsible for converting the Scala LST back to source code.
 * It extends JavaPrinter to reuse most of the Java printing logic.
 */
public class ScalaPrinter<P> extends JavaPrinter<P> {

    @Override
    protected void visitContainer(String before, @Nullable JContainer<? extends J> container, 
                                  JContainer.Location location, String suffixBetween, 
                                  @Nullable String after, PrintOutputCapture<P> p) {
        if (location == JContainer.Location.TYPE_PARAMETERS) {
            // For type parameters, check if we're being called with explicit brackets
            // If so, use them; otherwise default to Scala-style square brackets
            String openBracket = before.isEmpty() ? "[" : before;
            String closeBracket = (after == null || after.isEmpty()) ? "]" : after;
            
            if (container != null) {
                visitSpace(container.getBefore(), location.getBeforeLocation(), p);
                p.append(openBracket);
                visitRightPadded(container.getPadding().getElements(), location.getElementLocation(), suffixBetween, p);
                p.append(closeBracket);
            }
        } else {
            // Delegate to superclass for other container types
            super.visitContainer(before, container, location, suffixBetween, after, p);
        }
    }
    
    @Override
    public J visitTypeParameters(J.TypeParameters typeParams, PrintOutputCapture<P> p) {
        // Use Scala-style square brackets instead of angle brackets
        visitSpace(typeParams.getPrefix(), Space.Location.TYPE_PARAMETERS, p);
        visit(typeParams.getAnnotations(), p);
        p.append('[');
        visitRightPadded(typeParams.getPadding().getTypeParameters(), JRightPadded.Location.TYPE_PARAMETER, ",", p);
        p.append(']');
        return typeParams;
    }
    
    @Override
    public J visitTypeParameter(J.TypeParameter typeParam, PrintOutputCapture<P> p) {
        // Print type parameter, but bounds use Scala syntax
        beforeSyntax(typeParam, Space.Location.TYPE_PARAMETERS_PREFIX, p);
        visit(typeParam.getAnnotations(), p);
        visit(typeParam.getName(), p);
        
        // Print bounds if present using Scala syntax
        if (typeParam.getPadding().getBounds() != null) {
            visitSpace(typeParam.getPadding().getBounds().getBefore(), Space.Location.TYPE_BOUNDS, p);
            String op = typeParam.getMarkers().findFirst(TypeBoundOperator.class)
                .map(TypeBoundOperator::getOperator)
                .orElse("<:");
            p.append(op);
            visitRightPadded(typeParam.getPadding().getBounds().getPadding().getElements(),
                JRightPadded.Location.TYPE_BOUND, " with", p);
        }
        
        afterSyntax(typeParam, p);
        return typeParam;
    }

    @Override
    public J visitAssignment(J.Assignment assignment, PrintOutputCapture<P> p) {
        beforeSyntax(assignment, Space.Location.ASSIGNMENT_PREFIX, p);
        visit(assignment.getVariable(), p);
        visitLeftPadded("=", assignment.getPadding().getAssignment(), JLeftPadded.Location.ASSIGNMENT, p);
        afterSyntax(assignment, p);
        return assignment;
    }
    
    @Override
    public J visitAssignmentOperation(J.AssignmentOperation assignOp, PrintOutputCapture<P> p) {
        String keyword = "";
        switch (assignOp.getOperator()) {
            case Addition:
                keyword = "+=";
                break;
            case Subtraction:
                keyword = "-=";
                break;
            case Multiplication:
                keyword = "*=";
                break;
            case Division:
                keyword = "/=";
                break;
            case Modulo:
                keyword = "%=";
                break;
            case BitAnd:
                keyword = "&=";
                break;
            case BitOr:
                keyword = "|=";
                break;
            case BitXor:
                keyword = "^=";
                break;
            case LeftShift:
                keyword = "<<=";
                break;
            case RightShift:
                keyword = ">>=";
                break;
            case UnsignedRightShift:
                keyword = ">>>=";
                break;
        }
        beforeSyntax(assignOp, Space.Location.ASSIGNMENT_OPERATION_PREFIX, p);
        visit(assignOp.getVariable(), p);
        visitSpace(assignOp.getPadding().getOperator().getBefore(), Space.Location.ASSIGNMENT_OPERATION_OPERATOR, p);
        p.append(keyword);
        visit(assignOp.getAssignment(), p);
        afterSyntax(assignOp, p);
        return assignOp;
    }
    
    @Override
    public J visitTypeCast(J.TypeCast typeCast, PrintOutputCapture<P> p) {
        beforeSyntax(typeCast, Space.Location.TYPE_CAST_PREFIX, p);
        // In Scala, type casts are written as expression.asInstanceOf[Type]
        visit(typeCast.getExpression(), p);
        p.append(".asInstanceOf");
        
        // Extract the type from the control parentheses
        if (typeCast.getClazz() instanceof J.ControlParentheses) {
            J.ControlParentheses<?> controlParens = (J.ControlParentheses<?>) typeCast.getClazz();
            visitSpace(controlParens.getPrefix(), Space.Location.CONTROL_PARENTHESES_PREFIX, p);
            p.append('[');
            visitRightPadded(controlParens.getPadding().getTree(), JRightPadded.Location.PARENTHESES, "", p);
            p.append(']');
        }
        
        afterSyntax(typeCast, p);
        return typeCast;
    }
    
    @Override
    protected void printStatementTerminator(Statement s, PrintOutputCapture<P> p) {
        // In Scala, semicolons are optional and generally not used
        // Only print them if they were explicitly in the source
        // For now, we'll skip semicolons entirely as proper semicolon preservation
        // would require tracking whether they were present in the original source
        return;
    }
    
    @Override
    public J visit(@Nullable Tree tree, PrintOutputCapture<P> p) {
        if (tree instanceof S.CompilationUnit) {
            return visitScalaCompilationUnit((S.CompilationUnit) tree, p);
        } else if (tree instanceof S.Wildcard) {
            return visitWildcard((S.Wildcard) tree, p);
        } else if (tree instanceof S.TuplePattern) {
            return visitTuplePattern((S.TuplePattern) tree, p);
        } else if (tree instanceof S.BlockExpression) {
            return visitBlockExpression((S.BlockExpression) tree, p);
        } else if (tree instanceof S.InterpolatedString) {
            return visitInterpolatedString((S.InterpolatedString) tree, p);
        } else if (tree instanceof S.MatchExpression) {
            return visitMatchExpression((S.MatchExpression) tree, p);
        } else if (tree instanceof S.CaseClause) {
            return visitCaseClause((S.CaseClause) tree, p);
        } else if (tree instanceof S.ByNameType) {
            return visitByNameType((S.ByNameType) tree, p);
        } else if (tree instanceof S.TypeAlias) {
            return visitTypeAlias((S.TypeAlias) tree, p);
        } else if (tree instanceof S.TypeAscription) {
            return visitTypeAscription((S.TypeAscription) tree, p);
        } else if (tree instanceof S.FunctionType) {
            return visitFunctionType((S.FunctionType) tree, p);
        } else if (tree instanceof S.EnumCase) {
            return visitEnumCase((S.EnumCase) tree, p);
        } else if (tree instanceof S.InfixType) {
            return visitInfixType((S.InfixType) tree, p);
        }
        return super.visit(tree, p);
    }
    
    public J visitScalaCompilationUnit(S.CompilationUnit scu, PrintOutputCapture<P> p) {
        beforeSyntax(scu, Space.Location.COMPILATION_UNIT_PREFIX, p);

        if (scu.getPackageDeclaration() != null) {
            visit(scu.getPackageDeclaration(), p);
            // In Scala, package declarations are followed by a newline
            // Check if the next element has a newline in its prefix, if not add one
            if (!scu.getImports().isEmpty()) {
                J.Import firstImport = scu.getImports().get(0);
                if (!firstImport.getPrefix().getWhitespace().startsWith("\n")) {
                    p.append("\n");
                }
            } else if (!scu.getStatements().isEmpty()) {
                Statement firstStatement = scu.getStatements().get(0);
                if (!firstStatement.getPrefix().getWhitespace().startsWith("\n")) {
                    p.append("\n");
                }
            }
        }

        for (J.Import anImport : scu.getImports()) {
            visit(anImport, p);
            // Scala imports don't end with semicolons but need newlines between them
            if (!anImport.getPrefix().getWhitespace().isEmpty() || scu.getImports().indexOf(anImport) < scu.getImports().size() - 1) {
                // Already has whitespace or not the last import
            }
        }

        for (int i = 0; i < scu.getStatements().size(); i++) {
            Statement statement = scu.getStatements().get(i);
            visit(statement, p);
        }

        visitSpace(scu.getEof(), Space.Location.COMPILATION_UNIT_EOF, p);
        afterSyntax(scu, p);
        return scu;
    }

    @Override
    public J visitPackage(J.Package pkg, PrintOutputCapture<P> p) {
        beforeSyntax(pkg, Space.Location.PACKAGE_PREFIX, p);
        p.append("package");
        visit(pkg.getExpression(), p);
        // Note: No semicolon in Scala package declarations
        afterSyntax(pkg, p);
        return pkg;
    }
    
    @Override
    public J visitFieldAccess(J.FieldAccess fieldAccess, PrintOutputCapture<P> p) {
        String separator = fieldAccess.getMarkers().findFirst(TypeProjection.class).isPresent() ? "#" : ".";
        beforeSyntax(fieldAccess, Space.Location.FIELD_ACCESS_PREFIX, p);
        visit(fieldAccess.getTarget(), p);
        visitLeftPadded(separator, fieldAccess.getPadding().getName(), JLeftPadded.Location.FIELD_ACCESS_NAME, p);
        afterSyntax(fieldAccess, p);
        return fieldAccess;
    }

    @Override
    public J visitImport(J.Import import_, PrintOutputCapture<P> p) {
        beforeSyntax(import_, Space.Location.IMPORT_PREFIX, p);
        p.append("import ");
        
        // Visit the import expression
        // Need to handle wildcard imports specially for Scala (_ instead of *)
        J.FieldAccess qualid = import_.getQualid();
        if (isWildcardImport(qualid)) {
            // Print the package part
            visitFieldAccessUpToWildcard(qualid, p);
            p.append("._");
        } else {
            visit(qualid, p);
        }
        
        // Handle aliases if present (for future use)
        if (import_.getAlias() != null) {
            p.append(" => ");
            visit(import_.getAlias(), p);
        }
        
        // Note: No semicolon in Scala import declarations
        afterSyntax(import_, p);
        return import_;
    }
    
    private boolean isWildcardImport(J.FieldAccess qualid) {
        J.Identifier name = qualid.getName();
        return "*".equals(name.getSimpleName());
    }
    
    private void visitFieldAccessUpToWildcard(J.FieldAccess qualid, PrintOutputCapture<P> p) {
        // Visit the target part (everything before the wildcard)
        visit(qualid.getTarget(), p);
    }

    @Override  
    public J visitClassDeclaration(J.ClassDeclaration classDecl, PrintOutputCapture<P> p) {
        // Check if this is a Scala object declaration
        boolean isObject = classDecl.getMarkers().findFirst(SObject.class).isPresent();
        
        // For Scala classes, we need special handling for extends/with clauses
        // Use custom handling only if this is actually a Scala class
        boolean needsScalaHandling = isObject;
        
        // Check if this is a trait (Interface kind in Scala)
        if (classDecl.getKind() == J.ClassDeclaration.Kind.Type.Interface) {
            needsScalaHandling = true;
        }
        
        // Check if we have Scala-style "with" clauses
        if (classDecl.getImplements() != null && !classDecl.getImplements().isEmpty()) {
            needsScalaHandling = true;
        }
        
        // Or if we have a primary constructor with actual parameters
        if (classDecl.getPadding().getPrimaryConstructor() != null && 
            !classDecl.getPadding().getPrimaryConstructor().getElements().isEmpty()) {
            needsScalaHandling = true;
        }
        
        // Or if we have type parameters (to ensure square brackets in Scala)
        if (classDecl.getPadding().getTypeParameters() != null &&
            !classDecl.getPadding().getTypeParameters().getElements().isEmpty()) {
            needsScalaHandling = true;
        }
        
        if (needsScalaHandling) {
            // Custom handling for Scala classes
            beforeSyntax(classDecl, Space.Location.CLASS_DECLARATION_PREFIX, p);
            visit(classDecl.getLeadingAnnotations(), p);
            
            // For objects, skip the final modifier (it's implicit)
            for (J.Modifier m : classDecl.getModifiers()) {
                if (!(isObject && m.getType() == J.Modifier.Type.Final)) {
                    visit(m, p);
                }
            }
            
            visit(classDecl.getPadding().getKind().getAnnotations(), p);
            visitSpace(classDecl.getPadding().getKind().getPrefix(), Space.Location.CLASS_KIND, p);
            
            // Print the appropriate keyword
            String kind = "";
            if (isObject) {
                // For objects, we print "object" - the "case" modifier is printed separately
                kind = "object";
            } else {
                switch (classDecl.getKind()) {
                    case Class:
                        kind = "class";
                        break;
                    case Enum:
                        kind = "enum";
                        break;
                    case Interface:
                        kind = "trait";  // Scala uses trait, not interface
                        break;
                    case Annotation:
                        kind = "@interface";
                        break;
                    case Record:
                        kind = "record";
                        break;
                }
            }
            p.append(kind);

            visit(classDecl.getName(), p);
            visitTypeParameters(classDecl.getPadding().getTypeParameters(), p);
            
            // For Scala: print primaryConstructor only if it has elements
            // The primaryConstructor container includes the parentheses and parameters
            if (classDecl.getPadding().getPrimaryConstructor() != null) {
                JContainer<Statement> primaryConstructor = classDecl.getPadding().getPrimaryConstructor();
                if (!primaryConstructor.getElements().isEmpty()) {
                    // Visit each element in the primary constructor
                    for (JRightPadded<Statement> statement : primaryConstructor.getPadding().getElements()) {
                        visit(statement.getElement(), p);
                        visitSpace(statement.getAfter(), Space.Location.RECORD_STATE_VECTOR_SUFFIX, p);
                    }
                }
            }
            
            if (classDecl.getPadding().getExtends() != null) {
                visitSpace(classDecl.getPadding().getExtends().getBefore(), Space.Location.EXTENDS, p);
                p.append("extends");
                visit(classDecl.getPadding().getExtends().getElement(), p);
            }

            if (classDecl.getPadding().getImplements() != null) {
                // In Scala, implements are printed with "with" keyword
                // The container already has the proper space before the first keyword
                
                String firstKeyword = "";
                String separator = "";
                
                if (classDecl.getPadding().getExtends() != null) {
                    // If we have extends, traits use "with"
                    firstKeyword = "with";
                    separator = "with";
                } else {
                    // If no extends, first trait uses "extends"
                    firstKeyword = "extends";
                    separator = "with";
                }
                
                // Custom handling for Scala traits
                JContainer<TypeTree> implContainer = classDecl.getPadding().getImplements();
                visitSpace(implContainer.getBefore(), Space.Location.IMPLEMENTS, p);
                p.append(firstKeyword);
                
                List<JRightPadded<TypeTree>> elements = implContainer.getPadding().getElements();
                for (int i = 0; i < elements.size(); i++) {
                    JRightPadded<TypeTree> elem = elements.get(i);
                    visit(elem.getElement(), p);
                    
                    if (i < elements.size() - 1) {
                        // Print space after element and the separator
                        visitSpace(elem.getAfter(), Space.Location.IMPLEMENTS_SUFFIX, p);
                        p.append(separator);
                    }
                }
            }

            if (classDecl.getPadding().getPermits() != null) {
                visitContainer(" permits", classDecl.getPadding().getPermits(), JContainer.Location.PERMITS, ",", "", p);
            }

            visit(classDecl.getBody(), p);
            afterSyntax(classDecl, p);
            return classDecl;
        } else {
            // For classes without Scala features, use Java printing but skip empty primary constructors
            // The Java printer would print empty parentheses for primary constructors
            if (classDecl.getPadding().getPrimaryConstructor() != null && 
                classDecl.getPadding().getPrimaryConstructor().getElements().isEmpty()) {
                // We have an empty primary constructor that shouldn't be printed
                // Use the default Java printer logic but without the primary constructor
                beforeSyntax(classDecl, Space.Location.CLASS_DECLARATION_PREFIX, p);
                visit(classDecl.getLeadingAnnotations(), p);
                for (J.Modifier m : classDecl.getModifiers()) {
                    visit(m, p);
                }
                visit(classDecl.getPadding().getKind().getAnnotations(), p);
                visitSpace(classDecl.getPadding().getKind().getPrefix(), Space.Location.CLASS_KIND, p);
                // For Scala, print "trait" for Interface kind
                String classKind = classDecl.getKind() == J.ClassDeclaration.Kind.Type.Interface ? 
                    "trait" : classDecl.getKind().name().toLowerCase();
                p.append(classKind);
                visit(classDecl.getName(), p);
                // Use our custom type parameter printing for Scala
                visitTypeParameters(classDecl.getPadding().getTypeParameters(), p);
                // Skip the empty primary constructor
                
                if (classDecl.getPadding().getExtends() != null) {
                    visitSpace(classDecl.getPadding().getExtends().getBefore(), Space.Location.EXTENDS, p);
                    p.append("extends");
                    visit(classDecl.getPadding().getExtends().getElement(), p);
                }

                if (classDecl.getPadding().getImplements() != null) {
                    visitContainer(" implements", classDecl.getPadding().getImplements(), JContainer.Location.IMPLEMENTS, ",", "", p);
                }

                if (classDecl.getPadding().getPermits() != null) {
                    visitContainer(" permits", classDecl.getPadding().getPermits(), JContainer.Location.PERMITS, ",", "", p);
                }

                visit(classDecl.getBody(), p);
                afterSyntax(classDecl, p);
                return classDecl;
            } else {
                // Use the default Java printing
                return super.visitClassDeclaration(classDecl, p);
            }
        }
    }
    
    private void visitTypeParameters(@Nullable JContainer<J.TypeParameter> typeParams, PrintOutputCapture<P> p) {
        if (typeParams != null && !typeParams.getElements().isEmpty()) {
            // In Scala, type parameters use square brackets, not angle brackets
            visitSpace(typeParams.getBefore(), Space.Location.TYPE_PARAMETERS, p);
            p.append('[');
            List<JRightPadded<J.TypeParameter>> elements = typeParams.getPadding().getElements();
            for (int i = 0; i < elements.size(); i++) {
                JRightPadded<J.TypeParameter> elem = elements.get(i);
                J.TypeParameter typeParam = elem.getElement();
                
                // Check if the type parameter name starts with variance
                if (typeParam.getName() instanceof J.Identifier) {
                    J.Identifier nameId = (J.Identifier) typeParam.getName();
                    String name = nameId.getSimpleName();
                    if (name.startsWith("-") || name.startsWith("+")) {
                        // For variance annotations, print them directly without visiting
                        // to avoid any special handling
                        visitSpace(typeParam.getPrefix(), Space.Location.TYPE_PARAMETERS_PREFIX, p);
                        p.append(name);
                    } else {
                        visit(elem.getElement(), p);
                    }
                } else {
                    visit(elem.getElement(), p);
                }
                
                if (i < elements.size() - 1) {
                    visitSpace(elem.getAfter(), Space.Location.TYPE_PARAMETER_SUFFIX, p);
                    p.append(',');
                }
            }
            p.append(']');
        }
    }

    @Override
    public J visitMethodDeclaration(J.MethodDeclaration method, PrintOutputCapture<P> p) {
        beforeSyntax(method, Space.Location.METHOD_DECLARATION_PREFIX, p);
        visit(method.getLeadingAnnotations(), p);
        for (J.Modifier m : method.getModifiers()) {
            visitModifier(m, p);
        }

        // Method name first (Scala puts type params after name, unlike Java)
        visit(method.getName(), p);

        // Type parameters - print with square brackets for Scala (after name)
        J.TypeParameters typeParameters = method.getAnnotations().getTypeParameters();
        if (typeParameters != null) {
            visit(typeParameters.getAnnotations(), p);
            visitSpace(typeParameters.getPrefix(), Space.Location.TYPE_PARAMETERS, p);
            p.append("[");
            visitRightPadded(typeParameters.getPadding().getTypeParameters(), JRightPadded.Location.TYPE_PARAMETER, ",", p);
            p.append("]");
        }

        // Parameters
        JContainer<Statement> params = method.getPadding().getParameters();
        if (params != null && !params.getElements().isEmpty()) {
            visitSpace(params.getBefore(), JContainer.Location.METHOD_DECLARATION_PARAMETERS.getBeforeLocation(), p);
            p.append("(");
            List<JRightPadded<Statement>> elements = params.getPadding().getElements();
            for (int i = 0; i < elements.size(); i++) {
                JRightPadded<Statement> element = elements.get(i);
                visit(element.getElement(), p);
                visitSpace(element.getAfter(), JRightPadded.Location.METHOD_DECLARATION_PARAMETER.getAfterLocation(), p);
                if (i < elements.size() - 1) {
                    p.append(",");
                }
            }
            p.append(")");
        }

        // Additional parameter lists (for curried methods)
        method.getMarkers().findFirst(org.openrewrite.scala.marker.AdditionalParameterLists.class)
                .ifPresent(apl -> {
                    for (JContainer<Statement> paramList : apl.getParameterLists()) {
                        visitSpace(paramList.getBefore(), JContainer.Location.METHOD_DECLARATION_PARAMETERS.getBeforeLocation(), p);
                        p.append("(");
                        List<JRightPadded<Statement>> elems = paramList.getPadding().getElements();
                        for (int j = 0; j < elems.size(); j++) {
                            JRightPadded<Statement> elem = elems.get(j);
                            visit(elem.getElement(), p);
                            visitSpace(elem.getAfter(), JRightPadded.Location.METHOD_DECLARATION_PARAMETER.getAfterLocation(), p);
                            if (j < elems.size() - 1) {
                                p.append(",");
                            }
                        }
                        p.append(")");
                    }
                });

        // Return type with colon (using TypeReferencePrefix marker for spacing)
        if (method.getReturnTypeExpression() != null) {
            method.getMarkers().findFirst(org.openrewrite.scala.marker.TypeReferencePrefix.class)
                    .ifPresent(trp -> visitSpace(trp.getPrefix(), Space.Location.LANGUAGE_EXTENSION, p));
            p.append(":");
            visit(method.getReturnTypeExpression(), p);
        }

        // Method body with = sign (using MethodBody marker for spacing)
        if (method.getBody() != null) {
            org.openrewrite.scala.marker.MethodBody bodyMarker =
                    method.getMarkers().findFirst(org.openrewrite.scala.marker.MethodBody.class).orElse(null);
            if (bodyMarker != null) {
                visitSpace(bodyMarker.getBeforeEquals(), Space.Location.LANGUAGE_EXTENSION, p);
                p.append("=");
            }

            if (method.getBody().getMarkers().findFirst(org.openrewrite.scala.marker.OmitBraces.class).isPresent()) {
                // Expression body: print statements directly without braces
                for (JRightPadded<Statement> stmt : method.getBody().getPadding().getStatements()) {
                    visitStatement(stmt, JRightPadded.Location.BLOCK_STATEMENT, p);
                }
            } else {
                // Block body: use standard block printing
                visit(method.getBody(), p);
            }
        }

        afterSyntax(method, p);
        return method;
    }

    @Override
    public J visitForEachLoop(J.ForEachLoop forEachLoop, PrintOutputCapture<P> p) {
        beforeSyntax(forEachLoop, Space.Location.FOR_EACH_LOOP_PREFIX, p);
        p.append("for");
        J.ForEachLoop.Control ctrl = forEachLoop.getControl();
        visitSpace(ctrl.getPrefix(), Space.Location.FOR_EACH_CONTROL_PREFIX, p);
        p.append('(');
        // Use "<-" instead of Java's ":"
        visitRightPadded(ctrl.getPadding().getVariable(), JRightPadded.Location.FOREACH_VARIABLE, "<-", p);
        visitRightPadded(ctrl.getPadding().getIterable(), JRightPadded.Location.FOREACH_ITERABLE, "", p);
        p.append(')');
        // Check if this is a for-yield (ScalaForLoop marker containing "yield")
        ScalaForLoop forMarker = forEachLoop.getMarkers().findFirst(ScalaForLoop.class).orElse(null);
        if (forMarker != null && forMarker.getOriginalSource() != null && forMarker.getOriginalSource().contains("yield")) {
            // Print the yield keyword with preserved spacing
            p.append(forMarker.getOriginalSource());
        }
        visitStatement(forEachLoop.getPadding().getBody(), JRightPadded.Location.FOR_BODY, p);
        afterSyntax(forEachLoop, p);
        return forEachLoop;
    }

    @Override
    public J visitBlock(J.Block block, PrintOutputCapture<P> p) {
        // Check if this block has the OmitBraces marker (for objects without body)
        if (block.getMarkers().findFirst(org.openrewrite.scala.marker.OmitBraces.class).isPresent()) {
            // Don't print the block at all
            return block;
        }
        return super.visitBlock(block, p);
    }
    
    @Override
    public J visitTry(J.Try tryable, PrintOutputCapture<P> p) {
        beforeSyntax(tryable, Space.Location.TRY_PREFIX, p);
        p.append("try");
        visit(tryable.getBody(), p);
        for (J.Try.Catch c : tryable.getCatches()) {
            visit(c, p);
        }
        if (tryable.getPadding().getFinally() != null) {
            visitSpace(tryable.getPadding().getFinally().getBefore(), Space.Location.TRY_FINALLY, p);
            p.append("finally");
            visit(tryable.getFinally(), p);
        }
        afterSyntax(tryable, p);
        return tryable;
    }

    @Override
    public J visitCatch(J.Try.Catch catch_, PrintOutputCapture<P> p) {
        if (catch_.getMarkers().findFirst(ScalaCatch.class).isPresent()) {
            // Scala catch uses pattern matching: catch { case e: Exception => handler }
            visitSpace(catch_.getPrefix(), Space.Location.CATCH_PREFIX, p);
            p.append("catch");
            visit(catch_.getBody(), p);
            afterSyntax(catch_, p);
            return catch_;
        }
        return super.visitCatch(catch_, p);
    }

    @Override
    public J visitReturn(J.Return return_, PrintOutputCapture<P> p) {
        return super.visitReturn(return_, p);
    }

    public J visitExpressionStatement(S.ExpressionStatement expressionStatement, PrintOutputCapture<P> p) {
        visit(expressionStatement.getExpression(), p);
        return expressionStatement;
    }
    
    @Override
    public J visitForLoop(J.ForLoop forLoop, PrintOutputCapture<P> p) {
        // Check if this is a Scala range-based for loop
        ScalaForLoop marker = forLoop.getMarkers().findFirst(ScalaForLoop.class).orElse(null);
        if (marker != null && marker.getOriginalSource() != null && !marker.getOriginalSource().isEmpty()) {
            // Print the original Scala syntax
            beforeSyntax(forLoop, Space.Location.FOR_PREFIX, p);
            p.append(marker.getOriginalSource());
            afterSyntax(forLoop, p);
            return forLoop;
        }
        // Otherwise use Java syntax
        return super.visitForLoop(forLoop, p);
    }
    
    // Override additional methods here for Scala-specific syntax as needed

    @Override
    public J visitVariableDeclarations(J.VariableDeclarations multiVariable, PrintOutputCapture<P> p) {
        beforeSyntax(multiVariable, Space.Location.VARIABLE_DECLARATIONS_PREFIX, p);
        visit(multiVariable.getLeadingAnnotations(), p);

        // Check if this is a lambda parameter - if so, don't print val/var
        boolean isLambdaParam = multiVariable.getMarkers().findFirst(
            org.openrewrite.scala.marker.LambdaParameter.class).isPresent();
        
        // Print modifiers but handle final specially since Scala has val/var
        boolean isVal = false;
        boolean hasLazy = false;
        boolean hasModifiers = false;
        boolean hasExplicitFinal = false;
        
        for (J.Modifier m : multiVariable.getModifiers()) {
            if (m.getType() == J.Modifier.Type.Final) {
                isVal = true;
                // Check if this final modifier has an explicit keyword (not implicit)
                if (m.getKeyword() != null && "final".equals(m.getKeyword())) {
                    hasExplicitFinal = true;
                    visit(m, p);
                    hasModifiers = true;
                }
            } else if (m.getKeyword() != null && "lazy".equals(m.getKeyword())) {
                // Skip lazy here as it's already handled in the val/var printing
                hasLazy = true;
            } else {
                visit(m, p);
                hasModifiers = true;
            }
        }
        
        // Add space after modifiers if any were printed
        if (hasModifiers) {
            p.append(" ");
        }
        
        // Print lazy if present (only once)
        if (hasLazy) {
            p.append("lazy ");
        }
        
        // Print val or var (unless it's a lambda parameter)
        if (!isLambdaParam) {
            // If annotations were printed, add space before val/var
            if (!multiVariable.getLeadingAnnotations().isEmpty() && !hasModifiers && !hasLazy) {
                p.append(" ");
            }
            p.append(isVal ? "val" : "var");
        }
        
        // In Scala, variable declarations don't have a type at the declaration level
        // Each variable has its own type annotation
        
        // Visit each variable (the variable's prefix already contains the space)
        visitRightPadded(multiVariable.getPadding().getVariables(), JRightPadded.Location.NAMED_VARIABLE, ",", p);
        
        afterSyntax(multiVariable, p);
        return multiVariable;
    }
    
    @Override
    public J visitVariable(J.VariableDeclarations.NamedVariable variable, PrintOutputCapture<P> p) {
        beforeSyntax(variable, Space.Location.VARIABLE_PREFIX, p);

        // Print the variable name (or tuple pattern for destructuring)
        if (variable.getDeclarator() instanceof S.TuplePattern) {
            visit((S.TuplePattern) variable.getDeclarator(), p);
        } else {
            visit(variable.getName(), p);
        }
        
        // In Scala, type annotation comes after the name
        J.VariableDeclarations parent = getCursor().getParentOrThrow().getValue();
        if (parent.getTypeExpression() != null) {
            p.append(":");
            // The type expression should have the space after colon in its prefix
            visit(parent.getTypeExpression(), p);
            
            // If there's an initializer, use visitLeftPadded to handle it properly
            visitLeftPadded("=", variable.getPadding().getInitializer(), JLeftPadded.Location.VARIABLE_INITIALIZER, p);
        } else {
            // No type annotation, handle initializer normally
            visitLeftPadded("=", variable.getPadding().getInitializer(), JLeftPadded.Location.VARIABLE_INITIALIZER, p);
        }
        
        afterSyntax(variable, p);
        return variable;
    }
    
    @Override
    public J visitNewClass(J.NewClass newClass, PrintOutputCapture<P> p) {
        beforeSyntax(newClass, Space.Location.NEW_CLASS_PREFIX, p);
        if (newClass.getPadding().getEnclosing() != null) {
            visitRightPadded(newClass.getPadding().getEnclosing(), JRightPadded.Location.NEW_CLASS_ENCLOSING, ".", p);
        }
        p.append("new");
        // Ensure space between "new" and the class name
        if (newClass.getClazz() != null && newClass.getClazz().getPrefix().isEmpty()) {
            p.append(" ");
        }
        visit(newClass.getClazz(), p);
        // In Scala, constructors can be called without parentheses
        if (newClass.getPadding().getArguments() != null) {
            visitContainer("(", newClass.getPadding().getArguments(), JContainer.Location.NEW_CLASS_ARGUMENTS, ",", ")", p);
        }
        visit(newClass.getBody(), p);
        afterSyntax(newClass, p);
        return newClass;
    }

    @Override
    public J visitParameterizedType(J.ParameterizedType type, PrintOutputCapture<P> p) {
        beforeSyntax(type, Space.Location.PARAMETERIZED_TYPE_PREFIX, p);
        visit(type.getClazz(), p);
        
        // Use Scala-style square brackets for type parameters
        visitContainer("[", type.getPadding().getTypeParameters(), JContainer.Location.TYPE_PARAMETERS, ",", "]", p);
        
        afterSyntax(type, p);
        return type;
    }
    
    @Override
    public J visitArrayAccess(J.ArrayAccess arrayAccess, PrintOutputCapture<P> p) {
        beforeSyntax(arrayAccess, Space.Location.ARRAY_ACCESS_PREFIX, p);
        visit(arrayAccess.getIndexed(), p);
        
        // In Scala, array access uses parentheses, not square brackets
        J.ArrayDimension dimension = arrayAccess.getDimension();
        visitSpace(dimension.getPrefix(), Space.Location.DIMENSION_PREFIX, p);
        p.append('(');
        visitRightPadded(dimension.getPadding().getIndex(), JRightPadded.Location.ARRAY_INDEX, "", p);
        p.append(')');
        
        afterSyntax(arrayAccess, p);
        return arrayAccess;
    }
    
    @Override
    public J visitInstanceOf(J.InstanceOf instanceOf, PrintOutputCapture<P> p) {
        beforeSyntax(instanceOf, Space.Location.INSTANCEOF_PREFIX, p);
        
        // In Scala, instanceof is written as expression.isInstanceOf[Type]
        visitRightPadded(instanceOf.getPadding().getExpression(), JRightPadded.Location.INSTANCEOF, "", p);
        p.append(".isInstanceOf");
        
        // Extract the type and wrap in square brackets
        p.append('[');
        visit(instanceOf.getClazz(), p);
        p.append(']');
        
        afterSyntax(instanceOf, p);
        return instanceOf;
    }
    
    @Override
    public J visitNewArray(J.NewArray newArray, PrintOutputCapture<P> p) {
        beforeSyntax(newArray, Space.Location.NEW_ARRAY_PREFIX, p);
        
        // In Scala, array creation uses Array(elements) or Array[Type](elements) syntax
        p.append("Array");
        
        // Print type parameter if present
        if (newArray.getTypeExpression() != null) {
            p.append('[');
            visit(newArray.getTypeExpression(), p);
            p.append(']');
        }
        
        // If we have an initializer, print the elements
        if (newArray.getInitializer() != null) {
            visitContainer("", newArray.getPadding().getInitializer(), JContainer.Location.NEW_ARRAY_INITIALIZER, ",", "", p);
        } else {
            // Empty array
            p.append("()");
        }
        
        afterSyntax(newArray, p);
        return newArray;
    }
    
    @Override
    public J visitMethodInvocation(J.MethodInvocation method, PrintOutputCapture<P> p) {
        // Check if this is block argument syntax (e.g., synchronized { ... })
        if (method.getPadding().getArguments().getMarkers().findFirst(BlockArgument.class).isPresent()) {
            boolean colonSyntax = method.getPadding().getArguments().getMarkers()
                    .findFirst(org.openrewrite.scala.marker.ColonToken.class).isPresent();
            beforeSyntax(method, Space.Location.METHOD_INVOCATION_PREFIX, p);

            // Print select with dot if present (e.g., "lock.")
            if (method.getPadding().getSelect() != null) {
                visitRightPadded(method.getPadding().getSelect(), JRightPadded.Location.METHOD_SELECT, ".", p);
            }

            // Print method name (e.g., "synchronized")
            visit(method.getName(), p);

            if (colonSyntax) {
                p.append(":");
            }

            // Print the block argument - it's a Lambda with BlockArgument marker
            for (Expression arg : method.getArguments()) {
                if (arg instanceof J.Lambda) {
                    J.Lambda lambda = (J.Lambda) arg;
                    if (colonSyntax && lambda.getBody() instanceof J.Block) {
                        // For colon syntax, print block contents without braces
                        J.Block block = (J.Block) lambda.getBody();
                        visitRightPadded(block.getPadding().getStatements(), JRightPadded.Location.BLOCK_STATEMENT, "", p);
                        visitSpace(block.getEnd(), Space.Location.BLOCK_END, p);
                    } else {
                        visit(lambda.getBody(), p);
                    }
                } else {
                    visit(arg, p);
                }
            }

            afterSyntax(method, p);
            return method;
        }

        // Check if this is function application syntax (arr(0) instead of arr.apply(0))
        if (method.getMarkers().findFirst(org.openrewrite.scala.marker.FunctionApplication.class).isPresent()) {
            beforeSyntax(method, Space.Location.METHOD_INVOCATION_PREFIX, p);

            // Print the select (e.g., "arr" or "println")
            visitRightPadded(method.getPadding().getSelect(), JRightPadded.Location.METHOD_SELECT, "", p);

            // Print type parameters if present (e.g., [Int])
            if (method.getPadding().getTypeParameters() != null) {
                visitContainer("[", method.getPadding().getTypeParameters(), JContainer.Location.TYPE_PARAMETERS, ",", "]", p);
            }

            // Print arguments directly with parentheses (no ".apply")
            visitContainer("(", method.getPadding().getArguments(), JContainer.Location.METHOD_INVOCATION_ARGUMENTS, ",", ")", p);

            // Print additional argument lists for curried calls
            printAdditionalArgumentLists(method, p);

            afterSyntax(method, p);
            return method;
        }
        
        // Check if this is infix notation (list map func instead of list.map(func))
        if (method.getMarkers().findFirst(org.openrewrite.scala.marker.InfixNotation.class).isPresent()) {
            beforeSyntax(method, Space.Location.METHOD_INVOCATION_PREFIX, p);
            
            // Print the select (e.g., "list")
            visitRightPadded(method.getPadding().getSelect(), JRightPadded.Location.METHOD_SELECT, "", p);
            
            // Print the method name with its prefix space (e.g., " map")
            visit(method.getName(), p);
            
            // Print the arguments without parentheses, just with their prefix space
            if (method.getArguments() != null && !method.getArguments().isEmpty()) {
                for (Expression arg : method.getArguments()) {
                    visit(arg, p);
                }
            }
            
            afterSyntax(method, p);
            return method;
        }
        
        // For regular method calls, print with Scala-style type params (after method name)
        beforeSyntax(method, Space.Location.METHOD_INVOCATION_PREFIX, p);

        // Print select with dot
        if (method.getPadding().getSelect() != null) {
            visitRightPadded(method.getPadding().getSelect(), JRightPadded.Location.METHOD_SELECT, ".", p);
        }

        // Print method name
        visit(method.getName(), p);

        // Print type parameters AFTER method name (Scala-style [T] instead of Java-style <T>)
        if (method.getPadding().getTypeParameters() != null) {
            visitContainer("[", method.getPadding().getTypeParameters(), JContainer.Location.TYPE_PARAMETERS, ",", "]", p);
        }

        // Print arguments
        visitContainer("(", method.getPadding().getArguments(), JContainer.Location.METHOD_INVOCATION_ARGUMENTS, ",", ")", p);

        // Print additional argument lists for curried calls
        printAdditionalArgumentLists(method, p);

        afterSyntax(method, p);
        return method;
    }

    private void printAdditionalArgumentLists(J.MethodInvocation method, PrintOutputCapture<P> p) {
        method.getMarkers().findFirst(org.openrewrite.scala.marker.AdditionalArgumentLists.class)
                .ifPresent(aal -> {
                    for (JContainer<Expression> argList : aal.getArgumentLists()) {
                        visitSpace(argList.getBefore(), JContainer.Location.METHOD_INVOCATION_ARGUMENTS.getBeforeLocation(), p);
                        p.append("(");
                        List<JRightPadded<Expression>> elems = argList.getPadding().getElements();
                        for (int j = 0; j < elems.size(); j++) {
                            JRightPadded<Expression> elem = elems.get(j);
                            visit(elem.getElement(), p);
                            visitSpace(elem.getAfter(), JRightPadded.Location.METHOD_INVOCATION_ARGUMENT.getAfterLocation(), p);
                            if (j < elems.size() - 1) {
                                p.append(",");
                            }
                        }
                        p.append(")");
                    }
                });
    }
    
    @Override
    public J visitMemberReference(J.MemberReference memberRef, PrintOutputCapture<P> p) {
        beforeSyntax(memberRef, Space.Location.MEMBER_REFERENCE_PREFIX, p);
        
        // Print the containing object
        visitRightPadded(memberRef.getPadding().getContaining(), JRightPadded.Location.MEMBER_REFERENCE_CONTAINING, p);
        
        // In Scala, member references use space + underscore instead of ::
        // e.g., "greet _" instead of "greet::apply"
        visit(memberRef.getPadding().getReference().getElement(), p);
        
        afterSyntax(memberRef, p);
        return memberRef;
    }
    
    public J visitLambda(J.Lambda lambda, PrintOutputCapture<P> p) {
        beforeSyntax(lambda, Space.Location.LAMBDA_PREFIX, p);
        
        // Check if this is an underscore placeholder lambda
        if (lambda.getMarkers().findFirst(UnderscorePlaceholderLambda.class).isPresent()) {
            // For underscore placeholder lambdas, just print the body
            // The underscores in the body will be printed as S.Wildcard
            visit(lambda.getBody(), p);
            afterSyntax(lambda, p);
            return lambda;
        }
        
        // Print lambda parameters
        J.Lambda.Parameters params = lambda.getParameters();
        visitSpace(params.getPrefix(), Space.Location.LAMBDA_PARAMETERS_PREFIX, p);
        
        if (params.isParenthesized()) {
            p.append('(');
        }
        
        visitRightPadded(params.getPadding().getParameters(), JRightPadded.Location.LAMBDA_PARAM, ",", p);
        
        if (params.isParenthesized()) {
            p.append(')');
        }
        
        // Print arrow with spacing
        visitSpace(lambda.getArrow(), Space.Location.LAMBDA_ARROW_PREFIX, p);
        p.append("=>");
        
        // Print lambda body
        visit(lambda.getBody(), p);
        
        afterSyntax(lambda, p);
        return lambda;
    }

    public J visitTuplePattern(S.TuplePattern tuplePattern, PrintOutputCapture<P> p) {
        beforeSyntax(tuplePattern, Space.Location.LANGUAGE_EXTENSION, p);
        p.append('(');
        visitContainer("", tuplePattern.getPadding().getElements(), JContainer.Location.LANGUAGE_EXTENSION, ",", "", p);
        p.append(')');
        afterSyntax(tuplePattern, p);
        return tuplePattern;
    }

    public J visitWildcard(S.Wildcard wildcard, PrintOutputCapture<P> p) {
        beforeSyntax(wildcard, Space.Location.LANGUAGE_EXTENSION, p);
        p.append('_');
        afterSyntax(wildcard, p);
        return wildcard;
    }

    public J visitBlockExpression(S.BlockExpression blockExpression, PrintOutputCapture<P> p) {
        beforeSyntax(blockExpression, Space.Location.LANGUAGE_EXTENSION, p);
        // Simply visit the contained block - it will print itself with braces
        visit(blockExpression.getBlock(), p);
        afterSyntax(blockExpression, p);
        return blockExpression;
    }

    public J visitInterpolatedString(S.InterpolatedString interpolatedString, PrintOutputCapture<P> p) {
        beforeSyntax(interpolatedString, Space.Location.LANGUAGE_EXTENSION, p);
        p.append(interpolatedString.getInterpolator());
        p.append('"');
        for (J part : interpolatedString.getParts()) {
            if (part instanceof J.Literal) {
                // String literal segment - print raw value without quotes
                J.Literal lit = (J.Literal) part;
                if (lit.getValue() != null) {
                    p.append(lit.getValue().toString());
                }
            } else if (part instanceof J.Unknown) {
                // Interpolated expression - print as-is from source
                J.Unknown unknown = (J.Unknown) part;
                p.append(unknown.getSource().getText());
            }
        }
        p.append('"');
        afterSyntax(interpolatedString, p);
        return interpolatedString;
    }

    public J visitMatchExpression(S.MatchExpression matchExpression, PrintOutputCapture<P> p) {
        beforeSyntax(matchExpression, Space.Location.LANGUAGE_EXTENSION, p);
        visit(matchExpression.getSelector(), p);
        visitSpace(matchExpression.getBeforeBrace(), Space.Location.LANGUAGE_EXTENSION, p);
        p.append("match");
        p.append(" {");
        for (S.CaseClause caseClause : matchExpression.getCases()) {
            visit(caseClause, p);
        }
        visitSpace(matchExpression.getEndSpace(), Space.Location.LANGUAGE_EXTENSION, p);
        p.append('}');
        afterSyntax(matchExpression, p);
        return matchExpression;
    }

    public J visitCaseClause(S.CaseClause caseClause, PrintOutputCapture<P> p) {
        beforeSyntax(caseClause, Space.Location.LANGUAGE_EXTENSION, p);
        p.append("case ");
        visit(caseClause.getPattern(), p);
        if (caseClause.getGuard() != null) {
            visitSpace(caseClause.getGuard().getBefore(), Space.Location.LANGUAGE_EXTENSION, p);
            p.append("if");
            visit(caseClause.getGuard().getElement(), p);
        }
        visitSpace(caseClause.getArrow(), Space.Location.LANGUAGE_EXTENSION, p);
        p.append("=>");
        for (JRightPadded<Statement> stmt : caseClause.getBody()) {
            visit(stmt.getElement(), p);
            visitSpace(stmt.getAfter(), Space.Location.LANGUAGE_EXTENSION, p);
        }
        afterSyntax(caseClause, p);
        return caseClause;
    }

    public J visitByNameType(S.ByNameType byNameType, PrintOutputCapture<P> p) {
        beforeSyntax(byNameType, Space.Location.LANGUAGE_EXTENSION, p);
        p.append("=>");
        visit(byNameType.getTypeTree(), p);
        afterSyntax(byNameType, p);
        return byNameType;
    }

    public J visitTypeAlias(S.TypeAlias typeAlias, PrintOutputCapture<P> p) {
        beforeSyntax(typeAlias, Space.Location.LANGUAGE_EXTENSION, p);
        for (J.Modifier mod : typeAlias.getModifiers()) {
            visit(mod, p);
        }
        visitSpace(typeAlias.getTypeKeyword(), Space.Location.LANGUAGE_EXTENSION, p);
        p.append("type");
        visit(typeAlias.getName(), p);
        if (typeAlias.getPadding().getTypeParameters() != null) {
            visitContainer("[", typeAlias.getPadding().getTypeParameters(),
                    JContainer.Location.TYPE_PARAMETERS, ",", "]", p);
        }
        if (typeAlias.getPadding().getInitializer() != null) {
            visitSpace(typeAlias.getPadding().getInitializer().getBefore(), Space.Location.LANGUAGE_EXTENSION, p);
            p.append("=");
            visit(typeAlias.getPadding().getInitializer().getElement(), p);
        }
        afterSyntax(typeAlias, p);
        return typeAlias;
    }

    public J visitTypeAscription(S.TypeAscription typeAscription, PrintOutputCapture<P> p) {
        beforeSyntax(typeAscription, Space.Location.LANGUAGE_EXTENSION, p);
        visit(typeAscription.getExpression(), p);
        p.append(":");
        visit(typeAscription.getTypeTree(), p);
        afterSyntax(typeAscription, p);
        return typeAscription;
    }

    public J visitFunctionType(S.FunctionType functionType, PrintOutputCapture<P> p) {
        beforeSyntax(functionType, Space.Location.LANGUAGE_EXTENSION, p);
        List<TypeTree> params = functionType.getParameters();
        boolean parenthesized = params.size() != 1 ||
                functionType.getPadding().getParameters().getBefore() != Space.EMPTY ||
                (params.size() == 1 && (params.get(0) instanceof S.InfixType ||
                        params.get(0) instanceof S.FunctionType));
        if (parenthesized) {
            visitSpace(functionType.getPadding().getParameters().getBefore(), Space.Location.LANGUAGE_EXTENSION, p);
            p.append("(");
            visitRightPadded(functionType.getPadding().getParameters().getPadding().getElements(),
                    JRightPadded.Location.LANGUAGE_EXTENSION, ",", p);
            p.append(")");
        } else {
            visit(params.get(0), p);
        }
        visitSpace(functionType.getArrow(), Space.Location.LANGUAGE_EXTENSION, p);
        p.append("=>");
        visit(functionType.getReturnType(), p);
        afterSyntax(functionType, p);
        return functionType;
    }

    public J visitEnumCase(S.EnumCase enumCase, PrintOutputCapture<P> p) {
        beforeSyntax(enumCase, Space.Location.LANGUAGE_EXTENSION, p);
        p.append("case");
        visit(enumCase.getName(), p);
        if (enumCase.getPadding().getExtending() != null) {
            visitSpace(enumCase.getPadding().getExtending().getBefore(), Space.Location.LANGUAGE_EXTENSION, p);
            p.append("extends");
            visit(enumCase.getPadding().getExtending().getElement(), p);
        }
        if (enumCase.getPadding().getArguments() != null) {
            visitContainer("(", enumCase.getPadding().getArguments(), JContainer.Location.LANGUAGE_EXTENSION, ",", ")", p);
        }
        afterSyntax(enumCase, p);
        return enumCase;
    }

    public J visitInfixType(S.InfixType infixType, PrintOutputCapture<P> p) {
        beforeSyntax(infixType, Space.Location.LANGUAGE_EXTENSION, p);
        visit(infixType.getLeft(), p);
        visitSpace(infixType.getPadding().getOperator().getBefore(), Space.Location.LANGUAGE_EXTENSION, p);
        p.append(infixType.getPadding().getOperator().getElement().getSimpleName());
        visit(infixType.getRight(), p);
        afterSyntax(infixType, p);
        return infixType;
    }
}