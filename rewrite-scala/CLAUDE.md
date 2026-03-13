# rewrite-scala

Scala language support for OpenRewrite, following the same pattern as rewrite-kotlin and rewrite-groovy.

## Active Development Plans

- **Scala Language Support**: See [Scala.md](../Scala.md) for the implementation plan and progress tracking.
- **Language Support Documentation**: As we implement Scala support, we're documenting the process in [Contributing Additional Language Support](../rewrite-docs/docs/authoring-recipes/contributing-language-support.md). This guide should be continuously updated with lessons learned during implementation.

## CRITICAL PRINCIPLES - NEVER VIOLATE THESE

### Never Regress from Rich Types to J.Unknown
**ABSOLUTE RULE**: Once a syntax element has been mapped to a rich type (J.* or S.*), NEVER revert it back to J.Unknown. This is a fundamental architectural principle. J.Unknown should only be used for:
1. Syntax we haven't implemented yet
2. Temporary placeholders during initial development
3. Truly unparseable or corrupted code

If you find yourself wanting to use J.Unknown for something already mapped, you're doing it wrong. Instead:
- Create a new S.* type if needed
- Use markers to preserve special behavior
- Extend existing J.* types with Scala-specific markers
- Find a way to map it to existing rich types

Going back to J.Unknown breaks type safety, loses semantic information, and makes the AST less useful for recipes.

## Architecture

- `S` interface extends `J` (Java's LST interface)
- Reuses common JVM constructs from the J model
- Adds Scala-specific constructs (pattern matching, traits, implicits, etc.) to the S interface
- Uses Scala 3 (Dotty) compiler for parsing
- LST model classes are implemented in Java (not Scala), following the K.java / G.java pattern

## Key Files

- `src/main/java/org/openrewrite/scala/tree/S.java` - Scala-specific AST types
- `src/main/java/org/openrewrite/scala/ScalaParserVisitor.java` - Bridges Scala compiler AST to LST
- `src/main/java/org/openrewrite/scala/ScalaPrinter.java` - LST to source code
- `src/main/java/org/openrewrite/scala/ScalaVisitor.java` - Base visitor
- `src/main/scala/org/openrewrite/scala/ScalaTreeVisitor.scala` - Core tree traversal

## Build Commands

```bash
./gradlew :rewrite-scala:assemble    # Compile only
./gradlew :rewrite-scala:test        # Run tests
```
