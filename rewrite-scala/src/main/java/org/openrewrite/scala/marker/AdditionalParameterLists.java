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
package org.openrewrite.scala.marker;

import org.openrewrite.java.tree.JContainer;
import org.openrewrite.java.tree.Statement;
import org.openrewrite.marker.Marker;

import java.util.List;
import java.util.UUID;

/**
 * Marks a J.MethodDeclaration to indicate it has additional parameter lists
 * beyond the first one. Scala supports multiple parameter lists (currying),
 * e.g. {@code def fold[B](z: B)(op: (B, A) => B): B}.
 * The first parameter list is stored in J.MethodDeclaration.parameters as usual,
 * and additional lists are stored in this marker.
 */
public class AdditionalParameterLists implements Marker {
    private final UUID id;
    private final List<JContainer<Statement>> parameterLists;

    public AdditionalParameterLists(UUID id, List<JContainer<Statement>> parameterLists) {
        this.id = id;
        this.parameterLists = parameterLists;
    }

    @Override
    public UUID getId() {
        return id;
    }

    public List<JContainer<Statement>> getParameterLists() {
        return parameterLists;
    }

    @Override
    public AdditionalParameterLists withId(UUID id) {
        return new AdditionalParameterLists(id, parameterLists);
    }

    public AdditionalParameterLists withParameterLists(List<JContainer<Statement>> parameterLists) {
        return new AdditionalParameterLists(id, parameterLists);
    }
}
