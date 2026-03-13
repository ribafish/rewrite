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

import org.openrewrite.java.tree.Expression;
import org.openrewrite.java.tree.JContainer;
import org.openrewrite.marker.Marker;

import java.util.List;
import java.util.UUID;

/**
 * Marks a J.MethodInvocation to indicate it has additional argument lists
 * beyond the first one. Scala supports curried function calls,
 * e.g. {@code add(1)(2)} or {@code fold(0)(_ + _)}.
 * The first argument list is stored in J.MethodInvocation.arguments as usual,
 * and additional lists are stored in this marker.
 */
public class AdditionalArgumentLists implements Marker {
    private final UUID id;
    private final List<JContainer<Expression>> argumentLists;

    public AdditionalArgumentLists(UUID id, List<JContainer<Expression>> argumentLists) {
        this.id = id;
        this.argumentLists = argumentLists;
    }

    @Override
    public UUID getId() {
        return id;
    }

    public List<JContainer<Expression>> getArgumentLists() {
        return argumentLists;
    }

    @Override
    public AdditionalArgumentLists withId(UUID id) {
        return new AdditionalArgumentLists(id, argumentLists);
    }

    public AdditionalArgumentLists withArgumentLists(List<JContainer<Expression>> argumentLists) {
        return new AdditionalArgumentLists(id, argumentLists);
    }
}
