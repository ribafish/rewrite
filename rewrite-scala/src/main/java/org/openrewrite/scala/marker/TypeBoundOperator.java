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

import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * Indicates the type bound operator used in Scala type parameters.
 * Scala uses {@code <:} for upper bounds and {@code >:} for lower bounds,
 * compared to Java's {@code extends} and {@code super}.
 */
public class TypeBoundOperator implements Marker {
    private final UUID id;
    private final String operator;

    public TypeBoundOperator(UUID id, String operator) {
        this.id = id;
        this.operator = operator;
    }

    @Override
    public UUID getId() {
        return id;
    }

    public String getOperator() {
        return operator;
    }

    @Override
    public TypeBoundOperator withId(UUID id) {
        return new TypeBoundOperator(id, operator);
    }
}
