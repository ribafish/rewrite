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

import org.openrewrite.java.tree.Space;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * Marks a method body to indicate it uses Scala's {@code = expression} syntax.
 * Preserves spacing around the {@code =} sign.
 */
public class MethodBody implements Marker {
    private final UUID id;
    private final Space beforeEquals;

    public MethodBody(UUID id, Space beforeEquals) {
        this.id = id;
        this.beforeEquals = beforeEquals;
    }

    @Override
    public UUID getId() {
        return id;
    }

    public Space getBeforeEquals() {
        return beforeEquals;
    }

    @Override
    public MethodBody withId(UUID id) {
        return new MethodBody(id, beforeEquals);
    }

    public MethodBody withBeforeEquals(Space beforeEquals) {
        return new MethodBody(id, beforeEquals);
    }
}
