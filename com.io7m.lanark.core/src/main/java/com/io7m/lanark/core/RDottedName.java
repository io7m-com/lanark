/*
 * Copyright © 2022 Mark Raynsford <code@io7m.com> https://www.io7m.com
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

package com.io7m.lanark.core;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;

/**
 * A standard restricted dotted name.
 *
 * @param value The name
 */

public record RDottedName(String value)
  implements Comparable<RDottedName>
{
  /**
   * A standard restricted dotted name.
   *
   * @param value The name
   */

  public RDottedName
  {
    Objects.requireNonNull(value, "value");

    final var pattern = RDottedNamePatterns.dottedName();
    if (!pattern.matcher(value).matches()) {
      throw new IllegalArgumentException(
        "Name '%s' must match %s".formatted(value, pattern)
      );
    }
  }

  @Override
  public String toString()
  {
    return this.value;
  }

  /**
   * @return The name as a list of segments
   */

  public List<String> segments()
  {
    return List.of(this.value.split("\\."));
  }

  /**
   * Construct a name from a list of segments.
   *
   * @param segments The segments
   *
   * @return A name
   */

  public static RDottedName ofSegments(
    final List<String> segments)
  {
    return new RDottedName(String.join(".", segments));
  }

  @Override
  public int compareTo(
    final RDottedName o)
  {
    return Comparator.comparing(RDottedName::value).compare(this, o);
  }
}
