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


package com.io7m.lanark.arbitraries;

import com.io7m.lanark.core.RDottedPrefix;
import net.jqwik.api.Arbitraries;
import net.jqwik.api.Arbitrary;
import net.jqwik.api.Combinators;
import net.jqwik.api.providers.ArbitraryProvider;
import net.jqwik.api.providers.TypeUsage;

import java.util.Set;

/**
 * Arbitrary restricted dotted prefixes.
 */

public final class RArbDottedPrefixes implements ArbitraryProvider
{
  /**
   * Arbitrary restricted dotted prefixes.
   */

  public RArbDottedPrefixes()
  {

  }

  @Override
  public boolean canProvideFor(
    final TypeUsage targetType)
  {
    return targetType.isOfType(RDottedPrefix.class);
  }

  @Override
  public Set<Arbitrary<?>> provideFor(
    final TypeUsage targetType,
    final SubtypeProvider subtypeProvider)
  {
    final var starts =
      Arbitraries.chars()
        .with("abcdefghijklmnopqrstuvwxyz");

    final var primarySegment =
      Arbitraries.strings()
        .ofMinLength(0)
        .ofMaxLength(63)
        .withChars("abcdefghijklmnopqrstuvwxyz1234567890-_")
        .map(s -> starts.sample() + s);

    final var secondarySegment =
      Arbitraries.strings()
        .ofMinLength(0)
        .ofMaxLength(62)
        .withChars("abcdefghijklmnopqrstuvwxyz1234567890-_")
        .map(s -> starts.sample() + s);

    final var secondarySegments =
      secondarySegment.list()
        .ofMinSize(0)
        .ofMaxSize(15)
        .map(xs -> {
          return String.join(".", xs);
        });

    return Set.of(
      Combinators.combine(primarySegment, secondarySegments)
        .as((start, end) -> {
          if (end.isEmpty()) {
            return new RDottedPrefix("%s.".formatted(start));
          } else {
            return new RDottedPrefix("%s.%s.".formatted(start, end));
          }
        })
    );
  }
}
