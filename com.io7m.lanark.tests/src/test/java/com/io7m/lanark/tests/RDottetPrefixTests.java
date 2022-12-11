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

package com.io7m.lanark.tests;

import com.io7m.lanark.core.RDottedName;
import com.io7m.lanark.core.RDottedPrefix;
import net.jqwik.api.ForAll;
import net.jqwik.api.Property;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class RDottetPrefixTests
{
  @TestFactory
  public Stream<DynamicTest> testInvalid()
  {
    return Stream.of(
      "a",
      "a.b.c.d.e.f.g.h.i.a.b.c.d.e.f.g.h.i."
      ).map(s -> {
      return DynamicTest.dynamicTest("testInvalid_" + s, () -> {
        assertThrows(IllegalArgumentException.class, () -> {
          new RDottedPrefix(s);
        });
      });
    });
  }

  @Property
  public void testPrefixLength(
    final @ForAll RDottedPrefix prefix)
  {
    assertTrue(prefix.value().length() <= 1024);
  }

  @Property
  public void testSegmentsIdentity(
    final @ForAll RDottedPrefix name)
  {
    assertEquals(
      name,
      RDottedPrefix.ofSegments(name.segments())
    );
  }

  @Property
  public void testCompareEqual(
    final @ForAll RDottedPrefix name)
  {
    assertEquals(
      0,
      name.compareTo(name)
    );
  }

  @Property
  public void testToStringIdentity(
    final @ForAll RDottedPrefix name)
  {
    assertEquals(
      name,
      new RDottedPrefix(name.toString())
    );
  }

  @Property
  public void testNameRelation(
    final @ForAll RDottedPrefix prefix)
  {
    final var segments =
      prefix.segments();
    final var name =
      RDottedName.ofSegments(segments);

    assertEquals(
      prefix,
      RDottedPrefix.ofSegments(name.segments())
    );
  }
}
