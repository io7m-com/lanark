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

public final class RDottedNameTests
{
  @TestFactory
  public Stream<DynamicTest> testInvalid()
  {
    return Stream.of(
        " ",
        "a.b.c.d.e.f.g.h.i.a.b.c.d.e.f.g.h.i")
      .map(s -> {
        return DynamicTest.dynamicTest("testInvalid_" + s, () -> {
          assertThrows(IllegalArgumentException.class, () -> {
            new RDottedName(s);
          });
        });
      });
  }

  @Property
  public void testNameLength(
    final @ForAll RDottedName name)
  {
    assertTrue(name.value().length() <= 1024);
  }

  @Property
  public void testSegmentsIdentity(
    final @ForAll RDottedName name)
  {
    assertEquals(
      name,
      RDottedName.ofSegments(name.segments())
    );
  }

  @Property
  public void testCompareEqual(
    final @ForAll RDottedName name)
  {
    assertEquals(
      0,
      name.compareTo(name)
    );
  }

  @Property
  public void testToStringIdentity(
    final @ForAll RDottedName name)
  {
    assertEquals(
      name,
      new RDottedName(name.toString())
    );
  }

  @Property
  public void testPrefixRelation(
    final @ForAll RDottedName name)
  {
    final var segments =
      name.segments();
    final var prefix =
      RDottedPrefix.ofSegments(segments);

    assertEquals(
      name,
      RDottedName.ofSegments(prefix.segments())
    );
  }
}
