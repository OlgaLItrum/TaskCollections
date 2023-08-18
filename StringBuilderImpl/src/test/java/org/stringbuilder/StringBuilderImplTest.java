package org.stringbuilder;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;



import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

class StringBuilderImplTest {

  private StringBuilderImpl builder;
  private final CharSequence sequence = "CharSequence";
  private int indexStart;
  private int indexEnd;

  @BeforeEach
  void setUp() {
    builder = new StringBuilderImpl();
    indexStart = 4;
    indexEnd = 8;
  }

  @Test
  void append_ByCharSequence_ReturnsStringBuilderWithAppendedCharSequence() {
  builder.append(sequence);
  assertEquals(sequence.toString(), builder.toString());
  }

  @Test
  void append_ByCharSequenceAndIndexStartEnd_ReturnsStringBuilderWithAppendedSubSequence() {
    CharSequence expected = sequence.subSequence(indexStart, indexEnd);
    builder.append(sequence, indexStart, indexEnd);
    assertEquals(expected.toString(), builder.toString());
  }

  @Test
  void append_ByChar_ReturnsStringBuilderWithAppendedChar() {
    char character = 'a';
    builder.append(character);
    assertEquals(Character.toString(character), builder.toString());
  }

  @Test
  void codePointAt_ByIndex_ResultsCodePointElement() {
    fail();
  }

  @Test
  void append_ByArrayCharIndexOffsetAndLen_ResultsStringBuilderWithArrayChar() {
    char[] array = {'d','w','o','a', '[', '2'};
    builder.append(array);
    assertEquals("dwoa[2", builder.toString());
  }

  @Test
  void appendCodePoint_ByCodePoint_ResultsStringBuilderWithChar() {
    builder.appendCodePoint(115);
    assertEquals("s", builder.toString());
  }

  @Test
  void testAppend5() {
    builder.append(sequence);
    String expected = "Sequence";
    assertEquals(expected, builder.substring(4));
  }

  @Test
  void undo() {
  }

  @Test
  void capacity() {
  }

  @Test
  void trimToSize() {
  }

  @Test
  void setLength() {
  }

  @Test
  void codePointAt() {
  }

  @Test
  void codePointCount() {
  }

  @Test
  void offsetByCodePoints() {
  }

  @Test
  void getChars() {
  }

  @Test
  void setCharAt() {
  }

  @Test
  void testAppend6() {
  }

  @Test
  void testAppend7() {
  }

  @Test
  void testAppend8() {
  }

  @Test
  void testAppend9() {
  }

  @Test
  void testAppend10() {
  }

  @Test
  void testAppend11() {
  }

  @Test
  void delete() {
  }

  @Test
  void appendCodePoint() {
  }

  @Test
  void deleteCharAt() {
  }

  @Test
  void replace() {
  }

  @Test
  void substring() {
  }

  @Test
  void length() {
  }

  @Test
  void charAt() {
  }

  @Test
  void isEmpty() {
  }

  @Test
  void subSequence() {
  }

  @Test
  void testSubstring() {
  }

  @Test
  void insert() {
  }

  @Test
  void testInsert() {
  }

  @Test
  void testInsert1() {
  }

  @Test
  void testInsert2() {
  }

  @Test
  void testInsert3() {
  }

  @Test
  void testInsert4() {
  }

  @Test
  void testInsert5() {
  }

  @Test
  void testInsert6() {
  }

  @Test
  void testInsert7() {
  }

  @Test
  void testInsert8() {
  }

  @Test
  void testInsert9() {
  }

  @Test
  void testInsert10() {
  }

  @Test
  void indexOf() {
  }

  @Test
  void testIndexOf() {
  }

  @Test
  void lastIndexOf() {
  }

  @Test
  void testLastIndexOf() {
  }

  @Test
  void reverse() {
  }

  @Test
  void testToString() {
  }

  @Test
  void chars() {
  }

  @Test
  void codePoints() {
  }
}