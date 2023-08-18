package org.stringbuilder;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

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
  void codePointAt_ByIndex_ReturnsCodePointElement() {
    fail();
  }

  @Test
  void append_ByArrayCharIndexOffsetAndLen_ReturnsStringBuilderWithArrayChar() {
    char[] array = {'d', 'w', 'o', 'a', '[', '2'};
    builder.append(array, 2, 4);
    assertEquals("oa[2", builder.toString());
  }

  @Test
  void appendCodePoint_ByCodePoint_ReturnsStringBuilderWithChar() {
    builder.appendCodePoint(115);
    assertEquals("s", builder.toString());
  }

  @Test
  void append_ByBoolean_ReturnsStringBuilderWithBoolean() {
    builder.append(true);
    assertEquals("true", builder.toString());
  }

  @Test
  void append_ByInt_ReturnsStringBuilderWithInt() {
    int number = 10;
    builder.append(number);
    assertEquals(String.valueOf(number), builder.toString());
  }

  @Test
  void append_ByDouble_ReturnsStringBuilderWithDouble() {
    double number = 10.20;
    builder.append(number);
    assertEquals(String.valueOf(number), builder.toString());
  }

  @Test
  void append_ByString_ReturnsStringBuilderWithString() {
    String str1 = "String1";
    String str2 = "String2";
    builder.append(str1);
    builder.append(str2);
    assertEquals(str1 + str2, builder.toString());
  }

  @Test
  void append_ByObject_ReturnsStringBuilderWithObject() {
    Object obj = new Object();
    builder.append(obj);
    assertEquals(obj.toString(), builder.toString());
  }

  @Test
  void append_ByStringBuffer_ReturnsStringBuilderWithStringBuffer() {
    StringBuffer buffer = new StringBuffer();
    buffer.append(sequence);
    builder.append(sequence);
    builder.append(buffer);
    assertEquals(buffer.toString() + buffer.toString(), builder.toString());
  }

  @Test
  void append_ByArrayChar_ReturnsStringBuilderWithArrayChar() {
    char[] array = {'d', 'w', 'o', 'a', '[', '2'};
    builder.append(array);
    assertEquals("dwoa[2", builder.toString());
  }

  @Test
  void append_ByLong_ReturnsStringBuilderWithLong() {
    long number = Math.abs(3000);
    builder.append(number);
    assertEquals(Long.toString(number), builder.toString());
  }

  @Test
  void append_ByFloat_ReturnsStringBuilderWithFloat() {
    float number = 8.5F;
    builder.append(number);
    assertEquals(Float.toString(number), builder.toString());
  }

  @Test
  void setLength_ByNewLength_SetsNewLength() {
    int newLength = 6;
    builder.append(sequence).setLength(newLength);
    CharSequence newSequence = sequence.subSequence(0, 6);

    assertEquals(newSequence.toString(), builder.toString());
    assertEquals(newSequence.length(), builder.length());
  }

  @Test
  void subString_ByIndexStart_ReturnsString() {
    fail();
  }

  @Test
  void undo_SetUndoValue_IfUndoValueIsExist() {
    builder.append(sequence);
    builder.append(indexEnd);
    builder.append(indexStart);
    builder.undo();
    builder.undo();
    assertEquals(sequence.toString(), builder.toString());
  }

  @Test
  void undo_ThrowNoSuchElementException_IfUndoValueIsNotExist() {
    builder.append(indexEnd);
    builder.undo();

    assertEquals(String.valueOf(indexEnd), builder.toString());
    assertThrows(NoSuchElementException.class, () -> builder.undo());
  }

  @Test
  void chars_ReturnsIntStream() {
    int[] extended = Stream.of('S', 'e', 'q', 'u', 'e', 'n', 'c', 'e').mapToInt(c -> (int) c).toArray();
    builder.append(sequence, 4, 12);
    assertEquals(Arrays.stream(extended).count(), builder.length());
    int[] builderInt = builder.chars().toArray();
    assertArrayEquals(extended, builderInt);
  }

  @Test
  void codePointAt_ReturnsIntStream() {
    String str = "Sequence";
    builder.append(sequence, 4, 12);
    assertArrayEquals(str.codePoints().toArray(), builder.codePoints().toArray());
  }

  @Test
  void reverse_ReturnsStringBuilder() {
    StringBuilder stringBuilder = new StringBuilder().append(sequence).reverse();
    builder.append(sequence);

    assertEquals(stringBuilder.toString(), builder.reverse().toString());
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