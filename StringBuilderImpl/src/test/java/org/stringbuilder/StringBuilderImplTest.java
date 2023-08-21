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
  void undo_SetUndoValue_IfUndoValueIsExist() {
    builder.append(sequence);
    builder.append(indexEnd);
    builder.append(indexStart);
    builder.undo();
    builder.undo();
    assertEquals(sequence.toString(), builder.toString());
  }

  @Test
  void undo_ThrowsNoSuchElementException_IfUndoValueIsNotExist() {
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
  void codePoints_ReturnsIntStream() {
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
  void codePointAt_ByIndex_ReturnsIntCodePointChar() {
    builder.append(sequence);
    int expected = Character.codePointAt("S", 0);
    int action = builder.codePointAt(indexStart);
    assertEquals(expected, action);
  }

  @Test
  void codePointAt_ByIndex_ReturnsIntCodePointNumber() {
    CharSequence number = "100";
    builder.append(number);
    assertEquals(Character.codePointAt(number, 0), builder.codePointAt(0));
  }

  @Test
  void codePointAt_ByIndex_ThrowsIndexOutOfBoundsException_IfIndexIsOutOfBounds() {
    builder.append(sequence);
    assertThrows(IndexOutOfBoundsException.class, () -> builder.codePointAt(-1));
  }

  @Test
  void charAt_ByIndex_ReturnsCharByIndex() {
    builder.append(926439);
    CharSequence seq = "926439";
    char expected = seq.charAt(3);
    assertEquals(expected, builder.charAt(3));
  }

  @Test
  void charAt_ByIndex_ThrowsIndexOutOfBoundsException_IfIndexIsOutOfBounds() {
    builder.append(926439);
    assertThrows(IndexOutOfBoundsException.class, () -> builder.charAt(20));
  }

  @Test
  void deleteCharAt_ByIndex_ReturnsStringBuilderWithoutChar() {
    builder.append(sequence);
    String expected = "Charequence";
    String action = builder.deleteCharAt(indexStart).toString();
    assertEquals(expected, action);
  }

  @Test
  void deleteCharAt_ByIndex_ReturnsStringBuilderWithoutNumber() {
    builder.append(1234567);
    String expected = "123467";
    String action = builder.deleteCharAt(indexStart).toString();
    assertEquals(expected, action);
  }

  @Test
  void deleteCharAt_ByIndex_ThrowsIndexOutOfBoundsException_IfIndexIsOutOfBounds() {
    assertThrows(IndexOutOfBoundsException.class, () -> builder.append(sequence).deleteCharAt(100));
  }

  @Test
  void subString_ByIndexStart_ReturnsString() {
    char[] array = new char[10];
    new StringBuilderImpl().append(1023048540).getChars(0, 4, array, 0);
  }

  @Test
  void getChars_ByIntSrcBeginSrcEndCharArrayDstIntDstBegin_SetCharArray(){
    char[] dst = new char[indexEnd - indexStart];
    builder.append(sequence);
    builder.getChars(indexStart, indexEnd, dst, 0);
    char[] expectedSequence = sequence.subSequence(indexStart, indexEnd).toString().toCharArray();
    assertEquals(Arrays.toString(expectedSequence), Arrays.toString(dst));
  }

  @Test
  void offsetByCodePoints() {
    fail();
  }


  @Test
  void setCharAt() {
    builder.append(92344).append(sequence);
    builder.setCharAt(2, 'c');
    char a = 'a';
    char num = '1';
    var b = Character.isLetter(a);
    var c = Character.isLetter(num);
  }

  @Test
  void delete() {
    fail();
  }

  @Test
  void replace() {
    fail();
  }

  @Test
  void substring() {
    fail();
  }

  @Test
  void length() {
    fail();
  }

  @Test
  void subSequence() {
    fail();
  }

  @Test
  void testSubstring() {
    fail();
  }

  @Test
  void insert() {
    fail();
  }

  @Test
  void testInsert() {
    fail();
  }

  @Test
  void testInsert1() {
    fail();
  }

  @Test
  void testInsert2() {
    fail();
  }

  @Test
  void testInsert3() {
    fail();
  }

  @Test
  void testInsert4() {
    fail();
  }

  @Test
  void testInsert5() {
    fail();
  }

  @Test
  void testInsert6() {
    fail();
  }

  @Test
  void testInsert7() {
    fail();
  }

  @Test
  void testInsert8() {
    fail();
  }

  @Test
  void testInsert9() {
    fail();
  }

  @Test
  void testInsert10() {
    fail();
  }

  @Test
  void indexOf() {
    fail();
  }

  @Test
  void testIndexOf() {
    fail();
  }

  @Test
  void lastIndexOf() {
    fail();
  }

  @Test
  void testLastIndexOf() {
    fail();
  }

  @Test
  void testToString() {
    fail();
  }

}