package org.stringbuilder;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class StringBuilderImpl implements StringBuilderInterface {
  private static final int DEFAULT_CAPACITY_FOR_BYTES = 10;
  private static final byte[] EMPTY_BYTES = new byte[DEFAULT_CAPACITY_FOR_BYTES];

  private final ArrayDeque<byte[]> changes;
  private byte[] bytes;

  private int capacity;

  private int size;

  {
    size = 0;
    capacity = DEFAULT_CAPACITY_FOR_BYTES;
    changes = new ArrayDeque<>();
  }

  public StringBuilderImpl() {
    bytes = EMPTY_BYTES;
  }

  public StringBuilderImpl(int initialCapacity) {
    bytes = new byte[initialCapacity];
  }


  private void addChanges() {
    changes.add(Arrays.copyOf(bytes, size));
  }

  public StringBuilderImpl(String string) {
    createNewBytes(getNeededCapacity(string), string);
    addChanges();
  }

  @Override
  public StringBuilderImpl append(CharSequence csq) throws IOException {
    if (!csq.isEmpty())
      append(Stream.of(csq).collect(Collectors.joining()));
    return this;
  }

  @Override
  public Appendable append(CharSequence csq, int start, int end) throws IOException {
    return null;
  }

  public StringBuilderImpl append(char character) {
    if (isFill()) {
      createNewBytes(
        getNeededCapacity(Character.toString(character)));
    }
    if (isNotNull(character)) {
      bytes[size] = Byte.parseByte(String.valueOf((int) character));
      size++;
    }
    addChanges();
    return this;
  }

  public StringBuilderImpl append(boolean bool) {
    if (isFill()) {
      createNewBytes(
        getNeededCapacity(Boolean.toString(bool)));
    }
    if (bool)
      append(Boolean.TRUE.toString());
    else append(Boolean.FALSE.toString());
    addChanges();
    return this;
  }

  public StringBuilderImpl append(int number) {
    if (isFill() || capacity >= size + 2 + Integer.toString(number).length()) {
      createNewBytes(
        getNeededCapacity(Integer.toString(number)));
    }
    if (isNotNull(number)) {
      byte[] numberArray = ByteBuffer.allocate(Integer.BYTES).putInt(number).array();
      bytes[size] = -1;
      size++;
      for (int i = 0; i < numberArray.length; i++) {
        bytes[size] = numberArray[i];
        size++;
      }
      bytes[size] = -1;
      size++;
    }
    addChanges();
    return this;
  }

  public StringBuilderImpl append(double number) {
    if (isFill() || capacity - size < Double.toString(number).length()) {
      createNewBytes(
        getNeededCapacity(Double.toString(number)));
    }
    if (isNotNull(number)) {
      byte[] numberArray = ByteBuffer.allocate(Double.BYTES).putDouble(number).array();
      bytes[size] = -2;
      size++;
      for (int i = 0; i < numberArray.length; i++) {
        if (isFill()) {
          createNewBytes(
            getNeededCapacity(Double.toString(number)));
        }
        bytes[size] = numberArray[i];
        size++;
      }
      bytes[size] = -2;
      size++;
    }
    addChanges();
    return this;
  }

  public StringBuilderImpl append(String string) {
    if (isFill() || capacity - size < string.length()) {
      createNewBytes(
        getNeededCapacity(string));
    }
    if (isNotNull(string)) {
      for (Byte value : string.getBytes()) {
        bytes[size] = value;
        size++;
      }
      addChanges();
    }
    addChanges();
    return this;
  }

  @Override
  public void undo() {
    if(changes.isEmpty())
      throw new NoSuchElementException("no element to undo");
    if (!Arrays.equals(changes.getFirst(), changes.getLast())) {
      changes.removeLast();
    }
    byte[] poolChanges = changes.removeLast();
    bytes = new byte[poolChanges.length];
    size = poolChanges.length;
    System.arraycopy(poolChanges, 0, bytes, 0, size);
    capacity = size;
  }

  @Override
  public int capacity() {
    return 0;
  }

  @Override
  public void trimToSize() {

  }

  @Override
  public void setLength(int newLength) {

  }

  @Override
  public int codePointAt(int index) {
    return 0;
  }

  @Override
  public int codePointBefore(int index) {
    return 0;
  }

  @Override
  public int codePointCount(int beginIndex, int endIndex) {
    return 0;
  }

  @Override
  public int offsetByCodePoints(int index, int codePointOffset) {
    return 0;
  }

  @Override
  public void getChars(int srcBegin, int srcEnd, char[] dst, int dstBegin) {

  }

  @Override
  public void setCharAt(int index, char ch) {

  }

  @Override
  public StringBuilderInterface append(Object obj) {
    return null;
  }

  @Override
  public StringBuilderInterface append(StringBuffer sb) {
    return null;
  }

  @Override
  public StringBuilderInterface append(char[] str) {
    return null;
  }

  @Override
  public StringBuilderInterface append(char[] str, int offset, int len) {
    return null;
  }

  @Override
  public StringBuilderInterface append(long l) {
    return null;
  }

  @Override
  public StringBuilderInterface append(float f) {
    return null;
  }

  @Override
  public StringBuilderInterface delete(int start, int end) {
    return null;
  }

  @Override
  public StringBuilderInterface appendCodePoint(int codePoint) {
    return null;
  }

  @Override
  public StringBuilderInterface deleteCharAt(int index) {
    return null;
  }

  @Override
  public StringBuilderInterface replace(int start, int end, String str) {
    return null;
  }

  @Override
  public String substring(int start) {
    return null;
  }

  private int getNeededCapacity(String string) {
    if (capacity - size < string.length()) {
      return capacity += string.length();
    }
    return capacity += DEFAULT_CAPACITY_FOR_BYTES;
  }

  private boolean isFill() {
    return size == capacity;
  }

  private void createNewBytes(int neededCapacity) {
    byte[] oldBytes = bytes;
    bytes = new byte[neededCapacity];
    copyArray(oldBytes, 0, bytes, 0, size);
  }

  private void createNewBytes(int neededCapacity, String string) {
    byte[] stringBytes = string.getBytes();
    bytes = new byte[neededCapacity];
    size = string.length();
    copyArray(stringBytes, 0, bytes, 0, size);
  }

  private void copyArray(byte[] src, int srcPoc, Object dest, int destPos, int length) {
    System.arraycopy(src, srcPoc, dest, destPos, length);
  }

  private <T> boolean isNotNull(T s) {
    if (s == null)
      return false;
    else return !s.toString().isBlank();
  }

  @Override
  public int length() {
    return 0;
  }

  @Override
  public char charAt(int index) {
    return 0;
  }

  @Override
  public boolean isEmpty() {
    return StringBuilderInterface.super.isEmpty();
  }

  @Override
  public CharSequence subSequence(int start, int end) {
    return null;
  }

  @Override
  public String substring(int start, int end) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int index, char[] str, int offset, int len) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, Object obj) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, String str) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, char[] str) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int dstOffset, CharSequence s) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int dstOffset, CharSequence s, int start, int end) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, boolean b) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, char c) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, int i) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, long l) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, float f) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, double d) {
    return null;
  }


  @Override
  public int indexOf(String str) {
    return 0;
  }

  @Override
  public int indexOf(String str, int fromIndex) {
    return 0;
  }

  @Override
  public int lastIndexOf(String str) {
    return 0;
  }

  @Override
  public int lastIndexOf(String str, int fromIndex) {
    return 0;
  }

  @Override
  public StringBuilderInterface reverse() {
    return null;
  }


  @Override
  public String toString() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < size; i++) {
      if (bytes[i] == -1) {
        i = getNumber(buffer, i + 1, -1, 4, Integer.class.getSimpleName());
      }
      if (bytes[i] == -2)
        i = getNumber(buffer, i + 1, -2, 8, Double.class.getSimpleName());
      else
        buffer.append((char) bytes[i]);
    }
    return buffer.toString();
  }

  @Override
  public IntStream chars() {
    return StringBuilderInterface.super.chars();
  }

  @Override
  public IntStream codePoints() {
    return StringBuilderInterface.super.codePoints();
  }

  private <T> int getNumber(StringBuffer buffer, int iBytes, int numberEnd, int capacity, String name) {
    int iInt = 0;
    byte[] array = new byte[capacity];
    while (bytes[iBytes] != numberEnd) {
      array[iInt] = bytes[iBytes];
      iBytes++;
      iInt++;
    }
    if (Objects.equals(name, Double.class.getSimpleName()))
      buffer.append(ByteBuffer.wrap(array).getDouble());
    if (Objects.equals(name, Integer.class.getSimpleName()))
      buffer.append(ByteBuffer.wrap(array).getInt());
    return iBytes;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    StringBuilderImpl that = (StringBuilderImpl) o;
    return capacity == that.capacity && size == that.size && Objects.equals(changes, that.changes) && Arrays.equals(bytes, that.bytes);
  }

  @Override
  public int hashCode() {
    int result = Objects.hash(changes, capacity, size);
    result = 31 * result + Arrays.hashCode(bytes);
    return result;
  }

}
