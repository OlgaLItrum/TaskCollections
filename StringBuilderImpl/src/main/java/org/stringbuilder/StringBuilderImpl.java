package org.stringbuilder;

import java.nio.ByteBuffer;
import java.util.*;
import java.util.stream.IntStream;

public class StringBuilderImpl implements StringBuilderInterface {
  private static final int DEFAULT_CAPACITY_FOR_BYTES = 10;
  private static final byte[] EMPTY_BYTES = new byte[DEFAULT_CAPACITY_FOR_BYTES];

  private final ArrayDeque<byte[]> changes;

  private HashMap<Integer, EntryNumber> mapNumber;
  private byte[] bytes;

  private int capacity;

  private int size;

  {
    size = 0;
    capacity = DEFAULT_CAPACITY_FOR_BYTES;
    changes = new ArrayDeque<>();
    mapNumber = new HashMap<>();
  }

  public StringBuilderImpl() {
    bytes = EMPTY_BYTES;
  }

  public StringBuilderImpl(int initialCapacity) {
    bytes = new byte[initialCapacity];
  }

  public StringBuilderImpl(String string) {
    createNewBytes(getNeededCapacity(string), string);
    addChanges();
  }

  @Override
  public StringBuilderImpl append(CharSequence csq) {
    if (!csq.isEmpty()) {
      append(csq.toString());
      addChanges();
    }
    return this;
  }

  @Override
  public StringBuilderImpl append(CharSequence csq, int start, int end) {
    if (!csq.isEmpty()) {
      CharSequence charSub = csq.subSequence(start, end);
      append(charSub);
      addChanges();
    }
    return this;
  }

  public StringBuilderImpl append(char character) {
    if (isFill()) {
      createNewBytes(
        getNeededCapacity(Character.toString(character)));
    }
    bytes[size] = Byte.parseByte(String.valueOf((int) character));
    size++;
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

  enum TypeNumber {
    Double, Long, Integer, Float
  }

  class EntryNumber {
    Integer key = 0;
    TypeNumber type;
    byte[] arrayByte;

    {
      --key;
    }

    EntryNumber(TypeNumber type, byte[] arrayNumber) {
      this.type = type;
      this.arrayByte = arrayNumber;
    }

    static byte[] getArrayNumber(int number) {
      return ByteBuffer.allocate(Integer.BYTES).putInt(number).array();
    }

    static byte[] getArrayNumber(double number) {
      return ByteBuffer.allocate(Double.BYTES).putDouble(number).array();
    }

    static byte[] getArrayNumber(long number){
      return ByteBuffer.allocate(Long.BYTES).putLong(number).array();
    }

    static byte[] getArrayNumber(float number){
      return ByteBuffer.allocate(Float.BYTES).putFloat(number).array();
    }
  }

  public StringBuilderImpl append(int number) {
    if (isFill() || capacity >= size + 2 + Integer.toString(number).length()) {
      createNewBytes(
        getNeededCapacity(Integer.toString(number)));
    }
    if (isNotNull(number)) {
      EntryNumber entryNumber = new EntryNumber(TypeNumber.Integer, EntryNumber.getArrayNumber(number));
      mapNumber.put(entryNumber.key, entryNumber);

      bytes[size] = entryNumber.key.byteValue();
      size++;

      addChanges();
    }
    return this;
  }

  public StringBuilderImpl append(double number) {
    if (isFill() || capacity - size < Double.toString(number).length()) {
      createNewBytes(
        getNeededCapacity(Double.toString(number)));
    }
    if (isNotNull(number)) {
      EntryNumber entryNumber = new EntryNumber(TypeNumber.Double, EntryNumber.getArrayNumber(number));
      mapNumber.put(entryNumber.key, entryNumber);

      bytes[size] = entryNumber.key.byteValue();
      size++;

      addChanges();
    }
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
    return this;
  }

  @Override
  public StringBuilderInterface append(Object obj) {
    append(String.valueOf(obj));
    return this;
  }

  @Override
  public StringBuilderInterface append(StringBuffer sb) {
    append(sb.toString());
    return this;
  }

  @Override
  public StringBuilderInterface append(char[] str) {
    if (isFill() || capacity - size < str.length) {
      createNewBytes(
        getNeededCapacity(str.length));
    }
    if (isNotNull(str.length > 0)) {
      for (char value : str) {
        bytes[size] = (byte) value;
        size++;
      }
      addChanges();
    }
    return this;
  }

  @Override
  public StringBuilderInterface append(char[] str, int offset, int len) {
    if (str.length > 0) {
      if (isFill() || capacity - size < str.length) {
        createNewBytes(
          getNeededCapacity(str.length));
      }
      for (int index = offset; len > 0; index++) {
        bytes[size] = (byte) str[index];
        size++;
        len--;
      }
      addChanges();
    }
    return this;
  }

  @Override
  public StringBuilderInterface append(long number) {
    if (isFill()) {
      createNewBytes(
        getNeededCapacity(Long.toString(number)));
    }
    if (isNotNull(number)) {
      EntryNumber entryNumber = new EntryNumber(TypeNumber.Long, EntryNumber.getArrayNumber(number));

      mapNumber.put(entryNumber.key, entryNumber);
      bytes[size] = entryNumber.key.byteValue();

      size++;
      addChanges();
    }
    return this;
  }

  @Override
  public StringBuilderInterface append(float number) {
    if (isFill() || capacity - size < Float.toString(number).length()) {
      createNewBytes(
        getNeededCapacity(Float.toString(number)));
    }
    if (isNotNull(number)) {
      EntryNumber entryNumber = new EntryNumber(TypeNumber.Float, EntryNumber.getArrayNumber(number));

      mapNumber.put(entryNumber.key, entryNumber);
      bytes[size] = entryNumber.key.byteValue();

      size++;
      addChanges();
    }
    return this;
  }

  @Override
  public void setLength(int newLength) {
    if (newLength < 0) {
      throw new StringIndexOutOfBoundsException();
    }
    if (capacity - size < newLength) {
      createNewBytes(
        getNeededCapacity(newLength));
    }
    bytes = Arrays.copyOf(bytes, size = newLength);
  }

  @Override
  public void undo() {
    if (changes.isEmpty())
      throw new NoSuchElementException();
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
    return capacity;
  }

  @Override
  public void trimToSize() {
    capacity = size;
    bytes = Arrays.copyOf(bytes, size);
  }

  @Override
  public IntStream chars() {
    return this.toString().chars();
  }

  @Override
  public IntStream codePoints() {
    return this.toString().codePoints();
  }

  @Override
  public StringBuilderInterface appendCodePoint(int codePoint) {
    append((char) codePoint);
    return this;
  }

  @Override
  public StringBuilderInterface reverse() {
    Stack<Byte> chars = new Stack<>();
    for (int i = 0; i < size; i++) {
      chars.push(bytes[i]);
    }
    int index = 0;
    while (!chars.isEmpty()) {
      bytes[index] = chars.pop();
      index++;
    }
    return this;
  }

  @Override
  public int codePointAt(int index) {
    if (index > bytes.length || index < 0) {
      throw new IndexOutOfBoundsException();
    }
    if(bytes[index] < 0){
      mapNumber.get((int) bytes[index]);

    }
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
  public StringBuilderInterface delete(int start, int end) {
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
    return "";
  }

  @Override
  public int length() {
    return size;
  }

  @Override
  public char charAt(int index) {
    return 0;
  }

  @Override
  public boolean isEmpty() {
    return size == 0;
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
  public String toString() {
    StringBuffer buffer = new StringBuffer();

    for (int i = 0; i < size; i++) {

      if (bytes[i] < 0) {
        EntryNumber number = mapNumber.get((int) bytes[i]);
        getNumber(number, buffer);
      } else
        buffer.append((char) bytes[i]);

    }
    return buffer.toString();
  }

  private int getNeededCapacity(String string) {
    if (capacity - size < string.length()) {
      return capacity += string.length();
    }
    return capacity += DEFAULT_CAPACITY_FOR_BYTES;
  }

  private int getNeededCapacity(int length) {
    if (capacity - size < length) {
      return capacity += length;
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

  private void addChanges() {
    changes.add(Arrays.copyOf(bytes, size));
  }

  private void getNumber(EntryNumber number, StringBuffer buffer) {
    switch (number.type) {
      case Long -> buffer.append(ByteBuffer.wrap(number.arrayByte).getLong());
      case Double -> buffer.append(ByteBuffer.wrap(number.arrayByte).getDouble());
      case Integer -> buffer.append(ByteBuffer.wrap(number.arrayByte).getInt());
      case Float -> buffer.append(ByteBuffer.wrap(number.arrayByte).getFloat());
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;

    if (o == null || getClass() != o.getClass()) return false;

    StringBuilderImpl that = (StringBuilderImpl) o;
    return capacity == that.capacity &&
      size == that.size &&
      Objects.equals(changes, that.changes) &&
      Arrays.equals(bytes, that.bytes);
  }

  @Override
  public int hashCode() {
    int result = Objects.hash(changes, capacity, size);
    result = 31 * result + Arrays.hashCode(bytes);
    return result;
  }


}
