package org.stringbuilder;

import java.nio.ByteBuffer;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class StringBuilderImpl implements StringBuilderInterface {
  private static final int DEFAULT_CAPACITY_FOR_BYTES = 10;
  private static final byte[] EMPTY_BYTES = new byte[DEFAULT_CAPACITY_FOR_BYTES];

  private final ArrayDeque<byte[]> changes;

  private static final HashMap<Integer, EntryNumber> mapNumber = new HashMap<>();
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

  public StringBuilderImpl(String string) {
    createNewBytes(getNeededCapacity(string), string);
    addChanges();
  }

  private enum TypeNumber {
    Double(java.lang.Double.class),
    Long(java.lang.Long.class),
    Integer(java.lang.Integer.class),
    Float(java.lang.Float.class);

    private final Class clazz;

    <T> TypeNumber(Class<T> clazz) {
      this.clazz = clazz;
    }
  }

  private static class EntryNumber {

    private Integer key;
    private final TypeNumber type;
    private byte[] arrayByte;


    private EntryNumber(TypeNumber type, byte[] arrayNumber) {
      this.key = -1 - mapNumber.size();
      this.type = type;
      this.arrayByte = arrayNumber;
    }

    private static <T> byte[] getArrayNumber(Number number, Class<T> type) {
      if (type.isAssignableFrom(Integer.class)) {
        return ByteBuffer.allocate(Integer.BYTES).putInt((int) number).array();
      }

      if (type.isAssignableFrom(Double.class)) {
        return ByteBuffer.allocate(Double.BYTES).putDouble((double) number).array();
      }

      if (type.isAssignableFrom(Long.class)) {
        return ByteBuffer.allocate(Long.BYTES).putLong((long) number).array();
      }

      if (type.isAssignableFrom(Float.class)) {
        return ByteBuffer.allocate(Float.BYTES).putFloat((float) number).array();
      }
      return null;
    }

    private static boolean isNumber(byte num) {
      return (num == 0) || (num < 0);
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      EntryNumber that = (EntryNumber) o;
      return Objects.equals(key, that.key) && type == that.type && Arrays.equals(arrayByte, that.arrayByte);
    }

    @Override
    public int hashCode() {
      int result = Objects.hash(key, type);
      result = 31 * result + Arrays.hashCode(arrayByte);
      return result;
    }
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

  public StringBuilderImpl append(int number) {
    if (isFill() || capacity >= size + 2 + Integer.toString(number).length()) {
      createNewBytes(
        getNeededCapacity(Integer.toString(number)));
    }
    if (isNotNull(number)) {
      EntryNumber entryNumber = new EntryNumber(TypeNumber.Integer, EntryNumber.getArrayNumber(number, Integer.class));
      addNumber(entryNumber, number, -1);
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
      EntryNumber entryNumber = new EntryNumber(TypeNumber.Double, EntryNumber.getArrayNumber(number, Double.class));

      addNumber(entryNumber, number, -1);

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
      EntryNumber entryNumber = new EntryNumber(TypeNumber.Long, EntryNumber.getArrayNumber(number, Long.class));

      addNumber(entryNumber, number, -1);

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
      EntryNumber entryNumber = new EntryNumber(TypeNumber.Float, EntryNumber.getArrayNumber(number, Float.class));

      addNumber(entryNumber, number, -1);
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
    checkIndexOutOfBound(index);
    return this.toString().codePointAt(index);
  }

  @Override
  public char charAt(int index) {
    checkIndexOutOfBound(index);
    return this.toString().charAt(index);
  }

  @Override
  public boolean isEmpty() {
    return size == 0;
  }

  @Override
  public StringBuilderInterface deleteCharAt(int index) {
    checkIndexOutOfBound(index);

    if (EntryNumber.isNumber(bytes[index])) {
      deleteNumberAt(index);
    } else
      copyArray(bytes, index + 1, bytes, index, size - 1);

    size--;
    addChanges();
    trimToSize();

    return this;
  }

  @Override
  public StringBuilderInterface replace(int start, int end, String str) {
    int lengthString = str.length();
    if (isFill() || capacity - size <= lengthString){
      createNewBytes(getNeededCapacity(lengthString));
    }

    char[] strArray = str.toCharArray();
    for (int index = start, i = 0; index < end; index++) {
      setCharAt(index, strArray[i]);
      i++;
    }
    if(str.length() > end - start){
      for(int i = end - start, index = end; i < strArray.length; i++){
        insert(index, String.valueOf(strArray[i]));
        index++;
      }
    }
    return this;
  }

  @Override
  public String substring(int start) {
    return toString().substring(start);
  }

  @Override
  public CharSequence subSequence(int start, int end) {
    checkIndexOutOfBound(start);
    checkIndexOutOfBound(end);
    return toString().subSequence(start, end);
  }

  @Override
  public String substring(int start, int end) {
    checkIndexOutOfBound(start);
    checkIndexOutOfBound(end);
    return toString().substring(start, end);
  }

  @Override
  public int length() {
    return size;
  }

  @Override
  public int codePointBefore(int index) {
    checkIndexOutOfBound(index);
    return this.toString().codePointBefore(index);
  }

  @Override
  public int codePointCount(int beginIndex, int endIndex) {
    checkIndexOutOfBound(beginIndex);
    checkIndexOutOfBound(endIndex);
    return toString().codePointCount(beginIndex, endIndex);
  }

  @Override
  public int offsetByCodePoints(int index, int codePointOffset) {
    checkIndexOutOfBound(index);

    return toString().offsetByCodePoints(index, codePointOffset);
  }

  @Override
  public void getChars(int srcBegin, int srcEnd, char[] dst, int dstBegin) {
    checkIndexOutOfBound(srcBegin);
    checkIndexOutOfBound(srcEnd);

    System.arraycopy(toString().toCharArray(), srcBegin, dst, dstBegin, srcEnd - srcBegin);
  }

  @Override
  public void setCharAt(int index, char ch) {
    checkIndexOutOfBound(index);

    byte num = bytes[index];
    boolean isBeginOfNumber = num < 0;
    boolean isNotBeginOfNumber = num == 0;


    if (EntryNumber.isNumber(num)) {
      if (isBeginOfNumber) {
        setValueIfBeginOfNumber(index, ch);
      } else if (isNotBeginOfNumber) {
        setValueIfNotBeginOfNumber(index, ch);
      }
    } else
      bytes[index] = (byte) ch;
  }

  private void setValueIfNotBeginOfNumber(int index, char ch) {

    int startIndexForNumber = getStartIndexOfNumber(index);
    EntryNumber number = mapNumber.get((int) bytes[startIndexForNumber]);
    Number numberValue = getNumber(number);
    boolean isEndIndexOfNumber = index == numberValue.toString().length() - 1;
    boolean isMiddleIndexOfNumber = index != numberValue.toString().length() - 1;
    boolean isChar = !Character.isDigit(ch);

    if (isEndIndexOfNumber) {
      if (isChar) {
        setCharIsNotDigitAndEndIndexOfNumber(index, ch, startIndexForNumber, number);
      } else {
        setCharIsDigitAndEndIndexOfNumber(index, ch, startIndexForNumber, number);
      }

    } else if (isMiddleIndexOfNumber) {
      if (isChar) {
        setCharIsNotDigitAndMiddleIndexOfNumber(index, ch, startIndexForNumber, number);
      } else {
        setCharIsDigitAndMiddleIndexOfNumber(index, ch, startIndexForNumber, number);
      }
    }
  }

  private void setCharIsNotDigitAndMiddleIndexOfNumber(int index, char ch, int startIndexForNumber, EntryNumber number) {
    String newNumberBeginString = createNewStringNumber(number, index, startIndexForNumber)
      .substring(startIndexForNumber, index - startIndexForNumber);

    String newNumberEndString = createNewStringNumber(number, index, startIndexForNumber)
      .substring(index - startIndexForNumber);

    Number newNumberFirst = createNewNumber(newNumberBeginString, number.type.clazz);
    Number newNumberSecond = createNewNumber(newNumberEndString, number.type.clazz);

    number.arrayByte = EntryNumber.getArrayNumber(newNumberFirst, number.type.clazz);

    byte[] arrayByteForSecondNumber = EntryNumber.getArrayNumber(newNumberSecond, number.type.clazz);
    EntryNumber secondNumber = new EntryNumber(number.type, arrayByteForSecondNumber);

    addNumber(number, getNumber(number), startIndexForNumber);

    bytes[index] = (byte) ch;
    addNumber(secondNumber, getNumber(secondNumber), index + 1);
  }

  private void setCharIsDigitAndMiddleIndexOfNumber(int index, char ch, int startIndexForNumber, EntryNumber number) {
    String newNumberString = createNewStringNumber(number, index, startIndexForNumber)
      .substring(startIndexForNumber, index - startIndexForNumber)
      + ch
      + createNewStringNumber(number, index, startIndexForNumber)
      .substring(index - startIndexForNumber);

    Number newNumber = createNewNumber(newNumberString, number.type.clazz);

    number.arrayByte = EntryNumber.getArrayNumber(newNumber, number.type.clazz);

    addNumber(number, getNumber(number), startIndexForNumber);
  }

  private void setCharIsDigitAndEndIndexOfNumber(int index, char ch, int startIndexForNumber, EntryNumber number) {
    String newNumberString = createNewStringNumber(number, index, startIndexForNumber)
      .substring(startIndexForNumber, index) + ch;

    Number newNumber = createNewNumber(newNumberString, number.type.clazz);

    number.arrayByte = EntryNumber.getArrayNumber(newNumber, number.type.clazz);

    addNumber(number, newNumber, -1);
  }

  private void setCharIsNotDigitAndEndIndexOfNumber(int index, char ch, int startIndexForNumber, EntryNumber number) {
    String newNumberBeginString = createNewStringNumber(number, index, startIndexForNumber)
      .substring(startIndexForNumber, index - startIndexForNumber);

    Number newNumberFirst = createNewNumber(newNumberBeginString, number.type.clazz);

    number.arrayByte = EntryNumber.getArrayNumber(newNumberFirst, number.type.clazz);

    addNumber(number, getNumber(number), startIndexForNumber);

    bytes[index] = (byte) ch;
  }

  private void setValueIfBeginOfNumber(int index, char ch) {
    byte num = bytes[index];
    EntryNumber entryNumber = mapNumber.get((int) num);
    String oldNumber = getNumber(entryNumber).toString();

    if (Character.isDigit(ch)) {
      char oldNumFirst = oldNumber.charAt(0);

      String newNumberString = oldNumber.replace(oldNumFirst, ch);
      Number newNumber = createNewNumber(newNumberString, entryNumber.type.clazz);
      entryNumber.arrayByte = EntryNumber.getArrayNumber(newNumber, entryNumber.type.clazz);

      addNumber(entryNumber, newNumber, index);

    } else if (!Character.isDigit(ch)) {
      byte[] newBytes = bytes;
      newBytes[index] = (byte) ch;

      if (isFill() || size + 2 == capacity)
        createNewBytes(
          getNeededCapacity(1));

      copyArray(newBytes, index, bytes, index + 1, newBytes.length);
      bytes[index + 1] = entryNumber.key.byteValue();
      deleteCharAt(index + 1);
    }
  }

  @Override
  public StringBuilderInterface delete(int start, int end) {
    return null;
  }


  @Override
  public StringBuilderInterface insert(int index, char[] str, int offset, int len) {
    return null;
  }

  @Override
  public StringBuilderInterface insert(int offset, Object obj) {
    insert(offset, String.valueOf(obj));
    return this;
  }

  @Override
  public StringBuilderInterface insert(int offset, String str) {
    checkIndexOutOfBound(offset);

    byte[] arrayBegin = Arrays.copyOfRange(bytes, 0, offset);
    byte[] arrayMiddle = str.getBytes();
    byte[] arrayEnd = Arrays.copyOfRange(bytes, offset, size);
    byte[] newBytes = new byte[arrayEnd.length + arrayBegin.length + arrayMiddle.length];

    copyArray(arrayBegin, 0, newBytes, 0, arrayBegin.length);
    size = arrayBegin.length;
    copyArray(arrayMiddle, 0, newBytes, size, arrayMiddle.length);
    size += arrayMiddle.length;
    copyArray(arrayEnd, 0, newBytes, size, arrayEnd.length);
    size += arrayEnd.length;
    bytes = newBytes;
    return this;
  }

  @Override
  public StringBuilderInterface insert(int offset, char[] str) {
    insert(offset, new String(str));
    return this;
  }

  @Override
  public StringBuilderInterface insert(int dstOffset, CharSequence s) {
    insert(dstOffset, s.toString());
    return this;
  }

  @Override
  public StringBuilderInterface insert(int dstOffset, CharSequence s, int start, int end) {
    checkIndexOutOfBound(start);
    checkIndexOutOfBound(end);

    CharSequence sequence = s.subSequence(start, end);
    insert(dstOffset, sequence);
    return this;
  }

  @Override
  public StringBuilderInterface insert(int offset, boolean b) {
    insert(offset, Boolean.toString(b));
    return this;
  }

  @Override
  public StringBuilderInterface insert(int offset, char c) {
    insert(offset, Character.toString(c));
    return this;
  }

  @Override
  public StringBuilderInterface insert(int offset, int i) {
    insert(offset, Integer.toString(i));
    return this;
  }

  @Override
  public StringBuilderInterface insert(int offset, long l) {
    insert(offset, Long.toString(l));
    return this;
  }

  @Override
  public StringBuilderInterface insert(int offset, float f) {
    insert(offset, Float.toString(f));
    return this;
  }

  @Override
  public StringBuilderInterface insert(int offset, double d) {
    insert(offset, Double.toString(d));
    return this;
  }

  @Override
  public int indexOf(String str) {
    return toString().indexOf(str);
  }

  @Override
  public int indexOf(String str, int fromIndex) {
    checkIndexOutOfBound(fromIndex);
    return toString().indexOf(str, fromIndex);
  }

  @Override
  public int lastIndexOf(String str) {
    return toString().lastIndexOf(str);
  }

  @Override
  public int lastIndexOf(String str, int fromIndex) {
    checkIndexOutOfBound(fromIndex);
    return toString().lastIndexOf(str, fromIndex);
  }

  @Override
  public String toString() {
    StringBuffer buffer = new StringBuffer();

    for (int i = 0; i < size; i++) {
      if (bytes[i] == 0) continue;
      if (bytes[i] < 0) {
        EntryNumber number = mapNumber.get((int) bytes[i]);
        buffer.append(getNumber(number));
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

  private void deleteNumberAt(int index) {
    byte num = bytes[index];

    boolean isNotBeginOfNumber = num == 0;
    boolean isBeginOfNumber = num < 0;

    if (isNotBeginOfNumber) {
      int startIndexForNumber = getStartIndexOfNumber(index);
      EntryNumber number = mapNumber.get((int) bytes[startIndexForNumber]);
      String newNumberString = createNewStringNumber(number, index, startIndexForNumber);
      Number newNumber = createNewNumber(newNumberString, number.type.clazz);
      number.arrayByte = EntryNumber.getArrayNumber(newNumber, number.type.clazz);
      mapNumber.put(number.key, number);

    } else if (isBeginOfNumber) {
      EntryNumber number = mapNumber.get((int) bytes[index]);
      String newNumberString = createNewStringNumber(number, index, index);
      Number newNumber = createNewNumber(newNumberString, number.type.clazz);
      number.arrayByte = EntryNumber.getArrayNumber(newNumber, number.type.clazz);
      mapNumber.put(number.key, number);
    }
  }

  private <T extends Number> T createNewNumber(String newNumberString, Class<T> type) {
    if (type.isAssignableFrom(Integer.class)) {
      return (T) (Integer) Integer.parseInt(newNumberString);
    }
    if (type.isAssignableFrom(Double.class)) {
      return (T) (Double) Double.parseDouble(newNumberString);
    }
    if (type.isAssignableFrom(Long.class)) {
      return (T) (Long) Long.parseLong(newNumberString);
    }
    if (type.isAssignableFrom(Float.class)) {
      return (T) (Float) Float.parseFloat(newNumberString);
    }

    return null;
  }

  private String createNewStringNumber(EntryNumber number, int index, int startIndexForNumber) {
    String numberString = getNumber(number).toString();
    if (index > startIndexForNumber) {
      String numberStringBegin = numberString.substring(0, index - startIndexForNumber);
      String numberStringEnd = numberString.substring(index - startIndexForNumber + 1);
      return numberStringBegin + numberStringEnd;
    } else {
      return numberString.substring((index - startIndexForNumber) + 1);
    }
  }

  private int getStartIndexOfNumber(int index) {
    int startIndexNumber = index;

    while (bytes[startIndexNumber] == 0) {
      startIndexNumber--;
    }

    return startIndexNumber;
  }

  private void checkIndexOutOfBound(int index) {
    if (index > bytes.length || index < 0)
      throw new IndexOutOfBoundsException();
  }

  private Number getNumber(EntryNumber number) {
    return switch (number.type) {
      case Long -> ByteBuffer.wrap(number.arrayByte).getLong();
      case Double -> ByteBuffer.wrap(number.arrayByte).getDouble();
      case Integer -> ByteBuffer.wrap(number.arrayByte).getInt();
      case Float -> ByteBuffer.wrap(number.arrayByte).getFloat();
    };
  }

  private void addNumber(EntryNumber entryNumber, Number number, int indexIfReplaceOldValue) {
    if (isFill()) {
      createNewBytes(getNeededCapacity(number.toString().length()));
    }

    mapNumber.put(entryNumber.key, entryNumber);
    if (indexIfReplaceOldValue != -1) {
      bytes[indexIfReplaceOldValue] = entryNumber.key.byteValue();
      copyArray(bytes, indexIfReplaceOldValue, bytes, indexIfReplaceOldValue, number.toString().length() - 1);
    } else {
      bytes[size] = entryNumber.key.byteValue();
      size++;

      for (int i = size; i < String.valueOf(number).length(); i++) {
        bytes[i] = 0;
        size++;
      }
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
