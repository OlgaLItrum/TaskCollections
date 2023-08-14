package org.stringbuilder;

import java.nio.ByteBuffer;
import java.util.*;

public class StringBuilderImpl implements StringBuilderInterface<StringBuilderImpl> {
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

  public StringBuilderImpl append(Character character) {
    if (isFill()) {
      createNewBytes(
        getNeededCapacity(character.toString()));
    }
    if (isNotNull(character)) {
      bytes[size] = Byte.parseByte(String.valueOf((int) character));
      size++;
    }
    addChanges();
    return this;
  }

  public StringBuilderImpl append(Boolean bool) {
    if (isFill()) {
      createNewBytes(
        getNeededCapacity(bool.toString()));
    }
    if (bool)
      append(Boolean.TRUE.toString());
    else append(Boolean.FALSE.toString());
    addChanges();
    return this;
  }

  public StringBuilderImpl append(Integer number) {
    if (isFill() || capacity >= size + 2 + number.toString().length()) {
      createNewBytes(
        getNeededCapacity(number.toString()));
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

  public StringBuilderImpl append(Double number) {
    if (isFill() || capacity - size < number.toString().length()) {
      createNewBytes(
        getNeededCapacity(number.toString()));
    }
    if (isNotNull(number)) {
      byte[] numberArray = ByteBuffer.allocate(Double.BYTES).putDouble(number).array();
      bytes[size] = -2;
      size++;
      for (int i = 0; i < numberArray.length; i++) {
        if (isFill()) {
          createNewBytes(
            getNeededCapacity(number.toString()));
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

  private <T> int getNumber(StringBuffer buffer, int iBytes, int numberEnd, int capacity, String name) {
    int iInt = 0;
    byte[] array = new byte[capacity];
    while (bytes[iBytes] != numberEnd) {
      array[iInt] = bytes[iBytes];
      iBytes++;
      iInt++;
    }
    System.out.println(name);
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
