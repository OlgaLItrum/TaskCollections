package org.arraylist;

import java.util.*;

public class ArrayListImpl<E> implements List<E> {

  private final int DEFAULT_CAPACITY = 20;
  private final Object[] EMPTY_LIST = {};
  private E[] list;
  private int capacity;
  private int count;

  {
    count = 0;
  }

  public ArrayListImpl() {
    list = (E[]) new Object[capacity = DEFAULT_CAPACITY];
  }

  public ArrayListImpl(int initialCapacity) {
    list = (E[]) new Object[capacity = initialCapacity];
  }

  @Override
  public int size() {
    return list.length;
  }

  @Override
  public boolean isEmpty() {
    return count == 0;
  }

  @Override
  public boolean contains(Object o) {
    return false;
  }

  @Override
  public Iterator<E> iterator() {
    return new Iterator<>() {
      private int count = 0;

      @Override
      public boolean hasNext() {
        return count + 1 <= size();
      }

      @Override
      public E next() {
        return list[count++];
      }
    };
  }

  @Override
  public Object[] toArray() {
    return list;
  }

  @Override
  public boolean add(E o) {
    if (isFill()) {
      copyList();
    }
    list[count++] = o;
    return true;
  }

  private void copyList() {
    E[][] arrays = createOldArray(getCountOldArray(count));
    int index = 0;
    list = (E[]) new Object[capacity += 10];
    for (E[] array : arrays) {
      System.arraycopy(array, 0, list, index, array.length);
      index += array.length;
    }
  }


  private E[][] createOldArray(int countOldList) {
    int capacityArrays;
    int fromIndex = 0;
    E[][] arrays = (E[][]) new Object[countOldList][capacityArrays = count/countOldList];
    for (int i = 0; i < countOldList; i++) {
      arrays[i] = Arrays.copyOfRange(list, fromIndex, capacityArrays < size() ? capacityArrays : size());
      fromIndex = capacityArrays - 1;
      capacityArrays += arrays[i].length;
    }
    return arrays;
  }

  private int getCapacityArrays(int countOldList){
    int capacityArrays = count;
    int countArray = 1;
    while(capacityArrays % countOldList != 0){
      capacityArrays = capacityArrays / countOldList;
      countArray++;
    }
    return capacityArrays;
  }

  private int getCountOldArray(int count) {
    return Double.valueOf(Math.floor(
        Math.log(count % 2 == 0 ? count : ++count)))
      .intValue();
  }

  private boolean isFill() {
    return (Objects.equals(count, size()));
  }

  @Override
  public boolean remove(Object o) {
    return false;
  }

  @Override
  public boolean addAll(Collection c) {
    return false;
  }

  @Override
  public boolean addAll(int index, Collection c) {
    return false;
  }

  @Override
  public void clear() {
    list = (E[]) EMPTY_LIST;
    capacity = 0;
    count = 0;
  }

  @Override
  public E get(int index) {
    if (count - 1 >= index) {
      return list[index];
    } else throw new ArrayIndexOutOfBoundsException();
  }

  @Override
  public E set(int index, E element) {
    if (isIndexOutBounds(index)) {
      list[index] = element;
      return element;
    } else throw new ArrayIndexOutOfBoundsException();
  }

  private boolean isIndexOutBounds(int index) {
    return count - 1 >= index;
  }

  @Override
  public void add(int index, E element) {

  }

  @Override
  public E remove(int index) {
    return null;
  }

  @Override
  public int indexOf(Object o) {
    return 0;
  }

  @Override
  public int lastIndexOf(Object o) {
    return 0;
  }

  @Override
  public ListIterator listIterator() {
    return null;
  }

  @Override
  public ListIterator listIterator(int index) {
    return null;
  }

  @Override
  public List subList(int fromIndex, int toIndex) {
    return null;
  }

  @Override
  public boolean retainAll(Collection c) {
    return false;
  }

  @Override
  public boolean removeAll(Collection c) {
    return false;
  }

  @Override
  public boolean containsAll(Collection c) {
    return false;
  }

  @Override
  public E[] toArray(Object[] a) {
    return (E[]) new Object[0];
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder().append("[");
    if (isEmpty()) {
      builder.append("]");
    } else {
      for (int index = 0; index < count; index++) {
        builder.append(",").append(list[index]);
      }
      builder.deleteCharAt(1).append("]");
    }
    return builder.toString();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    ArrayListImpl<?> arrayList = (ArrayListImpl<?>) o;
    return capacity == arrayList.capacity && count == arrayList.count && Arrays.equals(list, arrayList.list);
  }

  @Override
  public int hashCode() {
    int result = Objects.hash(capacity, count);
    result = 31 * result + Arrays.hashCode(list);
    return result;
  }
}
