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
  
  public ArrayListImpl(E[] array){
    this();
    checkFillSizeArray(count + array.length);
    invokeArrayCopy(array, 0, list, 0, array.length);
  }

  @Override
  public int size() {
    return count;
  }

  @Override
  public boolean isEmpty() {
    return count == 0;
  }

  @Override
  public boolean contains(Object o) {
    for (E item : list) {
      if (o.equals(item))
        return true;
    }
    return false;
  }

  @Override
  public Iterator<E> iterator() {
    return new Iterator<>() {
      private int count = 0;

      @Override
      public boolean hasNext() {
        return count + 1 <= list.length;
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
    if (isFill(count)) {
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
      invokeArrayCopy(array, 0, list, index, array.length);
      index += array.length;
    }
  }

  private E[][] createOldArray(int countOldList) {
    int capacityArrays;
    int fromIndex = 0;
    E[][] arrays = (E[][]) new Object[countOldList][capacityArrays = count / countOldList];
    for (int i = 0; i < countOldList; i++) {
      arrays[i] = Arrays.copyOfRange(list, fromIndex, capacityArrays < list.length ? capacityArrays : list.length);
      fromIndex = capacityArrays;
      capacityArrays += arrays[i].length + 1;
    }
    return arrays;
  }

  private int getCountOldArray(int count) {
    return Double.valueOf(Math.floor(
        Math.log(count % 2 == 0 ? count : ++count)))
      .intValue();
  }

  private boolean isFill(int count) {
    return count >= list.length;
  }

  @Override
  public boolean remove(Object o) {
    for (int i = 0; i < size(); i++) {
      if (o.equals(list[i])) {
        remove(i);
        count--;
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean addAll(Collection c) {
    if (c.isEmpty())
      return false;
    else {
      checkFillSizeArray(count);
      invokeArrayCopy((E[]) c.toArray(), 0, list, count, c.size());
      count += c.size();
      return true;
    }
  }

  @Override
  public boolean addAll(int index, Collection c) {
    if (c.isEmpty())
      return false;
    else {
      checkFillSizeArray(count + c.size());
      E[] subList = Arrays.copyOfRange(list, index, size());
      invokeArrayCopy((E[]) c.toArray(), 0, list, count - subList.length, c.size());
      checkFillSizeArray(count = (count - subList.length) + c.size());
      invokeArrayCopy(subList, 0, list, count, subList.length);
      count += subList.length;
      return true;
    }
  }

  @Override
  public void clear() {
    list = (E[]) EMPTY_LIST;
    capacity = 0;
    count = 0;
  }

  @Override
  public E get(int index) {
    checkIndexOfBounds(index);

    return list[index];
  }

  @Override
  public E set(int index, E element) {
    checkIndexOfBounds(index);
    list[index] = element;

    return element;
  }

  private boolean isIndexOutBounds(int index) {
    return index >= count;
  }

  @Override
  public void add(int index, E element) {
    checkIndexOfBounds(index);
    checkFillSizeArray(count);

    invokeArrayCopy(list, index, list, index + 1, size());
    count++;
    set(index, element);
  }

  private void checkIndexOfBounds(int index) {
    if (isIndexOutBounds(index))
      throw new IndexOutOfBoundsException();
  }

  private void checkFillSizeArray(int count) {
    if (isFill(count)) {
      copyList();
    }
  }

  @Override
  public E remove(int index) {
    checkIndexOfBounds(index);
    checkFillSizeArray(count);

    invokeArrayCopy(list, index + 1, list, index, size() - index);
    list[count] = null;
    count--;
    return list[index];
  }

  private void invokeArrayCopy(E[] src, int srcPoc, E[] dest, int destPoc, int length) {
    System.arraycopy(src, srcPoc, dest, destPoc, length);
  }

  @Override
  public int indexOf(Object o) {
    E item = (E) o;
    for (int i = 0; i < count; i++) {
      if (list[i].equals(item)) {
        return i;
      }
    }
    return -1;
  }

  @Override
  public int lastIndexOf(Object o) {
    E item = (E) o;
    for (int i = count - 1; i > 0; i--) {
      if (list[i].equals(item)) {
        return i;
      }
    }
    return -1;
  }

  //TODO
  @Override
  public ListIterator<E> listIterator() {
    return getListIterator();
  }

  //TODO
  @Override
  public ListIterator<E> listIterator(int index) {
    return getListIterator();
  }

  //TODO
  private ListIterator<E> getListIterator() {
    return new ListIterator<>() {
      private int count = 0;

      @Override
      public boolean hasNext() {
        return count + 1 <= list.length;
      }

      @Override
      public E next() {
        return list[count++];
      }

      @Override
      public boolean hasPrevious() {
        return count > 0;
      }

      @Override
      public E previous() {
        return list[count - 1];
      }

      @Override
      public int nextIndex() {
        return count + 1;
      }

      @Override
      public int previousIndex() {
        return count - 1;
      }

      @Override
      public void remove() {
        ArrayListImpl.this.remove(count);
      }

      @Override
      public void set(E e) {
        set(e);
      }

      @Override
      public void add(E e) {
        add(e);
      }
    };
  }

  //TODO
  @Override
  public List subList(int fromIndex, int toIndex) {
    return null;
  }

  //TODO
  @Override
  public boolean retainAll(Collection c) {
    int action = 0;
    for (int i = 0; i < size(); i++) {
      int count = 0;
      for (Object item : c) {
        if (!item.equals(list[i])) {
          count++;
        }
      }
      if (count == c.size()) {
        action++;
        checkFillSizeArray(count);
        remove(i);
      }
    }
    return action > 0;
  }

  //TODO
  @Override
  public boolean removeAll(Collection c) {
    if (c.isEmpty())
      return false;
    int action = 0;
    for (Object item : c) {
      int index = 0;
      while (index < size()) {
        if (item.equals(list[index])) {
          action++;
          remove(index);
        }
        if (!item.equals(list[index]))
          index++;
      }
    }
    return action != 0;
  }

  @Override
  public boolean containsAll(Collection c) {
    int count = 0;
    for (Object item : c) {
      for (E itemArray : this) {
        if (item.equals(itemArray)) {
          count++;
          break;
        }
      }
    }
    return c.size() == count;
  }

  @Override
  public Object[] toArray(Object[] a) {
    return Arrays.copyOf(list, count, a.getClass());
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
    return Arrays.equals(list, arrayList.list);
  }

  @Override
  public int hashCode() {
    int result = Objects.hash(capacity, count);
    result = 31 * result + Arrays.hashCode(list);
    return result;
  }
}
