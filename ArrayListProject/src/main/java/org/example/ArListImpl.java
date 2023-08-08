package org.example;

import java.lang.ref.WeakReference;
import java.util.*;

public class ArListImpl<E> implements List<E> {

  private final int DEFAULT_CAPACITY = 10;
  private final Object[] EMPTY_LIST = {};
  //  private WeakReference<Object[]> weakReference;
  private E[] list;
  private int capacity;
  private int count;

  {
    count = 0;
  }

  public ArListImpl() {
//    weakReference = new WeakReference<>(new Object[capacity = DEFAULT_CAPACITY]);
//    list = (E[]) weakReference.get();
    list = (E[]) new Object[capacity = DEFAULT_CAPACITY];
  }

  public ArListImpl(int initialCapacity) {
//    weakReference = new WeakReference<>(new Object[capacity = initialCapacity]);
//    list = (E[]) weakReference.get();
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
    E[] oldList1 =  Arrays.copyOfRange(list, 0, capacity / 2);
    E[] oldList2 =  Arrays.copyOfRange(list, capacity / 2, capacity);


//    list = (E[]) new WeakReference<>(new Object[capacity += 10]).get();
    list = (E[]) new Object[capacity += 10];
    System.arraycopy(oldList1, 0, list, 0, count / 2);
    System.arraycopy(oldList2, 0, list, count / 2, count/ 2);
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
//    weakReference = new WeakReference<>(EMPTY_LIST);
//    list = (E[]) weakReference.get();
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
}
