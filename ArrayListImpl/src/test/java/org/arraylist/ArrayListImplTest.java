package org.arraylist;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;


public class ArrayListImplTest {

  private ArrayListImpl<Integer> list;
  private static List<Integer> paramList;
  private static final int initialArraySize = 10;
  private int index = (int) (Math.random() * initialArraySize);


  @BeforeAll
  public static void setList() {
    paramList = new ArrayList<>();
    for (int i = 0; i < initialArraySize; i++) {
      if (i % 2 == 0)
        paramList.add(i);
      else paramList.add(i * i);
    }
  }

  @BeforeEach
  public void setUp() {
    list = new ArrayListImpl<>();
    for (int i = 0; i < initialArraySize; i++) {
      list.add(i);
    }
  }

  @Test
  public void size_ReturnsZeroIfSizeIsZero() {
    list.clear();
    assertEquals(0, list.size());
  }

  @Test
  public void size_ReturnsActualSizeOfArray() {
    assertEquals(initialArraySize, list.size());
  }

  @Test
  public void isEmpty_ReturnsTrueIfEmpty() {
    list.clear();
    assertTrue(list.isEmpty());
  }

  @Test
  public void isEmpty_ReturnsFalseIfNotEmpty() {
    assertFalse(list.isEmpty());
  }

  @Test
  public void contains_ReturnsFalseIfValueIsNotPresent() {
    int value = initialArraySize;
    assertFalse(list.contains(value));
  }

  @Test
  public void toArray() {
    fail();
  }

  @Test
  public void add_ByItemReturnsTrueIfItemWasAdded() {
    int value = initialArraySize;
    list.add(index, value);
    assertTrue(list.contains(value));
    assertEquals(value, list.get(index));
  }

  @Test
  public void add_ByIndexReturnsItemIfIndexIsOutOfBounds() {
    int value = initialArraySize;
    list.add(index, value);
    assertEquals(value, list.get(index));
  }

  @Test
  public void add_ThrowsIndexOutOfBoundsExceptionIfIndexIsOutOfBounds() {
    assertThrows(IndexOutOfBoundsException.class, () -> list.add(-1, 10));
  }

  @Test
  public void remove_ByIndexReturnsItemIfIndexIsOutOfBounds() {
    int value = initialArraySize;
    list.add(index, value);
    assertEquals(value, list.remove(index));
  }

  @Test
  public void remove_ThrowsIndexOutOfBoundsExceptionIfIndexIsOutOfBounds() {
    assertThrows(IndexOutOfBoundsException.class, () -> list.remove(-1));
  }

  //remove(E e)
  //returns boolean
  @Test
  public void remove_ByItemReturnsFalseIfItemWasNotFound() {
    int value = initialArraySize;
    assertFalse(list.remove((Integer) value));
    assertFalse(list.contains(value));
  }

  //if duplicate value returns true and deleted one element
  @Test
  public void remove_ByItemReturnsTrueIfItemWasFound() {
    int value = initialArraySize;
    list.add(value);
    assertTrue(list.contains(value));
    assertTrue(list.remove((Integer) value));
  }

  @Test
  public void addAll_ByReturnsTrueIfCollectionIsNotEmpty() {
    assertFalse(paramList.isEmpty());
    assertTrue(list.addAll(paramList));
    assertTrue(list.containsAll(paramList));
  }

  @Test
  public void addAll_ReturnsFalseIfCollectionIsEmpty() {
    paramList.clear();
    assertFalse(list.addAll(paramList));
  }

  @Test
  public void addAll_AtIndexReturnsTrueIfCollectionIsNotEmptyAndIndexIsOutOfBounds() {
    list.addAll(index, paramList);
    ArrayListImpl<Integer> subList = (ArrayListImpl<Integer>) list.subList(index, paramList.size() + index);
    subList.trimToSize();
    assertArrayEquals(paramList.toArray(), subList.toArray());
  }

  @Test
  public void clear_ReturnsEmptyArray() {
    list.clear();
    assertTrue(list.isEmpty());
  }

  @Test
  public void get_ReturnsItemIfIndexInOfBoundsOfList() {
    int value = initialArraySize;
    list.add(index, value);
    assertEquals(value, list.get(index));
  }

  @Test
  public void get_ThrowsIndexOutOfBoundsExceptionIfIndexOutOfBounds() {
    assertThrows(IndexOutOfBoundsException.class, () -> list.get(-1));
  }

  @Test
  public void set_ReturnsItemIfIndexInOfBoundsOfList() {
    int value = (int) (Math.random() * 100);
    assertNotEquals(value, list.get(index));
    assertEquals(value, list.set(index, value));
  }

  @Test
  public void indexOf_ReturnsIndexOfItemIfItemWasFound() {
    int value = list.get(index);
    assertEquals(index, list.indexOf(value));
  }

  @Test
  public void indexOf_ReturnsOneLessOfItemIfItemWasNotFound() {
    int value = (int) (Math.random() * 100);
    assertNotEquals(index, list.indexOf(value));
  }

  @Test
  public void lastIndexOf_ByObjectReturnsIndexIfIndexInOutOfBounds() {
    int indexFirst = list.indexOf(5);
    int indexLast = indexFirst + 1;
    list.set(indexLast, 5);
    assertTrue(list.lastIndexOf(5) > indexFirst);
    assertNotEquals(indexFirst, list.lastIndexOf(5));
    assertEquals(list.get(indexFirst), list.get(indexLast));
  }

  @Test
  public void subList_ReturnsListIfIndexInOutOfBoundsAnd() {
    int toIndex = index + 1 > list.size() ? index - 1 : index + 1;
    Object[] subListExpected = Arrays.copyOfRange(list.toArray(), index, toIndex);
    ArrayListImpl<Integer> subListResult = (ArrayListImpl<Integer>) list.subList(index, toIndex);
    subListResult.trimToSize();
    assertArrayEquals(subListExpected, subListResult.toArray());
  }

  @Test
  public void retainAll_ReturnsFalseIfCollectionIsNotEmpty() {
    List<Integer> expectedList = List.of(1,2,3);
    assertTrue(list.retainAll(expectedList));
    list.trimToSize();
    assertTrue(expectedList.containsAll(list));
  }

  @Test
  public void removeAll_ReturnsTrueIfCollectionIsNotEmptyAndItemsWasFoundedAndRemoved() {
    assertTrue(list.removeAll(paramList));
  }

  @Test
  public void removeAll_ReturnsFalseIfCollectionIsEmptyOrItemsWasNotFoundedAndRemoved() {
    assertFalse(list.removeAll(List.of(200, 150)));
  }

  @Test
  public void containsAll_ReturnsTrueIfListIsContains() {
    List<Integer> isContainList = List.of(1, 2, 4);
    assertTrue(list.containsAll(isContainList));
  }

  @Test
  public void containsAll_ReturnsFalseIfListIsContains() {
    List<Integer> isContainList = paramList;
    assertFalse(list.containsAll(isContainList));
  }

  @Test
  public void testToArray() {
    fail();
  }
}