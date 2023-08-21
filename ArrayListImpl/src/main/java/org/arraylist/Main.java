package org.arraylist;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Main {
  public static void main(String[] args) {
    var millis = System.currentTimeMillis();
    ArrayListImpl<Integer> integerArList = new ArrayListImpl<>();
    System.out.println("Create " + (System.currentTimeMillis() - millis));

    for (int i = 0; i < 30; i++) {
      if (i % 2 == 0) {
        integerArList.add(i);
      } else
        integerArList.add(10);
    }
    System.out.println(integerArList);
    List<Integer> array = new ArrayList<Integer>(List.of(1,2,4));
    integerArList.add(null);
    integerArList.trimToSize();
    System.out.println(System.currentTimeMillis() - millis);
  }
}