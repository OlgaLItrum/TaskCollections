package org.arraylist;

import java.util.ArrayList;

public class Main {
  public static void main(String[] args) {
    var millis = System.currentTimeMillis();
    ArrayListImpl<Integer> integerArList = new ArrayListImpl<>();
    System.out.println("Create " + (System.currentTimeMillis() - millis));
    ArrayList<Integer> integers = new ArrayList<>();
    for(int i = 0; i < 100000; i++){
      integerArList.add(i);
    }
    integerArList.set(5, 1000000000);
    System.out.println(integerArList);
//    int count = 50;
//    System.out.println(Double.valueOf(Math.floor(
//        Math.log(count % 2 == 0 ? count : ++count)))
//      .intValue());
    System.out.println(System.currentTimeMillis() - millis);
    //if n % 2 != 0 -> (n + 1) % 2 == 0 -> Math.Log(N % 2 == 0 ? N : (N + 1))
  }
}