package org.example;

import java.util.ArrayList;

public class Main {
  public static void main(String[] args) {
    var millis = System.currentTimeMillis();
    ArListImpl<Integer> integerArList = new ArListImpl<>();
    System.out.println("Create" + (System.currentTimeMillis() - millis));
    ArrayList<Integer> integers = new ArrayList<>();
    for(int i = 0; i < 11000; i++){
      integerArList.add(i);
    }
//    integerArList.set(5, 1000000000);
//    System.out.println(integerArList);
    System.out.println(System.currentTimeMillis() - millis);
    System.out.println(Double.valueOf(Math.floor(Math.log(100))).intValue());
    //if n % 2 != 0 -> (n + 1) % 2 == 0 -> Math.Log(N % 2 == 0 ? N : (N + 1))
    int number = 100;
    int logN = Double.valueOf(Math.floor(Math.log(number))).intValue();
    int n = 0;
    while (n <= logN){
      System.out.println(number % 2 + " " + number/2);
      number /= 2;
      n++;

    }
  }
}