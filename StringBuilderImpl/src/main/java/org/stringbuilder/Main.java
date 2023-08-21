package org.stringbuilder;

import java.util.concurrent.ThreadLocalRandom;

public class Main {
  public static void main(String[] args) {
    StringBuilderImpl builder1 = new StringBuilderImpl("q32e").append(1.72);
    builder1.append("1111111")
      .append(200);
    builder1.undo();
    builder1.undo();
    builder1.undo();
    System.out.println(builder1);
  }
}