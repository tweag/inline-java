
public class Classpath {

  public static void main(String[] args) {
    org.apache.commons.collections4.OrderedMap map =
      new org.apache.commons.collections4.map.LinkedMap();
    map.put("FIVE", "5");
    System.out.println(map.firstKey());
  }
}
