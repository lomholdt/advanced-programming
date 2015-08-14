// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen

class Person {
	
	private String name;
	private int age;

	// read only accessors for val fields
	public String name() { return name; }
	public int age() { return age; }

	public Person(String name, int age) {
		this.name = name;
		this.age = age;
		System.out.println("Just constructed a person");
	}

	public String description () 
	{ return name + "is " + age + " years old"; }
}
