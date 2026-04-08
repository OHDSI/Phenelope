package org.ohdsi.conceptSetCondenser;

public class ConceptExpression {
	public int conceptId;
	public boolean exclude;
	public boolean descendants;
	
	public ConceptExpression(int conceptId, boolean exclude, boolean descendants) {
		this.conceptId = conceptId;
		this.exclude = exclude;
		this.descendants = descendants;
	}
	
	public void print() {
		System.out.println("Concept ID: " + conceptId + ", exclude: " + exclude + ", descendants: " + descendants);
	}
}
