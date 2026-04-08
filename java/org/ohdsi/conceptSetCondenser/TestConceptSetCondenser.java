package org.ohdsi.conceptSetCondenser;

public class TestConceptSetCondenser {

	public static void main(String[] args) {

		CandidateConcept c1 = new CandidateConcept(1, new int[]{1, 2, 3, 4, 5});
		CandidateConcept c2 = new CandidateConcept(2, new int[]{2, 3});
		CandidateConcept c3 = new CandidateConcept(3, new int[]{3});
		CandidateConcept c4 = new CandidateConcept(4, new int[]{4});
		CandidateConcept c5 = new CandidateConcept(5, new int[]{5});
		
		ConceptSetCondenser condenser = new ConceptSetCondenser(new int[]{1, 2, 3, 4}, new CandidateConcept[] {c1, c2, c3, c4, c5});
		condenser.condense();
		ConceptExpression[] expression = condenser.getConceptSetExpression();
		
		for (ConceptExpression conceptExpression : expression) 
			conceptExpression.print();	
	}

}
