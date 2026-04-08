/*******************************************************************************
 * Copyright 2026 Observational Health Data Sciences and Informatics
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package org.ohdsi.conceptSetCondenser;

import java.util.HashSet;
import java.util.Set;

public class CandidateConcept {
	
	public enum Options {
		INCLUDE, INCLUDE_WITH_DESCENDANTS, EXCLUDE, EXCLUDE_WITH_DESCENDANTS, IGNORE
	}
	
	public int conceptId;
	public Set<Integer> descendants; 
	public Options[] validOptions; 
	public boolean inConceptSet;
	public int relevantDescendantCount;
	
	/**
	 * @param conceptId   The concept ID
	 * @param descendants All descendant concept IDs. This follows OHDSI convention that the descendants 
	 *                    include the concept set itself, so the minimum size is 1.
	 */
	public CandidateConcept(int conceptId, int[] descendants) {
		this.conceptId = conceptId;
		this.descendants = new HashSet<Integer>(descendants.length);
		for (int descendant : descendants)
			this.descendants.add(descendant);
		if (!this.descendants.contains(conceptId))
			throw new IllegalArgumentException("Descendants do not include concept itself for concept " + conceptId);
	}
	
	public boolean hasValidOption(Options option) {
		for (Options validOption : validOptions)
			if (validOption == option)
				return true;
		return false;
	}
}


