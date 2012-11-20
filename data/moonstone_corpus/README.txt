This the a corpus annotated for topical shifts by several annotators. It consists of 20 chapters from the novel The Moonstone by Wilkie Collins. 12 chapters are annotated by 6 people, 4 chapters are annotated by 5 people and 4 chapters are annotated by 4 people.

We had 27 annotators that were divided into 5 groups as specified in annot_to_group.csv (the three empty values corrspond to 3 people who did not complete their annotations). Each group annotated 4 (usually non-consecutive chapters) as specified in group_to_chapter.csv.

The text for each chapter of the corpus is in ./text directory. Each sentence appears on a new line and paragraphs are separated by "\n\n\n\n".

The actual segment assignments are in ./segmentaions directory. The format is:

paragraph_id,annot1,annot2,annot3,annot4[,annot5,annot6]

The texts were annotated at level paragraph and, therefore, all indices in ./segmentaions directory are paragraph indices, not sentence indices. It is also imortant to note that they are 1-based, not 0-based.

The actual instructions that were given to the annotators are in ./annotator_guidelines_group1.pdf

The annotators were also asked to briefly describe each episode, effectively creating an outline. This data will be released shortly and it is now available upon request.

