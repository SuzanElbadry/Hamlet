# Hamlet

# Task 1: Show the number of lines said per character throughout the play.
## Preprocessing
 1. Actions taking the format "\n\t[]" are replaced by an empty string.
 2. The title "HAMLET" coming before the start of every ACT having the format "\n\nHAMLET\n\n" is replaced by an empty string.
 3. Extra newlines are replaced by a single newline.
 4. Character introduction part is excluded and preprocessing is applied from SCENE 1.
 5. Scene indices are determined to mark the start of every scene.
 6. Final preprocessed output included whole paragrapg conversation for each charcter and the corresponding scene.
 7. Punctuation is removed from text after multi speakers are extracted.

- Identifyinhg speakers: MultiSpeakers are identified according to the delimiter "|" and are extracted using regex "([A-Z][A-Z]+)|.", otherwise single speakers are identified by "\n\n".
- Count Multi-line conversation for each speaker: Multi-lines are extracted using regex "[a-z|A-Z]+\\n\\t".
- Count Single-line conversation: conversation for each speaker: Multi-lines are extracted using regex "\\t".
- Count the number of speakers in each sentence: Speakers count are identified by regex "[A-Z]+\t"

# Task 2: Make a word cloud of unusual words in the script.
## Assumptions:
 Unusual words in hamlet are considered to be words with the lowest frequencies.
## Preprocessing
 1. Preprocessing steps 1-4 are applied to the text. 
 2. Punctuation removed.
 3. Delimiter "\t" is replaced by space. 
 4. Text is lowered cased. 
 5. Stop words are removed. 
 6. Word steming is applied. 

# Task 3: Compute the sentiment score for each scene.
## Preprocessing
1. Preprocessing steps 1-4 are applied to the text.
2. Punctuation removed.
3. Delimiter "\n\t" is replaced by "." Delimiters "\n+" and "\t+" are replaced by space.

- The sentiment score of each line is caculated and averaged for each scene.
- Sentiment analysis models trained on the old english language will provide better sentiment scores. 
