# Exploring the role of pitch in detecting clauses, sentence boundaries and questions

As part of the ***Topics in Speech to Speech Machine Translation*** course.

### Tanmai Khanna
### Ganesh Mirishkar

## Flow

- Speech sample taken as a .wav file
- Prosogram generates the pitch contour, and the prosogram
- `extract_pitch.py` gives labels to the pitch contour
- Then trends are extracted from these pitch labels
- Rules written based on these trends try to predict commas, and sentence boundaries.
- If input is given as only one sentence, it can also predict declarative sentences turned into questions.

NOTE: Systematic Evaluation of these predictions have not been done. We have done preliminary evaluations as of now.