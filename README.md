# Exploring the role of pitch in detecting clauses, sentence boundaries and questions

### Tanmai Khanna
### Ganesh Mirishkar

## How to Use

- This code uses Python3.
- Run `pip install -r requirements.txt` or `pip3 install -r requirements.txt` to install the required dependencies.
- Go into `prosogram/`
- Run `python3 extract_pitch.py path_to_input_file.wav`
- Several prosogram data files will be generated in the location of the input sound file. Our code uses these files.
- The Phrase and Sentence Boundaries will be predicted with their timestamps.

## Flow

- Speech sample taken as a .wav file
- Prosogram generates the pitch contour, and the prosogram
- Labels given to the pitch contour
- Then trends are extracted from these pitch labels
- Rules written based on these trends try to predict commas, and sentence boundaries.
- If input is given as only one sentence, it can also predict declarative sentences turned into questions.

NOTE: Systematic Evaluation of these predictions have not been done. We have done preliminary evaluations as of now.
