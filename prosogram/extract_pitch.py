import sys
from collections import OrderedDict
import matplotlib.pyplot as plt
import parselmouth
import re

def run_script_with_pluseq(init_script):
    script = init_script

    to_include = True
    while to_include:
        match = re.search(r'^include ((?:\w|\.)+)', script, re.MULTILINE)
        if match:
            with open(match.group(1)) as f:
                script = script[:match.start()] + f.read() + '\n' + script[match.end():]
        else:
            to_include = False

    script = re.sub(r'((?:\w|\.)+\$)\s*\+=', r'\1 = \1 +', script)
    parselmouth.praat.run(script)


#MAIN

if len(sys.argv) < 2:
	print("Error! Give argument: input_file.wav")#wavfile_data.txt wavfile_profile_data.txt")
	sys.exit(0)

file_in = sys.argv[1]

init_script = 'include prosomain.praat\n'
init_script += 'call prosogram file=' + file_in + ' save=yes draw=no\n'
init_script += 'exit'

run_script_with_pluseq(init_script) #Generate data and profile_data files using prosogram in praat

#Now using the datafiles for patterns

datafile_in = file_in[:-4] + "_data.txt" #removing ".wav"
profile_in = file_in[:-4] + "_profile_data.txt"

f = open(datafile_in)

text = f.read()
lines = text.split('\n')

new_lines = []

for line in lines:
	split_line = line.split('\t')
	new_lines.append(split_line)

#print(new_lines)

row_label_index = new_lines[0].index("rowLabel")
nucl_t1_index = new_lines[0].index("nucl_t1")
nucl_t2_index = new_lines[0].index("nucl_t2")
f0_start_index = new_lines[0].index("f0_start")
f0_end_index = new_lines[0].index("f0_end")


f.close()

fp = open(profile_in)

text = fp.read()

lines = text.split('\n')

new_profile_lines = []

for line in lines:
	split_line = line.split('\t')
	new_profile_lines.append(split_line)

#print(new_profile_lines)

speech_rate = new_profile_lines[1][new_profile_lines[0].index("SpeechRate")]
speech_time = new_profile_lines[1][new_profile_lines[0].index("SpeechTime")]
f0_mean = float(new_profile_lines[1][new_profile_lines[0].index("F0MeanHz")])
pitch_top = float(new_profile_lines[1][new_profile_lines[0].index("PitchTopHz")])
pitch_bottom = float(new_profile_lines[1][new_profile_lines[0].index("PitchBottomHz")])

print(speech_rate, speech_time, f0_mean, pitch_bottom, pitch_top)

fp.close()

def comparison(f0_value, pitch_top, pitch_bottom, f0_mean):
	L_value = abs(f0_value - pitch_bottom)
	M_value = abs(f0_value - f0_mean)
	H_value = abs(f0_value - pitch_top)

	if L_value <= M_value and L_value < H_value:
		return "L"
	elif M_value < L_value and M_value <= H_value:
		return "M"
	else:
		return "H"

#Getting L,H,M from data

new_lines = new_lines[1:] #remove header

labelled_rows = OrderedDict()

for row in new_lines:
	if len(row) > 1:
		#print("row_label:", row[row_label_index])
		#print("f0_start:", row[f0_start_index])
		#print("f0_end:", row[f0_end_index])

		label = row[row_label_index]
		f0_start = float(row[f0_start_index])
		f0_end = float(row[f0_end_index])
		#nucl_t1 = float(row[nucl_t1_index])
		#nucl_t2 = float(row[nucl_t2_index])


		if f0_start == f0_end:
			result_pitch = comparison(f0_start, pitch_top, pitch_bottom, f0_mean)
			#print(result_pitch)
			labelled_rows[label] =  result_pitch#, nucl_t1, nucl_t2]

		else:
			start_label = comparison(f0_start, pitch_top, pitch_bottom, f0_mean)
			end_label = comparison(f0_end, pitch_top, pitch_bottom, f0_mean)
			result_pitch = start_label + "-" + end_label
			#print(result_pitch)
			labelled_rows[label] =  result_pitch#, nucl_t1, nucl_t2]

#print(labelled_rows)

def compare_labels(last_pitch, pitch_label):
	if last_pitch == pitch_label:
		return True
	elif (last_pitch == 'L-L' and pitch_label == 'L') or (last_pitch == 'L' and pitch_label == 'L-L'):
		return True
	elif (last_pitch == 'M-M' and pitch_label == 'M') or (last_pitch == 'M' and pitch_label == 'M-M'):
		return True
	elif (last_pitch == 'H-H' and pitch_label == 'H') or (last_pitch == 'H' and pitch_label == 'H-H'):
		return True
	else:
		return False

#PREDICTION

prediction = ""

print("Time", "\t", "Label", "\t", "Trend", "\t", "Prediction")

trend = 0 #-1 when trend is down, +1 when trend is up
last_pitch = ''

count = 0

trend_count = 0

last_trend = 0

for i in labelled_rows:
	#FULL-STOP PREDICTION
	pitch_label = labelled_rows[i]
	
	if count == 0:
		last_pitch = pitch_label

	if last_pitch == 'L' and (pitch_label == 'M' or pitch_label == 'H'):
		trend = 1
	elif last_pitch == 'M' and pitch_label == 'H':
		trend = 1
	elif last_pitch == 'M' and pitch_label == 'L':
		trend = -1
	elif last_pitch == 'H' and (pitch_label == 'M' or pitch_label == 'L'):
		trend = -1
	elif (last_pitch == 'L-M' or  last_pitch == 'M-H' or last_pitch == 'L-H') and pitch_label == 'L':
		trend = -1
	elif last_pitch  == 'M-H' and pitch_label == 'M':
		trend = -1
	elif (last_pitch == 'H-M' or last_pitch == 'M-L' or last_pitch == 'H-L') and pitch_label == 'H':
		trend = 1
	elif last_pitch == 'H-M' and pitch_label ==  'H':
		trend = 1
	elif pitch_label == 'L-M' or pitch_label == 'M-H' or pitch_label == 'L-H':
		trend = 1
	elif pitch_label == 'H-M' or pitch_label == 'M-L' or pitch_label == 'H-L':
		trend = -1

	if compare_labels(last_pitch, pitch_label): #If pitch label doesn't change for a while then the trend becomes 0
		trend_count += 1

		if trend_count >= 4:
			trend = 0
	else:
		trend_count = 0

	last_pitch = pitch_label

	if last_trend == 1 and trend == -1:
		prediction = "Comma/Phrase Boundary"
	elif last_trend == -1 and trend == 1:
		prediction = "Sentence Boundary"
	else:
		prediction = ""

	last_trend = trend

	print(i, "\t", pitch_label, "\t", trend, "\t", prediction)

	count += 1

#Visualising the Data
y_array = []

for i in labelled_rows.keys():
	if labelled_rows[i] == 'H' or labelled_rows[i] == 'H-H':
		y_array.append(float(1))
	elif labelled_rows[i] == 'M' or labelled_rows == 'M-M':
		y_array.append(float(0))
	elif labelled_rows[i] == 'L' or labelled_rows == 'L-L':
		y_array.append(float(-1))
	elif labelled_rows[i] == 'L-M' or labelled_rows[i] == 'M-L':	
		y_array.append(float(-0.5))
	elif labelled_rows[i] == 'M-H' or labelled_rows[i] == 'H-M':	
		y_array.append(float(0.5))
	elif labelled_rows[i] == 'L-H' or labelled_rows[i] == 'H-L':
		y_array.append(float(0))
	else:
		y_array.append(float(0))

plt.scatter(labelled_rows.keys(), y_array, s=10)

color_dict = {'L': 'red', 'M': 'gold', 'H': 'green', 'L-L': 'red', 'M-M': 'yellow', 'H-H': 'green'}

count = 0
for row in labelled_rows:
	#print(float(row), labelled_rows[row])
	plt.text(row, y_array[count], labelled_rows[row], color = color_dict.get(labelled_rows[row], 'black'))

	count += 1
plt.xticks(rotation=90)

plt.show()


















