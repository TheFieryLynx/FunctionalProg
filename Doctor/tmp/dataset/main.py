import json

with open('coqa.json') as f:
	dataset = json.load(f)
	
#for i in dataset['data']:

res = []

for i in dataset['data']:
	for j in i['answers']:
		tmp_text = j['span_text'].split(' ')
		if len(tmp_text) > 4:
			for l in tmp_text:
				if l == '' or l == ' ':
					tmp_text.remove(l)
			text = ' '.join(tmp_text)
			while text[0] == ' ':
				text = text[1:]
			while text[len(text) - 1] == ' ':
				text = text[:len(text) - 1]
			
			if text[len(text) - 1] == '\n':
				text = text[:len(text) - 1]
			if text[0] == ".":
				text = text[1:]
			
			
			text = text.replace('\"', '')
			text = text.replace('"', '')
			text = text.replace('()', '')
			text = text.replace('\n', '')
			text = text.replace('--', '')
			text = text.replace('(,', '')
			text = text.replace(',)', '')
			text = text.replace(' ,', ',')
			text = text.replace("'", '')
			text = text.replace("\'", '')
			
			while text[0] == ' ':
				text = text[1:]
			while text[len(text) - 1] == ' ':
				text = text[:len(text) - 1]
				
			if text[len(text) - 1].isalpha():
				text += '.'
			else:
				text = text[:len(text) - 1] + '.'
			
			res.append(text)
		
final_text = ""
for i in res:
	final_text += i + ' '
final_text = final_text[:len(final_text) - 1]

with open(f'final.txt', 'w') as f:
	f.write(final_text)

with open(f'res.txt', 'w') as f:
	json.dump(res, f, ensure_ascii=False, indent=4)
	
	