#!usr/bin/python


""" Program to edit lisp code
	it erases outputs and uncomments loads of compiled files
"""


def prepareMoshak():
	fileName = 'scripts/SolF1.lisp'
	myFile = open(fileName, 'r')
	lines = myFile.readlines()
	myFile.close()
	myFile = open(fileName, 'w')
	for line in lines:
		if('(terpri)' in line):
			myFile.write('\n')
			print 'REMOVED TERPRI'
			print line
		elif('write' in line):
			myFile.write('\n')
			print 'REMOVED WRITE'
			print line
		elif(('.fas' in line) and (';' in line)):
			newLine = line[1:]
			print line
			myFile.write(newLine)
		elif(('.lisp' in line) and (';' not in line)):
			newLine = ';' + line
			print line
			myFile.write(newLine)
		else:
			myFile.write(line)
	myFile.close()

if __name__ == "__main__":
	prepareMoshak()
