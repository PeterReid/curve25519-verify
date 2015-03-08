import sys
import re

asm_path = sys.argv[1]

lines = open(asm_path).readlines()

def normalize_line(line):
	comment_start = line.find(';')
	if comment_start >= 0: line = line[:comment_start]

	line = line.strip(' \t\r\n')
	# Remove extra spaces
	line = re.sub('\s+', ' ', line)
	line = re.sub('\s*,\s*', ',', line)
	line = re.sub('\s*:\s*', ':', line)
	line = re.sub('\s*\+\s*', '+', line)
	line = line.upper()
	return line	

def parse_label(line):
	colon = line.find(':')
	if colon == -1:
		return '', line
	label = line[:colon]
	instruction = line[colon+1:]
	return (label, instruction)

dest_pat = "R(?P<dest>[012]?[0-9]|30|31)"
source_pat = "R(?P<source>[012]?[0-9]|30|31)"
reg_pat = "R(?P<reg>[012]?[0-9]|30|31)"
immediate_pat = "(?P<immediate>\d+|0[xX][0-9a-fA-F]+)"
label_pat = "(?P<label>[A-Z0-9_]+)"

# x as a binary string, bit_count characters long
def tobin(x, bit_count):
	with_0b = bin(x)
	padded = '0'*bit_count + with_0b[2:]
	return padded[-bit_count:]

def parse_8bit(x):
	val = int(x)
	if val < 0 or val >= 256: raise("Literal out of range: " + str(x))
	return val

def literal(s):
	return lambda compiler, match: s

def highreg_and_immediate8_encoder(opcode):
	def dest_imm_12bit(compiler, match):
		reg = int(match.group('reg'))
		if reg < 16: raise TypeError("Must use one of registers 16-31")
		immediate = parse_8bit(match.group('immediate')) 
		return tobin(immediate>>4, 4) + tobin(reg-16, 4) + tobin(immediate&0xf, 4)
	return [literal(opcode), dest_imm_12bit]

def any_source_dest_encoder(opcode):
	# encode source and dest as rd dddd rrrr
	def dest_src_10bit(compiler, match):
		source = int(match.group('source'))
		dest = int(match.group('dest'))
		return tobin(source>>4, 1) + tobin(dest>>4, 1) + tobin(dest&0xf, 4) + tobin(source&0xf, 4)
	return [literal(opcode), dest_src_10bit]

def reg_5bit(compiler, match):
	return tobin(int(match.group('reg')), 5)



def unary_5bit_encoder(prefix, suffix):
	def encode(compiler, match):
		reg = int(match.group('reg'))
		return prefix + tobin(reg, 5) + suffix
	return [literal(prefix), reg_5bit, literal(suffix)]

def jump_7bit_encoder(prefix, suffix):
	def encode_7bit_offset(compiler, match):
		label = match.group('label')
		offset = 0 # TODO: Actually put the offset in here
		if offset < -64 or offset >= 64:
			raise TypeError("Too far to jump")
		return tobin(offset, 7)
	return [literal(prefix), encode_7bit_offset, literal(suffix)] 

def jump_12bit_encoder(prefix):
	def encode_12bit_offset(compiler, match):
		label = match.group('label')
		offset = 0 # TODO: Actually put the offset in here
		if offset < -2048 or offset >= 2048:
			raise TypeError("Too far to jump")
		return tobin(offset, 12)
	return [literal(prefix), encode_12bit_offset] 
def patternify_conversion(conversion):
	pattern = conversion[0]
	pattern = pattern.replace('+', '\+')
	pattern = pattern.replace('source', source_pat)
	pattern = pattern.replace('dest', dest_pat)
	pattern = pattern.replace('reg', reg_pat)
	pattern = pattern.replace('immediate', immediate_pat)
	pattern = pattern.replace('label', label_pat)
	return (pattern, conversion[1])

def encode_CCCC_KKKK_dddd_KKKK(prefix):
	return highreg_and_immediate8_encoder(prefix)
def encode_CCCC_CCCd_dddd_CCCC(prefix, suffix):
	return unary_5bit_encoder(prefix, suffix)
def encode_CCCC_CCrd_dddd_rrrr(prefix):
	return any_source_dest_encoder(prefix)

instruction_conversions = [patternify_conversion(c) for c in [
	("ADC dest,source", encode_CCCC_CCrd_dddd_rrrr("000111")),
	("ADD dest,source", encode_CCCC_CCrd_dddd_rrrr("000011")),
	("AND dest,source", encode_CCCC_CCrd_dddd_rrrr("001000")),
	("ANDI reg,immediate", encode_CCCC_KKKK_dddd_KKKK("0111")),
	("BRNE label", jump_7bit_encoder("111101", "001")),
	("CP dest,source", any_source_dest_encoder("000101")),
	("DEC reg", unary_5bit_encoder("1001010", "1010")),
	("EOR dest,source", any_source_dest_encoder("001001")),
	("LDI reg,immediate", highreg_and_immediate8_encoder("1110")),
	("LPM reg,Z",  unary_5bit_encoder("1001000", "0100")),
	("LPM reg,Z+", unary_5bit_encoder("1001000", "0101")),
	("LSR reg", encode_CCCC_CCCd_dddd_CCCC("1001010", "0110")),
	("MOV dest,source", any_source_dest_encoder("001001")),
	("NEG reg", encode_CCCC_CCCd_dddd_CCCC("1001010", "0001")),
	("RCALL label", jump_12bit_encoder("1101")),
	("ST X,reg",  unary_5bit_encoder("1001001", "1100")),
	("ST X+,reg", unary_5bit_encoder("1001001", "1101")),
	("ST -X,reg", unary_5bit_encoder("1001001", "1110")),

]]

class Compiler:
	def __init__(self, asm_lines):
		self.asm_lines = asm_lines	
		self.write_addr = 0
		self.program_words = [None]*4096

	def run(self):
		prog_word = dict()
		for line in self.asm_lines:
			line = normalize_line(line)
			self.process_line(line)

	def process_line(self, line):
		if len(line)==0: return
		if line[0] == '.':
			return self.process_command(line)
		label, instruction = parse_label(line)
		if label:
			self.addr_for_label = self.write_addr
		if instruction:
			self.process_instruction(line)

	def process_command(self, line):
		pass # todo

	def process_instruction(self, line):
		print(":: ", line)
		for (pattern, to) in instruction_conversions:
			print(pattern)
			m = re.match(pattern, line)
			if m:
				result = ''.join([f(self, m) for f in to])
				if len(result) != 16:
					raise TypeError("Line did not produce a proper word: " + line)
				print(line, "matched", pattern, result)
				self.process_word(int(result, 2), line)
				return


		raise TypeError("Did not understand a line: " + line)
		#for template in templates:
		#	if template

	def process_word(self, word, source_code):
		if self.program_words[self.write_addr]:
			raise TypeError("Two instructions tried to write into word " + \
				str(self.write_addr) + ": \"" + \
				self.program_words[self.write_addr][1] + "\" and \"" + \
				source_code + "\"")
		self.program_words[self.write_addr] = (word, source_code)
		self.write_addr += 1

compiler = Compiler(lines)
compiler.run()

