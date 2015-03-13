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
	with_0b = bin(x + 0x10000)
	padded = '0'*bit_count + with_0b[2:]
	return padded[-bit_count:]

def parse_literal(x):
	hex = x.find('X')>=0
	val = int(x, 16 if hex else 10)
	return val

def literal(s):
	return lambda compiler, match: s

def highreg_and_immediate8_encoder(opcode):
	def dest_imm_12bit(compiler, match):
		reg = int(match.group('reg'))
		if reg < 16: raise TypeError("Must use one of registers 16-31")
		immediate = parse_literal(match.group('immediate')) 
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
		offset = compiler.jump_offset_for(label)
		if offset < -64 or offset >= 64:
			raise TypeError("Too far to jump")
		return tobin(offset, 7)
	return [literal(prefix), encode_7bit_offset, literal(suffix)] 

def jump_12bit_encoder(prefix):
	def encode_12bit_offset(compiler, match):
		label = match.group('label')
		offset = compiler.jump_offset_for(label)
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
	pattern = "^" + pattern + "$"
	return (pattern, conversion[1])

def encode_CCCC_KKKK_dddd_KKKK(prefix):
	return highreg_and_immediate8_encoder(prefix)
def encode_CCCC_CCCd_dddd_CCCC(prefix, suffix):
	return unary_5bit_encoder(prefix, suffix)
def encode_CCCC_CCrd_dddd_rrrr(prefix):
	return any_source_dest_encoder(prefix)
def encode_CCCC_CCCC_KKdd_KKKK(prefix):
	def encode_doublereg(compiler, match):
		lower_reg = int(match.group("reg"))
		upper_reg = match.group("source")
		immediate = parse_literal(match.group('immediate')) 
		if immediate<0 or immediate >= 64:
			raise TypeError("Immediate operand must be between 0 and 63")
		if upper_reg and int(upper_reg) != lower_reg+1:
			raise TypeError("Must modify consecutive registers")
		if lower_reg not in [24,26,28,30]:
			raise TypeError("Must modify R24, R26, R28, or R30")
		regindex = (lower_reg - 24)/2
		return prefix + tobin(immediate>>4,2) + tobin(regindex,2) + tobin(immediate&0xf, 4)
	return [encode_doublereg]

def encode_CCCC_CCCd_dddd_CCCC_k16(prefix, suffix):
	def encode(compiler, match):
		reg = int(match.group("reg"))
		immediate = parse_literal(match.group('immediate'))
		if immediate<0 or immediate > 0xffff:
			raise "Immediate operand out of range"
		return prefix + tobin(reg,5) + suffix + tobin(immediate, 16)
	return [encode]
def aliased_encoder(alias_pattern):
	def encode(compiler, match):
		rewritten = alias_pattern
		groups = match.groupdict()
		rewritten = rewritten.replace('dest', 'R'+groups.get('dest', ''))
		rewritten = rewritten.replace('source', 'R'+groups.get('source', ''))
		rewritten = rewritten.replace('immediate', groups.get('immediate', ''))
		rewritten = rewritten.replace('label', groups.get('label', ''))
		rewritten = rewritten.replace('reg', 'R'+groups.get('reg', ''))
		print("Rewrote to ", rewritten)
		return instruction_to_word(compiler, rewritten)
	return [encode]

instruction_conversions = [patternify_conversion(c) for c in [
	("ADC dest,source", encode_CCCC_CCrd_dddd_rrrr("000111")),
	("ADD dest,source", encode_CCCC_CCrd_dddd_rrrr("000011")),
	("AND dest,source", encode_CCCC_CCrd_dddd_rrrr("001000")),
	("ANDI reg,immediate", encode_CCCC_KKKK_dddd_KKKK("0111")),
	("BRNE label", jump_7bit_encoder("111101", "001")),
	("BRPL label", jump_7bit_encoder("111101", "010")),
	("CLC", [literal("1001 0100 1000 1000")]),
	("COM reg", encode_CCCC_CCCd_dddd_CCCC("1001010", "0000")),
	("CP dest,source", any_source_dest_encoder("000101")),
	("DEC reg", unary_5bit_encoder("1001010", "1010")),
	("EOR dest,source", any_source_dest_encoder("001001")),
	("LD reg,X", encode_CCCC_CCCd_dddd_CCCC("1001 000", "1100")),
	("LD reg,X+", encode_CCCC_CCCd_dddd_CCCC("1001 000", "1101")),
	("LD reg,-X", encode_CCCC_CCCd_dddd_CCCC("1001 000", "1110")),
	("LD reg,Y", encode_CCCC_CCCd_dddd_CCCC("1001 000", "1000")),
	("LD reg,Y+", encode_CCCC_CCCd_dddd_CCCC("1001 000", "1001")),
	("LD reg,-Y", encode_CCCC_CCCd_dddd_CCCC("1001 000", "1010")),
	("LD reg,Z", encode_CCCC_CCCd_dddd_CCCC("1000 000", "0000")),
	("LD reg,Z+", encode_CCCC_CCCd_dddd_CCCC("1001 000", "0001")),
	("LD reg,-Z", encode_CCCC_CCCd_dddd_CCCC("1001 000", "0010")),
	("LDI reg,immediate", highreg_and_immediate8_encoder("1110")),
	("LDS reg,immediate", encode_CCCC_CCCd_dddd_CCCC_k16("1001 000", "0000")),
	("LPM reg,Z",  unary_5bit_encoder("1001000", "0100")),
	("LPM reg,Z+", encode_CCCC_CCCd_dddd_CCCC("1001 000", "0101")),
	("LSL reg", aliased_encoder("ADD reg,reg")),
	("LSR reg", encode_CCCC_CCCd_dddd_CCCC("1001010", "0110")),
	("MOV dest,source", any_source_dest_encoder("001011")),
	("NEG reg", encode_CCCC_CCCd_dddd_CCCC("1001010", "0001")),
	("OR dest,source", encode_CCCC_CCrd_dddd_rrrr("001010")),
	("RET", [literal("1001 0101 0000 1000")]),
	("ROL reg", aliased_encoder("ADC reg,reg")),
	("RCALL label", jump_12bit_encoder("1101")),
	("SBC dest,source", encode_CCCC_CCrd_dddd_rrrr("000010")),
	("SBIW (source:)?reg,immediate", encode_CCCC_CCCC_KKdd_KKKK("1001 0111")),
	("SBIW (XH:)?XL,immediate", aliased_encoder("SBIW R27:R26,immediate")),
	("SBIW (YH:)?YL,immediate", aliased_encoder("SBIW R29:R28,immediate")),
	("SBIW (ZH:)?ZL,immediate", aliased_encoder("SBIW R31:R30,immediate")),
	("SBIW X,immediate", aliased_encoder("SBIW R27:R26,immediate")),
	("SBIW Y,immediate", aliased_encoder("SBIW R29:R28,immediate")),
	("SBIW Z,immediate", aliased_encoder("SBIW R31:R30,immediate")),
	("ST X,reg",  unary_5bit_encoder("1001001", "1100")),
	("ST X+,reg", unary_5bit_encoder("1001001", "1101")),
	("ST -X,reg", unary_5bit_encoder("1001001", "1110")),
	("ST Y,reg",  unary_5bit_encoder("1000001", "1000")),
	("ST Y+,reg", unary_5bit_encoder("1001001", "1001")),
	("ST -Y,reg", unary_5bit_encoder("1001001", "1010")),
	("ST Z,reg",  unary_5bit_encoder("1000001", "0000")),
	("ST Z+,reg", unary_5bit_encoder("1001001", "0001")),
	("ST -Z,reg", unary_5bit_encoder("1001001", "0010")),

]]

def instruction_to_word(compiler, line):
	for (pattern, to) in instruction_conversions:
		m = re.match(pattern, line)
		if m:
			result = ''.join([f(compiler, m) for f in to])
			result = result.replace(' ', '')
			if len(result) % 16 != 0:
				print(result)
				raise TypeError("Line did not produce a proper word: " + line)
			return result
	raise TypeError("Did not understand a line: " + line)

class Compiler:
	def __init__(self, asm_lines):
		self.asm_lines = asm_lines	
		self.write_addr = 0
		self.program_words = [None]*4096
		self.pass_addr_for_label = dict()
		self.addr_for_label = None 

	def run(self):
		prog_word = dict()
		for line in self.asm_lines:
			line = normalize_line(line)
			self.process_line(line)

	def process_line(self, line):
		if len(line)==0: return
		if line[0] == '.':
			return self.process_command(line)
		print(line)
		label, instruction = parse_label(line)
		if label:
			if label in self.pass_addr_for_label:
				raise TypeError("Label redefined: " + label)
			self.pass_addr_for_label[label] = self.write_addr
		if instruction:
			self.process_instruction(line)

	def process_command(self, line):
		print(line)
		command_and_args = line.split(' ')
		if len(command_and_args) != 2: raise TypeError("Invalid compiler command")
		command, args = command_and_args
		if command == '.ORG':
			return self.process_org_command(args)
		elif command == '.DB':
			return self.process_db_command(args)
		else:
			raise TypeError("Unrecognized compiler command")

	def process_org_command(self, args):
		self.write_addr = parse_literal(args)
				
	def process_db_command(self, args):
		bytes = [parse_literal(arg) for arg in args.split(',')]
		for byte in bytes:
			if byte<0 or byte>=256:
				raise TypeError("Byte literal out of range")
		for idx in range(0, len(bytes), 2):
			low_byte = bytes[idx]
			high_byte = bytes[idx+1] if idx+1 < len(bytes) else 0
			self.process_word(low_byte | (high_byte<<8), "(byte literals)")
			

	def process_instruction(self, line):
		result = instruction_to_word(self, line)
		for word_start in range(0, len(result), 16):
			self.process_word(int(result[word_start:word_start+16], 2), line)

	def process_word(self, word, source_code):
		if self.program_words[self.write_addr]:
			raise TypeError("Two instructions tried to write into word " + \
				str(self.write_addr) + ": \"" + \
				self.program_words[self.write_addr][1] + "\" and \"" + \
				source_code + "\"")
		self.program_words[self.write_addr] = (word, source_code)
		self.write_addr += 1

	def jump_offset_for(self, label):
		if self.addr_for_label:
			if label not in self.addr_for_label:
				raise TypeError("Unknown label: " + label)
			print("Resolving %s to %d", (label, self.addr_for_label[label]))
			return self.addr_for_label[label] - self.write_addr - 1
		else:
			return 0

	def output_bin(self, path):
		def word_to_bytes(word):
			if word == None: return [0xff, 0xff]
			return [word[0]&0xff, (word[0]>>8)&0xff]
		
		bytes = sum( [word_to_bytes(word) for word in self.program_words], [])
		with open(path, "wb") as output:
			output.write(bytearray(bytes))

	def output_lisp(self, path):
		
		with open(path, "w") as out:
			out.write("(defconst *prog-mem* (list\n");
			for word in self.program_words:
				if word == None:
					out.write("  nil\n");
				else:
					out.write("  #x%04x ; %s\n" % (word[0], word[1]))
			out.write("))\n");
			
compiler = Compiler(lines)
compiler.run()
compiler.addr_for_label = compiler.pass_addr_for_label
compiler.pass_addr_for_label = dict()
compiler.program_words = [None]*4096
compiler.run()
compiler.output_bin("out-compiled.bin")
compiler.output_lisp("out-compiled.lisp")

