import sys
import re

path = sys.argv[1]
output_path = sys.argv[2]

byte_at = dict()
segment_offset = 0

for line in open(path).readlines():
	line = line.rstrip('\r\n')
	if not re.match("^:[0-9a-fA-F]+$", line) or len(line)%2==0:
		raise TypeError("Malformed line: " + line)
	print(line)
	hex_bytes = line[1:]
	bytes = [int(hex_bytes[i:i+2], 16) for i in range(0, len(hex_bytes), 2)]
	
	if len(bytes) < 5:
		raise TypeError("Line too short: " + line)

	payload_count = bytes[0]
	address = (bytes[1]<<8) | bytes[2]
	record_type = bytes[3]
	if len(bytes) != payload_count + 5:
		raise TypeError("Payload length field mismatched with actual payload length")
	payload = bytes[4:-1]
	checksum = bytes[-1]
	if (-sum(bytes[:-1])&0xff) != checksum:
		raise TypeError("Invalid checksum in " + line)

	if record_type == 0:	
		for (idx, byte) in enumerate(payload):
			print(idx, byte)	
			offset_address = segment_offset*16 + address+idx
			if offset_address in byte_at:
				raise TypeError("Tried to write into %d twice" % offset_address)
			byte_at[address+idx] = byte
	elif record_type == 1:
		if len(payload): raise TypeError("EOF record should be empty")
		break
	elif record_type == 2:
		if len(payload)!=2: raise TypeError("Segment offset record should contain two bytes of data")
		segment_offset = (payload[0]<<8) | payload[1]
	else:
		raise TypeError("Unknown record type: %d" % record_type)

max_address = max(byte_at.keys())
binary = [byte_at.get(i, 0) for i in range(max_address)]
with open(output_path, 'wb') as output:
    output.write(bytearray(binary))

