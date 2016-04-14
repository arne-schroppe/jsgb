import sys, re


def print_usage_and_exit():
    print("Usage:\n" + sys.argv[0] + " rgb hex-string")
    sys.exit(-1)

if len(sys.argv) < 2:
    print_usage_and_exit()

rgb = re.search("#?(..)(..)(..)", sys.argv[1])

if rgb == None:
    print_usage_and_exit()



r = int(rgb.group(1), 16)
g = int(rgb.group(2), 16)
b = int(rgb.group(3), 16)

# Convert to 0-31 range
r = r >> 5
g = g >> 5
b = b >> 5

packed = (r << 10) + (g << 5) + b

print("Packed colors")
print("{0:b}".format(packed).zfill(16))
print( ("%x" % packed).zfill(4) )
